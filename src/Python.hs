{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE ViewPatterns #-}

module Python where

import Control.Lens ((^.), at)
import Control.Monad.Reader
import Control.Monad.Except

import Expand
import Pretty
import Prelude hiding (LT, EQ, GT, (<>))
import Syntax
import ShapeChecker
import Text.PrettyPrint
import qualified Data.Map as M

precedence :: M.Map Binop Int
precedence = M.fromList [(OR, 0), (AND, 1),
                         (EQ, 2), (NEQ, 2),
                         (LT, 3), (LE, 3), (GT, 3), (GE, 3),
                         (PLUS, 4), (MINUS, 4),
                         (MULT, 5), (DIV, 5)
                        ]

fixity :: M.Map Binop Int
fixity = M.fromList [(OR, 1), (AND, 1),
                     (EQ, 0), (NEQ, 0),
                     (LT, 0), (LE, 0), (GT, 0), (GE, 0),
                     (PLUS, 1), (MINUS, 1),
                     (MULT, 1), (DIV, 1)
                    ]

opDoc :: M.Map Binop Doc
opDoc = M.fromList [(OR, text "or"), (AND, text "and"),
                    (EQ, text "=="), (NEQ, text "!="),
                    (LT, text "<"), (LE, text "<="),
                    (GT, text ">"), (GE, text ">="),
                    (PLUS, text "+"), (MINUS, text "-"),
                    (MULT, text "*"), (DIV, text "/")
                   ]

type Constraints c e m = ( Get c Context
                         , Inj e ShapeError
                         , MonadReader c m
                         , MonadError e m )

dot :: Doc
dot = text "."

parensIf :: Bool -> Doc -> Doc
parensIf cond doc = if cond then lparen <> doc <> rparen else doc

transInit :: (Constraints c e m) => Tau -> m Doc
transInit TInt = return $ int 0
transInit TFloat = return $ float 0
transInit TBool = return $ text "False"
transInit TAny = return $ text "None"
transInit tarr@(TArr t (Just len))
  | isTauNumericArray tarr = do
      contents <- replicateM len (transInit t)
      let contentDoc = foldr (\c acc -> c <> comma <+> acc) empty contents
      return $ text "np.array" <> (parens . brackets $ contentDoc)
  | otherwise = do
      contents <- replicateM len (transInit t)
      return $ brackets $ foldr (\c acc -> c <> comma <+> acc) empty contents
transInit t@(TArr _ Nothing)
  | isTauNumericArray t = do
      return $ text "np.array" <> (parens . brackets $ empty)
  | otherwise =
      return $ brackets empty
transInit (TBag _) = return $ brackets empty
transInit _ = shapeFail trivialPosition $ "not supported"

isTauNumericArray' :: Bool -> Tau -> Bool
isTauNumericArray' inarray TInt       = inarray
isTauNumericArray' inarray TFloat     = inarray
isTauNumericArray' inarray TBool      = inarray
isTauNumericArray' inarray TAny       = inarray
isTauNumericArray' inarray (TArr t _) = inarray && (isTauNumericArray' True t)
isTauNumericArray' _       (TBag _)   = False
isTauNumericArray' _       (TRec _)   = False

isTauNumericArray :: Tau -> Bool
isTauNumericArray (TArr t _) = isTauNumericArray' True t
isTauNumericArray _ = False

tauNeedsCopy :: Tau -> Bool
tauNeedsCopy (TArr _ _) = True
tauNeedsCopy (TBag _) = True
tauNeedsCopy (TRec _) = True
tauNeedsCopy _ = False

transDecl :: (Constraints c e m) => Decl -> m Doc
transDecl (Decl _ x _ t) = do
  initDoc <- transInit t
  return $ text x <+> equals <+> initDoc

transDecls :: (Constraints c e m) => [Decl] -> m Doc
transDecls decls =
  vcat <$> mapM transDecl decls

transLit :: (Constraints c e m) => Lit -> m Doc
transLit (LInt i) = return $ int i
transLit (LFloat f) = return $ float f
transLit (LBool b) = return $
  if b then text "True" else text "False"
transLit (LArr es) = do
  ds <- mapM (flip transExpr 0) es
  let combinedDoc = foldr (\d acc -> d <> comma <+> acc) empty ds
  return $ text "np.array"
           <> (parens . brackets $ combinedDoc)
transLit (LBag es) = do
  ds <- mapM (flip transExpr 0) es
  let combinedDoc = foldr (\d acc -> d <> comma <+> acc) empty ds
  return . brackets $ combinedDoc

transExpr :: (Constraints c e m) => Expr -> Int -> m Doc
transExpr (EVar _ x) _ = return $ text x
transExpr (ELength _ e) _ = do
  docE <- transExpr e 0
  return $ text "len" <> parens docE
transExpr (ELit _ lit) _ = transLit lit
transExpr (EBinop pos el op er) prec = do
  let f = fixity ^. (at op)
  let p = precedence ^. (at op)
  let od = opDoc ^. (at op)
  case (f, p, od) of
    (Just f, Just p, Just od) -> do
      docL <- transExpr el p
      docR <- transExpr er (p + f)
      return $ parensIf (prec > p) $ docL <+> od <+> docR
    _ -> shapeFail pos $ "Unknown fixity or precendence for operator: " ++ show op
transExpr (EIndex _ earr eidx) _ = do
  darr <- transExpr earr 0
  didx <- transExpr eidx 0
  return $ darr <> (brackets $ didx)
transExpr (ERAccess _ erec label) _ = do
  drec <- transExpr erec 0
  return $ drec <> (brackets . quotes $ text label)
transExpr (EFloat _ e) _ = do
  docE <- transExpr e 0
  return $ text "float" <> parens docE
transExpr (EClip _ e lit) _ = do
  docE <- transExpr e 0
  docL <- transLit lit
  return $ text "np.clip" <> (parens $ docE <> comma <+> text "-" <> docL <> comma <+> docL)
transExpr (EScale p el er) prec =
  transExpr (EBinop p el MULT er) prec
transExpr (EDot _ el er) _ = do
  dl <- transExpr el 0
  dr <- transExpr er 0
  return $ text "np.dot" <> (parens $ dl <> comma <+> dr)
transExpr (EExp _ e) _ = do
  d <- transExpr e 0
  return $ text "np.exp" <> parens d
transExpr (ELog _ e) _ = do
  d <- transExpr e 0
  return $ text "np.log" <> parens d

getArrayOrBagContentTau :: (Constraints c e m) => Tau -> m Tau
getArrayOrBagContentTau (TArr t _) = return t
getArrayOrBagContentTau (TBag t) = return t
getArrayOrBagContentTau _ = shapeFail trivialPosition "not an array or bag type!"

transCmd :: (Constraints c e m) => Cmd -> m Doc
transCmd (CAssign p (EVar _ x) e) = do
  ctx <- askCtxt
  let tx = ctx ^. (at x)
  case tx of
    Just (TArr _ _) -> do
      docE <- transExpr e 0
      return $ text x <+> equals <+> text "np.array" <> (parens docE)
    Just t
      | tauNeedsCopy t -> do
          docE <- transExpr e 0
          return $ text x <+> equals <+> text "copy.deepcopy" <> (parens docE)
      | otherwise -> do
          docE <- transExpr e 0
          return $ text x <+> equals <+> docE
    Nothing ->
      shapeFail p "bug in shape checker?"
transCmd (CAssign _ (EIndex _ (EVar _ x) eidx) e) = do
  te <- checkExpr e
  case te of
    TArr _ _ -> do
      docIdx <- transExpr eidx 0
      docE <- transExpr e 0
      return $ text x <> (brackets docIdx) <+> equals <+> text "np.array" <> (parens docE)
    t | tauNeedsCopy t -> do
          docIdx <- transExpr eidx 0
          docE <- transExpr e 0
          return $ text x <> (brackets docIdx) <+> equals <+> text "copy.deepcopy" <> (parens docE)
      | otherwise -> do
          docIdx <- transExpr eidx 0
          docE <- transExpr e 0
          return $ text x <> (brackets docIdx) <+> equals <+> docE
transCmd (CAssign p (ELength _ (EVar _ x)) e) = do
  ctx <- askCtxt
  let tx = ctx ^. (at x)
  case isTauNumericArray <$> tx of
    Just True -> do
      docE <- transExpr e 0
      let xShape = text x <> dot <> text "shape"
          newLength = lparen <> docE <> comma <> rparen
          newShape = newLength <> text "+" <> xShape <> (brackets $ int 1 <> text ":")
      return $ text x <> dot <> text "resize" <> (parens $ newShape)
    Just False -> do
      docE <- transExpr e 0
      case tx of
        Just tArrOrBag -> do
          contentTau <- getArrayOrBagContentTau tArrOrBag
          defaultValDoc <- transInit contentTau
          return $ text x <+> equals
                          <+> text "resize_bag"
                          <> (parens $ text x <> comma <+> docE <> comma <+> defaultValDoc)
        _ -> shapeFail p "bug in shape checker?"
    _ -> shapeFail p "bug in shape checker?"
transCmd (CAssign p _ _) =
  shapeFail p "unsupported assignment form, bug in shape checker?"
transCmd (CLaplace _ x scale mean) = do
  docMean <- transExpr mean 0
  return $ text x <+> equals
                  <+> text "np.random.laplace"
                  <> (parens $ docMean <> comma <+> float scale)
transCmd (CIf _ e c1 c2) = do
  docE <- transExpr e 0
  doc1 <- transCmd c1
  doc2 <- transCmd c2
  return $ vcat [
    text "if" <+> docE <> colon,
    nest 2 doc1,
    text "else" <> colon,
    nest 2 doc2
    ]
transCmd (CWhile _ e c) = do
  docE <- transExpr e 0
  docC <- transCmd c
  return $ vcat [
    text "while" <+> docE <> colon,
    nest 2 docC
    ]
transCmd (CSeq _ c1 c2) = do
  doc1 <- transCmd c1
  doc2 <- transCmd c2
  return $ vcat [
    doc1,
    doc2
    ]
transCmd (CSkip _) = return $ text "pass"
transCmd (CBlock _ c) = transCmd c
transCmd c = shapeFail (cmdPosn c) $ "unsupported constructs: " ++ show (PrettyCmd c)

imports :: Doc
imports = vcat [
  text "import numpy as np",
  text "import json",
  text "import copy"
  ]

prologue :: Doc
prologue = text $ unlines [
  "def resize_bag(arr, new_len, v):",
  "  if new_len > len(arr):",
  "    return arr + [v] * (new_len - len(arr))",
  "  else:",
  "    return arr[0:new_len]",
  "",
  "def init_with_json(data, name):",
  "  return data[name]"
  ]

transInputs :: (Constraints c e m) => String -> [Decl] -> m Doc
transInputs path decls = do
  let readJsonDoc = text "input_data"
                    <+> equals
                    <+> text "json.load"
                    <> (parens $ text "open" <> (parens . quotes $ text path))
  return . vcat $ [
    readJsonDoc
    ] ++ (map go decls)
  where go (Decl _ x _ t) =
          let d =
                case isTauNumericArray t of
                  True ->
                    text x <+> equals
                           <+> text "np.array"
                               <> (parens $ text "init_with_json"
                                   <> (parens $ text "input_data"
                                                <> comma
                                                <+> (quotes $ text x)))
                  False -> text x <+> equals
                           <+> text "init_with_json"
                           <> (parens $ text "input_data" <> comma <+> (quotes $ text x))
          in vcat [
            text "try:",
            nest 2 d,
            text "except KeyError:",
            nest 2 $ text "pass"
            ]


transProg :: (Constraints c e m) => String -> Prog -> m Doc
transProg jsonPath (Prog decls cmd) = do
  docDecls <- transDecls decls
  docInputs <- transInputs jsonPath decls
  docC <- transCmd (desugarFix cmd fuzziExpansionRules)
  let documentation = vcat [
        text "\"\"\"",
        text "Fuzzi source code: ",
        prettyCmd cmd,
        text "\"\"\""
        ]
  return $ vcat [
    documentation,
    text "\n",
    imports,
    text "\n",
    prologue,
    text "\n",
    docDecls,
    text "\n",
    docInputs,
    text "\n",
    docC,
    text "\n"
    ]

runTranspiler :: String -> Prog -> Either ShapeError Doc
runTranspiler jsonPath p =
  runExcept (runReaderT (transProg jsonPath p) (declsToContext . getDecls $ p))
