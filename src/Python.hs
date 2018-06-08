{-# LANGUAGE ViewPatterns #-}

module Python where

import Prelude hiding (LT, EQ, GT)
import Syntax
import Typechecker.Basic
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
opDoc = M.fromList [(OR, text "||"), (AND, text "&&"),
                    (EQ, equals), (NEQ, text "!="),
                    (LT, text "<"), (LE, text "<="),
                    (GT, text ">"), (GE, text ">="),
                    (PLUS, text "+"), (MINUS, text "-"),
                    (MULT, text "*"), (DIV, text "/")
                   ]

transSLit :: SmallLit -> Doc
transSLit (SILit d) = int d
transSLit (SFLit f) = float f
transSLit (SBLit b) = if b then text "True" else text "False"

transRowLit :: M.Map String SmallLit -> Doc
transRowLit rlit =
  M.foldrWithKey
    (\key val doc -> (quotes $ text key) <> transSLit val <> comma <+> doc)
    empty
    rlit

transLit :: Literal -> Doc
transLit (SLit slit) = transSLit slit
transLit (RLit (getRowLits -> rlit)) = transRowLit rlit

transExpr :: Expr -> Int -> Doc
transExpr (EVar _ x)    _    = text x
transExpr (ELength _ e) _    = text "len" <> lparen <> transExpr e 0 <> rparen
transExpr (ELit _ lit)  _    = transLit lit
transExpr (EBinop _ el op er) prec =
  let f = fixity M.! op
      p = precedence M.! op
  in parensIf (prec > p)
       $ (transExpr el p) <+> (opDoc M.! op) <+> (transExpr er (p + f))
transExpr (EIndex _ earr eidx) _ =
  transExpr earr 0 <> lbrack <> transExpr eidx 0 <> rbrack
transExpr (ERUpdate _ erec key eval) _ =
  text "rec_update" <> lparen <>  transExpr erec 0 <> comma
                              <+> text key <> comma
                              <+> transExpr eval 0
                              <> rparen
transExpr (ERAccess _ erec key) _ =
  transExpr erec 0 <> lbrack <> quotes (text key) <> rbrack
transExpr (EArray _ exprs) _ =
  text "np.array"
  <> lparen
  <> lbrack <> foldr (\e doc -> transExpr e 0 <> comma <+> doc) empty exprs <> rbrack
  <> rparen
transExpr (EBag _ exprs) _ =
  text "np.array"
  <> lparen
  <> lbrack <> foldr (\e doc -> transExpr e 0 <> comma <+> doc) empty exprs <> rbrack
  <> rparen
transExpr (EFloat _ e) _ =
  text "float" <> lparen <> transExpr e 0 <> rparen
transExpr (EClip _ e bounds) _ =
  text "np.clip" <> lparen <>  transExpr e 0 <> comma
                           <+> text "-" <> transLit bounds <> comma
                           <+> transLit bounds <> rparen
transExpr (EScale p el er) prec =
  transExpr (EBinop p el MULT er) prec
transExpr (EDot _ el er) _ =
  text "np.dot" <> lparen <>  transExpr el 0 <> comma
                          <+> transExpr er 0 <> rparen
transExpr (EExp _ e) _ =
  text "np.exp" <> lparen <> transExpr e 0 <> rparen

parensIf :: Bool -> Doc -> Doc
parensIf cond doc = if cond then lparen <> doc <> rparen else doc

dot :: Doc
dot = text "."

initFromSmallType :: SmallType -> Doc
initFromSmallType STInt   = int 0
initFromSmallType STFloat = float 0
initFromSmallType STBool  = text "False"
initFromSmallType STAny   = text "None"

initFromLargeType :: LargeType -> Doc
initFromLargeType (LTSmall st) = initFromSmallType st
initFromLargeType (LTRow (getRowTypes -> rtyps)) =
  lbrace <>
  M.foldrWithKey
    (\key st initE ->
       text key <> colon <+> initFromSmallType st <>
       comma <+> initE) empty rtyps
  <> rbrace
initFromLargeType (LTArray _ Nothing) =
  text "np.array" <> lparen <> lbrack <> rbrack <> rparen
initFromLargeType (LTArray _ (Just len)) =
  text "np.zeros" <> lparen <> int len <> rparen
initFromLargeType t@(LTBag _) =
  text "np.array" <>  lparen <> lbrack <> rbrack <> comma
                  <+> text "ndmin = " <> int (numDim t) <> rparen
  where numDim (LTSmall _) = 0
        numDim (LTArray _ _) = 1
        numDim (LTRow _) = 0
        numDim LTAny = 0
        numDim (LTBag ty) = 1 + numDim ty
initFromLargeType LTAny = text "None"

transCmd :: Context -> Cmd -> Doc
transCmd _ (CAssign _ (ELength _ elhs) erhs) =
  let x         = indexedVar elhs
      xShape    = text x <> dot <> text "shape"
      newLength = lparen <> transExpr erhs 0 <> comma <> rparen
      newShape  = newLength <> text "+" <> xShape <> lbrack <> int 1 <> text ":" <> rbrack
  in text x <> dot <> text "resize" <> lparen <> newShape <> rparen
transCmd _ (CAssign _ elhs erhs) =
  transExpr elhs 0 <+> equals <+> transExpr erhs 0
transCmd _ (CLaplace _ x scale mean) =
  text x <+> equals
         <+> text "np.random.laplace" <> lparen <>  transExpr mean 0 <> comma
                                                <+> float scale      <> rparen
transCmd ctx (CIf _ e ct cf) = vcat [
    text "if" <+> transExpr e 0 <+> colon,
    nest 2 $ transCmd ctx ct,
    text "else" <+> colon,
    nest 2 $ transCmd ctx cf
  ]
transCmd ctx (CWhile _ e c) = vcat [
    text "while" <+> transExpr e 0 <+> colon,
    nest 2 $ transCmd ctx c
  ]
transCmd ctx (CSeq _ c1 c2) = vcat [
    transCmd ctx c1,
    transCmd ctx c2
  ]
transCmd _ (CSkip _) = text "pass"
transCmd _ (CDecl _ x _ typ) =
  text x <+> equals <+> initFromLargeType typ
transCmd _ _ = error "Undesugared macro in python transpilation"

imports :: Doc
imports = vcat [
  text "import numpy as np"
  ]

prologue :: Doc
prologue = text $ unlines [
  "def rec_update(rec, k, v):",
  "  rec_copy = rec.copy()",
  "  rec_copy[k] = v",
  "  return rec_copy"
  ]

transpile :: Context -> Cmd -> String
transpile ctx c = render $ vcat [
    imports,
    text "\n",
    prologue,
    text "\n",
    transCmd ctx c,
    text "\n"
  ]
