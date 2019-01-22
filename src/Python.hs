module Python where

import Prelude hiding (EQ, LT, GT, (<>))

import SyntaxExt
import Shape hiding (ShapeCheckError(..))

import Data.Typeable
import Data.Comp
import Data.Comp.Derive
import qualified Data.Map as M
import Text.PrettyPrint
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Catch
import Control.Monad.Cont

import Debug.Trace

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

data PythonCode = PythonCode {
  _pythoncode_doc :: Int -> Doc
  , _pythoncode_shape :: ShapeInfo
  }

defaultPythonCode :: PythonCode
defaultPythonCode = PythonCode (\_ -> mempty) defaultEInfo

data PythonifyError = UnknownBinop  Position Binop
                    | InternalError Position String
  deriving (Show, Typeable)

instance Exception PythonifyError

$(makeLensesWith underscoreFields ''PythonCode)

projectShapeCheck :: ( MonadReader ShapeCxt m
                     , MonadThrow           m
                     , MonadCont            m
                     , Functor              f
                     , ShapeCheck           f)
                  => f PythonCode -> m ShapeInfo
projectShapeCheck py =
  shapeCheck $ fmap (view shape) py

parensIf :: Bool -> Doc -> Doc
parensIf cond d = if cond then parens d else d

class Pythonify f where
  pythonify :: ( MonadReader ShapeCxt m
               , MonadThrow           m
               , MonadCont            m)
            => AlgM m f PythonCode

$(derive [liftSum] [''Pythonify])

pythonifyBinop :: (MonadThrow m) => Binop -> PythonCode -> PythonCode -> Position -> m PythonCode
pythonifyBinop operator lhs rhs pos = do
  let f = M.lookup operator fixity
      p = M.lookup operator precedence
      od = M.lookup operator opDoc
  case (f, p, od) of
    (Just f', Just p', Just od') -> do
      let docL = (lhs ^. doc) p'
      let docR = (rhs ^. doc) (p' + f')
      return $ defaultPythonCode & doc .~ (\prec -> parensIf (prec > p') $ docL <+> od' <+> docR)
    _ -> throwM $ UnknownBinop pos operator

instance Pythonify (Expr :&: Position) where
  pythonify c@(EVar x :&: _) = do
    spinfo <- projectShapeCheck c
    return $ defaultPythonCode & doc .~ (\_ -> text x)
                               & shape .~ spinfo

  pythonify c@(ELength code :&: _) = do
    spinfo <- projectShapeCheck c
    return $ defaultPythonCode & doc .~ (\_ -> text "len" <> parens ((code ^. doc) 0))
                               & shape .~ spinfo

  pythonify c@(ELit (LInt i) :&: _) = do
    spinfo <- projectShapeCheck c
    return $ defaultPythonCode & doc .~ (\_ -> int i)
                               & shape .~ spinfo

  pythonify c@(ELit (LFloat f) :&: _) = do
    spinfo <- projectShapeCheck c
    return $ defaultPythonCode & doc .~ (\_ -> float f)
                               & shape .~ spinfo

  pythonify c@(ELit (LBool b) :&: _) = do
    spinfo <- projectShapeCheck c
    return $ defaultPythonCode & doc .~ (\_ -> bool b)
                               & shape .~ spinfo
    where bool cond = text $ if cond then "True" else "False"

  pythonify c@(ELit (LArr codes) :&: _) = do
    spinfo <- projectShapeCheck c
    let entries = map ($ 0) $ codes ^.. traverse . doc
    let combinedDoc = foldr (\e acc -> e <> comma <+> acc) mempty entries
    let arrayDoc = text "np.array" <> (parens . brackets) combinedDoc
    return $ defaultPythonCode & doc .~ (\_ -> arrayDoc)
                               & shape .~ spinfo

  pythonify c@(ELit (LBag codes) :&: _) = do
    spinfo <- projectShapeCheck c
    let entries = map ($ 0) $ codes ^.. traverse . doc
    let combinedDoc = foldr (\e acc -> e <> comma <+> acc) mempty entries
    let bagDoc = brackets combinedDoc
    return $ defaultPythonCode & doc .~ (\_ -> bagDoc)
                               & shape .~ spinfo

  pythonify c@(EBinop operator lhs rhs :&: pos) = do
    spinfo <- projectShapeCheck c
    code <- pythonifyBinop operator lhs rhs pos
    return $ code & shape .~ spinfo

  pythonify c@(EIndex arr idx :&: _) = do
    spinfo <- projectShapeCheck c
    let arrDoc = (arr ^. doc) 0
        idxDoc = (idx ^. doc) 0
    return $ defaultPythonCode & doc .~ (\_ -> arrDoc <> brackets idxDoc)
                               & shape .~ spinfo

  pythonify c@(EFloat code :&: _) = do
    spinfo <- projectShapeCheck c
    return $ defaultPythonCode & doc .~ (\_ -> text "float" <> parens ((code ^. doc) 0))
                               & shape .~ spinfo

  pythonify c@(EExp code :&: _) = do
    spinfo <- projectShapeCheck c
    let d = (code ^. doc) 0
    return $ defaultPythonCode & doc .~ (\_ -> text "np.exp" <> parens d)
                               & shape .~ spinfo

  pythonify c@(ELog code :&: _) = do
    spinfo <- projectShapeCheck c
    let d = (code ^. doc) 0
    return $ defaultPythonCode & doc .~ (\_ -> text "np.log" <> parens d)
                               & shape .~ spinfo

  pythonify c@(EClip ecode (LFloat f) :&: _) = do
    spinfo <- projectShapeCheck c
    let litDoc = float f
    let eDoc = (ecode ^. doc) 0
    let clipDoc = text "np.clip" <> parens (eDoc <> comma <+> text "-" <> litDoc <> comma <+> litDoc)
    return $ defaultPythonCode & doc .~ (\_ -> clipDoc)
                               & shape .~ spinfo

  pythonify (EClip _ _ :&: p) =
    throwM $ InternalError p "shape checker let through bad clip operation"

  pythonify c@(EScale lcode rcode :&: p) = do
    spinfo <- projectShapeCheck c
    code <- pythonifyBinop MULT lcode rcode p
    return $ code & shape .~ spinfo

  pythonify c@(EDot lcode rcode :&: _) = do
    spinfo <- projectShapeCheck c
    let ldoc = (lcode ^. doc) 0
        rdoc = (rcode ^. doc) 0
        dotDoc = text "np.dot" <> parens (ldoc <> comma <+> rdoc)
    return $ defaultPythonCode & doc .~ (\_ -> dotDoc)
                               & shape .~ spinfo

  pythonify c@(EFst code :&: _) = do
    spinfo <- projectShapeCheck c
    let tupleDoc = (code ^. doc) 0
        fstDoc = tupleDoc <> (brackets $ int 0)
    return $ defaultPythonCode & doc .~ (\_ -> fstDoc)
                               & shape .~ spinfo

  pythonify c@(ESnd code :&: _) = do
    spinfo <- projectShapeCheck c
    let tupleDoc = (code ^. doc) 1
        fstDoc = tupleDoc <> (brackets $ int 1)
    return $ defaultPythonCode & doc .~ (\_ -> fstDoc)
                               & shape .~ spinfo

tauNeedsCopy :: Tau -> Bool
tauNeedsCopy (TArr _ _) = True
tauNeedsCopy (TBag _)   = True
tauNeedsCopy _ = False

isTauNumericArray' :: Bool -> Tau -> Bool
isTauNumericArray' inarray TInt        = inarray
isTauNumericArray' inarray TFloat      = inarray
isTauNumericArray' inarray TBool       = inarray
isTauNumericArray' inarray TAny        = inarray
isTauNumericArray' inarray (TArr t _)  = inarray && (isTauNumericArray' True t)
isTauNumericArray' _       (TBag _)    = False
isTauNumericArray' _       (TProd _ _) = False

isTauNumericArray :: Tau -> Bool
isTauNumericArray (TArr t _) = isTauNumericArray' True t
isTauNumericArray _ = False

dot :: Doc
dot = text "."

defaultValue :: (MonadThrow m) => Tau -> m Doc
defaultValue TInt = return $ int 0
defaultValue TFloat = return $ float 0
defaultValue TBool = return $ text "False"
defaultValue TAny = return $ text "None"
defaultValue tarr@(TArr t (Just len))
  | isTauNumericArray tarr = do
      contents <- replicateM len (defaultValue t)
      let contentDoc = foldr (\c acc -> c <> comma <+> acc) empty contents
      return $ text "np.array" <> (parens . brackets $ contentDoc)
  | otherwise = do
      contents <- replicateM len (defaultValue t)
      return $ brackets $ foldr (\c acc -> c <> comma <+> acc) empty contents
defaultValue t@(TArr _ Nothing)
  | isTauNumericArray t = do
      return $ text "np.array" <> (parens . brackets $ empty)
  | otherwise =
      return $ brackets empty
defaultValue (TBag _) = return $ brackets empty
defaultValue (TProd t1 t2) = do
  t1v <- defaultValue t1
  t2v <- defaultValue t2
  return $ brackets (t1v <> comma <+> t2v)

getArrayOrBagContentTau :: (MonadThrow m) => Tau -> Position -> m Tau
getArrayOrBagContentTau (TArr t _) _ = return t
getArrayOrBagContentTau (TBag t) _ = return t
getArrayOrBagContentTau _ p = throwM $ InternalError p "expecting an array or bag here"

instance Pythonify (Cmd :&: Position) where
  pythonify c@(CAssign lhs rhs :&: p) = callCC $ \ret -> do
    spinfo <- projectShapeCheck c
    let rdoc = (rhs ^. doc) 0
    let maybeLengthVar = projectELength (lhs ^. shape . term) >>= projectEVar
    shapeCxt <- getShapeCxt <$> ask
    let maybeLengthVarTau = maybeLengthVar >>= (\x -> M.lookup x shapeCxt)
    -- x = rhs
    case (maybeLengthVar, maybeLengthVarTau) of
      (Just x, Just t)
        | isTauNumericArray t -> do
            let xShape = text x <> dot <> text "shape"
                newLength = lparen <> rdoc <> comma <> rparen
                newShape = newLength <> text "+" <> xShape <> (brackets $ int 1 <> text ":")
            ret $ defaultPythonCode & doc .~ (\_ -> text x <> dot <> text "resize" <> parens newShape)
                                    & shape .~ spinfo
        | otherwise -> do
            innerTau <- getArrayOrBagContentTau t p
            dvDoc <- defaultValue innerTau
            let assignDoc = text x <+> equals <+>
                            text "resize_bag" <>
                            (parens $ text x <> comma <+> rdoc <> comma <+> dvDoc)
            ret $ defaultPythonCode & doc .~ (\_ -> assignDoc)
                                    & shape .~ spinfo
      _ -> return ()

    case lhs ^? shape . tau of
      Just (TArr _ _) -> do
        let ldoc = (lhs ^. doc) 0
            assignDoc = ldoc <+> equals <+> text "np.array" <> parens rdoc
        return $ defaultPythonCode & doc .~ (\_ -> assignDoc)
                                   & shape .~ spinfo
      Just t
        | tauNeedsCopy t -> do
            let ldoc = (lhs ^. doc) 0
                assignDoc = ldoc <+> equals <+> text "copy.deepcopy" <> parens rdoc
            return $ defaultPythonCode & doc .~ (\_ -> assignDoc)
                                       & shape .~ spinfo
        | otherwise -> do
            let ldoc = (lhs ^. doc) 0
                assignDoc = ldoc <+> equals <+> rdoc
            return $ defaultPythonCode & doc .~ (\_ -> assignDoc)
                                       & shape .~ spinfo

      _ -> traceShow (lhs ^. shape . term) $ throwM $ InternalError p "expecting an expression here"

  pythonify c@(CLaplace lhs width rhs :&: _) = do
    spinfo <- projectShapeCheck c
    let ldoc = (lhs ^. doc) 0
        rdoc = (rhs ^. doc) 0
        lapDoc = ldoc <+> equals <+> text "np.random.laplace" <> parens (rdoc <> comma <+> float width)
    return $ defaultPythonCode & doc   .~ (\_ -> lapDoc)
                               & shape .~ spinfo

  pythonify c@(CIf cond c1 c2 :&: _) = do
    spinfo <- projectShapeCheck c
    let condDoc = (cond ^. doc) 0
        c1Doc = (c1 ^. doc) 0
        c2Doc = (c2 ^. doc) 0
        ifDoc = vcat [text "if" <+> condDoc <> colon,
                      nest 2 c1Doc,
                      text "else" <> colon,
                      nest 2 c2Doc
                     ]
    return $ defaultPythonCode & doc .~ (\_ -> ifDoc)
                               & shape .~ spinfo

  pythonify c@(CWhile cond body :&: _) = do
    spinfo <- projectShapeCheck c
    let condDoc = (cond ^. doc) 0
        bodyDoc = (body ^. doc) 0
        whileDoc = vcat [text "while" <+> condDoc <> colon,
                         nest 2 bodyDoc
                        ]
    return $ defaultPythonCode & doc .~ (\_ -> whileDoc)
                               & shape .~ spinfo

  pythonify c@(CSeq c1 c2 :&: _) = do
    spinfo <- projectShapeCheck c
    let c1Doc = (c1 ^. doc) 0
        c2Doc = (c2 ^. doc) 0
        seqDoc = vcat [c1Doc, c2Doc]
    return $ defaultPythonCode & doc .~ (\_ -> seqDoc)
                               & shape .~ spinfo

  pythonify c@(CSkip :&: _) = do
    spinfo <- projectShapeCheck c
    return $ defaultPythonCode & doc .~ (\_ -> text "pass")
                               & shape .~ spinfo

instance Pythonify (CTCHint :&: Position) where
  pythonify c@(CTCHint extName _ code :&: _) = do
    spinfo <- projectShapeCheck c
    let expDoc = (code ^. doc) 0
        header = text "\"\"\"begin extension hint:" <+> text extName <> text "\"\"\""
        trailer = text "\"\"\"end extension hint:" <+> text extName <> text "\"\"\""
        hintDoc = vcat [header, expDoc, trailer]
    return $ defaultPythonCode & doc .~ (\_ -> hintDoc)
                               & shape .~ spinfo

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

transInputs :: (MonadThrow m) => String -> [Decl] -> m Doc
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

transDecl :: (MonadThrow m) => Decl -> m Doc
transDecl (Decl _ x _ t) = do
  initDoc <- defaultValue t
  return $ text x <+> equals <+> initDoc

transDecls :: (MonadThrow m) => [Decl] -> m Doc
transDecls decls =
  vcat <$> mapM transDecl decls

runPythonify :: String -> [Decl] -> Term ImpTCP -> Either SomeException Doc
runPythonify jsonDataPath decls prog = run $ do
  declsDoc <- transDecls decls
  inputsDoc <- transInputs jsonDataPath decls
  progDoc <- (view doc) <$> cataM pythonify prog
  let pythonCode = vcat [
        imports,
        text "\n",
        prologue,
        text "\n",
        declsDoc,
        text "\n",
        inputsDoc,
        text "\n",
        progDoc 0,
        text "\n"
        ]
  return pythonCode
  where run = (flip runReaderT (declsToShapeCxt decls))
              . (flip runContT return)

        declsToShapeCxt dcls =
          ShapeCxt $ M.fromList (map (\(Decl _ x _ t) -> (x, t)) dcls)
