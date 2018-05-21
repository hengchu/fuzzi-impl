module Interp where

import Syntax
import Control.Exception
import Data.Map
import Data.Map as M
import Prelude hiding (LT, GT, EQ)

type Label = String
type Var   = String

type RecValue = Map Label Value

data InterpException = FuzziUnknownVariable String
                     | FuzziUninitializedValue
                     | FuzziTypeError
                     | FuzziIndexOutOfBound
                     | FuzziUndesugaredMacro
                     deriving (Show)

data PosnInterpException = MkException Position InterpException
  deriving (Show)

instance Exception InterpException
instance Exception PosnInterpException

data Value = VInt     Int
           | VFloat   Float
           | VBool    Bool
           | VRec     RecValue
           | VArr Int [Value]
           | VBag Int [Value]
  deriving (Show, Eq)

type Memory = Map Var Value

interpSmallLit :: SmallLit -> Value
interpSmallLit (SILit i) = VInt i
interpSmallLit (SFLit f) = VFloat f
interpSmallLit (SBLit b) = VBool b

interpLiteral :: Literal -> Value
interpLiteral (SLit s) = interpSmallLit s
interpLiteral (RLit r) = VRec $ M.map interpSmallLit . getRowLits $ r

interpOp :: Position -> Binop -> Value -> Value -> Value
interpOp posn op vlhs vrhs =
  case op of
    LT ->  VBool $ caseAnalysis (<) (<) typeError vlhs vrhs
    LE ->  VBool $ caseAnalysis (<=) (<=) typeError vlhs vrhs
    GT ->  VBool $ caseAnalysis (>) (>) typeError vlhs vrhs
    GE ->  VBool $ caseAnalysis (>=) (>=) typeError vlhs vrhs
    AND -> VBool $ caseAnalysis typeError typeError (&&) vlhs vrhs
    OR ->  VBool $ caseAnalysis typeError typeError  (||) vlhs vrhs
    EQ ->  VBool $ caseAnalysis (==) (==) (==) vlhs vrhs
    NEQ -> VBool $ caseAnalysis (/=) (/=) (/=) vlhs vrhs
    PLUS -> case vlhs of
      VInt _ -> VInt $ caseAnalysis (+) typeError typeError vlhs vrhs
      VFloat _ -> VFloat $ caseAnalysis typeError (+) typeError vlhs vrhs
      _ -> throw . MkException posn $ typeError
    MINUS -> case vlhs of
      VInt _ -> VInt $ caseAnalysis (-) typeError typeError vlhs vrhs
      VFloat _ -> VFloat $ caseAnalysis typeError (-) typeError vlhs vrhs
      _ -> throw . MkException posn $ typeError
    MULT -> case vlhs of
      VInt _ -> VInt $ caseAnalysis (*) typeError typeError vlhs vrhs
      VFloat _ -> VFloat $ caseAnalysis typeError (*) typeError vlhs vrhs
      _ -> throw . MkException posn $ typeError
    DIV -> case vlhs of
      VInt _ -> VInt $ caseAnalysis div typeError typeError vlhs vrhs
      VFloat _ -> VFloat $ caseAnalysis typeError (/) typeError vlhs vrhs
      _ -> throw . MkException posn $ typeError
  where caseAnalysis :: (Int -> Int -> a)
                     -> (Float -> Float -> a)
                     -> (Bool -> Bool -> a)
                     -> Value
                     -> Value
                     -> a
        caseAnalysis fint ffloat fbool v1 v2 =
          case (v1, v2) of
            (VInt i1, VInt i2) -> fint i1 i2
            (VFloat f1, VFloat f2) -> ffloat f1 f2
            (VBool b1, VBool b2) -> fbool b1 b2
            (_, _) -> throw . MkException posn $ FuzziTypeError

        typeError = throw . MkException posn $ FuzziTypeError

interpExpr :: Expr -> Memory -> Value
interpExpr (EVar posn x) m =
  case M.lookup x m of
    Nothing -> throw . MkException posn . FuzziUnknownVariable $ x
    Just v  -> v
interpExpr (ELength posn e) m =
  case interpExpr e m of
    VArr len _ -> VInt len
    VBag len _ -> VInt len
    _ -> throw $ MkException posn FuzziTypeError
interpExpr (ELit _ lit) _ = interpLiteral lit
interpExpr (EBinop posn lhs op rhs) m =
  let vlhs = interpExpr lhs m
      vrhs = interpExpr rhs m
  in interpOp posn op vlhs vrhs
interpExpr (EIndex posn earr eidx) m =
  case (interpExpr earr m, interpExpr eidx m) of
    (VArr len vs, VInt idx) -> index len vs idx
    (VBag len vs, VInt idx) -> index len vs idx
    _ -> throw . MkException posn $ FuzziTypeError
  where index len vs idx =
          if idx >= len
          then throw . MkException posn $ FuzziIndexOutOfBound
          else case safeListIndex vs idx of
                 Nothing -> throw . MkException posn $ FuzziUninitializedValue
                 Just v -> v
interpExpr (ERUpdate posn e label ev) m =
  case interpExpr e m of
    VRec vrec -> VRec $ M.insert label (interpExpr ev m) vrec
    _ -> throw . MkException posn $ FuzziTypeError
interpExpr (ERAccess posn e label) m =
  case interpExpr e m of
    VRec vrec ->
      case M.lookup label vrec of
        Nothing -> throw . MkException posn $ FuzziTypeError
        Just v -> v
    _ -> throw . MkException posn $ FuzziTypeError
interpExpr (EClip posn e lit) m =
  clip (interpExpr e m) (interpLiteral lit)
  where
    clip (VInt i)   (VInt b)   = VInt   $ if abs i <= abs b then i else signum i * abs b
    clip (VFloat f) (VFloat b) = VFloat $ if abs f <= abs b then f else signum f * abs b
    clip (VRec r)   (VRec b)   = VRec   $ M.unionWith clip r b
    clip _          _          = throw . MkException posn $ FuzziTypeError

safeListIndex :: [a] -> Int -> Maybe a
safeListIndex []     _ = Nothing
safeListIndex (x:_)  0 = Just x
safeListIndex (_:xs) n = safeListIndex xs (n-1)

setList :: Position -> [a] -> Int -> a -> [a]
setList posn xs idx v = go xs idx []
  where go _      n _ | n < 0 = throw . MkException posn $ FuzziIndexOutOfBound
        go (_:vs) 0 acc = reverse acc ++ (v:vs)
        go (x:vs) n acc = go vs (n-1) (x:acc)
        go []     n acc = go [] (n-1) (unInitExc:acc)

        unInitExc = throw . MkException posn $ FuzziUninitializedValue

interpLhsExpr :: Expr -> Value -> Memory -> Memory
interpLhsExpr (EVar _ x) = M.insert x
interpLhsExpr (EIndex posn evar@(EVar _ x) eidx) = \v m ->
  case (interpExpr evar m, interpExpr eidx m) of
    (VArr len vs, VInt idx) ->
      let varr = VArr len (setList posn vs idx v)
      in M.insert x varr m
    (VBag len vs, VInt idx) ->
      let vbag = VBag len (setList posn vs idx v)
      in M.insert x vbag m
    _ -> throw . MkException posn $ FuzziTypeError
interpLhsExpr (EIndex posn e eidx) =
  let updateF = interpLhsExpr e
  in \v m ->
    let v' = case (interpExpr e m, interpExpr eidx m) of
               (VArr len vs, VInt idx) -> VArr len (setList posn vs idx v)
               (VBag len vs, VInt idx) -> VBag len (setList posn vs idx v)
               _ -> throw . MkException posn $ FuzziTypeError
    in updateF v' m
interpLhsExpr (ELength posn e) =
  let updateF = interpLhsExpr e
  in \v m ->
    let v' = case (interpExpr e m, v) of
               (VArr _ vs, VInt newLen) -> VArr newLen vs
               (VBag _ vs, VInt newLen) -> VBag newLen vs
               _ -> throw . MkException posn $ FuzziTypeError
    in updateF v' m
interpLhsExpr e = throw . MkException (exprPosn e) $ FuzziTypeError

interp :: Cmd -> Memory -> Memory
interp (CAssign _ x e) = \m -> M.insert x (interpExpr e m) m
interp (CAUpdate posn earr eidx erhs) =
  \m -> interpLhsExpr (EIndex posn earr eidx) (interpExpr erhs m) m
interp (CLUpdate posn earr erhs) =
  \m -> interpLhsExpr (ELength posn earr) (interpExpr erhs m) m
interp (CLaplace _ x _ e) = \m -> M.insert x (interpExpr e m) m
interp (CIf posn e ct cf) = \m ->
  case interpExpr e m of
    VBool b -> if b then interp ct m else interp cf m
    _ -> throw . MkException posn $ FuzziTypeError
interp c@(CWhile posn e cbody) =
  interp $ CIf posn e (CSeq posn cbody c) (CSkip posn)
interp (CDecl _ _ _ _) = id
interp (CSeq _ c1 c2) = interp c1 . interp c2
interp (CSkip _) = id
interp c = throw . MkException (cmdPosn c) $ FuzziUndesugaredMacro
