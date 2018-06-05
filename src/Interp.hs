{-# LANGUAGE ScopedTypeVariables #-}

module Interp where

import Control.Applicative
import Control.Exception hiding (try)
import Data.Scientific
import Data.Text (unpack, pack)
import Data.Map
import Data.Map as M
import Data.Foldable
import Prelude hiding (LT, GT, EQ)
import Syntax
import qualified Data.Vector as V
import qualified Data.Aeson as J
import qualified Data.HashMap.Strict as HM
import System.IO.Unsafe

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
  deriving (Show, Eq)

type Memory = Map Var Value

newtype JsonMemory = JsonMemory { getJsonMemory :: Memory }
  deriving (Show)

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
    _ -> throw $ MkException posn FuzziTypeError
interpExpr (ELit _ lit) _ = interpLiteral lit
interpExpr (EBinop posn lhs op rhs) m =
  let vlhs = interpExpr lhs m
      vrhs = interpExpr rhs m
  in interpOp posn op vlhs vrhs
interpExpr (EIndex posn earr eidx) m =
  case (interpExpr earr m, interpExpr eidx m) of
    (VArr len vs, VInt idx) -> index len vs idx
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
interpExpr (EArray _ exprs) m =
  let vs = fmap (\e -> interpExpr e m) exprs
  in VArr (length vs) vs
interpExpr (EBag _ exprs) m =
  let vs = fmap (\e -> interpExpr e m) exprs
  in VArr (length vs) vs
interpExpr (EExp posn e) m =
  case interpExpr e m of
    VFloat f -> VFloat (exp f)
    _ -> throw . MkException posn $ FuzziTypeError
interpExpr (EFloat posn e) m =
  case interpExpr e m of
    VInt i -> VFloat . fromIntegral $ i
    _ -> throw . MkException posn $ FuzziTypeError
interpExpr (EClip posn e lit) m =
  clip (interpExpr e m) (interpLiteral lit)
  where
    clip (VInt i)      (VInt b)   = VInt   $ if abs i <= abs b then i else signum i * abs b
    clip (VFloat f)    (VFloat b) = VFloat $ if abs f <= abs b then f else signum f * abs b
    clip (VRec r)      b          = VRec   $ M.map (flip clip b) r
    clip (VArr len vs) b          = VArr len $ fmap (flip clip b) vs
    clip _          _             = throw . MkException posn $ FuzziTypeError
interpExpr (EScale posn escalar evec) m =
  let scalar = interpExpr escalar m
      vec    = interpExpr evec m
  in interpScale scalar vec
  where
    interpScale (VInt sc)   (VInt v)      = VInt $ sc * v
    interpScale (VFloat sc) (VFloat v)    = VFloat $ sc * v
    interpScale sc          (VArr len vs) = VArr len $ fmap (interpScale sc) vs
    interpScale sc          (VRec vs)     = VRec $ M.map (interpScale sc) vs
    interpScale _           _             = throw . MkException posn $ FuzziTypeError
interpExpr (EDot posn evec1 evec2) m =
  let vec1 = interpExpr evec1 m
      vec2 = interpExpr evec2 m
  in interpDot vec1 vec2
  where
    interpDot (VInt v1)   (VInt v2)           = VInt (v1 * v2)
    interpDot (VFloat v1) (VFloat v2)         = VFloat (v1 * v2)
    interpDot (VArr len1 vs1) (VArr len2 vs2) =
      if len1 == 0 || len2 == 0 || len1 /= len2
      then throw . MkException posn $ FuzziIndexOutOfBound
      else let prods = zipWith interpDot vs1 vs2
           in foldr1 (\prod acc -> interpOp posn PLUS prod acc) prods
    interpDot _ _ = throw . MkException posn $ FuzziTypeError

safeListIndex :: [a] -> Int -> Maybe a
safeListIndex []     _ = Nothing
safeListIndex (x:_)  0 = Just x
safeListIndex (_:xs) n = safeListIndex xs (n-1)

setList :: Position -> [a] -> Int -> a -> [a]
setList posn xs idx v = go xs idx []
  where go _      n _ | n < 0 = throw . MkException posn $ FuzziIndexOutOfBound
        go []     0 acc = reverse acc ++ [v]
        go (_:vs) 0 acc = reverse acc ++ (v:vs)
        go (x:vs) n acc = go vs (n-1) (x:acc)
        go []     n acc = go [] (n-1) (unInitExc:acc)

        unInitExc = throw . MkException posn $ FuzziUninitializedValue

isUninitializedValue :: Value -> Bool
isUninitializedValue v = unsafePerformIO $
  (evaluate v >> return False)
  `catch`
  (\(e :: PosnInterpException) ->
     case e of
       MkException _ FuzziUninitializedValue -> return True
       _ -> return False)

interpLhsExpr :: Expr -> Value -> Memory -> Memory
interpLhsExpr (EVar _ x) = M.insert x
interpLhsExpr (EIndex posn evar@(EVar _ x) eidx) = \v m ->
  case (interpExpr evar m, interpExpr eidx m) of
    (VArr len vs, VInt idx) ->
      let varr = VArr len (setList posn vs idx v)
      in M.insert x varr m
    _ -> throw . MkException posn $ FuzziTypeError
interpLhsExpr (EIndex posn e eidx) =
  let updateF = interpLhsExpr e
  in \v m ->
    let v' = case (interpExpr e m, interpExpr eidx m) of
               (VArr len vs, VInt idx) -> VArr len (setList posn vs idx v)
               _ -> throw . MkException posn $ FuzziTypeError
    in updateF v' m
interpLhsExpr (ELength posn e) =
  let updateF = interpLhsExpr e
  in \v m ->
    let v' = case (interpExpr e m, v) of
               (VArr _ vs, VInt newLen) -> VArr newLen vs
               _ -> throw . MkException posn $ FuzziTypeError
    in updateF v' m
interpLhsExpr e = throw . MkException (exprPosn e) $ FuzziTypeError

interp :: Cmd -> Memory -> Memory
interp (CAssign _ x e) = \m -> interpLhsExpr x (interpExpr e m) m
interp (CLaplace _ x _ e) = \m -> M.insert x (interpExpr e m) m
interp (CIf posn e ct cf) = \m ->
  case interpExpr e m of
    VBool b -> if b then interp ct m else interp cf m
    _ -> throw . MkException posn $ FuzziTypeError
interp c@(CWhile posn e cbody) =
  interp $ CIf posn e (CSeq posn cbody c) (CSkip posn)
interp (CDecl _ _ _ _) = id
interp (CSeq _ c1 c2) = interp c2 . interp c1
interp (CSkip _) = id
interp c = throw . MkException (cmdPosn c) $ FuzziUndesugaredMacro

instance J.FromJSON Value where
  parseJSON v = pNum <|> pBool <|> pRec <|> pArr
    where pNum = J.withScientific
                   "Number"
                   (\num ->
                      case floatingOrInteger num of
                        Right i -> pure . VInt $ i
                        Left f -> pure . VFloat $ f)
                   v

          pBool = J.withBool
                    "Bool"
                    (\b -> pure . VBool $ b)
                    v

          pRec = VRec <$>
                   J.withObject
                     "Record"
                     (HM.foldrWithKey
                        (\key val acc ->
                            M.insert (unpack key) <$> (J.parseJSON val) <*> acc
                        )
                        (pure M.empty)
                     )
                     v

          pArr = J.withArray
                   "Array"
                   (\arr -> do
                       vals <- foldrM (\val acc -> do
                                          val' <- J.parseJSON val
                                          return $ val':acc)
                                      []
                                      arr
                       return $ VArr (length vals) vals)
                   v

instance J.ToJSON Value where
  toJSON (VInt i)    = J.Number . fromIntegral $ i
  toJSON (VFloat f)  = J.Number . realToFrac $ f
  toJSON (VBool b)   = J.Bool b
  toJSON (VArr _ vs) = J.Array . V.fromList $ fmap J.toJSON vs
  toJSON (VRec r)    = J.Object $
                         M.foldrWithKey
                           (\k v -> HM.insert (pack k) (J.toJSON v))
                           HM.empty
                           r

instance J.FromJSON JsonMemory where
  parseJSON v = JsonMemory <$>
                  J.withObject
                    "Memory"
                    (HM.foldrWithKey
                       (\k val acc ->
                           M.insert (unpack k) <$> J.parseJSON val <*> acc)
                       (pure M.empty)
                    )
                    v

instance J.ToJSON JsonMemory where
  toJSON (JsonMemory m) = J.Object $
                            M.foldrWithKey
                              (\k v -> HM.insert (pack k) (J.toJSON v))
                              HM.empty
                              m
