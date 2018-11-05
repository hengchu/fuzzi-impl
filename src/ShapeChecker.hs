module ShapeChecker where

import Prelude hiding (LT, GT, EQ)
import Pretty
import GHC.Generics
import Control.Lens ((^.), at)
import Syntax
import qualified Data.Map as M

import Control.Monad.Reader
import Control.Monad.Except

newtype Context = Context { getContext :: M.Map Var Tau }
  deriving (Show, Eq)
data ShapeError = ShapeError {
  shape_error_position :: Position
  , shape_error_message :: String
  } deriving (Show, Eq, Generic)

class Get c r where
  get :: c -> r

class Inj c r where
  inj :: r -> c

instance Get Context Context where
  get = id

instance Inj ShapeError ShapeError where
  inj = id

shapeFail :: ( Inj e ShapeError
             , MonadError e m
             )
          => Position -> String -> m a
shapeFail p msg = throwError . inj $ ShapeError p msg

askCtxt :: ( Get c Context
           , MonadReader c m
           )
        => m (M.Map Var Tau)
askCtxt = getContext . get <$> ask

isSubRec :: M.Map Var Tau -> M.Map Var Tau -> Bool
isSubRec m1 m2 =
  M.foldrWithKey
    (\label t1 acc ->
       case m2 ^. (at label) of
         Nothing -> False
         Just t2 -> (isCompat t1 t2) && acc
    )
    True
    m1

-- |Is t1 <: t2? AKA, is t1 more specific than t2
isCompat :: Tau -> Tau -> Bool
isCompat (TArr t1 (Just len1)) (TArr t2 (Just len2)) =
  isCompat t1 t2 && len1 == len2
isCompat (TArr t1 (Just _)) (TArr t2 _) =
  isCompat t1 t2
isCompat (TBag t1) (TBag t2) = isCompat t1 t2
isCompat (TRec m1) (TRec m2) = isSubRec m2 m1
isCompat _  TAny = True
isCompat t1 t2   = t1 == t2

litAllCompat :: ( Inj e ShapeError
                , MonadError e m)
             => Position -> [Tau] -> m Tau
litAllCompat p [] = shapeFail p $ "Expecting at least 1 type"
litAllCompat _ (t:[]) = return t
litAllCompat p (t1:t2:more) =
  if t1 `isCompat` t2
  then litAllCompat p (t1:more)
  else shapeFail p $ "Not compatible shapes in literal"

checkLit :: ( Get c Context
            , MonadReader c m
            , Inj e ShapeError
            , MonadError e m)
         => Position -> Lit -> m Tau
checkLit _ (LInt _) = return TInt
checkLit _ (LFloat _) = return TFloat
checkLit _ (LBool _) = return TBool
checkLit _ (LArr []) = return (TArr TAny (Just 0))
checkLit p (LArr es) = do
  ts <- mapM checkExpr es
  t <- litAllCompat p ts
  return $ TArr t (Just $ length es)
checkLit _ (LBag []) = return $ TBag TAny
checkLit p (LBag es) = do
  ts <- mapM checkExpr es
  t <- litAllCompat p ts
  return $ TBag t

isBoolOp :: Binop -> Bool
isBoolOp AND = True
isBoolOp OR  = True
isBoolOp _   = False

isOrdOp :: Binop -> Bool
isOrdOp LT = True
isOrdOp LE = True
isOrdOp GT = True
isOrdOp GE = True
isOrdOp _ = False

isEqOp :: Binop -> Bool
isEqOp EQ = True
isEqOp NEQ = True
isEqOp _ = False

isArithOp :: Binop -> Bool
isArithOp PLUS = True
isArithOp MINUS = True
isArithOp MULT = True
isArithOp DIV = True
isArithOp _ = False

checkExpr :: ( Get c Context
             , MonadReader c m
             , Inj e ShapeError
             , MonadError e m)
          => Expr -> m Tau
checkExpr (EVar p x) = do
  ctx <- askCtxt
  case ctx ^. (at x) of
    Nothing -> shapeFail p $ "Unknown variable: " ++ x
    Just t -> return t
checkExpr (ELength p e) = do
  te <- checkExpr e
  case te of
    TArr _ _ -> return TInt
    TBag _ -> return TInt
    _ -> shapeFail p
           $ "expr " ++ show (PrettyExpr e) ++ " is neither bag nor array"
checkExpr (ELit p lit) = checkLit p lit
checkExpr e@(EBinop p e1 op e2)
  | isBoolOp op = do
      t1 <- checkExpr e1
      t2 <- checkExpr e2
      case (t1, t2) of
        (TBool, TBool) -> return TBool
        (_, TBool) -> shapeFail p
                      $ "expr " ++ show (PrettyExpr e1) ++ " is not boolean"
        (TBool, _) -> shapeFail p
                      $ "expr " ++ show (PrettyExpr e2) ++ " is not boolean"
        _ -> shapeFail p
             $ "exprs " ++ show (PrettyExpr e1) ++ ", "
                        ++ show (PrettyExpr e2) ++ " are not boolean"
  | isOrdOp op = do
      t1 <- checkExpr e1
      t2 <- checkExpr e2
      when (t1 /= t2) $ do
        shapeFail p
          $ "binop expr " ++ show (PrettyExpr e) ++ "'s two sides do not match in shape"
      case t1 of
        TInt -> return TBool
        TFloat -> return TBool
        _ -> shapeFail p
          $ "binop expr " ++ show (PrettyExpr e) ++ " compares non-ordered shapes"
  | isEqOp op = do
      t1 <- checkExpr e1
      t2 <- checkExpr e2
      when (t1 /= t2) $ do
        shapeFail p
          $ "binop expr " ++ show (PrettyExpr e) ++ "'s two sides do not match in shape"
      return TBool
  | isArithOp op = do
      t1 <- checkExpr e1
      t2 <- checkExpr e2
      when (t1 /= t2) $ do
        shapeFail p
          $ "binop expr " ++ show (PrettyExpr e) ++ "'s two sides do not match in shape"
      case t1 of
        TInt -> return TInt
        TFloat -> return TFloat
        _ -> shapeFail p $
             "binop expr " ++ show (PrettyExpr e)
                           ++ " tries to perform arith on non-arith shapes"
  | otherwise = shapeFail p $ "Unknown operator: " ++ show op
checkExpr e@(EIndex p e1 e2) = do
  t1 <- checkExpr e1
  t2 <- checkExpr e2
  case (t1, t2) of
    (TArr t _, TInt) -> return t
    (TBag t, TInt) -> return t
    (_, TInt) -> shapeFail p
                 $ "expr " ++ show (PrettyExpr e)
                           ++ " indexes non-array/bag shape"
    (_, _) -> shapeFail p
              $ "expr " ++ show (PrettyExpr e)
                        ++ "'s index is not int"
checkExpr (ERAccess p e1 label) = do
  t <- checkExpr e1
  case t of
    TRec recty ->
      case recty ^. (at label) of
        Just tlabel -> return tlabel
        Nothing -> shapeFail p
                   $ "expr " ++ show (PrettyExpr e1)
                             ++ " does not have field: " ++ label
    _ -> shapeFail p $ "expr " ++ show (PrettyExpr e1)
                               ++ " is not a record shape"
checkExpr (EFloat p e) = do
  t <- checkExpr e
  case t of
    TInt -> return TFloat
    _ -> shapeFail p $ "expr " ++ show (PrettyExpr e) ++ " is not int"
checkExpr (EExp p e) = do
  t <- checkExpr e
  case t of
    TFloat -> return TFloat
    _ -> shapeFail p $ "expr " ++ show (PrettyExpr e) ++ " is not float"
checkExpr (ELog p e) = do
  t <- checkExpr e
  case t of
    TFloat -> return TFloat
    _ -> shapeFail p $ "expr " ++ show (PrettyExpr e) ++ " is not float"
checkExpr eclip@(EClip p e l) = do
  te <- checkExpr e
  tl <- checkLit p l
  case (te, tl) of
    (TFloat, TFloat) -> return TFloat
    (_, TFloat) -> shapeFail p $ "expr " ++ show (PrettyExpr e) ++ " is not float"
    (_, _) -> shapeFail p $ "expr " ++ show (PrettyExpr eclip) ++ " has bad operands"
checkExpr escale@(EScale p scalar vector) = do
  tscalar <- checkExpr scalar
  tvector <- checkExpr vector
  case (tscalar, isFloatArr tvector) of
    (TFloat, True) -> return tvector
    (_, True) -> shapeFail p $ "expr " ++ show (PrettyExpr escale) ++ "'s scalar is not float"
    _ -> shapeFail p $ "expr " ++ show (PrettyExpr escale) ++ " has bad operands"
  where isFloatArr :: Tau -> Bool
        isFloatArr TFloat = True
        isFloatArr (TArr t _) = isFloatArr t
        isFloatArr _ = False
checkExpr edot@(EDot p e1 e2) = do
  t1 <- checkExpr e1
  t2 <- checkExpr e2
  case (t1, t2) of
    (TArr TFloat (Just len1),
     TArr TFloat (Just len2)) | len1 == len2 -> return TFloat
    _ -> shapeFail p $ "expr " ++ show (PrettyExpr edot) ++ " cannot be dot-producted"

checkCmd :: ( Get c Context
            , MonadReader c m
            , Inj e ShapeError
            , MonadError e m)
         => Cmd -> m ()
checkCmd c@(CAssign p (EVar _ x) e) = do
  ctx <- askCtxt
  case ctx ^. (at x) of
    Nothing -> shapeFail p $ "assignment to unknown variable: " ++ x
    Just t -> do
      te <- checkExpr e
      when (not $ isCompat t te) $ do
        shapeFail p $ "incompatible assignment: " ++ show (PrettyCmd c)
                                                  ++ " tlhs = " ++ show t
                                                  ++ ", trhs = " ++ show te
checkCmd c@(CAssign p elhs@(EIndex _ (EVar _ _) _) erhs) = do
  tlhs <- checkExpr elhs
  trhs <- checkExpr erhs
  when (not $ isCompat trhs tlhs) $ do
    shapeFail p $ "incompatible assignment: " ++ show (PrettyCmd c)
checkCmd (CAssign p elhs@(ELength _ (EVar _ _)) erhs) = do
  tlhs <- checkExpr elhs
  trhs <- checkExpr erhs
  case (tlhs, trhs) of
    (TInt, TInt) -> return ()
    _ -> shapeFail p $ "length assignment with incompatible types"
checkCmd c@(CAssign p _ _) =
  shapeFail p $ "unsupported assignment form: " ++ show (PrettyCmd c)
checkCmd c@(CLaplace p x f e) = do
  when (f <= 0) $ do
    shapeFail p $ "laplace width must be positive"
  t <- checkExpr e
  when (t /= TFloat) $ do
    shapeFail p $ "laplace center must be float"
  ctx <- askCtxt
  case ctx ^. (at x) of
    Nothing -> shapeFail p $ "unknow variable in laplace: " ++ x
    Just TFloat -> return ()
    Just _ ->
      shapeFail p
      $ "laplace mechanism: " ++ show (PrettyCmd c) ++ " to non-float variable"
checkCmd (CIf p e ct cf) = do
  t <- checkExpr e
  when (t /= TBool) $ do
    shapeFail p $ "if condition: " ++ show (PrettyExpr e) ++ " is not boolean"
  checkCmd ct
  checkCmd cf
checkCmd (CWhile p e c) = do
  t <- checkExpr e
  when (t /= TBool) $ do
    shapeFail p $ "while condition: " ++ show (PrettyExpr e) ++ " is not boolean"
  checkCmd c
checkCmd (CSeq _ c1 c2) = do
  checkCmd c1
  checkCmd c2
checkCmd (CSkip _) = return ()
checkCmd c@(CExt p _ _) =
  shapeFail p $ "unexpanded extension: " ++ show (PrettyCmd c)
checkCmd (CBlock _ c) = checkCmd c

declsToContext :: [Decl] -> Context
declsToContext decls = Context . M.fromList $ go decls
  where go [] = []
        go ((Decl _ x _ t):more) = (x, t):(go more)

runShapeChecker :: Prog -> Either ShapeError ()
runShapeChecker (Prog decls c) =
  runExcept $ runReaderT (checkCmd c) ctxt
  where ctxt = declsToContext decls
