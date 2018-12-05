module Shape where

import Type.Reflection

import Control.Lens hiding (op)
import Prelude hiding (LT, EQ, GT)
import Data.Maybe
import Data.Comp hiding (inj)
import Data.Comp.Derive
import SyntaxExt
import qualified Data.Map as M

import Control.Monad.Catch
import Control.Monad.Reader

data ShapeCheckError = ExpectTau       Position Tau Tau -- expected type, got type
                     | ExpectArr       Position Tau     -- expected arrays/bags, got this type
                     | ExpectNum       Position Tau     -- expected numeric type, got type
                     | ExpectSmall     Position Tau     -- expected small type, got type
                     | Mismatch        Position [Tau]   -- the mismatching types
                     | UnknownVariable Position Var
                     | ExpectExpr      Position         -- expected expression, got something else
                     | ExpectCmd       Position         -- expected command, got something else
                     | UnsupportedAssign Position
                     | ExpectPositive  Position Float    -- expected positive float, got this
                     | InternalError   Position         -- the impossible happened, a bug!
                     deriving (Show, Eq, Typeable)

instance Exception ShapeCheckError

newtype ShapeCxt = ShapeCxt { getShapeCxt :: M.Map Var Tau }
  deriving (Show, Eq)

data ShapeInfo = ShapeInfo {
  _shapeinfo_is_var     :: Maybe Var
  , _shapeinfo_is_index :: Maybe Var -- the indexed variable
  , _shapeinfo_is_length :: Maybe Var -- the variable of the length expr
  , _shapeinfo_is_expr  :: Maybe Tau
  } deriving (Show, Eq)

$(makeLensesWith underscoreFields ''ShapeInfo)

defaultInfo :: ShapeInfo
defaultInfo = ShapeInfo Nothing Nothing Nothing Nothing

class ShapeCheck f where
  shapeCheck :: forall m.
                ( MonadReader ShapeCxt m
                , MonadThrow           m
                ) => AlgM m f ShapeInfo

$(derive [liftSum] [''ShapeCheck])

instance ShapeCheck (Expr :&: Position) where
  shapeCheck (EVar v :&: p) = do
    cxt <- getShapeCxt <$> ask
    case M.lookup v cxt of
      Nothing -> throwM $ UnknownVariable p v
      Just t -> return $ defaultInfo & is_var  .~ Just v
                                     & is_expr .~ Just t
  shapeCheck (ELength info :&: p) =
    case (info ^. is_expr, info ^. is_var) of
      (Just (TArr _ _), Just x) -> return $ defaultInfo & is_expr .~ (Just TInt)
                                                        & is_length .~ (Just x)
      (Just (TArr _ _), _)      -> return $ defaultInfo & is_expr .~ (Just TInt)
      (Just (TBag _), Just x)   -> return $ defaultInfo & is_expr .~ (Just TInt)
                                                        & is_length .~ (Just x)
      (Just (TBag _), _)        -> return $ defaultInfo & is_expr .~ (Just TInt)
      (Just t, _)               -> throwM $ ExpectArr p t
      _                         -> throwM $ ExpectExpr p
  shapeCheck (ELit (LInt _) :&: _) =
    return $ defaultInfo & is_expr .~ (Just TInt)
  shapeCheck (ELit (LFloat _) :&: _) =
    return $ defaultInfo & is_expr .~ (Just TFloat)
  shapeCheck (ELit (LBool _) :&: _) =
    return $ defaultInfo & is_expr .~ (Just TBool)
  shapeCheck (ELit (LArr []) :&: _) =
    return $ defaultInfo & is_expr .~ (Just (TArr TAny (Just 0)))
  shapeCheck (ELit (LArr ((map (view is_expr)) -> arr@(t:_))) :&: p) =
    if all isJust arr
    then let arr' = map fromJust arr
             t'   = fromJust t
         in if all (== t') arr'
            then return $ defaultInfo & is_expr .~ Just (TArr t' (Just $ length arr'))
            else throwM $ Mismatch p arr'
    else throwM $ ExpectExpr p
  shapeCheck (ELit (LBag []) :&: _) =
    return $ defaultInfo & is_expr .~ Just (TBag TAny)
  shapeCheck (ELit (LBag ((map (view is_expr)) -> arr@(t:_))) :&: p) =
    if all isJust arr
    then let arr' = map fromJust arr
             t'   = fromJust t
         in if all (== t') arr'
            then return $ defaultInfo & is_expr .~ Just (TBag t')
            else throwM $ Mismatch p arr'
    else throwM $ ExpectExpr p
  shapeCheck (EBinop op ((view is_expr) -> Just t1) ((view is_expr) -> Just t2) :&: p)
    | op == PLUS || op == MINUS || op == MULT || op == DIV = do
        case (t1, t2) of
          (TInt,   TInt)   -> return $ defaultInfo & is_expr .~ Just TInt
          (TFloat, TFloat) -> return $ defaultInfo & is_expr .~ Just TFloat
          _ | t1 == t2  -> throwM $ ExpectNum p t1
            | otherwise -> throwM $ Mismatch p [t1, t2]
    | op == AND || op == OR = do
        case (t1, t2) of
          (TBool, TBool) -> return $ defaultInfo & is_expr .~ Just TBool
          _ | t1 == t2   -> throwM $ ExpectTau p TBool t1
            | otherwise  -> throwM $ Mismatch p [t1, t2]
    | op == LT || op == EQ || op == GT || op == GE = do
        case (t1, t2) of
          (TInt,   TInt)   -> return $ defaultInfo & is_expr .~ Just TBool
          (TFloat, TFloat) -> return $ defaultInfo & is_expr .~ Just TBool
          _ | t1 == t2  -> throwM $ ExpectNum p t1
            | otherwise -> throwM $ Mismatch p [t1, t2]
    | op == EQ || op == NEQ = do
        case (t1, t2) of
          (TInt,   TInt)   -> return $ defaultInfo & is_expr .~ Just TBool
          (TFloat, TFloat) -> return $ defaultInfo & is_expr .~ Just TBool
          (TBool,  TBool)  -> return $ defaultInfo & is_expr .~ Just TBool
          _ | t1 == t2  -> throwM $ ExpectSmall p t1
            | otherwise -> throwM $ Mismatch p [t1, t2]
  shapeCheck (EIndex info ((view is_expr) -> Just t2) :&: p) = do
    case (info ^. is_expr, t2) of
      (Just (TArr t _), TInt) ->
        case info ^. is_var of
          Just v ->
            return $ defaultInfo & is_expr  .~ Just t
                                 & is_index .~ Just v
          Nothing ->
            return $ defaultInfo & is_expr .~ Just t
      (Just (TBag t),   TInt) ->
        case info ^. is_var of
          Just v ->
            return $ defaultInfo & is_expr  .~ Just t
                                 & is_index .~ Just v
          Nothing ->
            return $ defaultInfo & is_expr .~ Just t
      (Just t1,         TInt) -> throwM $ ExpectArr p t1
      (Just _,          _   ) -> throwM $ ExpectTau p TInt t2
      (_,               _   ) -> throwM $ ExpectExpr p
  shapeCheck (EFloat ((view is_expr) -> Just t) :&: p) = do
    case t of
      TInt -> return $ defaultInfo & is_expr .~ Just TFloat
      _    -> throwM $ ExpectTau p TInt t
  shapeCheck (EExp ((view is_expr) -> Just t) :&: p) = do
    case t of
      TFloat -> return $ defaultInfo & is_expr .~ Just TFloat
      _      -> throwM $ ExpectTau p TFloat t
  shapeCheck (ELog ((view is_expr) -> Just t) :&: p) = do
    case t of
      TFloat -> return $ defaultInfo & is_expr .~ Just TFloat
      _      -> throwM $ ExpectTau p TFloat t
  shapeCheck (EClip ((view is_expr) -> Just TInt) (LInt _) :&: _) = do
    return $ defaultInfo & is_expr .~ Just TInt
  shapeCheck (EClip ((view is_expr) -> Just TFloat) (LFloat _) :&: _) = do
    return $ defaultInfo & is_expr .~ Just TFloat
  shapeCheck (EClip ((view is_expr) -> Just t) _ :&: p) =
    throwM $ ExpectNum p t
  shapeCheck (EScale ((view is_expr) -> Just tscalar) ((view is_expr) -> Just tvector) :&: p) = do
    case (tscalar, tvector) of
      (TFloat, TArr TFloat _) -> return $ defaultInfo & is_expr .~ Just tvector
      (_,      TArr TFloat _) -> throwM $ ExpectTau p TFloat tscalar
      (_,      _            ) -> throwM $ ExpectTau p (TArr TFloat Nothing) tvector
  shapeCheck (EDot ((view is_expr) -> Just tv1) ((view is_expr) -> Just tv2) :&: p) = do
    case (tv1, tv2) of
      (TArr TFloat (Just len1), TArr TFloat (Just len2))
        | len1 == len2 -> return $ defaultInfo & is_expr .~ Just TFloat
      _ -> throwM $ Mismatch p [tv1, tv2]
  shapeCheck (_ :&: p) =
    throwM $ ExpectExpr p

instance ShapeCheck (Cmd :&: Position) where
  shapeCheck (CAssign linfo (view is_expr -> Just rt) :&: p) = do
    case (linfo ^. is_var, linfo ^. is_index, linfo ^. is_expr) of
      (Just _, _, Just lt)
        | lt == rt  -> return defaultInfo
        | otherwise -> throwM $ Mismatch p [lt, rt]
      (_, Just _, Just lt)
        | lt == rt -> return defaultInfo
        | otherwise -> throwM $ Mismatch p [lt, rt]
      (Nothing, Nothing, Just _) ->
        throwM $ UnsupportedAssign p
      (_, _, Nothing) ->
        throwM $ ExpectExpr p
  shapeCheck (CLaplace linfo w (view is_expr -> Just rt) :&: p) = do
    when (w <= 0) $ do
      throwM $ ExpectPositive p w
    case (linfo ^. is_var, linfo ^. is_expr) of
      (Just _, Just lt)
        | lt == rt -> return defaultInfo
        | otherwise -> throwM $ Mismatch p [lt, rt]
      (Just _, Nothing) ->
        throwM $ InternalError p
      (Nothing, Just _) ->
        throwM $ UnsupportedAssign p
      (Nothing, Nothing) ->
        throwM $ ExpectExpr p
  shapeCheck (CIf (view is_expr -> Just et) cinfo1 cinfo2 :&: p) = do
    case (et, cinfo1 ^. is_expr, cinfo2 ^. is_expr) of
      (TBool, Nothing, Nothing) -> return defaultInfo
      (_,     Nothing, Nothing) -> throwM $ ExpectTau p TBool et
      (_,     Just _,  _      ) -> throwM $ ExpectCmd p
      (_,     _, Just  _      ) -> throwM $ ExpectCmd p
  shapeCheck (CWhile (view is_expr -> Just et) cinfo :&: p) = do
    case (et, cinfo ^. is_expr) of
      (TBool, Nothing) -> return defaultInfo
      (_,     Nothing) -> throwM $ ExpectTau p TBool et
      (_,     Just _ ) -> throwM $ ExpectCmd p
  shapeCheck (CSeq cinfo1 cinfo2 :&: p) = do
    case (cinfo1 ^. is_expr, cinfo2 ^. is_expr) of
      (Nothing, Nothing) -> return defaultInfo
      _                  -> throwM $ ExpectCmd p
  shapeCheck (CSkip :&: _) = return defaultInfo
  shapeCheck (_ :&: p) = throwM $ ExpectExpr p

instance ShapeCheck (CTCHint :&: Position) where
  shapeCheck (CTCHint _ _ info :&: _) = return info

shapeImpTCP :: forall m.
               ( MonadReader ShapeCxt m
               , MonadThrow           m
               ) => AlgM m ImpTCP ShapeInfo
shapeImpTCP = shapeCheck
