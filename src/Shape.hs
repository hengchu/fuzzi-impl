module Shape where

import Type.Reflection

import Control.Lens hiding (op)
import Prelude hiding (LT, EQ, GT)
import Data.Maybe
import Data.Comp
import Data.Comp.Derive
import SyntaxExt
import qualified Data.Map as M

import Control.Monad.Cont
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

data ShapeInfo =
  EShapeInfo { _shapeinfo_term :: Term ImpTCP
             , _shapeinfo_tau :: Tau }
  | CShapeInfo { _shapinfo_term :: Term ImpTCP }
  deriving (Show, Eq)

$(makeLensesWith underscoreFields ''ShapeInfo)
$(makePrisms ''ShapeInfo)

defaultEInfo :: ShapeInfo
defaultEInfo = EShapeInfo (iAELit (Position 0 0) (LInt 0)) TAny

defaultCInfo :: ShapeInfo
defaultCInfo = CShapeInfo (iACSkip (Position 0 0))

class ShapeCheck f where
  shapeCheck :: forall m.
                ( MonadReader ShapeCxt m
                , MonadThrow           m
                , MonadCont            m
                ) => AlgM m f ShapeInfo

$(derive [liftSum] [''ShapeCheck])

instance ShapeCheck (Expr :&: Position) where
  shapeCheck (EVar v :&: p) = do
    cxt <- getShapeCxt <$> ask
    case M.lookup v cxt of
      Nothing -> throwM $ UnknownVariable p v
      Just t -> return $ defaultEInfo & tau .~ t
                                      & term .~ (iAEVar p v)
  shapeCheck (ELength info :&: p) =
    case info ^? tau of
      Just (TArr _ _) -> return $ defaultEInfo & tau .~ TInt
                                        & term .~ (iAELength p (info ^. term))
      Just (TBag _)   -> return $ defaultEInfo & tau .~ TInt
                                        & term .~ (iAELength p (info ^. term))
      Just t          -> throwM $ ExpectArr p t
      Nothing         -> throwM $ ExpectExpr p
  shapeCheck (ELit (LInt v) :&: p) =
    return $ defaultEInfo & tau .~ TInt
                          & term .~ (iAELit p (LInt v))
  shapeCheck (ELit (LFloat v) :&: p) =
    return $ defaultEInfo & tau .~ TFloat
                          & term .~ (iAELit p (LFloat v))
  shapeCheck (ELit (LBool b) :&: p) =
    return $ defaultEInfo & tau .~ TBool
                          & term .~ (iAELit p (LBool b))
  shapeCheck (ELit (LArr infos) :&: p) =
    if all (isJust . (preview tau)) infos
    then case infos ^.. traverse . tau of
           t:ts
             | all (== t) ts ->
               return $ defaultEInfo & tau  .~ TArr t (Just (length infos))
                                     & term .~ (iAELit p (LArr $ infos ^.. traverse . term))
             | otherwise ->
               throwM $ Mismatch p (t:ts)
           [] -> return $ defaultEInfo & tau .~ TArr TAny (Just 0)
                                       & term .~ (iAELit p (LArr []))
    else throwM $ ExpectExpr p
  shapeCheck (ELit (LBag infos) :&: p) =
    if all (isJust . (preview tau)) infos
    then case infos ^.. traverse . tau of
           t:ts
             | all (== t) ts ->
               return $ defaultEInfo & tau  .~ TBag t
                                     & term .~ (iAELit p (LBag $ infos ^.. traverse . term))
             | otherwise ->
               throwM $ Mismatch p (t:ts)
           [] -> return $ defaultEInfo & tau .~ TBag TAny
                                       & term .~ (iAELit p (LBag []))
    else throwM $ ExpectExpr p

  shapeCheck (EBinop op linfo@((preview tau) -> Just t1) rinfo@((preview tau) -> Just t2) :&: p)
    | op == PLUS || op == MINUS || op == MULT || op == DIV = do
        case (t1, t2) of
          (TInt,   TInt) ->
            return $ defaultEInfo & tau .~ TInt
                                  & term .~ (iAEBinop p op (linfo ^. term) (rinfo ^. term))
          (TFloat, TFloat) ->
            return $ defaultEInfo & tau .~ TFloat
                                  & term .~ (iAEBinop p op (linfo ^. term) (rinfo ^. term))
          _ | t1 == t2  -> throwM $ ExpectNum p t1
            | otherwise -> throwM $ Mismatch p [t1, t2]
    | op == AND || op == OR = do
        case (t1, t2) of
          (TBool, TBool) ->
            return $ defaultEInfo & tau .~ TBool
                                  & term .~ (iAEBinop p op (linfo ^. term) (rinfo ^. term))
          _ | t1 == t2   -> throwM $ ExpectTau p TBool t1
            | otherwise  -> throwM $ Mismatch p [t1, t2]
    | op == LT || op == EQ || op == GT || op == GE = do
        case (t1, t2) of
          (TInt,   TInt)   ->
            return $ defaultEInfo & tau .~ TBool
                                  & term .~ (iAEBinop p op (linfo ^. term) (rinfo ^. term))
          (TFloat, TFloat) ->
            return $ defaultEInfo & tau .~ TBool
                                  & term .~ (iAEBinop p op (linfo ^. term) (rinfo ^. term))
          _ | t1 == t2  -> throwM $ ExpectNum p t1
            | otherwise -> throwM $ Mismatch p [t1, t2]
    | op == EQ || op == NEQ = do
        case (t1, t2) of
          (TInt,   TInt)   ->
            return $ defaultEInfo & tau .~ TBool
                                  & term .~ (iAEBinop p op (linfo ^. term) (rinfo ^. term))
          (TFloat, TFloat) ->
            return $ defaultEInfo & tau .~ TBool
                                  & term .~ (iAEBinop p op (linfo ^. term) (rinfo ^. term))
          (TBool,  TBool)  ->
            return $ defaultEInfo & tau .~ TBool
                                  & term .~ (iAEBinop p op (linfo ^. term) (rinfo ^. term))
          _ | t1 == t2  -> throwM $ ExpectSmall p t1
            | otherwise -> throwM $ Mismatch p [t1, t2]
  shapeCheck (EIndex info idxInfo@((preview tau) -> Just t2) :&: p) = do
    case (info ^? tau, t2) of
      (Just (TArr t _), TInt) ->
        return $ defaultEInfo & tau  .~ t
                              & term .~ iAEIndex p (info ^. term) (idxInfo ^. term)
      (Just (TBag t),   TInt) ->
        return $ defaultEInfo & tau  .~ t
                              & term .~ iAEIndex p (info ^. term) (idxInfo ^. term)
      (Just t1,         TInt) -> throwM $ ExpectArr p t1
      (Just _,          _   ) -> throwM $ ExpectTau p TInt t2
      (_,               _   ) -> throwM $ ExpectExpr p

  shapeCheck (EFloat info@((preview tau) -> Just t) :&: p) = do
    case t of
      TInt -> return $ defaultEInfo & tau .~ TFloat
                                    & term .~ (iAEFloat p (info ^. term))
      _    -> throwM $ ExpectTau p TInt t
  shapeCheck (EExp info@((preview tau) -> Just t) :&: p) = do
    case t of
      TFloat -> return $ defaultEInfo & tau .~ TFloat
                                      & term .~ (iAEExp p (info ^. term))
      _      -> throwM $ ExpectTau p TFloat t

  shapeCheck (ELog info@((preview tau) -> Just t) :&: p) = do
    case t of
      TFloat -> return $ defaultEInfo & tau .~ TFloat
                                      & term .~ (iAELog p (info ^. term))
      _      -> throwM $ ExpectTau p TFloat t


  shapeCheck (EClip info@((preview tau) -> Just TInt) (LInt v) :&: p) = do
    return $ defaultEInfo & tau .~ TInt
                          & term .~ (iAEClip p (info ^. term) (LInt v))
  shapeCheck (EClip info@((preview tau) -> Just TFloat) (LFloat v) :&: p) = do
    return $ defaultEInfo & tau .~ TFloat
                          & term .~ (iAEClip p (info ^. term) (LFloat v))
  shapeCheck (EClip ((preview tau) -> Just t) _ :&: p) =
    throwM $ ExpectNum p t

  shapeCheck (EScale scalarInfo@((preview tau) -> Just tscalar)
                     vecInfo@((preview tau)    -> Just tvector) :&: p) = do
    case (tscalar, tvector) of
      (TFloat, TArr TFloat _) -> return $ defaultEInfo & tau .~ tvector
                                                       & term .~ (iAEScale p (scalarInfo ^. term)
                                                                             (vecInfo ^. term))
      (_,      TArr TFloat _) -> throwM $ ExpectTau p TFloat tscalar
      (_,      _            ) -> throwM $ ExpectTau p (TArr TFloat Nothing) tvector

  shapeCheck (EDot linfo@((preview tau) -> Just tv1) rinfo@((preview tau) -> Just tv2) :&: p) = do
    case (tv1, tv2) of
      (TArr TFloat (Just len1), TArr TFloat (Just len2))
        | len1 == len2 -> return $ defaultEInfo & tau .~ TFloat
                                                & term .~ (iAEDot p (linfo ^. term) (rinfo ^. term))
      _ -> throwM $ Mismatch p [tv1, tv2]
  shapeCheck (_ :&: p) =
    throwM $ ExpectExpr p

instance ShapeCheck (Cmd :&: Position) where
  shapeCheck (CAssign linfo@(preview tau -> Just lt) rinfo@(preview tau -> Just rt) :&: p) = do
    callCC $ \good -> do
      case project @ExprP (linfo ^. term) of
        Just (EVar _ :&: _) -> good ()
        Just (ELength lhs :&: _) ->
          case project @ExprP lhs of
            Just (EVar _ :&: _) -> good ()
            _ -> throwM $ UnsupportedAssign p
        Just (EIndex lhs _ :&: _) ->
          case project @ExprP lhs of
            Just (EVar _ :&: _) -> good ()
            _ -> throwM $ UnsupportedAssign p
        Just _ -> throwM $ UnsupportedAssign p
        Nothing -> throwM $ UnsupportedAssign p

    -- TODO: really we should check whether rt is compat with lt
    case lt == rt of
      True ->  return $ defaultCInfo & term .~ (iACAssign p (linfo ^. term) (rinfo ^. term))
      False -> throwM $ Mismatch p [lt, rt]

  shapeCheck (CLaplace linfo@(preview tau -> Just lt) w rinfo@(preview tau -> Just rt) :&: p) = do
    when (w <= 0) $ do
      throwM $ ExpectPositive p w

    callCC $ \good -> do
      case project @ExprP (linfo ^. term) of
        Just (EVar _ :&: _) -> good ()
        _                   -> throwM $ UnsupportedAssign p

    case (lt, rt) of
      (TFloat, TFloat) ->
        return $ defaultCInfo & term .~ (iACLaplace p (linfo ^. term) w (rinfo ^. term))
      _ -> throwM $ UnsupportedAssign p


  shapeCheck (CIf einfo@(preview tau -> Just et) cinfo1 cinfo2 :&: p) = do
    case (et, cinfo1 ^? _CShapeInfo, cinfo2 ^? _CShapeInfo) of
      (TBool, Just c1, Just c2) -> return $ defaultCInfo & term .~ (iACIf p (einfo ^. term) c1 c2)
      (_    , Just _,  Just _ ) -> throwM $ ExpectTau p TBool et
      (_    , _     ,  _      ) -> throwM $ ExpectCmd p

  shapeCheck (CWhile einfo@(preview tau -> Just et) cinfo :&: p) = do
    case (et, cinfo ^? _CShapeInfo) of
      (TBool, Just body) -> return $ defaultCInfo & term .~ (iACWhile p (einfo ^. term) body)
      (_,     Just _)    -> throwM $ ExpectTau p TBool et
      (_,     Nothing)   -> throwM $ ExpectCmd p


  shapeCheck (CSeq cinfo1 cinfo2 :&: p) = do
    case (cinfo1 ^? _CShapeInfo, cinfo2 ^? _CShapeInfo) of
      (Just c1, Just c2) -> return $ defaultCInfo & term .~ (iACSeq p c1 c2)
      _                  -> throwM $ ExpectCmd p

  shapeCheck (CSkip :&: p) = return $ defaultCInfo & term .~ (iACSkip p)

  shapeCheck (_ :&: p) = throwM $ ExpectExpr p


instance ShapeCheck (CTCHint :&: Position) where
  shapeCheck (CTCHint name params info :&: p) =
    return $ defaultCInfo & term .~ (iACTCHint p name (params ^.. traverse . term) (info ^. term))

{-
shapeImpTCP :: forall m.
               ( MonadReader ShapeCxt m
               , MonadThrow           m
               ) => AlgM m ImpTCP ShapeInfo
shapeImpTCP = shapeCheck
-}
