{-# OPTIONS_GHC -Wno-orphans #-}

module Speculative.Sensitivity where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Cont

import qualified Data.Map as M
import Data.Maybe
import Control.Lens hiding (op)
import Data.Comp
import Data.Comp.Derive
import SyntaxExt
import Shape hiding (defaultInfo, ShapeCheckError(..))
import qualified Shape as S

data Value = VInt   Int
           | VFloat Float
           | VBool  Bool
           | VArray [Value]
           | VBag   [Value]
           deriving (Show, Eq, Ord)

data AssignPat = APVar Var
               | APLength Var
               | APLength2 SpecSensInfo
               | APIndex Var SpecSensInfo

isVInt :: Value -> Maybe Int
isVInt (VInt v) = Just v
isVInt _ = Nothing

isVFloat :: Value -> Maybe Float
isVFloat (VFloat v) = Just v
isVFloat _ = Nothing

isVBool :: Value -> Maybe Bool
isVBool (VBool v) = Just v
isVBool _ = Nothing

isVArray :: Value -> Maybe [Value]
isVArray (VArray v) = Just v
isVArray _ = Nothing

isVBag :: Value -> Maybe [Value]
isVBag (VBag v) = Just v
isVBag _ = Nothing

type Eps   = Float
type Delta = Float

newtype SensCxt = SensCxt { getSensCxt :: (M.Map Var Float, Eps, Delta) }
  deriving (Show, Eq)

data SpecSensInfo = SpecSensInfo {
  _specsensinfo_expr_sens :: Maybe Float
  , _specsensinfo_is_literal :: Maybe Value
  , _specsensinfo_shape_info :: ShapeInfo
  , _specsensinfo_assign_pat :: Maybe AssignPat
  }

$(makeLensesWith underscoreFields ''SpecSensInfo)

newtype Sens = Sens { getSens :: Maybe Float }
  deriving (Show, Eq, Ord)

instance Semigroup Sens where
  (getSens -> Just a) <> (getSens -> Just b) = Sens $ Just $ a + b
  _                   <> _                   = Sens Nothing

instance Monoid Sens where
  mempty = Sens . Just $ 0

approx :: Maybe Float -> Maybe Float -> Maybe Float
approx (Just 0) (Just 0) = Just 0
approx _        _        = Nothing

approx1 :: Maybe Float -> Maybe Float
approx1 (Just 0) = Just 0
approx1 _        = Nothing

smin :: Maybe Float -> Maybe Float -> Maybe Float
smin (Just a) (Just b) = Just $ min a b
smin (Just s) _        = Just s
smin _        (Just s) = Just s
smin _        _        = Nothing

data SpecSensCheckError = UnknownVariable Position Var
                        | MayNotCoterminate Position
                        | InternalError Position String
  deriving (Show, Eq)

defaultInfo :: SpecSensInfo
defaultInfo = SpecSensInfo Nothing Nothing S.defaultInfo Nothing

-- We really should use MonadThrow instead of MonadExcept, but that's left for
-- refactor
instance MonadError e m => MonadError e (ContT a m) where
  throwError = lift . throwError
  catchError = error "catchError: cannot be used in a ContT stack"

class SpecSensCheck f where
  specSensCheck :: ( MonadError e m
                   , Inj e SpecSensCheckError
                   , Inj e S.ShapeCheckError
                   , MonadReader ShapeCxt m
                   , MonadCont m)
                => Alg f (ShapeInfo -> M.Map Var Float -> m (SpecSensInfo, SensCxt))

instance (SpecSensCheck f1, SpecSensCheck f2)
  => SpecSensCheck (f1 :+: f2) where
  specSensCheck = caseF (specSensCheck) (specSensCheck)

{-

instance SpecSensCheck (Expr :&: Position) where
  specSensCheck c@(EVar v :&: p) = \cxt -> do
    shapeInfo <- projShapeCheck cxt c
    case M.lookup v (cxt ^. _1) of
      Nothing -> throwErrorInj $ UnknownVariable p v
      Just s -> return $ defaultInfo & expr_sens .~ Just s
                                     & shape_info .~ shapeInfo
                                     & assign_pat .~ Just (APVar v)

  specSensCheck c@(ELength info :&: p) = callCC $ \ret -> do
    shapeInfo <- projShapeCheck c

    assignPat <-
      case info ^. assign_pat of
        Just (APVar x) -> return $ Just (APLength  x)
        _              -> return $ Just (APLength2 info)

    -- when the inner expression is a literal
    when (isJust $ info ^. is_literal) $ do
      case info ^. is_literal of
        Just (VArray arr) -> ret $ defaultInfo & expr_sens .~ Just 0
                                               & is_literal .~ Just (VInt $ length arr)
                                               & shape_info .~ shapeInfo
                                               & assign_pat .~ assignPat
        _ -> return ()

    -- if not literal, then check if it's a constant length array
    when (isJust $ shapeInfo ^? is_expr . _Just . fixed_length . _Just) $ do
      case shapeInfo ^? is_expr . _Just . fixed_length . _Just of
        Just len -> ret $ defaultInfo & expr_sens .~ Just 0
                                      & is_literal .~ Just (VInt len)
                                      & shape_info .~ shapeInfo
                                      & assign_pat .~ assignPat
        _ -> throwErrorInj $ InternalError p "bug: impossible case, lens told us it will be a Just"

    -- if not constant length array, then just check sensitivity of the inner
    -- bag/array
    case (shapeInfo ^. is_expr, info ^. expr_sens) of
      (Just (TBag _), Just s) ->
        return $ defaultInfo & expr_sens .~ Just s
                             & shape_info .~ shapeInfo
                             & assign_pat .~ assignPat
      (Just (TBag _), Nothing) ->
        return $ defaultInfo & shape_info .~ shapeInfo
                             & assign_pat .~ assignPat
      (Just (TArr _ _), Just _) ->
        return $ defaultInfo & expr_sens .~ Just 0
                             & shape_info .~ shapeInfo
                             & assign_pat .~ assignPat
      (Just (TArr _ _), Nothing) ->
        return $ defaultInfo & shape_info .~ shapeInfo
                             & assign_pat .~ assignPat
      _ -> throwErrorInj $ InternalError p "bug: shape checker didn't rule out bad length expression"

  specSensCheck c@(ELit (LInt v) :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ defaultInfo & expr_sens .~ Just 0
                         & is_literal .~ Just (VInt v)
                         & shape_info .~ shapeInfo

  specSensCheck c@(ELit (LFloat v) :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ defaultInfo & expr_sens .~ Just 0
                         & is_literal .~ Just (VFloat v)
                         & shape_info .~ shapeInfo

  specSensCheck c@(ELit (LBool b) :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ defaultInfo & expr_sens .~ Just 0
                         & is_literal .~ Just (VBool b)
                         & shape_info .~ shapeInfo

  specSensCheck c@(ELit (LArr infos) :&: p) = callCC $ \ret -> do
    shapeInfo <- projShapeCheck c
    -- if this is a literal array with literal contents
    when (all isJust $ map (view is_literal) infos) $ do
      case sequenceA $ map (view is_literal) infos of
        Just vals ->
          ret $ defaultInfo & expr_sens .~ Just 0
                            & is_literal .~ Just (VArray vals)
                            & shape_info .~ shapeInfo
        _ -> throwErrorInj $ InternalError p "bug: lens told us it's all Just"

    -- otherwise, add up the sensitivity of each entry
    let sens = getSens $ foldMap Sens (map (view expr_sens) infos)
    return $ defaultInfo & expr_sens .~ sens
                         & shape_info .~ shapeInfo

  specSensCheck c@(ELit (LBag infos) :&: p) = callCC $ \ret -> do
    shapeInfo <- projShapeCheck c
    -- if this is a literal array with literal contents
    when (all isJust $ map (view is_literal) infos) $ do
      case sequenceA $ map (view is_literal) infos of
        Just vals ->
          ret $ defaultInfo & expr_sens .~ Just 0
                            & is_literal .~ Just (VBag vals)
                            & shape_info .~ shapeInfo
        _ -> throwErrorInj $ InternalError p "bug: lens told us it's all Just"

    -- otherwise, each sensitive entry may contribute 2 bag sensitivity
    let numSens = fromIntegral $ length $ filter (> Just 0) (map (view expr_sens) infos)
    return $ defaultInfo & expr_sens .~ Just (numSens * 2)
                         & shape_info .~ shapeInfo

  specSensCheck c@(EBinop op linfo rinfo :&: _)
    | op == PLUS || op == MINUS = do
        shapeInfo <- projShapeCheck c
        let sens = getSens ((Sens $ linfo ^. expr_sens) <> (Sens $ rinfo ^. expr_sens))
        return $ defaultInfo & expr_sens  .~ sens
                             & shape_info .~ shapeInfo

    | op == MULT = do
        shapeInfo <- projShapeCheck c
        let lsens = linfo ^. expr_sens
        let rsens = rinfo ^. expr_sens

        resultInfo <-
          case (linfo ^. is_literal, rinfo ^. is_literal) of
            (Just (VInt k), _)   ->
              return $ defaultInfo & expr_sens .~ (fmap (* (fromIntegral k)) rsens)
            (Just (VFloat k), _) ->
              return $ defaultInfo & expr_sens .~ (fmap (* k) rsens)
            (_, Just (VInt k))   ->
              return $ defaultInfo & expr_sens .~ (fmap (* (fromIntegral k)) lsens)
            (_, Just (VFloat k)) ->
              return $ defaultInfo & expr_sens .~ (fmap (* k) lsens)
            _ -> return $ defaultInfo & expr_sens .~ (approx lsens rsens)

        return $ resultInfo & shape_info .~ shapeInfo

    | op == DIV = do
        shapeInfo <- projShapeCheck c

        let lsens = linfo ^. expr_sens
        let rsens = rinfo ^. expr_sens

        resultInfo <-
          case (rinfo ^. is_literal) of
            Just (VInt k)  ->
              return $ defaultInfo & expr_sens .~ (fmap (/ (fromIntegral k)) lsens)
            Just (VFloat k) ->
              return $ defaultInfo & expr_sens .~ (fmap (/ k) lsens)
            _ -> return $ defaultInfo & expr_sens .~ (approx lsens rsens)

        return $ resultInfo & shape_info .~ shapeInfo

    | otherwise = do
        shapeInfo <- projShapeCheck c

        let lsens = linfo ^. expr_sens
        let rsens = rinfo ^. expr_sens

        return $ defaultInfo & expr_sens .~ (approx lsens rsens)
                             & shape_info .~ shapeInfo

  specSensCheck c@(EIndex linfo rinfo :&: p) = do
    shapeInfo <- projShapeCheck c
    case rinfo ^. expr_sens of
      Just 0 ->
        case (linfo ^. shape_info . is_expr, linfo ^. expr_sens) of
          (Just (TArr _ _), s@(Just _)) ->
            return $ defaultInfo & expr_sens .~ s
                                 & shape_info .~ shapeInfo
          (Just (TArr _ _), Nothing) ->
            throwErrorInj $ MayNotCoterminate p
          (Just (TBag _), Just 0) ->
            return $ defaultInfo & shape_info .~ shapeInfo
          (Just (TBag _), _) ->
            throwErrorInj $ MayNotCoterminate p
          _ -> throwErrorInj $ InternalError p "bug: indexing neither bag nor array"
      _ -> throwErrorInj $ MayNotCoterminate p

  specSensCheck c@(EFloat info :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ defaultInfo & expr_sens .~ (info ^. expr_sens)
                         & shape_info .~ shapeInfo

  specSensCheck c@(EExp info :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ defaultInfo & expr_sens .~ (approx1 $ info ^. expr_sens)
                         & shape_info .~ shapeInfo

  specSensCheck c@(ELog info :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ defaultInfo & expr_sens .~ (approx1 $ info ^. expr_sens)
                         & shape_info .~ shapeInfo

  specSensCheck c@(EClip info litInfo :&: p) = do
    shapeInfo <- projShapeCheck c
    case litInfo of
      LInt k ->
        return $ defaultInfo & expr_sens .~ (smin (info ^. expr_sens) (Just $ realToFrac $ 2 * k))
                             & shape_info .~ shapeInfo
      LFloat k ->
        return $ defaultInfo & expr_sens .~ (smin (info ^. expr_sens) (Just $ 2 * k))
                             & shape_info .~ shapeInfo
      _ -> throwErrorInj $ InternalError p "bug: shape checker should've caught this"

  specSensCheck c@(EScale scalarInfo vecInfo :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ defaultInfo & expr_sens .~ ((*) <$> scalarInfo ^. expr_sens <*> vecInfo ^. expr_sens)
                         & shape_info .~ shapeInfo

  specSensCheck c@(EDot linfo rinfo :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ defaultInfo & expr_sens .~ (approx (linfo ^. expr_sens) (rinfo ^. expr_sens))
                         & shape_info .~ shapeInfo

-}

{-


instance SpecSensCheck (Cmd :&: Position) where
  specSensCheck c@(CAssign lhs rhs :&: p) = callCC $ \ret -> do
    shapeInfo <- projShapeCheck c

    case lhs ^. assign_pat of
      -- x = e
      Just (APVar x) -> do
        cxt <- getSensCxt <$> get
        let cxt' = M.update (const $ rhs ^. expr_sens) x (cxt ^. _1)
        put $ SensCxt $ cxt & _1 .~ cxt'
        ret $ defaultInfo & shape_info .~ shapeInfo

      -- length(x) = e
      Just (APLength x) -> do
        cxt <- getSensCxt <$> get
        shapeCxt <- getShapeCxt <$> ask

        -- this case has special refinement
        -- length(x) = length(e)
        let specialLengthCase sensy =
              case M.lookup x shapeCxt of
                Just (TArr _ _) ->
                  case sensy of
                    Nothing -> do
                      let cxt' = M.update (const Nothing) x (cxt ^. _1)
                      put $ SensCxt $ cxt & _1 .~ cxt'
                      ret $ defaultInfo & shape_info .~ shapeInfo
                    Just _ ->
                      -- no need to update sensitivity, just return
                      ret $ defaultInfo & shape_info .~ shapeInfo
                Just (TBag _) -> do
                  let cxt' = M.update (const Nothing) x (cxt ^. _1)
                  put $ SensCxt $ cxt & _1 .~ cxt'
                  ret $ defaultInfo & shape_info .~ shapeInfo
                -- fallthrough
                _ -> return ()

        case rhs ^. assign_pat of
          Just (APLength  y    ) -> specialLengthCase (M.lookup y (cxt ^. _1))
          Just (APLength2 einfo) -> specialLengthCase (einfo ^. expr_sens)
          _ -> return ()

        -- length(x) = e
        case M.lookup x shapeCxt of
          Just (TArr _ _) ->
            case rhs ^. expr_sens of
              Just 0 ->
                -- no update required
                ret $ defaultInfo & shape_info .~ shapeInfo
              _ -> throwErrorInj $ MayNotCoterminate p
          Just (TBag _) ->
            case rhs ^. expr_sens of
              Just 0 -> do
                let cxt' = M.update (const Nothing) x (cxt ^. _1)
                put $ SensCxt $ cxt & _1 .~ cxt'
                ret $ defaultInfo & shape_info .~ shapeInfo
              _ -> throwErrorInj $ MayNotCoterminate p
          _ -> return ()

      -- x[i] = e
      Just (APIndex x idxInfo) -> do
        cxt <- getSensCxt <$> get
        shapeCxt <- getShapeCxt <$> ask

        -- this case has special refinement
        -- x[k] = e
        case idxInfo ^. is_literal of
          Just (VInt k) -> do
            case M.lookup x shapeCxt of
              Just (TArr _ (Just len))
                | 0 <= k && k < len -> do
                    let sx = M.lookup x (cxt ^. _1)
                    let cxt' =
                          M.update
                            (const $ (+) <$> sx <*> rhs ^. expr_sens)
                            x
                            (cxt ^. _1)
                    put $ SensCxt $ cxt & _1 .~ cxt'
                    ret $ defaultInfo & shape_info .~ shapeInfo
              -- fallthrough
              _ -> return ()
          -- fallthrough
          _ -> return ()

        case (M.lookup x shapeCxt, idxInfo ^. expr_sens) of
          (Just (TArr _ _), Just 0) -> do
            let sx = M.lookup x (cxt ^. _1)
            case sx of
              Just _ -> do
                let cxt' = M.update (const $ (+) <$> sx <*> rhs ^. expr_sens) x (cxt ^. _1)
                put $ SensCxt $ cxt & _1 .~ cxt'
                ret $ defaultInfo & shape_info .~ shapeInfo
              Nothing ->
                throwErrorInj $ MayNotCoterminate p
          (Just (TBag _), Just 0) -> do
            let sx = M.lookup x (cxt ^. _1)
            case sx of
              Just 0 -> do
                let cxt' = M.update (const $ Just 2) x (cxt ^. _1)
                put $ SensCxt $ cxt & _1 .~ cxt'
                ret $ defaultInfo & shape_info .~ shapeInfo
              _ -> throwErrorInj $ MayNotCoterminate p
          _ -> return ()

      _ -> return ()

    throwErrorInj $ InternalError p "bug: shape checker didn't rule out bad assignment"

  specSensCheck c@(CLaplace lhs w rhs :&: p) = do
    shapeInfo <- projShapeCheck c

    case lhs ^. assign_pat of
      Just (APVar x) -> undefined

      _ -> throwErrorInj $ InternalError p "bug: shape checker didn't rule out bad laplace"

    return $ defaultInfo & shape_info .~ shapeInfo

instance SpecSensCheck (CTCHint :&: Position) where

-}
