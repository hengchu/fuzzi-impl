module Speculative.Sensitivity where

import Type.Reflection
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Cont
import Control.Monad.State

import qualified Data.Map as M
import Data.Maybe
import Control.Lens hiding (op)
import Data.Comp
import SyntaxExt
import Shape hiding (defaultInfo, ShapeCheckError(..))
import qualified Shape as S

data Value = VInt   Int
           | VFloat Float
           | VBool  Bool
           | VArray [Value]
           | VBag   [Value]
           deriving (Show, Eq, Ord)

data AssignPat m = APVar Var
                 | APLength Var
                 | APLength2 (SpecSensInfo m)
                 | APIndex Var (SpecSensInfo m)

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

newtype SensCxt = SensCxt { getSensCxt :: M.Map Var Float }
  deriving (Show, Eq)

data SpecSensInfo m = SpecSensInfo {
  _specsensinfo_is_expr      :: m (Maybe Float)
  , _specsensinfo_is_cmd     :: m (Eps, Delta)
  , _specsensinfo_is_literal :: Maybe Value
  , _specsensinfo_shape_info :: ShapeInfo
  , _specsensinfo_assign_pat :: Maybe (AssignPat m)
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

scxtMax :: SensCxt -> SensCxt -> SensCxt
scxtMax (SensCxt c1) (SensCxt c2) =
  SensCxt
  $ M.foldrWithKey
    (\k s1 acc ->
       case c2 ^. (at k) of
         Just s2 -> M.insert k (max s1 s2) acc
         _       -> M.delete k acc)
    M.empty
    c1

data SpecSensCheckError = UnknownVariable Position Var
                        | MayNotCoterminate Position
                        | OutOfBoundsIndex Position
                        | InternalError Position String
                        | CannotReleaseInfSensData Position
                        | CannotBranchOnSensData Position
                        | CannotEstablishInvariant Position
  deriving (Show, Eq, Typeable)

instance Exception SpecSensCheckError

defaultInfo :: (Monad m) => SpecSensInfo (StateT SensCxt m)
defaultInfo = SpecSensInfo
                (return Nothing)
                (return (0, 0))
                Nothing
                S.defaultInfo
                Nothing

projShapeCheck :: ( MonadThrow m
                  , MonadReader ShapeCxt m
                  , Functor f
                  , ShapeCheck f)
               => f (SpecSensInfo (StateT s m)) -> m ShapeInfo
projShapeCheck fa = shapeCheck $ fmap (view shape_info) fa

class SpecSensCheck f where
  specSensCheck :: ( MonadThrow m
                   , MonadReader ShapeCxt m
                   , MonadCont m)
                => AlgM m f (SpecSensInfo (StateT SensCxt m))

instance (SpecSensCheck f1, SpecSensCheck f2)
  => SpecSensCheck (f1 :+: f2) where
  specSensCheck = caseF (specSensCheck) (specSensCheck)

instance SpecSensCheck (Expr :&: Position) where
  specSensCheck c@(EVar v :&: _) = do
    shapeInfo <- projShapeCheck c
    let info = defaultInfo & shape_info .~ shapeInfo
                           & assign_pat .~ Just (APVar v)
    let eAct = do { cxt <- get; return $ M.lookup v (getSensCxt cxt) }
    return $ info & is_expr .~ eAct

  specSensCheck c@(ELength info :&: p) = do
    shapeInfo <- projShapeCheck c

    assignPat <-
      case info ^. assign_pat of
        Just (APVar x) -> return $ Just (APLength  x)
        _              -> return $ Just (APLength2 info)

    -- when the inner expression is a literal
    info' <- callCC $ \ret -> do
      when (isJust $ info ^. is_literal) $ do
        case info ^. is_literal of
          Just (VArray arr) -> ret $ defaultInfo & is_expr    .~ return (Just 0)
                                                 & is_literal .~ Just (VInt $ length arr)
                                                 & shape_info .~ shapeInfo
                                                 & assign_pat .~ assignPat
          _ -> return ()

      -- if not literal, then check if it's a constant length array
      when (isJust $ shapeInfo ^? is_expr . _Just . fixed_length . _Just) $ do
        case shapeInfo ^? is_expr . _Just . fixed_length . _Just of
          Just len -> ret $ defaultInfo & is_expr    .~ return (Just 0)
                                        & is_literal .~ Just (VInt len)
                                        & shape_info .~ shapeInfo
                                        & assign_pat .~ assignPat
          _ -> throwM $ InternalError p "bug: impossible case, lens told us it will be a Just"

      -- if not constant length array, then just check sensitivity of the inner
      -- bag/array
      case shapeInfo ^. is_expr of
        Just (TBag _) ->
          return $ defaultInfo & is_expr    .~ (info ^. is_expr)
                               & shape_info .~ shapeInfo
                               & assign_pat .~ assignPat
        Just (TArr _ _) -> do
          let arrSens = do
                s <- info ^. is_expr
                case s of
                  Just _ -> return $ Just 0
                  _      -> return Nothing
          return $ defaultInfo & is_expr    .~ arrSens
                               & shape_info .~ shapeInfo
                               & assign_pat .~ assignPat
        _ -> throwM $ InternalError p "bug: shape checker didn't rule out bad length expression"

    return info'


  specSensCheck c@(ELit (LInt v) :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ defaultInfo & is_expr    .~ return (Just 0)
                         & is_literal .~ Just (VInt v)
                         & shape_info .~ shapeInfo

  specSensCheck c@(ELit (LFloat v) :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ defaultInfo & is_expr    .~ return (Just 0)
                         & is_literal .~ Just (VFloat v)
                         & shape_info .~ shapeInfo

  specSensCheck c@(ELit (LBool b) :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ defaultInfo & is_expr    .~ return (Just 0)
                         & is_literal .~ Just (VBool b)
                         & shape_info .~ shapeInfo

  specSensCheck c@(ELit (LArr infos) :&: p) = callCC $ \ret -> do
    shapeInfo <- projShapeCheck c
    -- if this is a literal array with literal contents
    when (all isJust $ map (view is_literal) infos) $ do
      case sequenceA $ map (view is_literal) infos of
        Just vals ->
          ret $ defaultInfo & is_expr    .~ return (Just 0)
                            & is_literal .~ Just (VArray vals)
                            & shape_info .~ shapeInfo
        _ -> throwM $ InternalError p "bug: lens told us it's all Just"

    -- otherwise, add up the sensitivity of each entry
    let litArrSens = do
          sens <- sequenceA $ fmap (view is_expr) infos
          return $ getSens $ foldMap Sens sens
    return $ defaultInfo & is_expr    .~ litArrSens
                         & shape_info .~ shapeInfo

  specSensCheck c@(ELit (LBag infos) :&: p) = callCC $ \ret -> do
    shapeInfo <- projShapeCheck c
    -- if this is a literal array with literal contents
    when (all isJust $ map (view is_literal) infos) $ do
      case sequenceA $ map (view is_literal) infos of
        Just vals ->
          ret $ defaultInfo & is_expr    .~ return (Just 0)
                            & is_literal .~ Just (VBag vals)
                            & shape_info .~ shapeInfo
        _ -> throwM $ InternalError p "bug: lens told us it's all Just"

    -- otherwise, each sensitive entry may contribute 2 bag sensitivity
    let litBagSens = do
          sens <- sequenceA $ fmap (view is_expr) infos
          return $ Just $ 2 * (fromIntegral . length . filter (> Just 0) $ sens)
    return $ defaultInfo & is_expr .~ litBagSens
                         & shape_info .~ shapeInfo

  specSensCheck c@(EBinop op linfo rinfo :&: _)
    | op == PLUS || op == MINUS = do
        shapeInfo <- projShapeCheck c
        let sens = do
              lsens <- linfo ^. is_expr
              rsens <- rinfo ^. is_expr
              return $ (+) <$> lsens <*> rsens
        return $ defaultInfo & is_expr    .~ sens
                             & shape_info .~ shapeInfo

    | op == MULT = do
        shapeInfo <- projShapeCheck c

        resultInfo <-
          case (linfo ^. is_literal, rinfo ^. is_literal) of
            (Just (VInt k), _) -> do
              let sens = do
                    s <- rinfo ^. is_expr
                    return $ (* (fromIntegral k)) <$> s
              return $ defaultInfo & is_expr .~ sens
            (Just (VFloat k), _) -> do
              let sens = do
                    s <- rinfo ^. is_expr
                    return $ (* k) <$> s
              return $ defaultInfo & is_expr .~ sens
            (_, Just (VInt k)) -> do
              let sens = do
                    s <- linfo ^. is_expr
                    return $ (* (fromIntegral k)) <$> s
              return $ defaultInfo & is_expr .~ sens
            (_, Just (VFloat k)) -> do
              let sens = do
                    s <- rinfo ^. is_expr
                    return $ (* k) <$> s
              return $ defaultInfo & is_expr .~ sens
            _ -> do
              let sens = do
                    ls <- linfo ^. is_expr
                    rs <- rinfo ^. is_expr
                    return $ approx ls rs
              return $ defaultInfo & is_expr .~ sens

        return $ resultInfo & shape_info .~ shapeInfo
    | op == DIV = do
        shapeInfo <- projShapeCheck c

        result <-
          case (rinfo ^. is_literal) of
            Just (VInt k) -> do
              let sens = do { s <- linfo ^. is_expr; return $ (* (fromIntegral k)) <$> s }
              return $ defaultInfo & is_expr .~ sens
            Just (VFloat k) -> do
              let sens = do { s <- linfo ^. is_expr; return $ (* k) <$> s }
              return $ defaultInfo & is_expr .~ sens
            _ -> return $ defaultInfo & is_expr .~ (approx <$> linfo ^. is_expr <*> rinfo ^. is_expr)

        return $ result & shape_info .~ shapeInfo

    | otherwise = do
        shapeInfo <- projShapeCheck c

        let lsens = linfo ^. is_expr
        let rsens = rinfo ^. is_expr

        return $ defaultInfo & is_expr .~ (approx <$> lsens <*> rsens)
                             & shape_info .~ shapeInfo

  specSensCheck c@(EIndex linfo rinfo :&: p) = do
    shapeInfo <- projShapeCheck c
    let eindexSens = do
          lsens <- linfo ^. is_expr
          rsens <- rinfo ^. is_expr
          case rsens of
            Just 0 ->
              case (linfo ^. shape_info . is_expr, lsens) of
                (Just (TArr _ _), s@(Just _)) ->
                  return s
                (Just (TArr _ _), Nothing) ->
                  throwM $ MayNotCoterminate p
                (Just (TBag _), Just 0) ->
                  return Nothing
                (Just (TBag _), _) ->
                  throwM $ MayNotCoterminate p
                _ -> throwM $ InternalError p "bug: indexing neither bag nor array"
            _ -> throwM $ MayNotCoterminate p
    return $ defaultInfo & is_expr .~ eindexSens
                         & shape_info .~ shapeInfo

  specSensCheck c@(EFloat info :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ defaultInfo & is_expr .~ (info ^. is_expr)
                         & shape_info .~ shapeInfo

  specSensCheck c@(EExp info :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ defaultInfo & is_expr .~ (approx1 <$> info ^. is_expr)
                         & shape_info .~ shapeInfo

  specSensCheck c@(ELog info :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ defaultInfo & is_expr .~ (approx1 <$> info ^. is_expr)
                         & shape_info .~ shapeInfo

  specSensCheck c@(EClip info litInfo :&: p) = do
    shapeInfo <- projShapeCheck c
    case litInfo of
      LInt k ->
        return $ defaultInfo & is_expr .~ (smin <$> (info ^. is_expr) <*> (return . Just $ realToFrac $ 2 * k))
                             & shape_info .~ shapeInfo
      LFloat k ->
        return $ defaultInfo & is_expr .~ (smin <$> (info ^. is_expr) <*> (return . Just $ 2 * k))
                             & shape_info .~ shapeInfo
      _ -> throwM $ InternalError p "bug: shape checker should've caught this"


  specSensCheck c@(EScale scalarInfo vecInfo :&: _) = do
    shapeInfo <- projShapeCheck c
    let sens = do { ls <- scalarInfo ^. is_expr; rs <- vecInfo ^. is_expr; return $ (*) <$> ls <*> rs }
    return $ defaultInfo & is_expr    .~ sens
                         & shape_info .~ shapeInfo

  specSensCheck c@(EDot linfo rinfo :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ defaultInfo & is_expr .~ (approx <$> (linfo ^. is_expr) <*> (rinfo ^. is_expr))
                         & shape_info .~ shapeInfo

instance SpecSensCheck (Cmd :&: Position) where
  specSensCheck c@(CAssign lhs rhs :&: p) = do
    shapeInfo <- projShapeCheck c

    case lhs ^. assign_pat of
      -- x = e
      Just (APVar x) -> do
        let assignEff = do
              cxt <- getSensCxt <$> get
              rs <- rhs ^. is_expr
              let cxt' = M.update (const rs) x cxt
              put $ SensCxt $ cxt'
              return (0, 0)
        return $ defaultInfo & is_cmd .~ assignEff
                             & shape_info .~ shapeInfo

      -- length(x) = e
      Just (APLength x) -> callCC $ \ret -> do
        shapeCxt <- getShapeCxt <$> ask

        -- length(x) = length(y), this case requires special refinement
        let refinement syAct = do
              case M.lookup x shapeCxt of
                Just (TArr _ _) -> do
                  let assignEff = do
                        sensy <- syAct
                        case sensy of
                          Nothing -> do
                            cxt <- getSensCxt <$> get
                            put $ SensCxt $ M.update (const Nothing) x cxt
                            return (0, 0)
                          Just _ ->
                            return (0, 0)
                  ret $ defaultInfo & shape_info .~ shapeInfo
                                    & is_cmd .~ assignEff
                Just (TBag _) -> do
                  let assignEff = do
                        cxt <- getSensCxt <$> get
                        put $ SensCxt $ M.update (const Nothing) x cxt
                        return (0, 0)
                  ret $ defaultInfo & shape_info .~ shapeInfo
                                    & is_cmd .~ assignEff
                _ -> return ()

        case rhs ^. assign_pat of
          Just (APLength y)       -> refinement (get >>= \cxt -> return $ M.lookup y (getSensCxt cxt))
          Just (APLength2 reinfo) -> refinement (reinfo ^. is_expr)
          -- fallthrough
          _ -> return ()

        case M.lookup x shapeCxt of
          Just (TArr _ _) -> do
            let assignEff = do
                  rs <- rhs ^. is_expr
                  case rs of
                    Just 0 -> return (0, 0)
                    _ -> throwM $ MayNotCoterminate p
            ret $ defaultInfo & shape_info .~ shapeInfo
                              & is_cmd .~ assignEff
          Just (TBag _) -> do
            let assignEff = do
                  rs <- rhs ^. is_expr
                  case rs of
                    Just 0 -> do
                      cxt <- getSensCxt <$> get
                      put $ SensCxt $ M.update (const Nothing) x cxt
                      return (0, 0)
                    _ -> throwM $ MayNotCoterminate p
            ret $ defaultInfo & shape_info .~ shapeInfo
                              & is_cmd .~ assignEff
          _ -> return ()

        throwM $ InternalError p "bug: shape checker didn't catch bad length assignment"

      Just (APIndex x idxInfo) -> callCC $ \ret -> do
        shapeCxt <- getShapeCxt <$> ask

        -- x[k] = e, refinement
        case idxInfo ^. is_literal of
          Just (VInt k) -> do
            case M.lookup x shapeCxt of
              Just (TArr _ (Just len))
                | 0 <= k && k < len -> do
                    let assignEff = do
                          cxt <- getSensCxt <$> get
                          let sx = M.lookup x cxt
                          rs <- rhs ^. is_expr
                          let sx' = (+) <$> sx <*> rs
                          put $ SensCxt $ M.update (const sx') x cxt
                          return (0, 0)
                    ret $ defaultInfo & shape_info .~ shapeInfo
                                      & is_cmd .~ assignEff
                | otherwise -> throwM $ OutOfBoundsIndex p
              -- fallthrough
              _ -> return ()
          -- fallthrough
          _ -> return ()

        case M.lookup x shapeCxt of
          Just (TArr _ _) -> do
            let assignEff = do
                  sidx <- idxInfo ^. is_expr
                  cxt <- getSensCxt <$> get
                  let sx = M.lookup x cxt
                  case (sx, sidx) of
                    (Just _, Just 0) -> do
                      let sx' = (+) <$> sx <*> sidx
                      put $ SensCxt $ M.update (const sx') x cxt
                      return (0, 0)
                    _ -> throwM $ MayNotCoterminate p
            ret $ defaultInfo & shape_info .~ shapeInfo
                              & is_cmd .~ assignEff
          Just (TBag _) -> do
            let assignEff = do
                  sidx <- idxInfo ^. is_expr
                  cxt <- getSensCxt <$> get
                  let sx = M.lookup x cxt
                  case (sx, sidx) of
                    (Just 0, Just 0) -> do
                      put $ SensCxt $ M.insert x 2 cxt
                      return (0, 0)
                    _ -> throwM $ MayNotCoterminate p
            ret $ defaultInfo & shape_info .~ shapeInfo
                              & is_cmd .~ assignEff
          _ -> return ()

        throwM $ InternalError p "bug: shape checker didn't rule out bad index assignment"

      _ -> throwM $ InternalError p "bug: bad assignment pattern"

  specSensCheck c@(CLaplace lhs w rhs :&: p) = do
    shapeInfo <- projShapeCheck c

    case lhs ^. assign_pat of
      Just (APVar x) -> do
        let assignEff = do
              cxt <- getSensCxt <$> get
              put $ SensCxt $ M.insert x 0 cxt
              rs <- rhs ^. is_expr
              case rs of
                Just s -> return (s / w, 0)
                Nothing -> throwM $ CannotReleaseInfSensData p
        return $ defaultInfo & shape_info .~ shapeInfo
                             & is_cmd .~ assignEff
      _ -> throwM $ InternalError p "bug: shape checker didn't rule out bad laplace"

  specSensCheck c@(CIf e c1 c2 :&: p) = do
    shapeInfo <- projShapeCheck c

    let eff = do
          es <- e ^. is_expr
          case es of
            Just 0 -> do
              -- save the current state
              currSt <- get

              -- run the first branch
              (eps1, delt1) <- c1 ^. is_cmd
              st1 <- get

              -- run the second branch in a clean state
              put currSt
              (eps2, delt2) <- c2 ^. is_cmd
              st2 <- get

              put $ scxtMax st1 st2

              return (max eps1 eps2, max delt1 delt2)
            _ -> throwM $ CannotBranchOnSensData p

    return $ defaultInfo & shape_info .~ shapeInfo
                         & is_cmd     .~ eff

  specSensCheck c@(CWhile e body :&: p) = do
    shapeInfo <- projShapeCheck c

    let eff = do
          es <- e ^. is_expr
          case es of
            Just 0 -> do
              currSt <- get
              (eps, delt) <- body ^. is_cmd
              st <- get

              case (eps, delt, currSt == st) of
                (0, 0, True) -> return (0, 0)
                _ -> throwM $ CannotEstablishInvariant p
            _ -> throwM $ CannotBranchOnSensData p

    return $ defaultInfo & shape_info .~ shapeInfo
                         & is_cmd     .~ eff

  specSensCheck c@(CSeq c1 c2 :&: _) = do
    shapeInfo <- projShapeCheck c
    let eff = do
          (eps1, delt1) <- c1 ^. is_cmd
          (eps2, delt2) <- c2 ^. is_cmd
          return (eps1 + eps2, delt1 + delt2)
    return $ defaultInfo & shape_info .~ shapeInfo
                         & is_cmd .~ eff

  specSensCheck c@(CSkip :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ defaultInfo & shape_info .~ shapeInfo
                         & is_cmd .~ (return (0, 0))

instance SpecSensCheck (CTCHint :&: Position) where
  specSensCheck (CTCHint extName params body :&: p) =
    throwM $ InternalError p "Not yet implemented"

runSpecSensCheck :: ShapeCxt
                 -> SensCxt
                 -> Term ImpTCP
                 -> Either SomeException (SensCxt, Eps, Delta)
runSpecSensCheck shapeCxt sensCxt term = run $ do
  check <- cataM specSensCheck term
  ((eps, delt), sensCxt') <- runStateT (check ^. is_cmd) sensCxt
  return (sensCxt', eps, delt)
  where run = (flip runContT return) . (flip runReaderT shapeCxt)
