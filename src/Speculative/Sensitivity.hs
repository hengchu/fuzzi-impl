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
import Shape hiding (defaultEInfo, defaultCInfo, ShapeCheckError(..))
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

data SpecSensInfo m =
  ESpecSensInfo {
  _specsensinfo_sens         :: m (Maybe Float)
  , _specsensinfo_shape_info :: ShapeInfo
  }
  | CSpecSensInfo {
      _specsensinfo_eps_delta    :: m (Eps, Delta)
      , _specsensinfo_shape_info :: ShapeInfo
      }

$(makeLensesWith underscoreFields ''SpecSensInfo)
$(makePrisms ''SpecSensInfo)

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

defaultEInfo :: (Monad m) => SpecSensInfo (StateT SensCxt m)
defaultEInfo = ESpecSensInfo
                 (return Nothing)
                 S.defaultEInfo

defaultCInfo :: (Monad m) => SpecSensInfo (StateT SensCxt m)
defaultCInfo = CSpecSensInfo
                 (return (0, 0))
                 S.defaultCInfo

projShapeCheck :: ( MonadThrow m
                  , MonadReader ShapeCxt m
                  , MonadCont m
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
    let info = defaultEInfo & shape_info .~ shapeInfo
    let eAct = do { cxt <- get; return $ M.lookup v (getSensCxt cxt) }
    return $ info & sens .~ eAct

  specSensCheck c@(ELength innerInfo :&: p) = do
    shapeInfo <- projShapeCheck c

    let info = defaultEInfo & shape_info .~ shapeInfo

    -- when the inner expression is a literal
    info' <- callCC $ \ret -> do
      case (project @ExprP (shapeInfo ^. term)) of
        Just (ELit (LArr _) :&: _) -> ret $ info & sens .~ (return $ Just 0)
        Just (ELit (LBag _) :&: _) -> ret $ info & sens .~ (return $ Just 0)
        _ -> return ()

      -- is it a fixed-length array?
      case (innerInfo ^? shape_info . tau . fixed_length . _Just) of
        Just _ -> ret $ info & sens .~ (return $ Just 0)
        _ -> return ()

      -- check the inner expression's sensitivity
      case (innerInfo ^? shape_info . tau, innerInfo ^? sens) of
        (Just (TBag _), Just s) -> ret $ info & sens .~ s
        (Just (TArr _ _), Just s) -> do
          let s' = do
                sarr <- s
                case sarr of
                  Just _ -> return $ Just 0
                  Nothing -> return Nothing
          ret $ info & sens .~ s'
        _ -> return ()

      throwM $ InternalError p "bug: shape checker didn't rule out bad length expression"

    return info'

  specSensCheck c@(ELit (LInt _) :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ defaultEInfo & sens .~ (return $ Just 0)
                          & shape_info .~ shapeInfo

  specSensCheck c@(ELit (LFloat _) :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ defaultEInfo & sens       .~ return (Just 0)
                          & shape_info .~ shapeInfo

  specSensCheck c@(ELit (LBool _) :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ defaultEInfo & sens       .~ return (Just 0)
                          & shape_info .~ shapeInfo


  specSensCheck c@(ELit (LArr infos) :&: p) = callCC $ \ret -> do
    shapeInfo <- projShapeCheck c
    let info = defaultEInfo & shape_info .~ shapeInfo
    -- if this is a literal array with literal contents
    when (all isJust $ map (preview sens) infos) $ do
      case infos ^.. traverse . sens of
        innerSens -> do
          let eSens = do
                sensitivities <- sequenceA innerSens
                return $ getSens $ foldMap Sens sensitivities
          ret $ info & sens .~ eSens

    throwM $ InternalError p "bug: shape checker let through bad literal array"

  specSensCheck c@(ELit (LBag infos) :&: p) = callCC $ \ret -> do
    shapeInfo <- projShapeCheck c
    let info = defaultEInfo & shape_info .~ shapeInfo
    -- if this is a literal array with literal contents
    when (all isJust $ map (preview sens) infos) $ do
      case infos ^.. traverse . sens of
        innerSens -> do
          let eSens = do
                sensitivities <- sequenceA innerSens
                return $ Just $ 2 * (fromIntegral . length . filter (> Just 0) $ sensitivities)
          ret $ info & sens .~ eSens

    throwM $ InternalError p "bug: shape checker let through bad literal bag"

  specSensCheck c@(EBinop op linfo rinfo :&: p)
    | op == PLUS || op == MINUS = do
        shapeInfo <- projShapeCheck c
        case (linfo ^? sens, rinfo ^? sens) of
          (Just lsens, Just rsens) -> do
            let eSens = do
                  ls <- lsens
                  rs <- rsens
                  return $ (+) <$> ls <*> rs
            return $ defaultEInfo & sens       .~ eSens
                                  & shape_info .~ shapeInfo
          _ -> throwM $ InternalError p "bug: shape checker didn't rule out bad arith"

    | op == MULT = do
        shapeInfo <- projShapeCheck c

        case (project @ExprP (linfo ^. shape_info . term),
              project @ExprP (rinfo ^. shape_info . term),
              linfo ^? sens,
              rinfo ^? sens) of
          (Just (ELit (LInt k) :&: _), Just _,
           Just _, Just rsens) ->
            return $ defaultEInfo & shape_info .~ shapeInfo
                                  & sens .~ (rsens >>= \rs -> return $ (* fromIntegral k) <$> rs)
          (Just (ELit (LFloat k) :&: _), Just _,
           Just _, Just rsens) ->
            return $ defaultEInfo & shape_info .~ shapeInfo
                                  & sens .~ (rsens >>= \rs -> return $ (* k) <$> rs)
          (Just _, Just (ELit (LInt k) :&: _),
           Just lsens, Just _) ->
            return $ defaultEInfo & shape_info .~ shapeInfo
                                  & sens .~ (lsens >>= \ls -> return $ (* fromIntegral k) <$> ls)
          (Just _, Just (ELit (LFloat k) :&: _),
           Just lsens, Just _) ->
            return $ defaultEInfo & shape_info .~ shapeInfo
                                  & sens .~ (lsens >>= \ls -> return $ (* k) <$> ls)
          (Just _, Just _,
           Just lsens, Just rsens) ->
            return $ defaultEInfo & shape_info .~ shapeInfo
                                  & sens .~ (lsens >>= \ls -> rsens >>= \rs -> return $ (*) <$> ls <*> rs)
          _ -> throwM $ InternalError p "bug: shape checker let through bad multiplication"

    | op == DIV = do
        shapeInfo <- projShapeCheck c

        case (project @ExprP (rinfo ^. shape_info . term),
              linfo ^? sens,
              rinfo ^? sens) of
          (Just (ELit (LInt k) :&: _),
           Just lsens,
           Just _) ->
            return $ defaultEInfo & shape_info .~ shapeInfo
                                  & sens .~ (lsens >>= \ls -> return $ (/ fromIntegral k) <$> ls)
          (Just (ELit (LFloat k) :&: _),
           Just lsens,
           Just _) ->
            return $ defaultEInfo & shape_info .~ shapeInfo
                                  & sens .~ (lsens >>= \ls -> return $ (/ k) <$> ls)
          (Just _,
           Just lsens,
           Just rsens) ->
            return $ defaultEInfo & shape_info .~ shapeInfo
                                  & sens .~ (approx <$> lsens <*> rsens)
          _ -> throwM $ InternalError p "bug: shape checker let through bad division"

    | otherwise = do
        shapeInfo <- projShapeCheck c

        case (linfo ^? sens, rinfo ^? sens) of
          (Just lsens, Just rsens) ->
            return $ defaultEInfo & sens .~ (approx <$> lsens <*> rsens)
                                  & shape_info .~ shapeInfo
          _ ->
            throwM $ InternalError p "bug: shape checker let through bad binop expr"

  specSensCheck c@(EIndex linfo rinfo :&: p) = do
    shapeInfo <- projShapeCheck c
    case (linfo ^? shape_info . tau, linfo ^? sens, rinfo ^? sens) of
      (Just (TArr _ len), Just lsens, Just rsens) -> do
        let eSens = do
              ls <- lsens
              rs <- rsens
              case (ls, rs) of
                (Just _,  Just 0) -> lsens
                (Nothing, _) ->
                  case (len, projectELInt (rinfo ^. shape_info . term)) of
                    (Just len', Just k)
                      | 0 <= k && k < len' -> return Nothing
                    _ -> throwM $ MayNotCoterminate p
                _ -> throwM $ MayNotCoterminate p
        return $ defaultEInfo & shape_info .~ shapeInfo
                              & sens .~ eSens
      (Just (TBag _), Just lsens, Just rsens) -> do
        let eSens = do
              ls <- lsens
              rs <- rsens
              case (ls, rs) of
                (Just 0, Just 0) -> return Nothing
                _ -> throwM $ MayNotCoterminate p
        return $ defaultEInfo & shape_info .~ shapeInfo
                              & sens .~ eSens
      _ -> throwM $ InternalError p "bug: shape checker let through bad index expr"

  specSensCheck c@(EFloat info :&: p) = do
    shapeInfo <- projShapeCheck c
    case info ^? sens of
      Just innerSens ->
        return $ defaultEInfo & sens .~ innerSens
                              & shape_info .~ shapeInfo
      _ -> throwM $ InternalError p "bug: shape checker let through bad float cast"

  specSensCheck c@(EExp info :&: p) = do
    shapeInfo <- projShapeCheck c
    case info ^? sens of
      Just innerSens ->
        return $ defaultEInfo & sens .~ (approx1 <$> innerSens)
                              & shape_info .~ shapeInfo
      _ -> throwM $ InternalError p "bug: shape checker let through bad exponentiation expr"

  specSensCheck c@(ELog info :&: p) = do
    shapeInfo <- projShapeCheck c
    case info ^? sens of
      Just innerSens -> return $ defaultEInfo & sens .~ (approx1 <$> innerSens)
                                              & shape_info .~ shapeInfo
      _ -> throwM $ InternalError p "bug: shape checker let through bad log expr"

  specSensCheck c@(EClip info litInfo :&: p) = do
    shapeInfo <- projShapeCheck c
    case (info ^? sens, litInfo) of
      (Just lsens, LInt k) ->
        return $ defaultEInfo & sens .~ (smin <$> lsens <*> (return . Just $ realToFrac $ 2 * k))
                              & shape_info .~ shapeInfo
      (Just lsens, LFloat k) ->
        return $ defaultEInfo & sens .~ (smin <$> lsens <*> (return . Just $ 2 * k))
                              & shape_info .~ shapeInfo
      _ -> throwM $ InternalError p "bug: shape checker let through bad clip"

  specSensCheck c@(EScale scalarInfo vecInfo :&: p) = do
    shapeInfo <- projShapeCheck c
    case (scalarInfo ^? sens, vecInfo ^? sens) of
      (Just lsens, Just rsens) ->
        return $ defaultEInfo & shape_info .~ shapeInfo
                              & sens .~ (lsens >>= \ls -> rsens >>= \rs -> return $ (*) <$> ls <*> rs)
      _ -> throwM $ InternalError p "bug: shape checker let through bad scale"

  specSensCheck c@(EDot linfo rinfo :&: p) = do
    shapeInfo <- projShapeCheck c
    case (linfo ^? sens, rinfo ^? sens) of
      (Just lsens, Just rsens) ->
        return $ defaultEInfo & sens .~ (approx <$> lsens <*> rsens)
                              & shape_info .~ shapeInfo
      _ -> throwM $ InternalError p "bug: shape checker let through bad dot expr"


projectEVar :: Term ImpTCP -> Maybe Var
projectEVar t =
  case project @ExprP t of
    Just (EVar x :&: _) -> Just x
    _ -> Nothing

projectELInt :: Term ImpTCP -> Maybe Int
projectELInt t =
  case project @ExprP t of
    Just (ELit (LInt v) :&: _) -> Just v
    _ -> Nothing

projectELength :: Term ImpTCP -> Maybe (Term ImpTCP)
projectELength t =
  case project @ExprP t of
    Just (ELength e :&: _) -> Just e
    _ -> Nothing

instance SpecSensCheck (Cmd :&: Position) where
  specSensCheck c@(CAssign lhs rhs :&: p) = do
    shapeInfo <- projShapeCheck c

    case (project @ExprP (lhs ^. shape_info . term),
          lhs ^? sens,
          rhs ^? sens) of
      (Just (EVar x :&: _),
       Just _,
       Just rsens) -> do
        let epsDelta = do
              cxt <- getSensCxt <$> get
              rs <- rsens
              put $ SensCxt $ M.update (const rs) x cxt
              return (0, 0)
        return $ defaultCInfo & shape_info .~ shapeInfo
                              & eps_delta  .~ epsDelta
      (Just (EIndex (projectEVar -> Just x) _ :&: _),
       Just lsens,
       Just rsens) -> do
        let epsDelta = do
              shapeCxt <- getShapeCxt <$> ask
              cxt <- getSensCxt <$> get
              ls <- lsens
              rs <- rsens
              case M.lookup x shapeCxt of
                Just (TArr _ _) ->
                  put $ SensCxt $ M.update (const $ (+) <$> ls <*> rs) x cxt
                Just (TBag _) ->
                  put $ SensCxt $ M.update (const Nothing) x cxt
                _ -> throwM $ InternalError p "bug: shape checker let through bad indexed assignment"
              return (0, 0)
        return $ defaultCInfo & shape_info .~ shapeInfo
                              & eps_delta .~ epsDelta
      (Just (ELength (projectEVar -> Just x) :&: _),
       Just _,
       Just rsens) -> do
        let epsDelta = do
              shapeCxt <- getShapeCxt <$> ask
              cxt <- getSensCxt <$> get
              rs <- rsens
              case (M.lookup x shapeCxt, rs) of
                (Just (TArr _ _), Just 0) -> return ()
                (Just (TArr _ _), _)
                  | isJust $ projectELength (rhs ^. shape_info . term) ->
                    put $ SensCxt $ M.update (const Nothing) x cxt
                  | otherwise -> throwM $ MayNotCoterminate p
                (Just (TBag _),   Just 0) -> put $ SensCxt $ M.update (const Nothing) x cxt
                (Just (TBag _),   _)
                  | isJust $ projectELength (rhs ^. shape_info . term) ->
                    put $ SensCxt $ M.update (const Nothing) x cxt
                  | otherwise -> throwM $ MayNotCoterminate p
                _ -> throwM $ InternalError p "bug: shape checker let through bad length assignment"
              return (0, 0)
        return $ defaultCInfo & shape_info .~ shapeInfo
                              & eps_delta .~ epsDelta
      _ -> throwM $ InternalError p "bug: shape checker let through bad assignment"

  specSensCheck c@(CLaplace lhs w rhs :&: p) = do
    shapeInfo <- projShapeCheck c

    case (projectEVar (lhs ^. shape_info . term),
          rhs ^? sens) of
      (Just x, Just rsens) -> do
        let epsDelta = do
              rs <- rsens
              cxt <- getSensCxt <$> get
              case rs of
                Just finiteRs -> do
                  put $ SensCxt $ M.insert x 0 cxt
                  return (finiteRs / w, 0)
                Nothing -> throwM $ CannotReleaseInfSensData p
        return $ defaultCInfo & shape_info .~ shapeInfo
                              & eps_delta  .~ epsDelta
      _ -> throwM $ InternalError p "bug: shape checker let through bad laplace"

  specSensCheck c@(CIf e c1 c2 :&: p) = do
    shapeInfo <- projShapeCheck c

    case (e ^? sens, c1 ^? eps_delta, c2 ^? eps_delta) of
      (Just esens, Just c1ED, Just c2ED) -> do
        let epsDelta = do
              es <- esens
              case es of
                Just 0 -> do
                  currSt <- get

                  (eps1, delt1) <- c1ED
                  st1 <- get

                  put currSt
                  (eps2, delt2) <- c2ED
                  st2 <- get

                  put $ scxtMax st1 st2

                  return (max eps1 eps2, max delt1 delt2)
                _ -> throwM $ CannotBranchOnSensData p
        return $ defaultCInfo & shape_info .~ shapeInfo
                              & eps_delta .~ epsDelta
      _ -> throwM $ InternalError p "bug: shape checker let through bad if command"

  specSensCheck c@(CWhile e body :&: p) = do
    shapeInfo <- projShapeCheck c

    case (e ^? sens, body ^? eps_delta) of
      (Just esens, Just bodyED) -> do
        let epsDelta = do
              es <- esens
              case es of
                Just 0 -> do
                  currSt <- get
                  (eps, delt) <- bodyED
                  st <- get
                  put currSt
                  if eps == 0 && delt == 0 && st == currSt
                  then return (0, 0)
                  else throwM $ CannotEstablishInvariant p
                _ -> throwM $ CannotBranchOnSensData p
        return $ defaultCInfo & shape_info .~ shapeInfo
                              & eps_delta .~ epsDelta
      _ -> throwM $ InternalError p "bug: shape checker let through bad while command"

  specSensCheck c@(CSeq c1 c2 :&: p) = do
    shapeInfo <- projShapeCheck c
    case (c1 ^? eps_delta, c2 ^? eps_delta) of
      (Just c1ED, Just c2ED) -> do
        let epsDelta = do
              (eps1, delt1) <- c1ED
              (eps2, delt2) <- c2ED
              return (eps1 + eps2, delt1 + delt2)
        return $ defaultCInfo & shape_info .~ shapeInfo
                              & eps_delta .~ epsDelta
      _ -> throwM $ InternalError p "bug: shape checker let through bad sequence"

  specSensCheck c@(CSkip :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ defaultCInfo & shape_info .~ shapeInfo
                          & eps_delta .~ (return (0, 0))

instance SpecSensCheck (CTCHint :&: Position) where
  specSensCheck (CTCHint extName params body :&: p) =
    throwM $ InternalError p "Not yet implemented"

runSpecSensCheck :: ShapeCxt
                 -> SensCxt
                 -> Term ImpTCP
                 -> Either SomeException (SensCxt, Eps, Delta)
runSpecSensCheck shapeCxt sensCxt term = run $ do
  let (_, p) = projectA @ImpTC @Position (unTerm term)
  check <- cataM specSensCheck term
  case check ^? eps_delta of
    Just cmdCheck -> do
      ((eps, delt), sensCxt') <- runStateT cmdCheck sensCxt
      return (sensCxt', eps, delt)
    _ -> throwM $ S.ExpectCmd p
  where run = (flip runContT return) . (flip runReaderT shapeCxt)
