module Composed where

import Data.Comp
import Data.Comp.Derive
import Algebra
import SyntaxExt

import Shape
import Speculative.Sensitivity
import Termination hiding (shape_info)
import Affine hiding (shape_info)

import Data.Typeable

import Control.Lens
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Catch

data ComposedInfo m =
  ComposedInfo {
  _composedinfo_specsens :: SpecSensInfo m
  , _composedinfo_affineinfo :: AffineInfo
  , _composedinfo_terminfo :: TerminationInfo
  }

$(makeLensesWith underscoreFields ''ComposedInfo)

data ComposedCheckInfo = MapBodyMayNotTerminate Position
                       | MapBodyIsNotAffine Position
                       | AdvCompBodyMayNotTerminate Position
  deriving (Show, Eq, Typeable)

instance Exception ComposedCheckInfo

projSensCheck :: ( MonadThrow m
                 , MonadCont m
                 , Functor f
                 , SpecSensCheck f
                 , MonadReader ShapeCxt m)
              => f (ComposedInfo (StateT SensCxt m)) -> m (SpecSensInfo (StateT SensCxt m))
projSensCheck fa = specSensCheck (fmap (view specsens) fa)

projAffineCheck :: ( MonadReader ShapeCxt m
                   , MonadCont m
                   , MonadThrow m
                   , AffineCheck f
                   , Functor f)
                => f (ComposedInfo n)
                -> m AffineInfo
projAffineCheck fa = affineCheck (fmap (view affineinfo) fa)

projTermCheck :: ( MonadReader ShapeCxt m
                 , MonadCont m
                 , MonadThrow m
                 , TerminationCheck f
                 , Functor f) => f (ComposedInfo n) -> m TerminationInfo
projTermCheck fa = terminationCheck (fmap (view terminfo) fa)

class ComposedCheck f where
  composedCheck :: ( MonadCont m
                   , MonadReader ShapeCxt m
                   , MonadThrow m)
                => AlgM m f (ComposedInfo (StateT SensCxt m))

$(derive [liftSum] [''ComposedCheck])

freeComp :: (Monad m) => (SpecSensInfo n, AffineInfo, TerminationInfo) -> m (ComposedInfo n)
freeComp (v1, v2, v3) = return $ ComposedInfo v1 v2 v3

decomp :: (Monad m) => ComposedInfo n -> m (SpecSensInfo n, AffineInfo, TerminationInfo)
decomp (ComposedInfo v1 v2 v3) = return (v1, v2, v3)

instance ComposedCheck (Expr :&: Position) where
  composedCheck =
    buildAlgM freeComp decomp (prodM3 specSensCheck affineCheck terminationCheck)

instance ComposedCheck (Cmd :&: Position) where
  composedCheck =
    buildAlgM freeComp decomp (prodM3 specSensCheck affineCheck terminationCheck)

instance ComposedCheck (CTCHint :&: Position) where
  composedCheck c@(CTCHint "bmap" [_, _, _, _, _, bodyInfo] _ :&: p) = do
    specsensInfo <- projSensCheck c
    affineInfo   <- projAffineCheck c
    termInfo     <- projTermCheck c

    let affineInfo' = affineInfo & affine .~ True
    let termInfo'   = termInfo   & terminates .~ True

    case bodyInfo ^. terminfo . terminates of
      True -> return $ ComposedInfo specsensInfo affineInfo' termInfo'
      False -> throwM $ MapBodyMayNotTerminate p

  composedCheck c@(CTCHint "amap" [_, _, _, _, _, bodyInfo] _ :&: p) = do
    specsensInfo <- projSensCheck c
    affineInfo <- projAffineCheck c
    termInfo <- projTermCheck c

    let affineInfo' = affineInfo & affine .~ True
    let termInfo'   = termInfo   & terminates .~ True

    case (bodyInfo ^. terminfo . terminates,
          bodyInfo ^? affineinfo . affine) of
      (True, Just True)  -> return $ ComposedInfo specsensInfo affineInfo' termInfo'
      (False, _)         -> throwM $ MapBodyMayNotTerminate p
      (_,    Just False) -> throwM $ MapBodyIsNotAffine p
      _                  -> throwM $ ExpectCmd p

  composedCheck c@(CTCHint "partition" [_, _, _, _, _, _, _, _, _, bodyInfo] _ :&: p) = do
    specsensInfo <- projSensCheck c
    affineInfo <- projAffineCheck c
    termInfo <- projTermCheck c

    let affineInfo' = affineInfo & affine .~ True
    let termInfo'   = termInfo   & terminates .~ True

    case bodyInfo ^. terminfo . terminates of
      True  -> return $ ComposedInfo specsensInfo affineInfo' termInfo'
      False -> throwM $ MapBodyMayNotTerminate p

  composedCheck c@(CTCHint "bsum" _ _ :&: _) = do
    specsensInfo <- projSensCheck c
    affineInfo <- projAffineCheck c
    termInfo <- projTermCheck c

    let affineInfo' = affineInfo & affine .~ True
    let termInfo'   = termInfo   & terminates .~ True

    return $ ComposedInfo specsensInfo affineInfo' termInfo'

  composedCheck c@(CTCHint "repeat" [_, _, bodyInfo] _ :&: _) = do
    specsensInfo <- projSensCheck c
    affineInfo <- projAffineCheck c
    termInfo <- projTermCheck c

    let affineInfo' = affineInfo & affine .~ True
    let termInfo'   = termInfo   & terminates .~ (bodyInfo ^. terminfo . terminates)

    return $ ComposedInfo specsensInfo affineInfo' termInfo'

  composedCheck c@(CTCHint "ac" [_, _, _, bodyInfo] _ :&: p) = do
    specsensInfo <- projSensCheck c
    affineInfo <- projAffineCheck c
    termInfo <- projTermCheck c

    let affineInfo' = affineInfo & affine .~ True
    let termInfo'   = termInfo   & terminates .~ True

    case bodyInfo ^. terminfo . terminates of
      True -> return $ ComposedInfo specsensInfo affineInfo' termInfo'
      False -> throwM $ AdvCompBodyMayNotTerminate p

  composedCheck c = do
    specsensInfo <- projSensCheck c
    affineInfo <- projAffineCheck c
    termInfo <- projTermCheck c

    return $ ComposedInfo specsensInfo affineInfo termInfo

runComposedChecker :: ShapeCxt -> SensCxt -> Term ImpTCP -> Either SomeException (SensCxt, Eps, Delta)
runComposedChecker shapeCxt sensCxt cmd = run $ do
  let (_, p) = projectA @ImpTC @Position (unTerm cmd)
  check <- cataM composedCheck cmd
  case check ^? specsens . eps_delta of
    Just cmdCheck -> do
      ((eps, delt), sensCxt') <- runStateT cmdCheck sensCxt
      return (sensCxt', eps, delt)
    _ -> throwM $ ExpectCmd p
  where run = (flip runContT return) . (flip runReaderT shapeCxt)
