module Composed where

import Data.Comp
import Data.Comp.Derive
import Algebra
import SyntaxExt

import Shape
import Speculative.Sensitivity
import Termination
import Affine

import Control.Lens
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Catch

data ComposedInfo m =
  ComposedInfo {
  _composedinfo_sens :: SpecSensInfo m
  , _composedinfo_affine :: AffineInfo
  , _composedinfo_term :: TerminationInfo
  }

$(makeLensesWith underscoreFields ''ComposedInfo)

class ComposedCheck f where
  composedCheck :: (MonadCont m, MonadReader ShapeCxt m, MonadThrow m)
                => AlgM m f (ComposedInfo (StateT SensCxt m))

$(derive [liftSum] [''ComposedCheck])

comp :: (Monad m) => (SpecSensInfo n, AffineInfo, TerminationInfo) -> m (ComposedInfo n)
comp (v1, v2, v3) = return $ ComposedInfo v1 v2 v3

decomp :: (Monad m) => ComposedInfo n -> m (SpecSensInfo n, AffineInfo, TerminationInfo)
decomp (ComposedInfo v1 v2 v3) = return (v1, v2, v3)

instance ComposedCheck (Expr :&: Position) where
  composedCheck =
    buildAlgM comp decomp (prodM3 specSensCheck affineCheck terminationCheck)

instance ComposedCheck (Cmd :&: Position) where
  composedCheck =
    buildAlgM comp decomp (prodM3 specSensCheck affineCheck terminationCheck)

instance ComposedCheck (CTCHint :&: Position) where
  composedCheck =
    buildAlgM comp decomp (prodM3 specSensCheck affineCheck terminationCheck)

runComposedChecker :: ShapeCxt -> SensCxt -> Term ImpTCP -> Either SomeException (SensCxt, Eps, Delta)
runComposedChecker shapeCxt sensCxt cmd = run $ do
  let (_, p) = projectA @ImpTC @Position (unTerm cmd)
  check <- cataM composedCheck cmd
  case check ^? sens . eps_delta of
    Just cmdCheck -> do
      ((eps, delt), sensCxt') <- runStateT cmdCheck sensCxt
      return (sensCxt', eps, delt)
    _ -> throwM $ ExpectCmd p
  where run = (flip runContT return) . (flip runReaderT shapeCxt)
