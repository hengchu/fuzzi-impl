-- |This module implements the composed checker that "zips" together the
-- speculative sensitivity checker, the affine checker and the termination
-- checker.
module Composed where

import qualified Data.Map as M
import Data.Comp
import Data.Comp.Derive
import Algebra
import SyntaxExt

import Shape
import Speculative.Sensitivity hiding (static_values)
import Termination hiding (shape_info)
import Affine hiding (shape_info)

import Data.Typeable

import Control.Lens
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Catch

import Debug.Trace

-- |'ComposedInfo' collects information from all 3 checkers and lump them into a
-- single value.
data ComposedInfo m =
  ComposedInfo {
  _composedinfo_specsens :: SpecSensInfo (StateT SensCxt m)
  , _composedinfo_affineinfo :: AffineInfo
  , _composedinfo_terminfo :: TerminationInfo m
  }

$(makeLensesWith underscoreFields ''ComposedInfo)

-- |Errors that may result from composition. This is in addition to each of the
-- possible errors that may be thrown by the sub checkers.
data ComposedCheckInfo
  = MapBodyMayNotTerminate Position
  | MapBodyIsNotAffine Position
  | AdvCompBodyMayNotTerminate Position
  deriving (Show, Eq, Typeable)

instance Exception ComposedCheckInfo

-- |Calls sensitivity checker on the program fragment.
projSensCheck :: ( MonadThrow m
                 , MonadCont m
                 , Functor f
                 , SpecSensCheck f
                 , MonadReader ShapeCxt m)
              => f (ComposedInfo m) -> m (SpecSensInfo (StateT SensCxt m))
projSensCheck fa = specSensCheck (fmap (view specsens) fa)

-- |Calls affine checker on the program fragment.
projAffineCheck :: ( MonadReader ShapeCxt m
                   , MonadCont m
                   , MonadThrow m
                   , AffineCheck f
                   , Functor f)
                => f (ComposedInfo n)
                -> m AffineInfo
projAffineCheck fa = affineCheck (fmap (view affineinfo) fa)

-- |Calls termination checker on the program fragment.
projTermCheck :: ( MonadReader ShapeCxt m
                 , MonadCont m
                 , MonadThrow m
                 , TerminationCheck f
                 , Functor f
                 , MonadState TerminationCxt m) => f (ComposedInfo m) -> m (TerminationInfo m)
projTermCheck fa = terminationCheck (fmap (view terminfo) fa)

class ComposedCheck f where
  composedCheck :: ( MonadCont m
                   , MonadReader ShapeCxt m
                   , MonadThrow m
                   , MonadState TerminationCxt m)
                => AlgM m f (ComposedInfo m)

$(derive [liftSum] [''ComposedCheck])

-- |The free composition of the provided type information.
freeComp :: (Monad m)
         => (SpecSensInfo (StateT SensCxt m),
             AffineInfo,
             TerminationInfo m) -> m (ComposedInfo m)
freeComp (v1, v2, v3) = return $ ComposedInfo v1 v2 v3

-- |Straightforward decomposition of the provided type information.
decomp :: (Monad m)
       => ComposedInfo m
       -> m (SpecSensInfo (StateT SensCxt m),
             AffineInfo,
             TerminationInfo m)
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

    bodyTerminates <- bodyInfo ^. terminfo . terminates

    case bodyTerminates of
      True -> return $ ComposedInfo specsensInfo affineInfo' termInfo
      False -> return $ ComposedInfo specsensInfo affineInfo' (termInfo & extterms %~ (("bmap", p, False):))

  composedCheck c@(CTCHint "amap" [_, _, _, _, _, bodyInfo] _ :&: p) = do
    specsensInfo <- projSensCheck c
    affineInfo <- projAffineCheck c
    termInfo <- projTermCheck c

    let affineInfo' = affineInfo & affine .~ True
    let termInfo'   = termInfo   & terminates .~ return True

    bodyTerminates <- bodyInfo ^. terminfo . terminates

    case (bodyTerminates,
          bodyInfo ^? affineinfo . affine) of
      (True, Just True)  -> return $ ComposedInfo specsensInfo affineInfo' termInfo'
      (False, _)         -> return $ ComposedInfo specsensInfo affineInfo' (termInfo & extterms %~ (("amap", p, False):))
      (_,    Just False) -> throwM $ MapBodyIsNotAffine p
      _                  -> throwM $ ExpectCmd p

  composedCheck c@(CTCHint "partition" [_, _, _, _, _, _, _, _, _, bodyInfo] _ :&: p) = do
    specsensInfo <- projSensCheck c
    affineInfo <- projAffineCheck c
    termInfo <- projTermCheck c

    let affineInfo' = affineInfo & affine .~ True
    let termInfo'   = termInfo   & terminates .~ return True

    bodyTerminates <- bodyInfo ^. terminfo . terminates

    case bodyTerminates of
      True  -> return $ ComposedInfo specsensInfo affineInfo' termInfo'
      False -> return $ ComposedInfo specsensInfo affineInfo' (termInfo & extterms %~ (("partition", p, False):))

  composedCheck c@(CTCHint "bsum" _ _ :&: _) = do
    specsensInfo <- projSensCheck c
    affineInfo <- projAffineCheck c
    termInfo <- projTermCheck c

    let affineInfo' = affineInfo & affine .~ True
    let termInfo'   = termInfo   & terminates .~ return True

    return $ ComposedInfo specsensInfo affineInfo' termInfo'

  composedCheck c@(CTCHint "ac" [_, _, _, bodyInfo] _ :&: p) = do
    specsensInfo <- projSensCheck c
    affineInfo <- projAffineCheck c
    termInfo <- projTermCheck c

    let affineInfo' = affineInfo & affine .~ True
    let termInfo'   = termInfo   & terminates .~ return True

    bodyTerminates <- bodyInfo ^. terminfo . terminates

    case bodyTerminates of
      True -> return $ ComposedInfo specsensInfo affineInfo' termInfo'
      False -> return $ ComposedInfo specsensInfo affineInfo' (termInfo & extterms %~ (("ac", p, False):))

  composedCheck c = do
    specsensInfo <- projSensCheck c
    affineInfo   <- projAffineCheck c
    termInfo     <- projTermCheck c

    return $ ComposedInfo specsensInfo affineInfo termInfo

-- |Convenient function to run the composed type checker given a starting shape
-- context and sensitivity context.
runComposedChecker :: ShapeCxt -> SensCxt -> Term ImpTCP -> Either SomeException (SensCxt, Eps, Delta)
runComposedChecker shapeCxt sensCxt cmd = run $ do
  let (_, p) = projectA @ImpTC @Position (unTerm cmd)
  check <- cataM composedCheck cmd
  case filter (\triple -> not $ triple ^. _3) $ check ^. terminfo . extterms of
    (("bmap", pos, _) : _) -> throwM $ MapBodyMayNotTerminate pos
    (("amap", pos, _) : _) -> throwM $ MapBodyMayNotTerminate pos
    (("partition", pos, _) : _) -> throwM $ MapBodyMayNotTerminate pos
    (("ac", pos, _) : _) -> throwM $ AdvCompBodyMayNotTerminate pos
    _ -> return ()
  case check ^? specsens . eps_delta of
    Just cmdCheck -> do
      ((eps, delt), sensCxt') <- runStateT cmdCheck sensCxt
      return (sensCxt', eps, delt)
    _ -> throwM $ ExpectCmd p
  where run = (flip runContT return)
              . (flip runReaderT shapeCxt)
              . (flip evalStateT (TerminationCxt M.empty))
