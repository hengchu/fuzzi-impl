module Speculative.Sensitivity where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error

import Algebra
import Control.Lens
import Data.Comp
import Data.Comp.Derive
import SyntaxExt
import Shape hiding (defaultInfo)
import qualified Shape as S (defaultInfo)

data SpecSensInfo = SpecSensInfo {
  _specsensinfo_expr_sens :: Maybe Float
  , _specsensinfo_shape_info :: ShapeInfo
  } deriving (Show, Eq)

$(makeLensesWith underscoreFields ''SpecSensInfo)

data SpecSensCheckError = Foobar
  deriving (Show, Eq)

defaultInfo :: SpecSensInfo
defaultInfo = SpecSensInfo Nothing S.defaultInfo

class SpecSensCheck f where
  specSensCheck :: (MonadError e m, Inj e SpecSensCheckError)
                => AlgM m f SpecSensInfo

$(derive [liftSum] [''SpecSensCheck])

instance SpecSensCheck (Expr :&: Position) where

instance SpecSensCheck (Cmd :&: Position) where

instance SpecSensCheck (CTCHint :&: Position) where

specSensImpTCP :: forall e m.
                  (MonadError e m, Inj e SpecSensCheckError)
               => AlgM m ImpTCP SpecSensInfo
specSensImpTCP = specSensCheck

data CompResult = Foobar2

comp :: (Monad m) => (ShapeInfo, SpecSensInfo) -> m CompResult
comp = undefined

decomp :: (Monad m) => CompResult -> m (ShapeInfo, SpecSensInfo)
decomp = undefined

compImpTCP :: forall e m.
              (MonadError e m, Inj e SpecSensCheckError, Inj e ShapeCheckError, MonadReader ShapeCxt m)
              => AlgM m ImpTCP CompResult
compImpTCP = buildAlgM comp decomp (prodM shapeImpTCP specSensImpTCP)
