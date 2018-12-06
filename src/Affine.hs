module Affine where

import Type.Reflection
import Control.Lens
import Data.Comp
import Data.Comp.Derive
import SyntaxExt

import Shape hiding (ShapeCheckError(..))
import Control.Monad.Reader
import Control.Monad.Catch
import Control.Monad.Cont

data AffineInfo =
  EAffineInfo {
  _affineinfo_shape_info :: ShapeInfo
  }
  | CAffineInfo {
      _affineinfo_shape_info :: ShapeInfo
      , _affineinfo_affine :: Bool
      }

$(makeLensesWith underscoreFields ''AffineInfo)
$(makePrisms ''AffineInfo)

data AffineCheckError = InternalError Position String
  deriving (Show, Typeable, Eq)

instance Exception AffineCheckError

projShapeCheck :: ( MonadThrow m
                  , MonadReader ShapeCxt m
                  , MonadCont m
                  , Functor f
                  , ShapeCheck f
                  )
               => f AffineInfo -> m ShapeInfo
projShapeCheck fa =
  shapeCheck (fmap (view shape_info) fa)

-- we have a slightly funny definition of affine:
-- a command c is affine if
-- given {G1}   c {G2} and any k > 0
--       {k G1} c {k G2, (0, 0)} also holds
-- this is true for simple straight light programs + while loops
-- extensions are case specific
class AffineCheck f where
  affineCheck :: (MonadThrow m, MonadReader ShapeCxt m, MonadCont m) => AlgM m f AffineInfo

$(derive [liftSum] [''AffineCheck])

instance AffineCheck (Expr :&: Position) where
  affineCheck f = EAffineInfo <$> projShapeCheck f

instance AffineCheck (Cmd :&: Position) where
  affineCheck c@(CIf _ _ _ :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ CAffineInfo shapeInfo False

  affineCheck c@(CAssign _ _ :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ CAffineInfo shapeInfo True

  affineCheck c@(CLaplace _ _ _ :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ CAffineInfo shapeInfo False

  affineCheck c@(CWhile _ body :&: _) = do
    shapeInfo <- projShapeCheck c
    case body ^? affine of
      Just True -> return $ CAffineInfo shapeInfo True
      _ -> return $ CAffineInfo shapeInfo False

  affineCheck c@(CSeq c1 c2 :&: _) = do
    shapeInfo <- projShapeCheck c
    case (c1 ^? affine, c2 ^? affine) of
      (Just True, Just True) -> return $ CAffineInfo shapeInfo True
      _ -> return $ CAffineInfo shapeInfo False

  affineCheck c@(CSkip :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ CAffineInfo shapeInfo True

instance AffineCheck (CTCHint :&: Position) where
  affineCheck c@(CTCHint extName params body :&: p) =
    throwM $ InternalError p "not yet implemented"
