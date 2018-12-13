module Termination where

import Data.Monoid
import Data.Typeable
import Control.Lens
import Data.Comp
import Data.Comp.Derive
import SyntaxExt

import Shape hiding (ShapeCheckError(..))
import Control.Monad.Reader
import Control.Monad.Catch
import Control.Monad.Cont

data TerminationInfo =
  TerminationInfo {
  _terminationinfo_shape_info :: ShapeInfo
  , _terminationinfo_terminates :: Bool
  }

$(makeLensesWith underscoreFields ''TerminationInfo)
$(makePrisms ''TerminationInfo)

data TerminationCheckError = InternalError Position String
  deriving (Show, Typeable, Eq)

instance Exception TerminationCheckError

projShapeCheck :: ( MonadThrow m
                  , MonadReader ShapeCxt m
                  , MonadCont m
                  , Functor f
                  , ShapeCheck f
                  )
               => f TerminationInfo -> m ShapeInfo
projShapeCheck fa =
  shapeCheck (fmap (view shape_info) fa)

class TerminationCheck f where
  terminationCheck :: (MonadThrow m, MonadReader ShapeCxt m, MonadCont m)
                   => AlgM m f TerminationInfo

$(derive [liftSum] [''TerminationCheck])

instance TerminationCheck (Expr :&: Position) where
  terminationCheck c@(EVar _ :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo True

  terminationCheck c@(ELength info :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo (info ^. terminates)

  terminationCheck c@(ELit (LInt _) :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo True

  terminationCheck c@(ELit (LFloat _) :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo True

  terminationCheck c@(ELit (LBool _) :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo True

  terminationCheck c@(ELit (LArr infos) :&: _) = do
    shapeInfo <- projShapeCheck c
    if getAll $ foldMap All (infos ^.. traverse . terminates)
    then return $ TerminationInfo shapeInfo True
    else return $ TerminationInfo shapeInfo False

  terminationCheck c@(ELit (LBag infos) :&: _) = do
    shapeInfo <- projShapeCheck c
    if getAll $ foldMap All (infos ^.. traverse . terminates)
    then return $ TerminationInfo shapeInfo True
    else return $ TerminationInfo shapeInfo False

  terminationCheck c@(EBinop _ linfo rinfo :&: _) = do
    shapeInfo <- projShapeCheck c
    if linfo ^. terminates && rinfo ^. terminates
    then return $ TerminationInfo shapeInfo True
    else return $ TerminationInfo shapeInfo False

  terminationCheck c@(EIndex linfo rinfo :&: _) = do
    shapeInfo <- projShapeCheck c
    case (linfo ^? shape_info . tau,
          projectELInt $ rinfo ^. shape_info . term) of
      (Just (TArr _ (Just len)), Just k) ->
        return $ TerminationInfo shapeInfo (0 <= k && k < len)
      _ -> return $ TerminationInfo shapeInfo False

  terminationCheck c@(EFloat info :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo (info ^. terminates)

  terminationCheck c@(EExp info :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo (info ^. terminates)

  terminationCheck c@(ELog info :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo (info ^. terminates)

  terminationCheck c@(EClip info _ :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo (info ^. terminates)

  terminationCheck c@(EScale linfo rinfo :&: _) = do
    shapeInfo <- projShapeCheck c
    if linfo ^. terminates && rinfo ^. terminates
    then return $ TerminationInfo shapeInfo True
    else return $ TerminationInfo shapeInfo False

  terminationCheck c@(EDot linfo rinfo :&: _) = do
    shapeInfo <- projShapeCheck c
    if linfo ^. terminates && rinfo ^. terminates
    then return $ TerminationInfo shapeInfo True
    else return $ TerminationInfo shapeInfo False

instance TerminationCheck (Cmd :&: Position) where
  terminationCheck c@(CAssign linfo rinfo :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo (linfo ^. terminates && rinfo ^. terminates)

  terminationCheck c@(CLaplace linfo _ rinfo :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo (linfo ^. terminates && rinfo ^. terminates)

  terminationCheck c@(CIf einfo c1info c2info :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo
               shapeInfo
               (einfo ^. terminates && c1info ^. terminates && c2info ^. terminates)

  terminationCheck c@(CWhile _ _ :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo False

  terminationCheck c@(CSeq c1 c2 :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo (c1 ^. terminates && c2 ^. terminates)

  terminationCheck c@(CSkip :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo True

instance TerminationCheck (CTCHint :&: Position) where
  terminationCheck c@(CTCHint _ _ body :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo (body ^. terminates)
