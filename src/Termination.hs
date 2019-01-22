module Termination where

import qualified Data.Map as M
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
import Control.Monad.State

import Debug.Trace

data TerminationInfo m =
  TerminationInfo {
  _terminationinfo_shape_info :: ShapeInfo
  , _terminationinfo_terminates :: m Bool
  , _terminationinfo_extterms :: [(String, Position, Bool)]
  }

$(makeLensesWith underscoreFields ''TerminationInfo)
$(makePrisms ''TerminationInfo)

data TerminationCheckError = InternalError Position String
                           | InvalidExtensionArgs Position
  deriving (Show, Typeable, Eq)

data TerminationCxt = TerminationCxt {
  _terminationcxt_static_values :: M.Map Var Int
  } deriving (Show, Eq)

$(makeLensesWith underscoreFields ''TerminationCxt)

instance Exception TerminationCheckError

projShapeCheck :: ( MonadThrow m
                  , MonadReader ShapeCxt m
                  , MonadCont m
                  , Functor f
                  , ShapeCheck f
                  )
               => f (TerminationInfo m) -> m ShapeInfo
projShapeCheck fa =
  shapeCheck (fmap (view shape_info) fa)

hasStaticIntValue :: ( MonadThrow m
                     , MonadReader ShapeCxt m
                     , MonadState  TerminationCxt  m
                     , MonadCont m)
               => Term ImpTCP -> m (Maybe Int)
hasStaticIntValue e =
  case (deepProject @ExprP e) of
    Just e' ->
      case unTerm e' of
        (EBinop PLUS el er :&: _) -> do
          svl <- hasStaticIntValue (deepInject el)
          svr <- hasStaticIntValue (deepInject er)
          return $ (+) <$> svl <*> svr
        (EBinop MINUS el er :&: _) -> do
          svl <- hasStaticIntValue (deepInject el)
          svr <- hasStaticIntValue (deepInject er)
          return $ (-) <$> svl <*> svr
        (EBinop MULT el er :&: _) -> do
          svl <- hasStaticIntValue (deepInject el)
          svr <- hasStaticIntValue (deepInject er)
          return $ (*) <$> svl <*> svr
        (EBinop DIV el er :&: _) -> do
          svl <- hasStaticIntValue (deepInject el)
          svr <- hasStaticIntValue (deepInject er)
          return $ div <$> svl <*> svr
        _ ->
          case (projectELInt e, projectEVar e) of
            (Just v, _     ) -> return (Just v)
            (_,      Just x) -> do
              svs <- (view static_values) <$> get
              return $ M.lookup x svs
            _ -> return Nothing
    Nothing -> return Nothing

class TerminationCheck f where
  terminationCheck :: ( MonadThrow m
                      , MonadReader ShapeCxt m
                      , MonadState TerminationCxt m
                      , MonadCont m)
                   => AlgM m f (TerminationInfo m)

$(derive [liftSum] [''TerminationCheck])

instance TerminationCheck (Expr :&: Position) where
  terminationCheck c@(EVar _ :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo (return True) []

  terminationCheck c@(ELength info :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo (info ^. terminates) []

  terminationCheck c@(ELit (LInt _) :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo (return True) []

  terminationCheck c@(ELit (LFloat _) :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo (return True) []

  terminationCheck c@(ELit (LBool _) :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo (return True) []

  terminationCheck c@(ELit (LArr infos) :&: _) = do
    shapeInfo <- projShapeCheck c
    let termEff = do
          termInfos <- sequence (infos ^.. traverse . terminates)
          return $ getAll $ foldMap All termInfos
    return $ TerminationInfo shapeInfo termEff []

  terminationCheck c@(ELit (LBag infos) :&: _) = do
    shapeInfo <- projShapeCheck c
    let termEff = do
          termInfos <- sequence (infos ^.. traverse . terminates)
          return $ getAll $ foldMap All termInfos
    return $ TerminationInfo shapeInfo termEff []

  terminationCheck c@(EBinop _ linfo rinfo :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo ((&&) <$> linfo ^. terminates <*> rinfo ^. terminates) []

  terminationCheck c@(EIndex linfo rinfo :&: _) = do
    shapeInfo <- projShapeCheck c
    let termEff = do
          maybeStaticIdxValue <- hasStaticIntValue $ rinfo ^. shape_info . term
          case (linfo ^? shape_info . tau,
                maybeStaticIdxValue) of
            (Just (TArr _ (Just len)), Just k) ->
              return $ 0 <= k && k < len
            _ -> return False
    return $ TerminationInfo shapeInfo termEff []

  terminationCheck c@(EFloat info :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo (info ^. terminates) []

  terminationCheck c@(EExp info :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo (info ^. terminates) []

  terminationCheck c@(ELog info :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo (info ^. terminates) []

  terminationCheck c@(EClip info _ :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo (info ^. terminates) []

  terminationCheck c@(EScale linfo rinfo :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo ((&&) <$> linfo ^. terminates <*> rinfo ^. terminates) []

  terminationCheck c@(EDot linfo rinfo :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo ((&&) <$> linfo ^. terminates <*> rinfo ^. terminates) []

  terminationCheck c@(EFst info :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo (info ^. terminates) []

  terminationCheck c@(ESnd info :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo (info ^. terminates) []

instance TerminationCheck (Cmd :&: Position) where
  terminationCheck c@(CAssign linfo rinfo :&: _) = do
    shapeInfo <- projShapeCheck c
    let termEff = do
          tl <- linfo ^. terminates
          tr <- rinfo ^. terminates
          return $ (tl && tr)
    return $ TerminationInfo shapeInfo termEff []

  terminationCheck c@(CLaplace linfo _ rinfo :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo ((&&) <$> linfo ^. terminates <*> rinfo ^. terminates) []

  terminationCheck c@(CIf einfo c1info c2info :&: _) = do
    shapeInfo <- projShapeCheck c
    let termEff = do
          terms <- sequence [einfo ^. terminates, c1info ^. terminates, c2info ^. terminates]
          return $ getAll $ foldMap All terms
    return $ TerminationInfo
               shapeInfo
               termEff
               []

  terminationCheck c@(CWhile _ _ :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo (return False) []

  terminationCheck c@(CSeq c1 c2 :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo ((&&) <$> c1 ^. terminates <*> c2 ^. terminates) []

  terminationCheck c@(CSkip :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo (return True) []

instance TerminationCheck (CTCHint :&: Position) where
  terminationCheck c@(CTCHint "repeat" [vidx, litniters, rpBody] _ :&: p) = do
    shapeInfo <- projShapeCheck c
    case (projectEVar  (vidx ^. shape_info . term),
          projectELInt (litniters ^. shape_info . term)) of
      (Just _vidx,
       Just _niters) -> do
        let termEff n = do
              static_values %= M.alter (const $ Just n) _vidx
              rpBody ^. terminates
        let termEff' = do
              currSt <- get
              terms <- mapM termEff [0.._niters-1]
              put currSt
              let t = getAll $ foldMap All terms
              return t
        return $ TerminationInfo shapeInfo termEff' []
      _ -> throwM $ InvalidExtensionArgs p

  terminationCheck c@(CTCHint "bmap" [_, _, _, _, _, bodyInfo] _ :&: _) = do
    shapeInfo <- projShapeCheck c
    let termEff = bodyInfo ^. terminates
    return $ TerminationInfo shapeInfo termEff []

  terminationCheck c@(CTCHint "amap" [_, _, _, _, _, bodyInfo] _ :&: _) = do
    shapeInfo <- projShapeCheck c
    let termEff = bodyInfo ^. terminates
    return $ TerminationInfo shapeInfo termEff []

  terminationCheck c@(CTCHint "partition" [_, _, _, _, _, _, _, _, _, bodyInfo] _ :&: _) = do
    shapeInfo <- projShapeCheck c
    let termEff = bodyInfo ^. terminates
    return $ TerminationInfo shapeInfo termEff []

  terminationCheck c@(CTCHint "bsum" _ _ :&: _) = do
    shapeInfo <- projShapeCheck c
    let termEff = return True
    return $ TerminationInfo shapeInfo termEff []

  terminationCheck c@(CTCHint "ac" [_, _, _, _, bodyInfo] _ :&: _) = do
    shapeInfo <- projShapeCheck c
    let termEff = bodyInfo ^. terminates
    return $ TerminationInfo shapeInfo termEff []

  terminationCheck c@(CTCHint _ _ body :&: _) = do
    shapeInfo <- projShapeCheck c
    return $ TerminationInfo shapeInfo (body ^. terminates) []
