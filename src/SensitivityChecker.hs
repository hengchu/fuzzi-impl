{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module SensitivityChecker where

import GHC.Generics

import Data.Maybe (isJust)
import Data.Generics.Product
import Control.Lens ((^.), at)
import Control.Lens.Tuple
import Syntax
import Match hiding (Context)
import Expand
import Pretty
import PatternQQ
import ShapeChecker
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.Reader
import Control.Monad.Except

import Debug.Trace

newtype SContext = SContext { getSContext :: M.Map Var Float }
  deriving (Show, Eq, Generic)

sctxtUpdate :: Var -> Maybe Float -> SContext -> SContext
sctxtUpdate x (Just s) (SContext sctxt) =
  SContext $ M.insert x s sctxt
sctxtUpdate x Nothing (SContext sctxt) =
  SContext $ M.delete x sctxt

sctxUpdateBulk :: [Var] -> Maybe Float -> SContext -> SContext
sctxUpdateBulk xs s sctx =
  foldr (\x acc -> sctxtUpdate x s acc) sctx xs

sctxtMax :: SContext -> SContext -> SContext
sctxtMax (SContext c1) (SContext c2) =
  SContext
  $ M.foldrWithKey
    (\k s1 acc ->
       case c2 ^. (at k) of
         Just s2 -> M.insert k (max s1 s2) acc
         _       -> M.delete k acc)
    M.empty
    c1

relax :: SContext -> SContext
relax (SContext sctx) =
  SContext $ M.filter (== 0) sctx

data ContextPair = ContextPair {
  context_pair_shape :: Context
  , context_pair_sensitivity :: SContext
  } deriving (Show, Eq)

instance Get ContextPair Context where
  get = context_pair_shape

instance Get ContextPair SContext where
  get = context_pair_sensitivity

data SensError = SensError {
  sens_error_position :: Position
  , sens_error_shape :: String
  } deriving (Show, Eq, Generic)

data Error = Sens SensError
           | Shape ShapeError
           deriving (Show, Eq, Generic)

errorPosition :: Error -> Position
errorPosition (Sens se)  = se ^. (typed @Position)
errorPosition (Shape se) = se ^. (typed @Position)

errorMsg :: Error -> String
errorMsg (Sens se)  = se ^. (typed @String)
errorMsg (Shape se) = se ^. (typed @String)

instance Inj Error SensError where
  inj = Sens

instance Inj Error ShapeError where
  inj = Shape

type RuleFunc m = Position -> UniEnv -> Context -> SContext -> m [(Float, SContext)]
type Rule m = (CmdPattern, RuleFunc m)

askSCtxt :: ( Get c SContext
            , MonadReader c m
            )
         => m (M.Map Var Float)
askSCtxt = getSContext . get <$> ask

sensFail :: ( Inj e SensError
            , MonadError e m
            )
         => Position -> String -> m a
sensFail p msg = throwError . inj $ SensError p msg

scheckLit :: ( Get c SContext
             , Get c Context
             , MonadReader c m
             , Inj e SensError
             , Inj e ShapeError
             , MonadError e m)
           => Position -> Lit -> m (Maybe Float)
scheckLit _ (LInt _) = return $ Just 0
scheckLit _ (LFloat _) = return $ Just 0
scheckLit _ (LBool _) = return $ Just 0
scheckLit _ (LArr es) = do
  ss <- mapM scheckExpr es
  return $ foldr (\x y -> (+) <$> x <*> y) (Just 0) ss
scheckLit _ (LBag es) = do
  ss <- mapM scheckExpr es
  return $ foldr (\s acc -> case approx1 s of
                              Just 0 -> acc
                              _ -> (+1) <$> acc) (Just 0) ss

approx :: Maybe Float -> Maybe Float -> Maybe Float
approx (Just 0) (Just 0) = Just 0
approx _        _        = Nothing

approx1 :: Maybe Float -> Maybe Float
approx1 (Just 0) = Just 0
approx1 _ = Nothing

smin :: Maybe Float -> Maybe Float -> Maybe Float
smin (Just s1) (Just s2) = Just $ min s1 s2
smin (Just s) _ = Just s
smin _ (Just s) = Just s
smin _ _ = Nothing

smax :: Maybe Float -> Maybe Float -> Maybe Float
smax (Just s1) (Just s2) = Just $ max s1 s2
smax _ _ = Nothing

scheckExpr :: ( Get c SContext
              , Get c Context
              , MonadReader c m
              , Inj e SensError
              , Inj e ShapeError
              , MonadError e m)
           => Expr -> m (Maybe Float)
scheckExpr (EVar p x) = do
  ctx <- askSCtxt
  return $ ctx ^. (at x)
scheckExpr (ELength p e) = do
  t <- checkExpr e
  case t of
    TBag _   -> scheckExpr e
    TArr _ _ -> do
      s <- scheckExpr e
      case s of
        Nothing -> return Nothing
        Just _ -> return . Just $ 0
    _ -> sensFail p "impossible: shape checker should have caught this!"
scheckExpr (ELit p lit) = scheckLit p lit
scheckExpr (EBinop p e1 op e2)
  | isBoolOp op || isOrdOp op || isEqOp op = do
      s1 <- scheckExpr e1
      s2 <- scheckExpr e2
      return $ approx s1 s2
  | op == PLUS || op == MINUS = do
      s1 <- scheckExpr e1
      s2 <- scheckExpr e2
      return $ (+) <$> s1 <*> s2
  | op == MULT = do
      s1 <- checkMult p e1 e2
      s2 <- checkMult p e2 e1
      return $ smin s1 s2
  | op == DIV = do
      checkDiv p e1 e2
  | otherwise = sensFail p $ "unknown operator: " ++ show op
scheckExpr (EIndex p e1 e2) = do
  s2 <- scheckExpr e2
  case s2 of
    Just 0 -> do
      t <- checkExpr e1
      case t of
        TArr _ _ -> scheckExpr e1
        TBag _ -> do
          s1 <- scheckExpr e1
          case s1 of
            Just 0 -> return Nothing
            _ -> sensFail p $ "indexing sensitive bag expression: "
                              ++ show (PrettyExpr e1)
                              ++ " may not co-terminate"
        _ -> sensFail p $ "impossible: shape checker should've caught this"
    _ -> sensFail p $ "indexing with sensitive expression: "
                      ++ show (PrettyExpr e2)
                      ++ " may not co-terminate"
scheckExpr (ERAccess _ e _) = do
  _ <- scheckExpr e
  return Nothing
scheckExpr (EFloat _ e) = scheckExpr e
scheckExpr (EExp _ e) = approx1 <$> scheckExpr e
scheckExpr (ELog _ e) = approx1 <$> scheckExpr e
scheckExpr (EClip _ e (LInt x)) = do
  _ <- scheckExpr e
  return $ Just (realToFrac $ 2 * x)
scheckExpr (EClip _ e (LFloat x)) = do
  _ <- scheckExpr e
  return $ Just (2 * x)
scheckExpr e@(EClip p _ _) =
  sensFail p $ "impossible: shape checker should've caught this bad clip expression " ++ show (PrettyExpr e)
scheckExpr (EScale _ scalar vector) = do
  s1 <- scheckExpr scalar
  s2 <- scheckExpr vector
  return $ (*) <$> s1 <*> s2
scheckExpr (EDot _ e1 e2) = do
  s1 <- scheckExpr e1
  s2 <- scheckExpr e2
  return $ approx s1 s2

checkMult :: ( Get c SContext
             , Get c Context
             , MonadReader c m
             , Inj e SensError
             , Inj e ShapeError
             , MonadError e m)
           => Position -> Expr -> Expr -> m (Maybe Float)
checkMult p e1 e2 = do
  let li1 = tryUnify @Expr [epat|iesc(x)|] e1
  let lf1 = tryUnify @Expr [epat|fesc(x)|] e1
  case (li1, lf1) of
    (Just env, Nothing) ->
      case env ^. (at "x") of
        Just (UniLit (LInt x)) -> do
          s2 <- scheckExpr e2
          return $ (*(realToFrac x)) <$> s2
        _ -> sensFail p $ "impossible: bug in matchExpr?"
    (Nothing, Just env) ->
      case env ^. (at "x") of
        Just (UniLit (LFloat x)) -> do
          s2 <- scheckExpr e2
          return $ (*x) <$> s2
        _ -> sensFail p $ "impossible: bug in matchExpr?"
    (Just _, Just _) -> sensFail p $ "impossible: " ++ show (PrettyExpr e1)
                                                    ++ " matched both iesc and fesc!"
    _ -> do
      s1 <- scheckExpr e1
      s2 <- scheckExpr e2
      return $ approx s1 s2

checkDiv :: ( Get c SContext
            , Get c Context
            , MonadReader c m
            , Inj e SensError
            , Inj e ShapeError
            , MonadError e m)
         => Position -> Expr -> Expr -> m (Maybe Float)
checkDiv p e1 e2 = do
  let li2 = tryUnify @Expr [epat|iesc(x)|] e2
  let lf2 = tryUnify @Expr [epat|fesc(x)|] e2
  case (li2, lf2) of
    (Just env, Nothing) ->
      case env ^. (at "x") of
        Just (UniLit (LInt x)) -> do
          s1 <- scheckExpr e1
          return $ (/realToFrac x) <$> s1
        _ -> sensFail p $ "impossible: bug in matchExpr?"
    (Nothing, Just env) ->
      case env ^. (at "x") of
        Just (UniLit (LFloat x)) -> do
          s1 <- scheckExpr e1
          return $ (/x) <$> s1
        _ -> sensFail p $ "impossible: bug in matchExpr?"
    (Just _, Just _) -> sensFail p $ "impossible: " ++ show (PrettyExpr e2)
                                                    ++ " matched both iesc and fesc"
    _ -> approx <$> scheckExpr e1 <*> scheckExpr e2

type RuleConstraints e m = ( MonadReader ContextPair m
                           , Inj e SensError
                           , Inj e ShapeError
                           , MonadError e m)

assignPat :: CmdPattern
assignPat = [cpat|
v(x) = e(y);
|]

assignRuleFunc :: (RuleConstraints e m) => RuleFunc m
assignRuleFunc p uenv tctx sctx = do
  case (uenv ^. (at "x"), uenv ^. (at "y")) of
    (Just (UniVar x), Just (UniExpr e)) -> do
      s <- local (const $ ContextPair tctx sctx) $ scheckExpr e
      return [(0, sctxtUpdate x s sctx)]
    _ -> sensFail p $ "failed to match any assignment command"

assignRule :: (RuleConstraints e m) => Rule m
assignRule = (assignPat, assignRuleFunc)

indexAssignPat :: CmdPattern
indexAssignPat = [cpat|
v(x)[e(idx)] = e(y);
|]

indexAssignFunc :: (RuleConstraints e m) => RuleFunc m
indexAssignFunc p uenv tctx sctx = do
  case (uenv ^. (at "x"), uenv ^. (at "idx"), uenv ^. (at "y")) of
    (Just (UniVar x), Just (UniExpr idx), Just (UniExpr y)) -> do
      let tx = (getContext tctx) ^. (at x)
          sx = (getSContext sctx) ^. (at x)
      sidx <- local (const $ ContextPair tctx sctx) $ scheckExpr idx
      sy <- local (const $ ContextPair tctx sctx) $ scheckExpr y
      case (sidx, tx) of
        (_, Nothing) -> sensFail p $ "index assignment to unknown variable: " ++ x
        (Just 0, Just (TArr _ _)) -> return [(0, sctxtUpdate x ((+) <$> sx <*> sy) sctx)]
        (Just 0, Just (TBag _)) ->
          case sx of
            Just 0 -> return [(0, sctxtUpdate x (Just 2) sctx)]
            _ -> sensFail p $ "sensitive index assignment over bag may not co-terminate"
        _ -> sensFail p $ "sensitive index assignment may not co-terminate"
    _ -> sensFail p $ "failed to match any indexed assignment command"

indexAssignRule :: (RuleConstraints e m) => Rule m
indexAssignRule = (indexAssignPat, indexAssignFunc)

lengthAssignPat :: CmdPattern
lengthAssignPat = [cpat|
length(v(x)) = e(y);
|]

lengthAssignFunc :: (RuleConstraints e m) => RuleFunc m
lengthAssignFunc p uenv tctx sctx = do
  case (uenv ^. (at "x"), uenv ^. (at "y")) of
    (Just (UniVar x), Just (UniExpr y)) -> do
      sy <- local (const $ ContextPair tctx sctx) $ scheckExpr y
      let tx = (getContext tctx) ^. (at x)
      case (tx, sy) of
        (Just (TArr _ _), Just 0) -> return [(0, sctx)]
        (Just (TBag _), Just 0) -> return [(0, sctxtUpdate x Nothing sctx)]
        _ -> sensFail p $ "length assignment may not co-terminate"
    _ -> sensFail p $ "failed to match any length assignment command"

lengthAssignRule :: (RuleConstraints e m) => Rule m
lengthAssignRule = (lengthAssignPat, lengthAssignFunc)

laplacePat :: CmdPattern
laplacePat = [cpat|
v(x) $= lap(fesc(b), e(y));
|]

laplaceFunc :: (RuleConstraints e m) => RuleFunc m
laplaceFunc p uenv tctx sctx = do
  case (uenv ^. (at "x"), uenv ^. (at "b"), uenv ^. (at "y")) of
    (Just (UniVar x), Just (UniLit (LFloat b)), Just (UniExpr y)) -> do
      sy <- local (const $ ContextPair tctx sctx) $ scheckExpr y
      case sy of
        Nothing -> sensFail p $ "Cannot release infinitely sensitive expression"
        Just s -> return [(s / b, sctxtUpdate x (Just 0) sctx)]
    _ -> sensFail p $ "failed to match any laplace assignment command"

laplaceRule :: (RuleConstraints e m) => Rule m
laplaceRule = (laplacePat, laplaceFunc)

skipPat :: CmdPattern
skipPat = [cpat|
skip;
|]

skipFunc :: (RuleConstraints e m) => RuleFunc m
skipFunc _ _ _ sctx = return [(0, sctx)]

whilePat :: CmdPattern
whilePat = [cpat|
while e(cond) do
  c(body);
end
|]

type Recur = SContext -> Cmd -> [(Float, SContext)]

whileFunc :: (RuleConstraints e m)
          => Recur
          -> RuleFunc m
whileFunc recur p uenv tctx sctx = do
  case (uenv ^. (at "cond"), uenv ^. (at "body")) of
    (Just (UniExpr cond), Just (UniCmd body)) -> do
      scond <- local (const $ ContextPair tctx sctx) $ scheckExpr cond
      case scond of
        Just 0 -> do
          let bodyContexts = recur sctx body
          return [c | c <- bodyContexts, c ^._2 == sctx, c^._1 == 0]
        _ -> sensFail p $ "expecting while condition to be 0 sensitive"
    _ -> sensFail p $ "failed to match any while command"

whileRule :: (RuleConstraints e m)
          => Recur -> Rule m
whileRule recur = (whilePat, whileFunc recur)

ifPat :: CmdPattern
ifPat = [cpat|
if e(cond) then
  c(body1);
else
  c(body2);
end
|]

ifFunc :: (RuleConstraints e m) => Recur -> RuleFunc m
ifFunc recur p uenv tctx sctx = do
  case (uenv ^. (at "cond"), uenv ^. (at "body1"), uenv ^. (at "body2")) of
    (Just (UniExpr cond), Just (UniCmd ct), Just (UniCmd cf)) -> do
      scond <- local (const $ ContextPair tctx sctx) $ scheckExpr cond
      case scond of
        Just 0 -> do
          let ctxs1 = recur sctx ct
          let ctxs2 = recur sctx cf
          return [(max e1 e2, sctxtMax c1 c2) | (e1, c1) <- ctxs1, (e2, c2) <- ctxs2]
        _ -> sensFail p $ "expecting if condition to be 0 sensitive"
    _ -> sensFail p $ "failed to match any if command"

ifRule :: (RuleConstraints e m) => Recur -> Rule m
ifRule recur = (ifPat, ifFunc recur)

bmapPat :: CmdPattern
bmapPat = bmapTgt

blockPat :: CmdPattern
blockPat = [cpat|
{ c(body) }
|]

blockFunc :: (RuleConstraints e m) => Recur -> RuleFunc m
blockFunc recur p uenv tctx sctx = do
  case uenv ^. (at "body") of
    Just (UniCmd body) -> return $ recur sctx body
    _ -> sensFail p $ "failed to match any block"

blockRule :: (RuleConstraints e m) => Recur -> Rule m
blockRule recur = (blockPat, blockFunc recur)

determ :: (RuleConstraints e m) => Cmd -> m Bool
determ (CLaplace _ _ _ _) = return False
determ (CSeq _ c1 c2) = (&&) <$> determ c1 <*> determ c2
determ (CIf _ _ c1 c2) = (&&) <$> determ c1 <*> determ c2
determ (CWhile _ _ c) = determ c
determ (CSkip _) = return True
determ (CAssign _ _ _) = return True
determ (CBlock _ c) = determ c
determ (CExt p _ _) = sensFail p $ "unexpanded extension"

mvs :: (RuleConstraints e m) => Cmd -> m (S.Set Var)
mvs (CLaplace _ x _ _) = return $ S.singleton x
mvs (CSeq _ c1 c2) = S.union <$> mvs c1 <*> mvs c2
mvs (CIf _ _ c1 c2) = S.union <$> mvs c1 <*> mvs c2
mvs (CWhile _ _ c) = mvs c
mvs (CAssign _ (EVar _ x) _) = return $ S.singleton x
mvs (CAssign _ (EIndex _ (EVar _ x) _) _) = return $ S.singleton x
mvs (CAssign _ (ELength _ (EVar _ x)) _) = return $ S.singleton x
mvs (CAssign p _ _) = sensFail p $ "unsupported assignment form"
mvs (CSkip _) = return S.empty
mvs (CBlock _ c) = mvs c
mvs (CExt p _ _) = sensFail p $ "unexpanded extension"

bmapFunc :: (RuleConstraints e m) => Recur -> RuleFunc m
bmapFunc recur p uenv tctx sctx = do
  case ( uenv ^. (at "in")
       , uenv ^. (at "out")
       , uenv ^. (at "t_in")
       , uenv ^. (at "idx")
       , uenv ^. (at "t_out")
       , uenv ^. (at "body")
       ) of
    ( Just (UniVar vin)
      , Just (UniVar vout)
      , Just (UniVar t_in)
      , Just (UniVar idx)
      , Just (UniVar t_out)
      , Just (UniCmd body) ) -> do
      let tauIn = (getContext tctx) ^. (at vin)
      let tauOut = (getContext tctx) ^. (at vout)

      case (tauIn, tauOut) of
        (Just (TBag _), Just (TBag _)) -> return ()
        _ -> sensFail p $ "bag map should be applied to bags"

      isDeterm <- determ body
      when (not $ isDeterm) $
        sensFail p $ "bag map body is expected to be deterministic"
      mvsBody <- mvs body
      when (t_in `S.member` mvsBody ||
            vin `S.member` mvsBody ||
            vout `S.member` mvsBody ||
            idx `S.member` mvsBody) $
        sensFail p $ "bag map body should not modify in, out, t_in or idx"
      let s1 = relax $ sctxUpdateBulk (S.elems mvsBody ++ [idx, vout])
                                      Nothing
                                      (sctxtUpdate t_in (Just 0) sctx)
      let allMvsZero sctx =
            all (\x -> ((getSContext sctx) ^. (at x)) == Just 0) (S.elems mvsBody)
      let s1' = [s | (eps, s) <- recur s1 body, allMvsZero s, eps == 0]
      when (null s1') $
        sensFail p $ "bag map body uses other sensitive inputs"
      let s2 = sctxUpdateBulk (S.elems mvsBody ++ [t_in, idx, vout]) Nothing sctx
      let onlySensTOut sctx =
            S.foldr
              (\x acc -> if x == t_out
                         then case (getSContext sctx) ^. (at x) of
                                Nothing -> acc
                                Just s | s > 0 -> acc
                                Just 0 -> False
                         else case (getSContext sctx) ^. (at x) of
                                Nothing -> acc
                                Just s | s > 0 -> False
                                Just 0 -> acc)
              True
              mvsBody
      let s2' = [s | (eps, s) <- recur s2 body, onlySensTOut s ]
      when (null s2') $
        sensFail p $ "bag map body is not resetting outputs other than t_out"
      let s_in = (getSContext sctx) ^. (at vin)
      return $ [(0, sctxUpdateBulk
                      (S.elems mvsBody ++ [idx, t_in])
                      Nothing
                      (sctxtUpdate vout s_in sctx))
               ]
    _ -> sensFail p $ "failed to match any bag map command"

bmapRule :: (RuleConstraints e m) => Recur -> Rule m
bmapRule recur = (bmapPat, bmapFunc recur)

step :: Context
     -> SContext
     -> [Rule (ExceptT Error (Reader ContextPair))]
     -> Cmd
     -> [(Float, SContext, Maybe Cmd)]
step _    _    []            _ = []
step tctx sctx ((p, f):more) c =
  case (runReader (runExceptT go) (ContextPair tctx sctx)) of
    Left _ -> step tctx sctx more c
    Right results -> results ++ (step tctx sctx more c)
  where cp = cmdPosn c
        go = do
          case runUniM $ matchCmdPrefix p c of
            Right (uenv, remain) -> do
              results <- f cp uenv tctx sctx
              return [(fst es, snd es, remain)| es <- results]
            Left _ -> return []

search :: Context
       -> SContext
       -> [Rule (ExceptT Error (Reader ContextPair))]
       -> Cmd
       -> Int
       -> [(Float, SContext, Maybe Cmd)]
search _    _    _     _ d | d <= 0 = []
search _    _    []    _ _     = []
search tctx sctx rules c depth = do
  r <- step tctx sctx rules c
  case r ^._3 of
    Nothing -> return r
    Just remain -> do
      (eps, sctx, remain') <- search tctx (r ^._2) rules remain (depth - 1)
      return (r ^._1 + eps, sctx, remain')

declsToSContext :: [Decl] -> SContext
declsToSContext decls = SContext . M.fromList $ go decls
  where go []                    = []
        go ((Decl _ x s _):more) = (x, s):(go more)

fuzziTypingRules :: Recur -> [Rule (ExceptT Error (Reader ContextPair))]
fuzziTypingRules recur = [ assignRule
                         , indexAssignRule
                         , lengthAssignRule
                         , laplaceRule
                         , whileRule recur
                         , ifRule recur
                         , bmapRule recur
                         , blockRule recur
                         ]

runSensitivityChecker :: Prog -> Int -> Either ShapeError [(Float, SContext)]
runSensitivityChecker p@(Prog decls cmd) depth =
  case runShapeChecker p of
    Left err -> Left err
    Right _ -> Right $ recur depth sctx cmd
  where tctx    = declsToContext decls
        sctx    = declsToSContext decls
        recur depth sctx cmd
          | depth <= 0 = []
          | otherwise = do
              result <- search tctx sctx (fuzziTypingRules (recur $ depth - 1)) cmd depth
              case result ^._3 of
                Just _ -> []
                Nothing -> return (result ^._1, result ^._2)

runSensitivityCheckerIgnoreError :: Prog -> Int -> [(Float, SContext)]
runSensitivityCheckerIgnoreError p depth =
  case runSensitivityChecker p depth of
    Left _ -> []
    Right rs -> rs
