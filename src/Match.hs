{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Match where

import Syntax
import Pretty
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Either
import Test.QuickCheck

data UniResult = UniVar Var
               | UniLit Lit
               | UniExpr Expr
               | UniCmd Cmd
               deriving (Show, Eq, Ord)

data UniError = UniError { getUniError :: M.Map Var (S.Set UniResult) }
  deriving (Show, Eq)

type UniEnv = M.Map Var UniResult

instance Semigroup UniError where
  (<>) (UniError x) (UniError y) = UniError $ M.unionWith S.union x y

instance Monoid UniError where
  mempty = UniError M.empty
  mappend (UniError x) (UniError y) = UniError $ M.unionWith S.union x y

newtype UniM a = UniM { runUniM :: Either UniError a }
  deriving (Show, Eq, Functor)

instance Applicative UniM where
  pure = UniM . Right
  (<*>) (UniM (Left e1)) (UniM (Left e2)) =
    UniM . Left $ e1 <> e2
  (<*>) (UniM (Left e)) _ = UniM . Left $ e
  (<*>) _ (UniM (Left e)) = UniM . Left $ e
  (<*>) (UniM (Right f)) (UniM (Right a)) = UniM . Right $ f a

instance Monad UniM where
  return = pure
  (>>=) (UniM (Left e)) _ = UniM . Left $ e
  (>>=) (UniM (Right a)) f = f a

unify :: UniEnv -> UniEnv -> UniM UniEnv
unify env1 env2 =
  let (errs, unified) = go env1 env2 in
  if M.null errs
  then UniM . Right $ unified
  else UniM . Left . UniError $ errs
  where go :: UniEnv -> UniEnv -> (M.Map Var (S.Set UniResult), M.Map Var UniResult)
        go env1 env2 =
          M.foldrWithKey'
            (\x ur (errs, acc) ->
               case M.lookup x env2 of
                 Just ur2 ->
                   if ur == ur2
                   then (errs, acc)
                   else (M.insertWith S.union x (S.singleton ur) errs, acc)
                 Nothing ->
                   (errs, M.insert x ur acc))
            (M.empty, env2)
            env1

justFail :: UniM a
justFail = UniM . Left $ mempty

matchAtom :: (Eq a) => AtomPattern a -> a -> UniM (Maybe (Var, a))
matchAtom (AtomExact a1) a2 =
  if a1 == a2
  then pure Nothing
  else justFail
matchAtom (AtomWild x) a =
  pure . Just $ (x, a)

substIntPattern :: IntPattern -> UniEnv -> Maybe IntPattern
substIntPattern a@(AtomExact _) _ = Just a
substIntPattern (AtomWild x) env =
  case M.lookup x env of
    Just (UniLit (LInt v)) -> Just $ AtomExact v
    _ -> Nothing

substFloatPattern :: FloatPattern -> UniEnv -> Maybe FloatPattern
substFloatPattern a@(AtomExact _) _ = Just a
substFloatPattern (AtomWild x) env =
  case M.lookup x env of
    Just (UniLit (LFloat v)) -> Just $ AtomExact v
    _ -> Nothing

substBoolPattern :: BoolPattern -> UniEnv -> Maybe BoolPattern
substBoolPattern a@(AtomExact _) _ = Just a
substBoolPattern (AtomWild x) env =
  case M.lookup x env of
    Just (UniLit (LBool v)) -> Just $ AtomExact v
    _ -> Nothing

substVarPattern :: VarPattern -> UniEnv -> Maybe VarPattern
substVarPattern a@(AtomExact _) _ = Just a
substVarPattern (AtomWild x) env =
  case M.lookup x env of
    Just (UniVar v) -> Just $ AtomExact v
    _ -> Nothing

matchLit :: LitPattern -> Lit -> UniEnv -> UniM UniEnv
matchLit (LPInt ip) (LInt i) env = do
  r <- matchAtom ip i
  case r of
    Just (x, v) -> return $ M.insert x (UniLit (LInt v)) env
    Nothing -> return env
matchLit (LPFloat fp) (LFloat f) env = do
  r <- matchAtom fp f
  case r of
    Just (x, v) -> return $ M.insert x (UniLit (LFloat v)) env
    Nothing -> return env
matchLit (LPBool bp) (LBool b) env = do
  r <- matchAtom bp b
  case r of
    Just (x, v) -> return $ M.insert x (UniLit (LBool v)) env
    Nothing -> return env
matchLit (LPArr eps) (LArr es) env = matchPatterns matchExpr eps es env
matchLit (LPBag eps) (LBag es) env = matchPatterns matchExpr eps es env
matchLit _           _         _   = justFail

substLit :: LitPattern -> UniEnv -> Maybe LitPattern
substLit (LPInt ip) env = LPInt <$> substIntPattern ip env
substLit (LPFloat fp) env = LPFloat <$> substFloatPattern fp env
substLit (LPBool bp) env = LPBool <$> substBoolPattern bp env
substLit (LPArr eps) env = do
  es <- mapM (flip substExpr env) eps
  return $ LPArr es
substLit (LPBag eps) env = do
  es <- mapM (flip substExpr env) eps
  return $ LPBag es

matchPatterns :: (p -> e -> UniEnv -> UniM UniEnv) -> [p] -> [e] -> UniEnv -> UniM UniEnv
matchPatterns _ []     []     env = return env
matchPatterns f (p:ps) (e:es) env = do
  env' <- f p e env
  matchPatterns f ps es env'
matchPatterns _ _      _      _ = justFail

matchExpr :: ExprPattern -> Expr -> UniEnv -> UniM UniEnv
matchExpr (EPWild _ x) e env =
  case M.lookup x env of
    Just ur -> if (UniExpr e) == ur then pure env else justFail
    Nothing -> pure $ M.insert x (UniExpr e) env
matchExpr (EPVar _ vp) (EVar _ v) env = do
  vpv <- matchAtom vp v
  case vpv of
    Just (x, var) -> pure $ M.insert x (UniVar var) env
    Nothing -> pure env
matchExpr (EPLength _ ep) (ELength _ e) env =
  matchExpr ep e env
matchExpr (EPLit _ litp) (ELit _ lit) env =
  matchLit litp lit env
matchExpr (EPBinop _ ep1 op ep2) (EBinop _ e1 op' e2) env
  | op == op' = do
      env1 <- matchExpr ep1 e1 env
      env2 <- matchExpr ep2 e2 env
      unify env1 env2
matchExpr (EPIndex _ ep1 ep2) (EIndex _ e1 e2) env = do
  env1 <- matchExpr ep1 e1 env
  env2 <- matchExpr ep2 e2 env
  unify env1 env2
matchExpr (EPRAccess _ ep label) (ERAccess _ e label') env
  | label == label' = matchExpr ep e env
matchExpr (EPFloat _ ep) (EFloat _ e) env =
  matchExpr ep e env
matchExpr (EPExp _ ep) (EExp _ e) env =
  matchExpr ep e env
matchExpr (EPLog _ ep) (ELog _ e) env =
  matchExpr ep e env
matchExpr (EPClip _ ep lp) (EClip _ e l) env = do
  env1 <- matchExpr ep e env
  env2 <- matchLit lp l env
  unify env1 env2
matchExpr (EPScale _ ep1 ep2) (EScale _ e1 e2) env = do
  env1 <- matchExpr ep1 e1 env
  env2 <- matchExpr ep2 e2 env
  unify env1 env2
matchExpr (EPDot _ ep1 ep2) (EDot _ e1 e2) env = do
  env1 <- matchExpr ep1 e1 env
  env2 <- matchExpr ep2 e2 env
  unify env1 env2
matchExpr _ _ _ = justFail

substExpr :: ExprPattern -> UniEnv -> Maybe ExprPattern
substExpr (EPWild _ x) env =
  case M.lookup x env of
    Just (UniExpr e) -> Just $ exprToPattern e
    _ -> Nothing
substExpr (EPVar p vp) env =
  EPVar p <$> substVarPattern vp env
substExpr (EPLength p ep) env =
  EPLength p <$> substExpr ep env
substExpr (EPLit p lit) env =
  EPLit p <$> substLit lit env
substExpr (EPBinop p e1 op e2) env =
  EPBinop p <$> substExpr e1 env <*> pure op <*> substExpr e2 env
substExpr (EPIndex p e1 e2) env =
  EPIndex p <$> substExpr e1 env <*> substExpr e2 env
substExpr (EPRAccess p e label) env =
  EPRAccess p <$> substExpr e env <*> pure label
substExpr (EPFloat p e) env =
  EPFloat p <$> substExpr e env
substExpr (EPExp p e) env =
  EPExp p <$> substExpr e env
substExpr (EPLog p e) env =
  EPLog p <$> substExpr e env
substExpr (EPClip p e lit) env =
  EPClip p <$> substExpr e env <*> substLit lit env
substExpr (EPScale p e1 e2) env =
  EPScale p <$> substExpr e1 env <*> substExpr e2 env
substExpr (EPDot p e1 e2) env =
  EPDot p <$> substExpr e1 env <*> substExpr e2 env

matchParam :: ParamPattern -> Param -> UniEnv -> UniM UniEnv
matchParam (PPCmd cp) (PCmd c) env =
  matchCmd cp c env
matchParam (PPExpr ep) (PExpr e) env =
  matchExpr ep e env
matchParam _ _ _ = justFail

substParam :: ParamPattern -> UniEnv -> Maybe ParamPattern
substParam (PPCmd c) env = PPCmd <$> substCmd c env
substParam (PPExpr e) env = PPExpr <$> substExpr e env

matchCmd :: CmdPattern -> Cmd -> UniEnv -> UniM UniEnv
matchCmd (CPWild _ x) c env =
  case M.lookup x env of
    Just ur -> if (UniCmd c) == ur then pure env else justFail
    Nothing -> pure $ M.insert x (UniCmd c) env
matchCmd (CPAssign _ ep1 ep2) (CAssign _ e1 e2) env = do
  env1 <- matchExpr ep1 e1 env
  env2 <- matchExpr ep2 e2 env
  unify env1 env2
matchCmd (CPLaplace _ vp fp ep) (CLaplace _ v f e) env = do
  rv <- matchAtom vp v
  let modifyEnv1 =
        case rv of
          Nothing -> id
          Just (x, v) -> M.insert x (UniVar v)
  rf <- matchAtom fp f
  let modifyEnv2 =
        case rf of
          Nothing -> id
          Just (x, v) -> M.insert x (UniLit . LFloat $ v)
  matchExpr ep e (modifyEnv2 . modifyEnv1 $ env)
matchCmd (CPIf _ ep cp1 cp2) (CIf _ e c1 c2) env = do
  env1 <- matchExpr ep e env
  env11 <- matchCmd cp1 c1 env1
  env12 <- matchCmd cp2 c2 env1
  unify env11 env12
matchCmd (CPWhile _ ep cp) (CWhile _ e c) env = do
  env1 <- matchExpr ep e env
  env2 <- matchCmd cp c env
  unify env1 env2
matchCmd (CPSeq _ cp1 cp2) (CSeq _ c1 c2) env = do
  env1 <- matchCmd cp1 c1 env
  env2 <- matchCmd cp2 c2 env
  unify env1 env2
matchCmd (CPSkip _) (CSkip _) env = return env
matchCmd (CPExt _ name pps) (CExt _ name' ps) env
  | name == name' =
    matchPatterns matchParam pps ps env
matchCmd (CPBlock _ cp) (CBlock _ c) env =
  matchCmd cp c env
matchCmd _ _ _ = justFail

substCmd :: CmdPattern -> UniEnv -> Maybe CmdPattern
substCmd (CPWild _ x) env =
  case M.lookup x env of
    Just (UniCmd c) -> Just $ cmdToPattern c
    _ -> Nothing
substCmd (CPAssign p e1 e2) env =
  CPAssign p <$> substExpr e1 env <*> substExpr e2 env
substCmd (CPLaplace p vp fp ep) env =
  CPLaplace p <$> substVarPattern vp env <*> substFloatPattern fp env <*> substExpr ep env
substCmd (CPIf p e c1 c2) env =
  CPIf p <$> substExpr e env <*> substCmd c1 env <*> substCmd c2 env
substCmd (CPWhile p e c) env =
  CPWhile p <$> substExpr e env <*> substCmd c env
substCmd (CPSeq p c1 c2) env =
  CPSeq p <$> substCmd c1 env <*> substCmd c2 env
substCmd c@(CPSkip _) _ = Just c
substCmd (CPExt p name params) env = do
  params' <- mapM (flip substParam env) params
  return $ CPExt p name params'
substCmd (CPBlock p cp) env =
  CPBlock p <$> substCmd cp env

matchExprBool :: ExprPattern -> Expr -> Bool
matchExprBool ep e =
  case runUniM (matchExpr ep e M.empty) of
    Left _ -> False
    Right _ -> True

matchCmdBool :: CmdPattern -> Cmd -> Bool
matchCmdBool cp c =
  case runUniM (matchCmd cp c M.empty) of
    Left _ -> False
    Right _ -> True

matchCmdPrefix' :: CmdPattern -> Cmd -> UniEnv -> UniM (UniEnv, Maybe Cmd)
matchCmdPrefix' (CPSeq _ cp1 cp2) (CSeq _ c1 c2) env = do
  env1 <- matchCmd cp1 c1 env
  matchCmdPrefix' cp2 c2 env1
matchCmdPrefix' cp (CSeq _ c1 c2) env = do
  env1 <- matchCmd cp c1 env
  return (env1, Just c2)
matchCmdPrefix' cp c env = do
  env1 <- matchCmd cp c env
  return (env1, Nothing)

matchCmdPrefix :: CmdPattern -> Cmd -> UniM (UniEnv, Maybe Cmd)
matchCmdPrefix cp c = matchCmdPrefix' cp c M.empty

type Context a = a -> a

matchCmdAnywhereParams :: CmdPattern -> [Param] -> UniM (UniEnv, Cmd -> [Param])
matchCmdAnywhereParams cp params =
  go [] cp params
  where go :: [Param] -> CmdPattern -> [Param] -> UniM (UniEnv, Cmd -> [Param])
        go _   _  [] = UniM . Left $ mempty
        go acc cp (p@(PExpr _):more) = go (p:acc) cp more
        go acc cp (p@(PCmd c):more) =
          case runUniM $ matchCmdAnywhere cp c of
            Left _ -> go (p:acc) cp more
            Right (env, hole) ->
              return (env, \c -> (reverse (PCmd (hole c):acc)) ++ more)

-- |Returns a context with a hole where the original matched cmd is, that allows
-- us to fill in with another command.
matchCmdAnywhere :: CmdPattern -> Cmd -> UniM (UniEnv, Context Cmd)
matchCmdAnywhere cp c
  | isRight . runUniM $ matchCmdPrefix cp c = do
      (env, remains) <- matchCmdPrefix cp c
      case remains of
        Nothing -> return (env, id)
        Just r -> return (env, \c -> CSeq trivialPosition c r)
  | otherwise =
  case c of
    CSeq p c1 c2
      | isRight . runUniM $ matchCmdAnywhere cp c1 -> do
          (env, hole) <- matchCmdAnywhere cp c1
          return (env, \c -> CSeq p (hole c) c2)
      | isRight . runUniM $ matchCmdAnywhere cp c2 -> do
          (env, hole) <- matchCmdAnywhere cp c2
          return (env, \c -> CSeq p c1 (hole c))
    CIf p e c1 c2
      | isRight . runUniM $ matchCmdAnywhere cp c1 -> do
          (env, hole) <- matchCmdAnywhere cp c1
          return (env, \c -> CIf p e (hole c) c2)
      | isRight . runUniM $ matchCmdAnywhere cp c2 -> do
          (env, hole) <- matchCmdAnywhere cp c2
          return (env, \c -> CIf p e c1 (hole c))
    CWhile p e c
      | isRight . runUniM $ matchCmdAnywhere cp c -> do
      (env, hole) <- matchCmdAnywhere cp c
      return (env, \c -> CWhile p e (hole c))
    CExt p name params
      | isRight . runUniM $ matchCmdAnywhereParams cp params -> do
      (env, hole) <- matchCmdAnywhereParams cp params
      return (env, \c -> CExt p name (hole c))
    CBlock p c
      | isRight . runUniM $ matchCmdAnywhere cp c -> do
          (env, hole) <- matchCmdAnywhere cp c
          return (env, \c -> CBlock p (hole c))
    _ -> UniM . Left $ mempty

class Matching a where
  type Pattern a :: *
  type Env a :: *
  wildcard :: Var -> Pattern a
  pattern  :: a -> Pattern a
  matches  :: Pattern a -> a -> Bool
  closed   :: Pattern a -> Bool
  recover  :: Pattern a -> Maybe a
  subst    :: Pattern a -> Env a -> Maybe (Pattern a)
  tryUnify :: Pattern a -> a -> Maybe (Env a)

instance Matching Expr where
  type Pattern Expr = ExprPattern
  type Env Expr = UniEnv
  wildcard = EPWild trivialPosition
  pattern  = exprToPattern
  matches  = matchExprBool
  closed   = exprClosed
  recover  = exprRecover
  subst    = substExpr
  tryUnify ep e =
    case runUniM (matchExpr ep e M.empty) of
      Left _ -> Nothing
      Right env -> Just env

instance Matching Cmd where
  type Pattern Cmd = CmdPattern
  type Env Cmd = UniEnv
  wildcard = CPWild trivialPosition
  pattern  = cmdToPattern
  matches  = matchCmdBool
  closed   = cmdClosed
  recover  = cmdRecover
  subst    = substCmd
  tryUnify cp c =
    case runUniM (matchCmd cp c M.empty) of
      Left _ -> Nothing
      Right env -> Just env

data MatchingPair a = MatchingPair {
  matching_value :: a
  , matching_pattern :: Pattern a
  }

instance Show (MatchingPair Cmd) where
  show (MatchingPair c cp) =
    show (PrettyCmd c) ++ " ~ " ++ show (PrettyCmdPattern cp)

instance Show (MatchingPair Expr) where
  show (MatchingPair e ep) =
    show (PrettyExpr e) ++ " ~ " ++ show (PrettyExprPattern ep)

genMatchingPair' :: forall a. (Arbitrary a, Matching a) => [Var] -> Gen (MatchingPair a)
genMatchingPair' namesInScope = do
  v <- arbitrary
  wild <- genIdent `suchThat` (\x -> not $ x `elem` namesInScope)
  p <- frequency [(2, pure $ wildcard @a wild), (1, pure $ pattern v)]
  return $ MatchingPair v p

genMatchingPair :: forall a. (Arbitrary a, Matching a) => Gen (MatchingPair a)
genMatchingPair = genMatchingPair' []

instance (Arbitrary a, Matching a) => Arbitrary (MatchingPair a) where
  arbitrary = genMatchingPair
  -- not sure how to shrink...

data MatchingPrefixCmdPair = MatchingPrefixCmdPair {
  matching_prefix_value :: Cmd
  , matching_prefix_pattern :: CmdPattern
  }

genMatchingPrefixCmdPair :: Gen MatchingPrefixCmdPair
genMatchingPrefixCmdPair = do
  MatchingPair v p <- arbitrary
  tail <- arbitrary
  return $ MatchingPrefixCmdPair
            (normalizeSeq $ CSeq trivialPosition v tail)
            (normalizeSeqPattern p)

instance Arbitrary MatchingPrefixCmdPair where
  arbitrary = genMatchingPrefixCmdPair
  -- not sure how to shrink...

instance Show MatchingPrefixCmdPair where
  show (MatchingPrefixCmdPair c cp) =
    show (PrettyCmd c) ++ " ~ " ++ show (PrettyCmdPattern cp)

data MatchingAnywhereCmdPair = MatchingAnywhereCmdPair {
  matching_anywhere_value :: Cmd
  , matching_anywhere_pattern :: CmdPattern
  }

genMatchingAnywhereCmdPairSized :: MatchingPair Cmd -> Int -> Gen MatchingAnywhereCmdPair
genMatchingAnywhereCmdPairSized pair@(MatchingPair v p) sz
  | sz <= 0 = return (MatchingAnywhereCmdPair v p)
  | otherwise = do
  v' <- frequency [
    (1, CIf trivialPosition
      <$> arbitrary
      <*> (matching_anywhere_value <$> genMatchingAnywhereCmdPairSized pair (sz - 1))
      <*> arbitrary)
    , (1, CIf trivialPosition
        <$> arbitrary
        <*> arbitrary
        <*> (matching_anywhere_value <$> genMatchingAnywhereCmdPairSized pair (sz - 1)))
    , (1, CWhile trivialPosition
        <$> arbitrary
        <*> (matching_anywhere_value <$> genMatchingAnywhereCmdPairSized pair (sz - 1)))
    , (1, CSeq trivialPosition
        <$> (matching_anywhere_value <$> genMatchingAnywhereCmdPairSized pair (sz - 1))
        <*> arbitrary)
    , (1, CSeq trivialPosition
        <$> arbitrary
        <*> (matching_anywhere_value <$> genMatchingAnywhereCmdPairSized pair (sz - 1)))
    , (1, CBlock trivialPosition
        <$> (matching_anywhere_value <$> genMatchingAnywhereCmdPairSized pair (sz - 1)))
    ]
  return $ MatchingAnywhereCmdPair v' p

instance Arbitrary MatchingAnywhereCmdPair where
  arbitrary = do
    p <- arbitrary
    sized $ genMatchingAnywhereCmdPairSized p

instance Show MatchingAnywhereCmdPair where
  show (MatchingAnywhereCmdPair c cp) =
    show (PrettyCmd c) ++ " ~ " ++ show (PrettyCmdPattern cp)
