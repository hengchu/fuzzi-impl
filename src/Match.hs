{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Match where

import Syntax
import qualified Data.Set as S
import qualified Data.Map as M

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

matchParam :: ParamPattern -> Param -> UniEnv -> UniM UniEnv
matchParam (PPCmd cp) (PCmd c) env =
  matchCmd cp c env
matchParam (PPExpr ep) (PExpr e) env =
  matchExpr ep e env
matchParam _ _ _ = justFail

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
matchCmd _ _ _ = justFail

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
