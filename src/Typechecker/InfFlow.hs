{-# LANGUAGE FlexibleContexts #-}

module Typechecker.InfFlow where

import Syntax

import Data.Map
import Data.Map as M
import qualified Data.Set as S

import Prelude hiding (LT, EQ, GT)

type Context = Map String Bool

approx :: Bool -> Bool -> Bool
approx inf1 inf2 = inf1 || inf2

checkExpr :: Context -> Expr -> Bool
checkExpr ctx e =
  foldExpr checkVar checkLength checkLit checkBinop checkIndex checkRupdate checkRaccess checkArray checkBag checkClip e
  where
    checkVar _ x = ctx ! x

    checkLength _ se = se

    checkLit _ _ = False

    checkBinop _ infL _ infR =
      approx infL infR

    checkIndex _ infArr infIdx =
      approx infArr infIdx

    checkRupdate _ infR _ infV =
      approx infR infV

    checkRaccess _ infR _ =
      infR

    -- TODO: fix this
    checkArray _ _ = error "Not implemented"

    checkBag _ _ = error "Not implemented"

    checkClip _ infE _ = infE

checkToplevelDecl :: Cmd -> Context
checkToplevelDecl c =
  foldCmd
    checkAssign
    checkLaplace
    checkIf
    checkWhile
    checkDecl
    checkSeq
    checkSkip
    checkBmap
    checkAmap
    checkBsum
    checkPartition
    c
  where checkAssign _ _ _ = empty
        checkLaplace _ _ _ _ = empty
        checkIf _ _ _ _ = empty
        checkWhile _ _ _ = empty
        checkDecl _ x s _ = M.singleton x (s > 0)
        checkSeq _ ctx1 ctx2 = M.union ctx1 ctx2
        checkSkip _ = empty
        checkBmap _ _ _ _ _ _ _ = empty
        checkAmap _ _ _ _ _ _ _ = empty
        checkBsum _ _ _ _ _ _ = empty
        checkPartition _ _ _ _ _ _ _ _ = empty

checkCmd' :: Cmd -> (Context, S.Set String) -> (Context, S.Set String)
checkCmd' (CAssign _ x e) =
  let mv = modifiedVar x in
  \(ctx, mvs) -> (M.insert mv (checkExpr ctx e) ctx, S.insert mv mvs)
checkCmd' (CLaplace _ x _ _) =
  \(ctx, mvs) -> (M.insert x False ctx, S.insert x mvs)
checkCmd' (CIf _ e ct cf) =
  \(ctx, mvs) ->
    let (ctxt, mvst) = checkCmd' ct (ctx, S.empty)
        (ctxf, mvsf) = checkCmd' cf (ctx, S.empty)
        ctx'         = M.unionWith approx ctxt ctxf
        mvs'         = S.union mvst mvsf
    in if checkExpr ctx e
       then (S.foldr (\x -> M.insert x True) ctx' mvs', S.union mvs mvs')
       else (ctx', S.union mvs mvs')
checkCmd' while@(CWhile posn e c) =
  \(ctx, mvs) ->
    let (ctx', mvs') = checkCmd' unroll (ctx, mvs)
    in if ctx == ctx' && mvs == mvs'
       then (ctx, mvs)
       else checkCmd' while (ctx', mvs')
  where unroll = (CIf posn e c (CSkip posn))
checkCmd' (CDecl _ _ _ _) = id
checkCmd' (CSeq _ c1 c2) = checkCmd' c2 . checkCmd' c1
checkCmd' (CSkip _) = id
checkCmd' _ = error "Impossible: macros should have been desugared!"

checkCmd :: Cmd -> Context
checkCmd c =
  let ctx = checkToplevelDecl c
  in fst $ checkCmd' (desugar c) (ctx, S.empty)
