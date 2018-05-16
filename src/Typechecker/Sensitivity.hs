{-# LANGUAGE LambdaCase #-}

module Typechecker.Sensitivity where

import Prelude hiding (LT, EQ, GT)

import Syntax

import Data.Map
import Data.Map as M
import qualified Data.Set as S
import qualified Typechecker.Basic   as TB
import Data.Number.Transfinite

-- The derived Ord instance is the subtype relationship between sensitivity
-- types.
data Sensitivity = C Float
                 | S Float
  deriving (Show, Eq, Ord)

type Epsilon = Float

approx :: Sensitivity -> Sensitivity -> Sensitivity
approx (S s1) (S s2) =
  if s1 > 0 || s2 > 0
  then S infinity
  else S 0
approx (S s) (C _) =
  if s > 0
  then S infinity
  else S 0
approx (C _) (C _) = S 0
approx c@(C _) s@(S _) = approx s c

calcEps :: Sensitivity -> Float -> Float
calcEps (S s) w = s / w
calcEps (C _) _ = 0

instance Num Sensitivity where
  (S s)   + (S s') = S (s + s')
  (C c)   + (C c') = C (c + c')
  (S s)   + (C _)  = S s
  c@(C _) + s@(S _) = s + c


  s1@(S _) * s2@(S _) = approx s1 s2
  (C c)    * (C c')   = C (c * c')
  (S s)    * (C c)    = S (s * c)
  c@(C _)  * s@(S _)  = s * c

  fromInteger = S . fromInteger

  abs    _ = error "abs is not supported for Sensitivity"
  signum _ = error "signum is not supported for Sensitivity"
  negate _ = error "negate is not supported for Sensitivity"

instance Fractional Sensitivity where
  s1@(S _) / s2@(S _) = approx s1 s2
  (S s)    / (C c)    = S (s / c)
  (C c1)   / (C c2)   = C (c1 / c2)
  c@(C _)  / s@(S _)  = approx c s

  fromRational = S . fromRational

type Context = Map String Sensitivity

checkExpr :: Context -> Expr -> Sensitivity
checkExpr ctx =
  foldExpr checkVar checkLenVar checkLit checkBinop checkIndex checkRupdate checkRaccess checkClipS
  where
    checkVar _ x = ctx ! x

    checkLenVar _ x = ctx ! (lenVarName x)

    checkLit _ lit =
      case lit of
        SLit (SILit c) -> C . fromIntegral $ c
        SLit (SFLit c) -> C c
        _ -> S 0

    checkBinop _ sl op sr =
      case op of
        LT  -> approx sl sr
        LE  -> approx sl sr
        GT  -> approx sl sr
        GE  -> approx sl sr
        AND -> approx sl sr
        OR  -> approx sl sr
        EQ  -> approx sl sr
        NEQ -> approx sl sr
        PLUS -> sl + sr
        MINUS -> sl + sr
        MULT -> sl * sr
        DIV -> sl / sr

    checkIndex _ x sidx =
      approx (ctx ! x) sidx

    checkRupdate _ srec _ svalue =
      approx srec svalue

    checkRaccess _ srec _ =
      srec

scaleST :: SmallLit -> Sensitivity
scaleST = \case
  SILit c -> S . fromIntegral $ 2 * abs c
  SFLit c -> S $ 2 * abs c
  _       -> error $ "Impossible: clip should only be applied to numeric types."
                     ++ " This should have been caught be basic typechecker"

checkClipS :: Position -> Sensitivity -> Literal -> Sensitivity
checkClipS _ _ bound =
  case bound of
    SLit (SILit c) -> (S . fromIntegral $ 2 * abs c)
    SLit (SFLit c) -> (S (2 * abs c))
    RLit rlit -> scale rlit
    _ -> error $ "Impossible: clip should only be applied to numeric types."
                 ++ " This should have been caught be basic typechecker"
  where scale (RowLit rlit) =
          M.foldr (\lit -> max (scaleST lit)) (S 0) rlit

freeVars :: Expr -> S.Set String
freeVars =
  foldExpr checkVar checkLenVar checkLit checkBinop checkIndex checkRupdate checkRaccess checkClip
  where
    checkVar _ x = S.singleton x
    checkLenVar _ x = S.singleton . lenVarName $ x
    checkLit _ _ = S.empty
    checkBinop _ sl _ sr = S.union sl sr
    checkIndex _ x sidx = S.insert x sidx
    checkRupdate _ sr _ sv = S.union sr sv
    checkRaccess _ sr _ = sr
    checkClip _ s _ = s

checkToplevelDecl :: Cmd -> Context
checkToplevelDecl =
  foldCmd
    checkAssign
    checkAupdate
    checkLaplace
    checkIf
    checkWhile
    checkDecl
    checkSeq
    checkSkip
    checkBmap
    checkAmap
    checkBsum
  where checkAssign _ _ _ = empty
        checkAupdate _ _ _ _ = empty
        checkLaplace _ _ _ _ = empty
        checkIf _ _ _ _ = empty
        checkWhile _ _ _ = empty
        checkDecl _ x s t =
          case t of
            LTSmall _ -> M.singleton x (S s)
            LTBag _ -> M.fromList [(x, S s), (lenVarName x, S s)]
            LTArray _ -> M.fromList [(x, S s), (lenVarName x, 0)]
            LTRow _ -> M.singleton x (S s)
        checkSeq _ ctx1 ctx2 = M.union ctx2 ctx1
        checkSkip _ = empty
        checkBmap _ _ _ _ _ _ _ = empty
        checkAmap _ _ _ _ _ _ _ = empty
        checkBsum _ _ _ _ _ _ = empty

readVars :: Cmd -> S.Set String
readVars (CAssign _ _ rhs) = freeVars rhs
readVars (CAUpdate _ _ idx rhs) = S.union (freeVars idx) (freeVars rhs)
readVars (CLaplace _ _ _ e) = freeVars e
readVars (CIf _ e ct cf) = freeVars e `S.union` readVars ct `S.union` readVars cf
readVars (CWhile _ e c) = S.union (freeVars e) (readVars c)
readVars (CDecl _ _ _ _) = S.empty
readVars (CSeq _ c1 c2) = S.union (readVars c1) (readVars c2)
readVars (CSkip _) = S.empty
readVars c = readVars . desugar $ c

checkCmd' :: TB.Context -> Cmd -> (Context, S.Set String) -> (Context, S.Set String, Float)
checkCmd' _ (CAssign _ x e) = \(ctx, mvs) ->
  (M.insert x (checkExpr ctx e) ctx, S.insert x mvs, 0)
checkCmd' bctxt (CAUpdate _ x eidx erhs) = \(ctx, mvs) ->
  let tx = bctxt ! x
      mvs' = S.insert x mvs
      sidx = checkExpr ctx eidx
      srhs = checkExpr ctx erhs
  in case tx of
       LTArray _ ->
         if sidx > 0
         then (M.insert x (S infinity) ctx, mvs', 0)
         else (M.adjust (+ srhs) x ctx, mvs', 0)
       LTBag   _ ->
         if sidx > 0
         then (M.insert x (S infinity) ctx, mvs', 0)
         else
           if srhs > 0
           then (M.adjust (+ 1) x   ctx, mvs', 0)
           else (M.insert       x 0 ctx, mvs', 0)
       _ -> error $ "Impossible: array update on non-array/non-bag type, "
                    ++ "this should have been caught by basic typechecker"
checkCmd' bctxt (CLaplace _ x width e) = \(ctx, mvs) ->
  let s = checkExpr ctx e
      mvs' = S.insert x mvs
      tx = bctxt ! x
  in case tx of
       LTSmall _ -> (M.insert x 0 ctx, mvs',
                     calcEps s width)
       LTRow rt  -> (M.insert x 0 ctx, mvs',
                     (fromIntegral . M.size . getRowTypes $ rt) * calcEps s width)
       _ -> error $ "Impossible: laplace mechanism should only be applied to numeric variables, "
                    ++ "this should have been caught by basic typechecker"
checkCmd' bctxt (CIf _ e ct cf) = \(ctx, mvs) ->
  let s = checkExpr ctx e
      (ctxt, mvst, ept) = checkCmd' bctxt ct (ctx, mvs)
      (ctxf, mvsf, epf) = checkCmd' bctxt cf (ctx, mvs)
      ctx' = M.unionWith max ctxt ctxf
      mvs' = S.union mvst mvsf
  in if s > 0
     then (S.foldr (\x -> M.insert x (S infinity)) ctx' mvs', mvs', max ept epf)
     else (ctx', mvs', max ept epf)
checkCmd' bctxt (CWhile posn e c) =
  \(ctx, mvs) -> go (0 :: Int) ctx mvs 0
  where unroll = CIf posn e c (CSkip posn)
        go i ctx mvs ep
          | i > 1000  = (S.foldr (\x -> M.insert x (S infinity)) ctx mvs, mvs, infinity)
          | otherwise =
            let (ctx', mvs', ep') = checkCmd' bctxt unroll (ctx, mvs)
            in if ctx' == ctx && mvs' == mvs && ep' == 0
               then (ctx', mvs', ep')
               else go (i + 1) ctx' mvs' (ep + ep')
checkCmd' _ (CDecl _ _ _ _)   = \(ctx, mvs) -> (ctx, mvs, 0)
checkCmd' bctxt (CSeq _ c1 c2) = \(ctx, mvs) ->
  let (ctx1, mvs1, ep1) = checkCmd' bctxt c1 (ctx, mvs)
      (ctx2, mvs2, ep2) = checkCmd' bctxt c2 (ctx1, mvs1)
  in (ctx2, mvs2, ep1 + ep2)
checkCmd' _ (CSkip _) = \(ctxt, mvs) -> (ctxt, mvs, 0)
checkCmd' bctxt cmd@(CBMap _ invar outvar tvar ivar outtemp c) = \(ctx, mvs) ->
  let desugaredCmd = desugar cmd

      (ctx', mvs', _)  = checkCmd' bctxt desugaredCmd (ctx, mvs)
      (cctx, cmvs, ep) = checkCmd' bctxt c (M.insert tvar (S infinity) .
                                            M.insert ivar (S infinity) $
                                            ctx, S.empty)
      scmvs = S.filter (\x -> cctx ! x > 0) cmvs
      cInputs = readVars c

      deterministic = ep == 0
      onlySensVarModified = scmvs `S.isSubsetOf` (S.fromList [outtemp])
      onlyUsesTandI = cInputs `S.isSubsetOf` (S.fromList [tvar, ivar])

  in if deterministic && onlySensVarModified && onlyUsesTandI
     then (M.insert outvar (ctx ! invar) ctx', mvs', 0)
     else (ctx', mvs', ep)
checkCmd' bctxt cmd@(CAMap _ invar outvar tvar ivar outtemp c) = \(ctx, mvs) ->
  let desugaredCmd = desugar cmd

      (ctx', mvs', _) = checkCmd' bctxt desugaredCmd (ctx, mvs)
      (cctx, cmvs, ep) = checkCmd' bctxt c (M.insert tvar (ctx ! invar) .
                                           M.insert ivar 0             $
                                           ctx, S.empty)
      scmvs = S.filter (\x -> cctx ! x > 0) cmvs
      cInputs = readVars c

      deterministic = ep == 0
      onlySensVarModified = scmvs `S.isSubsetOf` (S.fromList [outtemp])
      onlyUsesTandI = cInputs `S.isSubsetOf` (S.fromList [tvar, ivar])

  in if deterministic && onlySensVarModified && onlyUsesTandI
     then (M.insert outvar (cctx ! outtemp) ctx', mvs', 0)
     else (ctx', mvs', ep)
checkCmd' bctxt cmd@(CBSum posn invar outvar _ _ bound) = \(ctx, mvs) ->
  let sout = checkClipS posn (ctx ! invar) bound
      (ctx', mvs', _) = checkCmd' bctxt (desugar cmd) (ctx, mvs)
  in (M.insert outvar sout ctx', mvs', 0)

checkCmd :: Cmd -> Either [TB.Error] (Context, Epsilon)
checkCmd c =
  case TB.runTcM $ TB.tcCmd c of
    Left errs -> Left errs
    Right bctxt ->
      let (ctx, _, ep) = checkCmd' bctxt c (checkToplevelDecl c, S.empty)
      in Right (ctx, ep)
