{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Typechecker.Sensitivity where

import Prelude hiding (LT, EQ, GT, isInfinite)

import Syntax

import GHC.Generics
import Data.Data
import Data.Map hiding (foldr)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Typechecker.Basic as TB
import Data.Number.Transfinite
import Debug.Trace

-- The derived Ord instance is the subtype relationship between sensitivity
-- types.
data Sensitivity = C Float
                 | S Float
  deriving (Show, Eq, Ord, Data, Generic)

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

checkExpr :: TB.Context -> Context -> Expr -> Sensitivity
checkExpr _     ctx (EVar _ x) = ctx ! x
checkExpr bctxt ctx (ELength _ e) =
  let s = checkExpr bctxt ctx e in
  case TB.runTcM $ TB.tcExpr bctxt e of
    Right (LTArray _ Nothing) ->
      case s of
        C _    -> 0
        S sval -> if isInfinite sval
                  then S infinity
                  else S 0
    Right (LTArray _ (Just _)) ->
      0
    Right (LTBag _) -> s
    _ -> error "Impossible: type error should be caught by basic typechecker"
checkExpr _ _ (ELit _ lit) =
  case lit of
    SLit (SILit c) -> C . fromIntegral $ c
    SLit (SFLit c) -> C c
    _ -> 0
checkExpr bctxt ctx (EBinop _ el op er) =
  let sl = checkExpr bctxt ctx el
      sr = checkExpr bctxt ctx er
  in case op of
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
checkExpr bctxt ctx (EIndex _ earr eidx) =
  let sarr = checkExpr bctxt ctx earr
      sidx = checkExpr bctxt ctx eidx
  in case TB.runTcM $ TB.tcExpr bctxt earr of
       Right _ -> approx sarr sidx
       _ -> error "Impossible: type error should be caught by basic typechecker"
checkExpr bctxt ctx (ERUpdate _ erow _ eval) =
  let srec = checkExpr bctxt ctx erow
      sval = checkExpr bctxt ctx eval
  in srec + sval
checkExpr bctxt ctx (ERAccess _ erow _) =
  checkExpr bctxt ctx erow
checkExpr bctxt ctx (EArray _ exprs) =
  foldr (\e acc -> acc + checkExpr bctxt ctx e) 0 exprs
checkExpr bctxt ctx (EBag _ exprs) =
  foldr (\e acc -> acc + if checkExpr bctxt ctx e > 0 then 2 else 0) 0 exprs
checkExpr bctxt ctx (EFloat _ e) =
  checkExpr bctxt ctx e
checkExpr bctxt ctx (EExp _ e) =
  if checkExpr bctxt ctx e > 0
  then S infinity
  else 0
checkExpr bctxt ctx (EClip posn e lit) =
  checkClipS posn (checkExpr bctxt ctx e) lit
checkExpr bctxt ctx (EScale _ escalar evec) =
  let sscalar = checkExpr bctxt ctx escalar
      svec = checkExpr bctxt ctx evec
  in sscalar * svec
checkExpr bctxt ctx (EDot _ evec1 evec2) =
  approx
    (checkExpr bctxt ctx evec1)
    (checkExpr bctxt ctx evec2)

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
  foldExpr checkVar checkLength checkLit checkBinop
           checkIndex checkRupdate checkRaccess
           checkArray checkBag checkFloat
           checkExp checkClip checkScale checkDot
  where
    checkVar _ x = S.singleton x
    checkLength _ fvs = fvs
    checkLit _ _ = S.empty
    checkBinop _ sl _ sr = S.union sl sr
    checkIndex _ sarr sidx = S.union sarr sidx
    checkRupdate _ sr _ sv = S.union sr sv
    checkArray _ sarr = foldr S.union S.empty sarr
    checkBag _ sarr = foldr S.union S.empty sarr
    checkRaccess _ sr _ = sr
    checkFloat _ s = s
    checkExp _ s = s
    checkClip _ s _ = s
    checkScale _ s1 s2 = S.union s1 s2
    checkDot _ s1 s2 = S.union s1 s2

checkToplevelDecl :: Cmd -> Context
checkToplevelDecl =
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
  where checkAssign _ _ _ = empty
        checkLaplace _ _ _ _ = empty
        checkIf _ _ _ _ = empty
        checkWhile _ _ _ = empty
        checkDecl _ x s _ = M.singleton x (S s)
        checkSeq _ ctx1 ctx2 = M.union ctx2 ctx1
        checkSkip _ = empty
        checkBmap _ _ _ _ _ _ _ = empty
        checkAmap _ _ _ _ _ _ _ = empty
        checkBsum _ _ _ _ _ _ = empty
        checkPartition _ _ _ _ _ _ _ _ = empty

readVars :: Cmd -> S.Set String
readVars (CAssign _ lhs rhs) =
  freeVars rhs `S.union` (modifiedVar lhs `S.delete` freeVars lhs)
readVars (CIf _ e ct cf) = freeVars e `S.union` readVars ct `S.union` readVars cf
readVars (CWhile _ e c) = S.union (freeVars e) (readVars c)
readVars (CDecl _ _ _ _) = S.empty
readVars (CSeq _ c1 c2) = S.union (readVars c1) (readVars c2)
readVars (CSkip _) = S.empty
readVars c = readVars . desugar $ c

checkCmd' :: TB.Context -> Cmd -> (Context, S.Set String) -> (Context, S.Set String, Float)
checkCmd' bctxt (CAssign _ (EVar _ x) e) = \(ctx, mvs) ->
  (M.insert x (checkExpr bctxt ctx e) ctx, S.insert x mvs, 0)
checkCmd' bctxt (CAssign _ (EIndex _ earr eidx) erhs) = \(ctx, mvs) ->
  let x = indexedVar earr
      tx = bctxt ! x
      mvs' = S.insert x mvs
      sidx = checkExpr bctxt ctx eidx
      srhs = checkExpr bctxt ctx erhs
  in case tx of
       LTArray _ _ ->
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
checkCmd' bctxt (CAssign _ (ELength _ lhs) rhs) = \(ctx, mvs) ->
  let srhs = checkExpr bctxt ctx rhs
      maybeX = getLengthVar lhs
      mv = indexedVar lhs
      tmv = bctxt ! mv
  in case (maybeX, tmv) of
       (_, LTArray _ Nothing) ->
         if srhs > 0
         then (M.insert mv (S infinity) ctx, S.insert mv mvs, 0)
         else (ctx, S.insert mv mvs, 0)
       (Just x, LTBag _) ->
         if srhs > 0
         then (M.adjust (+ srhs) x ctx, S.insert mv mvs, 0)
         else (ctx, S.insert mv mvs, 0)
       (Nothing, LTBag _) ->
         if srhs > 0
         then (M.adjust (+ 1) mv ctx, S.insert mv mvs, 0)
         else (ctx, S.insert mv mvs, 0)
       _ ->
         error $ "Impossible: length() update is applied to an"
                 ++ " expression that's not array/bag, this should"
                 ++ " haven been caught by the basic typechecker"
  where
    -- This function searches for a potential array/bag variable whose length is
    -- being updated.
    getLengthVar (EVar _ x) = Just x
    getLengthVar _          = Nothing
checkCmd' _ (CAssign _ _ _) =
  error $ "Impossible: assignment lhs is invalid, this"
          ++ " should have been caught by the basic typechecker"
checkCmd' bctxt (CLaplace _ x width e) = \(ctx, mvs) ->
  let s = checkExpr bctxt ctx e
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
  let s = checkExpr bctxt ctx e
      (ctxt, mvst, ept) = checkCmd' bctxt ct (ctx, S.empty)
      (ctxf, mvsf, epf) = checkCmd' bctxt cf (ctx, S.empty)
      ctx' = M.unionWith max ctxt ctxf
      mvs' = S.union mvst mvsf
  in if s > 0
     then (S.foldr (\x -> M.insert x (S infinity)) ctx' mvs', S.union mvs' mvs, max ept epf)
     else (ctx', S.union mvs' mvs, max ept epf)
checkCmd' bctxt (CWhile posn e c) =
  \(ctx, mvs) ->
    let (ctx', mvs', ep) = go (0 :: Int) ctx S.empty 0
    in (ctx', S.union mvs mvs', ep)
  where unroll = CIf posn e c (CSkip posn)
        go i ctx mvs ep
          | i > 1000  =
            (S.foldr (\x -> M.insert x (S infinity)) ctx mvs, mvs, infinity)
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
      cInputs = S.filter (\x -> ctx ! x > 0) $ readVars c

      deterministic = ep == 0
      onlySensVarModified = scmvs `S.isSubsetOf` (S.fromList [outtemp])
      onlyUsesT = cInputs `S.isSubsetOf` (S.fromList [tvar])

  in if deterministic && onlySensVarModified && onlyUsesT
     then (S.foldr
             (\x -> M.insert x (cctx ! x))
             (M.insert outvar (ctx ! invar) ctx')
             cmvs, mvs', 0)
     else (ctx', mvs', infinity)
checkCmd' bctxt cmd@(CAMap _ invar outvar tvar ivar outtemp c) = \(ctx, mvs) ->
  let desugaredCmd = desugar cmd

      (ctx', mvs', _)  = checkCmd' bctxt desugaredCmd (ctx, mvs)
      (cctx, cmvs, ep) = checkCmd' bctxt c (M.insert tvar (ctx ! invar) .
                                            M.insert ivar 0             $
                                            ctx, S.empty)
      scmvs = S.filter (\x -> cctx ! x > 0) cmvs
      cInputs = S.filter (\x -> ctx ! x > 0) $ readVars c

      deterministic = ep == 0
      onlySensVarModified = scmvs `S.isSubsetOf` (S.fromList [outtemp])
      onlyUsesTandI = cInputs `S.isSubsetOf` (S.fromList [tvar, ivar])

  in if deterministic && onlySensVarModified && onlyUsesTandI
     then (M.insert outvar (cctx ! outtemp) ctx', mvs', 0)
     else (ctx', mvs', infinity)
checkCmd' bctxt cmd@(CBSum posn invar outvar _ _ bound) = \(ctx, mvs) ->
  let sout = checkClipS posn (ctx ! invar) bound
      (ctx', mvs', _) = checkCmd' bctxt (desugar cmd) (ctx, mvs)
  in (M.insert outvar sout ctx', mvs', 0)
checkCmd'
  bctxt
  cmd@(CPartition _ invar outvar tvar ivar outindex _ partCmd) = \(ctx, mvs) ->
  let desugaredCmd = desugar cmd
      (ctx', mvs', _)  = checkCmd' bctxt desugaredCmd (ctx, mvs)
      (cctx, cmvs, ep) = checkCmd' bctxt partCmd (M.insert tvar (S infinity) .
                                                  M.insert ivar (S infinity) $
                                                  ctx, S.empty)
      scmvs = S.filter (\x -> cctx ! x > 0) cmvs
      cInputs = S.filter (\x -> ctx ! x > 0) $ readVars partCmd

      deterministic = ep == 0
      onlySensVarModified = scmvs `S.isSubsetOf` (S.fromList [outindex])
      onlyUsesT = cInputs `S.isSubsetOf` (S.fromList [tvar])
  in if deterministic && onlySensVarModified && onlyUsesT
     then (M.insert outvar (C 2 * ctx ! invar) ctx', mvs', 0)
     else (ctx', mvs', infinity)

checkCmd :: Cmd -> Either [TB.Error] (Context, Epsilon)
checkCmd c =
  case TB.runTcM $ TB.tcCmd c of
    Left errs -> Left errs
    Right bctxt ->
      let (ctx, _, ep) = checkCmd' bctxt c (checkToplevelDecl c, S.empty)
      in Right (ctx, ep)
