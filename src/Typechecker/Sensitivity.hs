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
import qualified Typechecker.Basic   as TB
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
checkExpr bctxt ctx e =
  fst $ foldExpr checkVar checkLength checkLit checkBinop
                 checkIndex checkRupdate checkRaccess
                 checkArray checkBag checkFloat
                 checkExp checkClip e
  where
    checkVar _ x = (ctx ! x, bctxt ! x)

    checkLength _ (s, t) =
      case t of
        LTArray _ ->
          case s of
            C _    -> (S 0, LTSmall STInt)
            S sval -> if isInfinite sval
                      then (S infinity, LTSmall STInt)
                      else (S 0,        LTSmall STInt)
        LTBag   _ -> (s,   LTSmall STInt)
        _ -> error "Impossible: length() is applied to an expression that's not bag/array"

    checkLit _ lit =
      let Right tlit = TB.runTcM $ TB.tcExpr bctxt (ELit undefined lit)
      in case lit of
           SLit (SILit c) -> (C . fromIntegral $ c, tlit)
           SLit (SFLit c) -> (C c, tlit)
           _ -> (S 0, tlit)

    checkBinop _ (sl, tl) op (sr, _) =
      case op of
        LT  -> (approx sl sr, LTSmall STBool)
        LE  -> (approx sl sr, LTSmall STBool)
        GT  -> (approx sl sr, LTSmall STBool)
        GE  -> (approx sl sr, LTSmall STBool)
        AND -> (approx sl sr, LTSmall STBool)
        OR  -> (approx sl sr, LTSmall STBool)
        EQ  -> (approx sl sr, LTSmall STBool)
        NEQ -> (approx sl sr, LTSmall STBool)
        PLUS -> (sl + sr, tl)
        MINUS -> (sl + sr, tl)
        MULT -> (sl * sr, tl)
        DIV -> (sl / sr, tl)

    checkIndex _ (sarr, tarr) (sidx, _) =
      case tarr of
        LTArray t -> (approx sarr sidx, LTSmall t)
        LTBag   t -> (approx sarr sidx, t)
        _ -> error "Impossible: index on a non bag/array type"

    checkRupdate _ (srec, trec) _ (svalue, _) =
      (approx srec svalue, trec)

    checkRaccess _ (srec, trec) label =
      case trec of
        LTRow trec' ->
          (srec, LTSmall $ getRowTypes trec' ! label)
        _ -> error "Impossible: record access on a non record type"

    checkArray _ sexprs =
      case typ of
        LTSmall t -> (sens, LTArray t)
        LTAny -> (sens, LTArray STAny)
        _ -> error "Impossible: the basic typechecker should not allow arrays with large types"
      where (sens, typ) = foldr (\(s, t) (acc, _) -> (s + acc, t)) (0, LTAny) sexprs

    checkFloat _ (s, _) = (s, LTSmall STFloat)

    checkExp _ (s, t) =
      if s > 0 then (S infinity, t) else (0, t)

    checkBag _ sexprs =
      case typ of
        LTAny -> (2 * sens, LTBag LTAny)
        t     -> (2 * sens, LTBag t)
      where (sens, typ) =
              foldr (\(s, t) (acc, _) -> (if s > 0 then (1 + acc, t) else (acc, t)))
                    (0, LTAny)
                    sexprs

    checkClip posn (s, _) bound =
      let s' = checkClipS posn s bound
          Right t = TB.runTcM $ TB.tcExpr bctxt (ELit undefined bound)
      in (s', t)

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
           checkExp checkClip
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
checkCmd' bctxt (CAssign _ (ELength _ lhs) rhs) = \(ctx, mvs) ->
  let srhs = checkExpr bctxt ctx rhs
      maybeX = getLengthVar lhs
      mv = indexedVar lhs
      tmv = bctxt ! mv
  in case (maybeX, tmv) of
       (_, LTArray _) ->
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

  in traceShow scmvs .
     traceShow cctx $
     if deterministic && onlySensVarModified && onlyUsesT
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
