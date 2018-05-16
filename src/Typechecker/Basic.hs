{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Typechecker.Basic where

import Syntax
import Control.Monad
import Data.List
import Data.Map
import Data.Map as M
import Prelude hiding (LT, EQ, GT)

type Context = Map String LargeType

type Error = String
newtype TcM a = TcM { runTcM :: Either [Error] a }
  deriving (Functor, Monad)

instance Applicative TcM where
  pure = TcM . Right
  f <*> a =
    case (runTcM f, runTcM a) of
      (Left errs, Left moreErrs) -> TcM . Left $ errs ++ moreErrs
      (Left errs, _)             -> TcM . Left $ errs
      (Right f', Right a')       -> TcM . Right $ f' a'
      (_, Left errs)             -> TcM . Left $ errs

tcError :: Position -> String -> TcM a
tcError (Position line col) err = TcM . Left $ [errMsg]
  where errMsg = "Line " ++ show line ++ ", col " ++ show col ++ ":\n" ++ err

tcErrorIf :: Bool -> Position -> String -> a -> TcM a
tcErrorIf cond posn err val =
  if cond then tcError posn err else pure val

tcExpr :: Context -> Expr -> TcM LargeType
tcExpr ctx = foldExprM tcVar tcLength tcLit tcBinop tcIndex tcRupdate tcRaccess tcClip
  where tcVar posn = \var ->
          case M.lookup var ctx of
            Nothing -> tcError posn $ "Unknown variable: " ++ var
            Just t -> return t

        tcLength posn = \t -> do
          case t of
            LTRow _ -> tcError posn $ lengthTcError
            LTSmall _ -> tcError posn $ lengthTcError
            _ -> return $ LTSmall STInt

        tcSmallLit = \case
          SILit _ -> STInt
          SFLit _ -> STFloat
          SBLit _ -> STBool

        tcLit _ = \case
          SLit slit -> pure . LTSmall $ tcSmallLit slit
          RLit (RowLit rlit) -> pure . LTRow . RowType $ M.map tcSmallLit rlit

        isNumericST = \case
          STInt -> True
          STFloat -> True
          _ -> False

        isNumericRT rt =
          M.foldr (\t acc -> (isNumericST t) && acc) True rt

        isNumericLT = \case
          LTSmall STInt -> True
          LTSmall STFloat -> True
          LTRow rt -> isNumericRT . getRowTypes $ rt
          _ -> False

        isSmallLT = \case
          LTSmall _ -> True
          _ -> False

        isRowLT = \case
          LTRow _ -> True
          _ -> False

        tcBinop posn = \tl bop tr ->
          if tl /= tr
          then tcError posn "Binary operation applied to two different types."
          else case bop of
            LT -> tcErrorIf (not $ isNumericLT tl) posn comparisonTcError (LTSmall STBool)
            LE -> tcErrorIf (not $ isNumericLT tl) posn comparisonTcError (LTSmall STBool)
            GT -> tcErrorIf (not $ isNumericLT tl) posn comparisonTcError (LTSmall STBool)
            GE -> tcErrorIf (not $ isNumericLT tl) posn comparisonTcError (LTSmall STBool)
            AND -> tcErrorIf (not $ tl == LTSmall STBool) posn boolOpTcError (LTSmall STBool)
            OR  -> tcErrorIf (not $ tl == LTSmall STBool) posn boolOpTcError (LTSmall STBool)
            EQ  -> tcErrorIf (not $ isSmallLT tl || isRowLT tl) posn eqOpTcError (LTSmall STBool)
            NEQ -> tcErrorIf (not $ isSmallLT tl || isRowLT tl) posn eqOpTcError (LTSmall STBool)
            PLUS  -> tcErrorIf (not $ isNumericLT tl) posn arithOpTcError tl
            MINUS -> tcErrorIf (not $ isNumericLT tl) posn arithOpTcError tl
            MULT  -> tcErrorIf (not $ isNumericLT tl) posn arithOpTcError tl
            DIV   -> tcErrorIf (not $ isNumericLT tl) posn arithOpTcError tl

        tcIndex posn = \varr tidx -> do
          tarr <- tcVar posn varr
          case (tarr, tidx) of
            (LTArray t, LTSmall STInt) -> return . LTSmall $ t
            (LTBag t, LTSmall STInt) -> return t
            (_, LTSmall STInt) -> tcError posn "Indexing can only be applied to arrays or bags"
            (_, _) -> tcError posn "Index must be an integer"

        getRt posn lt =
          case lt of
            LTRow rt -> return . getRowTypes $ rt
            _ -> tcError posn "Not a row type"

        tcRupdate posn = \trow label tvalue -> do
          trow' <- getRt posn trow
          case M.lookup label trow' of
            Nothing -> tcError posn $ label ++ " doesn't exist"
            Just t  ->
              if (LTSmall t) == tvalue
              then return trow
              else tcError posn "Update value doesn't match label type"

        tcRaccess posn = \trow label -> do
          trow' <- getRt posn trow
          case M.lookup label trow' of
            Nothing -> tcError posn $ label ++ " doesn't exist"
            Just t  -> return . LTSmall $ t

        tcClip posn = \tv lit -> do
          tlit <- tcExpr ctx (ELit posn lit)
          when (tv /= tlit) $
            tcError posn $ clipTcError
          when (not $ isNumericLT tv) $
            tcError posn "Clip can only be applied to numeric types"
          return tv

tcCmdTopLevelDecls :: Cmd -> TcM Context
tcCmdTopLevelDecls = foldCmdM tcCassign tcCAupdate tcClaplace tcCif tcCwhile tcCdecl tcCseq tcCskip tcBmap tcAmap tcBsum tcPartition
  where
    tcCassign _ _ _ = pure empty

    tcCAupdate _ _ _ _ = pure empty

    tcClaplace _ _ _ _ = pure empty

    tcCif _ _ _ _ = pure empty

    tcCwhile _ _ _ = pure empty

    tcCdecl _ x _ t =
      case t of
        LTSmall _ -> pure $ M.singleton x t
        LTRow   _ -> pure $ M.singleton x t
        LTArray _ -> pure $ M.fromList [(x, t), (lenVarName x, LTSmall STInt)]
        LTBag   _ -> pure $ M.fromList [(x, t), (lenVarName x, LTSmall STInt)]

    tcCseq posn m1 m2 = do
      let multiDeclVars = M.intersection m1 m2
      if M.null multiDeclVars
      then return $ M.union m1 m2
      else tcError posn $ multiDeclTcError multiDeclVars

    tcCskip _ = return empty

    tcBmap _ _ _ _ _ _ _ = return empty

    tcAmap _ _ _ _ _ _ _ = return empty

    tcBsum _ _ _ _ _ _ = return empty

    tcPartition _ _ _ _ _ _ _ = return empty

tcCmd' :: Context -> Cmd -> TcM ()
tcCmd' ctx c =
  foldCmdA
    tcCassign
    tcCAupdate
    tcClaplace
    tcCif
    tcCwhile
    tcCdecl
    tcCseq
    tcCskip
    tcBmap
    tcAmap
    tcBsum
    tcPartition
    c
  where
    lookupVar posn x =
      case M.lookup x ctx of
        Just t -> return t
        Nothing -> tcError posn $ undeclaredVariableTcError x

    tcCassign posn x e = do
      t <- lookupVar posn x
      te <- tcExpr ctx e
      when (t /= te) $
        tcError posn $ assignTcError x e

    tcCAupdate posn x eidx erhs = do
      tidx <- tcExpr ctx eidx
      when (tidx /= LTSmall STInt) $
        tcError posn $ indexNotIntTcError eidx
      trhs <- tcExpr ctx erhs
      tx <- lookupVar posn x
      case tx of
        LTArray st -> do
          when (LTSmall st /= trhs) $
            tcError posn $ arrayUpdateTcError x eidx erhs
        LTBag t -> do
          when (t /= trhs) $
            tcError posn $ arrayUpdateTcError x eidx erhs
        _ -> tcError posn "Indexing a non-array/non-bag type"

    tcClaplace posn x _ rhs = tcCassign posn x rhs

    tcCond e = do
      te <- tcExpr ctx e
      when (te /= LTSmall STBool) $
        tcError (exprPosn e) $ condNotBoolTcError e

    tcCif _ e tcT tcF =
      tcCond e *> tcT *> tcF

    tcCwhile _ e tcBody =
      tcCond e *> tcBody

    tcCdecl _ _ _ _ = pure ()

    tcCseq _ tc1 tc2 =
      tc1 *> tc2

    tcCskip _ = return ()

    tcBmap posn _ _ _ _ _ _ = tcError posn "Bagmap should have been desugared"

    tcAmap posn _ _ _ _ _ _ = tcError posn "Arraymap should have been desugared"

    tcBsum posn _ _ _ _ _ = tcError posn "Bagsum should have been desugared"

    tcPartition posn _ _ _ _ _ _ = tcError posn "Partition should have been desugared"

tcCmd :: Cmd -> TcM Context
tcCmd c = do
  ctx <- tcCmdTopLevelDecls c
  tcCmd' ctx (desugar c)
  return ctx

-- |Various error messages.
comparisonTcError :: Error
comparisonTcError = "Comparison operator can only be applied to numeric types"

boolOpTcError :: Error
boolOpTcError = "Boolean operator can only be applied to boolean types"

eqOpTcError :: Error
eqOpTcError = "Equality testing can only be applied between primitives and records"

arithOpTcError :: Error
arithOpTcError = "Arithmetic operators can only be applied between numeric types"

clipTcError :: Error
clipTcError = "Bound and value are different types"

multiDeclTcError :: Context -> Error
multiDeclTcError multiDecls =
  let vars = M.keys multiDecls
  in "Multiple type declarations for these variables: " ++ (intercalate ", " vars)

assignTcError :: String -> Expr -> Error
assignTcError var rhs =
  "Assignment type mismatch in: " ++ var ++ " = " ++ show rhs

undeclaredVariableTcError :: String -> Error
undeclaredVariableTcError var =
  "Undeclared variable: " ++ var

indexNotIntTcError :: Expr -> Error
indexNotIntTcError eidx =
  "The index expression: " ++ show eidx ++ " is not an int"

arrayUpdateTcError :: String -> Expr -> Expr -> Error
arrayUpdateTcError var idx rhs =
  "Array update type mismatch in: " ++ var ++ "[" ++ show idx ++ "]" ++ " = " ++ show rhs

condNotBoolTcError :: Expr -> Error
condNotBoolTcError econd = "Condition expression: " ++ show econd ++ " is not a boolean"

lengthTcError :: Error
lengthTcError = "length() can only be applied to array or bag expressions"
