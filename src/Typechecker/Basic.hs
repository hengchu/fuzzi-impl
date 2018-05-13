{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Typechecker.Basic where

import Syntax
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

tcError :: String -> TcM a
tcError err = TcM . Left $ [err]

tcErrorIf :: Bool -> String -> a -> TcM a
tcErrorIf cond err val =
  if cond then tcError err else pure val

tcExpr :: Context -> Expr -> TcM LargeType
tcExpr ctx = foldExprM tcVar tcLit tcBinop tcIndex tcRupdate tcRaccess
  where tcVar = \var ->
          case M.lookup var ctx of
            Nothing -> tcError $ "Unknown variable: " ++ var
            Just t -> return t

        tcSmallLit = \case
          SILit _ -> STInt
          SFLit _ -> STFloat
          SBLit _ -> STBool
        tcLit = \case
          SLit slit -> pure . LTSmall $ tcSmallLit slit
          RLit (RowLit rlit) -> pure . LTRow . RowType $ M.map tcSmallLit rlit

        isNumericLT = \case
          LTSmall STInt -> True
          LTSmall STFloat -> True
          _ -> False

        isSmallLT = \case
          LTSmall _ -> True
          _ -> False

        isRowLT = \case
          LTRow _ -> True
          _ -> False
        
        tcBinop = \tl bop tr ->
          if tl /= tr
          then tcError "Binary operation applied to two different types."
          else case bop of
            LT -> tcErrorIf (not $ isNumericLT tl) comparisonTcError (LTSmall STBool)
            LE -> tcErrorIf (not $ isNumericLT tl) comparisonTcError (LTSmall STBool)
            GT -> tcErrorIf (not $ isNumericLT tl) comparisonTcError (LTSmall STBool)
            GE -> tcErrorIf (not $ isNumericLT tl) comparisonTcError (LTSmall STBool)
            AND -> tcErrorIf (not $ tl == LTSmall STBool) boolOpTcError (LTSmall STBool)
            OR  -> tcErrorIf (not $ tl == LTSmall STBool) boolOpTcError (LTSmall STBool)
            EQ  -> tcErrorIf (not $ isSmallLT tl || isRowLT tl) eqOpTcError (LTSmall STBool)
            NEQ -> tcErrorIf (not $ isSmallLT tl || isRowLT tl) eqOpTcError (LTSmall STBool)
            PLUS  -> tcErrorIf (not $ isNumericLT tl) arithOpTcError tl
            MINUS -> tcErrorIf (not $ isNumericLT tl) arithOpTcError tl
            MULT  -> tcErrorIf (not $ isNumericLT tl) arithOpTcError tl
            DIV   -> tcErrorIf (not $ isNumericLT tl) arithOpTcError tl

        tcIndex = \tarr tidx ->
          case (tarr, tidx) of
            (LTArray t, LTSmall STInt) -> return . LTSmall $ t
            (LTBag t, LTSmall STInt) -> return t
            (_, LTSmall STInt) -> tcError "Indexing can only be applied to arrays or bags"
            (_, _) -> tcError "Index must be an integer"

        getRt lt =
          case lt of
            LTRow rt -> return . getRowTypes $ rt
            _ -> tcError "Not a row type"

        tcRupdate = \trow label tvalue -> do
          trow' <- getRt trow
          case M.lookup label trow' of
            Nothing -> tcError $ label ++ " doesn't exist"
            Just t  ->
              if (LTSmall t) == tvalue
              then return trow
              else tcError "Update value doesn't match label type"

        tcRaccess = \trow label -> do
          trow' <- getRt trow
          case M.lookup label trow' of
            Nothing -> tcError $ label ++ " doesn't exist"
            Just t  -> return . LTSmall $ t

-- |Various error messages.
comparisonTcError :: Error
comparisonTcError = "Comparison operator can only be applied to numeric types"

boolOpTcError :: Error
boolOpTcError = "Boolean operator can only be applied to boolean types"

eqOpTcError :: Error
eqOpTcError = "Equality testing can only be applied between primitives and records"

arithOpTcError :: Error
arithOpTcError = "Arithmetic operators can only be applied between primitives"
