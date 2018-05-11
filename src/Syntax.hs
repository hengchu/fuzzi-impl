module Syntax where

import Data.Map

data SmallType = STInt | STFloat | STBool
  deriving (Show, Eq)

newtype RowType = RowType { getRowTypes :: Map String SmallType }
  deriving (Show, Eq)

data LargeType = LTSmall SmallType
               | LTRow   RowType
               | LTArray SmallType
               | LTBag   LargeType
  deriving (Show, Eq)

data Binop = LT | LE | GT | GE | AND | OR | EQ | NEQ | PLUS | MINUS | MULT | DIV
  deriving (Show, Eq)

data SmallLit = SILit Int
              | SFLit Float
              | SBLit Bool
  deriving (Show, Eq)

newtype RowLit = RowLit { getRowLits :: Map String SmallLit }
  deriving (Show, Eq)

data Literal = SLit SmallLit
             | RLit RowLit
  deriving (Show, Eq)

data Expr = EVar     String
          | ELit     Literal
          | EBinop   Expr Binop Expr
          | EIndex   Expr Expr
          | ERUpdate Expr String Expr
          | ERAccess Expr String
  deriving (Show, Eq)

data Cmd = CAssign  String Expr
         | CAUpdate String Expr  Expr
         | CLaplace String Float Expr
         | CIf      Expr   Cmd   Cmd
         | CWhile   Expr   Cmd
         | CDecl    String Float LargeType
         | CSeq     Cmd    Cmd
         | CSkip
  deriving (Show, Eq)
