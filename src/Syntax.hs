module Syntax where

import Control.Lens
import GHC.Generics
import Data.Map
import Prelude hiding (LT, EQ, GT)
import Data.Generics.Product

type Rec = Map String Tau

data Tau = TInt
         | TFloat
         | TBool
         | TAny
         | TArr Tau (Maybe Int)
         | TBag Tau
         | TRec Rec
         deriving (Show, Eq)

data Binop = LT | LE | GT | GE | AND | OR | EQ | NEQ | PLUS | MINUS | MULT | DIV
  deriving (Show, Eq, Ord)

data Lit = LInt Int
         | LFloat Float
         | LBool Bool
         | LArr [Expr]
         | LBag [Expr]
  deriving (Show, Eq, Ord)

type Line   = Int
type Column = Int

data Position = Position Line Column
  deriving (Show, Eq, Ord)

type Var = String

data Expr = EVar     Position Var
          | ELength  Position Expr
          | ELit     Position Lit
          | EBinop   Position Expr Binop Expr
          | EIndex   Position Expr Expr
          | ERAccess Position Expr String
          | EFloat   Position Expr
          | EExp     Position Expr
          | ELog     Position Expr
          | EClip    Position
                     Expr     -- ^Expression to be clipped
                     Lit      -- ^The bound
          | EScale   Position
                     Expr     -- ^Scalar
                     Expr     -- ^Vector
          | EDot     Position
                     Expr     -- ^Vector
                     Expr     -- ^Vector
  deriving (Generic, Show, Eq, Ord)

data AtomPattern a = AtomExact a
                   | AtomWild Var
                   deriving (Show, Eq, Generic)

type VarPattern   = AtomPattern Var
type IntPattern   = AtomPattern Int
type FloatPattern = AtomPattern Float
type BoolPattern  = AtomPattern Bool
type BinopPattern = AtomPattern Binop

data LitPattern = LPInt IntPattern
                | LPFloat FloatPattern
                | LPBool BoolPattern
                | LPArr [ExprPattern]
                | LPBag [ExprPattern]
                deriving (Show, Eq, Generic)

data ExprPattern = EPWild    Position Var
                 | EPVar     Position VarPattern
                 | EPLength  Position ExprPattern
                 | EPLit     Position LitPattern
                 | EPBinop   Position ExprPattern Binop ExprPattern
                 | EPIndex   Position ExprPattern ExprPattern
                 | EPRAccess Position ExprPattern String
                 | EPFloat   Position ExprPattern
                 | EPExp     Position ExprPattern
                 | EPLog     Position ExprPattern
                 | EPClip    Position ExprPattern
                 | EPScale   Position ExprPattern ExprPattern
                 | EPDot     Position ExprPattern ExprPattern
                 deriving (Show, Eq, Generic)

exprPosn :: Expr -> Position
exprPosn e = e ^. (typed @Position)

data Decl = Decl Position Var Float Tau
  deriving (Generic, Show, Eq)

declPosn :: Decl -> Position
declPosn d = d ^. (typed @Position)

data Prog = Prog {
  getDecls :: [Decl]
  , getCmd :: Cmd
  } deriving (Show, Eq)

data Param = PExpr Expr
           | PCmd  Cmd
  deriving (Show, Eq, Ord)

data Cmd = CAssign       Position Expr   Expr
         | CLaplace      Position Var    Float Expr
         | CIf           Position Expr   Cmd   Cmd
         | CWhile        Position Expr   Cmd
         | CSeq          Position Cmd    Cmd
         | CSkip         Position
         | CExt          Position String [Param]
  deriving (Generic, Show, Eq, Ord)

data CmdPattern = CPWild    Position Var
                | CPAssign  Position ExprPattern ExprPattern
                | CPLaplace Position VarPattern  FloatPattern ExprPattern
                | CPIf      Position ExprPattern CmdPattern   CmdPattern
                | CPWhile   Position ExprPattern CmdPattern
                | CPSeq     Position CmdPattern  CmdPattern
                | CPSkip    Position
                deriving (Generic, Show, Eq)

cmdPosn :: Cmd -> Position
cmdPosn c = c ^. typed @Position
