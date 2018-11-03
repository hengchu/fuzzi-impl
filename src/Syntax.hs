{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Syntax where

import Data.List
import Control.Lens ((^.))
import GHC.Generics
import Data.Map hiding ((\\))
import Prelude hiding (LT, EQ, GT)
import Data.Generics.Product
import Test.QuickCheck

type RecordTy = Map String Tau

data Tau = TInt
         | TFloat
         | TBool
         | TAny
         | TArr Tau (Maybe Int)
         | TBag Tau
         | TRec RecordTy
         deriving (Show, Eq)

data Binop = LT | LE | GT | GE | AND | OR | EQ | NEQ | PLUS | MINUS | MULT | DIV
  deriving (Show, Eq, Ord, Enum, Bounded)

data Lit = LInt Int
         | LFloat Float
         | LBool Bool
         | LArr [Expr]
         | LBag [Expr]
  deriving (Show, Eq, Ord)

type Line   = Int
type Column = Int

data Position = Position Line Column
  deriving (Show, Ord)

instance Eq Position where
  _ == _ = True

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

-- Turn CSeqs into a linked list, flattening all tree structures
normalizeSeq :: Cmd -> Cmd
normalizeSeq (CSeq p (CSeq p' c1 c2) c3) =
  normalizeSeq $ CSeq p c1 (CSeq p' c2 c3)
normalizeSeq (CSeq p c1 c2) =
  CSeq p (normalizeSeq c1) (normalizeSeq c2)
normalizeSeq (CIf p e c1 c2) =
  CIf p e (normalizeSeq c1) (normalizeSeq c2)
normalizeSeq (CWhile p e c) =
  CWhile p e $ normalizeSeq c
normalizeSeq (CExt p name params) = (CExt p name (fmap normalizeParam params))
normalizeSeq c = c

normalizeParam :: Param -> Param
normalizeParam (PCmd c) = PCmd $ normalizeSeq c
normalizeParam p = p

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

trivialPosition :: Position
trivialPosition = Position 0 0

genIdent :: Gen Var
genIdent =
  (listOf1 . elements $ ['a'..'z'] ++ ['A'..'Z'])
  `suchThat` (\x -> not $ x `elem` keywords)

genAssocOp :: Gen Binop
genAssocOp = elements [AND, OR, PLUS, MINUS, MULT, DIV]

genNonAssocOp :: Gen Binop
genNonAssocOp = elements $ [minBound .. maxBound] \\ [AND, OR, PLUS, MINUS, MULT, DIV]

genLitSized :: Int -> Gen Lit
genLitSized sz
  | sz <= 0 = frequency [
      (1, LInt <$> arbitrary)
      , (1, LFloat <$> arbitrary)
      , (1, LBool <$> arbitrary)
      , (1, pure (LArr []))
      , (1, pure (LBag []))
      ]
  | otherwise = frequency [
      (1, LInt <$> arbitrary)
      , (1, LFloat <$> arbitrary)
      , (1, LBool <$> arbitrary)
      , (1, LArr <$> listOf (genExprSized (sz - 1)))
      , (1, LBag <$> listOf (genExprSized (sz - 1)))
      ]

genAssocBinopSized :: Int -> Gen Expr
genAssocBinopSized sz =
  EBinop trivialPosition <$> genExprSized (sz - 1) <*> genAssocOp <*> genExprSized (sz - 1)

isBinop :: Expr -> Bool
isBinop (EBinop _ _ _ _) = True
isBinop _ = False

genNonAssocBinopSized :: Int -> Gen Expr
genNonAssocBinopSized sz = do
  el <- genExprSized (sz - 1) `suchThat` (not . isBinop)
  er <- genExprSized (sz - 1) `suchThat` (not . isBinop)
  op <- genNonAssocOp
  return $ EBinop trivialPosition el op er

genExprSized :: Int -> Gen Expr
genExprSized sz
  | sz <= 0 =
    EVar trivialPosition
    <$> genIdent
  | otherwise = frequency [
        (1, EVar trivialPosition
          <$> genIdent)
      , (1, ELength trivialPosition
          <$> genExprSized (sz - 1))
      , (1, genAssocBinopSized sz)
      , (1, genNonAssocBinopSized sz)
      , (1, EIndex trivialPosition
          <$> genExprSized (sz - 1)
          <*> genExprSized (sz - 1))
      , (1, ERAccess trivialPosition
          <$> genExprSized (sz - 1)
          <*> genIdent)
      , (1, EFloat trivialPosition
          <$> genExprSized (sz - 1))
      , (1, EExp trivialPosition
          <$> genExprSized (sz - 1))
      , (1, ELog trivialPosition
          <$> genExprSized (sz - 1))
      , (1, EClip trivialPosition
          <$> genExprSized (sz - 1)
          <*> genLitSized (sz - 1))
      , (1, EScale trivialPosition
          <$> genExprSized (sz - 1)
          <*> genExprSized (sz - 1))
      , (1, EDot trivialPosition
          <$> genExprSized (sz - 1)
          <*> genExprSized (sz - 1))
      ]

genParamSized :: Int -> Gen Param
genParamSized sz = frequency [
  (1, PExpr <$> genExprSized (sz - 1))
  , (1, PCmd <$> genCmdSized (sz - 1))
  ]

shrinkParam :: Param -> [Param]
shrinkParam (PExpr e) = PExpr <$> shrink e
shrinkParam (PCmd c) = PCmd <$> shrink c

keywords :: [String]
keywords =
  ["exp", "log", "lap", "length", "clip", "scale"
  , "dot", "true", "false", "if", "then", "else"
  , "end", "do", "while", "repeat", "skip", "fc"]

genCmdSized :: Int -> Gen Cmd
genCmdSized sz
  | sz <= 0 = return (CSkip trivialPosition)
  | otherwise = frequency [
      (1, CAssign trivialPosition
        <$> arbitrary <*> arbitrary)
      , (1, CLaplace trivialPosition
          <$> genIdent <*> arbitrary `suchThat` (>0) <*> arbitrary)
      , (1, CIf trivialPosition
          <$> arbitrary <*> genCmdSized (sz - 1) <*> genCmdSized (sz - 1))
      , (1, CWhile trivialPosition
          <$> arbitrary <*> genCmdSized (sz - 1))
      , (1, CSeq trivialPosition
          <$> genCmdSized (sz - 1) <*> genCmdSized (sz - 1))
      , (1, pure $ CSkip trivialPosition)
      , (1, CExt trivialPosition
          <$> genIdent
          <*> (listOf1 $ genParamSized (sz - 1)))
      ]

shrinkCmd :: Cmd -> [Cmd]
shrinkCmd (CAssign _ e1 e2) =
  [CAssign trivialPosition e1' e2'
  | e1' <- shrinkExpr e1, e2' <- shrinkExpr e2] ++ [CSkip trivialPosition]
shrinkCmd (CLaplace _ x f e) =
  [CLaplace trivialPosition x f' e'
  | f' <- shrink f, e' <- shrink e] ++ [CSkip trivialPosition]
shrinkCmd (CIf _ e c1 c2) =
  [CIf trivialPosition e' c1' c2'
  | e' <- shrink e, c1' <- shrinkCmd c1, c2' <- shrinkCmd c2] ++ [c1, c2]
shrinkCmd (CWhile _ e c) =
  [CWhile trivialPosition e' c'
  | e' <- shrink e, c' <- shrinkCmd c] ++ [c]
shrinkCmd (CSeq _ c1 c2) =
  [c1, c2] ++ [CSeq trivialPosition c1' c2'
  | c1' <- shrinkCmd c1, c2' <- shrinkCmd c2]
shrinkCmd (CSkip _) = [CSkip trivialPosition]
shrinkCmd (CExt _ name params) =
  [CExt trivialPosition name params' | params' <- shrink params]

shrinkLit :: Lit -> [Lit]
shrinkLit (LInt i) =
  [LInt i' | i' <- shrink i]
shrinkLit (LFloat f) =
  [LFloat f' | f' <- shrink f]
shrinkLit (LBool b) =
  [LBool b' | b' <- shrink b]
shrinkLit (LArr es) =
  LArr <$> shrink es
shrinkLit (LBag es) =
  LBag <$> shrink es

shrinkExpr :: Expr -> [Expr]
shrinkExpr (EVar _ x) = [EVar trivialPosition x]
shrinkExpr (ELength _ e) =
  let e' = shrinkExpr e
  in (fmap (ELength trivialPosition) e') ++ e'
shrinkExpr (ELit _ lit) =
  fmap (ELit trivialPosition) (shrinkLit lit)
shrinkExpr (EBinop _ e1 op e2) =
  let e1' = shrinkExpr e1
      e2' = shrinkExpr e2
  in [EBinop trivialPosition lhs op rhs | lhs <- e1', rhs <- e2'] ++ e1' ++ e2'
shrinkExpr (EIndex _ e1 e2) =
  let e1' = shrinkExpr e1
      e2' = shrinkExpr e2
  in [EIndex trivialPosition lhs rhs | lhs <- e1', rhs <- e2'] ++ e1' ++ e2'
shrinkExpr (ERAccess _ e label) =
  let e' = shrinkExpr e
  in [ERAccess trivialPosition r label | r <- e'] ++ e'
shrinkExpr (EFloat _ e) =
  let e' = shrinkExpr e
  in fmap (EFloat trivialPosition) e' ++ e'
shrinkExpr (EExp _ e) =
  let e' = shrinkExpr e
  in fmap (EExp trivialPosition) e' ++ e'
shrinkExpr (ELog _ e) =
  let e' = shrinkExpr e
  in fmap (ELog trivialPosition) e' ++ e'
shrinkExpr (EClip _ e lit) =
  let e' = shrinkExpr e
  in [EClip trivialPosition e1 lit' | e1 <- e', lit' <- shrinkLit lit] ++ e'
shrinkExpr (EScale _ e1 e2) =
  let e1' = shrinkExpr e1
      e2' = shrinkExpr e2
  in [EScale trivialPosition lhs rhs | lhs <- e1', rhs <- e2'] ++ e1' ++ e2'
shrinkExpr (EDot _ e1 e2) =
  let e1' = shrinkExpr e1
      e2' = shrinkExpr e2
  in [EDot trivialPosition lhs rhs | lhs <- e1', rhs <- e2'] ++ e1' ++ e2'

instance Arbitrary Param where
  arbitrary = sized genParamSized
  shrink = shrinkParam

instance Arbitrary Lit where
  arbitrary = sized genLitSized
  shrink = shrinkLit

instance Arbitrary Expr where
  arbitrary = sized genExprSized
  shrink = shrinkExpr

instance Arbitrary Cmd where
  arbitrary = sized genCmdSized
  shrink = shrinkCmd
