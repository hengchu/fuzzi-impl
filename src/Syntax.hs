{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Syntax where

import Data.Data
import Data.List
import Control.Lens ((^.))
import GHC.Generics
import Data.Map hiding ((\\))
import Prelude hiding (LT, EQ, GT)
import Data.Generics.Product
import Test.QuickCheck
import Generic.Random

type RecordTy = Map String Tau

data Tau = TInt
         | TFloat
         | TBool
         | TAny
         | TArr Tau (Maybe Int)
         | TBag Tau
         | TRec RecordTy
         deriving (Show, Eq, Generic, Data)

data Binop = LT | LE | GT | GE | AND | OR | EQ | NEQ | PLUS | MINUS | MULT | DIV
  deriving (Show, Eq, Ord, Enum, Bounded, Data)

data Lit = LInt Int
         | LFloat Float
         | LBool Bool
         | LArr [Expr]
         | LBag [Expr]
  deriving (Show, Eq, Ord, Data)

type Line   = Int
type Column = Int

data Position = Position Line Column
  deriving (Show, Ord, Data)

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
  deriving (Generic, Show, Eq, Ord, Data)

data AtomPattern a = AtomExact a
                   | AtomWild Var
                   deriving (Show, Eq, Generic, Data)

atomToPattern :: a -> AtomPattern a
atomToPattern = AtomExact

atomClosed :: AtomPattern a -> Bool
atomClosed (AtomExact _) = True
atomClosed _ = False

atomRecover :: AtomPattern a -> Maybe a
atomRecover (AtomExact a) = Just a
atomRecover _ = Nothing

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
                deriving (Show, Eq, Generic, Data)

litToPattern :: Lit -> LitPattern
litToPattern (LInt i) = LPInt . atomToPattern $ i
litToPattern (LFloat f) = LPFloat . atomToPattern $ f
litToPattern (LBool b) = LPBool . atomToPattern $ b
litToPattern (LArr es) = LPArr $ fmap exprToPattern es
litToPattern (LBag es) = LPBag $ fmap exprToPattern es

litClosed :: LitPattern -> Bool
litClosed (LPInt ip) = atomClosed ip
litClosed (LPFloat fp) = atomClosed fp
litClosed (LPBool bp) = atomClosed bp
litClosed (LPArr ep) = all exprClosed ep
litClosed (LPBag ep) = all exprClosed ep

newtype AllJust a = AllJust { getAllJust :: Maybe [a] }

instance Semigroup (AllJust a) where
  (AllJust (Just xs)) <> (AllJust (Just ys)) = AllJust . Just $ xs ++ ys
  _                   <> _                   = AllJust Nothing

instance Monoid (AllJust a) where
  mempty = AllJust . Just $ []
  mappend = (<>)

litRecover :: LitPattern -> Maybe Lit
litRecover (LPInt ip) = LInt <$> atomRecover ip
litRecover (LPFloat fp) = LFloat <$> atomRecover fp
litRecover (LPBool bp) = LBool <$> atomRecover bp
litRecover (LPArr eps) =
  LArr <$> (getAllJust (foldMap (\ep -> AllJust $ (:[]) <$> (exprRecover ep)) eps))
litRecover (LPBag eps) =
  LBag <$> (getAllJust (foldMap (\ep -> AllJust $ (:[]) <$> (exprRecover ep)) eps))

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
                 | EPClip    Position ExprPattern LitPattern
                 | EPScale   Position ExprPattern ExprPattern
                 | EPDot     Position ExprPattern ExprPattern
                 deriving (Show, Eq, Generic, Data)

exprToPattern :: Expr -> ExprPattern
exprToPattern (EVar p v) = EPVar p $ atomToPattern v
exprToPattern (ELength p e) = EPLength p $ exprToPattern e
exprToPattern (ELit p lit) = EPLit p $ litToPattern lit
exprToPattern (EBinop p e1 op e2) = EPBinop p (exprToPattern e1) op (exprToPattern e2)
exprToPattern (EIndex p e1 e2) = EPIndex p (exprToPattern e1) (exprToPattern e2)
exprToPattern (ERAccess p e label) = EPRAccess p (exprToPattern e) label
exprToPattern (EFloat p e) = EPFloat p $ exprToPattern e
exprToPattern (EExp p e) = EPExp p $ exprToPattern e
exprToPattern (ELog p e) = EPLog p $ exprToPattern e
exprToPattern (EClip p e l) = EPClip p (exprToPattern e) (litToPattern l)
exprToPattern (EScale p e1 e2) = EPScale p (exprToPattern e1) (exprToPattern e2)
exprToPattern (EDot p e1 e2) = EPDot p (exprToPattern e1) (exprToPattern e2)

exprClosed :: ExprPattern -> Bool
exprClosed (EPWild _ _) = False
exprClosed (EPVar _ vp) = atomClosed vp
exprClosed (EPLength _ ep) = exprClosed ep
exprClosed (EPLit _ lp) = litClosed lp
exprClosed (EPBinop _ e1 _ e2) = exprClosed e1 && exprClosed e2
exprClosed (EPIndex _ e1 e2) = exprClosed e1 && exprClosed e2
exprClosed (EPRAccess _ e _) = exprClosed e
exprClosed (EPFloat _ e) = exprClosed e
exprClosed (EPExp _ e) = exprClosed e
exprClosed (EPLog _ e) = exprClosed e
exprClosed (EPClip _ e lit) = exprClosed e && litClosed lit
exprClosed (EPScale _ e1 e2) = exprClosed e1 && exprClosed e2
exprClosed (EPDot _ e1 e2) = exprClosed e1 && exprClosed e2

exprRecover :: ExprPattern -> Maybe Expr
exprRecover (EPWild _ _) = Nothing
exprRecover (EPVar p vp) = EVar p <$> atomRecover vp
exprRecover (EPLength p ep) = ELength p <$> exprRecover ep
exprRecover (EPLit p lp) = ELit p <$> litRecover lp
exprRecover (EPBinop p e1 op e2) =
  EBinop p <$> exprRecover e1 <*> pure op <*> exprRecover e2
exprRecover (EPIndex p e1 e2) =
  EIndex p <$> exprRecover e1 <*> exprRecover e2
exprRecover (EPRAccess p e label) =
  ERAccess p <$> exprRecover e <*> pure label
exprRecover (EPFloat p e) =
  EFloat p <$> exprRecover e
exprRecover (EPExp p e) =
  EExp p <$> exprRecover e
exprRecover (EPLog p e) =
  ELog p <$> exprRecover e
exprRecover (EPClip p e l) =
  EClip p <$> exprRecover e <*> litRecover l
exprRecover (EPScale p e1 e2) =
  EScale p <$> exprRecover e1 <*> exprRecover e2
exprRecover (EPDot p e1 e2) =
  EDot p <$> exprRecover e1 <*> exprRecover e2

isBinopPattern :: ExprPattern -> Bool
isBinopPattern (EPBinop _ _ _ _) = True
isBinopPattern _ = False

exprPosn :: Expr -> Position
exprPosn e = e ^. (typed @Position)

data Decl = Decl Position Var Float Tau
  deriving (Generic, Show, Eq, Data)

declPosn :: Decl -> Position
declPosn d = d ^. (typed @Position)

data Prog = Prog {
  getDecls :: [Decl]
  , getCmd :: Cmd
  } deriving (Show, Eq, Data)

data Param = PExpr Expr
           | PCmd  Cmd
  deriving (Show, Eq, Generic, Ord, Data)

data ParamPattern = PPExpr ExprPattern
                  | PPCmd  CmdPattern
  deriving (Show, Eq, Generic, Data)

paramToPattern :: Param -> ParamPattern
paramToPattern (PExpr e) = PPExpr $ exprToPattern e
paramToPattern (PCmd c) = PPCmd $ cmdToPattern c

paramClosed :: ParamPattern -> Bool
paramClosed (PPExpr e) = exprClosed e
paramClosed (PPCmd c) = cmdClosed c

paramRecover :: ParamPattern -> Maybe Param
paramRecover (PPExpr e) = PExpr <$> exprRecover e
paramRecover (PPCmd c) = PCmd <$> cmdRecover c

data Cmd = CAssign       Position Expr   Expr
         | CLaplace      Position Var    Float Expr
         | CIf           Position Expr   Cmd   Cmd
         | CWhile        Position Expr   Cmd
         | CSeq          Position Cmd    Cmd
         | CSkip         Position
         | CExt          Position String [Param]
         | CBlock        Position Cmd
  deriving (Generic, Show, Eq, Ord, Data)

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
normalizeSeq (CBlock p c) = CBlock p $ normalizeSeq c
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
                | CPExt     Position String      [ParamPattern]
                | CPBlock   Position CmdPattern
                deriving (Generic, Show, Eq, Data)

cmdToPattern :: Cmd -> CmdPattern
cmdToPattern (CAssign p e1 e2) =
  CPAssign p (exprToPattern e1) (exprToPattern e2)
cmdToPattern (CLaplace p v f e) =
  CPLaplace p (atomToPattern v) (atomToPattern f) (exprToPattern e)
cmdToPattern (CIf p e c1 c2) =
  CPIf p (exprToPattern e) (cmdToPattern c1) (cmdToPattern c2)
cmdToPattern (CWhile p e c) =
  CPWhile p (exprToPattern e) (cmdToPattern c)
cmdToPattern (CSeq p c1 c2) =
  CPSeq p (cmdToPattern c1) (cmdToPattern c2)
cmdToPattern (CSkip p) = CPSkip p
cmdToPattern (CExt p name params) =
  CPExt p name (fmap paramToPattern params)
cmdToPattern (CBlock p c) = CPBlock p $ cmdToPattern c

cmdClosed :: CmdPattern -> Bool
cmdClosed (CPWild _ _) = False
cmdClosed (CPAssign _ e1 e2) = exprClosed e1 && exprClosed e2
cmdClosed (CPLaplace _ vp fp ep) = atomClosed vp && atomClosed fp && exprClosed ep
cmdClosed (CPIf _ e c1 c2) = exprClosed e && cmdClosed c1 && cmdClosed c2
cmdClosed (CPWhile _ e c) = exprClosed e && cmdClosed c
cmdClosed (CPSeq _ c1 c2) = cmdClosed c1 && cmdClosed c2
cmdClosed (CPSkip _) = True
cmdClosed (CPExt _ _ params) = all paramClosed params
cmdClosed (CPBlock _ c) = cmdClosed c

cmdRecover :: CmdPattern -> Maybe Cmd
cmdRecover (CPWild _ _) = Nothing
cmdRecover (CPAssign p e1 e2) =
  CAssign p <$> exprRecover e1 <*> exprRecover e2
cmdRecover (CPLaplace p vp fp ep) =
  CLaplace p <$> atomRecover vp <*> atomRecover fp <*> exprRecover ep
cmdRecover (CPIf p e c1 c2) =
  CIf p <$> exprRecover e <*> cmdRecover c1 <*> cmdRecover c2
cmdRecover (CPWhile p e c) =
  CWhile p <$> exprRecover e <*> cmdRecover c
cmdRecover (CPSeq p c1 c2) =
  CSeq p <$> cmdRecover c1 <*> cmdRecover c2
cmdRecover (CPSkip p) = Just $ CSkip p
cmdRecover (CPExt p name params) =
  CExt p name <$> (getAllJust
                     (foldMap (\pp -> AllJust $ (:[]) <$> (paramRecover pp)) params))
cmdRecover (CPBlock p c) =
  CBlock p <$> cmdRecover c

normalizeSeqPattern :: CmdPattern -> CmdPattern
normalizeSeqPattern (CPSeq p (CPSeq p' c1 c2) c3) =
  normalizeSeqPattern $ CPSeq p c1 (CPSeq p' c2 c3)
normalizeSeqPattern (CPSeq p c1 c2) =
  CPSeq p (normalizeSeqPattern c1) (normalizeSeqPattern c2)
normalizeSeqPattern (CPIf p e c1 c2) =
  CPIf p e (normalizeSeqPattern c1) (normalizeSeqPattern c2)
normalizeSeqPattern (CPWhile p e c) =
  CPWhile p e $ normalizeSeqPattern c
normalizeSeqPattern (CPExt p name params) =
  (CPExt p name (fmap normalizeParamPattern params))
normalizeSeqPattern (CPBlock p c) = CPBlock p $ normalizeSeqPattern c
normalizeSeqPattern c = c

normalizeParamPattern :: ParamPattern -> ParamPattern
normalizeParamPattern (PPCmd c) = PPCmd $ normalizeSeqPattern c
normalizeParamPattern p = p

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
  [ "exp", "log", "lap", "length", "clip", "scale"
  , "dot", "true", "false", "if", "then", "else"
  , "end", "do", "while", "repeat", "skip", "fc"
  , "v", "iesc", "fesc", "besc", "e", "c"
  ]

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
      , (1, CBlock trivialPosition
            <$> genCmdSized (sz - 1))
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
shrinkCmd (CBlock _ c) =
  CBlock trivialPosition <$> shrinkCmd c

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
  in [EBinop trivialPosition lhs op rhs | lhs <- e1', rhs <- e2'] ++ [e1, e2]
shrinkExpr (EIndex _ e1 e2) =
  let e1' = shrinkExpr e1
      e2' = shrinkExpr e2
  in [EIndex trivialPosition lhs rhs | lhs <- e1', rhs <- e2'] ++ [e1, e2]
shrinkExpr (ERAccess _ e label) =
  let e' = shrinkExpr e
  in [ERAccess trivialPosition r label | r <- e'] ++ [e]
shrinkExpr (EFloat _ e) =
  let e' = shrinkExpr e
  in fmap (EFloat trivialPosition) e' ++ [e]
shrinkExpr (EExp _ e) =
  let e' = shrinkExpr e
  in fmap (EExp trivialPosition) e' ++ [e]
shrinkExpr (ELog _ e) =
  let e' = shrinkExpr e
  in fmap (ELog trivialPosition) e' ++ [e]
shrinkExpr (EClip _ e lit) =
  let e' = shrinkExpr e
  in [EClip trivialPosition e1 lit' | e1 <- e', lit' <- shrinkLit lit] ++ [e]
shrinkExpr (EScale _ e1 e2) =
  let e1' = shrinkExpr e1
      e2' = shrinkExpr e2
  in [EScale trivialPosition lhs rhs | lhs <- e1', rhs <- e2'] ++ [e1, e2]
shrinkExpr (EDot _ e1 e2) =
  let e1' = shrinkExpr e1
      e2' = shrinkExpr e2
  in [EDot trivialPosition lhs rhs | lhs <- e1', rhs <- e2'] ++ [e1, e2]

genIntPattern :: Gen IntPattern
genIntPattern = frequency [
  (1, AtomExact <$> arbitrary)
  , (1, AtomWild <$> genIdent)
  ]

shrinkIntPattern :: IntPattern -> [IntPattern]
shrinkIntPattern (AtomExact i) = AtomExact <$> shrink i
shrinkIntPattern x@(AtomWild _) = [x]

genFloatPattern :: Gen FloatPattern
genFloatPattern = frequency [
  (1, AtomExact <$> arbitrary)
  , (1, AtomWild <$> genIdent)
  ]

shrinkFloatPattern :: FloatPattern -> [FloatPattern]
shrinkFloatPattern (AtomExact i) = AtomExact <$> shrink i
shrinkFloatPattern x@(AtomWild _) = [x]

genBoolPattern :: Gen BoolPattern
genBoolPattern = frequency [
  (1, AtomExact <$> arbitrary)
  , (1, AtomWild <$> genIdent)
  ]

shrinkBoolPattern :: BoolPattern -> [BoolPattern]
shrinkBoolPattern (AtomExact i) = AtomExact <$> shrink i
shrinkBoolPattern x@(AtomWild _) = [x]

genVarPattern :: Gen VarPattern
genVarPattern = frequency [
  (1, AtomExact <$> genIdent)
  , (1, AtomWild <$> genIdent)
  ]

shrinkVarPattern :: VarPattern -> [VarPattern]
shrinkVarPattern x = [x]

genLitPatternSized :: Int -> Gen LitPattern
genLitPatternSized sz
  | sz <= 0 = frequency [
      (1, LPInt <$> genIntPattern)
      , (1, LPFloat <$> genFloatPattern)
      , (1, LPBool <$> genBoolPattern)
      ]
  | otherwise = frequency [
      (1, LPInt <$> genIntPattern)
      , (1, LPFloat <$> genFloatPattern)
      , (1, LPBool <$> genBoolPattern)
      , (1, LPArr <$> listOf (genExprPatternSized (sz - 1)))
      , (1, LPBag <$> listOf (genExprPatternSized (sz - 1)))
      ]

shrinkLitPattern :: LitPattern -> [LitPattern]
shrinkLitPattern (LPInt ip) = LPInt <$> shrinkIntPattern ip
shrinkLitPattern (LPFloat fp) = LPFloat <$> shrinkFloatPattern fp
shrinkLitPattern (LPBool bp) = LPBool <$> shrinkBoolPattern bp
shrinkLitPattern (LPArr es) = LPArr <$> shrink es
shrinkLitPattern (LPBag es) = LPBag <$> shrink es

genAssocBinopPatternSized :: Int -> Gen ExprPattern
genAssocBinopPatternSized sz =
  EPBinop trivialPosition
  <$> genExprPatternSized (sz - 1)
  <*> genAssocOp
  <*> genExprPatternSized (sz - 1)

genNonAssocBinopPatternSized :: Int -> Gen ExprPattern
genNonAssocBinopPatternSized sz = do
  lhs <- genExprPatternSized (sz - 1) `suchThat` (not . isBinopPattern)
  rhs <- genExprPatternSized (sz - 1) `suchThat` (not . isBinopPattern)
  op <- genNonAssocOp
  return $ EPBinop trivialPosition lhs op rhs

genExprPatternSized :: Int -> Gen ExprPattern
genExprPatternSized sz
  | sz <= 0 = frequency [
      (1, EPWild trivialPosition <$> genIdent)
      , (1, EPVar trivialPosition <$> genVarPattern)
      ]
  | otherwise = frequency [
      (1, EPWild trivialPosition
        <$> genIdent)
      , (1, EPVar trivialPosition
          <$> genVarPattern)
      , (1, EPLength trivialPosition
          <$> genExprPatternSized (sz - 1))
      , (1, EPLit trivialPosition
          <$> genLitPatternSized (sz - 1))
      , (1, genAssocBinopPatternSized (sz - 1))
      , (1, genNonAssocBinopPatternSized (sz - 1))
      , (1, EPIndex trivialPosition
          <$> genExprPatternSized (sz - 1) <*> genExprPatternSized (sz - 1))
      , (1, EPRAccess trivialPosition
          <$> genExprPatternSized (sz - 1) <*> genIdent)
      , (1, EPFloat trivialPosition
          <$> genExprPatternSized (sz - 1))
      , (1, EPExp trivialPosition
          <$> genExprPatternSized (sz - 1))
      , (1, EPLog trivialPosition
          <$> genExprPatternSized (sz - 1))
      , (1, EPClip trivialPosition
          <$> genExprPatternSized (sz - 1)
          <*> genLitPatternSized (sz - 1))
      , (1, EPScale trivialPosition
          <$> genExprPatternSized (sz - 1)
          <*> genExprPatternSized (sz - 1))
      , (1, EPDot trivialPosition
          <$> genExprPatternSized (sz - 1)
          <*> genExprPatternSized (sz - 1))
      ]

shrinkExprPattern :: ExprPattern -> [ExprPattern]
shrinkExprPattern (EPWild _ x) = [EPWild trivialPosition x]
shrinkExprPattern (EPVar _ vp) = EPVar trivialPosition <$> shrinkVarPattern vp
shrinkExprPattern (EPLength _ e) = EPLength trivialPosition <$> shrinkExprPattern e
shrinkExprPattern (EPLit _ lit) = EPLit trivialPosition <$> shrinkLitPattern lit
shrinkExprPattern (EPBinop _ lhs op rhs) =
  [lhs, rhs] ++ (EPBinop trivialPosition
                 <$> (shrinkExprPattern lhs)
                 <*> pure op
                 <*> (shrinkExprPattern rhs))
shrinkExprPattern (EPIndex _ e1 e2) =
  [e1, e2] ++ (EPIndex trivialPosition
               <$> shrinkExprPattern e1
               <*> shrinkExprPattern e2)
shrinkExprPattern (EPRAccess _ e label) =
  e : (EPRAccess trivialPosition
       <$> shrinkExprPattern e
       <*> pure label)
shrinkExprPattern (EPFloat _ e) =
  e : (EPFloat trivialPosition
       <$> shrinkExprPattern e)
shrinkExprPattern (EPExp _ e) =
  e : (EPExp trivialPosition
       <$> shrinkExprPattern e)
shrinkExprPattern (EPLog _ e) =
  e : (EPLog trivialPosition
       <$> shrinkExprPattern e)
shrinkExprPattern (EPClip _ e lit) =
  e : (EPClip trivialPosition
       <$> shrinkExprPattern e
       <*> shrink lit)
shrinkExprPattern (EPScale _ e1 e2) =
  [e1, e2] ++ (EPScale trivialPosition
               <$> shrinkExprPattern e1
               <*> shrinkExprPattern e2)
shrinkExprPattern (EPDot _ e1 e2) =
  [e1, e2] ++ (EPDot trivialPosition
               <$> shrinkExprPattern e1
               <*> shrinkExprPattern e2)

genParamPatternSized :: Int -> Gen ParamPattern
genParamPatternSized sz =
  frequency [
  (1, PPExpr <$> genExprPatternSized (sz - 1))
  , (1, PPCmd <$> genCmdPatternSized (sz - 1))
  ]

genParamPatternsSized :: Int -> Gen [ParamPattern]
genParamPatternsSized sz = listOf (genParamPatternSized sz)

genCmdPatternSized :: Int -> Gen CmdPattern
genCmdPatternSized sz
  | sz <= 0 = frequency [
      (1, CPWild trivialPosition <$> genIdent)
      ]
  | otherwise = frequency [
      (1, CPWild trivialPosition
        <$> genIdent)
      , (1, CPAssign trivialPosition
          <$> genExprPatternSized (sz - 1) <*> genExprPatternSized (sz - 1))
      , (1, CPLaplace trivialPosition
          <$> genVarPattern <*> genFloatPattern <*> genExprPatternSized (sz - 1))
      , (1, CPIf trivialPosition
          <$> genExprPatternSized (sz - 1)
          <*> genCmdPatternSized (sz - 1)
          <*> genCmdPatternSized (sz - 1))
      , (1, CPWhile trivialPosition
          <$> genExprPatternSized (sz - 1)
          <*> genCmdPatternSized (sz - 1))
      , (1, CPSeq trivialPosition
          <$> genCmdPatternSized (sz - 1)
          <*> genCmdPatternSized (sz - 1))
      , (1, pure $ CPSkip trivialPosition)
      , (1, CPExt trivialPosition <$> genIdent <*> genParamPatternsSized (sz - 1))
      , (1, CPBlock trivialPosition
            <$> genCmdPatternSized (sz - 1))
      ]

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

instance Arbitrary ParamPattern where
  arbitrary = sized genParamPatternSized

instance Arbitrary LitPattern where
  arbitrary = sized genLitPatternSized
  shrink = shrinkLitPattern

instance Arbitrary ExprPattern where
  arbitrary = sized genExprPatternSized
  shrink = shrinkExprPattern

instance Arbitrary CmdPattern where
  arbitrary = sized genCmdPatternSized

instance Arbitrary Tau where
  arbitrary = genericArbitraryRec (1 % 1 % 1 % 1 % 1 % 1 % 1 % ())
