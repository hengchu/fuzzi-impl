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

type Line   = Int
type Column = Int

data Position = Position Line Column
  deriving (Show, Eq)

data Expr = EVar     Position String
          | ELit     Position Literal
          | EBinop   Position Expr Binop Expr
          | EIndex   Position Expr Expr
          | ERUpdate Position Expr String Expr
          | ERAccess Position Expr String
  deriving (Show, Eq)

exprPosn :: Expr -> Position
exprPosn (EVar     p _) = p
exprPosn (ELit     p _) = p
exprPosn (EBinop   p _ _ _) = p
exprPosn (EIndex   p _ _) = p
exprPosn (ERUpdate p _ _ _) = p
exprPosn (ERAccess p _ _) = p

data Cmd = CAssign  Position String Expr
         | CAUpdate Position String Expr  Expr
         | CLaplace Position String Float Expr
         | CIf      Position Expr   Cmd   Cmd
         | CWhile   Position Expr   Cmd
         | CDecl    Position String Float LargeType
         | CSeq     Position Cmd    Cmd
         | CSkip    Position
  deriving (Show, Eq)

foldExpr :: (Position -> String -> a)
         -> (Position -> Literal -> a)
         -> (Position -> a -> Binop -> a -> a)
         -> (Position -> a -> a -> a)
         -> (Position -> a -> String -> a -> a)
         -> (Position -> a -> String -> a)
         -> Expr
         -> a
foldExpr fvar flit fbinop findex frupdate fraccess e =
  case e of
    EVar posn str -> fvar posn str
    ELit posn lit -> flit posn lit
    EBinop posn el op er -> fbinop posn (recur el) op (recur er)
    EIndex posn earr eidx -> findex posn (recur earr) (recur eidx)
    ERUpdate posn erow label evalue -> frupdate posn (recur erow) label (recur evalue)
    ERAccess posn erow label -> fraccess posn (recur erow) label
  where recur = foldExpr fvar flit fbinop findex frupdate fraccess

foldExprA :: (Applicative f)
          => (Position -> String -> f a)
          -> (Position -> Literal -> f a)
          -> (Position -> f a -> Binop -> f a -> f a)
          -> (Position -> f a -> f a -> f a)
          -> (Position -> f a -> String -> f a -> f a)
          -> (Position -> f a -> String -> f a)
          -> Expr
          -> f a
foldExprA fvar flit fbinop findex frupdate fraccess e =
  case e of
    EVar posn str -> fvar posn str
    ELit posn lit -> flit posn lit
    EBinop posn el op er ->
      fbinop posn (recur el) op (recur er)
    EIndex posn earr eidx ->
      findex posn (recur earr) (recur eidx)
    ERUpdate posn erow label evalue ->
      frupdate posn (recur erow) label (recur evalue)
    ERAccess posn erow label ->
      fraccess posn (recur erow) label
  where
    recur = foldExprA fvar flit fbinop findex frupdate fraccess

foldExprM :: (Monad m)
          => (Position -> String -> m a)
          -> (Position -> Literal -> m a)
          -> (Position -> a -> Binop -> a -> m a)
          -> (Position -> a -> a -> m a)
          -> (Position -> a -> String -> a -> m a)
          -> (Position -> a -> String -> m a)
          -> Expr
          -> m a
foldExprM fvar flit fbinop findex frupdate fraccess e =
  case e of
    EVar posn str -> fvar posn str
    ELit posn lit -> flit posn lit
    EBinop posn el op er -> do
      al <- recur el
      ar <- recur er
      fbinop posn al op ar
    EIndex posn earr eidx -> do
      aarr <- recur earr
      aidx <- recur eidx
      findex posn aarr aidx
    ERUpdate posn erow label evalue -> do
      arow <- recur erow
      avalue <- recur evalue
      frupdate posn arow label avalue
    ERAccess posn erow label -> do
      arow <- recur erow
      fraccess posn arow label
  where recur = foldExprM fvar flit fbinop findex frupdate fraccess

foldCmdA :: (Applicative f)
         => (Position -> String -> Expr -> f a)
         -> (Position -> String -> Expr -> Expr -> f a)
         -> (Position -> String -> Float -> Expr -> f a)
         -> (Position -> Expr -> f a -> f a -> f a)
         -> (Position -> Expr -> f a -> f a)
         -> (Position -> String -> Float -> LargeType -> f a)
         -> (Position -> f a -> f a -> f a)
         -> (Position -> f a)
         -> Cmd
         -> f a
foldCmdA fassign faupdate flaplace fif fwhile fdecl fseq fskip c =
  case c of
    CAssign  posn x e -> fassign posn x e
    CAUpdate posn x eidx eval -> faupdate posn x eidx eval
    CLaplace posn x width mean -> flaplace posn x width mean
    CIf posn e ct cf ->
      fif posn e (recur ct) (recur cf)
    CWhile posn e cbody ->
      fwhile posn e (recur cbody)
    CDecl posn x s t -> fdecl posn x s t
    CSeq posn c1 c2 ->
      fseq posn (recur c1) (recur c2)
    CSkip posn -> fskip posn
  where recur = foldCmdA fassign faupdate flaplace fif fwhile fdecl fseq fskip

foldCmdM :: (Monad m)
         => (Position -> String -> Expr -> m a)
         -> (Position -> String -> Expr -> Expr -> m a)
         -> (Position -> String -> Float -> Expr -> m a)
         -> (Position -> Expr -> a -> a -> m a)
         -> (Position -> Expr -> a -> m a)
         -> (Position -> String -> Float -> LargeType -> m a)
         -> (Position -> a -> a -> m a)
         -> (Position -> m a)
         -> Cmd
         -> m a
foldCmdM fassign faupdate flaplace fif fwhile fdecl fseq fskip c =
  case c of
    CAssign  posn x e -> fassign posn x e
    CAUpdate posn x eidx eval -> faupdate posn x eidx eval
    CLaplace posn x width mean -> flaplace posn x width mean
    CIf posn e ct cf -> do
      at <- recur ct
      af <- recur cf
      fif posn e at af
    CWhile posn e cbody -> do
      a <- recur cbody
      fwhile posn e a
    CDecl posn x s t -> fdecl posn x s t
    CSeq posn c1 c2 -> do
      a1 <- recur c1
      a2 <- recur1 (const . pure $ a1) c2
      fseq posn a1 a2
    CSkip posn -> fskip posn
  where recur  = foldCmdM fassign faupdate flaplace fif fwhile fdecl fseq fskip
        recur1 = foldCmdM fassign faupdate flaplace fif fwhile fdecl fseq
