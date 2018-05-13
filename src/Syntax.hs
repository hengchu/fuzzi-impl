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

foldExpr :: (String -> a)
         -> (Literal -> a)
         -> (a -> Binop -> a -> a)
         -> (a -> a -> a)
         -> (a -> String -> a -> a)
         -> (a -> String -> a)
         -> Expr
         -> a
foldExpr fvar flit fbinop findex frupdate fraccess e =
  case e of
    EVar str -> fvar str
    ELit lit -> flit lit
    EBinop el op er -> fbinop (recur el) op (recur er)
    EIndex earr eidx -> findex (recur earr) (recur eidx)
    ERUpdate erow label evalue -> frupdate (recur erow) label (recur evalue)
    ERAccess erow label -> fraccess (recur erow) label
  where recur = foldExpr fvar flit fbinop findex frupdate fraccess

foldExprA :: (Applicative f)
          => (String -> f a)
          -> (Literal -> f a)
          -> (f a -> Binop -> f a -> f a)
          -> (f a -> f a -> f a)
          -> (f a -> String -> f a -> f a)
          -> (f a -> String -> f a)
          -> Expr
          -> f a
foldExprA fvar flit fbinop findex frupdate fraccess e =
  case e of
    EVar str -> fvar str
    ELit lit -> flit lit
    EBinop el op er ->
      fbinop (recur el) op (recur er)
    EIndex earr eidx ->
      findex (recur earr) (recur eidx)
    ERUpdate erow label evalue ->
      frupdate (recur erow) label (recur evalue)
    ERAccess erow label ->
      fraccess (recur erow) label
  where
    recur = foldExprA fvar flit fbinop findex frupdate fraccess

foldExprM :: (Monad m)
          => (String -> m a)
          -> (Literal -> m a)
          -> (a -> Binop -> a -> m a)
          -> (a -> a -> m a)
          -> (a -> String -> a -> m a)
          -> (a -> String -> m a)
          -> Expr
          -> m a
foldExprM fvar flit fbinop findex frupdate fraccess e =
  case e of
    EVar str -> fvar str
    ELit lit -> flit lit
    EBinop el op er -> do
      al <- recur el
      ar <- recur er
      fbinop al op ar
    EIndex earr eidx -> do
      aarr <- recur earr
      aidx <- recur eidx
      findex aarr aidx
    ERUpdate erow label evalue -> do
      arow <- recur erow
      avalue <- recur evalue
      frupdate arow label avalue
    ERAccess erow label -> do
      arow <- recur erow
      fraccess arow label
  where recur = foldExprM fvar flit fbinop findex frupdate fraccess

foldCmdA :: (Applicative f)
         => (String -> Expr -> f a)
         -> (String -> Expr -> Expr -> f a)
         -> (String -> Float -> Expr -> f a)
         -> (Expr -> f a -> f a -> f a)
         -> (Expr -> f a -> f a)
         -> (String -> Float -> LargeType -> f a)
         -> (f a -> f a -> f a)
         -> a
         -> Cmd
         -> f a
foldCmdA fassign faupdate flaplace fif fwhile fdecl fseq aid c =
  case c of
    CAssign x e -> fassign x e
    CAUpdate x eidx eval -> faupdate x eidx eval
    CLaplace x width mean -> flaplace x width mean
    CIf e ct cf ->
      fif e (recur ct) (recur cf)
    CWhile e cbody ->
      fwhile e (recur cbody)
    CDecl x s t -> fdecl x s t
    CSeq c1 c2 ->
      fseq (recur c1) (recur c2)
    CSkip -> pure aid
  where recur = foldCmdA fassign faupdate flaplace fif fwhile fdecl fseq aid

foldCmdM :: (Monad m)
         => (String -> Expr -> m a)
         -> (String -> Expr -> Expr -> m a)
         -> (String -> Float -> Expr -> m a)
         -> (Expr -> a -> a -> m a)
         -> (Expr -> a -> m a)
         -> (String -> Float -> LargeType -> m a)
         -> (a -> a -> m a)
         -> a
         -> Cmd
         -> m a
foldCmdM fassign faupdate flaplace fif fwhile fdecl fseq aid c =
  case c of
    CAssign x e -> fassign x e
    CAUpdate x eidx eval -> faupdate x eidx eval
    CLaplace x width mean -> flaplace x width mean
    CIf e ct cf -> do
      at <- recur ct
      af <- recur cf
      fif e at af
    CWhile e cbody -> do
      a <- recur cbody
      fwhile e a
    CDecl x s t -> fdecl x s t
    CSeq c1 c2 -> do
      a1 <- recur c1
      a2 <- recur1 a1 c2
      fseq a1 a2
    CSkip -> return aid
  where recur  = foldCmdM fassign faupdate flaplace fif fwhile fdecl fseq aid
        recur1 = foldCmdM fassign faupdate flaplace fif fwhile fdecl fseq
