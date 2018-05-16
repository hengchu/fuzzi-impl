module Syntax where

import Data.Map
import Prelude hiding (LT, EQ, GT)

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
          | ELength  Position Expr
          | ELit     Position Literal
          | EBinop   Position Expr Binop Expr
          | EIndex   Position String Expr
          | ERUpdate Position Expr String Expr
          | ERAccess Position Expr String
          | EClip    Position
                     Expr     -- ^Expression to be clipped
                     Literal  -- ^The bound
  deriving (Show, Eq)

lenVarName :: String -> String
lenVarName x = x ++ "_len"

desugarLenVar :: Position -> String -> Expr
desugarLenVar posn x = EVar posn . lenVarName $ x

exprPosn :: Expr -> Position
exprPosn (EVar     p _) = p
exprPosn (ELength  p _) = p
exprPosn (ELit     p _) = p
exprPosn (EBinop   p _ _ _) = p
exprPosn (EIndex   p _ _) = p
exprPosn (ERUpdate p _ _ _) = p
exprPosn (ERAccess p _ _) = p
exprPosn (EClip    p _ _) = p

data Cmd = CAssign  Position String Expr
         | CAUpdate Position String Expr  Expr
         | CLaplace Position String Float Expr
         | CIf      Position Expr   Cmd   Cmd
         | CWhile   Position Expr   Cmd
         | CDecl    Position String Float LargeType
         | CSeq     Position Cmd    Cmd
         | CSkip    Position
         -- |Bagmap: input, output, temp, iterator, out temp, map operation
         | CBMap    Position String String String String String Cmd
         -- |Arraymap: input, output, temp, iterator, out temp, map operation
         | CAMap    Position String String String String String Cmd
         -- |Bag sum.
         | CBSum    Position
                    String  -- ^input bag
                    String  -- ^output var
                    String  -- ^temp variable
                    String  -- ^index variable
                    Literal -- ^the bound
         | CPartition Position
                      String   -- ^input bag
                      String   -- ^output bag
                      String   -- ^temp variable
                      String   -- ^index variable
                      String   -- ^out partition index variable
                      Cmd      -- ^partition operation
  deriving (Show, Eq)

desugar :: Cmd -> Cmd
desugar c =
  case c of
    CBMap posn invar outvar tvar ivar outtemp cmd -> desugarBMap posn invar outvar tvar ivar outtemp cmd
    CAMap posn invar outvar tvar ivar outtemp cmd -> desugarAMap posn invar outvar tvar ivar outtemp cmd
    CBSum  posn invar outvar tvar ivar bound      -> desugarBSum posn invar outvar tvar ivar bound
    CWhile posn e cmd -> CWhile posn e (desugar cmd)
    CSeq posn c1 c2 -> CSeq posn (desugar c1) (desugar c2)
    CIf posn e ct cf -> CIf posn e (desugar ct) (desugar cf)
    _ -> c

intLit :: Position -> Int -> Expr
intLit posn = ELit posn . SLit . SILit

incrementVar :: Position -> String -> Cmd
incrementVar posn x =
  CAssign posn x (EBinop posn (EVar posn x) PLUS (intLit posn 1))

desugarBMap :: Position -> String -> String -> String -> String -> String -> Cmd -> Cmd
desugarBMap posn invar outvar tvar ivar outtvar mapCmd =
  (CAssign posn ivar (intLit posn 0)) `cseq`
  (CWhile posn loopCond loopBody) `cseq`
  (CAssign posn (lenVarName outvar) (ELength posn (EVar posn invar)))
  where cseq = CSeq posn
        loopCond = EBinop posn (EVar posn ivar) LT (ELength posn (EVar posn invar))
        loopBody = (CAssign posn tvar (EIndex posn invar (EVar posn ivar))) `cseq`
                   mapCmd `cseq`
                   (CAUpdate posn outvar (EVar posn ivar) (EVar posn outtvar)) `cseq`
                   (incrementVar posn ivar)

desugarAMap :: Position -> String -> String -> String -> String -> String -> Cmd -> Cmd
desugarAMap = desugarBMap

desugarBSum :: Position -> String -> String -> String -> String -> Literal -> Cmd
desugarBSum posn invar outvar tvar ivar bound =
  (CAssign posn ivar (intLit posn 0)) `cseq`
  (CWhile posn loopCond loopBody)
  where cseq = CSeq posn
        loopCond = EBinop posn (EVar posn ivar) LT (ELength posn (EVar posn invar))
        runningSum = EBinop posn (EVar posn outvar) PLUS (EClip posn (EVar posn tvar) bound)
        loopBody = (CAssign posn tvar (EIndex posn invar (EVar posn ivar))) `cseq`
                   (CAssign posn outvar runningSum) `cseq`
                   (incrementVar posn ivar)

-- TODO: fix this
desugarPartition :: Position -> String -> String -> String -> String -> String -> Cmd -> Cmd
desugarPartition posn invar outvar tvar ivar outindex partCmd =
  (CAssign posn ivar (intLit posn 0)) `cseq`
  (CWhile posn undefined undefined)
  where cseq = CSeq posn

foldExpr :: (Position -> String -> a)
         -> (Position -> a -> a)
         -> (Position -> Literal -> a)
         -> (Position -> a -> Binop -> a -> a)
         -> (Position -> String -> a -> a)
         -> (Position -> a -> String -> a -> a)
         -> (Position -> a -> String -> a)
         -> (Position -> a -> Literal -> a)
         -> Expr
         -> a
foldExpr fvar flength flit fbinop findex frupdate fraccess fclip e =
  case e of
    EVar posn str -> fvar posn str
    ELength posn expr -> flength posn (recur expr)
    ELit posn lit -> flit posn lit
    EBinop posn el op er -> fbinop posn (recur el) op (recur er)
    EIndex posn varr eidx -> findex posn varr (recur eidx)
    ERUpdate posn erow label evalue -> frupdate posn (recur erow) label (recur evalue)
    ERAccess posn erow label -> fraccess posn (recur erow) label
    EClip posn ev bound -> fclip posn (recur ev) bound
  where recur = foldExpr fvar flength flit fbinop findex frupdate fraccess fclip

foldExprA :: (Applicative f)
          => (Position -> String -> f a)
          -> (Position -> f a -> f a)
          -> (Position -> Literal -> f a)
          -> (Position -> f a -> Binop -> f a -> f a)
          -> (Position -> String -> f a -> f a)
          -> (Position -> f a -> String -> f a -> f a)
          -> (Position -> f a -> String -> f a)
          -> (Position -> f a -> Literal -> f a)
          -> Expr
          -> f a
foldExprA fvar flength flit fbinop findex frupdate fraccess fclip e =
  case e of
    EVar posn str -> fvar posn str
    ELength posn expr -> flength posn (recur expr)
    ELit posn lit -> flit posn lit
    EBinop posn el op er ->
      fbinop posn (recur el) op (recur er)
    EIndex posn varr eidx ->
      findex posn varr (recur eidx)
    ERUpdate posn erow label evalue ->
      frupdate posn (recur erow) label (recur evalue)
    ERAccess posn erow label ->
      fraccess posn (recur erow) label
    EClip posn ev bound ->
      fclip posn (recur ev) bound
  where
    recur = foldExprA fvar flength flit fbinop findex frupdate fraccess fclip

foldExprM :: (Monad m)
          => (Position -> String -> m a)
          -> (Position -> a -> m a)
          -> (Position -> Literal -> m a)
          -> (Position -> a -> Binop -> a -> m a)
          -> (Position -> String -> a -> m a)
          -> (Position -> a -> String -> a -> m a)
          -> (Position -> a -> String -> m a)
          -> (Position -> a -> Literal -> m a)
          -> Expr
          -> m a
foldExprM fvar flength flit fbinop findex frupdate fraccess fclip e =
  case e of
    EVar posn str -> fvar posn str
    ELength posn expr -> recur expr >>= flength posn
    ELit posn lit -> flit posn lit
    EBinop posn el op er -> do
      al <- recur el
      ar <- recur er
      fbinop posn al op ar
    EIndex posn varr eidx -> do
      aidx <- recur eidx
      findex posn varr aidx
    ERUpdate posn erow label evalue -> do
      arow <- recur erow
      avalue <- recur evalue
      frupdate posn arow label avalue
    ERAccess posn erow label -> do
      arow <- recur erow
      fraccess posn arow label
    EClip posn ev bound -> do
      acc <- recur ev
      fclip posn acc bound
  where recur = foldExprM fvar flength flit fbinop findex frupdate fraccess fclip

foldCmd :: (Position -> String -> Expr -> a)
        -> (Position -> String -> Expr -> Expr -> a)
        -> (Position -> String -> Float -> Expr -> a)
        -> (Position -> Expr -> a -> a -> a)
        -> (Position -> Expr -> a -> a)
        -> (Position -> String -> Float -> LargeType -> a)
        -> (Position -> a -> a -> a)
        -> (Position -> a)
        -> (Position -> String -> String -> String -> String -> String -> a -> a)
        -> (Position -> String -> String -> String -> String -> String -> a -> a)
        -> (Position -> String -> String -> String -> String -> Literal -> a)
        -> (Position -> String -> String -> String -> String -> String -> a -> a)
        -> Cmd
        -> a
foldCmd fassign faupdate flaplace fif fwhile fdecl fseq fskip fbmap famap fbsum fpart c =
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
    CBMap posn invar outvar tvar ivar outtemp mapCmd ->
      fbmap posn invar outvar tvar ivar outtemp (recur mapCmd)
    CAMap posn invar outvar tvar ivar outtemp mapCmd ->
      famap posn invar outvar tvar ivar outtemp (recur mapCmd)
    CBSum posn invar outvar tvar ivar bound ->
      fbsum posn invar outvar tvar ivar bound
    CPartition posn invar outvar tvar ivar outindex partCmd ->
      fpart posn invar outvar tvar ivar outindex (recur partCmd)
  where recur = foldCmd fassign faupdate flaplace fif fwhile fdecl fseq fskip fbmap famap fbsum fpart

foldCmdA :: (Applicative f)
         => (Position -> String -> Expr -> f a)
         -> (Position -> String -> Expr -> Expr -> f a)
         -> (Position -> String -> Float -> Expr -> f a)
         -> (Position -> Expr -> f a -> f a -> f a)
         -> (Position -> Expr -> f a -> f a)
         -> (Position -> String -> Float -> LargeType -> f a)
         -> (Position -> f a -> f a -> f a)
         -> (Position -> f a)
         -> (Position -> String -> String -> String -> String -> String -> f a -> f a)
         -> (Position -> String -> String -> String -> String -> String -> f a -> f a)
         -> (Position -> String -> String -> String -> String -> Literal -> f a)
         -> (Position -> String -> String -> String -> String -> String -> f a -> f a)
         -> Cmd
         -> f a
foldCmdA fassign faupdate flaplace fif fwhile fdecl fseq fskip fbmap famap fbsum fpart c =
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
    CBMap posn invar outvar tvar ivar outtemp mapCmd ->
      fbmap posn invar outvar tvar ivar outtemp (recur mapCmd)
    CAMap posn invar outvar tvar ivar outtemp mapCmd ->
      famap posn invar outvar tvar ivar outtemp (recur mapCmd)
    CBSum posn invar outvar tvar ivar bound ->
      fbsum posn invar outvar tvar ivar bound
    CPartition posn invar outvar tvar ivar outindex partCmd ->
      fpart posn invar outvar tvar ivar outindex (recur partCmd)
  where recur =
          foldCmdA fassign faupdate flaplace fif fwhile fdecl fseq fskip fbmap famap fbsum fpart

foldCmdM :: (Monad m)
         => (Position -> String -> Expr -> m a)
         -> (Position -> String -> Expr -> Expr -> m a)
         -> (Position -> String -> Float -> Expr -> m a)
         -> (Position -> Expr -> a -> a -> m a)
         -> (Position -> Expr -> a -> m a)
         -> (Position -> String -> Float -> LargeType -> m a)
         -> (Position -> a -> a -> m a)
         -> (Position -> m a)
         -> (Position -> String -> String -> String -> String -> String -> a -> m a)
         -> (Position -> String -> String -> String -> String -> String -> a -> m a)
         -> (Position -> String -> String -> String -> String -> Literal -> m a)
         -> (Position -> String -> String -> String -> String -> String -> a -> m a)
         -> Cmd
         -> m a
foldCmdM fassign faupdate flaplace fif fwhile fdecl fseq fskip fbmap famap fbsum fpart c =
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
    CBMap posn invar outvar tvar ivar outtemp mapCmd -> do
      a <- recur mapCmd
      fbmap posn invar outvar tvar ivar outtemp a
    CAMap posn invar outvar tvar ivar outtemp mapCmd -> do
      a <- recur mapCmd
      famap posn invar outvar tvar ivar outtemp a
    CBSum posn invar outvar tvar ivar bound ->
      fbsum posn invar outvar tvar ivar bound
    CPartition posn invar outvar tvar ivar outindex partCmd ->
      recur partCmd >>= fpart posn invar outvar tvar ivar outindex
  where recur =
          foldCmdM fassign faupdate flaplace fif fwhile fdecl fseq fskip    fbmap famap fbsum fpart
        recur1 newfskip =
          foldCmdM fassign faupdate flaplace fif fwhile fdecl fseq newfskip fbmap famap fbsum fpart
