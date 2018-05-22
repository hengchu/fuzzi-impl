module Syntax where

import Data.Map
import Prelude hiding (LT, EQ, GT)

data SmallType = STInt | STFloat | STBool | STAny
  deriving (Show, Eq)

newtype RowType = RowType { getRowTypes :: Map String SmallType }
  deriving (Show, Eq)

data LargeType = LTSmall SmallType
               | LTRow   RowType
               | LTArray SmallType
               | LTBag   LargeType
               | LTAny
  deriving (Show, Eq)

data Binop = LT | LE | GT | GE | AND | OR | EQ | NEQ | PLUS | MINUS | MULT | DIV
  deriving (Show, Eq, Ord)

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
          | EIndex   Position Expr Expr
          | ERUpdate Position Expr String Expr
          | ERAccess Position Expr String
          | EArray   Position [Expr]
          | EBag     Position [Expr]
          | EClip    Position
                     Expr     -- ^Expression to be clipped
                     Literal  -- ^The bound
  deriving (Show, Eq)

exprPosn :: Expr -> Position
exprPosn (EVar     p _) = p
exprPosn (ELength  p _) = p
exprPosn (ELit     p _) = p
exprPosn (EBinop   p _ _ _) = p
exprPosn (EIndex   p _ _) = p
exprPosn (ERUpdate p _ _ _) = p
exprPosn (ERAccess p _ _) = p
exprPosn (EArray   p _) = p
exprPosn (EBag     p _) = p
exprPosn (EClip    p _ _) = p

data Cmd = CAssign  Position Expr Expr
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
                      Int      -- ^number of partitions to create
                      Cmd      -- ^partition operation
  deriving (Show, Eq)

cmdPosn :: Cmd -> Position
cmdPosn (CAssign    p _ _) = p
cmdPosn (CLaplace   p _ _ _) = p
cmdPosn (CIf        p _ _ _) = p
cmdPosn (CWhile     p _ _) = p
cmdPosn (CDecl      p _ _ _) = p
cmdPosn (CSeq       p _ _) = p
cmdPosn (CSkip      p) = p
cmdPosn (CBMap      p _ _ _ _ _ _) = p
cmdPosn (CAMap      p _ _ _ _ _ _) = p
cmdPosn (CBSum      p _ _ _ _ _) = p
cmdPosn (CPartition p _ _ _ _ _ _ _) = p

-- | Traverses to the left most EIndex where the term being indexed is a
-- variable. This is a partial function.
indexedVar :: Expr -> String
indexedVar (EVar _ x)     = x
indexedVar (EIndex _ e _) = indexedVar e
indexedVar e = error $ show e ++ " is not a valid index expression"

-- | Takes a LHS expression and returns the modified variable
modifiedVar :: Expr -> String
modifiedVar (ELength _ e) = indexedVar e
modifiedVar e = indexedVar e

desugar :: Cmd -> Cmd
desugar c =
  case c of
    CBMap posn invar outvar tvar ivar outtemp cmd
      -> desugarBMap posn invar outvar tvar ivar outtemp cmd
    CAMap posn invar outvar tvar ivar outtemp cmd
      -> desugarAMap posn invar outvar tvar ivar outtemp cmd
    CBSum  posn invar outvar tvar ivar bound
      -> desugarBSum posn invar outvar tvar ivar bound
    CPartition posn invar outvar tvar ivar outindex npart cmd
      -> desugarPartition posn invar outvar tvar ivar outindex npart cmd
    CWhile posn e cmd -> CWhile posn e (desugar cmd)
    CSeq posn c1 c2 -> CSeq posn (desugar c1) (desugar c2)
    CIf posn e ct cf -> CIf posn e (desugar ct) (desugar cf)
    _ -> c

intLit :: Position -> Int -> Expr
intLit posn = ELit posn . SLit . SILit

incrementVar :: Position -> Expr -> Cmd
incrementVar posn x =
  CAssign posn x (EBinop posn x PLUS (intLit posn 1))

desugarBMap :: Position -> String -> String -> String -> String -> String -> Cmd -> Cmd
desugarBMap posn invar outvar tvar ivar outtvar mapCmd =
  (CAssign posn (evar ivar) (intLit posn 0)) `cseq`
  (CWhile posn loopCond loopBody) `cseq`
  (CAssign posn (ELength posn (evar outvar)) (ELength posn (evar invar)))
  where cseq = CSeq posn
        evar = EVar posn
        eindex = EIndex posn
        loopCond = EBinop posn (evar ivar) LT (ELength posn (evar invar))
        loopBody = (CAssign posn (evar tvar) (EIndex posn (evar invar) (evar ivar))) `cseq`
                   mapCmd `cseq`
                   (CAssign posn (eindex (evar outvar) (evar ivar)) (evar outtvar)) `cseq`
                   (incrementVar posn (evar ivar))

desugarAMap :: Position -> String -> String -> String -> String -> String -> Cmd -> Cmd
desugarAMap = desugarBMap

desugarBSum :: Position -> String -> String -> String -> String -> Literal -> Cmd
desugarBSum posn invar outvar tvar ivar bound =
  (CAssign posn (evar ivar) (intLit posn 0)) `cseq`
  (CWhile posn loopCond loopBody)
  where cseq = CSeq posn
        evar = EVar posn
        eindex = EIndex posn
        loopCond = EBinop posn (evar ivar) LT (ELength posn (evar invar))
        runningSum = EBinop posn (evar outvar) PLUS (EClip posn (evar tvar) bound)
        loopBody = (CAssign posn (evar tvar) (eindex (evar invar) (evar ivar))) `cseq`
                   (CAssign posn (evar outvar) runningSum) `cseq`
                   (incrementVar posn (evar ivar))

desugarPartition :: Position
                 -> String
                 -> String
                 -> String
                 -> String
                 -> String
                 -> Int
                 -> Cmd
                 -> Cmd
desugarPartition posn invar outvar tvar ivar outindex npart partCmd =
  (CAssign posn (evar ivar) (intLit posn 0)) `cseq`
  initializePartitions `cseq`
  (CAssign posn (evar ivar) (intLit posn 0)) `cseq`
  (CWhile posn loopCond loopBody)
  where cseq = CSeq posn
        evar = EVar posn
        eindex = EIndex posn
        ebop = EBinop posn
        elen = ELength posn

        outExpr  = evar outvar

        initCond = ebop (evar ivar) LT (intLit posn npart)
        initBody = (CAssign posn
                            (elen outExpr)
                            (ebop (elen outExpr) PLUS (intLit posn 1))) `cseq`
                   (CAssign posn
                            (eindex outExpr (evar ivar))
                            (EBag posn [])) `cseq`
                   (incrementVar posn (evar ivar))

        initializePartitions = CWhile posn initCond initBody

        loopCond = ebop
                     (evar ivar)
                     LT
                     (elen (evar invar))
        validPartExpr = ebop (evar outindex) LT (intLit posn npart)
        loopBody = (CAssign posn
                            (evar tvar)
                            (eindex (evar invar) (evar ivar))) `cseq`
                   partCmd `cseq`
                   CIf posn validPartExpr assignToPart (CSkip posn) `cseq`
                   (incrementVar posn (evar ivar))
        partExpr = eindex (evar outvar) (evar outindex)
        partLenExpr = elen partExpr
        assignToPart =
          CAssign posn
                  (elen partExpr)
                  (ebop partLenExpr PLUS (intLit posn 1))
          `cseq`
          CAssign posn
                  (eindex partExpr (ebop partLenExpr MINUS (intLit posn 1)))
                  (evar tvar)

foldExpr :: (Position -> String -> a)
         -> (Position -> a -> a)
         -> (Position -> Literal -> a)
         -> (Position -> a -> Binop -> a -> a)
         -> (Position -> a -> a -> a)
         -> (Position -> a -> String -> a -> a)
         -> (Position -> a -> String -> a)
         -> (Position -> [a] -> a)
         -> (Position -> [a] -> a)
         -> (Position -> a -> Literal -> a)
         -> Expr
         -> a
foldExpr fvar flength flit fbinop findex frupdate fraccess farray fbag fclip e =
  case e of
    EVar posn str -> fvar posn str
    ELength posn expr -> flength posn (recur expr)
    ELit posn lit -> flit posn lit
    EBinop posn el op er -> fbinop posn (recur el) op (recur er)
    EIndex posn earr eidx -> findex posn (recur earr) (recur eidx)
    ERUpdate posn erow label evalue -> frupdate posn (recur erow) label (recur evalue)
    ERAccess posn erow label -> fraccess posn (recur erow) label
    EArray posn exprs -> farray posn (fmap recur exprs)
    EBag posn exprs -> fbag posn (fmap recur exprs)
    EClip posn ev bound -> fclip posn (recur ev) bound
  where recur = foldExpr fvar flength flit fbinop findex frupdate fraccess farray fbag fclip

foldExprA :: (Applicative f)
          => (Position -> String -> f a)
          -> (Position -> f a -> f a)
          -> (Position -> Literal -> f a)
          -> (Position -> f a -> Binop -> f a -> f a)
          -> (Position -> f a -> f a -> f a)
          -> (Position -> f a -> String -> f a -> f a)
          -> (Position -> f a -> String -> f a)
          -> (Position -> f [a] -> f a)
          -> (Position -> f [a] -> f a)
          -> (Position -> f a -> Literal -> f a)
          -> Expr
          -> f a
foldExprA fvar flength flit fbinop findex frupdate fraccess farray fbag fclip e =
  case e of
    EVar posn str -> fvar posn str
    ELength posn expr -> flength posn (recur expr)
    ELit posn lit -> flit posn lit
    EBinop posn el op er ->
      fbinop posn (recur el) op (recur er)
    EIndex posn earr eidx ->
      findex posn (recur earr) (recur eidx)
    ERUpdate posn erow label evalue ->
      frupdate posn (recur erow) label (recur evalue)
    ERAccess posn erow label ->
      fraccess posn (recur erow) label
    EArray posn exprs ->
      farray posn (sequenceA $ fmap recur exprs)
    EBag posn exprs ->
      fbag posn (sequenceA $ fmap recur exprs)
    EClip posn ev bound ->
      fclip posn (recur ev) bound
  where
    recur = foldExprA fvar flength flit fbinop findex frupdate fraccess farray fbag fclip

foldExprM :: (Monad m)
          => (Position -> String -> m a)
          -> (Position -> a -> m a)
          -> (Position -> Literal -> m a)
          -> (Position -> a -> Binop -> a -> m a)
          -> (Position -> a -> a -> m a)
          -> (Position -> a -> String -> a -> m a)
          -> (Position -> a -> String -> m a)
          -> (Position -> [a] -> m a)
          -> (Position -> [a] -> m a)
          -> (Position -> a -> Literal -> m a)
          -> Expr
          -> m a
foldExprM fvar flength flit fbinop findex frupdate fraccess farray fbag fclip e =
  case e of
    EVar posn str -> fvar posn str
    ELength posn expr -> recur expr >>= flength posn
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
    EArray posn exprs -> do
      as <- sequence $ fmap recur exprs
      farray posn as
    EBag posn exprs -> do
      as <- sequence $ fmap recur exprs
      fbag posn as
    EClip posn ev bound -> do
      acc <- recur ev
      fclip posn acc bound
  where recur = foldExprM fvar flength flit fbinop findex
                          frupdate fraccess farray fbag fclip

foldCmd :: (Position -> Expr -> Expr -> a)
        -> (Position -> String -> Float -> Expr -> a)
        -> (Position -> Expr -> a -> a -> a)
        -> (Position -> Expr -> a -> a)
        -> (Position -> String -> Float -> LargeType -> a)
        -> (Position -> a -> a -> a)
        -> (Position -> a)
        -> (Position -> String -> String -> String -> String -> String -> a -> a)
        -> (Position -> String -> String -> String -> String -> String -> a -> a)
        -> (Position -> String -> String -> String -> String -> Literal -> a)
        -> (Position -> String -> String -> String -> String -> String -> Int -> a -> a)
        -> Cmd
        -> a
foldCmd fassign flaplace fif fwhile fdecl fseq fskip fbmap famap fbsum fpart c =
  case c of
    CAssign posn x e -> fassign posn x e
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
    CPartition posn invar outvar tvar ivar outindex npart partCmd ->
      fpart posn invar outvar tvar ivar outindex npart (recur partCmd)
  where recur = foldCmd fassign flaplace fif fwhile fdecl fseq fskip fbmap famap fbsum fpart

foldCmdA :: (Applicative f)
         => (Position -> Expr -> Expr -> f a)
         -> (Position -> String -> Float -> Expr -> f a)
         -> (Position -> Expr -> f a -> f a -> f a)
         -> (Position -> Expr -> f a -> f a)
         -> (Position -> String -> Float -> LargeType -> f a)
         -> (Position -> f a -> f a -> f a)
         -> (Position -> f a)
         -> (Position -> String -> String -> String -> String -> String -> f a -> f a)
         -> (Position -> String -> String -> String -> String -> String -> f a -> f a)
         -> (Position -> String -> String -> String -> String -> Literal -> f a)
         -> (Position -> String -> String -> String -> String -> String -> Int -> f a -> f a)
         -> Cmd
         -> f a
foldCmdA fassign flaplace fif fwhile fdecl fseq fskip fbmap famap fbsum fpart c =
  case c of
    CAssign  posn x e -> fassign posn x e
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
    CPartition posn invar outvar tvar ivar outindex npart partCmd ->
      fpart posn invar outvar tvar ivar outindex npart (recur partCmd)
  where recur =
          foldCmdA fassign flaplace fif fwhile fdecl fseq fskip fbmap famap fbsum fpart

foldCmdM :: (Monad m)
         => (Position -> Expr -> Expr -> m a)
         -> (Position -> String -> Float -> Expr -> m a)
         -> (Position -> Expr -> a -> a -> m a)
         -> (Position -> Expr -> a -> m a)
         -> (Position -> String -> Float -> LargeType -> m a)
         -> (Position -> a -> a -> m a)
         -> (Position -> m a)
         -> (Position -> String -> String -> String -> String -> String -> a -> m a)
         -> (Position -> String -> String -> String -> String -> String -> a -> m a)
         -> (Position -> String -> String -> String -> String -> Literal -> m a)
         -> (Position -> String -> String -> String -> String -> String -> Int -> a -> m a)
         -> Cmd
         -> m a
foldCmdM fassign flaplace fif fwhile fdecl fseq fskip fbmap famap fbsum fpart c =
  case c of
    CAssign  posn x e -> fassign posn x e
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
    CPartition posn invar outvar tvar ivar outindex npart partCmd ->
      recur partCmd >>= fpart posn invar outvar tvar ivar outindex npart
  where recur =
          foldCmdM fassign flaplace fif
                   fwhile fdecl fseq fskip
                   fbmap famap fbsum fpart
        recur1 newfskip =
          foldCmdM fassign flaplace fif
                   fwhile fdecl fseq newfskip
                   fbmap famap fbsum fpart
