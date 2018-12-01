{-# OPTIONS_GHC -Wno-missing-signatures #-}

module SyntaxExt where

import Type.Reflection
import qualified Data.Set as S
import Data.Comp
import Data.Comp.Show ()
import Data.Comp.Equality ()
import Data.Comp.Derive
import Data.Comp.Variables

type Var = String

data Binop = LT | LE | GT | GE | AND | OR | EQ | NEQ | PLUS | MINUS | MULT | DIV
  deriving (Show, Eq, Ord, Enum, Bounded)

type Line   = Int
type Column = Int

data Position = Position Line Column
  deriving (Show, Eq, Ord)

data C = C deriving (Typeable, Show, Eq)
data E = E deriving (Typeable, Show, Eq)

data ExtVar :: * -> * -> * where
  CExtVar :: Var -> ExtVar C a
  EExtVar :: Var -> ExtVar E a

data AnyExtVar :: * where
  AnyExtVar :: (Typeable s) => ExtVar s a -> AnyExtVar

varSort :: forall s a. (Typeable s) => ExtVar s a -> TypeRep s
varSort _ = typeRep @s

anyExprVar :: Var -> AnyExtVar
anyExprVar = AnyExtVar . EExtVar

anyCmdVar :: Var -> AnyExtVar
anyCmdVar = AnyExtVar . CExtVar

instance Eq AnyExtVar where
  a == b =
    case (a, b) of
      (AnyExtVar a', AnyExtVar b') ->
        case eqTypeRep (varSort a') (varSort b') of
          Just HRefl ->
            case (a', b') of
              (CExtVar a'', CExtVar b'') -> a'' == b''
              (EExtVar a'', EExtVar b'') -> a'' == b''
          _ -> False

instance Ord AnyExtVar where
  compare a b =
    case (a, b) of
      (AnyExtVar a', AnyExtVar b') ->
        case eqTypeRep (varSort a') (varSort b') of
          Just HRefl ->
            case (a', b') of
              (CExtVar a'', CExtVar b'') -> compare a'' b''
              (EExtVar a'', EExtVar b'') -> compare a'' b''
          _ ->
            case (a', b') of
              (EExtVar _,   CExtVar _) -> Prelude.LT
              (CExtVar _,   EExtVar _) -> Prelude.GT
              _ -> error "impossible case"

deriving instance Show    (ExtVar s a)
deriving instance Functor (ExtVar s)
deriving instance Eq      (ExtVar s a)
deriving instance Ord     (ExtVar s a)
deriving instance Show    AnyExtVar

data Lit e = LInt Int
           | LFloat Float
           | LBool Bool
           | LArr [e]
           | LBag [e]
  deriving (Functor, Show, Eq, Foldable, Traversable)

data Expr e = EVar Var
            | ELength e
            | ELit (Lit e)
            | EBinop Binop e e
            | EIndex e e
            | EFloat e
            | EExp e
            | ELog e
            | EClip e (Lit e)
            | EScale e e
            | EDot e e
            deriving Functor

type ExprP  = Expr :&: Position

type Expr'  = ExtVar E :+: Expr
type ExprP' = (ExtVar E :&: Position) :+: ExprP

$(derive [makeTraversable, makeFoldable, makeEqF,
          makeShowF, smartConstructors, smartAConstructors]
         [''Expr, ''ExtVar])

data Cmd c = CAssign c c
           | CLaplace c Float c
           | CIf c c c
           | CWhile c c
           | CSeq c c
           | CSkip
           deriving Functor

type CmdP = Cmd :&: Position

data CExt c = CExt String [c]
  deriving (Functor)
data CExtDecl c = CExtDecl String [AnyExtVar] c
  deriving (Functor)

-- The language where extension definition is not allowed, but extension
-- application is possible
type Cmd'  = ExtVar C
           :+: CExt
           :+: Cmd
type CmdP' =   (ExtVar C :&: Position)
           :+: (CExt     :&: Position)
           :+: (Cmd      :&: Position)

-- The language for advanced users who want to define their own extensions
type Cmd''  =   ExtVar C
            :+: CExt
            :+: CExtDecl
            :+: Cmd
type CmdP'' =   (ExtVar C :&: Position)
            :+: (CExt     :&: Position)
            :+: (CExtDecl :&: Position)
            :+: (Cmd      :&: Position)

-- The vanilla IMP language
type Imp  =   Expr :+: Cmd
type ImpP =   Expr :&: Position
          :+: CmdP :&: Position

-- |The extended Imp language with extensions
type Imp'  =   ExtVar E
           :+: Expr
           :+: ExtVar C
           :+: CExt
           :+: Cmd
type ImpP' =   ExtVar E :&: Position
           :+: Expr     :&: Position
           :+: ExtVar C :&: Position
           :+: CExt     :&: Position
           :+: Cmd      :&: Position

-- |The extended Imp language with extensions and extension definition
type Imp''  =   ExtVar E
            :+: Expr
            :+: ExtVar C
            :+: CExt
            :+: CExtDecl
            :+: Cmd
type ImpP'' =   ExtVar E :&: Position
            :+: Expr     :&: Position
            :+: ExtVar C :&: Position
            :+: CExt     :&: Position
            :+: CExtDecl :&: Position
            :+: Cmd      :&: Position

$(derive [makeTraversable, makeFoldable, makeEqF, makeShowF,
          smartConstructors, smartAConstructors]
         [''Cmd, ''CExt, ''CExtDecl])

data SyntaxSort = Expression | Command
  deriving (Show, Eq, Ord)

class Wellformed f where
  syntaxSort :: AlgM Maybe f SyntaxSort

$(derive [liftSum] [''Wellformed])

instance Wellformed (ExtVar E) where
  syntaxSort _ = return Expression

guard :: Bool -> Maybe ()
guard True = return ()
guard False = Nothing

instance Wellformed Expr where
  syntaxSort (EVar _) = return Expression
  syntaxSort (ELength e) = do
    guard (e == Expression)
    return Expression
  syntaxSort (ELit (LArr es)) = do
    guard $ all (== Expression) es
    return Expression
  syntaxSort (ELit (LBag es)) = do
    guard $ all (== Expression) es
    return Expression
  syntaxSort (ELit _) = return Expression
  syntaxSort (EBinop _ e1 e2) = do
    guard (e1 == Expression)
    guard (e2 == Expression)
    return Expression
  syntaxSort (EIndex e1 e2) = do
    guard (e1 == Expression)
    guard (e2 == Expression)
    return Expression
  syntaxSort (EFloat e) = do
    guard (e == Expression)
    return Expression
  syntaxSort (EExp e) = do
    guard (e == Expression)
    return Expression
  syntaxSort (ELog e) = do
    guard (e == Expression)
    return Expression
  syntaxSort (EClip e lit) = do
    litSort <- syntaxSort (ELit lit)
    guard (litSort == Expression)
    guard (e == Expression)
    return Expression
  syntaxSort (EScale e1 e2) = do
    guard (e1 == Expression)
    guard (e2 == Expression)
    return Expression
  syntaxSort (EDot e1 e2) = do
    guard (e1 == Expression)
    guard (e2 == Expression)
    return Expression

instance Wellformed (ExtVar C) where
  syntaxSort _ = return Command

instance Wellformed Cmd where
  syntaxSort (CAssign e1 e2) = do
    guard (e1 == Expression)
    guard (e2 == Expression)
    return Command
  syntaxSort (CLaplace e1 _ e2) = do
    guard (e1 == Expression)
    guard (e2 == Expression)
    return Command
  syntaxSort (CIf e c1 c2) = do
    guard (e == Expression)
    guard (c1 == Command)
    guard (c2 == Command)
    return Command
  syntaxSort (CWhile e c) = do
    guard (e == Expression)
    guard (c == Command)
    return Command
  syntaxSort (CSeq c1 c2) = do
    guard (c1 == Command)
    guard (c2 == Command)
    return Command
  syntaxSort CSkip = return Command

instance Wellformed CExt where
  syntaxSort (CExt _ _) = return Command

instance HasVars (ExtVar E) AnyExtVar where
  isVar v = Just $ AnyExtVar v

instance HasVars Expr AnyExtVar

instance HasVars (ExtVar C) AnyExtVar where
  isVar v = Just $ AnyExtVar v

instance HasVars Cmd AnyExtVar
instance HasVars CExt AnyExtVar

instance HasVars CExtDecl AnyExtVar where
  bindsVars (CExtDecl _ vars c) =
    c |-> S.fromList vars

cmd2Position :: Term ImpP'' -> Position
cmd2Position c = snd . (projectA @Imp'') . unTerm $ c

{-

e1 :: Term Expr'
e1 = iEBinop PLUS (iELit (LInt 1)) (iEExtVar "x")

e2 :: Term Expr'
e2 = iEBinop MINUS (iEVar "y") (iEVar "z")

e4 :: Term Expr'
e4 = substVars (\v -> if v == (AnyExtVar $ EExtVar "x") then Just e2 else Nothing) e1

c1 :: Term Imp'
c1 = iCAssign (deepInject e1) (deepInject e2)

c2 :: Term Imp'
c2 = iCExt "test" [deepInject e1, deepInject e2, c1, iCExtVar "z"]

-}
