module SyntaxExt where

import Syntax (Var, Binop(..))

import Data.Constraint
import Type.Reflection
import Data.List (intercalate)
import Data.Either (either)
import qualified Data.Set as S
import Data.Comp
import Data.Comp.Show ()
import Data.Comp.Equality ()
import Data.Comp.Derive
import Data.Comp.Variables
import Data.Comp.Mapping

data C = C deriving (Typeable, Show, Eq)
data E = E deriving (Typeable, Show, Eq)

data ExtVar :: * -> * -> * where
  CExtVar :: Var -> ExtVar C a
  EExtVar :: Var -> ExtVar E a

data AnyExtVar :: * where
  AnyExtVar :: (Typeable s) => ExtVar s a -> AnyExtVar

varSort :: forall s a. (Typeable s) => ExtVar s a -> TypeRep s
varSort _ = typeRep @s

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

type Expr' = ExtVar E :+: Expr

$(derive [makeTraversable, makeFoldable, makeEqF,
          makeShowF, smartConstructors, smartAConstructors]
         [''Expr, ''ExtVar])

data Cmd c = CAssign c c
           | CLaplace Var Float c
           | CIf c c c
           | CWhile c c
           | CSeq c c
           | CSkip
           deriving Functor

data CExt c = CExt String [c]
  deriving (Functor)
data CExtDecl c = CExtDecl String [AnyExtVar] c
  deriving (Functor)

exprSubImp :: Dict (Expr :<: Imp)
exprSubImp = Dict

exprSubImp' :: Dict (Expr' :<: Imp')
exprSubImp' = Dict

type Cmd' = ExtVar C :+: CExt :+: CExtDecl :+: Cmd

-- The vanilla IMP language
type Imp = Expr :+: Cmd

-- |The extended Imp language with extensions
type Imp' = Expr' :+: Cmd'

$(derive [makeTraversable, makeFoldable, makeEqF, makeShowF,
          smartConstructors, smartAConstructors]
         [''Cmd, ''CExt, ''CExtDecl])

type Sort = Either C E

class Wellformed f where
  syntaxSort :: AlgM Maybe f Sort

$(derive [liftSum] [''Wellformed])

{-

instance HasVars (ExtVar E) (ExtVar E e) where
  isVar (EExtVar v) = Just (EExtVar v)

instance HasVars EVar     (ExtVar s e)
instance HasVars ELength  (ExtVar s e)
instance HasVars ELit     (ExtVar s e)
instance HasVars EBinop   (ExtVar s e)
instance HasVars EIndex   (ExtVar s e)
instance HasVars ERAccess (ExtVar s e)
instance HasVars EFloat   (ExtVar s e)
instance HasVars EExp     (ExtVar s e)
instance HasVars ELog     (ExtVar s e)
instance HasVars EClip    (ExtVar s e)
instance HasVars EScale   (ExtVar s e)
instance HasVars EDot     (ExtVar s e)

instance HasVars CAssign  (ExtVar E e)
instance HasVars CLaplace (ExtVar E e)
instance HasVars CIf      (ExtVar E e)
instance HasVars CWhile   (ExtVar E e)
instance HasVars CSeq     (ExtVar E e)
instance HasVars CSkip    (ExtVar E e)
instance HasVars CExt     (ExtVar E e)
instance HasVars CExtDecl (ExtVar E e) where
  bindsVars (CExtDecl _ vars c) =
    c |-> foldr iter S.empty vars
    where iter :: AnyExtVar -> S.Set (ExtVar E e) -> S.Set (ExtVar E e)
          iter v bvs =
            case v of
              AnyExtVar v' ->
                case eqTypeRep (varSort v') (typeRep @E) of
                  Just HRefl ->
                    case v' of
                      EExtVar v'' -> S.insert (EExtVar v'') bvs
                  _ -> bvs

instance HasVars (ExtVar C) (ExtVar E e)
instance HasVars (ExtVar E) (ExtVar C c)
instance HasVars (ExtVar C) (ExtVar C c) where
  isVar (CExtVar v) = Just (CExtVar v)

instance HasVars CAssign  (ExtVar C c)
instance HasVars CLaplace (ExtVar C c)
instance HasVars CIf      (ExtVar C c)
instance HasVars CWhile   (ExtVar C c)
instance HasVars CSeq     (ExtVar C c)
instance HasVars CSkip    (ExtVar C c)
instance HasVars CExt     (ExtVar C c)
instance HasVars CExtDecl (ExtVar C c) where
  bindsVars (CExtDecl _ vars c) =
    c |-> foldr iter S.empty vars
    where iter :: AnyExtVar -> S.Set (ExtVar C e) -> S.Set (ExtVar C e)
          iter v bvs =
            case v of
              AnyExtVar v' ->
                case eqTypeRep (varSort v') (typeRep @C) of
                  Just HRefl ->
                    case v' of
                      CExtVar v'' -> S.insert (CExtVar v'') bvs
                  _ -> bvs

-}

e1 :: Term Expr'
e1 = iEBinop PLUS (iELit (LInt 1)) (iEExtVar "x")

e2 :: Term Expr'
e2 = iEBinop MINUS (iEVar "y") (iEVar "z")

--e4 :: Term Expr'
--e4 = substVars (\v -> if v == (EExtVar "x") then Just e2 else Nothing) e1

c1 :: Term Imp'
c1 = withDict exprSubImp' $ iCAssign (deepInject e1) (deepInject e2)
