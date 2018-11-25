module SyntaxExt where

import Syntax (Var, Binop(..))

import Data.List (intercalate)
import Data.Either (either)
import Data.Comp
import Data.Comp.Show ()
import Data.Comp.Equality ()
import Data.Comp.Derive

data Lit e = LInt Int
           | LFloat Float
           | LBool Bool
           | LArr [e]
           | LBag [e]
  deriving (Functor, Show, Eq, Foldable, Traversable)

data ExtVar a = ExtVar Var
  deriving Functor

data EVar e = EVar Var
  deriving Functor
data ELength e = ELength e
  deriving Functor
data ELit e = ELit (Lit e)
  deriving Functor
data EBinop e = EBinop Binop e e
  deriving Functor
data EIndex e = EIndex e e
  deriving Functor
data ERAccess e = ERAccess e String
  deriving Functor
data EFloat e = EFloat e
  deriving Functor
data EExp e = EExp e
  deriving Functor
data ELog e = ELog e
  deriving Functor
data EClip e = EClip e (Lit e)
  deriving Functor
data EScale e = EScale e e
  deriving Functor
data EDot e = EDot e e
  deriving Functor

type Expr = EVar :+: ELength :+: ELit
            :+: EBinop :+: EIndex :+: ERAccess
            :+: EFloat :+: EExp :+: ELog
            :+: EClip :+: EScale :+: EDot
type Expr' = ExtVar :+: Expr

$(derive [makeTraversable, makeFoldable, makeEqF, makeShowF, smartConstructors, smartAConstructors]
         [''EVar, ''ELength, ''ELit, ''EBinop,
          ''EIndex, ''ERAccess, ''EFloat, ''EExp,
          ''ELog, ''EClip, ''EScale, ''EDot,
          ''ExtVar])

data ParamE e = ParamE e
  deriving Functor
data ParamC c = ParamC c
  deriving Functor

$(derive [makeTraversable, makeFoldable, makeEqF, makeShowF, smartConstructors, smartAConstructors]
         [''ParamE, ''ParamC])

data CAssign e c = CAssign e e
  deriving (Functor)
data CLaplace e c = CLaplace Var Float e
  deriving (Functor)
data CIf e c = CIf e c c
  deriving (Functor)
data CWhile e c = CWhile e c
  deriving (Functor)
data CSeq c = CSeq c c
  deriving (Functor)
data CSkip c = CSkip
  deriving (Functor)
data CExt e c = CExt String [Either (ParamE e) (ParamC c)]
  deriving (Functor, Foldable, Traversable)

type GImp e = CAssign (Term e) :+: CLaplace (Term e) :+: CIf (Term e)
              :+: CWhile (Term e) :+: CSeq :+: CSkip

-- |The vanilla Imp language
type Imp  = GImp Expr

-- |The extended Imp language with extensions
type Imp' = ExtVar :+: (CExt (Term Expr')) :+: (GImp Expr')

$(derive [makeTraversable, makeFoldable, makeEqF, makeShowF, smartConstructors, smartAConstructors]
         [''CAssign, ''CLaplace, ''CIf, ''CWhile, ''CSeq, ''CSkip])

iCExt :: (CExt e :<: f) => String -> [Either (ParamE e) (ParamC (Cxt h f a))] -> Cxt h f a
iCExt name params = inject $ CExt name params

instance (Functor e, ShowF e) => ShowF (CExt (Term e)) where
  showF (CExt name params) =
    "CExt " ++ name ++ " " ++ (intercalate " " . map (either paramE paramC) $ params)
    where paramE :: ParamE (Term e) -> String
          paramE = showF . (fmap show)

          paramC :: ParamC String -> String
          paramC = showF

-- |An extension declaration, with its name, bound variables, and the expansion
-- definition.
data CExtDecl = CExtDecl String [Var] (Term Imp')

class SubstE f t g where
  substE :: Var -> ParamE (Term t) -> Alg f (Term g)

class Subst f p t g where
  subst :: Var -> p (Term t) -> Alg f (Term g)

instance {-# OVERLAPPING #-}
         ( Functor t
         , SubstE f1 t g
         , SubstE f2 t g
         ) => SubstE (f1 :+: f2) t g where
  substE x p =
    caseF
      (substE @f1 @t @g x p)
      (substE @f2 @t @g x p)

instance {-# OVERLAPPING #-}
         ( Functor t
         , Subst f1 ParamE t g
         , Subst f2 ParamE t g
         ) => Subst (f1 :+: f2) ParamE t g where
  subst x p =
    caseF
      (subst @f1 @ParamE @t @g x p)
      (subst @f2 @ParamE @t @g x p)

instance {-# OVERLAPPABLE #-}
         ( ExtVar :<: expr
         , Functor t
         , t      :<: expr
         ) => SubstE ExtVar t expr where
  substE x (ParamE t) = \e ->
    case e of
      ExtVar y | x == y -> deepInject t
      _ -> inject e

instance {-# OVERLAPPABLE #-} EVar :<: e => SubstE EVar t e where
  substE _ _ = inject

instance {-# OVERLAPPABLE #-} ( ELength :<: e
         , Functor t
         , t :<: e
         , SubstE e t e
         ) => SubstE ELength t e where
  substE x p = inject . (fmap (substE x p . unTerm))

instance {-# OVERLAPPABLE #-} ( ELit :<: e
         , Functor t
         , t :<: e
         , SubstE e t e
         ) => SubstE ELit t e where
  substE x p = \e ->
    case e of
      ELit le -> iELit $ fmap (substE x p . unTerm) le

instance {-# OVERLAPPABLE #-} ( EBinop :<: e
         , Functor t
         , t :<: e
         , SubstE e t e
         ) => SubstE EBinop t e where
  substE x p = inject . (fmap (substE x p . unTerm))

instance {-# OVERLAPPABLE #-} ( EIndex :<: e
         , Functor t
         , t :<: e
         , SubstE e t e
         ) => SubstE EIndex t e where
  substE x p = inject . (fmap (substE x p . unTerm))

instance {-# OVERLAPPABLE #-} ( ERAccess :<: e
         , Functor t
         , t :<: e
         , SubstE e t e
         ) => SubstE ERAccess t e where
  substE x p = inject . (fmap (substE x p . unTerm))

instance {-# OVERLAPPABLE #-} ( EFloat :<: e
         , Functor t
         , t :<: e
         , SubstE e t e
         ) => SubstE EFloat t e where
  substE x p = inject . (fmap (substE x p . unTerm))

instance {-# OVERLAPPABLE #-} ( EExp :<: e
         , Functor t
         , t :<: e
         , SubstE e t e
         ) => SubstE EExp t e where
  substE x p = inject . (fmap (substE x p . unTerm))

instance {-# OVERLAPPABLE #-} ( ELog :<: e
         , Functor t
         , t :<: e
         , SubstE e t e
         ) => SubstE ELog t e where
  substE x p = inject . (fmap (substE x p . unTerm))

instance {-# OVERLAPPABLE #-} ( EScale :<: e
         , Functor t
         , t :<: e
         , SubstE e t e
         ) => SubstE EScale t e where
  substE x p = inject . (fmap (substE x p . unTerm))

instance {-# OVERLAPPABLE #-} ( EDot :<: e
         , Functor t
         , t :<: e
         , SubstE e t e
         ) => SubstE EDot t e where
  substE x p = inject . (fmap (substE x p . unTerm))

instance {-# OVERLAPPABLE #-} ( EClip :<: e
         , Functor t
         , t :<: e
         , SubstE e t e
         ) => SubstE EClip t e where
  substE x p = inject . (fmap (substE x p . unTerm))

instance {-# OVERLAPPABLE #-}
         ( ExtVar :<: cmd
         , Functor t
         , t      :<: cmd
         ) => Subst ExtVar ParamC t cmd where
  subst x (ParamC t) = \c ->
    case c of
      ExtVar y | x == y -> deepInject t
      _ -> inject c

instance {-# OVERLAPPABLE #-}
         ( ExtVar :<: cmd
         ) => Subst ExtVar ParamE t cmd where
  subst _ _ = inject

instance {-# OVERLAPPABLE #-} ( Functor e
         , SubstE e t e
         , t :<: e
         , CAssign (Term e)  :<: cmd
         ) => Subst (CAssign (Term e)) ParamE t cmd where
  subst x p = \c ->
    case c of
      CAssign lhs rhs ->
        iCAssign (cata (substE @e @t @e x p) lhs) (cata (substE @e @t @e x p) rhs)

instance {-# OVERLAPPABLE #-} ( Functor e
         , SubstE e t e
         , t :<: e
         , CLaplace (Term e) :<: cmd
         ) => Subst (CLaplace (Term e)) ParamE t cmd where
  subst x p = \c ->
    case c of
      CLaplace y w e ->
        iCLaplace y w (cata (substE @e @t @e x p) e)

instance {-# OVERLAPPABLE #-} ( Functor e
         , Functor cmd
         , SubstE e t e
         , Subst cmd ParamE t cmd
         , t :<: e
         , CIf (Term e)  :<: cmd
         ) => Subst (CIf (Term e)) ParamE t cmd where
  subst x p = \c ->
    case c of
      CIf e c1 c2 ->
        iCIf (cata (substE @e @t @e x p) e)
             ((subst @cmd @_ @t @cmd x p) . unTerm $ c1)
             ((subst @cmd @_ @t @cmd x p) . unTerm $ c2)

instance {-# OVERLAPPABLE #-} ( Functor e
         , Functor cmd
         , SubstE e t e
         , Subst cmd ParamE t cmd
         , t :<: e
         , CWhile (Term e) :<: cmd
         ) => Subst (CWhile (Term e)) ParamE t cmd where
  subst x p = \c ->
    case c of
      CWhile e body ->
        iCWhile (cata (substE @e @t @e x p) e)
                ((subst @cmd @_ @t @cmd x p) . unTerm $ body)

instance {-# OVERLAPPABLE #-} ( Functor cmd
         , Subst cmd ParamE t cmd
         , CSeq :<: cmd
         ) => Subst CSeq ParamE t cmd where
  subst x p = \c ->
    case c of
      CSeq c1 c2 ->
        iCSeq ((subst @cmd @_ @t @cmd x p) . unTerm $ c1)
              ((subst @cmd @_ @t @cmd x p) . unTerm $ c2)

instance {-# OVERLAPPABLE #-} (CSkip :<: cmd) => Subst CSkip ParamE t cmd where
  subst _ _ = inject

instance {-# OVERLAPPABLE #-} ( Functor e
         , Functor cmd
         , SubstE e t e
         , Subst cmd ParamE t cmd
         , t :<: e
         , CExt (Term e)  :<: cmd
         ) => Subst (CExt (Term e)) ParamE t cmd where
  subst x p = \c ->
    case c of
      CExt nm params ->
        iCExt nm (map (either paramE paramC) params)
    where paramE :: ParamE (Term e) -> Either (ParamE (Term e)) any
          paramE (ParamE term) = Left . ParamE $ cata (substE @e @t @e x p) term

          paramC :: ParamC (Term cmd) -> Either any (ParamC (Term cmd))
          paramC (ParamC term) = Right . ParamC . (subst @cmd @_ @t @cmd x p) . unTerm $ term

substEC :: Var -> ParamE (Term Expr') -> Term Imp' -> Term Imp'
substEC x p c = cata (subst @Imp' @ParamE @Expr' @Imp' x p) c

e1 :: Term Expr'
e1 = iEBinop PLUS (iELit . LInt $ 1) (iExtVar "x")

e2 :: Term Expr'
e2 = iEVar "y"

c1 :: Term Imp'
c1 = iCAssign e2 e1

c2 :: Term Imp'
c2 = iCExt "bmap" [Left . ParamE $ e1, Right . ParamC $ c1]
