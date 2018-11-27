module SyntaxExt where

import Syntax (Var, Binop(..))

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

data EVar e = EVar Var
  deriving (Functor)
data ELength e = ELength e
  deriving (Functor)
data ELit e = ELit (Lit e)
  deriving (Functor)
data EBinop e = EBinop Binop e e
  deriving (Functor)
data EIndex e = EIndex e e
  deriving (Functor)
data ERAccess e = ERAccess e String
  deriving (Functor)
data EFloat e = EFloat e
  deriving (Functor)
data EExp e = EExp e
  deriving (Functor)
data ELog e = ELog e
  deriving (Functor)
data EClip e = EClip e (Lit e)
  deriving (Functor)
data EScale e = EScale e e
  deriving (Functor)
data EDot e = EDot e e
  deriving (Functor)

type Expr = EVar :+: ELength :+: ELit
            :+: EBinop :+: EIndex :+: ERAccess
            :+: EFloat :+: EExp :+: ELog
            :+: EClip :+: EScale :+: EDot
type Expr' = ExtVar E :+: Expr

$(derive [makeTraversable, makeFoldable, makeEqF,
          makeShowF, smartConstructors, smartAConstructors]
         [''EVar, ''ELength, ''ELit, ''EBinop,
          ''EIndex, ''ERAccess, ''EFloat, ''EExp,
          ''ELog, ''EClip, ''EScale, ''EDot,
          ''ExtVar])

data CAssign e = CAssign e e
  deriving (Functor)
data CLaplace e = CLaplace Var Float e
  deriving (Functor)
data CIf c = CIf c c c
  deriving (Functor)
data CWhile c = CWhile c c
  deriving (Functor)
data CSeq c = CSeq c c
  deriving (Functor)
data CSkip c = CSkip
  deriving (Functor)
data CExt c = CExt String [c]
  deriving (Functor)
data CExtDecl c = CExtDecl String [AnyExtVar] c
  deriving (Functor)

-- The vanilla IMP language
type Imp = CAssign :+: CLaplace :+: CIf
           :+: CWhile :+: CSeq :+: CSkip :+: Expr

-- |The extended Imp language with extensions
type Imp' = ExtVar C :+: CExtDecl :+: CExt :+: Expr'

$(derive [makeTraversable, makeFoldable, makeEqF, makeShowF,
          smartConstructors, smartAConstructors]
         [''CAssign, ''CLaplace, ''CIf, ''CWhile,
          ''CSeq, ''CSkip, ''CExt, ''CExtDecl])

type Sort = Either C E

class Wellformed f where
  syntaxSort :: AlgM Maybe f Sort

$(derive [liftSum] [''Wellformed])

instance Wellformed EVar where
  syntaxSort _ = return . Right $ E

instance Wellformed ELength where
  syntaxSort (ELength s) = do
    case s of
      Left _ -> Nothing
      Right E -> return . Right $ E

instance Wellformed ELit where
  syntaxSort (ELit (LArr sorts)) = do
    if all (== (Right E)) sorts
      then return . Right $ E
      else Nothing
  syntaxSort (ELit (LBag sorts)) = do
    if all (== (Right E)) sorts
      then return . Right $ E
      else Nothing
  syntaxSort _ = return . Right $ E

instance Wellformed EBinop where
  syntaxSort (EBinop _ s1 s2) = do
    case (s1, s2) of
      (Right E, Right E) -> return $ Right E
      _ -> Nothing

instance Wellformed EIndex where
  syntaxSort (EIndex s1 s2) = do
    case (s1, s2) of
      (Right E, Right E) -> return $ Right E
      _ -> Nothing

instance Wellformed ERAccess where
  syntaxSort (ERAccess s _) = do
    if s == Right E
      then return $ Right E
      else Nothing

instance Wellformed EFloat where
  syntaxSort (EFloat s) = do
    if s == Right E
      then return $ Right E
      else Nothing

instance Wellformed EExp where
  syntaxSort (EExp s) = do
    if s == Right E
      then return $ Right E
      else Nothing

instance Wellformed ELog where
  syntaxSort (ELog s) = do
    if s == Right E
      then return $ Right E
      else Nothing

instance Wellformed EClip where
  syntaxSort (EClip s1 (LArr sorts)) = do
    case (s1, all (== Right E) sorts) of
      (Right E, True) -> return $ Right E
      _ -> Nothing
  syntaxSort (EClip s1 (LBag sorts)) = do
    case (s1, all (== Right E) sorts) of
      (Right E, True) -> return $ Right E
      _ -> Nothing
  syntaxSort (EClip s _) = do
    if s == Right E then return $ Right E else Nothing

instance Wellformed EScale where
  syntaxSort (EScale s1 s2) = do
    case (s1, s2) of
      (Right E, Right E) -> return $ Right E
      _ -> Nothing

instance Wellformed EDot where
  syntaxSort (EDot s1 s2) = do
    case (s1, s2) of
      (Right E, Right E) -> return $ Right E
      _ -> Nothing

instance Wellformed (ExtVar E) where
  syntaxSort _ = return $ Right E

instance Wellformed (ExtVar C) where
  syntaxSort _ = return $ Left C

instance Wellformed CAssign where
  syntaxSort (CAssign s1 s2) =
    case (s1, s2) of
      (Right E, Right E) -> return $ Left C
      _ -> Nothing

instance Wellformed CLaplace where
  syntaxSort (CLaplace _ _ s) =
    case s of
      Right E -> return $ Left C
      _ -> Nothing

instance Wellformed CIf where
  syntaxSort (CIf s1 s2 s3) =
    case (s1, s2, s3) of
      (Right E, Left C, Left C) -> return $ Left C
      _ -> Nothing

instance Wellformed CWhile where
  syntaxSort (CWhile s1 s2) =
    case (s1, s2) of
      (Right E, Left C) -> return $ Left C
      _ -> Nothing

instance Wellformed CSeq where
  syntaxSort (CSeq s1 s2) =
    case (s1, s2) of
      (Left C, Left C) -> return $ Left C
      _ -> Nothing

instance Wellformed CSkip where
  syntaxSort _ = return $ Left C

instance Wellformed CExt where
  syntaxSort (CExt _ _) = return $ Left C

instance Wellformed CExtDecl where
  syntaxSort (CExtDecl _ _ s) =
    case s of
      Left C -> return $ Left C
      _ -> Nothing

{-

instance HasVars (ExtVar E) (ExtVar E e) where
  isVar a =
    case a of
      EExtVar v -> Just (EExtVar v)

instance HasVars EVar     (ExtVar E e)
instance HasVars ELength  (ExtVar E e)
instance HasVars ELit     (ExtVar E e)
instance HasVars EBinop   (ExtVar E e)
instance HasVars EIndex   (ExtVar E e)
instance HasVars ERAccess (ExtVar E e)
instance HasVars EFloat   (ExtVar E e)
instance HasVars EExp     (ExtVar E e)
instance HasVars ELog     (ExtVar E e)
instance HasVars EClip    (ExtVar E e)
instance HasVars EScale   (ExtVar E e)
instance HasVars EDot     (ExtVar E e)

instance HasVars (ExtVar C) (ExtVar C c) where
  isVar a =
    case a of
      CExtVar v -> Just (CExtVar v)

instance HasVars (ExtVar C)   (ExtVar E e)
instance HasVars (CAssign e)  (ExtVar E e)
instance HasVars (CLaplace e) (ExtVar E e)
instance HasVars (CIf e)      (ExtVar E e)
instance HasVars (CWhile e)   (ExtVar E e)
instance HasVars CSeq         (ExtVar E e)
instance HasVars CSkip        (ExtVar E e)
instance HasVars (CExt e)     (ExtVar E e)
instance HasVars CExtDecl     (ExtVar E e) where
  bindsVars (CExtDecl _ vars c) = c |-> foldr iter S.empty vars
    where iter :: AnyExtVar -> S.Set (ExtVar E c) -> S.Set (ExtVar E c)
          iter v bvs =
            case v of
              AnyExtVar v' ->
                case eqTypeRep (varSort v') (typeRep @E) of
                  Just HRefl ->
                    case v' of
                      EExtVar v'' -> S.insert (EExtVar v'') bvs
                  _ -> bvs

instance HasVars (CAssign e)  (ExtVar C c)
instance HasVars (CLaplace e) (ExtVar C c)
instance HasVars (CIf e)      (ExtVar C c)
instance HasVars (CWhile e)   (ExtVar C c)
instance HasVars CSeq         (ExtVar C c)
instance HasVars CSkip        (ExtVar C c)
instance HasVars (CExt e)     (ExtVar C c)
instance HasVars CExtDecl     (ExtVar C c) where
  bindsVars (CExtDecl _ vars c) = c |-> foldr iter S.empty vars
    where  iter :: AnyExtVar -> S.Set (ExtVar C c) -> S.Set (ExtVar C c)
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

e3 :: Term Imp'
e3 = iEBinop PLUS (iELit (LInt 1)) (iCExtVar "x")

--e3 :: Term Expr'
--e3 = substVars (\v -> if v == (EExtVar "x") then Just e2 else Nothing) e1

--c1 :: Term Imp'
--c1 = iCAssign e1 e2
