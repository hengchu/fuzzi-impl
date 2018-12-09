{-# OPTIONS_GHC -Wno-missing-signatures -Wno-orphans #-}

module SyntaxExt where

import Control.Lens
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.Writer.Strict
import Type.Reflection
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Comp
import Data.Comp.Show ()
import Data.Comp.Equality ()
import Data.Comp.Derive
import Data.Comp.Variables

type Var = String

data Tau = TInt
         | TFloat
         | TBool
         | TAny
         | TArr { _tau_ty :: Tau, _tau_fixed_length :: (Maybe Int) }
         | TBag { _tau_ty :: Tau }
         deriving (Show, Eq)

$(makeLensesWith underscoreFields ''Tau)

data Decl = Decl Position Var Float Tau
  deriving (Show, Eq)


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

instance (EqF f, Eq a) => EqF (f :&: a) where
  eqF (f1 :&: a1) (f2 :&: a2) = eqF f1 f2 && a1 == a2

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
type ExprP' = (ExtVar E :&: Position) :+: Expr :&: Position

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
data CTCHint c = CTCHint String [c] c
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
          :+: Cmd  :&: Position

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

-- |The language that the typechecker sees, we remove all extension declarations
-- and extension application, but leave hints to the typechecker
type ImpTC   =   Expr
             :+: Cmd
             :+: CTCHint

type ImpTC'  =   ExtVar E
             :+: Expr
             :+: ExtVar C
             :+: CExt
             :+: Cmd
             :+: CTCHint

type ImpTCP  =   Expr     :&: Position
             :+: Cmd      :&: Position
             :+: CTCHint  :&: Position

type ImpTCP' =   ExtVar E :&: Position
             :+: Expr     :&: Position
             :+: ExtVar C :&: Position
             :+: CExt     :&: Position
             :+: Cmd      :&: Position
             :+: CTCHint  :&: Position

$(derive [makeTraversable, makeFoldable, makeEqF, makeShowF,
          smartConstructors, smartAConstructors]
         [''Cmd, ''CExt, ''CExtDecl, ''CTCHint])

data Prog = Prog {
  prog_decls :: [Decl]
  , prog_cmd :: Term ImpP''
  } deriving (Show, Eq)

data SyntaxSort = Expression | Command
  deriving (Show, Eq, Ord)

class Wellformed f where
  syntaxSort :: AlgM Maybe f SyntaxSort

$(derive [liftSum] [''Wellformed])

instance Wellformed (ExtVar E) where
  syntaxSort _ = return Expression

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

instance HasVars Cmd     AnyExtVar
instance HasVars CExt    AnyExtVar
instance HasVars CTCHint AnyExtVar

instance HasVars CExtDecl AnyExtVar where
  bindsVars (CExtDecl _ vars c) =
    c |-> S.fromList vars

cmd2Position :: Term ImpP'' -> Position
cmd2Position c = snd . (projectA @Imp'') . unTerm $ c

expr2Position :: Term ExprP' -> Position
expr2Position e = snd . (projectA @Expr') . unTerm $ e

-- A map from extension name to the list of bound variables, and the body of the
-- extension
type ExtensionLibrary = M.Map String ([AnyExtVar], Term ImpP')

data ExpandError = UnknownExtension Position String
                 | MismatchingNumberOfParams Position Int Int -- num bvs, num params
                 deriving (Show, Eq, Ord)

expand :: ExtensionLibrary -> Term ImpP' -> Either [ExpandError] (Term ImpTCP')
expand lib c =
  case runWriter (expand' lib c) of
    (c', [])  -> Right c'
    (_, errs) -> Left errs

expand' :: (MonadWriter [ExpandError] m)
        => ExtensionLibrary -> Term ImpP' -> m (Term ImpTCP')
expand' lib c = cataM (expandAlg lib) c

class Expand f where
  expandAlg :: (MonadWriter [ExpandError] m)
            => ExtensionLibrary -> AlgM m f (Term ImpTCP')

instance (Expand f1, Expand f2) => Expand (f1 :+: f2) where
  expandAlg lib = caseF (expandAlg lib) (expandAlg lib)

instance {-# OVERLAPPABLE #-}
  (Functor f, f :<: ImpTCP') => Expand f where
  expandAlg _ = return . inject

instance Expand (CExt :&: Position) where
  expandAlg lib c@(CExt name params :&: p) =
    case M.lookup name lib of
      Nothing ->
        reportError
          (UnknownExtension p name)
          (return . inject $ c)
      Just (bvs, bodyP)
        | length bvs == length params -> do
            let substCxt = M.fromList $ zip bvs (map (stripA @_ @ImpTC') params)
            expandedBodyP <- expand' lib bodyP
            case stripA @_ @ImpTC' expandedBodyP of
              body -> do
                let expandedBody = appSubst substCxt body
                return $ iACTCHint p name params $ ann p expandedBody
        | otherwise ->
            reportError
              (MismatchingNumberOfParams p (length bvs) (length params))
              (return . inject $ c)

reportError :: (MonadWriter [e] m) => e -> m a -> m a
reportError e m = tell [e] >> m

data RemoveCExtDeclError = NestedExtensionDeclaration Position String
                         | DuplicateExtensionDeclaration Position String
  deriving (Show, Eq, Ord)

class RemoveCExtDecl f g where
  removeCExtDeclHom :: (MonadError RemoveCExtDeclError m) => HomM m f g

instance {-# OVERLAPPABLE #-}
  (f :<: g, Functor g) => RemoveCExtDecl f g where
  removeCExtDeclHom = return . simpCxt . inj

$(derive [liftSum] [''RemoveCExtDecl])

instance
  (Cmd :&: Position :<: g) => RemoveCExtDecl (CExtDecl :&: Position) g where
  removeCExtDeclHom (CExtDecl _ _ _ :&: p) = return $ inject (CSkip :&: p)

removeCExtDecl'' :: Term ImpP'' -> Either RemoveCExtDeclError (Term ImpP')
removeCExtDecl'' t = appHomM removeCExtDeclHom t

class CollectCExtDecl f where
  collectCExtDecl :: ( MonadWriter [RemoveCExtDeclError] m
                     , MonadState ExtensionLibrary m)
                  => AlgM m f (Term ImpP'')

$(derive [liftSum] [''CollectCExtDecl])

getExtensionLibrary :: Term ImpP'' -> Either [RemoveCExtDeclError] ExtensionLibrary
getExtensionLibrary t =
  case runWriter $ execStateT (cataM collectCExtDecl t) M.empty of
    (lib, []) -> Right lib
    (_, errs) -> Left errs

instance {-# OVERLAPPABLE #-}
  (f :<: ImpP'') => CollectCExtDecl f where
  collectCExtDecl t = return (inject t)

instance CollectCExtDecl (CExtDecl :&: Position) where
  collectCExtDecl c@(CExtDecl name bvs body :&: p) = do
    lib <- get
    case M.lookup name lib of
      Just _ ->
        reportError
          (DuplicateExtensionDeclaration p name)
          (return . inject $ c)
      Nothing ->
        case deepProject @ImpP' body of
          Just body' -> do
            put $ M.insert name (bvs, body') lib
            return $ inject c
          Nothing ->
            reportError
              (NestedExtensionDeclaration p name)
              (return $ inject c)

data DesugarError = RemoveCExtE [RemoveCExtDeclError]
                  | ExpandE [ExpandError]
                  | FreeExtensionVariable Position Var
                  | UnexpandedCExt Position String
                  deriving (Show, Eq, Ord)

desugarExtensions' :: ExtensionLibrary -> Term ImpP'' -> Either DesugarError (Term ImpTCP)
desugarExtensions' lib t =
  let extensionLib = getExtensionLibrary t
      removedDecls = removeCExtDecl'' t
  in case (extensionLib, removedDecls) of
       (Left e, _) -> Left $ RemoveCExtE e
       (_, Left e) -> Left $ RemoveCExtE [e]
       (Right lib', Right t') ->
         case expand (lib `M.union` lib') t' of
           Left e -> Left $ ExpandE e
           Right t'' -> runExcept $ cataM verifyNoCExt t''

desugarExtensions :: Term ImpP'' -> Either DesugarError (Term ImpTCP)
desugarExtensions t = desugarExtensions' M.empty t

class VerifyNoCExt f where
  verifyNoCExt :: (MonadError DesugarError m) => AlgM m f (Term ImpTCP)

$(derive [liftSum] [''VerifyNoCExt])

instance {-# OVERLAPPABLE #-}
  (Functor f, f :<: ImpTCP) => VerifyNoCExt f where
  verifyNoCExt = return . inject

instance VerifyNoCExt (ExtVar s :&: Position) where
  verifyNoCExt (EExtVar v :&: p) = throwError $ FreeExtensionVariable p v
  verifyNoCExt (CExtVar v :&: p) = throwError $ FreeExtensionVariable p v

instance VerifyNoCExt (CExt :&: Position) where
  verifyNoCExt (CExt name _ :&: p) = throwError $ UnexpandedCExt p name

projectEVar :: Term ImpTCP -> Maybe Var
projectEVar t =
  case project @ExprP t of
    Just (EVar x :&: _) -> Just x
    _ -> Nothing

projectELInt :: Term ImpTCP -> Maybe Int
projectELInt t =
  case project @ExprP t of
    Just (ELit (LInt v) :&: _) -> Just v
    _ -> Nothing

projectELFloat :: Term ImpTCP -> Maybe Float
projectELFloat t =
  case project @ExprP t of
    Just (ELit (LFloat v) :&: _) -> Just v
    _ -> Nothing

projectELength :: Term ImpTCP -> Maybe (Term ImpTCP)
projectELength t =
  case project @ExprP t of
    Just (ELength e :&: _) -> Just e
    _ -> Nothing

projectExtName :: Term ImpTCP -> Maybe String
projectExtName t =
  case project @(CTCHint :&: Position) t of
    Just (CTCHint name _ _ :&: _) -> Just name
    _ -> Nothing
