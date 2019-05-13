{-# OPTIONS_GHC -Wno-missing-signatures -Wno-orphans #-}

-- |The 'SyntaxExt' module implements the data types that represent Fuzzi
-- terms. Since Fuzzi has a few slightly different variations that represent
-- terms in different stages of the typechecking process, we use the 'compdata'
-- package to implement these terms as extensible sums in order to reduce
-- redundant code.
--
-- That is why all term types are 'Functor's. These 'Functor's are then supplied
-- to the 'compdata''s 'Term' type constructor to produce a type-level fixpoint,
-- whose values are Fuzzi terms.
module SyntaxExt where

import Control.Lens
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.Writer.Strict
import Data.Typeable
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Comp
import Data.Comp.Show ()
import Data.Comp.Equality ()
import Data.Comp.Derive
import Data.Comp.Variables

-- |Variables are just 'String's in this implementation.
type Var = String

-- |Fuzzi types.
data Tau
  -- |Fuzzi int
  = TInt
  -- |Fuzzi float
  | TFloat
  -- |Fuzzi boolean
  | TBool
  -- |A top type that is only used in typechecking for assigning temporary types
  -- to empty array literals
  | TAny
  -- |Fuzzi products
  | TProd Tau Tau
  -- |Fuzzi arrays. Arrays can be of a fixed length.
  | TArr { _tau_ty :: Tau, _tau_fixed_length :: (Maybe Int) }
  -- |Fuzzi bags
  | TBag { _tau_ty :: Tau }
  deriving (Show, Eq)

$(makeLensesWith underscoreFields ''Tau)

-- |A Fuzzi type declaration contains a variable, its sensitivity and its type.
data Decl = Decl Position Var Float Tau
  deriving (Show, Eq)

-- |Binary operators in Fuzzi.
data Binop = LT | LE | GT | GE | AND | OR | EQ | NEQ | PLUS | MINUS | MULT | DIV
  deriving (Show, Eq, Ord, Enum, Bounded)

type Line   = Int
type Column = Int

-- |Another source position type. This is only used to remove dependency on the
-- lexer in this module.
data Position = Position Line Column
  deriving (Show, Eq, Ord)

-- |The 'C' type is only used as a type-level index in the 'ExtVar' type to denote
-- extension variables that can only be substituted with commands.
data C = C deriving (Typeable, Show, Eq)

-- |The 'E' type is only used as a type-level index in the 'ExtVar' type to denote
-- extension variables that can only be substituted with expressions.
data E = E deriving (Typeable, Show, Eq)

-- |An indexed type for extension variables.
data ExtVar :: * -> * -> * where
  CExtVar :: Var -> ExtVar C a
  EExtVar :: Var -> ExtVar E a

-- |'AnyExtVar' hides the type-level index. A value of this type can hold either
-- a 'C' extension variable or an 'E' extension variable.
data AnyExtVar :: * where
  AnyExtVar :: (Typeable s) => ExtVar s a -> AnyExtVar

-- |A function that tests whether two extension variables are of the same sort,
-- and produces evidence that can be used to refine the equality when 's' and
-- 's'' are indeed equal.
eqVarSort :: forall s s' a a'. (Typeable s, Typeable s')
          => ExtVar s a -> ExtVar s' a' -> Maybe (s :~: s')
eqVarSort _ _ = eqT @s @s'

-- |Wraps an 'E' extension variable.
anyExprVar :: Var -> AnyExtVar
anyExprVar = AnyExtVar . EExtVar

-- |Wraps a 'C' extension variable.
anyCmdVar :: Var -> AnyExtVar
anyCmdVar = AnyExtVar . CExtVar

instance Eq AnyExtVar where
  a == b =
    case (a, b) of
      (AnyExtVar a', AnyExtVar b') ->
        case eqVarSort a' b' of
          Just Refl ->
            case (a', b') of
              (CExtVar a'', CExtVar b'') -> a'' == b''
              (EExtVar a'', EExtVar b'') -> a'' == b''
          _ -> False

instance Ord AnyExtVar where
  compare a b =
    case (a, b) of
      (AnyExtVar a', AnyExtVar b') ->
        case eqVarSort a' b' of
          Just Refl ->
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

-- |Fuzzi literals.
data Lit e
  -- |A literal int
  = LInt Int
  -- |A literal float
  | LFloat Float
  -- |A literal boolean
  | LBool Bool
  -- |A literal array
  | LArr [e]
  -- |A literal bag
  | LBag [e]
  deriving (Functor, Show, Eq, Foldable, Traversable)

-- |Fuzzi expressions.
data Expr e
  -- |A variable
  = EVar Var
  -- |A length expression
  | ELength e
  -- |A Fuzzi literal
  | ELit (Lit e)
  -- |A binary operation
  | EBinop Binop e e
  -- |An index operation
  | EIndex e e
  -- |An int-to-float cast operation
  | EFloat e
  -- |A natural exponentiation operation
  | EExp e
  -- |A natural log operation
  | ELog e
  -- |A clip operation
  | EClip e (Lit e)
  -- |A scale operation
  | EScale e e
  -- |A dot product operation
  | EDot e e
  -- |Projection of first field from a product
  | EFst e
  -- |Projection of second field from a product
  | ESnd e
  deriving Functor

-- |Fuzzi expression annotated with 'Position'
type ExprP  = Expr :&: Position

-- |Fuzzi expression extended with extension variables
type Expr'  = ExtVar E :+: Expr

-- |Fuzzi expression extended with extension variables and annotated with 'Position'
type ExprP' = (ExtVar E :&: Position) :+: Expr :&: Position

$(derive [makeTraversable, makeFoldable, makeEqF,
          makeShowF, smartConstructors, smartAConstructors]
         [''Expr, ''ExtVar])

-- |Fuzzi commands
data Cmd c
  -- |Assignment
  = CAssign c c
  -- |Laplace sampling
  | CLaplace c Float c
  -- |Conditional branching
  | CIf c c c
  -- |While loops
  | CWhile c c
  -- |Sequence of commands
  | CSeq c c
  -- |No-op skip command
  | CSkip
  deriving Functor

-- |Fuzzi command annotated with 'Position'
type CmdP = Cmd :&: Position

-- |Fuzzi command to invoke an extension.
data CExt c = CExt String [c]
  deriving (Functor)

-- |Fuzzi extension declaration.
data CExtDecl c = CExtDecl String [AnyExtVar] c
  deriving (Functor)

-- |Fuzzi typechecker hints: this is only used in typechecking to provide
-- provenance on expanded code.
data CTCHint c = CTCHint String [c] c
  deriving (Functor)

-- |The Fuzzi variant where extension definition is not allowed, but extension
-- application is possible
type Cmd'  = ExtVar C
           :+: CExt
           :+: Cmd

-- |Same as 'Cmd'' but annotated with 'Position'.
type CmdP' =   (ExtVar C :&: Position)
           :+: (CExt     :&: Position)
           :+: (Cmd      :&: Position)

-- |The Fuzzi variant for advanced users who want to define their own extensions
type Cmd''  =   ExtVar C
            :+: CExt
            :+: CExtDecl
            :+: Cmd

-- |Same as 'Cmd''' but annotated with 'Position'.
type CmdP'' =   (ExtVar C :&: Position)
            :+: (CExt     :&: Position)
            :+: (CExtDecl :&: Position)
            :+: (Cmd      :&: Position)

-- |The vanilla IMP language
type Imp  =   Expr :+: Cmd

-- |Same as 'Imp' but annotated with 'Position'.
type ImpP =   Expr :&: Position
          :+: Cmd  :&: Position

-- |The extended Imp language with extensions
type Imp'  =   ExtVar E
           :+: Expr
           :+: ExtVar C
           :+: CExt
           :+: Cmd

-- |Same as 'Imp'' but annotated with 'Position'
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

-- |Same as 'Imp''' but annotated with 'Position'
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

-- |This is an intermediate representation of Fuzzi programs. Used during the
-- extension expansion process, where not all extension variables have been
-- substituted yet.
type ImpTC'  =   ExtVar E
             :+: Expr
             :+: ExtVar C
             :+: CExt
             :+: Cmd
             :+: CTCHint

-- |Same as 'ImpTC' but annotated with 'Position'.
type ImpTCP  =   Expr     :&: Position
             :+: Cmd      :&: Position
             :+: CTCHint  :&: Position

-- |Same as 'ImpTC'' but annotated with 'Position'.
type ImpTCP' =   ExtVar E :&: Position
             :+: Expr     :&: Position
             :+: ExtVar C :&: Position
             :+: CExt     :&: Position
             :+: Cmd      :&: Position
             :+: CTCHint  :&: Position

$(derive [makeTraversable, makeFoldable, makeEqF, makeShowF,
          smartConstructors, smartAConstructors]
         [''Cmd, ''CExt, ''CExtDecl, ''CTCHint])

-- |A Fuzzi program contains a list of type declarations, and the program term.
--
-- The 'Term' type constructor is provided by 'compdata', which takes a functor
-- and builds a fixpoint of that functor. Here the fixpoint of the 'ImpP'''
-- functor is the type that represents Fuzzi program terms.
data Prog = Prog {
  prog_decls :: [Decl]
  , prog_cmd :: Term ImpP''
  } deriving (Show, Eq)

-- |The computed sort of a Fuzzi term.
data SyntaxSort = Expression | Command
  deriving (Show, Eq, Ord)

-- |'Wellformed' implements a basic syntax sort checking procedure that rules
-- out invalid programs that, for example, applies a binary operation between
-- two commands.
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
  syntaxSort (EFst e1) = do
    guard (e1 == Expression)
    return Expression
  syntaxSort (ESnd e1) = do
    guard (e1 == Expression)
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

-- |Extracts the starting 'Position' from a command
cmd2Position :: Term ImpP'' -> Position
cmd2Position c = snd . (projectA @Imp'') . unTerm $ c

-- |Extracts the starting 'Position' from an expression
expr2Position :: Term ExprP' -> Position
expr2Position e = snd . (projectA @Expr') . unTerm $ e

-- |A map from extension name to the list of bound variables, and the body of
-- the extension
type ExtensionLibrary = M.Map String ([AnyExtVar], Term ImpP')

-- |Errors that can result from the expansion process.
data ExpandError
  -- |When an unknown extension is used, an error containing the source location
  -- and its name is reported
  = UnknownExtension Position String
  -- |When an extension is supplied an incorrect number of parameters, an error
  -- containing its position, expected number of params, and supplied number of
  -- params are reported
  | MismatchingNumberOfParams Position Int Int -- num bvs, num params
                 deriving (Show, Eq, Ord)

-- |Expand all extensions in an Fuzzi term given the 'ExtensionLibrary'.
expand :: ExtensionLibrary -> Term ImpP' -> Either [ExpandError] (Term ImpTCP')
expand lib c =
  case runWriter (expand' lib c) of
    (c', [])  -> Right c'
    (_, errs) -> Left errs

-- |The workhorse behind 'expand'
expand' :: (MonadWriter [ExpandError] m)
        => ExtensionLibrary -> Term ImpP' -> m (Term ImpTCP')
expand' lib c = cataM (expandAlg lib) c

-- |The 'Expand' class implements a monadic f-algebra that performs the
-- extension expansion. The algebra is then passed to the 'cataM' operator
-- provided by 'compdata' to be folded over the entire term.
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

-- |A shorthand function for reporting errors in a writer monad.
reportError :: (MonadWriter [e] m) => e -> m a -> m a
reportError e m = tell [e] >> m

-- |Errors that can result from the process of separating extension declaration
-- from Fuzzi program.
data RemoveCExtDeclError
  -- |Nested extension declarations are not allowed
  = NestedExtensionDeclaration Position String
  -- |Multiple extensions with the same name are not allowed
  | DuplicateExtensionDeclaration Position String
  deriving (Show, Eq, Ord)

-- |The 'RemoveCExtDecl' class implements a monadic homomorphism that strips
-- extension declarations from a Fuzzi term. The homomorphism is then supplied
-- to the 'appHomM' operator from 'compdata' to be applied structurally
-- throughout a Fuzzi term.
class RemoveCExtDecl f g where
  removeCExtDeclHom :: (MonadError RemoveCExtDeclError m) => HomM m f g

instance {-# OVERLAPPABLE #-}
  (f :<: g, Functor g) => RemoveCExtDecl f g where
  removeCExtDeclHom = return . simpCxt . inj

$(derive [liftSum] [''RemoveCExtDecl])

instance
  (Cmd :&: Position :<: g) => RemoveCExtDecl (CExtDecl :&: Position) g where
  removeCExtDeclHom (CExtDecl _ _ _ :&: p) = return $ inject (CSkip :&: p)

-- |Removes extension declarations from a Fuzzi term.
removeCExtDecl'' :: Term ImpP'' -> Either RemoveCExtDeclError (Term ImpP')
removeCExtDecl'' t = appHomM removeCExtDeclHom t

-- |The 'CollectCExtDecl' class implements a monadic f-algebra that collects all
-- extensions declarations in a Fuzzi term into an 'ExtensionLibrary'.
class CollectCExtDecl f where
  collectCExtDecl :: ( MonadWriter [RemoveCExtDeclError] m
                     , MonadState ExtensionLibrary m)
                  => AlgM m f (Term ImpP'')

$(derive [liftSum] [''CollectCExtDecl])

-- |Gathers all extension declaration as an 'ExtensionLibrary'
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

-- |A type that is the sum of all different kinds of errors that may appear in
-- the preprocessing phase of a Fuzzi program
data DesugarError
  = RemoveCExtE [RemoveCExtDeclError]
  | ExpandE [ExpandError]
  | FreeExtensionVariable Position Var
  | UnexpandedCExt Position String
  deriving (Show, Eq, Ord)

-- |Workhorse behind 'desugarExtensions'
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

-- |Removes all extension declarations, expands all extension, and verify there
-- are no more free extension variable and unexpanded extensions.
desugarExtensions :: Term ImpP'' -> Either DesugarError (Term ImpTCP)
desugarExtensions t = desugarExtensions' M.empty t

-- |The 'VerifyNoCExt' class implements an f-algebra that verifies there are no
-- more free extension variables and unexpanded extensions left.
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

-- |Extract a variable from a Fuzzi term.
projectEVar :: Term ImpTCP -> Maybe Var
projectEVar t =
  case project @ExprP t of
    Just (EVar x :&: _) -> Just x
    _ -> Nothing

-- |Extract an integer literal from a Fuzzi term.
projectELInt :: Term ImpTCP -> Maybe Int
projectELInt t =
  case project @ExprP t of
    Just (ELit (LInt v) :&: _) -> Just v
    _ -> Nothing

-- |Extracts a float literal from a Fuzzi term.
projectELFloat :: Term ImpTCP -> Maybe Float
projectELFloat t =
  case project @ExprP t of
    Just (ELit (LFloat v) :&: _) -> Just v
    _ -> Nothing

-- |Extracts the argument to a Fuzzi length operator.
projectELength :: Term ImpTCP -> Maybe (Term ImpTCP)
projectELength t =
  case project @ExprP t of
    Just (ELength e :&: _) -> Just e
    _ -> Nothing

-- |Extracts the name of an extension from a Fuzzi typechecker hint term.
projectExtName :: Term ImpTCP -> Maybe String
projectExtName t =
  case project @(CTCHint :&: Position) t of
    Just (CTCHint name _ _ :&: _) -> Just name
    _ -> Nothing
