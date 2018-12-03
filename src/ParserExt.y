{

{-# OPTIONS_GHC -Wno-unused-imports #-}

module ParserExt where

import Data.Foldable
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad.Except
import Control.Monad.State.Strict

import LexerExt
import SyntaxExt hiding (Tau(..))
import qualified SyntaxExt as S (Tau(..))
import Data.Comp
import Data.Comp.Annotation
import Data.Map (fromList)
import Prelude hiding (LT, GT, EQ)


}

%name      parseCmd  Cmd
%name      parseExpr Expr
%name      parseProg Prog
%tokentype { Token }
%error     { parseError }
%monad     { Parser } { (>>=) } { return }

%token
  ident     { TIdent _ _ }
  int       { TInt   _ _ }
  float     { TFloat _ _ }
  ','       { TComma _    }
  '('       { TLParen _   }
  ')'       { TRParen _   }
  '{'       { TLBrace _   }
  '}'       { TRBrace _   }
  '['       { TLBrack _   }
  ']'       { TRBrack _   }
  ':'       { TColon _    }
  ';'       { TSemiColon _ }
  '+'       { TPlus _ }
  '-'       { TMinus _ }
  '*'       { TMult _ }
  '/'       { TDiv _ }
  '.'       { TDot _ }
  '='       { TEq _ }
  '=='      { TEqEq _ }
  '!='      { TNeq _ }
  '<'       { TLt _ }
  '<='      { TLe _ }
  '>'       { TGt _ }
  '>='      { TGe _ }
  '&&'      { TAnd _ }
  '||'      { TOr _ }
  '$='      { TSample _ }
  true      { TTrue _ }
  false     { TFalse _ }
  if        { TIf _ }
  then      { TThen _ }
  else      { TElse _ }
  end       { TEnd _ }
  do        { TDo _ }
  while     { TWhile _ }
  skip      { TSkip _ }
  laplace   { TLaplace _ }
  length    { TLength _ }
  exp       { TExp _ }
  log       { TLog _ }
  clip      { TClip _ }
  scale     { TScale _ }
  dot       { TDotP _ }
  cmd       { TCmdAnn _ }
  expr      { TExprAnn _ }
  extension { TExt _ }
  types     { TTypes _ }

%right ';'
%nonassoc '='
%left '.'
%left '[' '{'
%left '||'
%left '&&'
%nonassoc '==' '!='
%nonassoc '<' '<=' '>' '>='
%left '+' '-'
%left '*' '/'
%nonassoc ATOM

%%

Ty
: ident
{% do
    v <- getIdent $1
    case v of
      "int" -> return S.TInt
      "float" -> return S.TFloat
      "bool" -> return S.TBool
      x -> failWithMsg $ "Unknown type: " ++ show x
}
| '[' Ty '(' int ')' ']'
{% do
    len <- getInt $4
    return $ S.TArr $2 (Just len)
}
| '[' Ty ']'
{ S.TArr $2 Nothing }
| '{' Ty '}'
{ S.TBag $2 }

Decl
: ident ':' Ty
{% do
    v <- getIdent $1
    return $ Decl (token2Position $1) v 0.0 $3
}
| ident ':' '[' float ']' Ty
{% do
    v <- getIdent $1
    s <- getFloat $4
    return $ Decl (token2Position $1) v s $6
}

ManyDecls
: Decl ';'
{ [$1] }
| Decl ';' ManyDecls
{ $1 : $3 }

Prog
: types ManyDecls end Cmd
{ Prog $2 $4 }

Cmd :: { Term ImpP'' }
: ident ':' cmd
{% do
    v <- getIdent $1
    useExtVarAs v Command (token2Position $1)
    return (inject $ CExtVar v :&: (token2Position $1))
}
| skip
{ inject $ CSkip :&: (token2Position $1) }
| Expr '=' Expr
{ inject $ CAssign (deepInject $1) (deepInject $3) :&: (token2Position $2) }
| Expr '$=' laplace '(' FloatExpr ',' Expr ')'
{ inject $ CLaplace (deepInject $1) (snd $5) (deepInject $7) :&: (token2Position $2) }
| if Expr then Cmd else Cmd end
{ inject $ CIf (deepInject $2) $4 $6 :&: (token2Position $1) }
| while Expr do Cmd end
{ inject $ CWhile (deepInject $2) $4 :&: (token2Position $1) }
| Cmd ';'
{ $1 }
| Cmd ';' Cmd
{
  let p = cmd2Position $1
  in inject $ CSeq $1 $3 :&: p
}
| ident '(' ExtensionParams ')'
{% do
    extName <- getIdent $1
    return $ iACExt (token2Position $1) extName $3
}
| extension ident '(' PushManyIdents ')' '{' Cmd '}' PopManyIdents
{% do
    extName <- getIdent $2
    return $ iACExtDecl (token2Position $1) extName $9 $7
}

PushManyIdents
: ManyIdents
{% do
    pushBoundExtVars $1
    return $1
}

PopManyIdents
:
{% popBoundExtVars }

ManyIdents
:
{ [] }
| ident
{% do
    v <- getIdent $1
    return [v]
}
| ident ',' ManyIdents
{% do
    v <- getIdent $1
    return $ v:$3
}

ExtensionParam :: { Term ImpP'' }
: ident ':' expr
{% do
    v <- getIdent $1
    useExtVarAs v Expression (token2Position $1)
    return $ inject $ EExtVar v :&: (token2Position $1)
}
| Expr
{ deepInject $1 }
| Cmd
{ $1 }

ExtensionParams
:
{ [] }
| ExtensionParam
{ [$1] }
| ExtensionParam ',' ExtensionParams
{ $1 : $3 }

IntExpr
: int
  {% getInt $1 >>= \v -> return (token2Position $1, v)
  }
| '-' int %prec ATOM
  {% getInt $1 >>= \v -> return (token2Position $1, (-v))
  }

FloatExpr
: float
  {% getFloat $1 >>= \v -> return (token2Position $1, v)
  }
| '-' float %prec ATOM
  {% getFloat $1 >>= \v -> return (token2Position $1, (-v))
  }

BoolExpr
: true
{ (token2Position $1, True) }
| false
{ (token2Position $1, False) }

Literal
: IntExpr
{ (fst $1, LInt $ snd $1) }
| FloatExpr
{ (fst $1, LFloat $ snd $1) }
| BoolExpr
{ (fst $1, LBool $ snd $1) }
| '[' ManyExprs ']'
{ (token2Position $1, LArr $2) }
| '{' ManyExprs '}'
{ (token2Position $1, LBag $2) }

AtomExpr :: { Term ExprP' }
: ident
{% do
  v <- getIdent $1
  isVExtVar <- isBoundExtVar v
  case isVExtVar of
    True -> do
      useExtVarAs v Expression (token2Position $1)
      return $ iAEExtVar (token2Position $1) v
    False -> return $ iAEVar (token2Position $1) v
}
| Literal
{ iAELit (fst $1) (snd $1) }
| '(' Expr ')'
{ $2 }

IndexExpr
: AtomExpr '[' Expr ']'
{ iAEIndex (token2Position $2) $1 $3 }
| IndexExpr '[' Expr ']'
{ iAEIndex (token2Position $2) $1 $3 }

Expr :: { Term ExprP' }
: AtomExpr
{ $1 }
| IndexExpr
{ $1 }
| length '(' Expr ')'
{ inject $ ELength $3 :&: (token2Position $1) }
| Expr '+' Expr
{ inject $ EBinop PLUS $1 $3 :&: (token2Position $2) }
| Expr '-' Expr
{ inject $ EBinop MINUS $1 $3 :&: (token2Position $2) }
| Expr '*' Expr
{ inject $ EBinop MULT $1 $3 :&: (token2Position $2) }
| Expr '/' Expr
{ inject $ EBinop DIV $1 $3 :&: (token2Position $2) }
| Expr '<' Expr
{ inject $ EBinop LT $1 $3 :&: (token2Position $2) }
| Expr '<=' Expr
{ inject $ EBinop LE $1 $3 :&: (token2Position $2) }
| Expr '>' Expr
{ inject $ EBinop GT $1 $3 :&: (token2Position $2) }
| Expr '>=' Expr
{ inject $ EBinop GE $1 $3 :&: (token2Position $2) }
| Expr '==' Expr
{ inject $ EBinop EQ $1 $3 :&: (token2Position $2) }
| Expr '!=' Expr
{ inject $ EBinop NEQ $1 $3 :&: (token2Position $2) }
| Expr '&&' Expr
{ inject $ EBinop AND $1 $3 :&: (token2Position $2) }
| Expr '||' Expr
{ inject $ EBinop OR $1 $3 :&: (token2Position $2) }
| log '(' Expr ')'
{ inject $ ELog $3 :&: (token2Position $1) }
| exp '(' Expr ')'
{ inject $ EExp $3 :&: (token2Position $1) }
| scale '(' Expr ',' Expr ')'
{ inject $ EScale $3 $5 :&: (token2Position $1) }
| dot '(' Expr ',' Expr ')'
{ inject $ EDot $3 $5 :&: (token2Position $1) }
| clip '(' Expr ',' Literal ')'
{ inject $ EClip $3 (snd $5) :&: (token2Position $1) }

ManyExprs
:
{ [] }
| Expr
{ [$1] }
| Expr ',' ManyExprs
{ $1:$3 }

{

parseCmd  :: [Token] -> Parser (Term ImpP'')
parseExpr :: [Token] -> Parser (Term ExprP')

type SortPosition = (SyntaxSort, Position)

data ParserState = ParserState {
  parser_state_bvs :: [String] -- list of bound extension variables
  , parser_state_bvs_sort :: M.Map String (S.Set SortPosition) -- how each extension variable was used
}

type ParserStateStack = [ParserState]

newtype Parser a = Parser { runParser :: ExceptT String (State ParserStateStack) a }
  deriving (Functor, Applicative, Monad, MonadError String, MonadState ParserStateStack)

pushBoundExtVars :: [String] -> Parser ()
pushBoundExtVars bvs = do
  stack <- get
  put $ (ParserState bvs M.empty):stack

useExtVarAs :: String -> SyntaxSort -> Position -> Parser ()
useExtVarAs bv sort p = do
  stack <- get
  (hd, tl) <- case stack of
    [] -> failWithMsg "No bound variable state yet, did you forget to pushBoundExtVars?"
    hd:tl -> return (hd, tl)
  let bvsSorts = parser_state_bvs_sort hd
  let hd' = hd{parser_state_bvs_sort=M.insertWith S.union bv (S.singleton (sort, p)) bvsSorts}
  put $ hd':tl

popBoundExtVars :: Parser [AnyExtVar]
popBoundExtVars = do
  stack <- get
  (hd, tl) <- case stack of
    [] -> failWithMsg "No bound variable state yet, did you forget to pushBoundExtVars?"
    hd:tl -> return (hd, tl)
  foldrM (iter $ parser_state_bvs_sort hd) [] (parser_state_bvs hd)
  where iter :: M.Map String (S.Set SortPosition) -> String -> [AnyExtVar] -> Parser [AnyExtVar]
        iter bvsSorts v acc = do
          vSorts <- case M.lookup v bvsSorts of
            Nothing -> failWithMsg $ "Extension variable " ++ v ++ " has ambiguous sort"
            Just sorts -> return sorts
          let vSortsList = S.toList vSorts
          if all (\s -> fst s == Expression) vSortsList
          then return $ (anyExprVar v):acc
          else
            if all (\s -> fst s == Command) vSortsList
            then return $ (anyCmdVar v):acc
            else failWithMsg $ "Extension variable "
                             ++ v
                             ++ " is used both as expression and command at these positions: "
                             ++ show (map snd vSortsList)

isBoundExtVar :: String -> Parser Bool
isBoundExtVar v = do
  stack <- get
  if null stack
  then return False
  else return $ v `elem` (parser_state_bvs . head $ stack)

failWithMsg :: String -> Parser a
failWithMsg msg = throwError msg

parseError :: [Token] -> Parser a
parseError [] = failWithMsg "Expecting more tokens, but none are left"
parseError (tok : _) = failWithMsg $ "Unexpected token: " ++ show tok

token2Position :: Token -> Position
token2Position tok =
  let AlexPn _ line col = getAlexPosn tok
  in Position line col

getIdent :: Token -> Parser String
getIdent (TIdent _ x) = return x
getIdent t = failWithMsg $ "Expecting an TIdent, position = " ++ show (getAlexPosn t)

getInt :: Token -> Parser Int
getInt (TInt _ v) = return v
getInt t = failWithMsg $ "Expecting an TInt, position = " ++ show (getAlexPosn t)

getFloat :: Token -> Parser Float
getFloat (TFloat _ v) = return v
getFloat t = failWithMsg $ "Expecting an TFloat, position = " ++ show (getAlexPosn t)

parse :: ([Token] -> Parser a) -> String -> Either String a
parse p input = do
  tokens <- runAlex input scanTokens
  evalState (runExceptT . runParser $ p tokens) []

}
