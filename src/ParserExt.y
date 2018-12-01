{
module ParserExt where

import Data.Foldable
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad.Except
import Control.Monad.State.Strict

import LexerExt
import SyntaxExt
import Data.Comp
import Data.Map (fromList)
import Prelude hiding (LT, GT, EQ)


}

%name      parseCmd  Cmd
%name      parseExpr Expr
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

Cmd
: ident ':' cmd
{% do
    v <- getIdent $1
    useExtVarAs v Command (token2Position $1)
    return $ iCExtVar v
}
| skip
{ iCSkip }
| Expr '=' Expr
{ iCAssign (deepInject $1) (deepInject $3) }
| Expr '$=' laplace '(' FloatExpr ',' Expr ')'
{ iCLaplace (deepInject $1) (snd $5) (deepInject $7) }
| if Expr then Cmd else Cmd end
{ iCIf (deepInject $2) $4 $6 }
| while Expr do Cmd end
{ iCWhile (deepInject $2) $4 }
| Cmd ';'
{ $1 }
| Cmd ';' Cmd
{ iCSeq $1 $3 }
| ident '(' ExtensionParams ')'
{% do
    extName <- getIdent $1
    return $ iCExt extName $3
}
| extension ident '(' PushManyIdents ')' '{' Cmd '}' PopManyIdents
{% do
    extName <- getIdent $2
    return $ iCExtDecl extName $9 $7
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

ExtensionParam
: ident ':' expr
{% do
    v <- getIdent $1
    useExtVarAs v Expression (token2Position $1)
    return $ iEExtVar v
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
{ (LInt $ snd $1) }
| FloatExpr
{ (LFloat $ snd $1) }
| BoolExpr
{ (LBool $ snd $1) }
| '[' ManyExprs ']'
{ (LArr $2) }
| '{' ManyExprs '}'
{ (LBag $2) }

AtomExpr
: ident
{% do
  v <- getIdent $1
  isVExtVar <- isBoundExtVar v
  case isVExtVar of
    True -> do
      useExtVarAs v Expression (token2Position $1)
      return $ iEExtVar v
    False -> return $ iEVar v
}
| Literal
{ iELit $1 }
| '(' Expr ')'
{ $2 }

IndexExpr
: AtomExpr '[' Expr ']'
{ iEIndex $1 $3 }
| IndexExpr '[' Expr ']'
{ iEIndex $1 $3 }

Expr
: AtomExpr
{ $1 }
| IndexExpr
{ $1 }
| length '(' Expr ')'
{ iELength $3 }
| Expr '+' Expr
{ iEBinop PLUS $1 $3 }
| Expr '-' Expr
{ iEBinop MINUS $1 $3 }
| Expr '*' Expr
{ iEBinop MULT $1 $3 }
| Expr '/' Expr
{ iEBinop DIV $1 $3 }
| Expr '<' Expr
{ iEBinop LT $1 $3 }
| Expr '<=' Expr
{ iEBinop LE $1 $3 }
| Expr '>' Expr
{ iEBinop GT $1 $3 }
| Expr '>=' Expr
{ iEBinop GE $1 $3 }
| Expr '==' Expr
{ iEBinop EQ $1 $3 }
| Expr '!=' Expr
{ iEBinop NEQ $1 $3 }
| Expr '&&' Expr
{ iEBinop AND $1 $3 }
| Expr '||' Expr
{ iEBinop OR $1 $3 }
| log '(' Expr ')'
{ iELog $3 }
| exp '(' Expr ')'
{ iEExp $3 }
| scale '(' Expr ',' Expr ')'
{ iEScale $3 $5 }
| dot '(' Expr ',' Expr ')'
{ iEDot $3 $5 }
| clip '(' Expr ',' Literal ')'
{ iEClip $3 $5 }

ManyExprs
:
{ [] }
| Expr
{ [$1] }
| Expr ',' ManyExprs
{ $1:$3 }

{

parseCmd  :: [Token] -> Parser (Term Imp'')
parseExpr :: [Token] -> Parser (Term Expr')

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
