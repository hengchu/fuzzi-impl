{
module PatternParser where

import PatternLexer
import Syntax hiding (Tau(..))
import qualified Syntax as S (Tau(..))
import Data.Map (fromList)
import Prelude hiding (LT, GT, EQ)
}

%name      parseCmdPattern  Cmd
%name      parseExprPattern Expr
%tokentype { Token }
%error     { parseError }

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
  fcast     { TFCast _ }
  vescape   { TVarEscape _ }
  intescape { TIntEscape _ }
  floatescape { TFloatEscape _ }
  exprescape { TExprEscape _ }
  cmdescape { TCmdEscape _ }

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

IntPat
: int { (token2Position $1, (AtomExact . getInt $ $1)) }
| intescape '(' ident ')' { (token2Position $1, (AtomWild . getIdent $ $3)) }

FloatPat
: float { (token2Position $1, (AtomExact . getFloat $ $1)) }
| floatescape '(' ident ')' { (token2Position $1, (AtomWild . getIdent $ $3)) }

VarPat
: ident { (token2Position $1, (AtomExact . getIdent $ $1)) }
| vescape '(' ident ')' { (token2Position $1, (AtomWild . getIdent $ $3))  }

Cmd
  : cmdescape '(' ident ')'                   { CPWild (token2Position $1) (getIdent $3) }
  | skip                                      { CPSkip (token2Position $1) }
  | Expr '=' Expr                             { CPAssign (token2Position $2) $1 $3 }
  | VarPat '$=' laplace '(' FloatPat ',' Expr ')' { CPLaplace (token2Position $2)
                                                              (snd $1)
                                                              (snd $5)
                                                              $7
                                              }
  | if Expr then Cmd else Cmd end             { CPIf      (token2Position $1) $2 $4 $6 }
  | while Expr do Cmd end                     { CPWhile   (token2Position $1) $2 $4 }
  | Cmd ';'                                   { $1 }
  | Cmd ';' Cmd                               { CPSeq     (token2Position $2) $1 $3 }

Literal
: IntPat                                   { (fst $1, LPInt . snd $ $1) }
| FloatPat                                 { (fst $1, LPFloat . snd $ $1) }
| '[' ManyExprs ']'                        { (token2Position $1, LPArr $2) }
| '{' ManyExprs '}'                        { (token2Position $1, LPBag $2) }

AtomExpr
: VarPat                            {EPVar (fst $1) (snd $1)}
| Literal                           {EPLit (fst $1) (snd $1)}
| '(' Expr ')'                      {$2}

IndexExpr
: AtomExpr '[' Expr ']'     {EPIndex (token2Position $2) $1 $3}
| IndexExpr '[' Expr ']'    {EPIndex (token2Position $2) $1 $3}

Expr
  : exprescape '(' ident ')'                 { EPWild (token2Position $1) (getIdent $3) }
  | AtomExpr                                 { $1 }
  | IndexExpr                                { $1 }
  | length '(' Expr ')'                      { EPLength (token2Position $1) $3 }
  | Expr '+' Expr                            { EPBinop (token2Position $2) $1 PLUS  $3 }
  | Expr '-' Expr                            { EPBinop (token2Position $2) $1 MINUS $3 }
  | Expr '*' Expr                            { EPBinop (token2Position $2) $1 MULT  $3 }
  | Expr '/' Expr                            { EPBinop (token2Position $2) $1 DIV   $3 }
  | Expr '<' Expr                            { EPBinop (token2Position $2) $1 LT    $3 }
  | Expr '<=' Expr                           { EPBinop (token2Position $2) $1 LE    $3 }
  | Expr '>' Expr                            { EPBinop (token2Position $2) $1 GT    $3 }
  | Expr '>=' Expr                           { EPBinop (token2Position $2) $1 GE    $3 }
  | Expr '==' Expr                           { EPBinop (token2Position $2) $1 EQ    $3 }
  | Expr '!=' Expr                           { EPBinop (token2Position $2) $1 NEQ   $3 }
  | Expr '&&' Expr                           { EPBinop (token2Position $2) $1 AND   $3 }
  | Expr '||' Expr                           { EPBinop (token2Position $2) $1 OR    $3 }
  | Expr '.' ident                           { EPRAccess (token2Position $2) $1 (getIdent $3) }
  | fcast '(' Expr ')'                       { EPFloat (token2Position $1) $3 }
  | log '(' Expr ')'                         { EPLog (token2Position $1) $3 }
  | exp '(' Expr ')'                         { EPExp (token2Position $1) $3 }
  | scale '(' Expr ',' Expr ')'              { EPScale (token2Position $1) $3 $5 }
  | dot '(' Expr ',' Expr ')'                { EPDot (token2Position $1) $3 $5 }

ManyExprs
  : Expr               { [$1]    }
  | Expr ',' ManyExprs { $1 : $3 }

{
parseError :: [Token] -> a
parseError (tok : _) = error $ "Unexpected token: " ++ show tok

token2Position :: Token -> Position
token2Position tok =
  let AlexPn _ line col = getAlexPosn tok
  in Position line col

getIdent (TIdent _ x) = x
getIdent _ = error "Expecting an TIdent"

getInt (TInt _ v) = v
getInt _ = error "Expecting an TInt"

getFloat (TFloat _ v) = v
getFloat _ = error "Expecting an TFloat"
}
