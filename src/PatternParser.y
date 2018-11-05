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
%monad     { Parser } { bindP } { returnP }

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
  boolescape { TBoolEscape _ }
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
: int
  {% getInt $1 `bindP` \i -> returnP (token2Position $1, AtomExact i) }
| '-' int %prec ATOM
  {% getInt $2 `bindP` \i -> returnP (token2Position $1, AtomExact (-i)) }
| intescape '(' ident ')'
  {% getIdent $3 `bindP` \ident -> returnP (token2Position $1, AtomWild ident) }

FloatPat
: float
  {% getFloat $1 `bindP` \f -> returnP (token2Position $1, AtomExact f) }
| '-' float %prec ATOM
  {% getFloat $2 `bindP` \f -> returnP (token2Position $1, AtomExact (-f))}
| floatescape '(' ident ')'
  {% getIdent $3 `bindP` \ident -> returnP (token2Position $1, AtomWild ident) }

BoolPat
: true
{ (token2Position $1, AtomExact True) }
| false
{ (token2Position $1, AtomExact False) }
| boolescape '(' ident ')'
{% getIdent $3 `bindP` \ident -> returnP (token2Position $1, AtomWild ident) }

VarPat
: ident
  {% getIdent $1 `bindP` \ident -> returnP (token2Position $1, AtomExact ident) }
| vescape '(' ident ')'
  {% getIdent $3 `bindP` \ident -> returnP (token2Position $1, AtomWild ident)  }

Cmd
  : cmdescape '(' ident ')'
    {% getIdent $3 `bindP` \ident -> returnP $ CPWild (token2Position $1) ident }
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
  | CmdBlock                                  { $1 }
  | ident '(' ExtensionParams ')'
  {% getIdent $1 `bindP` \ident ->
     returnP $ CPExt (token2Position $1) ident $3
  }

CmdBlock
: '{' Cmd '}' { CPBlock (token2Position $1) $2 }

ExtensionParam
: Expr     { PPExpr $1 }
| Cmd      { PPCmd $1  }

ExtensionParams
:                { [] }
| ExtensionParam { [$1] }
| ExtensionParam ',' ExtensionParams { $1 : $3 }

Literal
: IntPat                                   { (fst $1, LPInt . snd $ $1) }
| FloatPat                                 { (fst $1, LPFloat . snd $ $1) }
| BoolPat                                  { (fst $1, LPBool . snd $ $1 ) }
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
  : exprescape '(' ident ')'
  {% getIdent $3 `bindP` \ident -> returnP $ EPWild (token2Position $1) ident }
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
  | AtomExpr '.' ident
  {% getIdent $3 `bindP` \ident -> returnP $ EPRAccess (token2Position $2) $1 ident }
  | fcast '(' Expr ')'                       { EPFloat (token2Position $1) $3 }
  | log '(' Expr ')'                         { EPLog (token2Position $1) $3 }
  | exp '(' Expr ')'                         { EPExp (token2Position $1) $3 }
  | scale '(' Expr ',' Expr ')'              { EPScale (token2Position $1) $3 $5 }
  | dot '(' Expr ',' Expr ')'                { EPDot (token2Position $1) $3 $5 }
  | clip '(' Expr ',' Literal ')'            { EPClip (token2Position $1) $3 (snd $5) }

ManyExprs
  :                    { [] }
  | Expr               { [$1]    }
  | Expr ',' ManyExprs { $1 : $3 }

{
newtype Parser a = Parser { runParser :: Either String a }

returnP :: a -> Parser a
returnP a = Parser . Right $ a

bindP :: Parser a -> (a -> Parser b) -> Parser b
bindP (Parser pa) f =
  case pa of
    Left err -> Parser . Left $ err
    Right a  -> f a

failWithMsg :: String -> Parser a
failWithMsg msg = Parser . Left $ msg

parseError :: [Token] -> Parser a
parseError [] = failWithMsg "Expecting more tokens, but none are left"
parseError (tok : _) = failWithMsg $ "Unexpected token: " ++ show tok

token2Position :: Token -> Position
token2Position tok =
  let AlexPn _ line col = getAlexPosn tok
  in Position line col

getIdent :: Token -> Parser String
getIdent (TIdent _ x) = returnP x
getIdent t = failWithMsg $ "Expecting an TIdent, position = " ++ show (getAlexPosn t)

getInt :: Token -> Parser Int
getInt (TInt _ v) = returnP v
getInt t = failWithMsg $ "Expecting an TInt, position = " ++ show (getAlexPosn t)

getFloat :: Token -> Parser Float
getFloat (TFloat _ v) = returnP v
getFloat t = failWithMsg $ "Expecting an TFloat, position = " ++ show (getAlexPosn t)

parse :: ([Token] -> Parser a) -> String -> Either String a
parse p input = do
  tokens <- runAlex input scanTokens
  runParser $ p tokens
}
