{
module Parser where

import Lexer
import Syntax hiding (Tau(..))
import qualified Syntax as S (Tau(..))
import Data.Map (fromList)
import Prelude hiding (LT, GT, EQ)
}

%name      parseProg Prog
%name      parseCmd  Cmd
%name      parseExpr Expr
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

RecTyPair
  : ident ':' Ty                              {% getIdent $1 `bindP` \ident ->
                                                 returnP (ident, $3)
                                              }

RecTyPairs
  : RecTyPair                                 { [$1] }
  | RecTyPair ',' RecTyPairs                  { $1 : $3 }

Ty
  : ident                                     {% getIdent $1 `bindP` \ident ->
                                                 case ident of
                                                   "int" -> returnP S.TInt
                                                   "float" -> returnP S.TFloat
                                                   "bool" -> returnP S.TBool
                                                   x -> failWithMsg $ "Unknown type: " ++ show x
                                              }
  | '[' Ty '(' int ')' ']'                    {% getInt $4 `bindP` \i ->
                                                 returnP $ S.TArr $2 (Just i) }
  | '[' Ty ']'                                { S.TArr $2 Nothing }
  | '{' Ty '}'                                { S.TBag $2 }
  | '{' RecTyPairs '}'                        { S.TRec . fromList $ $2 }

Decl
  : ident ':' Ty                          {% getIdent $1 `bindP` \ident ->
                                             returnP [Decl (token2Position $1) ident 0.0 $3]
                                          }
  | ident ':' '[' float ']' Ty            {% getIdent $1 `bindP` \ident ->
                                             getFloat $4 `bindP` \f ->
                                             returnP [Decl (token2Position $1) ident f $6]
                                          }
  | Decl ';' Decl                             { $1 ++ $3 }

Prog
  : Decl Cmd                                 { Prog $1 $2 }

Cmd
  : skip                                      { CSkip    (token2Position $1) }
  | Expr '=' Expr                             { CAssign (token2Position $2) $1 $3 }
  | ident '$=' laplace '(' float ',' Expr ')' {% getIdent $1 `bindP` \ident ->
                                                 getFloat $5 `bindP` \f ->
                                                 returnP $ CLaplace (token2Position $2)
                                                                    ident
                                                                    f
                                                                    $7
                                              }
  | if Expr then Cmd else Cmd end             { CIf      (token2Position $1) $2 $4 $6 }
  | while Expr do Cmd end                     { CWhile   (token2Position $1) $2 $4 }
  | Cmd ';'                                   { $1 }
  | Cmd ';' Cmd                               { CSeq     (token2Position $2) $1 $3 }
  | ident '(' ExtensionParams ')'             {% getIdent $1 `bindP` \ident ->
                                                 returnP $ CExt (token2Position $1) ident $3 }

Literal
  : int                                      {% getInt $1 `bindP` \i ->
                                                returnP $ (token2Position $1, LInt i) }
  | '-' int %prec ATOM                       {% getInt $2 `bindP` \i ->
                                                returnP $ (token2Position $1, LInt (-i)) }
  | float                                    {% getFloat $1 `bindP` \f ->
                                                returnP $ (token2Position $1, LFloat f) }
  | '-' float %prec ATOM                     {% getFloat $2 `bindP` \f ->
                                                returnP $ (token2Position $1, LFloat (-f)) }
  | true                                     { (token2Position $1, LBool True) }
  | false                                    { (token2Position $1, LBool False) }
  | '[' ManyExprs ']'                        { (token2Position $1, LArr $2) }
  | '{' ManyExprs '}'                        { (token2Position $1, LBag $2) }

AtomExpr
: ident                     {% getIdent $1 `bindP` \ident ->
                               returnP $ EVar (token2Position $1) ident}
| Literal                   {ELit (fst $1) (snd $1)}
| '(' Expr ')'              {$2}

IndexExpr
: AtomExpr '[' Expr ']'     {EIndex (token2Position $2) $1 $3}
| IndexExpr '[' Expr ']'    {EIndex (token2Position $2) $1 $3}

Expr
  : AtomExpr                                 { $1 }
  | IndexExpr                                { $1 }
  | length '(' Expr ')'                      { ELength (token2Position $1) $3 }
  | Expr '+' Expr                            { EBinop (token2Position $2) $1 PLUS  $3 }
  | Expr '-' Expr                            { EBinop (token2Position $2) $1 MINUS $3 }
  | Expr '*' Expr                            { EBinop (token2Position $2) $1 MULT  $3 }
  | Expr '/' Expr                            { EBinop (token2Position $2) $1 DIV   $3 }
  | Expr '<' Expr                            { EBinop (token2Position $2) $1 LT    $3 }
  | Expr '<=' Expr                           { EBinop (token2Position $2) $1 LE    $3 }
  | Expr '>' Expr                            { EBinop (token2Position $2) $1 GT    $3 }
  | Expr '>=' Expr                           { EBinop (token2Position $2) $1 GE    $3 }
  | Expr '==' Expr                           { EBinop (token2Position $2) $1 EQ    $3 }
  | Expr '!=' Expr                           { EBinop (token2Position $2) $1 NEQ   $3 }
  | Expr '&&' Expr                           { EBinop (token2Position $2) $1 AND   $3 }
  | Expr '||' Expr                           { EBinop (token2Position $2) $1 OR    $3 }
  | AtomExpr '.' ident                       {% getIdent $3 `bindP` \ident ->
                                                returnP $ ERAccess (token2Position $2) $1 ident }
  | fcast '(' Expr ')'                       { EFloat (token2Position $1) $3 }
  | log '(' Expr ')'                         { ELog (token2Position $1) $3 }
  | exp '(' Expr ')'                         { EExp (token2Position $1) $3 }
  | scale '(' Expr ',' Expr ')'              { EScale (token2Position $1) $3 $5 }
  | dot '(' Expr ',' Expr ')'                { EDot (token2Position $1) $3 $5 }
  | clip '(' Expr ',' Literal ')'            { EClip (token2Position $1) $3 (snd $5) }

CmdBlock
: '{' Cmd '}' { $2 }

ExtensionParam
: Expr     { PExpr $1 }
| CmdBlock { PCmd $1  }

ExtensionParams
:                { [] }
| ExtensionParam { [$1] }
| ExtensionParam ',' ExtensionParams { $1 : $3 }

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
