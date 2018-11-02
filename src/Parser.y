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
  : ident ':' Ty                              { (getIdent $1, $3) }

RecTyPairs
  : RecTyPair                                 { [$1] }
  | RecTyPair ',' RecTyPairs                  { $1 : $3 }

Ty
  : ident                                     { case getIdent $1 of
                                                  "int" -> S.TInt
                                                  "float" -> S.TFloat
                                                  "bool" -> S.TBool
                                                  x -> error $ "Unknown type: " ++ x
                                              }
  | '[' Ty '(' int ')' ']'                    { S.TArr $2 (Just $ getInt $4) }
  | '[' Ty ']'                                { S.TArr $2 Nothing }
  | '{' Ty '}'                                { S.TBag $2 }
  | '{' RecTyPairs '}'                        { S.TRec . fromList $ $2 }

Decl
  : ident ':' Ty                          { [Decl (token2Position $1) (getIdent $1) 0.0 $3] }
  | ident ':' '[' float ']' Ty            { [Decl (token2Position $1) (getIdent $1) (getFloat $4) $6] }
  | Decl ';' Decl                             { $1 ++ $3 }

Prog
  : Decl Cmd                                 { Prog $1 $2 }

Cmd
  : skip                                      { CSkip    (token2Position $1) }
  | Expr '=' Expr                             { CAssign (token2Position $2) $1 $3 }
  | ident '$=' laplace '(' float ',' Expr ')' { CLaplace (token2Position $2)
                                                         (getIdent $1)
                                                         (getFloat $5)
                                                         $7
                                              }
  | if Expr then Cmd else Cmd end             { CIf      (token2Position $1) $2 $4 $6 }
  | while Expr do Cmd end                     { CWhile   (token2Position $1) $2 $4 }
  | Cmd ';'                                   { $1 }
  | Cmd ';' Cmd                               { CSeq     (token2Position $2) $1 $3 }
  | ident '(' ExtensionParams ')'             { CExt (token2Position $1) (getIdent $1) $3 }

Literal
  : int                                      { (token2Position $1, LInt (getInt $1)) }
  | '-' int %prec ATOM                       { (token2Position $1, LInt (-(getInt $2))) }
  | float                                    { (token2Position $1, LFloat (getFloat $1)) }
  | '-' float %prec ATOM                     { (token2Position $1, LFloat (-(getFloat $2))) }
  | true                                     { (token2Position $1, LBool True) }
  | false                                    { (token2Position $1, LBool False) }
  | '[' ManyExprs ']'                        { (token2Position $1, LArr $2) }
  | '{' ManyExprs '}'                        { (token2Position $1, LBag $2) }

AtomExpr
: ident                     {EVar (token2Position $1) (getIdent $1)}
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
  | AtomExpr '.' ident                       { ERAccess (token2Position $2) $1 (getIdent $3) }
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
