{
module Parser where

import Lexer
import Syntax
import Data.Map (fromList)
import Prelude hiding (LT, GT, EQ)
}

%name      parseProg
%tokentype { Token }
%error     { parseError }

%token
  ident    { TIdent _ $$ }
  int      { TInt   _ $$ }
  float    { TFloat _ $$ }
  ','      { TComma _    }
  '('      { TLParen _   }
  ')'      { TRParen _   }
  '{'      { TLBrace _   }
  '}'      { TRBrace _   }
  '['      { TLBrack _   }
  ']'      { TRBrack _   }
  ':'      { TColon _    }
  ';'      { TSemiColon _ }
  '+'      { TPlus _ }
  '-'      { TMinus _ }
  '*'      { TMult _ }
  '/'      { TDiv _ }
  '.'      { TDot _ }
  '='      { TEq _ }
  '=='     { TEqEq _ }
  '!='     { TNeq _ }
  '<'      { TLt _ }
  '<='     { TLe _ }
  '>'      { TGt _ }
  '>='     { TGe _ }
  '&&'     { TAnd _ }
  '||'     { TOr _ }
  '$='     { TSample _ }
  true     { TTrue _ }
  false    { TFalse _ }
  if       { TIf _ }
  then     { TThen _ }
  else     { TElse _ }
  end      { TEnd _ }
  do       { TDo _ }
  while    { TWhile _ }
  skip     { TSkip _ }
  laplace  { TLaplace _ }

%right ';'
%left '.'
%left '[' '{'
%nonassoc '<' '<=' '>' '>='
%left '||'
%left '&&'
%nonassoc '==' '!='
%left '+' '-'
%left '*' '/'
%nonassoc ATOM

%%

Cmd
  : skip                                      { CSkip }
  | ident '=' Expr                            { CAssign  $1 $3 }
  | ident '[' Expr ']' '=' Expr               { CAUpdate $1 $3 $6 }
  | ident '$=' laplace '(' float ',' Expr ')' { CLaplace $1 $5 $7 }
  | ident ':' LargeType                       { CDecl    $1 0 $3 }
  | ident ':' '[' float ']' LargeType         { CDecl    $1 $4 $6 }
  | if Expr then Cmd else Cmd end             { CIf      $2 $4 $6 }
  | while Expr do Cmd end                     { CWhile   $2 $4 }
  | Cmd ';' Cmd                               { CSeq     $1 $3 }

SmallType
  : ident { case $1 of
              "int"   -> STInt
              "float" -> STFloat
              "bool"  -> STBool
              _       -> error $ "Unknown type: " ++ $1
          }

LabelTypePair
  : ident ':' SmallType { ($1, $3) }

LabelTypePairs
  : LabelTypePair                    { [$1]    }
  | LabelTypePair ',' LabelTypePairs { $1 : $3 }

LargeType
  : SmallType              { LTSmall $1 }
  | '{' LabelTypePairs '}' { LTRow . RowType . fromList $ $2 }
  | '[' SmallType ']'      { LTArray  $2 }
  | '{' LargeType '}'      { LTBag $2 }

Expr
  : Literal                                  { ELit $1            }
  | ident                                    { EVar $1            }
  | Expr '+' Expr                            { EBinop $1 PLUS  $3 }
  | Expr '-' Expr                            { EBinop $1 MINUS $3 }
  | Expr '*' Expr                            { EBinop $1 MULT  $3 }
  | Expr '/' Expr                            { EBinop $1 DIV   $3 }
  | Expr '<' Expr                            { EBinop $1 LT    $3 }
  | Expr '<=' Expr                           { EBinop $1 LE    $3 }
  | Expr '>' Expr                            { EBinop $1 GT    $3 }
  | Expr '>=' Expr                           { EBinop $1 GE    $3 }
  | Expr '==' Expr                           { EBinop $1 EQ    $3 }
  | Expr '!=' Expr                           { EBinop $1 NEQ   $3 }
  | Expr '&&' Expr                           { EBinop $1 AND   $3 }
  | Expr '||' Expr                           { EBinop $1 OR    $3 }
  | Expr '[' Expr ']'                        { EIndex $1 $3       }
  | Expr '{' ident '=' Expr '}'              { ERUpdate $1 $3 $5  }
  | Expr '.' ident                           { ERAccess $1 $3     }
  | '(' Expr ')' %prec ATOM                  { $2 }

SmallLiteral
  : int   { SILit $ $1 }
  | float { SFLit $ $1 }
  | true  { SBLit True }
  | false { SBLit False }

Literal
  : SmallLiteral              { SLit $1 }
  | '{' LabelLiteralPairs '}' { RLit . RowLit $ fromList $2 }

LabelLiteralPair
  : ident '=' SmallLiteral   { ($1, $3) }

LabelLiteralPairs
  : LabelLiteralPair                       { [$1]    }
  | LabelLiteralPair ',' LabelLiteralPairs { $1 : $3 }

{
parseError = undefined
}
