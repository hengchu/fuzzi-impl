{
module Parser where

import Lexer
import Syntax
import Data.Map (fromList)
import Prelude hiding (LT, GT, EQ)
}

%name      parseProg Cmd
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
  bmap      { TBMap _ }
  amap      { TAMap _ }
  bsum      { TBSum _ }
  partition { TPartition _ }
  length    { TLength _ }
  clip      { TClip _ }

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
  : skip                                      { CSkip    (token2Position $1) }
  | ident '=' Expr                            { CAssign  (token2Position $2) (getIdent $1) $3 }
  | ident '[' Expr ']' '=' Expr               { CAUpdate (token2Position $5) (getIdent $1) $3 $6 }
  | ident '$=' laplace '(' float ',' Expr ')' { CLaplace (token2Position $2) (getIdent $1) (getFloat $5) $7 }
  | ident ':' LargeType                       { CDecl    (token2Position $2) (getIdent $1) 0 $3 }
  | ident ':' '[' float ']' LargeType         { CDecl    (token2Position $2) (getIdent $1) (getFloat $4) $6 }
  | if Expr then Cmd else Cmd end             { CIf      (token2Position $1) $2 $4 $6 }
  | while Expr do Cmd end                     { CWhile   (token2Position $1) $2 $4 }
  | Cmd ';'                                   { $1 }
  | Cmd ';' Cmd                               { CSeq     (token2Position $2) $1 $3 }
  | bmap '(' ident ',' ident ',' ident ',' ident ',' ident ',' '{' Cmd '}' ')'
      { CBMap (token2Position $1)
              (getIdent $3)
              (getIdent $5)
              (getIdent $7)
              (getIdent $9)
              (getIdent $11)
              $14
      }
  | amap '(' ident ',' ident ',' ident ',' ident ',' ident ',' '{' Cmd '}' ')'
      { CAMap (token2Position $1)
              (getIdent $3)
              (getIdent $5)
              (getIdent $7)
              (getIdent $9)
              (getIdent $11)
              $14
      }
  | bsum '(' ident ',' ident ',' ident ',' ident ',' Literal ')'
      { CBSum (token2Position $1) (getIdent $3) (getIdent $5) (getIdent $7) (getIdent $9) (fst $11) }

SmallType
  : ident { case (getIdent $1) of
              "int"   -> STInt
              "float" -> STFloat
              "bool"  -> STBool
              x       -> error $ "Unknown type: " ++ x
          }

LabelTypePair
  : ident ':' SmallType { (getIdent $1, $3) }

LabelTypePairs
  : LabelTypePair                    { [$1]    }
  | LabelTypePair ',' LabelTypePairs { $1 : $3 }

LargeType
  : SmallType              { LTSmall $1 }
  | '{' LabelTypePairs '}' { LTRow . RowType . fromList $ $2 }
  | '[' SmallType ']'      { LTArray  $2 }
  | '{' LargeType '}'      { LTBag $2 }

Expr
  : Literal                                  { ELit (snd $1) (fst $1) }
  | ident                                    { EVar (token2Position $1) (getIdent $1) }
  | length '(' ident ')'                     { ELenVar (token2Position $1) (getIdent $1) }
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
  | ident '[' Expr ']'                       { EIndex (token2Position $2) (getIdent $1) $3       }
  | Expr '{' ident '=' Expr '}'              { ERUpdate (token2Position $2) $1 (getIdent $3) $5  }
  | Expr '.' ident                           { ERAccess (token2Position $2) $1 (getIdent $3)     }
  | '(' Expr ')' %prec ATOM                  { $2 }
  | clip '(' Expr ',' Literal ')'            { EClip (token2Position $1) $3 (fst $5) }

SmallLiteral
  : int   { (SILit (getInt $1), token2Position $1) }
  | float { (SFLit (getFloat $1), token2Position $1) }
  | true  { (SBLit True, token2Position $1) }
  | false { (SBLit False, token2Position $1) }

Literal
  : SmallLiteral              { (SLit (fst $1), snd $1) }
  | '{' LabelLiteralPairs '}' { (RLit . RowLit $ fromList $2, token2Position $1) }

LabelLiteralPair
  : ident '=' SmallLiteral   { (getIdent $1, fst $3) }

LabelLiteralPairs
  : LabelLiteralPair                       { [$1]    }
  | LabelLiteralPair ',' LabelLiteralPairs { $1 : $3 }

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
