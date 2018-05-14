{

module Lexer where

}

%wrapper "posn"

$digit      = 0-9
$alpha      = [a-zA-Z]
$whitespace = [\ \t]
$eol        = [\n\r]

@number     = $digit+
@identifier = $alpha($alpha|_|$digit|')*

tokens :-

$whitespace                   ;
$eol                          ;
"("                           { \p _ -> TLParen p }
")"                           { \p _ -> TRParen p }
","                           { \p _ -> TComma p }
"["                           { \p _ -> TLBrack p }
"]"                           { \p _ -> TRBrack p }
"{"                           { \p _ -> TLBrace p }
"}"                           { \p _ -> TRBrace p }
":"                           { \p _ -> TColon p }
";"                           { \p _ -> TSemiColon p }
"+"                           { \p _ -> TPlus p }
"-"                           { \p _ -> TMinus p }
"*"                           { \p _ -> TMult p }
"/"                           { \p _ -> TDiv p }
"."                           { \p _ -> TDot p }
"<"                           { \p _ -> TLt p }
"<="                          { \p _ -> TLe p }
">"                           { \p _ -> TGt p }
">="                          { \p _ -> TGe p }
"!="                          { \p _ -> TNeq p }
"&&"                          { \p _ -> TAnd p }
"||"                          { \p _ -> TOr p }
"$="                          { \p _ -> TSample p }
"=="                          { \p _ -> TEqEq p }
"="                           { \p _ -> TEq p }
"bmap"                        { \p _ -> TBMap p }
"amap"                        { \p _ -> TAMap p }
"bsum"                        { \p _ -> TBSum p }
"partition"                   { \p _ -> TPartition p }
"lap"                         { \p _ -> TLaplace p }
"length"                      { \p _ -> TLength p }
"clip"                        { \p _ -> TClip p }
"true"                        { \p _ -> TTrue p }
"false"                       { \p _ -> TFalse p }
"if"                          { \p _ -> TIf p }
"then"                        { \p _ -> TThen p }
"else"                        { \p _ -> TElse p }
"end"                         { \p _ -> TEnd p }
"do"                          { \p _ -> TDo p }
"while"                       { \p _ -> TWhile p }
"skip"                        { \p _ -> TSkip p }
@identifier                   { \p s -> TIdent p s }
@number \. @number            { \p s -> TFloat p (read s) }
@number                       { \p s -> TInt p (read s) }

{

data Token = TIdent     AlexPosn String
           | TInt       AlexPosn Int
           | TFloat     AlexPosn Float
           | TComma     AlexPosn
           | TLParen    AlexPosn
           | TRParen    AlexPosn
           | TLBrace    AlexPosn
           | TRBrace    AlexPosn
           | TLBrack    AlexPosn
           | TRBrack    AlexPosn
           | TColon     AlexPosn
           | TSemiColon AlexPosn
           | TPlus      AlexPosn
           | TMinus     AlexPosn
           | TMult      AlexPosn
           | TDiv       AlexPosn
           | TDot       AlexPosn
           | TEq        AlexPosn
           | TEqEq      AlexPosn
           | TNeq       AlexPosn
           | TLt        AlexPosn
           | TLe        AlexPosn
           | TGt        AlexPosn
           | TGe        AlexPosn
           | TAnd       AlexPosn
           | TOr        AlexPosn
           | TIf        AlexPosn
           | TThen      AlexPosn
           | TElse      AlexPosn
           | TEnd       AlexPosn
           | TDo        AlexPosn
           | TWhile     AlexPosn
           | TSkip      AlexPosn
           | TTrue      AlexPosn
           | TFalse     AlexPosn
           | TSample    AlexPosn
           | TLaplace   AlexPosn
           | TBMap      AlexPosn
           | TAMap      AlexPosn
           | TBSum      AlexPosn
           | TPartition AlexPosn
           | TLength    AlexPosn
           | TClip      AlexPosn
  deriving (Show, Eq)

getAlexPosn :: Token -> AlexPosn
getAlexPosn (TIdent     p _) = p
getAlexPosn (TInt       p _) = p
getAlexPosn (TFloat     p _) = p
getAlexPosn (TComma     p) = p
getAlexPosn (TLParen    p) = p
getAlexPosn (TRParen    p) = p
getAlexPosn (TLBrace    p) = p
getAlexPosn (TRBrace    p) = p
getAlexPosn (TLBrack    p) = p
getAlexPosn (TRBrack    p) = p
getAlexPosn (TColon     p) = p
getAlexPosn (TSemiColon p) = p
getAlexPosn (TPlus      p) = p
getAlexPosn (TMinus     p) = p
getAlexPosn (TMult      p) = p
getAlexPosn (TDiv       p) = p
getAlexPosn (TDot       p) = p
getAlexPosn (TEq        p) = p
getAlexPosn (TEqEq      p) = p
getAlexPosn (TNeq       p) = p
getAlexPosn (TLt        p) = p
getAlexPosn (TLe        p) = p
getAlexPosn (TGt        p) = p
getAlexPosn (TGe        p) = p
getAlexPosn (TAnd       p) = p
getAlexPosn (TOr        p) = p
getAlexPosn (TIf        p) = p
getAlexPosn (TThen      p) = p
getAlexPosn (TElse      p) = p
getAlexPosn (TEnd       p) = p
getAlexPosn (TDo        p) = p
getAlexPosn (TWhile     p) = p
getAlexPosn (TSkip      p) = p
getAlexPosn (TTrue      p) = p
getAlexPosn (TFalse     p) = p
getAlexPosn (TSample    p) = p
getAlexPosn (TLaplace   p) = p
getAlexPosn (TBMap      p) = p
getAlexPosn (TAMap      p) = p
getAlexPosn (TBSum      p) = p
getAlexPosn (TPartition p) = p
getAlexPosn (TLength    p) = p
getAlexPosn (TClip      p) = p
}
