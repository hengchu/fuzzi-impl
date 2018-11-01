{

module Lexer where

import Control.Lens
import GHC.Generics
import Data.Generics.Product

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
"exp"                         { \p _ -> TExp p }
"log"                         { \p _ -> TLog p }
"lap"                         { \p _ -> TLaplace p }
"length"                      { \p _ -> TLength p }
"clip"                        { \p _ -> TClip p }
"scale"                       { \p _ -> TScale p }
"dot"                         { \p _ -> TDotP p }
"true"                        { \p _ -> TTrue p }
"false"                       { \p _ -> TFalse p }
"if"                          { \p _ -> TIf p }
"then"                        { \p _ -> TThen p }
"else"                        { \p _ -> TElse p }
"end"                         { \p _ -> TEnd p }
"do"                          { \p _ -> TDo p }
"while"                       { \p _ -> TWhile p }
"repeat"                      { \p _ -> TRepeat p }
"skip"                        { \p _ -> TSkip p }
"fc"                          { \p _ -> TFCast p }
"v"                           { \p _ -> TVarEscape p }
"iesc"                        { \p _ -> TIntEscape p }
"fesc"                        { \p _ -> TFloatEscape p }
"e"                           { \p _ -> TExprEscape p }
"c"                           { \p _ -> TCmdEscape p }
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
           | TRepeat    AlexPosn
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
           | TExp       AlexPosn
           | TLog       AlexPosn
           | TClip      AlexPosn
           | TScale     AlexPosn
           | TDotP      AlexPosn
           | TFCast     AlexPosn
           | TVarEscape AlexPosn
           | TIntEscape AlexPosn
           | TFloatEscape AlexPosn
           | TExprEscape AlexPosn
           | TCmdEscape AlexPosn
  deriving (Generic, Show, Eq)

getAlexPosn :: Token -> AlexPosn
getAlexPosn token = token ^. (typed @AlexPosn)
}
