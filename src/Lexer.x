{

module Lexer where

import Control.Lens
import GHC.Generics
import Data.Generics.Product

}

%wrapper "monad"

$digit      = 0-9
$alpha      = [a-zA-Z]
$whitespace = [\ \t]
$eol        = [\n\r]

@number     = $digit+
@decimal    = @number \. @number
@scientific = @decimal "e" "-"{0,1} @number
@float      = (@decimal|@scientific)
@identifier = $alpha($alpha|_|$digit|')*

tokens :-

$whitespace                   ;
$eol                          ;
"("                           { tokAct $ \p _ -> TLParen p }
")"                           { tokAct $ \p _ -> TRParen p }
","                           { tokAct $ \p _ -> TComma p }
"["                           { tokAct $ \p _ -> TLBrack p }
"]"                           { tokAct $ \p _ -> TRBrack p }
"{"                           { tokAct $ \p _ -> TLBrace p }
"}"                           { tokAct $ \p _ -> TRBrace p }
":"                           { tokAct $ \p _ -> TColon p }
";"                           { tokAct $ \p _ -> TSemiColon p }
"+"                           { tokAct $ \p _ -> TPlus p }
"-"                           { tokAct $ \p _ -> TMinus p }
"*"                           { tokAct $ \p _ -> TMult p }
"/"                           { tokAct $ \p _ -> TDiv p }
"."                           { tokAct $ \p _ -> TDot p }
"<"                           { tokAct $ \p _ -> TLt p }
"<="                          { tokAct $ \p _ -> TLe p }
">"                           { tokAct $ \p _ -> TGt p }
">="                          { tokAct $ \p _ -> TGe p }
"!="                          { tokAct $ \p _ -> TNeq p }
"&&"                          { tokAct $ \p _ -> TAnd p }
"||"                          { tokAct $ \p _ -> TOr p }
"$="                          { tokAct $ \p _ -> TSample p }
"=="                          { tokAct $ \p _ -> TEqEq p }
"="                           { tokAct $ \p _ -> TEq p }
"exp"                         { tokAct $ \p _ -> TExp p }
"log"                         { tokAct $ \p _ -> TLog p }
"lap"                         { tokAct $ \p _ -> TLaplace p }
"length"                      { tokAct $ \p _ -> TLength p }
"clip"                        { tokAct $ \p _ -> TClip p }
"scale"                       { tokAct $ \p _ -> TScale p }
"dot"                         { tokAct $ \p _ -> TDotP p }
"true"                        { tokAct $ \p _ -> TTrue p }
"false"                       { tokAct $ \p _ -> TFalse p }
"if"                          { tokAct $ \p _ -> TIf p }
"then"                        { tokAct $ \p _ -> TThen p }
"else"                        { tokAct $ \p _ -> TElse p }
"end"                         { tokAct $ \p _ -> TEnd p }
"do"                          { tokAct $ \p _ -> TDo p }
"while"                       { tokAct $ \p _ -> TWhile p }
"repeat"                      { tokAct $ \p _ -> TRepeat p }
"skip"                        { tokAct $ \p _ -> TSkip p }
"fc"                          { tokAct $ \p _ -> TFCast p }
@identifier                   { ident }
@float                        { float }
@number                       { int }

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
           | TEOF       AlexPosn
  deriving (Generic, Show, Eq)

getAlexPosn :: Token -> AlexPosn
getAlexPosn tok = tok ^. (typed @AlexPosn)

tokAct :: (AlexPosn -> String -> Token) -> AlexInput -> Int -> Alex Token
tokAct f (p, _, _, s) _ = return $ f p s

ident :: AlexInput -> Int -> Alex Token
ident (p, _, _, s) len = return $ TIdent p (take len s)

float :: AlexInput -> Int -> Alex Token
float (p, _, _, s) len = return $ TFloat p (read $ take len s)

int :: AlexInput -> Int -> Alex Token
int (p, _, _, s) len = return $ TInt p (read $ take len s)

alexEOF :: Alex Token
alexEOF = return $ TEOF (AlexPn 0 0 0)

scanTokens' :: [Token] -> Alex [Token]
scanTokens' acc = do
  t <- alexMonadScan
  case t of
    TEOF _ -> return . reverse $ acc
    _      -> scanTokens' (t:acc)

scanTokens :: Alex [Token]
scanTokens = scanTokens' []
}
