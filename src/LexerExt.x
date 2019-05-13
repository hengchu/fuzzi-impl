{
-- |'LexerExt' implements a lexer for Fuzzi using Alex.
module LexerExt (
  -- * Functions
  -- ** The monadic lexer action
  scanTokens
  -- ** Extract source position from a token
  , getAlexPosn
  -- ** Invoke the lexer monad
  , runAlex
  -- * Types
  -- ** Tokens of Fuzzi
  , Token(..)
  -- ** Generated type from Alex that holds source positions
  , AlexPosn(..)
) where

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

<comment>"*/"                         { begin 0 }
<comment>.                            ;
<comment>$eol                         ;
<comment>$whitespace                  ;

<0>"/*"                          { begin comment }

<0>$whitespace                   ;
<0>$eol                          ;
<0>"("                           { tokAct $ \p _ -> TLParen p }
<0>")"                           { tokAct $ \p _ -> TRParen p }
<0>","                           { tokAct $ \p _ -> TComma p }
<0>"["                           { tokAct $ \p _ -> TLBrack p }
<0>"]"                           { tokAct $ \p _ -> TRBrack p }
<0>"{"                           { tokAct $ \p _ -> TLBrace p }
<0>"}"                           { tokAct $ \p _ -> TRBrace p }
<0>":"                           { tokAct $ \p _ -> TColon p }
<0>";"                           { tokAct $ \p _ -> TSemiColon p }
<0>"+"                           { tokAct $ \p _ -> TPlus p }
<0>"-"                           { tokAct $ \p _ -> TMinus p }
<0>"*"                           { tokAct $ \p _ -> TMult p }
<0>"/"                           { tokAct $ \p _ -> TDiv p }
<0>"."                           { tokAct $ \p _ -> TDot p }
<0>"<"                           { tokAct $ \p _ -> TLt p }
<0>"<="                          { tokAct $ \p _ -> TLe p }
<0>">"                           { tokAct $ \p _ -> TGt p }
<0>">="                          { tokAct $ \p _ -> TGe p }
<0>"!="                          { tokAct $ \p _ -> TNeq p }
<0>"&&"                          { tokAct $ \p _ -> TAnd p }
<0>"||"                          { tokAct $ \p _ -> TOr p }
<0>"$="                          { tokAct $ \p _ -> TSample p }
<0>"=="                          { tokAct $ \p _ -> TEqEq p }
<0>"="                           { tokAct $ \p _ -> TEq p }
<0>"fst"                         { tokAct $ \p _ -> TFst p }
<0>"snd"                         { tokAct $ \p _ -> TSnd p }
<0>"exp"                         { tokAct $ \p _ -> TExp p }
<0>"log"                         { tokAct $ \p _ -> TLog p }
<0>"lap"                         { tokAct $ \p _ -> TLaplace p }
<0>"length"                      { tokAct $ \p _ -> TLength p }
<0>"clip"                        { tokAct $ \p _ -> TClip p }
<0>"scale"                       { tokAct $ \p _ -> TScale p }
<0>"dot"                         { tokAct $ \p _ -> TDotP p }
<0>"fc"                          { tokAct $ \p _ -> TFCast p }
<0>"true"                        { tokAct $ \p _ -> TTrue p }
<0>"false"                       { tokAct $ \p _ -> TFalse p }
<0>"if"                          { tokAct $ \p _ -> TIf p }
<0>"then"                        { tokAct $ \p _ -> TThen p }
<0>"else"                        { tokAct $ \p _ -> TElse p }
<0>"end"                         { tokAct $ \p _ -> TEnd p }
<0>"do"                          { tokAct $ \p _ -> TDo p }
<0>"while"                       { tokAct $ \p _ -> TWhile p }
<0>"skip"                        { tokAct $ \p _ -> TSkip p }
<0>"cmd"                         { tokAct $ \p _ -> TCmdAnn p }
<0>"expr"                        { tokAct $ \p _ -> TExprAnn p }
<0>"extension"                   { tokAct $ \p _ -> TExt p }
<0>"types"                       { tokAct $ \p _ -> TTypes p }
<0>@identifier                   { ident }
<0>@float                        { float }
<0>@number                       { int }

{
-- | Values of type 'Token' represent all possible tokens in the Fuzzi language.
-- Each token contains its source position.
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
           | TExp       AlexPosn
           | TLog       AlexPosn
           | TClip      AlexPosn
           | TScale     AlexPosn
           | TDotP      AlexPosn
           | TFCast     AlexPosn
           | TCmdAnn    AlexPosn
           | TExprAnn   AlexPosn
           | TExt       AlexPosn
           | TTypes     AlexPosn
           | TFst       AlexPosn
           | TSnd       AlexPosn
           | TEOF       AlexPosn
  deriving (Generic, Show, Eq)

-- |Returns the source position of the token.
getAlexPosn :: Token -> AlexPosn
getAlexPosn tok = tok ^. (typed @AlexPosn)

-- |A utility function for building a monad token parsing action.
tokAct :: (AlexPosn -> String -> Token) -> AlexInput -> Int -> Alex Token
tokAct f (p, _, _, s) _ = return $ f p s

-- |A shorthand action for parsing identifier tokens.
ident :: AlexInput -> Int -> Alex Token
ident (p, _, _, s) len = return $ TIdent p (take len s)

-- |A shorthand action for parsing floating point numbers.
float :: AlexInput -> Int -> Alex Token
float (p, _, _, s) len = return $ TFloat p (read $ take len s)

-- |A shorthand action for parsing integers.
int :: AlexInput -> Int -> Alex Token
int (p, _, _, s) len = return $ TInt p (read $ take len s)

-- |A shorthand action for EOF.
alexEOF :: Alex Token
alexEOF = return $ TEOF (AlexPn 0 0 0)

-- |Invoke the actual lexer and appends the result to the accumulator.
scanTokens' :: [Token] -> Alex [Token]
scanTokens' acc = do
  t <- alexMonadScan
  case t of
    TEOF _ -> return . reverse $ acc
    _      -> scanTokens' (t:acc)

-- |Wraps 'scanTokens'' and starts the lexer with an empty accumulator.
scanTokens :: Alex [Token]
scanTokens = scanTokens' []
}
