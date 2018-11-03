import qualified Lexer        as L
import qualified PatternLexer as PL
import qualified Parser       as P
import qualified PatternParser as PP
import Syntax
import Pretty
import Match
import Test.QuickCheck
import Test.QuickCheck.Test
import Text.RawString.QQ
import qualified Data.Map as M

import Control.Monad
import System.Exit
import Debug.Trace

prop_roundtripExpr :: PrettyExpr -> Bool
prop_roundtripExpr pe@(PrettyExpr e) =
  let pp = render $ prettyExpr e 0
  in case (P.parse P.parseExpr pp) of
       Left err -> False
       Right e' -> e == e'

prop_roundtripExprPattern :: PrettyExprPattern -> Bool
prop_roundtripExprPattern pe@(PrettyExprPattern e) =
  let pp = render $ prettyExprPattern e 0
  in case (PP.parse PP.parseExprPattern pp) of
       Left err -> False
       Right e' -> e == e'

prop_roundtrip :: PrettyCmd -> Bool
prop_roundtrip pc@(PrettyCmd c) =
  let pp = render . prettyCmd $ c
  in case (P.parse P.parseCmd pp) of
       Left err -> False
       Right c' -> normalizeSeq c == c'

prop_roundtripPattern :: PrettyCmdPattern -> Bool
prop_roundtripPattern pc@(PrettyCmdPattern c) =
  let pp = render . prettyCmdPattern $ c
  in case (PP.parse PP.parseCmdPattern pp) of
       Left err -> False
       Right c' -> normalizeSeqPattern c == c'

prop_exprMatchSelf :: PrettyExpr -> Bool
prop_exprMatchSelf (PrettyExpr e) =
  let ep = exprToPattern e
  in matchExprBool ep e

prop_cmdMatchSelf :: PrettyCmd -> Bool
prop_cmdMatchSelf (PrettyCmd c) =
  let cp = cmdToPattern c
  in matchCmdBool cp c

main :: IO ()
main = do
  putStrLn ""
  putStrLn "*********************"
  putStrLn "*  QuickCheckTests  *"
  putStrLn "*********************"
  let args = stdArgs{maxSize=15, maxSuccess=500, chatty=True, maxShrinks=100}
  r <- quickCheckWithResult args
       $ prop_roundtrip
       .&&. prop_roundtripExpr
       .&&. prop_roundtripPattern
       .&&. prop_roundtripExprPattern
       .&&. prop_exprMatchSelf
       .&&. prop_cmdMatchSelf
  unless (isSuccess r) exitFailure
