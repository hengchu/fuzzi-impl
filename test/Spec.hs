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
import PatternQQ
import qualified Data.Map as M

import Control.Monad
import System.Exit
import Debug.Trace

cpat1 :: CmdPattern
cpat1 = [cpat|
while v(x) + 1 do
  c(c1);
end
|]

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
  in matches ep e

prop_cmdMatchSelf :: PrettyCmd -> Bool
prop_cmdMatchSelf (PrettyCmd c) =
  let cp = cmdToPattern c
  in matches cp c

prop_matchingPairMatches :: (Matching a) => MatchingPair a -> Bool
prop_matchingPairMatches (MatchingPair v p) =
  matches p v

prop_closedPatterns :: forall a. (Matching a) => a -> Bool
prop_closedPatterns v =
  (closed @a) . (pattern @a) $ v

prop_valuePatternRecovers :: forall a. (Matching a, Eq a) => a -> Bool
prop_valuePatternRecovers v =
  case recover @a (pattern @a v) of
    Nothing -> False
    Just v' -> v == v'

prop_matchingPairRecovers :: forall a. (Matching a, Eq a) => MatchingPair a -> Bool
prop_matchingPairRecovers (MatchingPair v p) =
  case tryUnify p v of
    Nothing -> False
    Just env ->
      case subst @a p env of
        Nothing -> False
        Just p' ->
          case recover @a p' of
            Nothing -> False
            Just v' -> v == v'

main :: IO ()
main = do
  putStrLn ""

  putStrLn "    ______                _ "
  putStrLn "   / ____/_  __________  (_)"
  putStrLn "  / /_  / / / /_  /_  / / / "
  putStrLn " / __/ / /_/ / / /_/ /_/ /  "
  putStrLn "/_/    \\__,_/ /___/___/_/   "

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
       .&&. (prop_matchingPairMatches @Cmd)
       .&&. (prop_matchingPairMatches @Expr)
       .&&. (prop_closedPatterns @Cmd)
       .&&. (prop_closedPatterns @Expr)
       .&&. (prop_valuePatternRecovers @Cmd)
       .&&. (prop_valuePatternRecovers @Expr)
       .&&. (prop_matchingPairRecovers @Cmd)
       .&&. (prop_matchingPairRecovers @Expr)
  unless (isSuccess r) exitFailure
