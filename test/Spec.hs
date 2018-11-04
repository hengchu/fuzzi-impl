import qualified Lexer        as L
import qualified PatternLexer as PL
import qualified Parser       as P
import qualified PatternParser as PP
import Syntax
import Pretty
import Match
import Expand
import Test.QuickCheck
import Test.QuickCheck.Test
import Text.RawString.QQ
import PatternQQ
import qualified Data.Map as M
import Data.Either.Combinators

import Control.Monad
import System.Exit
import Debug.Trace

cpat1 :: CmdPattern
cpat1 = [cpat|
while v(x) + 1 do
  c(c1);
end
|]

prog1 :: Cmd
prog1 =  [cmd|
bmap(in, out, t_in, i, t_out, { t_out = t_in; });
bmap(in2, out2, t_in2, i2, t_out2, { t_out2 = t_in2; })
|]

prog1' :: Cmd
prog1' = [cmd|
i = 0;
length(out) = length(in);
while i < length(in) do
  t_in = (in)[i];
  t_out = t_in;
  (out)[idx] = t_out;
  i = i + 1;
end;
i2 = 0;
length(out2) = length(in2);
while i2 < length(in2) do
  t_in2 = (in2)[i2];
  t_out2 = t_in2;
  (out2)[idx] = t_out2;
  i2 = i2 + 1;
end
|]

prog2 :: Cmd
prog2 = [cmd|
amap(in, out, t_in, i, t_out, { t_out = t_in; });
amap(in2, out2, t_in2, i2, t_out2, { t_out2 = t_in2; })
|]

prog2' :: Cmd
prog2' = [cmd|
i = 0;
length(out) = length(in);
while i < length(in) do
  t_in = (in)[i];
  t_out = t_in;
  (out)[idx] = t_out;
  i = i + 1;
end;
i2 = 0;
length(out2) = length(in2);
while i2 < length(in2) do
  t_in2 = (in2)[i2];
  t_out2 = t_in2;
  (out2)[idx] = t_out2;
  i2 = i2 + 1;
end
|]

prog3 :: Cmd
prog3 = [cmd|
partition(in, out, t_in, idx, t_out, t_idx, out_idx, 10, { skip; });
|]

prog3' :: Cmd
prog3' = [cmd|
idx = 0;
length(out) = 10;
while idx < 10 do
  length((out)[idx]) = 0;
  idx = idx + 1;
end;
idx = 0;
length(out_idx) = length(in);
while idx < length(in) do
  t_in = (in)[idx];
  skip;
  (out_idx)[idx] = t_out;
  idx = idx + 1;
end;
while idx < length(out_idx) do
  t_idx = (out_idx)[idx];
  if 0 <= t_idx && t_idx < length(out_idx) then
    length((out)[t_idx]) = length((out)[t_idx]) + 1;
    ((out)[t_idx])[length((out)[t_idx]) - 1] = (in)[idx];
  else
    skip;
  end;
  idx = idx + 1;
end
|]

prog4 :: Cmd
prog4 = [cmd|
bsum(in, out, idx, temp, 10.0);
|]

prog4' :: Cmd
prog4' = [cmd|
idx = 0;
out = 0.0;
while idx < length(in) do
  temp = (in)[idx];
  if temp < 10.0 then
    out = out - 10.0;
  else
    if temp > 10.0 then
      out = out + 10.0;
    else
      out = out + temp;
    end;
  end;
  idx = idx + 1;
end
|]

prog5 :: Cmd
prog5 = [cmd|
bmap(in, out, t_in, idx, t_out, {
  bmap(t_in, t_out, t_in', idx', t_out', {
    t_out' = t_in';
  });
});
|]

prog5' :: Cmd
prog5' = [cmd|
idx = 0;
length(out) = length(in);
while idx < length(in) do
  t_in = (in)[idx];
  idx' = 0;
  length(t_out) = length(t_in);
  while idx' < length(t_in) do
    t_in' = (t_in)[idx'];
    t_out' = t_in';
    (t_out)[idx] = t_out';
    idx' = idx' + 1;
  end;
  (out)[idx] = t_out;
  idx = idx + 1;
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

prop_matchingCmdPairShouldMatchAsPrefix :: MatchingPair Cmd -> Bool
prop_matchingCmdPairShouldMatchAsPrefix (MatchingPair c cp) =
  case runUniM $ matchCmdPrefix cp c of
    Right (_, _) -> True
    _ -> False

prop_matchingPrefixRecover :: MatchingPrefixCmdPair -> Bool
prop_matchingPrefixRecover (MatchingPrefixCmdPair c cp) =
  case runUniM $ matchCmdPrefix cp c of
    Right (env, Nothing) ->
      case subst @Cmd cp env of
        Nothing -> False
        Just cp' -> case recover @Cmd cp' of
                      Nothing -> False
                      Just c' -> c == c'
    Right (env, Just remains) ->
      case subst @Cmd cp env of
        Nothing -> False
        Just cp' -> case recover @Cmd cp' of
                      Nothing -> False
                      Just c' -> c == (normalizeSeq $ CSeq trivialPosition c' remains)
    _ -> False

prop_matchAnywhere :: MatchingAnywhereCmdPair -> Bool
prop_matchAnywhere (MatchingAnywhereCmdPair c cp) =
  case runUniM $ matchCmdAnywhere cp c of
    Left _ -> False
    Right (env, hole) ->
      case subst @Cmd cp env of
        Just cp' ->
          case recover @Cmd cp' of
            Nothing -> False
            Just c' -> (normalizeSeq c) == (normalizeSeq $ hole c')
        Nothing -> False

isSeqRightAssociative :: Cmd -> Bool
isSeqRightAssociative (CSeq _ (CSeq _ _ _) _) = False
isSeqRightAssociative (CSeq _ c1 c2) =
  isSeqRightAssociative c1 && isSeqRightAssociative c2
isSeqRightAssociative (CIf p _ c1 c2) =
  isSeqRightAssociative c1 && isSeqRightAssociative c2
isSeqRightAssociative (CWhile _ e c) =
  isSeqRightAssociative c
isSeqRightAssociative _ = True

prop_normalizeSeq :: Cmd -> Bool
prop_normalizeSeq c = isSeqRightAssociative $ normalizeSeq c

testprog :: Cmd
testprog = [cmd|
NBh({ skip
    }, log(pi));
clip(Ezx * Gq, 0) == (dot(LDK, bn))[clip(tVD, [])] = clip(scale(log(I), scale(wVt, fL)), -3);
skip;
N(oC, { skip
      }, y);
Y(GP, { skip
      })
|]

testprogpat :: CmdPattern
testprogpat = [cpat|
skip
|]

assert :: String -> Bool -> IO ()
assert label cond = do
  putStrLn label
  when (not $ cond) $ do
    putStrLn "Failed!"
    exitFailure

unitTests :: IO ()
unitTests = do
  assert "prog1 expands correctly"
    $ normalizeSeq (desugarFix prog1 fuzziExpansionRules) == prog1'
  assert "prog2 expands correctly"
    $ normalizeSeq (desugarFix prog2 fuzziExpansionRules) == prog2'
  assert "prog3 expands correctly"
    $ normalizeSeq (desugarFix prog3 fuzziExpansionRules) == prog3'
  assert "prog4 expands correctly"
    $ normalizeSeq (desugarFix prog4 fuzziExpansionRules) == prog4'
  assert "prog5 expands correctly"
    $ normalizeSeq (desugarFix prog5 fuzziExpansionRules) == prog5'

main :: IO ()
main = do
  putStrLn ""

  putStrLn "    ______                _ "
  putStrLn "   / ____/_  __________  (_)"
  putStrLn "  / /_  / / / /_  /_  / / / "
  putStrLn " / __/ / /_/ / / /_/ /_/ /  "
  putStrLn "/_/    \\__,_/ /___/___/_/   "

  putStrLn "*********************"
  putStrLn "*     UnitTests     *"
  putStrLn "*********************"
  unitTests

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
       .&&. prop_matchingCmdPairShouldMatchAsPrefix
       .&&. prop_matchingPrefixRecover
       .&&. prop_matchAnywhere
       .&&. prop_normalizeSeq
       .&&. prop_matchingPrefixRecover
  unless (isSuccess r) exitFailure
