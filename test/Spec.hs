import qualified Lexer        as L
import qualified PatternLexer as PL
import qualified Parser       as P
import qualified PatternParser as PP
import Syntax
import Pretty
import Match
import Expand
import ShapeChecker
import SensitivityChecker
import Test.QuickCheck
import Test.QuickCheck.Test
import Text.RawString.QQ
import PatternQQ
import qualified Data.Map as M
import Data.Either.Combinators

import Control.Lens
import Control.Lens.Tuple
import Control.Monad
import System.Exit
import Debug.Trace

import Examples

cpat1 :: CmdPattern
cpat1 = [cpat|
while v(x) + 1 do
  c(c1);
end
|]

prog1 :: Cmd
prog1 =  [cmd|
bmap(in, out, t_in, i, t_out, t_out = t_in);
bmap(in2, out2, t_in2, i2, t_out2, t_out2 = t_in2)
|]

prog1' :: Cmd
prog1' = [cmd|
i = 0;
out = {};
length(out) = length(in);
while i < length(in) do
  t_in = (in)[i];
  { t_out = t_in
  };
  (out)[i] = t_out;
  i = i + 1;
end;
i2 = 0;
out2 = {};
length(out2) = length(in2);
while i2 < length(in2) do
  t_in2 = (in2)[i2];
  { t_out2 = t_in2
  };
  (out2)[i2] = t_out2;
  i2 = i2 + 1;
end
|]

prog2 :: Cmd
prog2 = [cmd|
amap(in, out, t_in, i, t_out, t_out = t_in);
amap(in2, out2, t_in2, i2, t_out2, t_out2 = t_in2)
|]

prog2' :: Cmd
prog2' = [cmd|
i = 0;
out = [];
length(out) = length(in);
while i < length(in) do
  t_in = (in)[i];
  { t_out = t_in
  };
  (out)[i] = t_out;
  i = i + 1;
end;
i2 = 0;
out2 = [];
length(out2) = length(in2);
while i2 < length(in2) do
  t_in2 = (in2)[i2];
  { t_out2 = t_in2
  };
  (out2)[i2] = t_out2;
  i2 = i2 + 1;
end
|]

prog3 :: Cmd
prog3 = [cmd|
partition(in, out, t_in, idx, t_out, t_idx, out_idx, t_part, 10, skip);
|]

prog3' :: Cmd
prog3' = [cmd|
idx = 0;
length(out) = 10;
while idx < 10 do
  t_part = (out)[idx];
  length(t_part) = 0;
  (out)[idx] = t_part;
  idx = idx + 1;
end;
idx = 0;
out_idx = {};
length(out_idx) = length(in);
while idx < length(in) do
  t_in = (in)[idx];
  { skip
  };
  (out_idx)[idx] = t_out;
  idx = idx + 1;
end;
while idx < length(out_idx) do
  t_idx = (out_idx)[idx];
  if 0 <= t_idx && t_idx < length(out_idx) then
    t_part = (out)[t_idx];
    length(t_part) = length(t_part) + 1;
    (t_part)[length(t_part) - 1] = (in)[idx];
    (out)[t_idx] = t_part;
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
  bmap(t_in, t_out, t_in', idx', t_out',
    t_out' = t_in'
  );
});
|]

prog5' :: Cmd
prog5' = [cmd|
idx = 0;
out = {};
length(out) = length(in);
while idx < length(in) do
  t_in = (in)[idx];
  { { idx' = 0;
      t_out = {};
      length(t_out) = length(t_in);
      while idx' < length(t_in) do
        t_in' = (t_in)[idx'];
        { t_out' = t_in'
        };
        (t_out)[idx'] = t_out';
        idx' = idx' + 1;
      end
    }
  };
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

prop_isCompatRefl :: Tau -> Bool
prop_isCompatRefl t = isCompat t t

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

prog6 :: Prog
prog6 = [prog|
x : int

x = x + 1;
|]

expandProg :: Prog -> Prog
expandProg (Prog decls c) = Prog decls (desugarFix c fuzziExpansionRules)

prog7 :: Prog
prog7 = [prog|
in :[1.0] {int};
out : {float};
idx : int;
t_in : int;
t_out : float

bmap(in, out, t_in, idx, t_out, { t_out = fc(t_in); });
|]

prog8 :: Prog
prog8 = [prog|
in :[1.0] {bool};
out : [{bool}];
t_in : bool;
idx : int;
t_out : int;
t_idx : int;
out_idx : {int};
t_part : {bool}

partition(in, out, t_in, idx, t_out, t_idx, out_idx, t_part, 10, {
  if t_in then
    t_out = 1;
  else
    t_out = 0;
  end
});
|]

prog9 :: Prog
prog9 = [prog|
in :[1.0] {float};
out : float;
idx : int;
t_in : float

bsum(in, out, idx, t_in, 5.0);
|]

prog10 :: Prog
prog10 = [prog|
x :[1.0] float;
y :[1.0] int;
z : float

x = x + x * 1.5;
z = 2.0 * fc(y);
|]

prog11 :: Prog
prog11 = [prog|
x : bool;
y : bool;
n :[1.0] int;
z : [int(5)]

x = x && y;
z = [1, 2, 3, n, 5];
|]

prog12 :: Prog
prog12 = [prog|
x : [int];
y : {int};
z :[1.0] int

x[0] = z;
y[0] = z;
|]

prog13 :: Prog
prog13 = [prog|
x :[1.0] [int]

length(x) = 10;
|]

prog14 :: Prog
prog14 = [prog|
x :[1.0] [int];
y :[1.0] int

length(x) = y;
|]

prog15 :: Prog
prog15 = [prog|
x :[1.0] {int};
y : int

length(x) = y;
|]

prog16 :: Prog
prog16 = [prog|
x :[1.0] float

x $= lap(2.0, x);
|]

prog17 :: Prog
prog17 = [prog|
x :[1.0] float;
y :[2.0] float

x $= lap(2.0, x);
y $= lap(2.0, y)
|]

prog18 :: Prog
prog18 = [prog|
x : int

while x < 10 do
  { x = x + 1; }
end
|]

prog19 :: Prog
prog19 = [prog|
x :[1.0] int;
y : int

if y > 0 then
  { x = 2 * x; }
else
  { x = 3 * x; }
end
|]

prog20 :: Prog
prog20 = [prog|
x :[1.0] float;
y : int

if y > 0 then {
  x = 2.0 * x;
} else {
  x = 3.0 * x;
  x $= lap(1.0, x);
} end
|]

prog21 :: Prog
prog21 = [prog|
x :[1.0] {float};
y : {float};
t_in : float;
i : int;
t_out : float

bmap(x, y, t_in, i, t_out, { t_out = 2.0 * t_in; });
|]

prog22 :: Prog
prog22 = [prog|
x : [1.0] {{float}};
y : {{float}};
t_in : {float};
i : int;
t_out : {float};

t_in2 : float;
i2 : int;
t_out2 : float

bmap(x, y, t_in, i, t_out, {
    bmap(t_in, t_out, t_in2, i2, t_out2, {
      t_out2 = t_in2 * 2.0;
    });

    t_in2 = 0.0;
    i2 = 0;
    t_out2 = 0.0
});
|]

prog23 :: Prog
prog23 = [prog|
x :[1.0] [[float]];
y : [[float]];
t_in : [float];
i : int;
t_out : [float];

t_in2 : float;
i2 : int;
t_out2 : float

amap(x, y, t_in, i, t_out, {
    amap(t_in, t_out, t_in2, i2, t_out2, {
      t_out2 = t_in2 * 2.0;
    });

    t_in2 = 0.0;
    i2 = 0;
    t_out2 = 0.0
});
|]

prog24 :: Prog
prog24 = [prog|
x :[1.0] {float};
y : {float}

x = {};
length(x) = length(y)
|]

prog25 :: Prog
prog25 = [prog|
x : [float];
y : [float]

length(x) = length(y)
|]

prog26 :: Prog
prog26 = [prog|
x :[1.0] float;
y : float

{ y = x * 2.0 }
|]

prog27 :: Prog
prog27 = [prog|
x : [1.0] {{{float}}};
y : {{{float}}};
t_in : {{float}};
i : int;
t_out : {{float}};

t_in2 : {float};
i2 : int;
t_out2 : {float};

t_in3 : float;
i3 : int;
t_out3 : float

bmap(x, y, t_in, i, t_out, {
    bmap(t_in, t_out, t_in2, i2, t_out2, {
      bmap(t_in2, t_out2, t_in3, i3, t_out3, {
        t_out3 = t_in3 * 2.0;
      });

      t_in3 = 0.0;
      i3 = 0;
      t_out3 = 0.0
    });

    t_in3 = 0.0;
    i3 = 0;
    t_out3 = 0.0;

    t_in2 = {};
    i2 = 0;
    t_out2 = {}
});
|]

prog28 :: Prog
prog28 = [prog|
x :[1.0] {float};
y : [{float}];
t_in: float;
idx : int;
t_out: int;
t_idx: int;
out_idx: {int};
t_part:{float}

partition(x, y, t_in, idx, t_out, t_idx, out_idx, t_part, 2, {
  if t_in > 5.0 then
    t_out = 0;
  else
    t_out = 1;
  end
});
|]

prog29 :: Prog
prog29 = [prog|
x :[1.0] {float};
y : float;
i : int;
t_in: float

bsum(x, y, i, t_in, 2.0);
|]

prog30 :: Prog
prog30 = [prog|
x :[1.0] float;
y : float;
i : int

i = 0;
while i < 100 do
  { y $= lap(1.0, x); };
  i = i + 1
end
|]

expectMaxEps :: Float -> Prog -> Bool
expectMaxEps eps p =
  not . null $ filter (\es -> es ^._1 <= eps) (runSensitivityCheckerIgnoreError p 10000)

expectMaxSens' :: Var -> Float -> SContext -> Bool
expectMaxSens' x s (SContext sctx) =
  case sctx ^. (at x) of
    Nothing -> False
    Just s' -> s' <= s

expectMaxSens :: [(Var, Float)] -> Prog -> Bool
expectMaxSens xs p = any id $ do
  (_, sctx) <- runSensitivityCheckerIgnoreError p 10000
  return $ all id $ map (\(x, s) -> expectMaxSens' x s sctx) xs

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
  assert "prog6 shape checks"
    $ isRight . runShapeChecker $ prog6
  assert "prog7 shape checks"
    $ isRight . runShapeChecker
    $ expandProg prog7
  assert "prog8 shape checks"
    $ isRight . runShapeChecker
    $ expandProg prog8
  assert "prog9 shape checks"
    $ isRight . runShapeChecker
    $ expandProg prog9
  assert "prog10 sens checks"
    $ expectMaxEps 0 prog10
      && expectMaxSens [("x", 2.5), ("y", 1.0), ("z", 2.0)] prog10
  assert "prog12 sens checks"
    $ expectMaxEps 0 prog12
      && expectMaxSens [("x", 1), ("y", 2)] prog12
  assert "prog14 should not sens check"
    $ (not $ expectMaxEps 0 prog14)
      && (not $ expectMaxSens [] prog14)
  assert "prog15 should sens check"
    $ expectMaxEps 0 prog15
  assert "prog16 should sens check"
    $ expectMaxEps 0.5 prog16
    && expectMaxSens [("x", 0)] prog16
  assert "prog17 should sens check"
    $ expectMaxEps 1.5 prog17
    && expectMaxSens [("x", 0), ("y", 0)] prog17
  assert "prog18 should sens check"
    $ expectMaxEps 0 prog18
    && expectMaxSens [("x", 0)] prog18
  assert "prog19 should sens check"
    $ expectMaxEps 0 prog19
    && expectMaxSens [("x", 3)] prog19
  assert "prog20 should sens check"
    $ expectMaxEps 3 prog20
    && expectMaxSens [("x", 2)] prog20
  assert "prog21 should sens check"
    $ expectMaxEps 0 (expandProg prog21)
    && expectMaxSens [("y", 1)] (expandProg prog21)
  assert "prog22 should sens check"
    $ expectMaxEps 0 (expandProg prog22)
    && expectMaxSens [("y", 1)] (expandProg prog22)
  assert "prog23 should sens check"
    $ expectMaxEps 0 (expandProg prog23)
    && expectMaxSens [("y", 2)] (expandProg prog23)
  assert "prog24 should sens check"
    $ expectMaxEps 0 (expandProg prog24)
    && expectMaxSens [("y", 0), ("x", 0)] (expandProg prog24)
  assert "prog27 should sens check"
    $ expectMaxEps 0 (expandProg prog27)
    && expectMaxSens [("y", 1)] (expandProg prog27)
  assert "prog28 should sens check"
    $ expectMaxEps 0 (expandProg prog28)
    && expectMaxSens [("x", 1), ("y", 1)] (expandProg prog28)
  assert "prog29 should sens check"
    $ expectMaxEps 0 (expandProg prog29)
    && expectMaxSens [("x", 1), ("y", 2)] (expandProg prog29)
  assert "prog30 should sens check"
    $ expectMaxEps 100 (expandProg prog30)
    && expectMaxSens [("x", 1), ("y", 0)] (expandProg prog30)
  assert "mnist100 should sens check"
    $ expectMaxEps 102 (expandProg mnist100)
    && expectMaxSens [("j_sum", 0)] (expandProg mnist100)
  assert "kmeans should sens check"
    $ expectMaxEps 135 (expandProg kmeans)
    && expectMaxSens [("cs", 0)] (expandProg kmeans)
  assert "naive bayes should sens check"
    $ expectMaxEps 59 (expandProg naiveBayes)
    && expectMaxSens [("b", 0)] (expandProg naiveBayes)

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
  let args = stdArgs{maxSize=10, maxSuccess=500, chatty=True, maxShrinks=100}
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
       .&&. prop_isCompatRefl
  unless (isSuccess r) exitFailure
