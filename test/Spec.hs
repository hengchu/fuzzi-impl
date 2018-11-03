import qualified Lexer        as L
import qualified PatternLexer as PL
import Parser
import PatternParser
import Syntax
import Pretty
import Match
import Test.QuickCheck
import Text.RawString.QQ
import qualified Data.Map as M

import Control.Monad
import System.Exit

prog1 :: String
prog1 = [r|
x : int

skip;
skip;
skip
|]

prog2 :: String
prog2 = [r|
x :[1.0] int;
y : float

bmap(x, y);
|]

prog3 :: String
prog3 = [r|
x :[1.0] [int];
y : int

y = x[1];
|]

prog4 :: String
prog4 = [r|
x :[1.0] [int(10)];
y : { abc: int, bcd: float, abcd: [int]}

x = length(x);
|]

prog5 :: String
prog5 = [r|
x : { abc : int, bcd: float }

x.abc = 10;
x = [1, 2, 3, x, x.abc];
x = {1, 2, 3, x, x.abc};
x = x < y;
while x < y do
  x = y;
end;
|]

progs :: [String]
progs = [prog1, prog2, prog3, prog4, prog5]

pat1 :: String
pat1 = [r|
while v(i_) < 10 do
  c(c_)
end
|]

pat2 :: String
pat2 = [r|
x = x + 1;
x = v(x_) + 1;
x = x < 10;
x = v(x_) < 10;

while x < 10 do
  x = x + 1;
end;

while v(x_) < 10 do
  v(x_) = v(x_) + 1;
end
|]

pat3 :: String
pat3 = [r|
x = x + 1;

while v(x) > iesc(y) do
  x = x + x;
end;

x = 0;
|]

epat1 :: String
epat1 = [r|
v(x_) + v(y_) + e(z_)
|]

e1 :: String
e1 = "x + y + z"

epat2 :: String
epat2 = [r|
v(x_) + v(y_) + iesc(z_)
|]

e2 :: String
e2 = "x + y + 10"

epat3 :: String
epat3 = [r|
e(x_) + e(z_)
|]

e3 :: String
e3 = "x * y + 10"

e4 :: String
e4 = "x + y * 10"

epatterns :: [(String, String)]
epatterns = [(epat1, e1), (epat2, e2), (epat3, e3), (epat3, e4)]

patterns :: [String]
patterns = [pat1, pat2, pat3]

prop_roundtripExpr :: PrettyExpr -> Bool
prop_roundtripExpr pe@(PrettyExpr e) =
  let pp = render $ prettyExpr e 0
  in case (parse parseExpr pp) of
       Left err -> False
       Right e' -> e == e'

prop_roundtrip :: PrettyCmd -> Bool
prop_roundtrip pc@(PrettyCmd c) =
  let pp = render . prettyCmd $ c
  in case (parse parseCmd pp) of
       Left err -> False
       Right c' -> normalizeSeq c == c'

isSuccess :: Result -> Bool
isSuccess (Success _ _ _) = True
isSuccess _ = False

testprog = [r|
skip;
skip;
skip
|]

main :: IO ()
main = do
  -- unitTests
  putStrLn ""
  putStrLn "*******************"
  putStrLn "*  QuiCheckTests  *"
  putStrLn "*******************"
  r <- quickCheckWithResult
         stdArgs{maxSize=15, maxSuccess=1000, chatty=True, maxShrinks=100}
         prop_roundtrip
  when (not $ isSuccess r) $ do
    putStrLn "-------------------"
    forM_ (failingTestCase r) $ \failedCase -> do
      case parse parseCmd failedCase of
        Left err -> putStrLn $ "Parse error: " ++ err
        Right c -> do
          let t = show (PrettyCmd c)
          case parse parseCmd t of
            Left err -> putStrLn $ "Parse error: " ++ err
            Right c2 -> do
              putStr "prop_roundtrip = "
              print $ prop_roundtrip (PrettyCmd c)
              putStr "Equivalence testing = "
              print $ c == c2
    putStrLn "-------------------"
    print r
    exitWith (ExitFailure 1)
