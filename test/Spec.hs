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

x = A < D && (exp(jaYfA) && (fc(H)).rRN)
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

prop_roundtrip :: PrettyCmd -> Bool
prop_roundtrip pc@(PrettyCmd c) =
  let pp = show pc
      c' = parseCmd . L.alexScanTokens $ pp
      ppc = show (PrettyCmd c')
  in ppc == pp

prop_exprEquiv_id :: PrettyExpr -> Bool
prop_exprEquiv_id (PrettyExpr e) =
  exprEquiv e e

prop_cmdEquiv_id :: PrettyCmd -> Bool
prop_cmdEquiv_id (PrettyCmd c) =
  cmdEquiv c c

unitTests :: IO ()
unitTests = do
  forM_ progs $ \p -> do
    putStrLn "=============="
    putStrLn p
    let ast = parseProg . L.alexScanTokens $ p
    let pp  = render . prettyCmd $ (getCmd ast)
    putStrLn pp
    let ast2 = parseCmd . L.alexScanTokens $ pp
    when (not $ (getCmd ast) `cmdEquiv` ast2) $ do
      putStrLn "Prettyprinter and parser disagreed!"
      print (getCmd ast)
      print ast2
      exitWith (ExitFailure 1)

  forM_ patterns $ \p -> do
    putStrLn "--------------"
    putStrLn p
    print . parseCmdPattern . PL.alexScanTokens $ p

  forM_ epatterns $ \(p, e) -> do
    putStrLn "**************"
    putStrLn p
    putStrLn e
    let pp = parseExprPattern . PL.alexScanTokens $ p
    let pe = parseExpr . L.alexScanTokens $ e
    print pp
    print pe
    print $ matchExpr pp pe M.empty

isSuccess :: Result -> Bool
isSuccess (Success _ _ _) = True
isSuccess _ = False

main :: IO ()
main = do
  unitTests
  putStrLn ""
  putStrLn "*******************"
  putStrLn "*  QuiCheckTests  *"
  putStrLn "*******************"
  r <- quickCheckWithResult stdArgs{maxSize=10, maxSuccess=1000} prop_roundtrip
  when (not $ isSuccess r) $ do
    putStrLn "-------------------"
    print r
