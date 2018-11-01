import Lexer
import Parser
import PatternParser
import Text.RawString.QQ

import Control.Monad

prog1 :: String
prog1 = [r|
x : int;
y : float

x = fc(x) + y;
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

patterns :: [String]
patterns = [pat1, pat2, pat3]

main :: IO ()
main = do
  forM_ progs $ \p -> do
    putStrLn "=============="
    putStrLn p
    print . parseProg . alexScanTokens $ p

  forM_ patterns $ \p -> do
    putStrLn "--------------"
    putStrLn p
    print . parseCmdPattern . alexScanTokens $ p
