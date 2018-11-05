module Examples where

import Syntax
import PatternQQ

mnist100 :: Prog
mnist100 = [prog|
db:[1.0] { [float(101)] };
db1: { [float(102)] };
trow: [float(101)];
trow1: [float(102)];

w:      [float(101)];
dws: {[float(101)]};
dws_j: {float};
j_sum: float;
tf_out: float;
twout: [float(101)];

i: int;
clear_idx: int;

dt: float;
temp: float;
prob: float;
sc: float;

size: float

bmap(db, db1, trow, i, trow1, {
  clear(trow1, clear_idx);
  trow1[0] = 1.0;
  clear_idx = 0;
  clear_idx = 0;
  while clear_idx < 101 do
    { trow1[clear_idx + 1] = trow[clear_idx] };
    clear_idx = clear_idx + 1;
  end;
  clear_idx = 0;
});

i = 0;
clear(trow, i);
i = 0;
clear(trow1, i);
i = 0;

bmap(db1, dws, trow1, i, twout, {
  clear(twout, clear_idx);
  clear_idx = 0;

  clear_idx = 0;
  while clear_idx < 101 do
    { twout[clear_idx] = trow1[clear_idx] };
    clear_idx = clear_idx + 1;
  end;
  clear_idx = 0;

  dt = dot(twout, w) / 1000.0;
  temp = clip(exp(-1.0 * trow1[101] * dt), 10000.0);
  prob = 1.0 / (1.0 + temp);
  sc = (1.0 - prob) * trow1[101];
  twout = scale(sc, twout);

  dt = 0.0;
  temp = 0.0;
  prob = 0.0;
  sc = 0.0;
  clear_idx = 0;
});

size $= lap(1.0, fc(length(db)));

clear(twout, clear_idx);
clear_idx = 0;

clear_idx = 0;
while clear_idx < 101 do
  {
    bmap(dws, dws_j, twout, i, tf_out, { tf_out = twout[clear_idx]; });
    i = 0;
    tf_out = 0.0;
    bsum(dws_j, j_sum, i, tf_out, 2.0);
    j_sum $= lap(1.0, j_sum);

    w[clear_idx] = j_sum;
  };
  clear_idx = clear_idx + 1;
end
|]

testprog1 :: Prog
testprog1 = [prog|

w: [float(101)];
dt: float;
sc: float;
temp: float;
prob: float;
trow1: [float(102)];
twout: [float(101)]

dt = dot(twout, w) / 1000.0;
temp = clip(exp(-1.0 * trow1[101] * dt), 10000.0);
prob = 1.0 / (1.0 + temp);
sc = (1.0 - prob) * trow1[101];
twout = scale(sc, twout);

|]
