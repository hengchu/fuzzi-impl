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

/* Extend each row with a constant 1 for bias */
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

/* Compute the gradient from each row */
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

/* Release the size of the db, for normalizing gradients */
size $= lap(1.0, fc(length(db)));

clear(twout, clear_idx);
clear_idx = 0;

/* Project out each column of the gradient, sum up, and release */
clear_idx = 0;
while clear_idx < 101 do
  {
    bmap(dws, dws_j, twout, i, tf_out, { tf_out = twout[clear_idx]; });
    i = 0;
    tf_out = 0.0;
    bsum(dws_j, j_sum, i, tf_out, 1.0);
    j_sum $= lap(1.0, j_sum);

    w[clear_idx] = j_sum / size;
  };
  clear_idx = clear_idx + 1;
end
|]

kmeans :: Prog
kmeans = [prog|
/* input data */
db:[1.0] { [float(5)] };
/* current centroids */
cs: [ [float(5)] ];
/* partitioned input data */
parts: [ { [float(5)] } ];

/* Auxilary variables for the partition extension */
t_in: [float(5)];
i: int;
t_out: int;
t_idx: int;
out_idx: {int};
t_part: {[float(5)]};

/* Temporary variables used in the partition computations */
min_dist: float;
this_dist: float;
clear_idx: int;

k_INFINITY: float;

/* Temporary variables used for computing the centroids */
k : int;
cs_j: [float(5)];
this_partition: { [float(5)] };
this_partition_k: { float };
t_coord_k: float;
t_coord_k_sum: float;
part_size: float

k_INFINITY = 100000000.0;

/* Partition the input database into 3 partitions, same as the number of centroids */
partition(db, parts, t_in, i, t_out, t_idx, out_idx, t_part, 3, {
  min_dist = k_INFINITY;
  this_dist = 0.0;
  t_out = 0;

  clear_idx = 0;
  while clear_idx < 3 do
    {
      this_dist = (cs[clear_idx][0] - t_in[0]) * (cs[clear_idx][0] - t_in[0])
                + (cs[clear_idx][1] - t_in[1]) * (cs[clear_idx][1] - t_in[1])
                + (cs[clear_idx][2] - t_in[2]) * (cs[clear_idx][2] - t_in[2])
                + (cs[clear_idx][3] - t_in[3]) * (cs[clear_idx][3] - t_in[3]);

      if this_dist < min_dist then
        min_dist = this_dist;
        t_out = clear_idx;
      else
        skip;
      end
    };
    clear_idx = clear_idx + 1;
  end;

  min_dist = 0.0;
  this_dist = 0.0;
});


clear_idx = 0;
while clear_idx < 3 do
  {
    this_partition = parts[clear_idx];
    k = 0;
    while k < 5 do
      {
        i = 0;
        bmap(this_partition, this_partition_k, t_in, i, t_coord_k, {t_coord_k = t_in[clear_idx]});
        t_coord_k_sum = 0.0;
        t_coord_k = 0.0;
        i = 0;
        /* Add up all entries in kth coordinate */
        bsum(this_partition_k, t_coord_k_sum, i, t_coord_k, 8.0);

        /* normalize */
        part_size $= lap(1.0, fc(length(this_partition_k)));
        t_coord_k_sum $= lap(1.0, t_coord_k_sum);
        t_coord_k_sum = t_coord_k_sum / part_size;

        cs_j = cs[clear_idx];
        cs_j[k] = t_coord_k_sum;
        cs[clear_idx] = cs_j;
      };
      k = k + 1;
    end
  };
  clear_idx = clear_idx + 1;
end
|]


naiveBayes :: Prog
naiveBayes = [prog|
db:[1.0] { [float(58)] };
all_labels: { float };
label_sum: float;
db_size: float;

positive_features: { float };
negative_features: { float };

positive_feature_sum_j: float;
negative_feature_sum_j: float;

positive_fraction: float;
negative_fraction: float;

theta_positive: [float(57)];
theta_negative: [float(57)];

w: [float(57)];

b_collection: [float(57)];
b: float;

/* Auxilary variables for extensions */
t_in: [float(58)];
i: int;
t_out_label: float;
t_label_sum: float;
j: int;
t_feature: float;
t_sum: float

db_size $= lap(1.0, fc(length(db)));
bmap(db, all_labels, t_in, i, t_out_label, {
  if t_in[57] == 1.0 then
    t_out_label = 1.0;
  else
    t_out_label = 0.0;
  end
});

i = 0;
label_sum = 0.0;
t_label_sum = 0.0;
bsum(all_labels, label_sum, i, t_label_sum, 1.0);
label_sum $= lap(1.0, label_sum);

positive_fraction = label_sum / db_size;
negative_fraction = 1.0 - positive_fraction;

j = 0;
while j < 57 do
  {
    bmap(db, positive_features, t_in, i, t_feature, {
      if t_in[57] == 0.0 then
        t_feature = t_in[j];
      else
        t_feature = 0.0;
      end
    });

    bmap(db, negative_features, t_in, i, t_feature, {
      if t_in[57] == 1.0 then
        t_feature = t_in[j];
      else
        t_feature = 0.0;
      end
    });

    positive_feature_sum_j = 0.0;
    negative_feature_sum_j = 0.0;
    t_sum = 0.0;
    bsum(positive_features, positive_feature_sum_j, i, t_sum, 1.0);
    bsum(negative_features, positive_feature_sum_j, i, t_sum, 1.0);

    positive_feature_sum_j $= lap(1.0, positive_feature_sum_j);
    negative_feature_sum_j $= lap(1.0, negative_feature_sum_j);

    theta_positive[j] = positive_feature_sum_j / label_sum;
    theta_negative[j] = negative_feature_sum_j / label_sum;

    w[j] = log(theta_positive[j] / theta_negative[j])
         - log((1.0 - theta_positive[j]) / (1.0 - theta_negative[j]));

    b_collection[j] = log((1.0 - theta_positive[j]) / (1.0 - theta_negative[j]))
                    - log(negative_fraction / positive_fraction);
  };
  j = j + 1;
end;

b = 0.0;
i = 0;
while i < length(b_collection) do
  b = b + b_collection[i];
end

|]

pate :: Prog
pate = [prog|
/* Input partitions of data points */
db_partitions:[1.0] { { [float(101)] } };
/* Datapoints with extra 1's for biases */
db1_partitions: { { [float(102)] } };

/* The gradients computed from each row in each partition */
dws_partitions: { { [float(101)] } };
/* Model parameters, one for each partition */
ws_partitions: { [float(101)] };

/* initial weights */
w: [float(101)];

/* Auxillary variables used for array mapping the partitions */
t_db: { [float(101)] };
t_db1: { [float(102)] };
i: int;

/* Auxillary variables used for bag mapping inside the array map */
tt_db: [float(101)];
tt_db1 : [float(102)];
j: int;
clear_idx: int;

/* Auxillary variables used for calculating the gradients */
t_dws : { [float(101)] };
t_dws_j : { float };
tt_dws_j : float;
tt_dws : [float(101)];
t_ws: [float(101)];
dt: float;
temp: float;
prob: float;
sc: float;
size: float;
sum: float

/* For each partition, extend each row with a constant 1 for bias */
bmap(db_partitions, db1_partitions, t_db, i, t_db1, {
  bmap(t_db, t_db1, tt_db, j, tt_db1, {
    clear(tt_db1, clear_idx);
    tt_db1[0] = 1.0;
    clear_idx = 0;
    clear_idx = 0;
    while clear_idx < 101 do
      { tt_db1[clear_idx+1] = tt_db[clear_idx]; };
      clear_idx = clear_idx + 1;
    end;
    clear_idx = 0;
  });

  j = 0;
  clear(tt_db, clear_idx);
  clear(tt_db1, clear_idx);
  clear_idx = 0;
});

i = 0;
j = 0;

clear_idx = 0;
clear(tt_db1, clear_idx);

clear_idx = 0;
clear(tt_dws, clear_idx);

/* Compute gradients for each partition */
bmap(db1_partitions, dws_partitions, t_db1, i, t_dws, {
  bmap(t_db1, t_dws, tt_db1, j, tt_dws, {
    clear_idx = 0;
    clear(tt_dws, clear_idx);

    clear_idx = 0;
    while clear_idx < 101 do
      { tt_dws[clear_idx] = tt_db1[clear_idx] };
      clear_idx = clear_idx + 1;
    end;

    dt = dot(tt_dws, w) / 1000.0;
    temp = clip(exp(-1.0 * tt_db1[101] * dt), 10000.0);
    prob = 1.0 / (1.0 + temp);
    sc = (1.0 - prob) * tt_db1[101];
    tt_dws = scale(sc, tt_dws);

    dt = 0.0;
    temp = 0.0;
    prob = 0.0;
    sc = 0.0;
  });

  dt = 0.0;
  temp = 0.0;
  prob = 0.0;
  sc = 0.0;

  j = 0;

  clear_idx = 0;
  clear(tt_db1, clear_idx);

  clear_idx = 0;
  clear(tt_dws, clear_idx);
});

t_dws = {};
i = 0;
j = 0;

/* Compute new weights for each partition */
bmap(dws_partitions, ws_partitions, t_dws, i, t_ws, {
  size = fc(length(t_dws));
  clear_idx = 0;
  clear(t_ws, clear_idx);

  clear_idx = 0;
  while clear_idx < 101 do
    {
      sum = 0.0;
      j = 0;
      t_dws_j = {};
      bmap(t_dws, t_dws_j, tt_dws, j, tt_dws_j, {tt_dws_j = tt_dws[clear_idx]});
      bsum(t_dws_j, sum, j, tt_dws_j, 100000000.0);
      t_ws[clear_idx] = sum / size;
    };
    clear_idx = clear_idx + 1;
  end;

  t_dws_j = {};

  clear_idx = 0;
  clear(tt_dws, clear_idx);

  tt_dws_j = 0.0;
  sum = 0.0;
  j = 0;
  size = 0.0;
  clear_idx = 0;
});
|]

paperExample :: Prog
paperExample = [prog|
/* The input database */
income:[1.0] { float };
/* The partitions of low, middle, high income groups */
income_groups: [ { float }(3) ];

/* Auxillary variables for partition extension */
t_income: float;
i: int;
t_part_idx: int;
part_indices: { int };
t_part: { float };
t_idx: int;

/* Constants */
k_LOW_INCOME_THRESHOLD : float;
k_MID_INCOME_THRESHOLD : float

k_LOW_INCOME_THRESHOLD = 1000.0;
k_MID_INCOME_THRESHOLD = 5000.0;

partition(income, income_groups, t_income, i, t_part_idx, t_idx, part_indices, t_part, 3, {
  if t_income < k_LOW_INCOME_THRESHOLD then
    t_part_idx = 0;
  else
    if t_income < k_MID_INCOME_THRESHOLD then
      t_part_idx = 1;
    else
      t_part_idx = 2;
    end
  end
});
|]

testprog1 :: Prog
testprog1 = [prog|
x :[1.0] [{int}];
y : [{float}];

t_x : {int};
t_y: {float};

tt_x: int;
tt_y: float;

i: int;
j: int

amap(x, y, t_x, i, t_y, {
  bmap(t_x, t_y, tt_x, j, tt_y, {
    tt_y = fc(tt_x);
  });

  tt_x = 0;
  tt_y = 0.0;
  j = 0;
});

|]
