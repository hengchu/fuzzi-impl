Artifact for *Fuzzi: A Three-Level Logic for Differential Privacy*

# Building Fuzzi

There are two options for building Fuzzi: 1). use the provided docker image, or
2). build directly from source.

## Using Docker

1. Install Docker following the [official guide](https://docs.docker.com/install/)
2. Download the image [here](https://drive.google.com/file/d/16JyxDwii9Np3KGBCti95bhp5vSkh2LGV/view?usp=sharing)
3. Start the docker daemon. Docker will ask for your host system credential on first time startup, and it may also show a login UI for dockerhub. However, you do *not* need to login for the following steps
4. Run `docker image load -i fuzzi-artifact.tgz`, this may take a few minutes
5. Run `docker images`, and verify it shows an image with `REPOSITORY fuzzi-impl`
6. Run `docker run --rm -it fuzzi-impl`

This will start a shell at the directory `/tmp/fuzzi-impl`, which holds the
source code and built binaries of `fuzzi`. All following instructions assume
your current working directory is `/tmp/fuzzi-impl`.

## Building on your host system from source

TODO

# Step-by-step guide

We provide a `Makefile` for typechecking, transpiling and running the
evaluations described in the paper. There are 4 evaluation experiments:
1). logistic regression on MNIST, 2). ensemble of logistic regression models on
MNIST, 3). naive bayes on spambase, and 4). kmeans clustering on iris data.

To start, first run `source fuzzi-gen/venv/bin/activate`. This starts a virtual
environment for python code that we will later run. All instructions below
assume your shell is in this virtual environment.

The `Makefile` targets:

- `fuzzi`: builds the `fuzzi` typechecker binary from Haskell source code
- `preprocess`: preprocesses the raw datasets of each evaluation experiment into json formats that will be consumed by the transpiled python code from `fuzzi` programs
- `typecheck`: runs the `fuzzi` typechecker on each evaluation experiment's source code, and prints the type information as a human readable json blob
- `transpile`: runs the `fuzzi` transpiler on each evaluation experiment's source code, and emits the output to proper locations within the `fuzzi-gen` directory; the `fuzzi-gen` directory is a `python3` project that holds the generated code
- `evaluate`: runs each of the evaluate experiment's emitted python code, and print accuracy information to standard out

The docker image has already run the targets `fuzzi`, `preprocess`, and
`transpile`. Feel free to re-run them to compile, preprocess data, and transpile
fuzzi code again, or simply skip them.

Run `make typecheck` to verify that `fuzzi` correctly typechecks each program.

Run `make evaluate` to re-run the evaluation experiments (this takes several
minutes).

## Writing your own `fuzzi` program

Each `fuzzi` program must start with a non-empty type declaration segment, and a
non-empty program body. The most trivial `fuzzi` program is something like this:

```
/* examples/trivial.fuzzi */
types
x :[1.0] float;
end

skip;
```

This program declares variable `x` as a 1-sensitive float, and the program does
nothing with `x`.

Running the typechecker on this program produces the following json output:

```
$ stack exec -- fuzzi -f examples/trivial.fuzzi
{"sensitivities":{"x":1.0},"epsilon":0.0,"delta":0.0}
```

The output contains the sensitivity of all declared program variables, and the
(epsilon, delta) privacy cost of the program.

----

A more slighly more interesting `fuzzi` program performs some arithmetic
operations. Here, we declare 3 program variables `x`, `y`, and `z`. The
variables `x` and `y` are 1- and 2-sensitive respectively, while `z` is not
sensitive.

```
/* examples/arithmetic.fuzzi */
types
x :[1.0] float;
y :[2.0] float;
z : float;
end

z = 2.0 * x;
z = z + y;
```

Running the typechecker on this program produces the following json output:

```
$ stack exec -- fuzzi -I fuzzi-lib/stdexts.fuzzi -f examples/arithmetic.fuzzi
{"sensitivities":{"x":1.0,"y":2.0,"z":4.0},"epsilon":0.0,"delta":0.0}
```

The typechecker automatically infers the sensitivity of $z$ based on the typing
rules described in the paper.

### Running your own `fuzzi` program

We will use `examples/arithmetic.fuzzi` as the running example. To run a `fuzzi`
program, we need to do 2 things:

1. Create a `json` file that contains the initial values program inputs
2. Transpile the fuzzi code into a python3 program

Each `fuzzi` type is mapped to a json value type. The following table gives some
examples of how fuzzi values are represented by json values.

|          | int             | float                     | [int]     | {int}     | (int, float) | {[int]}          |
|----------|-----------------|---------------------------|-----------|-----------|--------------|------------------|
| examples | 1, 2, 3...      | 1.0, 1.1, 2.2...          | [1, 2, 3] | [1, 2, 3] | [1, 2.1]     | [[1, 2], [2, 3]] |
|          | [int(5)]        | [float(5)]                |           |           |              |                  |
| examples | [1, 2, 3, 4, 5] | [1.0, 2.0, 3.0, 4.0, 5.0] |           |           |              |                  |

For `examples/arithmetic.fuzzi`, we create the following file `fuzzi-gen/fuzzi/data/other/arithmetic.json` with contents:

```
{
  "x": 10.0,
  "y": 5.0
}
```

Notice that we did not specify a starting value for `z` since we do not care
what it is. In these cases, we can omit variables with "don't care" values from
the json data file.

To transpile, we run

```
stack exec -- fuzzi -I fuzzi-lib/stdexts.fuzzi \
                    -f examples/arithmetic.fuzzi \
                    -t fuzzi-gen/fuzzi/data/other/arithmetic.json \
                    > fuzzi-gen/fuzzi/generated/arithmetic.py
```

This places the generated python code inside `fuzzi-gen/fuzzi/generated`. We can
run this python code by first re-installing the python project in `fuzzi-gen` with

```
pip3 install --editable fuzzi-gen
```

and then we can start a `python3.7` session, and run the following commands

```
>>> from fuzzi.generated import arithmetic
>>> arithmetic.z
25.0
```

The first line imports the emitted arithmetic code as a python module. The
import statement itself actually executes the arithmetic code as well. Each
`fuzzi` variable can be accessed as the `arithmetic` module's attributes. Here
we checked the value of `z`, which indeed is what we had expected.

## More sophisticated examples that release private information and use extensions

For this example, let's assume we have a 1-sensitive bag of input floats, we
will clip and sum these values up, release the sum as public information. And
just to kick it up a notch, we will repeat this process 100 times using advanced
composition, and write the output from each iteration to an output array.

For actual execution, we will specify a json data file that has inputs as the bag
that contains these 50 floats `[10.0, 11.0, ..., 49.0]`.

```
/* examples/kitchensink.fuzzi */
types
inputs: [1.0] {float};
outputs: [float];
private_sum: float;
public_sum: float;

/* aux variables used by bsum */
i: int;
tin: float;

/* aux variables used by ac */
adv_comp_iter: int;
end

length(outputs) = 100;
ac(adv_comp_iter, 100, 1.0e-6,
  bsum(inputs, private_sum, i, tin, 50.0);
  public_sum $= lap(200.0, private_sum);
  outputs[adv_comp_iter] = public_sum;

  /* clear aux variables */
  private_sum = 0.0;
  tin = 0.0;
  i = 0;
);
```

Running the typechecker produces these type information (the commands after `|` pipe just prettifies the json blob)
```
$ stack exec -- fuzzi -I fuzzi-lib/stdexts.fuzzi -f examples/kitchensink.fuzzi | python3 -m json.tool
{
    "sensitivities": {
        "adv_comp_iter": 0.0,
        "i": 0.0,
        "inputs": 1.0,
        "outputs": 0.0,
        "private_sum": 0.0,
        "public_sum": 0.0,
        "tin": 0.0
    },
    "epsilon": 20.24194,
    "delta": 1e-06
}
```

We transpile the kitchensink code, and re-install the `fuzz-gen` python project code:

```
stack exec -- fuzzi -I fuzzi-lib/stdexts.fuzzi \
                    -f examples/kitchensink.fuzzi \
                    -t fuzzi-gen/fuzzi/data/other/kitchensink.json \
                    > fuzzi-gen/fuzzi/generated/kitchensink.py
pip3 install --editable fuzzi-gen
```

Starting a `python3.7` session, and running the following commands shows us the output array:

```
>>> from fuzzi.generated import kitchensink
>>> kitchensink.outputs
array([1270.34626426, 1285.60710652, 1107.95582968,  864.8935631 ,
        864.8799554 , 1355.74364957, 1318.84538832,  639.5628238 ,
       1211.90239211, 1209.36389526, 1230.78210391, 1448.13753059,
        662.38117227, 1234.63656972, 1320.06414565, 1181.48467053,
        833.33824031, 1229.0333383 , 1286.81541403, 2360.63781974,
        950.55904149, 1370.06107391, 1159.77349716, 1958.03026798,
       1316.17394409, 1184.57899083, 1012.74872581, 1429.73116379,
       1224.65520118, 1197.30157432, 1650.7208213 , 1125.95034336,
       1441.33247842, 1170.98967715, 1011.28084091, 1292.28730032,
        877.92357058, 1835.32165319, 1198.67877942, 1189.09593111,
       1184.15775171,  891.93272606, 1374.13495064, 1274.73404676,
       1081.03236592, 1204.50092898, 1199.73543915, 1381.68728412,
        729.19618247, 1271.69211607,  746.19590015, 1766.99368763,
       1375.31958367, 1029.23592535, 1130.86495208, 1165.16771441,
       1251.61795835, 1361.12772687, 1228.43554069, 1658.09666243,
       1254.1172897 , 1078.10060668, 1194.75272409, 1200.58675122,
       1452.51320408, 1431.62428189, 1529.88418302, 1074.24569801,
       2087.05231285, 1502.79665787,  836.82566772, 1090.07743468,
        724.83372995, 1261.29191586,  915.65062021,  978.97104872,
       1140.7561113 , 1072.01169312, 1170.74188385, 1197.60293344,
       1040.11100636, 1245.00369045, 1248.31624636, 1142.83090714,
       1336.34926141, 1974.37553371, 1097.77988805, 1073.94295111,
        836.18489166, 1220.71915658, 1063.27321974, 1077.67167613,
       1403.98179854, 1925.16622434, 1285.32882336, 1268.03009738,
       1330.54362042, 1184.97042221,  418.27973393, 1230.01235741])
```

Notice that most answers are quite close to the actual answer `1180.0`, thanks
to the fact that we are summing up a non-trivial amount of data. If the input
array only contained a few floats, then the laplace noise added to the
`private_sum` would ruin the utility of the sum.
