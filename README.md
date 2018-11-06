# Fuzzi [![Build Status](https://travis-ci.org/hengchu/fuzzi-impl.svg?branch=master)](https://travis-ci.org/hengchu/fuzzi-impl)

### Install dependencies

I recommend using `virtualenvwrapper` to setup an isolated sandbox that doesn't
interfere with global python packages.

```python
pip install -r requirements.txt
```

### Transpile a fuzzi program into python:

```bash
stack exec -- fuzzi -f testdata/new/mnist100.fuzzi -t testdata/mnist.json > mnist100.py
```

Now, you can run this program through `python mnist100.py`, or you can import it
into another python script by doing

```python
import mnist100
```

If the fuzzi program defines a variable called `w`, you can access the output of
this variable in the python script with

```python
import mnist100

print(mnist100.w)
```
