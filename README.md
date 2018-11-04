# Fuzzi [![Build Status](https://travis-ci.org/hengchu/fuzzi-impl.svg?branch=master)](https://travis-ci.org/hengchu/fuzzi-impl)

### Install dependencies

I recommend using `virtualenvwrapper` to setup an isolated sandbox that doesn't
interfere with global python packages.

```python
pip install -r requirements.txt
```

### Transpile a fuzzi program into python:

```bash
stack exec -- fuzzi --file program.fuzzi --transpile --interp input.json > program.py
```

Now, you can run this program through `python program.py`, or you can import it
into another python script by doing

```python
import program
```

If the fuzzi program defines a variable called `w`, you can access the output of
this variable in the python script with

```python
import program

print(program.w)
```
