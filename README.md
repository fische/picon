# Picon

Picon is a Python-to-Cython tool. It aims to produce cython code as close as
possible to the original python code. It supports versions 2.x and 3.x of
Python.

## Prerequisites

* Glasgow Haskell Compiler 8.0.2 or later.
* Cabal 1.10 or later.

## Installation

```shell
cabal install picon
```

## Usage

```
picon - a Python-to-Cython tool

Usage: picon [-o|--output TARGET_DIR] ([-2] | [-3]) FILE
  Convert python code from FILE to cython code

Available options:
  -o,--output TARGET_DIR   Target directory where to write translated files
  -2                       Enable Python version 2 parsing
  -3                       Enable Python version 3 parsing
  -h,--help                Show this help text
```

## Quick start

Write your python code:

```python
def fib(n):
    if n < 2:
        return n
    return fib(n-2) + fib(n-1)

fib(30)
```

Convert it to cython code:

```shell
picon /path/to/your/file.py
```

Picon will generate the cython code at `/path/to/your/file.pyx`:

```cython
cdef int fib(int n):
    if n < 2:
        return n
    return fib(n - 2) + fib(n - 1)
fib(30)
```

## Limitations

The following list provide the current limitations of picon. This does not mean
they won't be supported in the future.

- Modules.
- Built-in such as print, range, str, etc. Any variable that have not been
  declared in the current module.
- List, dict and tuple.
- C type semantic (overflows, etc).
- Variables holding multiple types.
- Function pointers.
- Special methods such as \_\_init\_\_, \_\_str\_\_, \_\_radd\_\_, etc.
- Nested functions.
