# mini-pytype

A minimal, OCaml‑based static type checker for Python (**subset** of PEP 484).

## Features

* Parses Python AST via `py/py2json.py` → OCaml IR
* Hindley–Milner constraint generation and union‑find unification
* CLI via Dune: single‑file and multi‑file modes
- Supports :
    - literals (int, float, bool, string), variables, binary operations
    - function calls, attribute access, lists, dicts
    - PEP 484 annotations (function signatures, generics like list[T]/dict[K,V])

## Prerequisites

* OCaml ≥ 5.0, OPAM
* Dune build system
* Python 3 (to run `py2json.py`)

## Quickstart

```bash
# Clone & install dependencies
git clone <repo> && cd mini-pytype
opam install . --deps-only -y

# Build
dune build

# Run type checker on a file (inline errors)
dune exec -- mini-pytype src/example.py
```

## Testing

```bash
dune runtest
```

# Demo Run

### Contents of test_error.py
```python
def f(x: int) -> int:
    return x

# Intentional type mismatch:
f("oops")
```

### Which returns
```bash
dune exec -- mini-pytype demo/test_error.py

[DEBUG] param x : int               
[DEBUG] return : int
error: Argument type mismatch: expected int but got str
1 error(s)
❌  cannot unify str with int
```

