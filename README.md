# mini-pytype

A minimal, OCaml‑based static type checker for Python (**subset** of PEP 484).

## Features

* Parses Python AST via `py/py2json.py` → OCaml IR with source locations
* Hindley–Milner constraint generation and union‑find unification
* CLI via Dune: single‑file and multi‑file modes
* Built‑in basic types (`int`, `str`, `bool`, `float`, `list`, `dict`, `func`)

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