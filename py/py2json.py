#!/usr/bin/env python3
"""
py2json.py  –  helper invoked by OCaml code.

Why?
• Writing a full Python parser in OCaml would blow the 30-hour budget.
• CPython’s stdlib already yields an AST → we just dump it as JSON.

Design choices:
• We emit *verbatim* field names from `ast.AST`; OCaml side does the mapping.
• Only pure-stdlib; zero external dependencies.
"""
import ast, json, sys

def to_json(node):
    """Recursively turn ast.AST → plain dict/list structures."""
    if isinstance(node, ast.AST):
        fields = {k: to_json(v) for k, v in ast.iter_fields(node)}
        return {"_type": node.__class__.__name__, **fields}
    if isinstance(node, list):
        return [to_json(x) for x in node]
    # leaf: constant, None, str, int …
    return node

if __name__ == "__main__":
    if len(sys.argv) != 2:
        sys.exit("usage: py2json.py <file.py>")
    src_path = sys.argv[1]
    tree = ast.parse(open(src_path, encoding="utf-8").read(), filename=src_path)
    json.dump(to_json(tree), sys.stdout)
