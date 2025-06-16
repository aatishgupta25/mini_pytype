def f(x: int) -> int:
    return x * 2

def g(y: int) -> int:
    return f(y) + 1

z: int = g(f(3))
