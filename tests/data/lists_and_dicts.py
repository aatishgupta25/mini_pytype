def first(xs: list[int]) -> int:
    return xs[0]

def count_keys(d: dict[str, int]) -> int:
    return len(d)

lst: list[int] = [1, 2, 3]
dct: dict[str, int] = {"a": 1, "b": 2}

x: int = first(lst)
n: int = count_keys(dct)