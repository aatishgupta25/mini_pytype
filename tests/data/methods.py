class C:
    def __init__(self, x: int) -> None:
        self.x = x

    def incr(self) -> None:
        self.x = self.x + 1

c = C(5)
c.incr()
