class Vector:
    def __init__(self, x, y):
        self.x = x
        self.y = y
    def __mul__(self, scalar):
        self.x *= scalar
        self.y *= scalar


