def id(x):
    """ id """
    return x

print(id.__doc__)

class Option():
    def __init__(self, value):
        self.value = value

    def map(self, f):
        if self.value is None:
            return self
        return Option(f(self.value))

    def __call__(self, f):
        return self.map(f)
