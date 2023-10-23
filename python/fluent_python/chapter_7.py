def id(x):
    """ id """
    return x

print(id.__doc__)

class Option():
    def __init__(self, value):
        self.value = value

    def map(self, f):
        if self.value != None:
            return self
        return f(self.value)


