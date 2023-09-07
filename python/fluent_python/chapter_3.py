def foo(*args, **kwargs):
    return args
    return kwargs['bar']

class Test(dict):
    def __missing__(self, key):
        return key, 42




