def foo(x, y):
    return x * y

def lim(f, x):
    h = 1e-5
    return (f(x + h) - f(x)) / h

def gradient_foo(x, y):
    δx = lim(lambda x: foo(x, y), x)
    δy = lim(lambda y: foo(x, y), y)

    return δx, δy


