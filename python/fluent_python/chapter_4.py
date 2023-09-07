from collections import namedtuple

Coordinates = namedtuple('Coordinates', ['lat', 'lon'])

a = Coordinates(37.24, -115.81)
b = Coordinates(37.24, -115.81)
print(a)
print(a == b)

