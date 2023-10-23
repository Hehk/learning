from collections import namedtuple

Coordinates = namedtuple('Coordinates', ['lat', 'lon'])

a = Coordinates(37.24, -115.81)
b = Coordinates(37.24, -115.81)
print(a)
print(a == b)

import typing

class DemoTyping(typing.NamedTuple):
    lat: float
    lon: float = 5.0
    test = 5

DemoTyping(37.24, -115.81)
print(DemoTyping.__annotations__)
print(DemoTyping.__doc__)



