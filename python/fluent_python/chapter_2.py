def cartesian_product(xs, ys):
    return [(x, y) for x in xs for y in ys]

def mul(a,b):
    return a * b

lax_cors = (33.9425, -118.408056)
lat, long = lax_cors

print(mul(*lax_cors))

s = 'bicycle'
print(s[::3])
print(s[::-1])
