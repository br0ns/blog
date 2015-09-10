import gmpy

N = 2**64

xs = [0x1337deadbeef0095]
for _ in range(8):
    xs.append((xs[-1] * 0xcafe + 0xbeef) % N)

d = gmpy.invert(13337, 2**63)
for x in xs[1:]:
    y = pow(x, d, N)
    assert pow(y, 13337, N) == x
    print y
