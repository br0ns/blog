stream = ''
for n in range(1, 51):
    # T_(n-1) in closed form
    m = (n * (n - 1)) / 2 + 1
    t = blackbox('A' * m)
    assert len(t) == 4 + n * 2
    stream += xor(t[-2:], ('A', 0))
print hexdump(stream)
