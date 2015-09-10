for _ in range(1000):
    s = randoms(random.randint(1, 100))
    assert decompress(compress(s)) == s
