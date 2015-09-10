from pwn import *

# Read the writeup here: br0ns.dk/posts/0ctf/slimming

# The code pieces shown in the writeup (except for function definitions) are the
# `step_XXX` functions.

def blackbox(s):
    write('/tmp/foo', s)
    os.system('./slimming /tmp/foo /tmp/bar >/dev/null')
    return read('/tmp/bar')

def step_probe():
    for n in range(1, 10):
        s = 'A' * n
        t = blackbox(s)
        print '"%s" -> ' % s
        print hexdump(t)

def step_recover_xor_stream():
    stream = ''
    for n in range(1, 51):
        # T_(n-1) in closed form
        m = (n * (n - 1)) / 2 + 1
        t = blackbox('A' * m)
        assert len(t) == 4 + n * 2
        stream += xor(t[-2:], ('A', 0))
    print hexdump(stream)

def blackbox_inner(s):
    stream = [207, 67, 171, 70, 156, 48, 178, 80, 173, 51, 140, 53, 203, 49,
              152, 79, 141, 49, 139, 72, 146, 109, 204, 77, 186, 100, 203, 0]
    t = blackbox(s)[4:]
    return ordlist(xor(t, stream, cut = 'left'))

def step_hypothesis1():
    assert blackbox_inner('ABAB') == \
        [ord('A'), 0,
         ord('B'), 0,
         255, 1,
        ]
    assert blackbox_inner('ABABBAB') == \
        [ord('A'), 0,
         ord('B'), 0,
         255, 1,
         254, 1,
        ]

def step_hypothesis2():
    assert blackbox_inner('ABABXABX') == \
        [ord('A'), 0,
         ord('B'), 0,
         255, 1,
         ord('X'), 0,
         253, 1,
        ]
    assert blackbox_inner('ABABXABXBA') == \
        [ord('A'), 0,
         ord('B'), 0,
         255, 1,
         ord('X'), 0,
         253, 1,
         254, 1,
        ]

def step_questions1():
    s = ''.join(chr(x) for x in range(256)) + 'AAA'
    print blackbox_inner(s)

def step_questions2():
    print blackbox_inner('ABABABA')

def compress_inner(s):
    d = []
    i = 0
    prev = None
    out = []
    while i < len(s):
        if prev:
            d.append(prev + s[i])
        j = i + 1
        while j < len(s) and s[i:j+1] in d:
            j += 1
        token = s[i:j]
        if len(token) > 1:
            idx = d.index(token)
            idxa = (255 - idx & 0xff)
            idxb = (idx >> 8) + 1
            out += [idxa, idxb]
        else:
            out += [ord(token), 0]
        prev = token
        i = j
    return out

def compress(s):
    stream = [207, 67, 171, 70, 156, 48, 178, 80, 173, 51, 140, 53, 203, 49,
              152, 79, 141, 49, 139, 72, 146, 109, 204, 77, 186, 100, 203, 0]
    return '0ops' + xor(compress_inner(s), stream, cut = 'left')

def step_test_compress():
    for _ in range(1000):
        s = randoms(random.randint(1, 100))
        assert blackbox(s) == compress(s)

def decompress_inner(xs):
    d = []
    prev = None
    out = ''
    for a, b in group(2, xs):
        if b == 0:
            token = chr(a)
        else:
            idxa = a
            idxb = b
            idx = (idxb - 1) << 8 | (255 - idxa)
            if idx == len(d):
                token = prev + prev[0]
            else:
                token = d[idx]
        if prev:
            d.append(prev + token[0])
        prev = token
        out += token
    return out

def decompress(s):
    stream = [207, 67, 171, 70, 156, 48, 178, 80, 173, 51, 140, 53, 203, 49,
              152, 79, 141, 49, 139, 72, 146, 109, 204, 77, 186, 100, 203, 0]
    return decompress_inner(ordlist(xor(s[4:], stream, cut = 'left')))

def step_test_decompress():
    for _ in range(1000):
        s = randoms(random.randint(1, 100))
        assert decompress(compress(s)) == s

def step_final():
    write('slimming_inner', decompress(read('slimming_data')))
