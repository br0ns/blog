def decompress_inner(xs):
    d = []
    prev = None
    out = ''
    for a, b in group(2, xs):
        print a, b
        print d
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
