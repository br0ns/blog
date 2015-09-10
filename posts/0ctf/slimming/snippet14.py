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
