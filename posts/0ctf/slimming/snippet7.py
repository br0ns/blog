def blackbox_inner(s):
    stream = [207, 67, 171, 70, 156, 48, 178, 80, 173, 51, 140, 53, 203, 49,
              152, 79, 141, 49, 139, 72, 146, 109, 204, 77, 186, 100, 203, 0]
    t = blackbox(s)[4:]
    return ordlist(xor(t, stream, cut = 'left'))
