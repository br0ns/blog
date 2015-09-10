s = ''.join(chr(x) for x in range(256)) + 'AAA'
blackbox_inner(s)
  [..., 65, 0, 255, 2]
