from pwn import *

def blackbox(s):
    write('/tmp/foo', s)
    os.system('./slimming /tmp/foo /tmp/bar >/dev/null')
    return read('/tmp/bar')
