---
title: 0ctf 2015: "r0ops" (Reverse 150)
tags: Writeups, IDA Pro
---

> allways [r0ops](r0ops)

This challenge was quite hard for the points given.  Anyway, the "reversing" is
easy enough.

First off the program accepts a network connection and reads up to 0x1000 bytes
to a global buffer.  Then it copies 0x1000 random looking bytes from one global
buffer to another.  I have no idea what this step is for; an attempt at
obfuscation maybe?

Lastly `rdi` is loaded with the address of the received data, `rsi` with the
address of an uninitialized global buffer, and `rsp` with an address in the
middle of the random looking bytes.  Upon return from the program will then of
course jump to the first address stored here.

As it turns out that address contains a jump to a short code sequence which ends
in a `ret`, which of course means that the program jumps to the next address in
the "stack":

```
0x0dead1f4: jmp     2 ;; 0x0dead1f8
0x0dead1f6: or      esi, esi ;; probably just for alignment
0x0dead1f8: pop     rcx
0x0dead1f8: retn
```

Looking at a few more gadgets on the "stack" I see the pattern that they all
start with a short jump and end with a return.

So I write an [IDA script](decode.py) to extract the "real" program:

```{#decode .python}
from idaapi import *
from idautils import *
from idc import *
import sys, os

# IDA does something to fd 1 and 2
def info(s):
    os.write(42, '%s\n' % s)

rsp = 0xe0b08a0
program = []
while True:
    addr = Qword(rsp)
    line = '0x%08x -> ' % rsp
    rsp += 8
    # Looks like all gadgets start on a jump
    jmp = DecodeInstruction(addr)
    # Not a pointer: The "stack" is probably exhausted
    if jmp is None:
        break
    assert jmp.get_canon_mnem() == 'jmp'
    addr = jmp.Op1.addr
    while True:
        size = MakeCode(addr)
        assert size > 0
        mnem = GetMnem(addr)
        line += '0x%08x: %s' % (addr, GetDisasm(addr))
        if   mnem == 'retn':
            break
        elif mnem == 'pop':
            line += ' ;; %d' % Qword(rsp)
            rsp += 8
        addr += size
        program.append(line)
        line = ' ' * 14

info('\n'.join(program))

exit()
```

And run it:

```
$ idaq64 -Sdecode.py -B r0ops 42>&1
0x0e0b08a0 -> 0x0dead1f8: pop     rcx ;; = 0x8
0x0e0b08b0 -> 0x0dead275: pop     r9 ;; = 0x1337deadbeef0095
0x0e0b08c0 -> 0x0dead127: mov     rax, [rdi]
0x0e0b08c8 -> 0x0dead0f1: add     rsi, 8
0x0e0b08d0 -> 0x0dead208: mov     [rsi], rax
0x0e0b08d8 -> 0x0dead26b: mov     r8, [rsi]
0x0e0b08e0 -> 0x0dead0fc: sub     rsi, 8
0x0e0b08e8 -> 0x0dead107: add     rdi, 8
0x0e0b08f0 -> 0x0dead0f1: add     rsi, 8
0x0e0b08f8 -> 0x0dead27e: mov     [rsi], r9
0x0e0b0900 -> 0x0dead212: mov     rax, [rsi]
0x0e0b0908 -> 0x0dead0fc: sub     rsi, 8
0x0e0b0910 -> 0x0dead1f0: pop     rbx ;; = 0xcafe
0x0e0b0920 -> 0x0dead145: imul    rax, rbx
0x0e0b0928 -> 0x0dead0f1: add     rsi, 8
0x0e0b0930 -> 0x0dead208: mov     [rsi], rax
0x0e0b0938 -> 0x0dead288: mov     r9, [rsi]
0x0e0b0940 -> 0x0dead0fc: sub     rsi, 8
0x0e0b0948 -> 0x0dead0f1: add     rsi, 8
0x0e0b0950 -> 0x0dead27e: mov     [rsi], r9
0x0e0b0958 -> 0x0dead212: mov     rax, [rsi]
0x0e0b0960 -> 0x0dead0fc: sub     rsi, 8
0x0e0b0968 -> 0x0dead1f0: pop     rbx ;; = 0xbeef
0x0e0b0978 -> 0x0dead131: add     rax, rbx
0x0e0b0980 -> 0x0dead0f1: add     rsi, 8
0x0e0b0988 -> 0x0dead208: mov     [rsi], rax
0x0e0b0990 -> 0x0dead288: mov     r9, [rsi]
0x0e0b0998 -> 0x0dead0fc: sub     rsi, 8
0x0e0b09a0 -> 0x0dead2cc: pop     r12 ;; = 0x1
0x0e0b09b0 -> 0x0dead292: pop     r10 ;; = 0x3419
0x0e0b09c0 -> 0x0dead1e8: pop     rax ;; = 0x0
0x0e0b09d0 -> 0x0dead1f0: pop     rbx ;; = 0x0
0x0e0b09e0 -> 0x0dead200: pop     rdx ;; = 0x1d8
0x0e0b09f0 -> 0x0dead19f: cmp     rax, rbx
              0x0dead1a2: jnz     short near ptr unk_DEAD1A7
              0x0dead1a4: add     rsp, rdx
0x0e0b09f8 -> 0x0dead0f1: add     rsi, 8
0x0e0b0a00 -> 0x0dead29b: mov     [rsi], r10
0x0e0b0a08 -> 0x0dead2c2: mov     r11, [rsi]
0x0e0b0a10 -> 0x0dead0fc: sub     rsi, 8
0x0e0b0a18 -> 0x0dead0f1: add     rsi, 8
0x0e0b0a20 -> 0x0dead2b8: mov     [rsi], r11
0x0e0b0a28 -> 0x0dead212: mov     rax, [rsi]
0x0e0b0a30 -> 0x0dead0fc: sub     rsi, 8
0x0e0b0a38 -> 0x0dead1f0: pop     rbx ;; = 0x1
0x0e0b0a48 -> 0x0dead177: and     rax, rbx
0x0e0b0a50 -> 0x0dead0f1: add     rsi, 8
0x0e0b0a58 -> 0x0dead208: mov     [rsi], rax
0x0e0b0a60 -> 0x0dead2c2: mov     r11, [rsi]
0x0e0b0a68 -> 0x0dead0fc: sub     rsi, 8
0x0e0b0a70 -> 0x0dead0f1: add     rsi, 8
0x0e0b0a78 -> 0x0dead2b8: mov     [rsi], r11
0x0e0b0a80 -> 0x0dead212: mov     rax, [rsi]
0x0e0b0a88 -> 0x0dead0fc: sub     rsi, 8
0x0e0b0a90 -> 0x0dead1f0: pop     rbx ;; = 0x1
0x0e0b0aa0 -> 0x0dead200: pop     rdx ;; = 0x68
0x0e0b0ab0 -> 0x0dead1ae: cmp     rax, rbx
              0x0dead1b1: jz      short near ptr unk_DEAD1B6
              0x0dead1b3: add     rsp, rdx
0x0e0b0ab8 -> 0x0dead0f1: add     rsi, 8
0x0e0b0ac0 -> 0x0dead2d5: mov     [rsi], r12
0x0e0b0ac8 -> 0x0dead212: mov     rax, [rsi]
0x0e0b0ad0 -> 0x0dead0fc: sub     rsi, 8
0x0e0b0ad8 -> 0x0dead0f1: add     rsi, 8
0x0e0b0ae0 -> 0x0dead261: mov     [rsi], r8
0x0e0b0ae8 -> 0x0dead226: mov     rbx, [rsi]
0x0e0b0af0 -> 0x0dead0fc: sub     rsi, 8
0x0e0b0af8 -> 0x0dead145: imul    rax, rbx
0x0e0b0b00 -> 0x0dead0f1: add     rsi, 8
0x0e0b0b08 -> 0x0dead208: mov     [rsi], rax
0x0e0b0b10 -> 0x0dead2df: mov     r12, [rsi]
0x0e0b0b18 -> 0x0dead0fc: sub     rsi, 8
0x0e0b0b20 -> 0x0dead0f1: add     rsi, 8
0x0e0b0b28 -> 0x0dead261: mov     [rsi], r8
0x0e0b0b30 -> 0x0dead212: mov     rax, [rsi]
0x0e0b0b38 -> 0x0dead0fc: sub     rsi, 8
0x0e0b0b40 -> 0x0dead0f1: add     rsi, 8
0x0e0b0b48 -> 0x0dead261: mov     [rsi], r8
0x0e0b0b50 -> 0x0dead226: mov     rbx, [rsi]
0x0e0b0b58 -> 0x0dead0fc: sub     rsi, 8
0x0e0b0b60 -> 0x0dead145: imul    rax, rbx
0x0e0b0b68 -> 0x0dead0f1: add     rsi, 8
0x0e0b0b70 -> 0x0dead208: mov     [rsi], rax
0x0e0b0b78 -> 0x0dead26b: mov     r8, [rsi]
0x0e0b0b80 -> 0x0dead0fc: sub     rsi, 8
0x0e0b0b88 -> 0x0dead0f1: add     rsi, 8
0x0e0b0b90 -> 0x0dead29b: mov     [rsi], r10
0x0e0b0b98 -> 0x0dead212: mov     rax, [rsi]
0x0e0b0ba0 -> 0x0dead0fc: sub     rsi, 8
0x0e0b0ba8 -> 0x0dead195: shr     rax, 1
0x0e0b0bb0 -> 0x0dead0f1: add     rsi, 8
0x0e0b0bb8 -> 0x0dead208: mov     [rsi], rax
0x0e0b0bc0 -> 0x0dead2a5: mov     r10, [rsi]
0x0e0b0bc8 -> 0x0dead0fc: sub     rsi, 8
0x0e0b0bd0 -> 0x0dead0f1: add     rsi, 8
0x0e0b0bd8 -> 0x0dead29b: mov     [rsi], r10
0x0e0b0be0 -> 0x0dead212: mov     rax, [rsi]
0x0e0b0be8 -> 0x0dead0fc: sub     rsi, 8
0x0e0b0bf0 -> 0x0dead1f0: pop     rbx ;; = 0x0
0x0e0b0c00 -> 0x0dead200: pop     rdx ;; = 0xfffffffffffffde0
0x0e0b0c10 -> 0x0dead1ae: cmp     rax, rbx
              0x0dead1b1: jz      short locret_DEAD1B6
              0x0dead1b3: add     rsp, rdx
0x0e0b0c18 -> 0x0dead0f1: add     rsi, 8
0x0e0b0c20 -> 0x0dead2d5: mov     [rsi], r12
0x0e0b0c28 -> 0x0dead212: mov     rax, [rsi]
0x0e0b0c30 -> 0x0dead0fc: sub     rsi, 8
0x0e0b0c38 -> 0x0dead0f1: add     rsi, 8
0x0e0b0c40 -> 0x0dead27e: mov     [rsi], r9
0x0e0b0c48 -> 0x0dead226: mov     rbx, [rsi]
0x0e0b0c50 -> 0x0dead0fc: sub     rsi, 8
0x0e0b0c58 -> 0x0dead200: pop     rdx ;; = 0x20
0x0e0b0c68 -> 0x0dead1ae: cmp     rax, rbx
              0x0dead1b1: jz      short locret_DEAD1B6
              0x0dead1b3: add     rsp, rdx
0x0e0b0c70 -> 0x0dead200: pop     rdx ;; = 0xfffffffffffffc38
0x0e0b0c80 -> 0x0dead1df: loop    near ptr unk_DEAD1DB
0x0e0b0c88 -> 0x0dead340: sub     rsp, 10h
              0x0dead344: mov     edi, 0DEAD544h
              0x0dead349: call    near ptr _puts
              0x0dead34e: mov     edi, 0DEAD54Eh
              0x0dead353: mov     eax, 0
              0x0dead358: call    near ptr _printf
              0x0dead35d: mov     dword ptr [rbp-4], 0
              0x0dead364: jmp     short near ptr unk_DEAD38B
              0x0dead366: mov     eax, [rbp-4]
              0x0dead369: cdqe
              0x0dead36b: mov     rax, ds:qword_E0B10C0[rax*8]
              0x0dead373: mov     eax, eax
              0x0dead375: mov     rsi, rax
              0x0dead378: mov     edi, 0DEAD55Dh
              0x0dead37d: mov     eax, 0
              0x0dead382: call    near ptr _printf
              0x0dead387: add     dword ptr [rbp-4], 1
              0x0dead38b: cmp     dword ptr [rbp-4], 7
              0x0dead38f: jle     short loc_DEAD366
              0x0dead391: mov     edi, 0DEAD564h
              0x0dead396: call    near ptr _puts
              0x0dead39b: mov     eax, 0
              0x0dead3a0: call    near ptr unk_DEAD3AF
              0x0dead3a5: leave
0x0e0b0c90 -> 0x0dead3b3: sub     rsp, 10h
              0x0dead3b7: mov     edx, 0
              0x0dead3bc: mov     esi, 0
              0x0dead3c1: mov     edi, 3
              0x0dead3c6: call    near ptr _accept
              0x0dead3cb: mov     [rbp-4], eax
              0x0dead3ce: mov     eax, [rbp-4]
              0x0dead3d1: mov     ecx, 0
              0x0dead3d6: mov     edx, 1000h
              0x0dead3db: mov     esi, 0E0B10C0h
              0x0dead3e0: mov     edi, eax
              0x0dead3e2: call    near ptr _recv
              0x0dead3e7: mov     eax, [rbp-4]
              0x0dead3ea: mov     edi, eax
              0x0dead3ec: mov     eax, 0
              0x0dead3f1: call    near ptr _close
              0x0dead3f6: mov     edx, 0E0AF0A0h
              0x0dead3fb: mov     esi, 0E0B00A0h
              0x0dead400: mov     eax, 200h
              0x0dead405: mov     rdi, rdx
              0x0dead408: mov     rcx, rax
              0x0dead40b: rep movsq
              0x0dead40e: mov     eax, 0E0B10C0h
              0x0dead413: mov     rdi, rax
              0x0dead416: mov     eax, 0E0B20C0h
              0x0dead41b: mov     rsi, rax
              0x0dead41e: mov     eax, 0E0AF8A0h
              0x0dead423: mov     rsp, rax
```

There are four conditional jumps in there.  They all jump past the next
instruction and directly to the `retn`.  In all four cases the following
instruction adds to `rsp` which is effectively a jump in the stack-program.

Looking at the stack-program I notice that

 1. It looks like it is "compiled" from some higher-level language (`add
    rsi`/`sub rsi` -instructions?)

 2. The last two gadgets are huge; they are probably "real" functions.  The
    second one (the pointer at 0x0e0bc90) is just back in the `accept`-loop, so
    the first one (pointer at 0x0ebc88) is probably a win-function.

First I group the program by which instructions I guess belong to the same
higher-level language operation:

```
    pop     rcx ;; = 0x8
    pop     r9  ;; = 0x1337deadbeef0095

    mov     rax, [rdi]
    add     rsi, 8
    mov     [rsi], rax
    mov     r8, [rsi]
    sub     rsi, 8

    add     rdi, 8

    add     rsi, 8
    mov     [rsi], r9
    mov     rax, [rsi]
    sub     rsi, 8

    pop     rbx ;; = 0xcafe
    imul    rax, rbx

    add     rsi, 8
    mov     [rsi], rax
    mov     r9, [rsi]
    sub     rsi, 8

    add     rsi, 8
    mov     [rsi], r9
    mov     rax, [rsi]
    sub     rsi, 8

    pop     rbx ;; = 0xbeef
    add     rax, rbx

    add     rsi, 8
    mov     [rsi], rax
    mov     r9, [rsi]
    sub     rsi, 8

    pop     r12 ;; = 0x1
    pop     r10 ;; = 0x3419
    pop     rax ;; = 0x0
    pop     rbx ;; = 0x0

    pop     rdx ;; = 0x1d8
    cmp     rax, rbx
    jnz     short near ptr unk_DEAD1A7
    add     rsp, rdx

    add     rsi, 8
    mov     [rsi], r10
    mov     r11, [rsi]
    sub     rsi, 8

    add     rsi, 8
    mov     [rsi], r11
    mov     rax, [rsi]
    sub     rsi, 8

    pop     rbx ;; = 0x1
    and     rax, rbx

    add     rsi, 8
    mov     [rsi], rax
    mov     r11, [rsi]
    sub     rsi, 8

    add     rsi, 8
    mov     [rsi], r11
    mov     rax, [rsi]
    sub     rsi, 8

    pop     rbx ;; = 0x1

    pop     rdx ;; = 0x68
    cmp     rax, rbx
    jz      short near ptr unk_DEAD1B6
    add     rsp, rdx

    add     rsi, 8
    mov     [rsi], r12
    mov     rax, [rsi]
    sub     rsi, 8

    add     rsi, 8
    mov     [rsi], r8
    mov     rbx, [rsi]
    sub     rsi, 8

    imul    rax, rbx

    add     rsi, 8
    mov     [rsi], rax
    mov     r12, [rsi]
    sub     rsi, 8

    add     rsi, 8
    mov     [rsi], r8
    mov     rax, [rsi]
    sub     rsi, 8

    add     rsi, 8
    mov     [rsi], r8
    mov     rbx, [rsi]
    sub     rsi, 8

    imul    rax, rbx

    add     rsi, 8
    mov     [rsi], rax
    mov     r8, [rsi]
    sub     rsi, 8

    add     rsi, 8
    mov     [rsi], r10
    mov     rax, [rsi]
    sub     rsi, 8

    shr     rax, 1

    add     rsi, 8
    mov     [rsi], rax
    mov     r10, [rsi]
    sub     rsi, 8

    add     rsi, 8
    mov     [rsi], r10
    mov     rax, [rsi]
    sub     rsi, 8

    pop     rbx ;; = 0x0
    pop     rdx ;; = 0xfffffffffffffde0

    cmp     rax, rbx
    jz      short locret_DEAD1B6
    add     rsp, rdx

    add     rsi, 8
    mov     [rsi], r12
    mov     rax, [rsi]
    sub     rsi, 8

    add     rsi, 8
    mov     [rsi], r9
    mov     rbx, [rsi]
    sub     rsi, 8

    pop     rdx ;; = 0x20

    cmp     rax, rbx
    jz      short locret_DEAD1B6
    add     rsp, rdx

    pop     rdx ;; = 0xfffffffffffffc38
    loop    near ptr unk_DEAD1DB
WIN:
    ...
EXIT:
    ...
```

Points of interest:

 - `rsi` is only used for moving a value from one register to another.
 - `rdi` (pointer to the data received from the network, remember?) is only
   modified in one place: 8 is being added shortly after reading one qword from
   `[rdi]` into `rax`.
 - Before each conditional jump a constant is loaded into `rdx` and depending on
   whether `rax` and `rbx` is equal or not `rdx` is added to `rsp`.
 - Right before the win-function is a `loop` instruction that jumps to `add rsp,
   rdx ; retn`.  These instructions were of course not found by my IDA script.
 - `rcx` is set to 8 in the beginning and then only modified by the `loop`
   instruction.

Rewriting register assignment, jumps and inserting labels gives:

```
    rcx <- 8
    r9  <- 0x1337deadbeef0095
START:
    rax <- get_next_input_qword
    r8  <- rax

    rax <- r9
    rbx <- 0xcafe
    rax <- rax * rbx
    r9  <- rax

    rax <- r9
    rbx <- 0xbeef
    rax <- rax + rbx
    r9  <- rax

    r12 <- 1
    r10 <- 0x3419

    rax <- 0
    rbx <- 0
    goto LBL3 if rax == rbx

LBL1:
    r11 <- r10

    rax <- r11
    rbx <- 1
    rax <- rax & rbx
    r11 <- rax

    rax <- r11
    rbx <- 1
    goto LBL2 if rax <> rbx

    rax <- r12
    rbx <- r8
    rax <- rax * rbx
    r12 <- rax

LBL2:
    rax <- r8
    rbx <- r8
    rax <- rax * rbx
    r8  <- rax

    rax <- r10
    rax <- rax >> 1
    r10 <- rax

LBL3:
    rax <- r10
    rbx <- 0
    goto LBL1 if rax <> rbx

    rax <- r12
    rbx <- r9
    goto EXIT if rax <> rbx

    loop START
WIN:
    ...
EXIT:
    ...
```

Aha, `rax` and `rbx` are only used as temporary variables (notice that the
compiler completely breaks character in the right-shift operation where 1 is
used directly rather than being loaded into `rbx`).

Getting rid of `rax` and `rbx`:

```
    rcx <- 8
    r9  <- 0x1337deadbeef0095
START:
    r8  <- get_next_input_qword

    r9  <- r9 * 0xcafe
    r9  <- r9 + 0xbeef

    r12 <- 1
    r10 <- 0x3419
    goto LBL3

LBL1:
    r11 <- r10
    r11 <- r11 & 1
    goto LBL2 if r11 <> 1
    r12 <- r12 * r8

LBL2:
    r8  <- r8 * r8
    r10 <- r10 >> 1

LBL3:
    goto LBL1 if r10 <> 0

    goto EXIT if r12 <> r9

    loop START
WIN:
    ...
EXIT:
    ...
```

One final rewrite (even though I'm sure you have it figured out):

```
r9  <- 0x1337deadbeef0095
run 8 times {
  r8  <- get_next_input_qword
  r9  <- r9 * 0xcafe + 0xbeef
  r12 <- 1
  r10 <- 13337
  while (r10) {
    if (r10 & 1) {
      r12 <- r12 * r8
    }
    r8  <- r8 * r8
    r10 <- r10 / 2
  }
  if (r9 != r12) {
    exit();
  }
}
win();
```

This code computes a series as:

$$
    x_0 = \tt{0x1337deadbeef0095} \\
    x_n = x_{n-1} \cdot \tt{0xcafe} + \tt{0xbeef}
$$

Split the input into qwords, $y_1, ..., y_8$.  For each $y_i$
[exponentiation by squaring](http://en.wikipedia.org/wiki/Exponentiation_by_squaring)
is used to compute $y_{i}^{13337}$.

If $y_{i}^{13337} = x_i$ for $i = 1, ..., 8$ the win-function is called.
Easy-peasy.

#### Team member kriztw solved this part:

Because register values wrap around, computation effectively happens modulo
$2^{64}$.

By [Euler's theorem](http://en.wikipedia.org/wiki/Euler%27s_theorem) we have
that $a^{\phi(n)} \equiv 1 \mod n$, so if we can find $d \equiv 13337^{-1} \mod
\phi(n)$ we will have $x_{i}^{d} = (y_{i}^{13337})^d = y_{i}^{13337 \cdot d} =
y_{i}^1 = y_i$.

Every odd number is relatively prime to $2^{64}$ so $\phi(2^{64})$ is $2^{63}$.  And
here's the program:

```python
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
```

Flag:
**`0ctf{c97155a5e288fa45f926b1058e4e0385d6ccde8513002885dc67948524bcbc85}`**
