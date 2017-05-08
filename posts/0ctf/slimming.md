---
title: "0ctf 2015: slimming"
byline: "Points: 200, Category: reverse"
tags: Writeups
---

> Foodie has eaten so much delicious food and decide to slim now >_<.
>
> [slimming](slimming.tar.gz)


**TL;DR**: [doit.py](doit.py)

The [TAR archive](slimming.tar.gz) contains two files; `slimming` which is an
AMD64 ELF binary, and `slimming_data` which is more or less (but mostly less)
random looking binary data.

Running the binary with no arguments prints a usage message:

```
$ ./slimming
Usage: slimming <input> <output>
```

And with arguments it converts a file to that random-y looking format:

```
$ echo foobar > /tmp/foo
$ ./slimming /tmp/foo /tmp/bar
Done!
$ phd /tmp/bar
00000000  30 6f 70 73  a9 43 c4 46  f3 30 d0 50  cc 33 fe 35  │0ops│·C·F│·0·P│·3·5│
00000010  c1 31                                               │·1│
00000012
```

After firing up IDA and looking at the binary for about five seconds I decide
no, I'm going to blackbox this thing (It's full of MMX ugliness).

So first off I create a python function to run a string through the program.

```python
from pwn import *

def blackbox(s):
    write('/tmp/foo', s)
    os.system('./slimming /tmp/foo /tmp/bar >/dev/null')
    return read('/tmp/bar')
```

Tradition dictates that I must now trow A's at it:

```
for n in range(1, 10):
    s = 'A' * n
    t = blackbox(s)
    print s
    print hexdump(t)
```

Output (formatted slightly):

```
"A" ->
00000000  30 6f 70 73  8e 43                                  │0ops│·C│
"AA" ->
00000000  30 6f 70 73  8e 43 ea 46                            │0ops│·C·F││
"AAA" ->
00000000  30 6f 70 73  8e 43 54 47                            │0ops│·CTG││
"AAAA" ->
00000000  30 6f 70 73  8e 43 54 47  dd 30                     │0ops│·CTG│·0│
"AAAAA" ->
00000000  30 6f 70 73  8e 43 54 47  63 31                     │0ops│·CTG│c1│
"AAAAAA" ->
00000000  30 6f 70 73  8e 43 54 47  62 31                     │0ops│·CTG│b1│
"AAAAAAA" ->
00000000  30 6f 70 73  8e 43 54 47  62 31 f3 50               │0ops│·CTG│b1·P││
"AAAAAAAA" ->
00000000  30 6f 70 73  8e 43 54 47  62 31 4d 51               │0ops│·CTG│b1MQ││
"AAAAAAAAA" ->
00000000  30 6f 70 73  8e 43 54 47  62 31 4c 51               │0ops│·CTG│b1LQ││
```

There are few interesting points to notice already:

 * The output always starts with "0ops".

 * Bytes 5 and 6 are `8e 43` in all my tests.

 * The output grows by increments of two.  This suggests that the output bytes
   should be interpreted as pairs.

 * The output grows slower as more A's are added.  This suggest we're dealing
   with a compression program (which also fits well withe the name of the
   challenge).

 * The output seems to be obfuscated on a per-byte basis, and the obfuscation is
   static.  If not, the output would have changed for the different inputs.

 * A given pair will change as the input grows.  After a certain number of
   changes the pair stays the same.  The second byte in the pair only changes
   the first time, and only its LSB is changed.  The first byte changes much the
   first time, and then only a little on subsequent inputs.

The last two points and a little bit of guessing is enough to de-obfuscate the
output; First I guess that the output is xor'ed with a static stream before
being written to the output file.  Why xor?  It always is.

So to recover the stream a known output is needed.  Why is it that the first
byte change by a lot and the second only by one bit?  Since the program is doing
compression it is natural to believe that it maintains some kind of dictionary
of things that it has already seen; when it sees something it knows it just
outputs a reference.

This model fits well with our data: The second byte changes from "literal" to
"reference" and the first byte changes from the literal "A" to a reference.
Also notice that references always encode strings longer that a single character
(or else the last A would not be encoded as a literal).

Lastly the first pair will always be a literal since there will not yet be
anything to reference.

I guess that "literal" is encoded as `0` &mdash; the actual value used
internally in the program doesn't matter.

The input strings that are of interest to me are those that first produce a new
pair, that is "A", "AA", "AAAA", "AAAAAAA", &hellip;

Given the model I have guessed at, these strings must be encoded as:

 - "A": `[Lit "A"]`

 - "AA": `[Lit "A", Lit "A"]`

 - "AAAA": `[Lit "A", Ref "AA", Lit "A"]`

 - "AAAAAAA": `[Lit "A", Ref "AA", Ref "AAA", Lit "A"]`

There's a pattern here: For _n_ pairs the number of A's encoded by the first _n
- 1_ pairs is _1 + &hellip; + n - 1 = T~n-1~_ and the last pair then encodes a
literal A.

Now we can recover the first, say 100, bytes of the xor stream:

```python
stream = ''
for n in range(1, 51):
    # T_(n-1) in closed form
    m = (n * (n - 1)) / 2 + 1
    t = blackbox('A' * m)
    assert len(t) == 4 + n * 2
    stream += xor(t[-2:], ('A', 0))
print hexdump(stream)
```

This is the result:

```
00000000  cf 43 ab 46  9c 30 b2 50  ad 33 8c 35  cb 31 98 4f  │·C·F│·0·P│·3·5│·1·O│
00000010  8d 31 8b 48  92 6d cc 4d  ba 64 cb 00  cf 43 ab 46  │·1·H│·m·M│·d··│·C·F│
00000020  9c 30 b2 50  ad 33 8c 35  cb 31 98 4f  8d 31 8b 48  │·0·P│·3·5│·1·O│·1·H│
00000030  92 6d cc 4d  ba 64 cb 00  cf 43 ab 46  9c 30 b2 50  │·m·M│·d··│·C·F│·0·P│
00000040  ad 33 8c 35  cb 31 98 4f  8d 31 8b 48  92 6d cc 4d  │·3·5│·1·O│·1·H│·m·M│
00000050  ba 64 cb 00  cf 43 ab 46  9c 30 b2 50  ad 33 8c 35  │·d··│·C·F│·0·P│·3·5│
00000060  cb 31 98 4f                                         │·1·O││
00000064
```

Looks like the stream is cyclic with a period of 28.  I now define a more usable
blackbox without the obfuscation:

```python
def blackbox_inner(s):
    stream = [207, 67, 171, 70, 156, 48, 178, 80, 173, 51, 140, 53, 203, 49,
              152, 79, 141, 49, 139, 72, 146, 109, 204, 77, 186, 100, 203, 0]
    t = blackbox(s)[4:]
    return ordlist(xor(t, stream, cut = 'left'))
```

Running `blackbox_inner('A' * 20)` yields:

```python
[65, 0, 255, 1, 254, 1, 254, 1]
```

Looks like the references are encoded as negative indexes into a dictionary of
tokens.

Encoding "AAAAAAAAA" (nine A's) must go something like this:

 - 1^st^ pair:
     * Is "AA" in the dictionary? No.
     * Output literal "A": `('A', 0)`
 - 2^nd^ pair:
     * Is "AA" in the dictionary? Yes.
     * Is "AAA" in the dictionary? No.
     * Output reference to "AA": `(-1, 0)`
 - 3^rd^ pair:
     * Is "AA" in the dictionary? Yes.
     * Is "AAA" in the dictionary? Yes.
     * Is "AAAA" in the dictionary? No.
     * Output reference to "AAA": `(-2, 0)`
 - 4^th^ pair:
     * Is "AA" in the dictionary? Yes.
     * Is "AAA" in the dictionary? Yes.
     * Is "AAAA" in the dictionary? No.
     * Output reference to "AAA": `(-2, 0)`

Of course the question now is "how is the table being build".  The token "AA"
must have been added to the dictionary between outputting the 1^st^ and
2^nd^ pair.

Hypothesis: Each time a token is outputted a token consisting of itself and the
letter just before it is added to the dictionary.

This model fits with the data, so I perform a few more tests to confirm or
discard the hypothesis:

```python
blackbox_inner('ABAB')
  [65, 0, 66, 0, 255, 1]
blackbox_inner('ABABBAB')
  [65, 0, 66, 0, 255, 1, 254, 1, 66, 0]
```

Oops, hypothesis discarded &#9785;.  (The second "AB" would have added "BAB" to
the dictionary, which was clearly not the case.)

Revised hypothesis: Each time a token is outputted a token consisting of the
previous token and the first letter of the token being outputted is added to the
dictionary.

Note that this model fits both the A-tests and the two tests above.

Testing:

```python
blackbox_inner('ABABXABX')
  [65, 0, 66, 0, 255, 1, 88, 0, 253, 1]
```

Yep, "ABX" (_=_ "AB" + "X") was in the dictionary.

Why is the reference 253? Because "BA" was added between the 2^nd^ and
3^rd^ pair:

```python
blackbox_inner('ABABXABXBA')
  [65, 0, 66, 0, 255, 1, 88, 0, 253, 1, 254, 1]
```

Two questions remain:

 1. What happens if the dictionary grows beyond 255 entries?
 1. Is the _n_^th^ entry added to the dictionary before or after the
    _n_^th^ pair is generated?  That is, can a token reference itself?

The first one is easy to test:
```python
s = ''.join(chr(x) for x in range(256)) + 'AAA'
blackbox_inner(s)
  [..., 65, 0, 255, 2]
```

Aha, so the second component in the pairs is part of the dictionary index.

For the second question I design this test:
```python
blackbox_inner('ABABABA')
  [65, 0, 66, 0, 255, 1, 253, 1]
```

The last token added to the dictionary is "ABA" which is referenced in the last
pair.  This might seem a little odd; how would you know what the token is when
you decompress?  Actually it's not a problem: when decompressing a pair which
reference a token which was not yet added to the dictionary then the referenced
token must be the one which are about to be added.  That token is the previous
outputted token plus the first letter of the current &mdash; but the first
letter must then also be the first letter of the previous token!

In the example above, the last token references itself: the previous token was
"AB", so the last one must be "AB" + "AB"[0] = "ABA".

I can now build a compression function:
```python
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
```

And test it for correctness:
```python
for _ in range(1000):
    s = randoms(random.randint(1, 100))
    assert blackbox(s) == compress(s)
```

And then, finally, a decompression function:

```python
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
```

Of course I test it too:

```python
for _ in range(1000):
    s = randoms(random.randint(1, 100))
    assert decompress(compress(s)) == s
```

And the final step:

```python
write('slimming_inner', decompress(read('slimming_data')))
```

The result, [slimming_inner](slimming_inner), is an extremely small 32bit ELF.
It is the simplest crackme you have ever seen:

```
$ objdump -dMintel slimming_inner
slimming_inner:     file format elf32-i386


Disassembly of section .text:

08048054 <.text>:
 8048054:	83 ec 40             	sub    esp,0x40
 8048057:	ba 40 00 00 00       	mov    edx,0x40
 804805c:	89 e1                	mov    ecx,esp
 804805e:	bb 00 00 00 00       	mov    ebx,0x0
 8048063:	b8 03 00 00 00       	mov    eax,0x3
 8048068:	cd 80                	int    0x80
 804806a:	eb 17                	jmp    0x8048083
 804806c:	89 c2                	mov    edx,eax
 804806e:	89 e1                	mov    ecx,esp
 8048070:	bb 01 00 00 00       	mov    ebx,0x1
 8048075:	b8 04 00 00 00       	mov    eax,0x4
 804807a:	cd 80                	int    0x80
 804807c:	b8 01 00 00 00       	mov    eax,0x1
 8048081:	cd 80                	int    0x80
 8048083:	31 c0                	xor    eax,eax
 8048085:	8a 04 24             	mov    al,BYTE PTR [esp]
 8048088:	83 e8 30             	sub    eax,0x30
 804808b:	0f 85 b0 02 00 00    	jne    0x8048341
 8048091:	31 c0                	xor    eax,eax
 8048093:	8a 44 24 01          	mov    al,BYTE PTR [esp+0x1]
 8048097:	83 e8 63             	sub    eax,0x63
 804809a:	0f 85 a1 02 00 00    	jne    0x8048341
 ...
 8048309:	eb 00                	jmp    0x804830b
 804830b:	c6 04 24 43          	mov    BYTE PTR [esp],0x43
 804830f:	c6 44 24 01 6f       	mov    BYTE PTR [esp+0x1],0x6f
 8048314:	c6 44 24 02 72       	mov    BYTE PTR [esp+0x2],0x72
 8048319:	c6 44 24 03 72       	mov    BYTE PTR [esp+0x3],0x72
 804831e:	c6 44 24 04 65       	mov    BYTE PTR [esp+0x4],0x65
 8048323:	c6 44 24 05 63       	mov    BYTE PTR [esp+0x5],0x63
 8048328:	c6 44 24 06 74       	mov    BYTE PTR [esp+0x6],0x74
 804832d:	c6 44 24 07 0a       	mov    BYTE PTR [esp+0x7],0xa
 8048332:	c6 44 24 08 00       	mov    BYTE PTR [esp+0x8],0x0
 8048337:	b8 08 00 00 00       	mov    eax,0x8
 804833c:	e9 2b fd ff ff       	jmp    0x804806c
 8048341:	c6 04 24 4e          	mov    BYTE PTR [esp],0x4e
 8048345:	c6 44 24 01 6f       	mov    BYTE PTR [esp+0x1],0x6f
 804834a:	c6 44 24 02 2e       	mov    BYTE PTR [esp+0x2],0x2e
 804834f:	c6 44 24 03 4e       	mov    BYTE PTR [esp+0x3],0x4e
 8048354:	c6 44 24 04 6f       	mov    BYTE PTR [esp+0x4],0x6f
 8048359:	c6 44 24 05 2e       	mov    BYTE PTR [esp+0x5],0x2e
 804835e:	c6 44 24 06 2e       	mov    BYTE PTR [esp+0x6],0x2e
 8048363:	c6 44 24 07 4e       	mov    BYTE PTR [esp+0x7],0x4e
 8048368:	c6 44 24 08 6f       	mov    BYTE PTR [esp+0x8],0x6f
 804836d:	c6 44 24 09 2e       	mov    BYTE PTR [esp+0x9],0x2e
 8048372:	c6 44 24 0a 2e       	mov    BYTE PTR [esp+0xa],0x2e
 8048377:	c6 44 24 0b 2e       	mov    BYTE PTR [esp+0xb],0x2e
 804837c:	c6 44 24 0c 0a       	mov    BYTE PTR [esp+0xc],0xa
 8048381:	c6 44 24 0d 00       	mov    BYTE PTR [esp+0xd],0x0
 8048386:	b8 0d 00 00 00       	mov    eax,0xd
 804838b:	e9 dc fc ff ff       	jmp    0x804806c
```

The flag is: **`0ctf{AdD_15t_cHar_t0_7h3_eNd_wHEh_3xCept10n}`**
