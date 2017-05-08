---
title: "PCTF 2017: YACP"
byline: "Points: 300, Category: Pwnable"
tags: Writeups
---

> What’s this? [Yet another crypto problem](yacp_13c264f8b1a8f3d0081086e6f12d7d05.tar.bz2)?
>
> You’ve got to be kidding me!
>
> Running at yacp.chal.pwning.xxx:7961

**TL;DR**: [doit.py](doit.py)

# Introduction

We are given an archive containing three files:

- `yacp`
- `libc.so.6`
- `libcrypto.so.1.0.0`

The first is a 32 bit ELF binary and the latter two are presumably versions of
libraries running on the remote server.

First, the program requires a proof of work.  We are given a 16 byte prefix
which is the hex-encoding of 8 random bytes read from `/dev/urandom`.  The goal
is then to find a 32 byte string starting with that prefix whose SHA-256 sum's
28 upper bits are 1.

This check can however be disabled by giving the program any number of command
line arguments.  This is very convenient during exploit development.

The actual program is some sort of "crypto service".  We are presented with a
main menu where we can choose to:

- Load data
- Generate random data
- Hash data
- Encrypt data
- Decrypt data
- Display data

Data is kept in 32 buffers each 2048 bytes which are statically allocated in the
data segment.  The datum sizes are 4 byte unsigned integers and are also stored
in the data segment, right after the buffers.

For hashing and encryption/decryption [OpenSSL
EVP](https://wiki.openssl.org/index.php/EVP) is used.  Pointers to the EVP
digest/cipher being used and corresponding digest/cipher contexts are stored
after the buffer sizes.

The (relevant part of) data segment is laid out like this:\
&nbsp;&nbsp; `0x0804c0e0`: Buffer #0\
&nbsp;&nbsp; ...\
&nbsp;&nbsp; `0x0805b8e0`: Buffer #31\
&nbsp;&nbsp; `0x0805c0e0`: Size of datum #0\
&nbsp;&nbsp; ...\
&nbsp;&nbsp; `0x0805c15c`: Size of datum #31\
&nbsp;&nbsp; `0x0805c160`: OpenSSL EVP digest object (24 bytes)\
&nbsp;&nbsp; `0x0805c178`: OpenSSL EVP cipher object (140 bytes)\
&nbsp;&nbsp; `0x0805c204`: Pointer to EVP digest\
&nbsp;&nbsp; `0x0805c208`: Pointer to EVP cipher

# Vulnerabilities

I spotted two mistakes in the program:

- The largest message that can be encrypted is a full buffer.  But because of
  plaintext padding the resulting ciphertext may be larger than a full buffer.

- If an unknown digest/cipher algorithm is selected an error message is printed
  but the program does not actually exit.  The currently selected digest/cipher
  will continue to be used.

For the first error consider this diagram which shows encrypting a full buffer
with AES-256 in [CBC
mode](https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation#Cipher_Block_Chaining_.28CBC.29).

![](encrypt.svg)

Since a block of padding is added to the message we have a one block overflow
past the end of the ciphertext buffer.  If the ciphertext is placed in buffer
#31 this overflow will overwrite the first four datum sizes.

The second error can be relevant depending on the exact exploit you decide to
implement.  In the exploit I'll describe below it will not be relevant.
Exercise to the reader: what exploit, then, will rely on this error?

# Information leak

By overwriting the size of datum #0 and then display it we can leak the data
segment.  But we need to be able to control the size, not just overwrite it.
Consider the diagram above again.  For convenience we define $x_n = c_{n-1}
\oplus p_0$.

Then we have

$$
    \begin{align}
    size_{0-3} & = c_{128} \\
               & = E(x_{128}) \\
               & = E(\texttt{"\x10}\ldots\texttt{"} \oplus c_{127}) \\
               & = E(\texttt{"\x10}\ldots\texttt{"} \oplus E(x_{127})) \\
               & = E(\texttt{"\x10}\ldots\texttt{"} \oplus E(p_{127} \oplus
                   c_{126}))
    \end{align}
$$

Solving for $size_{0-3}$ we get

$$
    \begin{align}
    size_{0-3} & =
      E(\texttt{"\x10}\ldots\texttt{"} \oplus E(p_{127} \oplus c_{126})) \\
    D(size_{0-3}) & =
      \texttt{"\x10}\ldots\texttt{"} \oplus E(p_{127} \oplus c_{126}) \\
    \texttt{"\x10}\ldots\texttt{"} \oplus D(size_{0-3}) & =
      E(p_{127} \oplus c_{126}) \\
    D(\texttt{"\x10}\ldots\texttt{"} \oplus D(size_{0-3})) & =
      p_{127} \oplus c_{126} \\
    c_{126} \oplus D(\texttt{"\x10}\ldots\texttt{"} \oplus D(size_{0-3})) & =
      p_{127} \\
    \end{align}
$$

OK, so to compute plaintext which will let us control $size_{0-3}$ we first
encrypt 127 arbitrary data blocks ($p_0 \ldots p_{126}$) with an arbitrary key
and IV, then derive the last plaintext block per the equations above.

# Memory write

By setting the size of datum #0 and encrypting/decrypting it into buffer #31 we
can overflow arbitrarily far into the data segment.  The part that will overflow
past buffer #31 should be placed in buffer #1, which is convenient since that
will not change the size of datum #0.  One (and the easiest, IMO) way to carry
out this attack is to place the encrypted overflow in buffer #1, then decrypt
datum #0 (with its size set appropriately) into buffer #31.  This diagram should
make it clear:

![](decrypt.svg)

Another thing to note is that the plaintext doesn't need to have the correct
padding because the decryption function never checks the return value of
`EVP_DecryptFinal_ex`.

From the diagram it is clear that the "IV" for the ciphertext in buffer #1 is
the last block in buffer #0.  And if we never actually write anything to buffer
#0 then it will be all zeros.

# Exploit

**Note**: There are many ways to exploit this service given the vulnerabilities
  outlined above.  The one I'll go through here is actually not the one I wrote
  during the CTF (this one's prettier).  During the CTF I used sizes of "big
  enough" and weeded out segfaults using a combination of GDB
  ([`gdb.attach`](http://docs.pwntools.com/en/stable/gdb.html?highlight=gdb#pwnlib.gdb.attach))
  and
  [`cyclic`](http://docs.pwntools.com/en/stable/util/cyclic.html?highlight=cyclic#pwnlib.util.cyclic.cyclic)
  until I had a working exploit.

On to the exploit.  So we can read and write memory in the data segment.  What
can we do with that?

We can overwrite the digest/cipher contexts and pointers.  Lets have a look at
the [OpenSSL source code](https://github.com/openssl/openssl).  At the time of this
writing the latest commit was
[d396da](https://github.com/openssl/openssl/tree/d396da33130aba2e77478d00fd369eb8d34bd8bf),
so the code below is taken from that commit.

The definition of a cipher context is given in
[`crypto/evp/evp_locl.h`](https://github.com/openssl/openssl/blob/d396da33130aba2e77478d00fd369eb8d34bd8bf/crypto/evp/evp_locl.h):

```{.c linenostart=24}
struct evp_cipher_ctx_st {
    const EVP_CIPHER *cipher;
    ENGINE *engine;             /* functional reference if 'cipher' is
                                 * ENGINE-provided */
    int encrypt;                /* encrypt or decrypt */
    int buf_len;                /* number we have left */
    unsigned char oiv[EVP_MAX_IV_LENGTH]; /* original iv */
    unsigned char iv[EVP_MAX_IV_LENGTH]; /* working iv */
    unsigned char buf[EVP_MAX_BLOCK_LENGTH]; /* saved partial block */
    int num;                    /* used by cfb/ofb/ctr mode */
    /* FIXME: Should this even exist? It appears unused */
    void *app_data;             /* application stuff */
    int key_len;                /* May change for variable length cipher */
    unsigned long flags;        /* Various flags */
    void *cipher_data;          /* per EVP data */
    int final_used;
    int block_mask;
    unsigned char final[EVP_MAX_BLOCK_LENGTH]; /* possible final block */
} /* EVP_CIPHER_CTX */ ;
```

And the definition of a cipher is given in
[`crypto/include/internal/evp_int.h`](https://github.com/openssl/openssl/blob/d396da33130aba2e77478d00fd369eb8d34bd8bf/crypto/include/internal/evp_int.h):

```{.c linenostart=109}
struct evp_cipher_st {
    int nid;
    int block_size;
    /* Default value for variable length ciphers */
    int key_len;
    int iv_len;
    /* Various flags */
    unsigned long flags;
    /* init key */
    int (*init) (EVP_CIPHER_CTX *ctx, const unsigned char *key,
                 const unsigned char *iv, int enc);
    /* encrypt/decrypt data */
    int (*do_cipher) (EVP_CIPHER_CTX *ctx, unsigned char *out,
                      const unsigned char *in, size_t inl);
    /* cleanup ctx */
    int (*cleanup) (EVP_CIPHER_CTX *);
    /* how big ctx->cipher_data needs to be */
    int ctx_size;
    /* Populate a ASN1_TYPE with parameters */
    int (*set_asn1_parameters) (EVP_CIPHER_CTX *, ASN1_TYPE *);
    /* Get parameters from a ASN1_TYPE */
    int (*get_asn1_parameters) (EVP_CIPHER_CTX *, ASN1_TYPE *);
    /* Miscellaneous operations */
    int (*ctrl) (EVP_CIPHER_CTX *, int type, int arg, void *ptr);
    /* Application data */
    void *app_data;
} /* EVP_CIPHER */ ;
```

See all those juicy pointers?  We can overflow into the `cipher` field of the
cipher context and point it to somewhere we control (say one of the buffers) and
place a fake cipher there.

The program has this sequence of calls in its decryption function:

```c
EVP_CIPHER_CTX_init(cipher_ctx);
EVP_CipherInit_ex(cipher_ctx, cipher, 0, &buffers[2048 * key], &buffers[2048 * iv], 0);
EVP_CipherUpdate(cipher_ctx, &buffers[2048 * out], &outl, &buffers[2048 * in], sizes[in]);
EVP_CipherFinal_ex(cipher_ctx, &buffers[2048 * out + outl] + outl, &outl_final);
EVP_CIPHER_CTX_cleanup(cipher_ctx);
```

The function
[`EVP_CipherUpdate`](https://github.com/openssl/openssl/blob/d396da33130aba2e77478d00fd369eb8d34bd8bf/crypto/evp/evp_enc.c#L207)
encrypts/decrypts a number of full blocks and saves any remaining data in an
internal buffer.  It will make the exploit easier if we overflow a number of
full blocks.  So keep that in mind when reading the final exploit.

When
[`EVP_CipherFinal`](https://github.com/openssl/openssl/blob/d396da33130aba2e77478d00fd369eb8d34bd8bf/crypto/evp/evp_enc.c#L216)
is called the overflow will have happened and (part of) `cipher_ctx` is
controlled by us.  So lets have a look at that function:

```{.c linenostart=224}
int EVP_CipherFinal_ex(EVP_CIPHER_CTX *ctx, unsigned char *out, int *outl)
{
    if (ctx->encrypt)
        return EVP_EncryptFinal_ex(ctx, out, outl);
    else
        return EVP_DecryptFinal_ex(ctx, out, outl);
}
```

For this exploit it doesn't matter whether
[`EVP_EncryptFinal_ex`](https://github.com/openssl/openssl/blob/d396da33130aba2e77478d00fd369eb8d34bd8bf/crypto/evp/evp_enc.c#L379)
or
[`EVP_DecryptFinal_ex`](https://github.com/openssl/openssl/blob/d396da33130aba2e77478d00fd369eb8d34bd8bf/crypto/evp/evp_enc.c#L497)
is called, so let's just pick `EVP_EncryptFinal_ex` as that will allow for a
wider range of values in `ctx->encrypt` ($2^{32} - 1$ vs. 1):

```{.c linenostart=379}
int EVP_EncryptFinal_ex(EVP_CIPHER_CTX *ctx, unsigned char *out, int *outl)
{
    int n, ret;
    unsigned int i, b, bl;

    if (ctx->cipher->flags & EVP_CIPH_FLAG_CUSTOM_CIPHER) {
        ret = ctx->cipher->do_cipher(ctx, out, NULL, 0);
        if (ret < 0)
            return 0;
        else
            *outl = ret;
        return 1;
    }
// ...
```

If the condition in the `if`-statement is satisfied the `do_cipher` function
pointer is called.  Note that the cipher context is given as the first argument.
If we set `do_cipher` to be the address of `system` then the context will be run
as a shell command.  The first four bytes are the address of the faked cipher.
They will probably not be a valid command (or one that hangs anyway) so if we
follow them with `";sh #"` we should be able to get a shell.

The final challenge is figuring out the address of `system`.  We can use the
information leak described above to leak the address current cipher.  Since
we're given the server's versions of `libc.so` and `libcrypto.so` we can then
calculate the address of `system`.

In the information leak we set the size of buffer #0.  The size should be set to

$$
    \begin{align}
    32 \cdot 2048 &+ \hspace{5em} \textrm{(buffers)} \\
    32 \cdot 4 &+ \hspace{5em} \textrm{(sizes)} \\
    24 &+ \hspace{5em} \textrm{(digest context)} \\
    140 &+ \hspace{5em} \textrm{(cipher context)} \\
    4 &+ \hspace{5em} \textrm{(digest pointer)} \\
    4 &+ \hspace{5em} \textrm{(cipher pointer)} \\
    &= 65836
    \end{align}
$$

## Proof of work

As mentioned `yacp` requires a proof of work, but this can be disabled.  Of
course I disabled the check when I was reversing and exploiting the program.

Unfortunately I then forgot about it and when time came to fire off the exploit
against the real service I was in for an unpleasant reminder.  It was 10 or 15
minutes till the end of the CTF and I failed to implement proof of work
generation in that time.

Bitterness prompted me to write a more general proof of work generation tool, so
I won't end up in this situation again.  You can find it on Github here:
[POW](https://www.github.com/br0ns/pow).
