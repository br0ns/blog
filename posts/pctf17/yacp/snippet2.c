EVP_CIPHER_CTX_init(cipher_ctx);
EVP_CipherInit_ex(cipher_ctx, cipher, 0, &buffers[2048 * key], &buffers[2048 * iv], 0);
EVP_CipherUpdate(cipher_ctx, &buffers[2048 * out], &outl, &buffers[2048 * in], sizes[in]);
EVP_CipherFinal_ex(cipher_ctx, &buffers[2048 * out + outl] + outl, &outl_final);
EVP_CIPHER_CTX_cleanup(cipher_ctx);
