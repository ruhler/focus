
#include <openssl/ssl.h>

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <netinet/in.h>

#define UNIX_PATH_MAX    108

int start_server()
{
    int lsfd = socket(AF_INET, SOCK_STREAM, 0);
    if (lsfd < 0) {
        perror("socket");
        return -1;
    }

    struct sockaddr_in inaddr;
    inaddr.sin_family = AF_INET;
    inaddr.sin_port = htons(4433);
    inaddr.sin_addr.s_addr = INADDR_ANY;

    if (bind(lsfd, (struct sockaddr *) &inaddr, sizeof(struct sockaddr_in)) < 0) {
        perror("bind");
        return -1;
    }

    if (listen(lsfd, 3) < 0) {
        perror("listen");
        return -1;
    }
    return lsfd;
}

DH *get_dh512()
	{
	static unsigned char dh512_p[]={
		0xBF,0xB5,0xA4,0x89,0xA3,0x03,0x4E,0xB5,0x1B,0x5B,0x51,0x44,
		0xA1,0x12,0xED,0x9A,0xC7,0x44,0xFC,0xC5,0x7F,0xC2,0x45,0x62,
		0x6E,0x3C,0x69,0x83,0x4B,0xDC,0xFC,0x42,0xB3,0x47,0xA3,0xFD,
		0xEA,0xD8,0x6C,0x51,0xE4,0x27,0x56,0x76,0xC2,0xF8,0x2B,0xB4,
		0xD1,0x90,0xC5,0x76,0x34,0xDF,0xE0,0x3B,0x0A,0xC3,0xA6,0xC4,
		0xCF,0xFA,0x59,0x5B,
		};
	static unsigned char dh512_g[]={
		0x02,
		};
	DH *dh;

	if ((dh=DH_new()) == NULL) return(NULL);
	dh->p=BN_bin2bn(dh512_p,sizeof(dh512_p),NULL);
	dh->g=BN_bin2bn(dh512_g,sizeof(dh512_g),NULL);
	if ((dh->p == NULL) || (dh->g == NULL))
		{ DH_free(dh); return(NULL); }
	return(dh);
	}

int main (int argc, char* arv[])
{
    SSL_load_error_strings();
    SSL_library_init();

    int lsfd = start_server();
    if (lsfd < 0) {
        return 1;
    }

    //while (1) {
        printf("Waiting for connection...");
        fflush(stdout);

        struct sockaddr_in paddr;
        socklen_t paddr_size;
        int pfd = accept(lsfd, (struct sockaddr*) &paddr, &paddr_size);
        if (pfd < 0) {
            perror("accept");
            return 1;
        }

        int encrypt = 1;

        SSL* ssl;
        if (encrypt) {
            SSL_METHOD* method = SSLv3_server_method();
            if (!method) {
                perror("SSLv3_server_method");
                return 1;
            }

            SSL_CTX* ctx = SSL_CTX_new(method);
            if (!ctx) {
                perror("SSL_ctx");
                return 1;
            }
            SSL_CTX_set_options(ctx, SSL_OP_SINGLE_DH_USE);

            if (!SSL_CTX_set_tmp_dh(ctx, get_dh512())) {
                perror("SSL_CTX_set_tmp_dh");
                return 1;
            }

            const char cipher[] = "ADH-AES256-SHA";
            if (!SSL_CTX_set_cipher_list(ctx, cipher)) {
                perror("SSL_CTX_set_cipher_list");
                return 1;
            }

            ssl = SSL_new(ctx);
            if (!ssl) {
                perror("SSL_new");
                return 1;
            }

            if (!SSL_set_fd(ssl, pfd)) {
                perror("SSL_set_fd");
                return 1;
            }

            printf("cipher 0: %s\n", SSL_get_cipher_list(ssl, 0));

            int sslaret = SSL_accept(ssl);
            if (sslaret < 0) {
                switch (SSL_get_error(ssl, sslaret)) {
                    case SSL_ERROR_ZERO_RETURN:
                        fprintf(stderr, "zero return\n");
                        break;
                    case SSL_ERROR_SSL:
                        fprintf(stderr, "ssl error\n");
                        break;
                    default:
                        fprintf(stderr, "err %i\n", SSL_get_error(ssl, sslaret));
                        break;
                }
                return 1;
            }
        }

        printf("\nACCEPTED CLIENT:\n");

        char buf[BUFSIZ] = {0};
        int red;
        do 
        { 
            if (encrypt) {
                red = SSL_read(ssl, buf, BUFSIZ-1);
            } else {
                red = read(pfd, buf, BUFSIZ-1);
            }
            write(STDOUT_FILENO, buf, red);
        } while (red);

        if (encrypt) {
            SSL_shutdown(ssl);
        }

        printf("\nCLOSED CLIENT\n");
    //}

    return 0;
}

