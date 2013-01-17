
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
        return 1;
    }

    struct sockaddr_in inaddr;
    inaddr.sin_family = AF_INET;
    inaddr.sin_port = htons(4433);
    inaddr.sin_addr.s_addr = INADDR_ANY;

    if (bind(lsfd, (struct sockaddr *) &inaddr, sizeof(struct sockaddr_in)) < 0) {
        perror("bind");
        return 1;
    }

    if (listen(lsfd, 3) < 0) {
        perror("listen");
        return 1;
    }
    return lsfd;
}

int main (int argc, char* arv[])
{
    SSL_load_error_strings();
    SSL_library_init();

    int lsfd = start_server();
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

//        SSL_METHOD* method = SSLv3_server_method();
//        if (!method) {
//            perror("SSLv3_server_method");
//            return 1;
//        }
//
//        SSL_CTX* ctx = SSL_CTX_new(method);
//        if (!ctx) {
//            perror("SSL_ctx");
//            return 1;
//        }
//
//        SSL* ssl = SSL_new(ctx);
//        if (!ssl) {
//            perror("SSL_new");
//            return 1;
//        }
//
//        if (!SSL_set_fd(ssl, pfd)) {
//            perror("SSL_set_fd");
//            return 1;
//        }
//
//        printf("ciphers: %p\n", SSL_get_ciphers(ssl));
//
//        int sslaret = SSL_accept(ssl);
//        if (sslaret < 0) {
//            switch (SSL_get_error(ssl, sslaret)) {
//                case SSL_ERROR_ZERO_RETURN:
//                    fprintf(stderr, "zero return\n");
//                    break;
//                case SSL_ERROR_SSL:
//                    fprintf(stderr, "ssl error\n");
//                    break;
//                default:
//                    fprintf(stderr, "err %i\n", SSL_get_error(ssl, sslaret));
//                    break;
//            }
//            return 1;
//        }

        printf("\nACCEPTED CLIENT:\n");

        char buf[BUFSIZ] = {0};
        int red;
        do 
        { 
            //red = SSL_read(ssl, buf, BUFSIZ-1);
            red = read(pfd, buf, BUFSIZ-1);
            write(STDOUT_FILENO, buf, red);
        } while (red);

        //SSL_shutdown(ssl);

        printf("\nCLOSED CLIENT\n");
    //}

    return 0;
}

