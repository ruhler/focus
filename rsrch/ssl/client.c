
#include <openssl/ssl.h>

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/un.h>
#include <unistd.h>

#define UNIX_PATH_MAX 108


int main(int argc, char* argv[])
{
    SSL_load_error_strings();
    SSL_library_init();

    // Connect to the server.
    int sfd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (sfd < 0) {
        perror("socket");
        return 1;
    }

    struct sockaddr_un saddr;
    saddr.sun_family = AF_UNIX;
    strncpy(saddr.sun_path, "foo.sock", UNIX_PATH_MAX);
    saddr.sun_path[UNIX_PATH_MAX-1] = '\0';


    if (connect(sfd, (struct sockaddr*) &saddr, sizeof(struct sockaddr_un)) < 0) {
        perror("connect");
        return 1;
    }

//    SSL_METHOD* method = SSLv3_client_method();
//    if (!method) {
//        perror("SSLv3_client_method");
//        return 1;
//    }
//
//    SSL_CTX* ctx = SSL_CTX_new(method);
//    if (!ctx) {
//        perror("SSL_ctx");
//        return 1;
//    }
//
//    SSL* ssl = SSL_new(ctx);
//    if (!ssl) {
//        perror("SSL_new");
//        return 1;
//    }
//
//    if (!SSL_set_fd(ssl, sfd)) {
//        perror("SSL_set_fd");
//        return 1;
//    }
//
//    int sslcret = SSL_connect(ssl);
//    if (sslcret < 0) {
//        switch (SSL_get_error(ssl, sslcret)) {
//            case SSL_ERROR_ZERO_RETURN:
//                fprintf(stderr, "zero return\n");
//                break;
//            case SSL_ERROR_SSL:
//                fprintf(stderr, "ssl error\n");
//                break;
//            default:
//                fprintf(stderr, "err %i\n", SSL_get_error(ssl, sslcret));
//                break;
//        }
//        return 1;
//    }

    char buf[BUFSIZ] = {0};
    int red;
    do 
    { 
        red = read(STDIN_FILENO, buf, BUFSIZ-1);
        write(sfd, buf, red);
        //SSL_write(ssl, buf, red);
    } while (red);

    //SSL_shutdown(ssl);
    
    close(sfd);
    return 0;
}

