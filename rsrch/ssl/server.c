
//#include <ssl.h>
//    SSL_library_init();
//    SSL_CTX_new();
//    SSL_new
//    SSL_set_fd
//    SSL_accept
//    SSL_connect
//    SSL_read
//    SSL_write
//    SSL_shutdown

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/un.h>

#define UNIX_PATH_MAX    108

int start_server()
{
    int lsfd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (lsfd < 0) {
        perror("socket");
        return 1;
    }

    struct sockaddr_un inaddr;
    inaddr.sun_family = AF_UNIX;
    unlink("foo.sock");
    snprintf(inaddr.sun_path, UNIX_PATH_MAX, "%s", "foo.sock");

    if (bind(lsfd, (struct sockaddr *) &inaddr, sizeof(struct sockaddr_un)) < 0) {
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
    int lsfd = start_server();
    while (1) {
        printf("Waiting for connection...");
        fflush(stdout);
        struct sockaddr_un paddr;
        socklen_t paddr_size;
        int pfd = accept(lsfd, (struct sockaddr*) &paddr, &paddr_size);
        if (pfd < 0) {
            perror("accept");
            continue;
        }

        printf("\nACCEPTED CLIENT:\n");

        char buf[BUFSIZ] = {0};
        int red;
        do 
        { 
            red = read(pfd, buf, BUFSIZ-1);
            write(STDOUT_FILENO, buf, red);
        } while (red);

        close(pfd);

        printf("\nCLOSED CLIENT\n");
    }

    return 0;
}

