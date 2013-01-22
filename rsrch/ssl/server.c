
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
    inaddr.sin_port = htons(5234);
    inaddr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);

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

int main (int argc, char* arv[])
{
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

        printf("\nACCEPTED CLIENT:\n");

        char buf[BUFSIZ] = {0};
        int red;
        do 
        { 
            red = read(pfd, buf, BUFSIZ-1);
            write(STDOUT_FILENO, buf, red);
        } while (red);

        printf("\nCLOSED CLIENT\n");
    //}

    return 0;
}

