
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>


int main(int argc, char* argv[])
{
    // Start listening on /tmp/green for client connections.
    int lsfd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (lsfd < 0) {
        perror("socket");
        return 1;
    }

    struct sockaddr_un inaddr;
    inaddr.sun_family = AF_UNIX;
    strcpy(inaddr.sun_path, "/tmp/green");

    if (bind(lsfd, (struct sockaddr *) &inaddr, sizeof(struct sockaddr_un)) < 0) {
        perror("bind");
        return 1;
    }

    if (listen(lsfd, 3) < 0) {
        perror("listen");
        return 1;
    }

    while (1) {
        struct sockaddr_un paddr;
        socklen_t paddr_size;
        int pfd = accept(lsfd, (struct sockaddr*) &paddr, &paddr_size);
        if (pfd < 0) {
            perror("accept");
            return 1;
        }

        printf("accepted client connection\n");

        FILE* pf = fdopen(pfd, "r");
        if (!pf) {
            perror("fdopen");
            return 1;
        }

        unsigned int numargs;
        fread(&numargs, sizeof(unsigned int), 1, pf);
        int i;
        for (i = 0; i < numargs; i++) {
            unsigned int len;
            char buf[BUFSIZ+1];

            fread(&len, sizeof(unsigned int), 1, pf);
            if (len > BUFSIZ) {
                len = BUFSIZ;
            }

            fread(buf, sizeof(char), len, pf);
            buf[len] = '\0';
            printf("arg %i: %s\n", i, buf);
        }

        fclose(pf);
    }

    return 0;
}

