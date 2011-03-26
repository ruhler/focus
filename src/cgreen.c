
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

int main(int argc, char* argv[])
{
    unsigned int pargc = argc-1;
    char** pargv = argv+1;

    int sfd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (sfd < 0) {
        perror("socket");
        return 1;
    }

    struct sockaddr_un saddr;
    saddr.sun_family = AF_UNIX;
    strcpy(saddr.sun_path, "/tmp/green");

    if (connect(sfd, (struct sockaddr*) &saddr, sizeof(struct sockaddr_un)) < 0) {
        perror("connect");
        return 1;
    }
    printf("connected to server");

    FILE* pf = fdopen(sfd, "w");
    if (!pf) {
        perror("fdopen");
        return 1;
    }

    fwrite(&pargc, sizeof(unsigned int), 1, pf);
    int i;
    for (i = 0; i < pargc; i++) {
        unsigned int l = strlen(pargv[i]);
        fwrite(&l, sizeof(unsigned int), 1, pf);
        fwrite(pargv[i], sizeof(char), l, pf);
    }

    fclose(pf);

    printf("sent args to server\n");
    return 0;
}

