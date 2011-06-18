
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
    if (argc > 1 && strcmp(argv[1], "--version") == 0) {
        printf("cgreen %s\n", FOCUS_VERSION_STRING);
        return 0;
    }

    int dofork = 1;

    if (argc > 1 && strcmp(argv[1], "-e") == 0) {
        dofork = 0;
        argc--;
        argv++;
    }

    unsigned int pargc = argc-1;
    char** pargv = argv+1;

    const char* server = getenv("GREENSVR");
    if (server == NULL) {
        fprintf(stderr, "GREENSVR is not set.\n");
        return 1;
    }

    // Connect to the server.
    int sfd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (sfd < 0) {
        perror("socket");
        return 1;
    }

    struct sockaddr_un saddr;
    saddr.sun_family = AF_UNIX;
    strncpy(saddr.sun_path, server, UNIX_PATH_MAX);
    saddr.sun_path[UNIX_PATH_MAX-1] = '\0';


    if (connect(sfd, (struct sockaddr*) &saddr, sizeof(struct sockaddr_un)) < 0) {
        perror("connect");
        return 1;
    }
    
    if (dup2(sfd, STDIN_FILENO) < 0) {
        perror("dup2");
        return 1;
    }

    if (dup2(sfd, STDOUT_FILENO) < 0) {
        perror("dup2");
        return 1;
    }

    if (dofork) {
        pid_t pid = fork();
        if (pid < 0) {
            perror("fork");
            return 1;
        } else if (pid > 0) {
            return 0;
        }
    }

    if (execvp(pargv[0], pargv) < 0) {
        perror("execvp");
        return 1;
    }
    return 0;
}

