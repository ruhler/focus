
#include <fcntl.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/un.h>
#include <unistd.h>



int main(int argc, char* argv[])
{
    unsigned int pargc = argc-1;
    char** pargv = argv+1;

    // Make named pipes for client and server to communicate with.
    const char* pipename = "/tmp/cgreen";
    if (mkfifo("/tmp/cgreen.toclient", 0600) < 0) {
        perror("mkfifo");
        return 1;
    }

    if (mkfifo("/tmp/cgreen.frclient", 0600) < 0) {
        perror("mkfifo");
        unlink("/tmp/cgreen.toclient");
        return 1;
    }

    // Fork the client program.
    pid_t pid = fork();
    if (pid < 0) { 
        perror("fork");
        return 1;
    }

    if (pid == 0) {
        int nfdin = open("/tmp/cgreen.toclient", O_RDONLY);
        if (nfdin < 0) {
            perror("open");
            return 1;
        }

        int nfdout = open("/tmp/cgreen.frclient", O_WRONLY);
        if (nfdout < 0) {
            perror("open");
            return 1;
        }

        if (dup2(nfdin, STDIN_FILENO) < 0) {
            perror("dup2");
            return 1;
        }

        if (dup2(nfdout, STDOUT_FILENO) < 0) {
            perror("dup2");
            return 1;
        }

        // Exec should hopefully never return.
        if (execvp(pargv[0], pargv) < 0) {
            perror("execvp");
            return 1;
        }
    }

    // Send the prefix of the pipe name to the server.
    int sfd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (sfd < 0) {
        perror("socket");
        return 1;
    }

    struct sockaddr_un saddr;
    saddr.sun_family = AF_UNIX;
    strcpy(saddr.sun_path, "/tmp/green");

    printf("attempting to connect to /tmp/green...");
    if (connect(sfd, (struct sockaddr*) &saddr, sizeof(struct sockaddr_un)) < 0) {
        perror("connect");
        return 1;
    }
    printf("connected to server\n");
    
    write(sfd, pipename, strlen(pipename));
    close(sfd);
    return 0;
}

