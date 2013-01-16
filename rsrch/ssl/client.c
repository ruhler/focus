

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

    char buf[BUFSIZ] = {0};
    int red;
    do 
    { 
        red = read(STDIN_FILENO, buf, BUFSIZ-1);
        write(sfd, buf, red);
    } while (red);
    
    close(sfd);
    return 0;
}

