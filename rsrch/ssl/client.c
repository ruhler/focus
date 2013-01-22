
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <netdb.h>
#include <unistd.h>

#define UNIX_PATH_MAX 108


int main(int argc, char* argv[])
{
    // Connect to the server.
    int sfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sfd < 0) {
        perror("socket");
        return 1;
    }

    struct hostent* server = gethostbyname("localhost");
    if (server == NULL)
    {
        perror("gethostbyname");
        return 1;
    }

    struct sockaddr_in saddr;
    saddr.sin_family = AF_INET;
    saddr.sin_port = htons(5234);
    bcopy((char*)server->h_addr,
          (char*)&saddr.sin_addr.s_addr,
          server->h_length);

    if (connect(sfd, (struct sockaddr*) &saddr, sizeof(struct sockaddr_in)) < 0) {
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

