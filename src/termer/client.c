
#include <pty.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "client.h"

CLIENT_Client CLIENT_Open()
{
    setenv("TERM", "termer", 1);

    CLIENT_Client client;
    pid_t pid = forkpty(&client, NULL, NULL, NULL);
    if (pid < 0) {
        perror("CLIENT_Open: forkpty");
        return -1;
    }
    
    if (pid == 0) {
        const char* shell = getenv("SHELL");
        if (!shell) {
            shell = "/bin/sh";
        }
        
        execl(shell, shell, NULL);
        perror("execl");
        exit(1);
    }
    return client;
}

void CLIENT_Close(CLIENT_Client client)
{
    close(client);
}

void CLIENT_Write(CLIENT_Client client, char c)
{
    write(client, &c, 1);
}

char client_read_buf[BUFSIZ+1];

char* CLIENT_Read(CLIENT_Client client)
{
    int red = read(client, client_read_buf, BUFSIZ);
    if (red < 0) {
        red = 0;
    }
    client_read_buf[red] = '\0';
    fprintf(stderr, "read: %s\n", client_read_buf);
    return client_read_buf;
}

