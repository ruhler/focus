
// Copyright (C) 2011 Richard Uhler <ruhler@member.fsf.org>
//
// This file is part of Focus.
//
// Focus is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Focus is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Focus.  If not, see <http://www.gnu.org/licenses/>.

#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <pthread.h>

#include "config.h"
#include "consoler.h"
#include "green.h"

GRN_Green green;

// Handle a client.
//  - adds it to the green object
//  - allocates a display for it
//  - receives and deals with display updates
//  - detects when client closes, and removes from green object.
void handle_client(CNSL_Client client)
{
    int width = GRN_Width(green);
    int height = GRN_Height(green);

    CNSL_Display display = CNSL_AllocDisplay(width, height);
    CNSL_FillRect(display, 0, 0, width, height, CNSL_MakeColor(0, 0, 0));

    client_id id = GRN_AddClient(green, client);
    GRN_SendDisplay(green, id, display, 0, 0, 0, 0, width, height);
    GRN_ChangeCurrent(green, id);

    unsigned int x, y, w, h = 0;
    bool recved = CNSL_RecvDisplay(client, display, &x, &y, &w, &h);
    while (recved) {
        GRN_SendDisplay(green, id, display, x, y, x, y, w, h);
        recved = CNSL_RecvDisplay(client, display, &x, &y, &w, &h);
    }

    GRN_RemoveClient(green, id);
    CNSL_FreeDisplay(display);
    CNSL_CloseClient(client);
}

// wrapper around handle_client for pthreads.
// ud should be a pointer to a malloced client.
void* handle_client_thread(void* ud)
{
    CNSL_Client* client = (CNSL_Client*)ud;
    handle_client(*client);
    free(client);
    return NULL;
}

// Yet another wrapper (I love pthreads) around handle_client.
// This one launches a handle_client thread.
void handle_client_create(CNSL_Client client)
{
    CNSL_Client* ptr = malloc(sizeof(CNSL_Client));
    *ptr = client;
    pthread_t thread;
    pthread_create(&thread, NULL, &handle_client_thread, (void*)ptr);
}

void shell_client_create()
{
    char* shellclient = getenv("CNSLSHELL");
    if (!shellclient) {
        shellclient = "termer";
    }

    char* argv[] = {shellclient, NULL};
    CNSL_Client client = CNSL_LaunchClient(shellclient, argv);
    handle_client_create(client);
}

// Start a server and return the file descriptor for it if successful,
// otherwise -1.
int start_server(const char* port)
{
    int lsfd = socket(AF_INET, SOCK_STREAM, 0);
    if (lsfd < 0) {
        perror("socket");
        return -1;
    }

    uint16_t portno;
    if (port) {
        portno = atoi(port);
    } else {
        portno = 0x1000 | getpid();
    }

    struct sockaddr_in inaddr;
    inaddr.sin_family = AF_INET;
    inaddr.sin_port = htons(portno);
    inaddr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);

    char portstr[10] = {0};
    snprintf(portstr, 10, "%i", portno);
    setenv("GREENSVR", portstr, 1);

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

void* serve_clients_thread(void* lsfdp)
{
    int lsfd = *((int*)lsfdp);
    while (1) {
        struct sockaddr_in paddr;
        socklen_t paddr_size;
        int pfd = accept(lsfd, (struct sockaddr*) &paddr, &paddr_size);
        if (pfd < 0) {
            perror("accept");
            continue;
        }

        CNSL_Client client;
        client.fdout = pfd;
        client.fdin = pfd;
        handle_client_create(client);
    }

    return NULL;
}

void handle_input()
{
    CNSL_Event event;
    bool ctrlon = false;
    bool commandpending = false;
    bool started = false;
    bool insert = true;
    while (!started || GRN_HasClients(green)) {
        event = CNSL_RecvEvent(stdcon);
        int sym;
        int w, h;

        if (CNSL_IsKeypress(event, &sym) && (sym == CNSLK_LCTRL || sym == CNSLK_RCTRL)) {
            ctrlon = true;
        }

        if (CNSL_IsKeyrelease(event, &sym) && (sym == CNSLK_LCTRL || sym == CNSLK_RCTRL)) {
            ctrlon = false;
        }

        if (insert && ctrlon && CNSL_IsKeypress(event, &sym) && sym == CNSLK_QUOTE) {
            // This is a control sequence. Mark it.
            commandpending = true;
        } else if ((!insert || commandpending) && CNSL_IsKeypress(event, &sym)) {
            if (sym >= CNSLK_0 && sym <= CNSLK_9) {
                GRN_ChangeCurrent(green, sym - CNSLK_0);
            } else if (sym == CNSLK_c) {
                shell_client_create();
            } else if (sym == CNSLK_i) {
                insert = true;
            } else if (sym == CNSLK_o) {
                insert = false;
            } else if (sym == CNSLK_s) {
                GRN_Split(green);
            } else if (sym == CNSLK_q) {
                GRN_Unsplit(green);
            } else if (sym == CNSLK_j) {
                GRN_SetFocus(green, GRN_FOCUS_BOTTOM);
            } else if (sym == CNSLK_k) {
                GRN_SetFocus(green, GRN_FOCUS_TOP);
            }

            commandpending = false;
        } else if (CNSL_IsResize(event, &w, &h)) {
            // TODO: handle this resize event.
        } else if (insert) {
            started = true;
            GRN_SendEvent(green, event);
        }
    }
}

int main(int argc, char* argv[])
{
    if (argc > 1 && strcmp(argv[1], "--version") == 0) {
        printf("sgreen %s\n", PACKAGE_VERSION);
        return 0;
    }

    if (argc > 1 && strcmp(argv[1], "--help") == 0) {
        printf("Usage: sgreen [-s port]\n");
        printf("A green server\n");
        printf("\n");
        printf("Options\n");
        printf("  --help            output this help message and exit\n");
        printf("  --version         output version information and exit\n");
        printf("  -s port           use port as the host port\n");
        printf("\n");
        return 0;
    }

    char* port = NULL;
    if (argc > 2 && strcmp(argv[1], "-s") == 0) {
        port = argv[2];
    }

    int width;
    int height;

    CNSL_Event event = CNSL_RecvEvent(stdcon);
    if (!CNSL_IsResize(event, &width, &height)) {
        fprintf(stderr, "sgreen: expected resize event. Got %i\n", event.type);
        return 1;
    }

    green = GRN_CreateGreen(width, height);


    int lsfd = start_server(port);
    if (lsfd < 0) {
        return 1;
    }

    pthread_t scthread;
    pthread_create(&scthread, NULL, (void* (*)(void*))&serve_clients_thread, &lsfd);

    shell_client_create();
    handle_input();

    close(lsfd);
    return 0;
}

