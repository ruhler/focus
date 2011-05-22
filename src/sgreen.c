
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <pthread.h>

#include "consoler.h"

#define MAX_WINDOWS 10

typedef struct {
    CNSL_Client client;
    CNSL_Display display;
} ClientInfo;

// Displays for each window.
// The display is NULL if the window is not in use.
ClientInfo g_clients[MAX_WINDOWS];

// The currently active window.
int g_curwin;
int done = 0;
int lsfd = 0;

void switch_to_window(int windowid)
{
    if (g_clients[windowid].client) {
        g_curwin = windowid;
        CNSL_Display b = g_clients[windowid].display;
        CNSL_SendDisplay(stdcon, b, 0, 0, 0, 0, b->width, b->height);
    }
}

// Handle output 
// Input is pointer to integer specifying which output to handle.
// That pointer will be freed by this function, so make sure it's been
// malloced to begin with.
void* handle_output(void* vwid) {
    int id = *(int*)vwid;
    free(vwid);

    assert(g_clients[id].client && "tried to handle output of NULL client");
    fprintf(stderr, "handling output for client %i\n", id);

    CNSL_Client client = g_clients[id].client;
    CNSL_Display display = g_clients[id].display;

    while (1) {
        int x, y, w, h;
        if (CNSL_RecvDisplay(client, &x, &y, &w, &h, CNSL_RDToDisplay, (void*)display) == 0) {
            break;
        }

        if (id == g_curwin) {
            CNSL_SendDisplay(stdcon, display, x, y, x, y, w, h);
        }
    }

    g_clients[id].client = NULL;
    g_clients[id].display = NULL;

    CNSL_CloseClient(client);
    CNSL_FreeDisplay(display);

    if (id == g_curwin) {
        int i;
        for (i = 0; i < MAX_WINDOWS; i++) {
            if (g_clients[i].client) {
                switch_to_window(i);
                return;
            }
        }

        // we are out of clients. Time to stop.
        fprintf(stderr, "sgreen: all clients finished\n");
        done = 1;
    }

    return NULL;
}

void new_client(char* const argv[])
{
    int id = -1;
    int i;
    for (i = 0; i < MAX_WINDOWS; i++) {
        if (g_clients[i].client == NULL) {
            id = i;
            break;
        }
    }


    if (id == -1) {
        fprintf(stderr, "exceeded max clients\n");
        return;
    }
    fprintf(stderr, "sgreen: new client (id = %i)\n", id);

    int width = 640;
    int height = 480;
    CNSL_GetGeometry(&width, &height);

    g_clients[id].client = CNSL_LaunchClient(argv[0], argv);
    g_clients[id].display = CNSL_AllocDisplay(width, height);
    switch_to_window(id);

    // Spawn the thread to handle output from this client.
    pthread_t thread;
    int* vid = (int*)malloc(sizeof(int));
    assert(vid && "malloc failed");
    *vid = id;
    pthread_create(&thread, NULL, &handle_output, (void*)vid);
}

void new_shellclient()
{
    char* argv[] = {"./build/src/termer/termer", "./build/src/termer/termer", NULL};
    new_client(argv);
}

void* serve_clients(void* arg)
{
    // Start listening on /tmp/green for client connections.
    lsfd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (lsfd < 0) {
        perror("socket");
        done = 1;
        return NULL;
    }

    struct sockaddr_un inaddr;
    inaddr.sun_family = AF_UNIX;
    strcpy(inaddr.sun_path, "/tmp/green");

    if (bind(lsfd, (struct sockaddr *) &inaddr, sizeof(struct sockaddr_un)) < 0) {
        perror("bind");
        done = 1;
        return NULL;
    }

    if (listen(lsfd, 3) < 0) {
        perror("listen");
        done = 1;
        return NULL;
    }

    while (1) {
        struct sockaddr_un paddr;
        socklen_t paddr_size;
        int pfd = accept(lsfd, (struct sockaddr*) &paddr, &paddr_size);
        if (pfd < 0) {
            perror("accept");
            done = 1;
            return NULL;
        }

        FILE* pf = fdopen(pfd, "r");
        if (!pf) {
            perror("fdopen");
            done = 1;
            return NULL;
        }

        unsigned int numargs;
        fread(&numargs, sizeof(unsigned int), 1, pf);
        char** argv = (char**)malloc(sizeof(char*)*(numargs+1));
        assert(argv && "malloc failed");

        int i;
        for (i = 0; i < numargs; i++) {
            unsigned int len;
            char buf[BUFSIZ+1];

            fread(&len, sizeof(unsigned int), 1, pf);
            argv[i] = malloc(sizeof(char)*(len+1));
            assert(argv[i] && "malloc failed");

            fread(argv[i], sizeof(char), len, pf);
            argv[i][len] = '\0';
        }
        argv[i] = NULL;
        fclose(pf);

        new_client(argv);
    }

    return NULL;
}

void handle_input()
{
    CNSL_Event event;
    int ctrlon = 0;
    int commandpending = 0;
    while (!done) {
        CNSL_RecvEvent(stdcon, &event);
        int sym;

        if (CNSL_IsKeypress(&event, &sym) && (sym == CNSLK_LCTRL || sym == CNSLK_RCTRL)) {
            ctrlon = 1;
        }

        if (CNSL_IsKeyrelease(&event, &sym) && (sym == CNSLK_LCTRL || sym == CNSLK_RCTRL)) {
            ctrlon = 0;
        }

        // (for now: number keys choose the window)
        if (ctrlon && CNSL_IsKeypress(&event, &sym) && sym == CNSLK_QUOTE) {
            // This is a control sequence. Mark it.
            commandpending = 1;
        } else if (commandpending && CNSL_IsKeypress(&event, &sym)) {
            if (sym >= CNSLK_0 && sym <= CNSLK_9) {
                switch_to_window(sym - CNSLK_0);
            } else if (sym == CNSLK_c) {
                new_shellclient();
            }

            commandpending = 0;
        } else {
            // Forward the event to the current window
            if (g_clients[g_curwin].client) {
                CNSL_SendEvent(g_clients[g_curwin].client, &event);
            }
        }
    }
}

int main(int argc, char* argv[])
{
    CNSL_Init();

    int i;
    for (i = 0; i < MAX_WINDOWS; i++) {
        g_clients[i].client = NULL;
        g_clients[i].display = NULL;
    }
    g_curwin = 0;


    pthread_t scthread;
    pthread_create(&scthread, NULL, &serve_clients, NULL);

    new_shellclient();
    handle_input();

    unlink("/tmp/green");
    CNSL_Quit();
    return 0;
}

