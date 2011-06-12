
#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/un.h>
#include <pthread.h>

#include "consoler.h"
#include "fonter.h"

#define MAX_WINDOWS 10
#define UNIX_PATH_MAX    108

typedef struct {
    bool valid;
    CNSL_Client client;
    CNSL_Display display;
} ClientInfo;

// Displays for each window.
ClientInfo g_clients[MAX_WINDOWS];

// fonter object for drawing the status bar.
FNTR_Fonter g_fonter;

// The currently active window.
int g_curwin;
bool done = false;

// switch to the given window.
// Does nothing if that window is not valid.
void switch_to_window(int windowid)
{
    if (g_clients[windowid].valid) {
        g_curwin = windowid;
        CNSL_Display b = g_clients[windowid].display;
        CNSL_SendDisplay(stdcon, b, 0, 0, 0, 0, b.width, b.height);
    }
}

// Handle output 
// Input is pointer to integer specifying which output to handle.
// That pointer will be freed by this function, so make sure it's been
// malloced to begin with.
void* handle_output(void* vwid)
{
    int id = *(int*)vwid;
    free(vwid);

    assert(g_clients[id].valid && "tried to handle output of invalid client");

    CNSL_Client client = g_clients[id].client;
    CNSL_Display display = g_clients[id].display;

    int x, y, w, h;
    bool recved = CNSL_RecvDisplay(client, display, &x, &y, &w, &h);
    while (recved) {
        if (id == g_curwin) {
            CNSL_SendDisplay(stdcon, display, x, y, x, y, w, h);
        }
        recved = CNSL_RecvDisplay(client, display, &x, &y, &w, &h);
    }

    g_clients[id].valid = false;

    CNSL_CloseClient(client);
    CNSL_FreeDisplay(display);

    if (id == g_curwin) {
        int i;
        for (i = 0; i < MAX_WINDOWS; i++) {
            if (g_clients[i].valid) {
                switch_to_window(i);
                return;
            }
        }

        // we are out of clients. Time to stop.
        done = true;
    }

    return NULL;
}

void new_client(CNSL_Client client)
{
    int id = -1;
    int i;
    for (i = 0; i < MAX_WINDOWS; i++) {
        if (!g_clients[i].valid) {
            id = i;
            break;
        }
    }

    if (id == -1) {
        fprintf(stderr, "exceeded max clients\n");
        return;
    }

    g_clients[id].valid = true;
    g_clients[id].client = client;

    int width = 640;
    int height = 480;
    CNSL_GetGeometry(&width, &height);
    g_clients[id].display = CNSL_AllocDisplay(width, height);

    // clear the display
    int x, y;
    for (y = 0; y < height; y++) {
        for (x = 0; x < width; x++) {
            CNSL_SetPixel(g_clients[id].display, x, y, CNSL_MakeColor(0, 0, 0));
        }
    }

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
    char* shellclient = getenv("CNSLSHELL");
    if (!shellclient) {
        shellclient = "termer";
    }

    char* argv[] = {shellclient, NULL};
    CNSL_Client client = CNSL_LaunchClient(shellclient, argv);
    new_client(client);
}

int start_server()
{
    int lsfd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (lsfd < 0) {
        perror("socket");
        done = true;
        return -1;
    }

    struct sockaddr_un inaddr;
    inaddr.sun_family = AF_UNIX;
    snprintf(inaddr.sun_path, UNIX_PATH_MAX, "/tmp/green-%u.%i", getenv("USER"), getpid());
    setenv("GREENSVR", inaddr.sun_path, 1);

    if (bind(lsfd, (struct sockaddr *) &inaddr, sizeof(struct sockaddr_un)) < 0) {
        perror("bind");
        done = true;
        return -1;
    }

    if (listen(lsfd, 3) < 0) {
        perror("listen");
        done = true;
        return -1;
    }
    return lsfd;
} 

void* serve_clients(int* lsfdp)
{
    int lsfd = *lsfdp;
    while (1) {
        struct sockaddr_un paddr;
        socklen_t paddr_size;
        int pfd = accept(lsfd, (struct sockaddr*) &paddr, &paddr_size);
        if (pfd < 0) {
            perror("accept");
            done = true;
            return NULL;
        }

        CNSL_Client client;
        client.fdout = pfd;
        client.fdin = pfd;
        new_client(client);
    }

    return NULL;
}

void handle_input()
{
    CNSL_Event event;
    bool ctrlon = false;
    bool commandpending = false;
    while (!done) {
        event = CNSL_RecvEvent(stdcon);
        int sym;

        if (CNSL_IsKeypress(event, &sym) && (sym == CNSLK_LCTRL || sym == CNSLK_RCTRL)) {
            ctrlon = true;
        }

        if (CNSL_IsKeyrelease(event, &sym) && (sym == CNSLK_LCTRL || sym == CNSLK_RCTRL)) {
            ctrlon = false;
        }

        // (for now: number keys choose the window)
        if (ctrlon && CNSL_IsKeypress(event, &sym) && sym == CNSLK_QUOTE) {
            // This is a control sequence. Mark it.
            commandpending = true;
        } else if (commandpending && CNSL_IsKeypress(event, &sym)) {
            if (sym >= CNSLK_0 && sym <= CNSLK_9) {
                switch_to_window(sym - CNSLK_0);
            } else if (sym == CNSLK_c) {
                new_shellclient();
            } else if (sym == CNSLK_w) {
                // Print out which windows are active.
                char str[] = "0  1  2  3  4  5  6  7  8  9";
                int i;
                for (i = 0; i < MAX_WINDOWS; i++) {
                    if (!g_clients[i].valid) {
                        str[3*i] = '_';
                    }
                }
                str[3*g_curwin + 1] = '*';
                CNSL_Color fg = CNSL_MakeColor(0xFF, 0xFF, 0xFF);
                CNSL_Color bg = CNSL_MakeColor(0x00, 0x00, 0x80);
                CNSL_Display d = g_clients[g_curwin].display;
                int y = d.height - FNTR_Height(g_fonter);
                FNTR_DrawString(g_fonter, d, fg, bg, 0, y, str);
                CNSL_SendDisplay(stdcon, d, 0, y, 0, y, d.width, FNTR_Height(g_fonter));
            }

            commandpending = false;
        } else {
            // Forward the event to the current window
            if (g_clients[g_curwin].valid) {
                CNSL_SendEvent(g_clients[g_curwin].client, event);
            }
        }
    }
}

int main(int argc, char* argv[])
{
    g_fonter = FNTR_Create("Monospace-24:Bold");

    int i;
    for (i = 0; i < MAX_WINDOWS; i++) {
        g_clients[i].valid = false;
    }
    g_curwin = 0;


    int lsfd = start_server();
    if (lsfd < 0) {
        return 1;
    }

    pthread_t scthread;
    pthread_create(&scthread, NULL, (void* (*)(void*))&serve_clients, &lsfd);

    new_shellclient();
    handle_input();

    unlink(getenv("GREENSVR"));
    return 0;
}

