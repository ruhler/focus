
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <pthread.h>

#include "ccl.h"
#include "csr.h"

#define MAX_WINDOWS 10
#define WIDTH 640
#define HEIGHT 480

typedef struct {
    Client client;
    Buffer buffer;
} ClientInfo;

// Buffers for each window.
// The buffer is NULL if the window is not in use.
ClientInfo g_clients[MAX_WINDOWS];

// The currently active window.
int g_curwin;

void switch_to_window(int windowid)
{
    fprintf(stderr, "switching to window %i\n", windowid);
    if (g_clients[windowid].client) {
        g_curwin = windowid;
        Buffer b = g_clients[windowid].buffer;
        ccl_blit(b, 0, 0, 0, 0, b->width, b->height);
    }
}

void tobuffer(void* vb, int x, int y, Color c)
{
    Buffer b = (Buffer)vb;
    if (x >= 0 && x < b->width && y >= 0 && y < b->height) {
        ccl_setpixel(b, x, y, c);
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

    Client client = g_clients[id].client;
    Buffer buffer = g_clients[id].buffer;

    while (1) {
        int x, y, w, h;
        if (csr_update(client, &x, &y, &w, &h, tobuffer, (void*)buffer) == 0) {
            break;
        }

        if (id == g_curwin) {
            ccl_blit(buffer, x, y, x, y, w, h);
        }
    }

    g_clients[id].client = NULL;
    g_clients[id].buffer = NULL;

    csr_close(client);
    ccl_free_buffer(buffer);

    if (id == g_curwin) {
        int i;
        for (i = 0; i < MAX_WINDOWS; i++) {
            if (g_clients[i].client) {
                g_curwin = i;
                break;
            }
        }
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

    g_clients[id].client = csr_launch(argv[0], argv);
    g_clients[id].buffer = ccl_alloc_buffer(WIDTH, HEIGHT);
    switch_to_window(id);

    // Spawn the thread to handle output from this client.
    pthread_t thread;
    int* vid = (int*)malloc(sizeof(int));
    assert(vid && "malloc failed");
    *vid = id;
    pthread_create(&thread, NULL, &handle_output, (void*)vid);
}

void* serve_clients(void* arg)
{
    // Start listening on /tmp/green for client connections.
    int lsfd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (lsfd < 0) {
        perror("socket");
        return NULL;
    }

    struct sockaddr_un inaddr;
    inaddr.sun_family = AF_UNIX;
    strcpy(inaddr.sun_path, "/tmp/green");

    if (bind(lsfd, (struct sockaddr *) &inaddr, sizeof(struct sockaddr_un)) < 0) {
        perror("bind");
        return NULL;
    }

    if (listen(lsfd, 3) < 0) {
        perror("listen");
        return NULL;
    }

    while (1) {
        struct sockaddr_un paddr;
        socklen_t paddr_size;
        int pfd = accept(lsfd, (struct sockaddr*) &paddr, &paddr_size);
        if (pfd < 0) {
            perror("accept");
            return NULL;
        }

        FILE* pf = fdopen(pfd, "r");
        if (!pf) {
            perror("fdopen");
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
    Event event;
    while (1) {
        ccl_event(&event);
        int sym;

        // Check for a control sequence.
        // (for now: number keys choose the window)
        if (ccl_keypress(&event, &sym) && sym >= CCLK_0 && sym <= CCLK_9) {
            // Switch to the given window
            switch_to_window(sym - CCLK_0);
        } else {
            // Forward the event to the current window
            if (g_clients[g_curwin].client) {
                csr_event(g_clients[g_curwin].client, event);
            }
        }
    }
}

int main(int argc, char* argv[])
{
    int i;
    for (i = 0; i < MAX_WINDOWS; i++) {
        g_clients[i].client = NULL;
        g_clients[i].buffer = NULL;
    }
    g_curwin = 0;


    pthread_t scthread;
    pthread_create(&scthread, NULL, &serve_clients, NULL);

    handle_input();
    return 0;
}

