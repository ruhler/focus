
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <pthread.h>

#include "ccl.h"

#define MAX_WINDOWS 10

// Buffers for each window.
// The buffer is NULL if the window is not in use.
Buffer g_displays[MAX_WINDOWS];

// The currently active window.
int g_curwin;


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
        int i;
        for (i = 0; i < numargs; i++) {
            unsigned int len;
            char buf[BUFSIZ+1];

            fread(&len, sizeof(unsigned int), 1, pf);
            if (len > BUFSIZ) {
                len = BUFSIZ;
            }

            fread(buf, sizeof(char), len, pf);
            buf[len] = '\0';
        }

        fclose(pf);
    }

    return NULL;
}

void switch_to_window(int windowid)
{
    // Verify windowid is valid
    // Set curwin to that
    // update the display from that buffer.
}

void handle_input()
{
    Event event;
    while (1) {
        ccl_event(&event);
        int sym;

        // Check for a control sequence.
        // (for now: function keys choose the window)
        if (ccl_keypress(&event, &sym) && sym >= CCLK_F0 && sym <= CCLK_F9)
            // Switch to the given window
            switch_to_window(sym - CCLK_F0);
        } else {
            // Forward the event to the current window
        }
    }
}

int main(int argc, char* argv[])
{
    // I think C does this for us automatically, but it can't hurt to make
    // sure.
    int i;
    for (i = 0; i < MAX_WINDOWS; i++) {
        g_displays[i] = NULL;
    }
    g_curwin = 0;

    // start serving clients in a new thread.
    serve_clients();
    pthread_t scthread;
    pthread_create(&scthread, NULL, &serve_clients, NULL);

    // handle the input (in this thread).
    handle_input();
    return 0;
}

