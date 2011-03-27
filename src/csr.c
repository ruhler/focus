
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "ccl.h"
#include "csr.h"

int csr_update(Client client, int* x, int* y, int* w, int* h, UpdateFunction f, void* ud)
{
    unsigned int header[4] = {0};
    if (fread(header, 4, 4, client->fr) < 4) {
        return 0;
    }

    *x = header[0];
    *y = header[1];
    *w = header[2];
    *h = header[3];
    int r = 0;
    int c = 0;
    for (r = 0; r < *h; r++) {
        for (c = 0; c < *w; c++) {
            Color col;
            fread(&col, 4, 1, client->fr);
            f(ud, c+*x, r+*y, col);
        }
    }

    return 1;
}

void csr_event(Client c, Event e)
{
    switch (e.type) {
        case EVENT_KEYPRESS:
            fprintf(c->fw, "P%02X", e.value);
            fflush(c->fw);
            break;

        case EVENT_KEYRELEASE:
            fprintf(c->fw, "R%02X", e.value);
            fflush(c->fw);
            break;
    }
}

void csr_close(Client c)
{
    fclose(c->fr);
    fclose(c->fw);
    free(c);
}

Client csr_launch(const char* path, char* const args[])
{
    int stoc[2];
    int ctos[2];

    if (pipe(stoc) < 0) {
        perror("pipe");
        return NULL;
    }

    if (pipe(ctos) < 0) {
        perror("pipe");
        return NULL;
    }

    pid_t pid = fork();
    if (pid < 0) {
        perror("fork");
        return NULL;
    }

    if (pid == 0) {
        if (dup2(stoc[0], STDIN_FILENO) < 0) {
            perror("dup2");
            return NULL;
        }
        close(stoc[0]);
        close(stoc[1]);

        if (dup2(ctos[1], STDOUT_FILENO) < 0) {
            perror("dup2");
            return NULL;
        }
        close(ctos[0]);
        close(ctos[1]);

        if (execvp(path, args) < 0) {
            perror("execvp");
            return NULL;
        }

    } else {
        close(stoc[0]);
        close(ctos[1]);

        int fdw = stoc[1];
        int fdr = ctos[0];

        Client c = malloc(sizeof(Client));
        if (!c) {
            return NULL;
        }
        
        c->fw = fdopen(fdw, "w");
        c->fr = fdopen(fdr, "r");
        if (!c->fw || !c->fr) {
            free(c);
            return NULL;
        }

        return c;
    }

    return NULL;
}

