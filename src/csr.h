
#ifndef CSR_H
#define CSR_H

typedef struct {
    FILE* fr;
    FILE* fw;
} Client_;

typedef Client_* Client;

// Launch a client program, returning a handle to the client.
// Returns NULL on failure.
// Arguments are same as for exec.
Client csr_launch(const char* path, char* const args[]);

// Close a client connection.
void csr_close(Client c);

// Send an event to a client.
void csr_event(Client c, Event e);

// Get a screen update from the client.
// This is a blocking call.
// Calls function f for each pixel to be updated. Returns the updated
// rectangle via x, y, w, and h.
// Returns nonzero on success, zero on end of file, -1 on error.
typedef void (*UpdateFunction)(void* ud, int x, int y, Color c);
int csr_update(Client c, int* x, int* y, int* w, int* h, UpdateFunction f, void* ud);

#endif//CSR_H

