
all: server client

server: server.c
	gcc -o $@ $< -lssl

client: client.c
	gcc -o $@ $< -lssl

