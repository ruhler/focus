

bin_PROGRAMS = sdlcsr

sdlcsr_SOURCES = sdlcsr.c
sdlcsr_CPPFLAGS = -I../consoler $(SDL_CFLAGS)
sdlcsr_LDADD = ../consoler/libconsoler.a $(SDL_LIBS)

man_MANS = sdlcsr.1

SUFFIXES = .1.txt .1
.1.txt.1:
	a2x -v -f manpage -a VERSION=$(VERSION) $<
