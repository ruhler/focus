
bin_PROGRAMS = sgreen cgreen
dist_bin_SCRIPTS = rgreen

sgreen_SOURCES = sgreen.c green.c green.h
sgreen_CPPFLAGS = -I../consoler
sgreen_LDADD = ../consoler/libconsoler.a

cgreen_SOURCES = cgreen.c

man_MANS = cgreen.1 sgreen.1

SUFFIXES = .1.txt .1
.1.txt.1:
	a2x -v -f manpage -a VERSION=$(VERSION) $<
