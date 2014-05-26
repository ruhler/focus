
bin_PROGRAMS = filler

filler_SOURCES = main.c filler.c eventer.c eventer.h filler.h
filler_LDADD = ../consoler/libconsoler.a
filler_CPPFLAGS = -I../consoler

TESTS = fillertest

check_PROGRAMS = fillertest
fillertest_SOURCES = fillertest.c
fillertest_CPPFLAGS = -I../consoler
fillertest_LDADD = ../consoler/libconsoler.a

man_MANS = filler.1

SUFFIXES = .1.txt .1
.1.txt.1:
	a2x -v -f manpage -a VERSION=$(VERSION) $<
