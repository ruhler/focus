
bin_PROGRAMS = boxer

boxer_SOURCES = boxer.c
boxer_LDADD = ../consoler/libconsoler.a
boxer_CPPFLAGS = -I../consoler

TESTS = boxertest

check_PROGRAMS = boxertest
boxertest_SOURCES = boxertest.c
boxertest_CPPFLAGS = -I../consoler
boxertest_LDADD = ../consoler/libconsoler.a

man_MANS = boxer.1

boxer.1: boxer.1.txt boxer.txt
	a2x -v -f manpage -a VERSION=$(VERSION) boxer.1.txt

