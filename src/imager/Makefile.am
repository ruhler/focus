

bin_PROGRAMS = imager

imager_SOURCES = imagermain.c imager.c pnger.c jpeger.c imager.h jpeger.h pnger.h
imager_CPPFLAGS = -I../consoler
imager_LDADD = ../consoler/libconsoler.a

TESTS = imagertest

check_PROGRAMS = imagertest

imagertest_SOURCES = imagertest.c
imagertest_CPPFLAGS = -I../consoler
imagertest_LDADD = ../consoler/libconsoler.a

man_MANS = imager.1

SUFFIXES = .1.txt .1
.1.txt.1:
	a2x -v -f manpage -a VERSION=$(VERSION) $<
