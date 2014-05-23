
include_HEADERS = consoler.h consoler_keysym.h

lib_LIBRARIES = libconsoler.a
libconsoler_a_SOURCES = consoler.c

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = consoler.pc

TESTS = consolertest

check_PROGRAMS = consolertest
consolertest_SOURCES = consolertest.c
consolertest_LDADD = libconsoler.a

