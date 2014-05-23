
include_HEADERS = fonter.h

lib_LIBRARIES = libfonter.a
libfonter_a_SOURCES = fonter.c
libfonter_a_CFLAGS = -I../consoler $(FREETYPE_CFLAGS) $(FONTCONFIG_CFLAGS)

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = fonter.pc

TESTS = fontertest

check_PROGRAMS = fontertest
fontertest_SOURCES = fontertest.c
fontertest_CPPFLAGS = -I../consoler $(FREETYPE_CFLAGS) $(FONTCONFIG_CFLAGS)
fontertest_LDADD = $(FREETYPE_LIBS) $(FONTCONFIG_LIBS) ../consoler/libconsoler.a libfonter.a

