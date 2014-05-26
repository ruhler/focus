
bin_PROGRAMS = pdfer

pdfer_SOURCES = pdfermain.cpp pdfer.cpp
pdfer_CPPFLAGS = -I../consoler -I../fonter $(FONTCONFIG_CFLAGS) $(FREETYPE_CFLAGS) $(POPPLER_CFLAGS)
pdfer_LDADD = ../consoler/libconsoler.a ../fonter/libfonter.a $(FONTCONFIG_LIBS) $(FREETYPE_LIBS) $(POPPLER_LIBS)

TESTS = pdfertest

check_PROGRAMS = pdfertest

pdfertest_SOURCES = pdfertest.c
pdfertest_CPPFLAGS = -I../consoler
pdfertest_LDADD = ../consoler/libconsoler.a

man_MANS = pdfer.1

SUFFIXES = .1.txt .1
.1.txt.1:
	a2x -v -f manpage -a VERSION=$(VERSION) $<

