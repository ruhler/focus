
bin_PROGRAMS = termer

termer_SOURCES = termer.c client.c display.c inputter.c outputter.c screen.c client.h display.h inputter.h outputter.h screen.h
termer_CPPFLAGS = -I../consoler -I../fonter $(FREETYPE_CFLAGS) $(FONTCONFIG_CFLAGS)
termer_LDADD = ../consoler/libconsoler.a ../fonter/libfonter.a $(FREETYPE_LIBS) $(FONTCONFIG_LIBS)

TESTS = inputtertest outputtertest

check_PROGRAMS = inputtertest screentest outputtertest

inputtertest_SOURCES = inputtertest.c inputter.c
inputtertest_CPPFLAGS = -I../consoler
inputtertest_LDADD = ../consoler/libconsoler.a

screentest_SOURCES = screentest.c screen.c

outputtertest_SOURCES = outputtertest.c outputter.c screen.c




