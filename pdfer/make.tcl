
cmd::prog pdfer { g++ -o pdfer $::FONTER_CFLAGS $::POPPLER_CFLAGS pdfermain.cpp pdfer.cpp $::FONTER_LIBS $::POPPLER_LIBS -DPACKAGE_VERSION="$::VERSION" }
cmd::test pdfertest { gcc -o pdfertest $::CONSOLER_CFLAGS pdfertest.c $::CONSOLER_LIBS } ./pdfertest
cmd::man1 pdfer

