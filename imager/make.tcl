
cmd::prog imager { gcc -o imager $::CONSOLER_CFLAGS imagermain.c imager.c pnger.c jpeger.c $::CONSOLER_LIBS -DPACKAGE_VERSION="$::VERSION" -lpng -ljpeg }
cmd::test imagertest {gcc -o imagertest $::CONSOLER_CFLAGS imagertest.c $::CONSOLER_LIBS -lpng -ljpeg } ./imagertest
cmd::man1 imager

