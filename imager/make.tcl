
cmd::prog imager { gcc -o imager $::CONSOLER_CFLAGS imagermain.c imager.c pnger.c jpeger.c $::CONSOLER_LIBS -DPACKAGE_VERSION="$::VERSION" -lpng -ljpeg }
cmd::man1 imager

make::check {
    execv gcc -o imagertest $::CONSOLER_CFLAGS imagertest.c $::CONSOLER_LIBS -lpng -ljpeg
    execv ./imagertest
}

make::clean execv rm -f imagertest

