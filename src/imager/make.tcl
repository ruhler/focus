
execv gcc -o imager $::CONSOLER_CFLAGS imagermain.c imager.c pnger.c jpeger.c $::CONSOLER_LIBS -DPACKAGE_VERSION="$::VERSION" -lpng -ljpeg


if $::CHECK {
    execv gcc -o imagertest $::CONSOLER_CFLAGS imagertest.c $::CONSOLER_LIBS -lpng -ljpeg
    execv ./imagertest
}

execv a2x -v -f manpage -a VERSION=$::VERSION imager.1.txt

if $::INSTALL {
    install $::PREFIX/bin imager
    install $::PREFIX/share/man/man1 imager.1
}

