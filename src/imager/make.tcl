
make::all {
    execv gcc -o imager $::CONSOLER_CFLAGS imagermain.c imager.c pnger.c jpeger.c $::CONSOLER_LIBS -DPACKAGE_VERSION="$::VERSION" -lpng -ljpeg
    execv a2x -v -f manpage -a VERSION=$::VERSION imager.1.txt
}

make::check {
    execv gcc -o imagertest $::CONSOLER_CFLAGS imagertest.c $::CONSOLER_LIBS -lpng -ljpeg
    execv ./imagertest
}

make::install {
    cmd::install $::PREFIX/bin imager
    cmd::install $::PREFIX/share/man/man1 imager.1
}

make::clean execv rm -f imager imager.1 imagertest

