

execv g++ -o pdfer $::FONTER_CFLAGS $::POPPLER_CFLAGS pdfermain.cpp pdfer.cpp $::FONTER_LIBS $::POPPLER_LIBS -DPACKAGE_VERSION="$::VERSION"


if $::CHECK {
    execv gcc -o pdfertest $::CONSOLER_CFLAGS pdfertest.c $::CONSOLER_LIBS
    execv -ignorestderr ./pdfertest
}

execv a2x -v -f manpage -a VERSION=$::VERSION pdfer.1.txt

if $::INSTALL {
    install $::PREFIX/bin pdfer
    install $::PREFIX/share/man/man1 pdfer.1
}

