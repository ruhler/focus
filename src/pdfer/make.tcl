
make::all {
    execv g++ -o pdfer $::FONTER_CFLAGS $::POPPLER_CFLAGS pdfermain.cpp pdfer.cpp $::FONTER_LIBS $::POPPLER_LIBS -DPACKAGE_VERSION="$::VERSION"
    execv a2x -v -f manpage -a VERSION=$::VERSION pdfer.1.txt
}

make::check {
    execv gcc -o pdfertest $::CONSOLER_CFLAGS pdfertest.c $::CONSOLER_LIBS
    execv -ignorestderr ./pdfertest
}


make::install {
    cmd::install $::PREFIX/bin pdfer
    cmd::install $::PREFIX/share/man/man1 pdfer.1
}

make::clean execv rm -f pdfer pdfer.1 pdfertest

