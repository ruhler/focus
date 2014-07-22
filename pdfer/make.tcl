
make::all execv g++ -o pdfer $::FONTER_CFLAGS $::POPPLER_CFLAGS pdfermain.cpp pdfer.cpp $::FONTER_LIBS $::POPPLER_LIBS -DPACKAGE_VERSION="$::VERSION"

cmd::man1 pdfer

make::check {
    execv gcc -o pdfertest $::CONSOLER_CFLAGS pdfertest.c $::CONSOLER_LIBS
    execv -ignorestderr ./pdfertest
}


make::install cmd::install $::PREFIX/bin pdfer
make::clean execv rm -f pdfer pdfertest

