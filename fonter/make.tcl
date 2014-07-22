
set ::FONTER_CFLAGS "-I[pwd] $::CONSOLER_CFLAGS $::FREETYPE_CFLAGS $::FONTCONFIG_CFLAGS"
set ::FONTER_LIBS "$::CONSOLER_LIBS -L[pwd] -lfonter $::FREETYPE_LIBS $::FONTCONFIG_LIBS -lm"

make::all {
    execv gcc -c -o fonter.o $::FONTER_CFLAGS fonter.c -DPACKAGE_VERSION="$::VERSION"
    execv ar cr libfonter.a fonter.o
    execv gcc -o fonterexample $::FONTER_CFLAGS fonterexample.c $::FONTER_LIBS
}

librarydoc fonter.h
cmd::pkgconfig fonter

make::check {
    execv gcc -o fontertest $::FONTER_CFLAGS fontertest.c $::FONTER_LIBS
    execv ./fontertest
}

make::install {
    cmd::install $::PREFIX/include fonter.h
    cmd::install $::PREFIX/lib libfonter.a
}

make::clean execv rm -f fonter.o fonterexample fontertest libfonter.a
