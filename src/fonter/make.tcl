
set ::FONTER_CFLAGS "-I[pwd] $::CONSOLER_CFLAGS $::FREETYPE_CFLAGS $::FONTCONFIG_CFLAGS"
set ::FONTER_LIBS "$::CONSOLER_LIBS -L[pwd] -lfonter $::FREETYPE_LIBS $::FONTCONFIG_LIBS -lm"

# libfonter.a
execv gcc -c -o fonter.o $::FONTER_CFLAGS fonter.c -DPACKAGE_VERSION="$::VERSION"
execv ar cr libfonter.a fonter.o

if $::CHECK {
    execv gcc -o fontertest $::FONTER_CFLAGS fontertest.c $::FONTER_LIBS
    execv ./fontertest
}

execv gcc -o fonterexample $::FONTER_CFLAGS fonterexample.c $::FONTER_LIBS

# fonter.pc
exec sed -e s=@PACKAGE_VERSION@=$::VERSION= -e s=@prefix@=$::PREFIX= fonter.pc.in > fonter.pc

librarydoc fonter.h

if $::INSTALL {
    install $::PREFIX/include fonter.h
    install $::PREFIX/lib libfonter.a
    install $::PREFIX/lib/pkgconfig fonter.pc
}


