
# libconsoler.a
execv gcc -c -o consoler.o consoler.c -DPACKAGE_VERSION="$::VERSION"
execv ar cr libconsoler.a consoler.o

set ::CONSOLER_CFLAGS "-I[pwd]"
set ::CONSOLER_LIBS "-L[pwd] -lconsoler"

# consolertest
if $::CHECK {
    execv gcc -o consolertest $::CONSOLER_CFLAGS consolertest.c $::CONSOLER_LIBS
    execv ./consolertest
}

# consoler.pc
execv sed -e s=@PACKAGE_VERSION@=$::VERSION= -e s=@prefix@=$::PREFIX= consoler.pc.in > consoler.pc

librarydoc consoler.h

if $::INSTALL {
    install $::PREFIX/include consoler.h consoler_keysym.h
    install $::PREFIX/lib libconsoler.a
    install $::PREFIX/lib/pkgconfig consoler.pc
}

