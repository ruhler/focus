
set ::CONSOLER_CFLAGS "-I[pwd]"
set ::CONSOLER_LIBS "-L[pwd] -lconsoler"

make::all {
    execv gcc -c -o consoler.o consoler.c -DPACKAGE_VERSION="$::VERSION"
    execv ar cr libconsoler.a consoler.o
    execv sed -e s=@PACKAGE_VERSION@=$::VERSION= -e s=@prefix@=$::PREFIX= consoler.pc.in > consoler.pc
}

librarydoc consoler.h

make::check {
    execv gcc -o consolertest $::CONSOLER_CFLAGS consolertest.c $::CONSOLER_LIBS
    execv ./consolertest
}

make::install {
    cmd::install $::PREFIX/include consoler.h consoler_keysym.h
    cmd::install $::PREFIX/lib libconsoler.a
    cmd::install $::PREFIX/lib/pkgconfig consoler.pc
}

make::clean execv rm -f consoler.o consoler.pc consolertest libconsoler.a

