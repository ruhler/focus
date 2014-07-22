
set ::CONSOLER_CFLAGS "-I[pwd]"
set ::CONSOLER_LIBS "-L[pwd] -lconsoler"

make::all {
    execv gcc -c -o consoler.o consoler.c -DPACKAGE_VERSION="$::VERSION"
    execv ar cr libconsoler.a consoler.o
}

librarydoc consoler.h
cmd::pkgconfig consoler

make::check {
    execv gcc -o consolertest $::CONSOLER_CFLAGS consolertest.c $::CONSOLER_LIBS
    execv ./consolertest
}

make::install {
    cmd::install $::PREFIX/include consoler.h consoler_keysym.h
    cmd::install $::PREFIX/lib libconsoler.a
}

make::clean execv rm -f consoler.o consolertest libconsoler.a

