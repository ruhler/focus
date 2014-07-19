
make::all {
    execv gcc -o sgreen $::CONSOLER_CFLAGS sgreen.c green.c green.h $::CONSOLER_LIBS -DPACKAGE_VERSION="$::VERSION" -lpthread
    execv gcc -o cgreen $::CONSOLER_CFLAGS cgreen.c $::CONSOLER_LIBS -DPACKAGE_VERSION="$::VERSION"
    execv a2x -v -f manpage -a VERSION=$::VERSION sgreen.1.txt
    execv a2x -v -f manpage -a VERSION=$::VERSION cgreen.1.txt
}

make::check {
    execv gcc -o sgreentest $::CONSOLER_CFLAGS sgreentest.c $::CONSOLER_LIBS
    execv -ignorestderr ./sgreentest ./sgreen ../filler/filler

    execv gcc -o cgreentest $::CONSOLER_CFLAGS cgreentest.c $::CONSOLER_LIBS
    execv -ignorestderr ./cgreentest ./sgreen ./cgreen ../filler/filler
}


make::install {
    cmd::install $::PREFIX/bin sgreen cgreen rgreen
    cmd::install $::PREFIX/share/man/man1 sgreen.1 cgreen.1
}

make::clean execv rm -f cgreen cgreen.1 sgreen sgreen.1 cgreentest sgreentest

