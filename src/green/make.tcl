
execv gcc -o sgreen $::CONSOLER_CFLAGS sgreen.c green.c green.h $::CONSOLER_LIBS -DPACKAGE_VERSION="$::VERSION" -lpthread
execv gcc -o cgreen $::CONSOLER_CFLAGS cgreen.c $::CONSOLER_LIBS -DPACKAGE_VERSION="$::VERSION"

execv a2x -v -f manpage -a VERSION=$::VERSION sgreen.1.txt
execv a2x -v -f manpage -a VERSION=$::VERSION cgreen.1.txt

if $::INSTALL {
    install -t $::PREFIX/bin sgreen cgreen rgreen
    install -t $::PREFIX/share/man/man1 sgreen.1 cgreen.1
}

