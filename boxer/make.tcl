
make::all execv gcc -o boxer $::CONSOLER_CFLAGS boxer.c $::CONSOLER_LIBS -DPACKAGE_VERSION="$::VERSION"
cmd::man1 boxer

make::check {
    execv gcc -o boxertest $::CONSOLER_CFLAGS boxertest.c $::CONSOLER_LIBS
    execv ./boxertest
}

make::install cmd::install $::PREFIX/bin boxer
make::clean execv rm -f boxer boxertest

