
cmd::prog boxer gcc -o boxer $::CONSOLER_CFLAGS boxer.c $::CONSOLER_LIBS -DPACKAGE_VERSION="$::VERSION"
cmd::man1 boxer

make::check {
    execv gcc -o boxertest $::CONSOLER_CFLAGS boxertest.c $::CONSOLER_LIBS
    execv ./boxertest
}

make::clean execv rm -f boxertest

