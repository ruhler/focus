
cmd::prog boxer { gcc -o boxer $::CONSOLER_CFLAGS boxer.c $::CONSOLER_LIBS -DPACKAGE_VERSION="$::VERSION" }
cmd::test boxertest { gcc -o boxertest $::CONSOLER_CFLAGS boxertest.c $::CONSOLER_LIBS } ./boxertest
cmd::man1 boxer

