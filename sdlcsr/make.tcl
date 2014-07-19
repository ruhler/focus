
make::all {
    execv gcc -o sdlcsr $::CONSOLER_CFLAGS $::SDL_CFLAGS sdlcsr.c $::CONSOLER_LIBS $::SDL_LIBS -DPACKAGE_VERSION="$::VERSION"
    execv a2x -v -f manpage -a VERSION=$::VERSION sdlcsr.1.txt
}

make::install {
    cmd::install $::PREFIX/bin sdlcsr
    cmd::install $::PREFIX/share/man/man1 sdlcsr.1
}

make::clean execv rm -f sdlcsr sdlcsr.1

