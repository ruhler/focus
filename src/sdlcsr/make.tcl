

execv gcc -o sdlcsr $::CONSOLER_CFLAGS $::SDL_CFLAGS sdlcsr.c $::CONSOLER_LIBS $::SDL_LIBS -DPACKAGE_VERSION="$::VERSION"

execv a2x -v -f manpage -a VERSION=$::VERSION sdlcsr.1.txt

if $::INSTALL {
    install $::PREFIX/bin sdlcsr
    install $::PREFIX/share/man/man1 sdlcsr.1
}

