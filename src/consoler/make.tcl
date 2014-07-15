
# libconsoler.a
exec gcc -c -o consoler.o consoler.c -DPACKAGE_VERSION="$::VERSION"
exec ar cr libconsoler.a consoler.o

# consolertest
if $::CHECK {
    exec gcc -o consolertest consolertest.c -L. -lconsoler
    exec ./consolertest
}

# consoler.pc
exec sed -e s=@PACKAGE_VERSION@=$::VERSION= -e s=@prefix@=$::PREFIX= consoler.pc.in > consoler.pc

if $::INSTALL {
    exec install -t $::PREFIX/include consoler.h consoler_keysym.h
    exec install -t $::PREFIX/lib libconsoler.a
    exec install -t $::PREFIX/lib/pkgconfig consoler.pc
}

