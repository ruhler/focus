
all:
	scons

install:
	scons --prefix=$(HOME)/local install

clean:
	scons --clean

