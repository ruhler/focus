
AC_INIT([focus], [1.2.0], [ruhler@csail.mit.edu])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])

AC_PROG_CC
AC_PROG_CXX
AM_PROG_AR
AC_PROG_RANLIB

PKG_CHECK_MODULES([FREETYPE], [freetype2]) 
PKG_CHECK_MODULES([FONTCONFIG], [fontconfig]) 
PKG_CHECK_MODULES([POPPLER], [poppler-cpp])
PKG_CHECK_MODULES([SDL], [sdl])

AC_CHECK_LIB([m],[ceil])
AC_CHECK_LIB([pthread],[pthread_create])
AC_CHECK_LIB([png],[png_create_read_struct])
AC_CHECK_LIB([jpeg],[jpeg_std_error])
AC_CHECK_LIB([util],[forkpty])
AC_CHECK_LIB([curses],[initscr])

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([
 Makefile
 src/Makefile
 src/consoler/Makefile
 src/consoler/consoler.pc
 src/boxer/Makefile
 src/fonter/Makefile
 src/fonter/fonter.pc
 src/green/Makefile
 src/imager/Makefile
 src/pdfer/Makefile
 src/sdlcsr/Makefile
 src/termer/Makefile
])
AC_OUTPUT
