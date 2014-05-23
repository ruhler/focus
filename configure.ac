
AC_INIT([focus], [1.2.0], [ruhler@csail.mit.edu])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])

AC_PROG_CC
AM_PROG_AR
AC_PROG_RANLIB

PKG_CHECK_MODULES([FREETYPE], [freetype2]) 
PKG_CHECK_MODULES([FONTCONFIG], [fontconfig]) 

AC_CHECK_LIB([m],[ceil])

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([
 Makefile
 src/Makefile
 src/consoler/Makefile
 src/consoler/consoler.pc
 src/fonter/Makefile
 src/fonter/fonter.pc
 src/boxer/Makefile
])
AC_OUTPUT
