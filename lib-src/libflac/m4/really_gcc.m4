dnl @synopsis XIPH_GCC_REALLY_IS_GCC
dnl
dnl Find out if a compiler claiming to be gcc really is gcc (clang lies).
dnl @version 1.0	Oct 31 2013
dnl @author Erik de Castro Lopo <erikd AT mega-nerd DOT com>
dnl
dnl Permission to use, copy, modify, distribute, and sell this file for any
dnl purpose is hereby granted without fee, provided that the above copyright
dnl and this permission notice appear in all copies.  No representations are
dnl made about the suitability of this software for any purpose.  It is
dnl provided "as is" without express or implied warranty.
dnl

# If the configure script has already detected GNU GCC, then make sure it
# isn't CLANG masquerading as GCC.

AC_DEFUN([XIPH_GCC_REALLY_IS_GCC],
[	AC_LANG_ASSERT(C)
	if test "x$ac_cv_c_compiler_gnu" = "xyes" ; then
		AC_TRY_LINK([
			#include <stdio.h>
			],
			[
			#ifdef __clang__
				This is clang!
			#endif
			],
		ac_cv_c_compiler_gnu=yes,
		ac_cv_c_compiler_gnu=no
		)
		fi
])
