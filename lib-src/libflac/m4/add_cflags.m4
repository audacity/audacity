dnl @synopsis XIPH_ADD_CFLAGS
dnl
dnl Add the given option to CFLAGS, if it doesn't break the compiler

AC_DEFUN([XIPH_ADD_CFLAGS],
[AC_MSG_CHECKING([if $CC accepts $1])
	ac_add_cflags__old_cflags="$CFLAGS"
	CFLAGS="$1"
	AC_TRY_LINK([
			#include <stdio.h>
			],
		[puts("Hello, World!"); return 0;],
		AC_MSG_RESULT([yes])
			CFLAGS="$ac_add_cflags__old_cflags $1",
		AC_MSG_RESULT([no])
			CFLAGS="$ac_add_cflags__old_cflags"
		)
])# XIPH_ADD_CFLAGS
