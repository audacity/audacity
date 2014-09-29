dnl @synopsis AC_ADD_CFLAGS
dnl
dnl Add the given option to CFLAGS, if it doesn't break the compiler

AC_DEFUN([AC_ADD_CFLAGS],
[AC_MSG_CHECKING([if $CC accepts $1])
	ac_add_cflags__old_cflags="$CFLAGS"
	CFLAGS="$CFLAGS $1"
	AC_TRY_LINK([#include <stdio.h>],
		[puts("Hello, World!"); return 0;],
		AC_MSG_RESULT([yes]),
		AC_MSG_RESULT([no])
		CFLAGS="$ac_add_cflags__old_cflags")
		])
])# AC_ADD_CFLAGS
