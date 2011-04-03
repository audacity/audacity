dnl @synopsis MN_C99_FLEXIBLE_ARRAY
dnl
dnl Dose the compiler support the 1999 ISO C Standard "stuct hack".
dnl @version 1.1	Mar 15 2004
dnl @author Erik de Castro Lopo <erikd AT mega-nerd DOT com>
dnl
dnl Permission to use, copy, modify, distribute, and sell this file for any 
dnl purpose is hereby granted without fee, provided that the above copyright 
dnl and this permission notice appear in all copies.  No representations are
dnl made about the suitability of this software for any purpose.  It is 
dnl provided "as is" without express or implied warranty.

AC_DEFUN([MN_C99_FLEXIBLE_ARRAY],
[AC_CACHE_CHECK(C99 struct flexible array support, 
	ac_cv_c99_flexible_array,

# Initialize to unknown
ac_cv_c99_flexible_array=no

AC_TRY_LINK([[
	#include <stdlib.h>
	typedef struct {
	int k;
	char buffer [] ;
	} MY_STRUCT ;
	]], 
	[  MY_STRUCT *p = calloc (1, sizeof (MY_STRUCT) + 42); ],
	ac_cv_c99_flexible_array=yes,
	ac_cv_c99_flexible_array=no
	))]
) # MN_C99_FLEXIBLE_ARRAY

