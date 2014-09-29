dnl Copyright (C) 2013  Xiph.org Foundation
dnl
dnl Redistribution and use in source and binary forms, with or without
dnl modification, are permitted provided that the following conditions
dnl are met:
dnl
dnl - Redistributions of source code must retain the above copyright
dnl notice, this list of conditions and the following disclaimer.
dnl
dnl - Redistributions in binary form must reproduce the above copyright
dnl notice, this list of conditions and the following disclaimer in the
dnl documentation and/or other materials provided with the distribution.
dnl
dnl - Neither the name of the Xiph.org Foundation nor the names of its
dnl contributors may be used to endorse or promote products derived from
dnl this software without specific prior written permission.
dnl
dnl THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
dnl ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
dnl LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
dnl A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE FOUNDATION OR
dnl CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
dnl EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
dnl PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
dnl PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
dnl LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
dnl NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
dnl SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

dnl Want to know of GCC stack protector works, botfor the C and for the C++
dnl compiler.
dnl
dnl Just checking if the compiler accepts the required CFLAGSs is not enough
dnl because we have seen at least one instance where this check was
dnl in-sufficient.
dnl
dnl Instead, try to compile and link a test program with the stack protector
dnl flags. If that works, we use it.

AC_DEFUN([XIPH_GCC_STACK_PROTECTOR],
[AC_LANG_ASSERT(C)
	AC_MSG_CHECKING([if $CC supports stack smash protection])
	xiph_stack_check_old_cflags="$CFLAGS"
	SSP_FLAGS="-fstack-protector --param ssp-buffer-size=4"
	CFLAGS=$SSP_FLAGS
	AC_TRY_LINK([
			#include <stdio.h>
			],
		[puts("Hello, World!"); return 0;],
		AC_MSG_RESULT([yes])
			CFLAGS="$xiph_stack_check_old_cflags $SSP_FLAGS",
		AC_MSG_RESULT([no])
			CFLAGS="$xiph_stack_check_old_cflags"
		)
])# XIPH_GCC_STACK_PROTECTOR

AC_DEFUN([XIPH_GXX_STACK_PROTECTOR],
[AC_LANG_PUSH([C++])
	AC_MSG_CHECKING([if $CXX supports stack smash protection])
	xiph_stack_check_old_cflags="$CFLAGS"
	SSP_FLAGS="-fstack-protector --param ssp-buffer-size=4"
	CFLAGS=$SSP_FLAGS
	AC_TRY_LINK([
			#include <cstdio>
			],
		[puts("Hello, World!"); return 0;],
		AC_MSG_RESULT([yes])
			CFLAGS="$xiph_stack_check_old_cflags $SSP_FLAGS",
		AC_MSG_RESULT([no])
			CFLAGS="$xiph_stack_check_old_cflags"
		)
	AC_LANG_POP([C++])
])# XIPH_GXX_STACK_PROTECTOR
