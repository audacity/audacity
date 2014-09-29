dnl Copyright (C) 2012  Xiph.org Foundation
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


dnl @synopsis XIPH_C_FIND_ENDIAN
dnl
dnl Determine endian-ness of target processor.
dnl @version 1.1	Mar 03 2002
dnl @author Erik de Castro Lopo <erikd@mega-nerd.com>
dnl
dnl Majority written from scratch to replace the standard autoconf macro
dnl AC_C_BIGENDIAN. Only part remaining from the original is the invocation
dnl of the AC_TRY_RUN macro.
dnl
dnl Find endian-ness in the following way:
dnl    1) Look in <endian.h>.
dnl    2) If 1) fails, look in <sys/types.h> and <sys/param.h>.
dnl    3) If 1) and 2) fails and not cross compiling run a test program.
dnl    4) If 1) and 2) fails and cross compiling then guess based on target.

AC_DEFUN([XIPH_C_FIND_ENDIAN],
[AC_CACHE_CHECK(processor byte ordering,
	ac_cv_c_byte_order,

# Initialize to unknown
ac_cv_c_byte_order=unknown

if test x$ac_cv_header_endian_h = xyes ; then

	# First try <endian.h> which should set BYTE_ORDER.

	[AC_TRY_LINK([
		#include <endian.h>
		#if BYTE_ORDER != LITTLE_ENDIAN
			not big endian
		#endif
		], return 0 ;,
			ac_cv_c_byte_order=little
		)]

	[AC_TRY_LINK([
		#include <endian.h>
		#if BYTE_ORDER != BIG_ENDIAN
			not big endian
		#endif
		], return 0 ;,
			ac_cv_c_byte_order=big
		)]

	fi

if test $ac_cv_c_byte_order = unknown ; then

	[AC_TRY_LINK([
		#include <sys/types.h>
		#include <sys/param.h>
		#if !BYTE_ORDER || !BIG_ENDIAN || !LITTLE_ENDIAN
			bogus endian macros
		#endif
		], return 0 ;,

		[AC_TRY_LINK([
			#include <sys/types.h>
			#include <sys/param.h>
			#if BYTE_ORDER != LITTLE_ENDIAN
				not big endian
			#endif
			], return 0 ;,
				ac_cv_c_byte_order=little
			)]

		[AC_TRY_LINK([
			#include <sys/types.h>
			#include <sys/param.h>
			#if BYTE_ORDER != LITTLE_ENDIAN
				not big endian
			#endif
			], return 0 ;,
				ac_cv_c_byte_order=little
			)]

		)]

 	fi

if test $ac_cv_c_byte_order = unknown ; then
	if test $cross_compiling = yes ; then
		# This is the last resort. Try to guess the target processor endian-ness
		# by looking at the target CPU type.
		[
		case "$target_cpu" in
			alpha* | i?86* | mipsel* | ia64*)
				ac_cv_c_byte_order=little
				;;

			m68* | mips* | powerpc* | hppa* | sparc*)
				ac_cv_c_byte_order=big
				;;

			esac
		]
	else
		AC_TRY_RUN(
		[[
		int main (void)
		{	/* Are we little or big endian?  From Harbison&Steele.  */
			union
			{	long l ;
				char c [sizeof (long)] ;
			} u ;
			u.l = 1 ;
			return (u.c [sizeof (long) - 1] == 1);
			}
			]], , ac_cv_c_byte_order=big,
			)

		AC_TRY_RUN(
		[[int main (void)
		{	/* Are we little or big endian?  From Harbison&Steele.  */
			union
			{	long l ;
				char c [sizeof (long)] ;
			} u ;
			u.l = 1 ;
			return (u.c [0] == 1);
			}]], , ac_cv_c_byte_order=little,
			)
		fi
	fi

)

if test $ac_cv_c_byte_order = big ; then
	ac_cv_c_big_endian=1
	ac_cv_c_little_endian=0
elif test $ac_cv_c_byte_order = little ; then
	ac_cv_c_big_endian=0
	ac_cv_c_little_endian=1
else
	ac_cv_c_big_endian=0
	ac_cv_c_little_endian=0

	AC_MSG_WARN([[*****************************************************************]])
	AC_MSG_WARN([[*** Not able to determine endian-ness of target processor.       ]])
	AC_MSG_WARN([[*** The constants CPU_IS_BIG_ENDIAN and CPU_IS_LITTLE_ENDIAN in  ]])
	AC_MSG_WARN([[*** config.h may need to be hand editied.                        ]])
	AC_MSG_WARN([[*****************************************************************]])
	fi

]
)# XIPH_C_FIND_ENDIAN
