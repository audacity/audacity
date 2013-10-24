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


dnl @synopsis XIPH_C_BSWAP32
dnl
dnl @author Erik de Castro Lopo <erikd@mega-nerd.com>
dnl
dnl Dtermine whether the compiler has the __builtin_bswap32() intrinsic which
dnl is likely to be present for most versions of GCC as well as Clang.

AC_DEFUN([XIPH_C_BSWAP32],
[AC_CACHE_CHECK(has bswap32 instrinsic,
	ac_cv_c_bswap,

	# Initialize to no
	ac_cv_c_bswap=no
	HAVE_BSWAP32=0

	[AC_TRY_LINK([],
		return __builtin_bswap32 (0) ;,
			ac_cv_c_bswap=yes
			HAVE_BSWAP32=1
			)]
	AC_DEFINE_UNQUOTED(HAVE_BSWAP32, ${HAVE_BSWAP32},
					[Compiler has the __builtin_bswap32 intrinsic])

	)]
)# XIPH_C_BSWAP32
