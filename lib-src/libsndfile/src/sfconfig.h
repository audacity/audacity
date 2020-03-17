/*
** Copyright (C) 2005-2017 Erik de Castro Lopo <erikd@mega-nerd.com>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU Lesser General Public License as published by
** the Free Software Foundation; either version 2.1 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU Lesser General Public License for more details.
**
** You should have received a copy of the GNU Lesser General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

/*
**	Autoconf leaves many config parameters undefined.
**	Here we change then from being undefined to defining them to 0.
**	This allows things like:
**
**		#if HAVE_CONFIG_PARAM
**
**	and
**
**		if (HAVE_CONFIG_PARAM)
**			do_something () ;
*/

#ifndef SFCONFIG_H
#define SFCONFIG_H

/* Include the Autoconf generated file. */
#include "config.h"

/* Now fiddle the values. */

#ifndef HAVE_ALSA_ASOUNDLIB_H
#define HAVE_ALSA_ASOUNDLIB_H 0
#endif

#ifndef HAVE_BYTESWAP_H
#define HAVE_BYTESWAP_H 0
#endif

#ifndef HAVE_DECL_S_IRGRP
#define	HAVE_DECL_S_IRGRP 0
#endif

#ifndef HAVE_ENDIAN_H
#define HAVE_ENDIAN_H 0
#endif

#ifndef HAVE_FSTAT64
#define HAVE_FSTAT64 0
#endif

#ifndef HAVE_FSYNC
#define HAVE_FSYNC 0
#endif

#ifndef HAVE_LOCALE_H
#define HAVE_LOCALE_H 0
#endif

#ifndef HAVE_LRINT
#define HAVE_LRINT 0
#endif

#ifndef HAVE_LRINTF
#define HAVE_LRINTF 0
#endif

#ifndef HAVE_MMAP
#define HAVE_MMAP 0
#endif

#ifndef HAVE_SETLOCALE
#define HAVE_SETLOCALE 0
#endif

#ifndef HAVE_SQLITE3
#define HAVE_SQLITE3 0
#endif

#ifndef HAVE_STDINT_H
#define HAVE_STDINT_H 0
#endif

#ifndef HAVE_SYS_WAIT_H
#define HAVE_SYS_WAIT_H 0
#endif

#ifndef HAVE_SYS_TIME_H
#define HAVE_SYS_TIME_H 0
#endif

#ifndef HAVE_UNISTD_H
#define HAVE_UNISTD_H 0
#endif

#ifndef HAVE_PIPE
#define HAVE_PIPE 0
#endif

#ifndef HAVE_WAITPID
#define	HAVE_WAITPID 0
#endif

#ifndef HAVE_X86INTRIN_H
#define HAVE_X86INTRIN_H 0
#endif

#if (defined __x86_64__) || (defined _M_X64)
#define CPU_IS_X86_64	1	/* Define both for x86_64 */
#define CPU_IS_X86		1
#elif defined (__i486__) || defined (__i586__) || defined (__i686__) || defined (_M_IX86)
#define CPU_IS_X86 		1
#define CPU_IS_X86_64 	0
#else
#define CPU_IS_X86		0
#define CPU_IS_X86_64	0
#endif

#ifndef HAVE_SSIZE_T
#define HAVE_SSIZE_T 0
#endif

#if (HAVE_SSIZE_T == 0)
#define ssize_t intptr_t
#endif

#endif
