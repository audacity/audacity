/*
** Copyright (C) 2005-2009 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#ifndef HAVE_PREAD
#define HAVE_PREAD 0
#endif

#ifndef HAVE_PWRITE
#define HAVE_PWRITE 0
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

#ifndef HAVE_UNISTD_H
#define HAVE_UNISTD_H 0
#endif

#endif

