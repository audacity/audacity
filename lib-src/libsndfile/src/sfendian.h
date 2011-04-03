/*
** Copyright (C) 1999-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#ifndef SFENDIAN_INCLUDED
#define SFENDIAN_INCLUDED

#include "sfconfig.h"

#if HAVE_STDINT_H
#include <stdint.h>
#elif HAVE_INTTYPES_H
#include <inttypes.h>
#endif

#if (defined (SIZEOF_INT64_T) && (SIZEOF_INT64_T == 8))
/* Good, we have int64_t. */
#elif (defined (SIZEOF_LONG_LONG) && (SIZEOF_LONG_LONG == 8))
typedef long long int64_t ;
#elif (defined (SIZEOF_LONG) && (SIZEOF_LONG == 8))
typedef long int64_t ;
#elif (defined (WIN32) || defined (_WIN32))
typedef __int64 int64_t ;
#else
#error "No 64 bit integer type."
#endif

#if HAVE_BYTESWAP_H

#include <byteswap.h>

#define	ENDSWAP_SHORT(x)	((short) bswap_16 (x))
#define	ENDSWAP_INT(x)		((int) bswap_32 (x))

#else

#define	ENDSWAP_SHORT(x)	((((x) >> 8) & 0xFF) + (((x) & 0xFF) << 8))
#define	ENDSWAP_INT(x)		((((x) >> 24) & 0xFF) + (((x) >> 8) & 0xFF00) + (((x) & 0xFF00) << 8) + (((x) & 0xFF) << 24))

#endif

/*
** Many file types (ie WAV, AIFF) use sets of four consecutive bytes as a
** marker indicating different sections of the file.
** The following MAKE_MARKER macro allows th creation of integer constants
** for these markers.
*/

#if (CPU_IS_LITTLE_ENDIAN == 1)
	#define	MAKE_MARKER(a,b,c,d)	((a) | ((b) << 8) | ((c) << 16) | ((d) << 24))
#elif (CPU_IS_BIG_ENDIAN == 1)
	#define	MAKE_MARKER(a,b,c,d)	(((a) << 24) | ((b) << 16) | ((c) << 8) | (d))
#else
	#error "Target CPU endian-ness unknown. May need to hand edit src/sfconfig.h"
#endif

/*
** Macros to handle reading of data of a specific endian-ness into host endian
** shorts and ints. The single input is an unsigned char* pointer to the start
** of the object. There are two versions of each macro as we need to deal with
** both big and little endian CPUs.
*/

#if (CPU_IS_LITTLE_ENDIAN == 1)
	#define LES2H_SHORT(x)			(x)
	#define LEI2H_INT(x)			(x)

	#define BES2H_SHORT(x)			ENDSWAP_SHORT (x)
	#define BEI2H_INT(x)			ENDSWAP_INT (x)

	#define H2BE_SHORT(x)			ENDSWAP_SHORT (x)
	#define H2BE_INT(x)				ENDSWAP_INT (x)

#elif (CPU_IS_BIG_ENDIAN == 1)
	#define LES2H_SHORT(x)			ENDSWAP_SHORT (x)
	#define LEI2H_INT(x)			ENDSWAP_INT (x)

	#define BES2H_SHORT(x)			(x)
	#define BEI2H_INT(x)			(x)

	#define H2LE_SHORT(x)			ENDSWAP_SHORT (x)
	#define H2LE_INT(x)				ENDSWAP_INT (x)

#else
	#error "Target CPU endian-ness unknown. May need to hand edit src/sfconfig.h"
#endif

#define LET2H_SHORT_PTR(x)		((x) [1] + ((x) [2] << 8))
#define LET2H_INT_PTR(x)		(((x) [0] << 8) + ((x) [1] << 16) + ((x) [2] << 24))

#define BET2H_SHORT_PTR(x)		(((x) [0] << 8) + (x) [1])
#define BET2H_INT_PTR(x)		(((x) [0] << 24) + ((x) [1] << 16) + ((x) [2] << 8))

/*-----------------------------------------------------------------------------------------------
** Generic functions for performing endian swapping on integer arrays.
*/

static inline void
endswap_short_array (short *ptr, int len)
{	short	temp ;

	while (--len >= 0)
	{	temp = ptr [len] ;
		ptr [len] = ENDSWAP_SHORT (temp) ;
		} ;
} /* endswap_short_array */

static inline void
endswap_short_copy (short *dest, const short *src, int len)
{
	while (--len >= 0)
	{	dest [len] = ENDSWAP_SHORT (src [len]) ;
		} ;
} /* endswap_short_copy */

static inline void
endswap_int_array (int *ptr, int len)
{	int temp ;

	while (--len >= 0)
	{	temp = ptr [len] ;
		ptr [len] = ENDSWAP_INT (temp) ;
		} ;
} /* endswap_int_array */

static inline void
endswap_int_copy (int *dest, const int *src, int len)
{
	while (--len >= 0)
	{	dest [len] = ENDSWAP_INT (src [len]) ;
		} ;
} /* endswap_int_copy */

/*========================================================================================
*/

#if	(HAVE_BYTESWAP_H && defined (SIZEOF_INT64_T) && (SIZEOF_INT64_T == 8))

static inline void
endswap_int64_t_array (int64_t *ptr, int len)
{	int64_t value ;

	while (--len >= 0)
	{	value = ptr [len] ;
		ptr [len] = bswap_64 (value) ;
		} ;
} /* endswap_int64_t_array */

static inline void
endswap_int64_t_copy (int64_t *dest, const int64_t *src, int len)
{	int64_t value ;

	while (--len >= 0)
	{	value = src [len] ;
		dest [len] = bswap_64 (value) ;
		} ;
} /* endswap_int64_t_copy */

#else

static inline void
endswap_int64_t_array (int64_t *ptr, int len)
{	unsigned char *ucptr, temp ;

	ucptr = (unsigned char *) ptr ;
	ucptr += 8 * len ;
	while (--len >= 0)
	{	ucptr -= 8 ;

		temp = ucptr [0] ;
		ucptr [0] = ucptr [7] ;
		ucptr [7] = temp ;

		temp = ucptr [1] ;
		ucptr [1] = ucptr [6] ;
		ucptr [6] = temp ;

		temp = ucptr [2] ;
		ucptr [2] = ucptr [5] ;
		ucptr [5] = temp ;

		temp = ucptr [3] ;
		ucptr [3] = ucptr [4] ;
		ucptr [4] = temp ;
		} ;
} /* endswap_int64_t_array */

static inline void
endswap_int64_t_copy (int64_t *dest, const int64_t *src, int len)
{	const unsigned char *psrc ;
	unsigned char *pdest ;

	if (dest == src)
	{	endswap_int64_t_array (dest, len) ;
		return ;
		} ;

	psrc = ((const unsigned char *) src) + 8 * len ;
	pdest = ((unsigned char *) dest) + 8 * len ;
	while (--len >= 0)
	{	psrc -= 8 ;
		pdest -= 8 ;

		pdest [0] = psrc [7] ;
		pdest [2] = psrc [5] ;
		pdest [4] = psrc [3] ;
		pdest [6] = psrc [1] ;
		pdest [7] = psrc [0] ;
		pdest [1] = psrc [6] ;
		pdest [3] = psrc [4] ;
		pdest [5] = psrc [2] ;
		} ;
} /* endswap_int64_t_copy */

#endif

/* A couple of wrapper functions. */

static inline void
endswap_float_array (float *ptr, int len)
{	endswap_int_array ((void *) ptr, len) ;
} /* endswap_float_array */

static inline void
endswap_double_array (double *ptr, int len)
{	endswap_int64_t_array ((void *) ptr, len) ;
} /* endswap_double_array */

static inline void
endswap_float_copy (float *dest, const float *src, int len)
{	endswap_int_copy ((int *) dest, (const int *) src, len) ;
} /* endswap_float_copy */

static inline void
endswap_double_copy (double *dest, const double *src, int len)
{	endswap_int64_t_copy ((int64_t *) dest, (const int64_t *) src, len) ;
} /* endswap_double_copy */

#endif /* SFENDIAN_INCLUDED */

