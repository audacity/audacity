/*
** Copyright (C) 1999-2019 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#include <config.h>

#include <stdarg.h>
#include <string.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#else
#include "sf_unistd.h"
#endif
#include <ctype.h>
#include <math.h>
#include <time.h>
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include "sndfile.h"
#include "sfendian.h"
#include "common.h"

#define	INITIAL_HEADER_SIZE	256

/* Allocate and initialize the SF_PRIVATE struct. */
SF_PRIVATE *
psf_allocate (void)
{	SF_PRIVATE * psf ;

	if ((psf = calloc (1, sizeof (SF_PRIVATE))) == NULL)
		return	NULL ;

	if ((psf->header.ptr = calloc (1, INITIAL_HEADER_SIZE)) == NULL)
	{	free (psf) ;
		return	NULL ;
		} ;
	psf->header.len = INITIAL_HEADER_SIZE ;

	return psf ;
} /* psf_allocate */

static int
psf_bump_header_allocation (SF_PRIVATE * psf, sf_count_t needed)
{
	sf_count_t newlen, smallest = INITIAL_HEADER_SIZE ;
	void * ptr ;

	newlen = (needed > psf->header.len) ? 2 * SF_MAX (needed, smallest) : 2 * psf->header.len ;

	if (newlen > 100 * 1024)
	{	psf_log_printf (psf, "Request for header allocation of %D denied.\n", newlen) ;
		return 1 ;
		}

	if ((ptr = realloc (psf->header.ptr, newlen)) == NULL)
	{	psf_log_printf (psf, "realloc (%p, %D) failed\n", psf->header.ptr, newlen) ;
		psf->error = SFE_MALLOC_FAILED ;
		return 1 ;
		} ;

	/* Always zero-out new header memory to avoid un-initializer memory accesses. */
	if (newlen > psf->header.len)
		memset ((char *) ptr + psf->header.len, 0, newlen - psf->header.len) ;

	psf->header.ptr = ptr ;
	psf->header.len = newlen ;
	return 0 ;
} /* psf_bump_header_allocation */

/*-----------------------------------------------------------------------------------------------
** psf_log_printf allows libsndfile internal functions to print to an internal parselog which
** can later be displayed.
** The format specifiers are as for printf but without the field width and other modifiers.
** Printing is performed to the parselog char array of the SF_PRIVATE struct.
** Printing is done in such a way as to guarantee that the log never overflows the end of the
** parselog array.
*/

static inline void
log_putchar (SF_PRIVATE *psf, char ch)
{	if (psf->parselog.indx < SIGNED_SIZEOF (psf->parselog.buf) - 1)
	{	psf->parselog.buf [psf->parselog.indx++] = ch ;
		psf->parselog.buf [psf->parselog.indx] = 0 ;
		} ;
	return ;
} /* log_putchar */

void
psf_log_printf (SF_PRIVATE *psf, const char *format, ...)
{	va_list		ap ;
	uint32_t	u ;
	int			d, tens, shift, width, width_specifier, left_align, slen ;
	char		c, *strptr, istr [5], lead_char, sign_char ;

	va_start (ap, format) ;

	while ((c = *format++))
	{	if (c != '%')
		{	log_putchar (psf, c) ;
			continue ;
			} ;

		if (format [0] == '%') /* Handle %% */
		{ 	log_putchar (psf, '%') ;
			format ++ ;
			continue ;
			} ;

		sign_char = 0 ;
		left_align = SF_FALSE ;
		while (1)
		{	switch (format [0])
			{	case ' ' :
				case '+' :
					sign_char = format [0] ;
					format ++ ;
					continue ;

				case '-' :
					left_align = SF_TRUE ;
					format ++ ;
					continue ;

				default : break ;
				} ;

			break ;
			} ;

		if (format [0] == 0)
			break ;

		lead_char = ' ' ;
		if (format [0] == '0')
			lead_char = '0' ;

		width_specifier = 0 ;
		while ((c = *format++) && isdigit (c))
			width_specifier = width_specifier * 10 + (c - '0') ;

		switch (c)
		{	case 0 : /* NULL character. */
					va_end (ap) ;
					return ;

			case 's': /* string */
					strptr = va_arg (ap, char *) ;
					if (strptr == NULL)
						break ;
					slen = strlen (strptr) ;
					width_specifier = width_specifier >= slen ? width_specifier - slen : 0 ;
					if (left_align == SF_FALSE)
						while (width_specifier -- > 0)
							log_putchar (psf, ' ') ;
					while (*strptr)
						log_putchar (psf, *strptr++) ;
					while (width_specifier -- > 0)
						log_putchar (psf, ' ') ;
					break ;

			case 'd': /* int */
					d = va_arg (ap, int) ;

					if (d < 0)
					{	d = -d ;
						sign_char = '-' ;
						if (lead_char != '0' && left_align == SF_FALSE)
							width_specifier -- ;
						} ;

					tens = 1 ;
					width = 1 ;
					while (d / tens >= 10)
					{	tens *= 10 ;
						width ++ ;
						} ;

					width_specifier -= width ;

					if (sign_char == ' ')
					{	log_putchar (psf, ' ') ;
						width_specifier -- ;
						} ;

					if (left_align == SF_FALSE && lead_char != '0')
					{	if (sign_char == '+')
							width_specifier -- ;

						while (width_specifier -- > 0)
							log_putchar (psf, lead_char) ;
						} ;

					if (sign_char == '+' || sign_char == '-')
					{	log_putchar (psf, sign_char) ;
						width_specifier -- ;
						} ;

					if (left_align == SF_FALSE)
						while (width_specifier -- > 0)
							log_putchar (psf, lead_char) ;

					while (tens > 0)
					{	log_putchar (psf, '0' + d / tens) ;
						d %= tens ;
						tens /= 10 ;
						} ;

					while (width_specifier -- > 0)
						log_putchar (psf, lead_char) ;
					break ;

			case 'D': /* sf_count_t */
					{	sf_count_t		D, Tens ;

						D = va_arg (ap, sf_count_t) ;

						if (D == 0)
						{	while (-- width_specifier > 0)
								log_putchar (psf, lead_char) ;
							log_putchar (psf, '0') ;
							break ;
							}
						if (D < 0)
						{	log_putchar (psf, '-') ;
							D = -D ;
							} ;
						Tens = 1 ;
						width = 1 ;
						while (D / Tens >= 10)
						{	Tens *= 10 ;
							width ++ ;
							} ;

						while (width_specifier > width)
						{	log_putchar (psf, lead_char) ;
							width_specifier-- ;
							} ;

						while (Tens > 0)
						{	log_putchar (psf, '0' + D / Tens) ;
							D %= Tens ;
							Tens /= 10 ;
							} ;
						} ;
					break ;

			case 'u': /* unsigned int */
					u = va_arg (ap, unsigned int) ;

					tens = 1 ;
					width = 1 ;
					while (u / tens >= 10)
					{	tens *= 10 ;
						width ++ ;
						} ;

					width_specifier -= width ;

					if (sign_char == ' ')
					{	log_putchar (psf, ' ') ;
						width_specifier -- ;
						} ;

					if (left_align == SF_FALSE && lead_char != '0')
					{	if (sign_char == '+')
							width_specifier -- ;

						while (width_specifier -- > 0)
							log_putchar (psf, lead_char) ;
						} ;

					if (sign_char == '+' || sign_char == '-')
					{	log_putchar (psf, sign_char) ;
						width_specifier -- ;
						} ;

					if (left_align == SF_FALSE)
						while (width_specifier -- > 0)
							log_putchar (psf, lead_char) ;

					while (tens > 0)
					{	log_putchar (psf, '0' + u / tens) ;
						u %= tens ;
						tens /= 10 ;
						} ;

					while (width_specifier -- > 0)
						log_putchar (psf, lead_char) ;
					break ;

			case 'c': /* char */
					c = va_arg (ap, int) & 0xFF ;
					log_putchar (psf, c) ;
					break ;

			case 'x': /* hex */
			case 'X': /* hex */
					d = va_arg (ap, int) ;

					if (d == 0)
					{	while (--width_specifier > 0)
							log_putchar (psf, lead_char) ;
						log_putchar (psf, '0') ;
						break ;
						} ;
					shift = 28 ;
					width = (width_specifier < 8) ? 8 : width_specifier ;
					while (! ((((uint32_t) 0xF) << shift) & d))
					{	shift -= 4 ;
						width -- ;
						} ;

					while (width > 0 && width_specifier > width)
					{	log_putchar (psf, lead_char) ;
						width_specifier-- ;
						} ;

					while (shift >= 0)
					{	c = (d >> shift) & 0xF ;
						log_putchar (psf, (c > 9) ? c + 'A' - 10 : c + '0') ;
						shift -= 4 ;
						} ;
					break ;

			case 'M': /* int2str */
					d = va_arg (ap, int) ;
					if (CPU_IS_LITTLE_ENDIAN)
					{	istr [0] = d & 0xFF ;
						istr [1] = (d >> 8) & 0xFF ;
						istr [2] = (d >> 16) & 0xFF ;
						istr [3] = (d >> 24) & 0xFF ;
						}
					else
					{	istr [3] = d & 0xFF ;
						istr [2] = (d >> 8) & 0xFF ;
						istr [1] = (d >> 16) & 0xFF ;
						istr [0] = (d >> 24) & 0xFF ;
						} ;
					istr [4] = 0 ;
					strptr = istr ;
					while (*strptr)
					{	c = *strptr++ ;
						log_putchar (psf, c) ;
						} ;
					break ;

			default :
					log_putchar (psf, '*') ;
					log_putchar (psf, c) ;
					log_putchar (psf, '*') ;
					break ;
			} /* switch */
		} /* while */

	va_end (ap) ;
	return ;
} /* psf_log_printf */

/*-----------------------------------------------------------------------------------------------
**  ASCII header printf functions.
**  Some formats (ie NIST) use ascii text in their headers.
**  Format specifiers are the same as the standard printf specifiers (uses vsnprintf).
**  If this generates a compile error on any system, the author should be notified
**  so an alternative vsnprintf can be provided.
*/

void
psf_asciiheader_printf (SF_PRIVATE *psf, const char *format, ...)
{	va_list	argptr ;
	int		maxlen ;
	char	*start ;

	if (! format)
		return ;

	maxlen = strlen ((char*) psf->header.ptr) ;
	start	= ((char*) psf->header.ptr) + maxlen ;
	maxlen	= psf->header.len - maxlen ;

	va_start (argptr, format) ;
	vsnprintf (start, maxlen, format, argptr) ;
	va_end (argptr) ;

	/* Make sure the string is properly terminated. */
	start [maxlen - 1] = 0 ;

	psf->header.indx = strlen ((char*) psf->header.ptr) ;

	return ;
} /* psf_asciiheader_printf */

/*-----------------------------------------------------------------------------------------------
**  Binary header writing functions. Returns number of bytes written.
**
**  Format specifiers for psf_binheader_writef are as follows
**		m	- marker - four bytes - no endian manipulation
**
**		e   - all following numerical values will be little endian
**		E   - all following numerical values will be big endian
**
**		t   - all following O types will be truncated to 4 bytes
**		T   - switch off truncation of all following O types
**
**		1	- single byte value
**		2	- two byte value
**		3	- three byte value
**		4	- four byte value
**		8	- eight byte value (sometimes written as 4 bytes)
**
**		s   - string preceded by a four byte length
**		S   - string including null terminator
**      p   - a Pascal string
**
**		f	- floating point data
**		d	- double precision floating point data
**		h	- 16 binary bytes value
**
**		b	- binary data (see below)
**		z   - zero bytes (ses below)
**		j	- jump forwards or backwards
**
**	To write a word followed by an int (both little endian) use:
**		psf_binheader_writef ("e24", wordval, longval) ;
**
**	To write binary data use:
**		psf_binheader_writef ("b", &bindata, sizeof (bindata)) ;
**
**	To write N zero bytes use:
**			NOTE: due to platform issues (ie x86-64) you should cast the
**			argument to size_t or ensure the variable type is size_t.
**		psf_binheader_writef ("z", N) ;
*/

/* These macros may seem a bit messy but do prevent problems with processors which
** seg. fault when asked to write an int or short to a non-int/short aligned address.
*/

static inline void
header_put_byte (SF_PRIVATE *psf, char x)
{	psf->header.ptr [psf->header.indx++] = x ;
} /* header_put_byte */

#if (CPU_IS_BIG_ENDIAN == 1)
static inline void
header_put_marker (SF_PRIVATE *psf, int x)
{	psf->header.ptr [psf->header.indx++] = (x >> 24) ;
	psf->header.ptr [psf->header.indx++] = (x >> 16) ;
	psf->header.ptr [psf->header.indx++] = (x >> 8) ;
	psf->header.ptr [psf->header.indx++] = x ;
} /* header_put_marker */

#elif (CPU_IS_LITTLE_ENDIAN == 1)
static inline void
header_put_marker (SF_PRIVATE *psf, int x)
{	psf->header.ptr [psf->header.indx++] = x ;
	psf->header.ptr [psf->header.indx++] = (x >> 8) ;
	psf->header.ptr [psf->header.indx++] = (x >> 16) ;
	psf->header.ptr [psf->header.indx++] = (x >> 24) ;
} /* header_put_marker */

#else
#	error "Cannot determine endian-ness of processor."
#endif


static inline void
header_put_be_short (SF_PRIVATE *psf, int x)
{	psf->header.ptr [psf->header.indx++] = (x >> 8) ;
	psf->header.ptr [psf->header.indx++] = x ;
} /* header_put_be_short */

static inline void
header_put_le_short (SF_PRIVATE *psf, int x)
{	psf->header.ptr [psf->header.indx++] = x ;
	psf->header.ptr [psf->header.indx++] = (x >> 8) ;
} /* header_put_le_short */

static inline void
header_put_be_3byte (SF_PRIVATE *psf, int x)
{	psf->header.ptr [psf->header.indx++] = (x >> 16) ;
	psf->header.ptr [psf->header.indx++] = (x >> 8) ;
	psf->header.ptr [psf->header.indx++] = x ;
} /* header_put_be_3byte */

static inline void
header_put_le_3byte (SF_PRIVATE *psf, int x)
{	psf->header.ptr [psf->header.indx++] = x ;
	psf->header.ptr [psf->header.indx++] = (x >> 8) ;
	psf->header.ptr [psf->header.indx++] = (x >> 16) ;
} /* header_put_le_3byte */

static inline void
header_put_be_int (SF_PRIVATE *psf, int x)
{	psf->header.ptr [psf->header.indx++] = (x >> 24) ;
	psf->header.ptr [psf->header.indx++] = (x >> 16) ;
	psf->header.ptr [psf->header.indx++] = (x >> 8) ;
	psf->header.ptr [psf->header.indx++] = x ;
} /* header_put_be_int */

static inline void
header_put_le_int (SF_PRIVATE *psf, int x)
{	psf->header.ptr [psf->header.indx++] = x ;
	psf->header.ptr [psf->header.indx++] = (x >> 8) ;
	psf->header.ptr [psf->header.indx++] = (x >> 16) ;
	psf->header.ptr [psf->header.indx++] = (x >> 24) ;
} /* header_put_le_int */

#if (SIZEOF_SF_COUNT_T == 8)

static inline void
header_put_be_8byte (SF_PRIVATE *psf, sf_count_t x)
{	psf->header.ptr [psf->header.indx++] = (x >> 56) ;
	psf->header.ptr [psf->header.indx++] = (x >> 48) ;
	psf->header.ptr [psf->header.indx++] = (x >> 40) ;
	psf->header.ptr [psf->header.indx++] = (x >> 32) ;
	psf->header.ptr [psf->header.indx++] = (x >> 24) ;
	psf->header.ptr [psf->header.indx++] = (x >> 16) ;
	psf->header.ptr [psf->header.indx++] = (x >> 8) ;
	psf->header.ptr [psf->header.indx++] = x ;
} /* header_put_be_8byte */

static inline void
header_put_le_8byte (SF_PRIVATE *psf, sf_count_t x)
{	psf->header.ptr [psf->header.indx++] = x ;
	psf->header.ptr [psf->header.indx++] = (x >> 8) ;
	psf->header.ptr [psf->header.indx++] = (x >> 16) ;
	psf->header.ptr [psf->header.indx++] = (x >> 24) ;
	psf->header.ptr [psf->header.indx++] = (x >> 32) ;
	psf->header.ptr [psf->header.indx++] = (x >> 40) ;
	psf->header.ptr [psf->header.indx++] = (x >> 48) ;
	psf->header.ptr [psf->header.indx++] = (x >> 56) ;
} /* header_put_le_8byte */

#else
#error "SIZEOF_SF_COUNT_T != 8"
#endif

int
psf_binheader_writef (SF_PRIVATE *psf, const char *format, ...)
{	va_list	argptr ;
	sf_count_t 		countdata ;
	unsigned long 	longdata ;
	unsigned int 	data ;
	float			floatdata ;
	double			doubledata ;
	void			*bindata ;
	size_t			size ;
	char			c, *strptr ;
	int				count = 0, trunc_8to4 = SF_FALSE ;

	if (! format)
		return psf_ftell (psf) ;

	va_start (argptr, format) ;

	while ((c = *format++))
	{
		if (psf->header.indx + 16 >= psf->header.len && psf_bump_header_allocation (psf, 16))
			return count ;

		switch (c)
		{	case ' ' : /* Do nothing. Just used to space out format string. */
					break ;

			case 'e' : /* All conversions are now from LE to host. */
					psf->rwf_endian = SF_ENDIAN_LITTLE ;
					break ;

			case 'E' : /* All conversions are now from BE to host. */
					psf->rwf_endian = SF_ENDIAN_BIG ;
					break ;

			case 't' : /* All 8 byte values now get written as 4 bytes. */
					trunc_8to4 = SF_TRUE ;
					break ;

			case 'T' : /* All 8 byte values now get written as 8 bytes. */
					trunc_8to4 = SF_FALSE ;
					break ;

			case 'm' :
					data = va_arg (argptr, unsigned int) ;
					header_put_marker (psf, data) ;
					count += 4 ;
					break ;

			case '1' :
					data = va_arg (argptr, unsigned int) ;
					header_put_byte (psf, data) ;
					count += 1 ;
					break ;

			case '2' :
					data = va_arg (argptr, unsigned int) ;
					if (psf->rwf_endian == SF_ENDIAN_BIG)
					{	header_put_be_short (psf, data) ;
						}
					else
					{	header_put_le_short (psf, data) ;
						} ;
					count += 2 ;
					break ;

			case '3' : /* tribyte */
					data = va_arg (argptr, unsigned int) ;
					if (psf->rwf_endian == SF_ENDIAN_BIG)
					{	header_put_be_3byte (psf, data) ;
						}
					else
					{	header_put_le_3byte (psf, data) ;
						} ;
					count += 3 ;
					break ;

			case '4' :
					data = va_arg (argptr, unsigned int) ;
					if (psf->rwf_endian == SF_ENDIAN_BIG)
					{	header_put_be_int (psf, data) ;
						}
					else
					{	header_put_le_int (psf, data) ;
						} ;
					count += 4 ;
					break ;

			case '8' :
					countdata = va_arg (argptr, sf_count_t) ;
					if (psf->rwf_endian == SF_ENDIAN_BIG && trunc_8to4 == SF_FALSE)
					{	header_put_be_8byte (psf, countdata) ;
						count += 8 ;
						}
					else if (psf->rwf_endian == SF_ENDIAN_LITTLE && trunc_8to4 == SF_FALSE)
					{	header_put_le_8byte (psf, countdata) ;
						count += 8 ;
						}
					else if (psf->rwf_endian == SF_ENDIAN_BIG && trunc_8to4 == SF_TRUE)
					{	longdata = countdata & 0xFFFFFFFF ;
						header_put_be_int (psf, longdata) ;
						count += 4 ;
						}
					else if (psf->rwf_endian == SF_ENDIAN_LITTLE && trunc_8to4 == SF_TRUE)
					{	longdata = countdata & 0xFFFFFFFF ;
						header_put_le_int (psf, longdata) ;
						count += 4 ;
						}
					break ;

			case 'f' :
					/* Floats are passed as doubles. Is this always true? */
					floatdata = (float) va_arg (argptr, double) ;
					if (psf->rwf_endian == SF_ENDIAN_BIG)
						float32_be_write (floatdata, psf->header.ptr + psf->header.indx) ;
					else
						float32_le_write (floatdata, psf->header.ptr + psf->header.indx) ;
					psf->header.indx += 4 ;
					count += 4 ;
					break ;

			case 'd' :
					doubledata = va_arg (argptr, double) ;
					if (psf->rwf_endian == SF_ENDIAN_BIG)
						double64_be_write (doubledata, psf->header.ptr + psf->header.indx) ;
					else
						double64_le_write (doubledata, psf->header.ptr + psf->header.indx) ;
					psf->header.indx += 8 ;
					count += 8 ;
					break ;

			case 's' :
					/* Write a C string (guaranteed to have a zero terminator). */
					strptr = va_arg (argptr, char *) ;
					size = strlen (strptr) + 1 ;

					if (psf->header.indx + 4 + (sf_count_t) size + (sf_count_t) (size & 1) > psf->header.len && psf_bump_header_allocation (psf, 4 + size + (size & 1)))
						return count ;

					if (psf->rwf_endian == SF_ENDIAN_BIG)
						header_put_be_int (psf, size + (size & 1)) ;
					else
						header_put_le_int (psf, size + (size & 1)) ;
					memcpy (&(psf->header.ptr [psf->header.indx]), strptr, size) ;
					size += (size & 1) ;
					psf->header.indx += size ;
					psf->header.ptr [psf->header.indx - 1] = 0 ;
					count += 4 + size ;
					break ;

			case 'S' :
					/*
					**	Write an AIFF style string (no zero terminator but possibly
					**	an extra pad byte if the string length is odd).
					*/
					strptr = va_arg (argptr, char *) ;
					size = strlen (strptr) ;
					if (psf->header.indx + 4 + (sf_count_t) size + (sf_count_t) (size & 1) > psf->header.len && psf_bump_header_allocation (psf, 4 + size + (size & 1)))
						return count ;
					if (psf->rwf_endian == SF_ENDIAN_BIG)
						header_put_be_int (psf, size) ;
					else
						header_put_le_int (psf, size) ;
					memcpy (&(psf->header.ptr [psf->header.indx]), strptr, size + (size & 1)) ;
					size += (size & 1) ;
					psf->header.indx += size ;
					count += 4 + size ;
					break ;

			case 'p' :
					/* Write a PASCAL string (as used by AIFF files).
					*/
					strptr = va_arg (argptr, char *) ;
					size = strlen (strptr) ;
					size = (size & 1) ? size : size + 1 ;
					size = (size > 254) ? 254 : size ;

					if (psf->header.indx + 1 + (sf_count_t) size > psf->header.len && psf_bump_header_allocation (psf, 1 + size))
						return count ;

					header_put_byte (psf, size) ;
					memcpy (&(psf->header.ptr [psf->header.indx]), strptr, size) ;
					psf->header.indx += size ;
					count += 1 + size ;
					break ;

			case 'b' :
					bindata	= va_arg (argptr, void *) ;
					size	= va_arg (argptr, size_t) ;

					if (psf->header.indx + (sf_count_t) size > psf->header.len && psf_bump_header_allocation (psf, size))
						return count ;

					memcpy (&(psf->header.ptr [psf->header.indx]), bindata, size) ;
					psf->header.indx += size ;
					count += size ;
					break ;

			case 'z' :
					size = va_arg (argptr, size_t) ;

					if (psf->header.indx + (sf_count_t) size > psf->header.len && psf_bump_header_allocation (psf, size))
						return count ;

					count += size ;
					while (size)
					{	psf->header.ptr [psf->header.indx] = 0 ;
						psf->header.indx ++ ;
						size -- ;
						} ;
					break ;

			case 'h' :
					bindata = va_arg (argptr, void *) ;
					memcpy (&(psf->header.ptr [psf->header.indx]), bindata, 16) ;
					psf->header.indx += 16 ;
					count += 16 ;
					break ;

			case 'j' :	/* Jump forwards/backwards by specified amount. */
					size = va_arg (argptr, size_t) ;

					if (psf->header.indx + (sf_count_t) size > psf->header.len && psf_bump_header_allocation (psf, size))
						return count ;

					psf->header.indx += size ;
					count += size ;
					break ;

			case 'o' :	/* Jump to specified offset. */
					size = va_arg (argptr, size_t) ;

					if ((sf_count_t) size >= psf->header.len && psf_bump_header_allocation (psf, size))
						return count ;

					psf->header.indx = size ;
					break ;

			default :
				psf_log_printf (psf, "*** Invalid format specifier `%c'\n", c) ;
				psf->error = SFE_INTERNAL ;
				break ;
			} ;
		} ;

	va_end (argptr) ;
	return count ;
} /* psf_binheader_writef */

/*-----------------------------------------------------------------------------------------------
**  Binary header reading functions. Returns number of bytes read.
**
**	Format specifiers are the same as for header write function above with the following
**	additions:
**
**		p   - jump a given number of position from start of file.
**
**	If format is NULL, psf_binheader_readf returns the current offset.
*/

#if (CPU_IS_BIG_ENDIAN == 1)
#define	GET_MARKER(ptr)	(	(((uint32_t) (ptr) [0]) << 24)	| ((ptr) [1] << 16) |	\
							((ptr) [2] << 8)	| ((ptr) [3]))

#elif (CPU_IS_LITTLE_ENDIAN == 1)
#define	GET_MARKER(ptr)	(	((ptr) [0])			| ((ptr) [1] << 8) |	\
							((ptr) [2] << 16)	| (((uint32_t) (ptr) [3]) << 24))

#else
#	error "Cannot determine endian-ness of processor."
#endif

#define	GET_LE_SHORT(ptr)	(((ptr) [1] << 8) | ((ptr) [0]))
#define	GET_BE_SHORT(ptr)	(((ptr) [0] << 8) | ((ptr) [1]))

#define	GET_LE_3BYTE(ptr)	(	((ptr) [2] << 16) | ((ptr) [1] << 8) | ((ptr) [0]))
#define	GET_BE_3BYTE(ptr)	(	((ptr) [0] << 16) | ((ptr) [1] << 8) | ((ptr) [2]))

#define	GET_LE_INT(ptr)		(	((ptr) [3] << 24)	| ((ptr) [2] << 16) |	\
								((ptr) [1] << 8)	| ((ptr) [0]))

#define	GET_BE_INT(ptr)		(	((ptr) [0] << 24)	| ((ptr) [1] << 16) |	\
								((ptr) [2] << 8)	| ((ptr) [3]))

#define	GET_LE_8BYTE(ptr)	(	(((sf_count_t) (ptr) [7]) << 56)	| (((sf_count_t) (ptr) [6]) << 48) |	\
								(((sf_count_t) (ptr) [5]) << 40)	| (((sf_count_t) (ptr) [4]) << 32) |	\
								(((sf_count_t) (ptr) [3]) << 24)	| (((sf_count_t) (ptr) [2]) << 16) |	\
								(((sf_count_t) (ptr) [1]) << 8)		| ((ptr) [0]))

#define	GET_BE_8BYTE(ptr)	(	(((sf_count_t) (ptr) [0]) << 56)	| (((sf_count_t) (ptr) [1]) << 48) |	\
								(((sf_count_t) (ptr) [2]) << 40)	| (((sf_count_t) (ptr) [3]) << 32) |	\
								(((sf_count_t) (ptr) [4]) << 24)	| (((sf_count_t) (ptr) [5]) << 16) |	\
								(((sf_count_t) (ptr) [6]) << 8)		| ((ptr) [7]))



static int
header_read (SF_PRIVATE *psf, void *ptr, int bytes)
{	int count = 0 ;

	if (psf->header.indx + bytes >= psf->header.len && psf_bump_header_allocation (psf, bytes))
		return count ;

	if (psf->header.indx + bytes > psf->header.end)
	{	count = psf_fread (psf->header.ptr + psf->header.end, 1, bytes - (psf->header.end - psf->header.indx), psf) ;
		if (count != bytes - (int) (psf->header.end - psf->header.indx))
		{	psf_log_printf (psf, "Error : psf_fread returned short count.\n") ;
			return count ;
			} ;
		psf->header.end += count ;
		} ;

	memcpy (ptr, psf->header.ptr + psf->header.indx, bytes) ;
	psf->header.indx += bytes ;

	return bytes ;
} /* header_read */

static void
header_seek (SF_PRIVATE *psf, sf_count_t position, int whence)
{
	switch (whence)
	{	case SEEK_SET :
			if (psf->header.indx + position >= psf->header.len)
				psf_bump_header_allocation (psf, position) ;
			if (position > psf->header.len)
			{	/* Too much header to cache so just seek instead. */
				psf_fseek (psf, position, whence) ;
				return ;
				} ;
			if (position > psf->header.end)
				psf->header.end += psf_fread (psf->header.ptr + psf->header.end, 1, position - psf->header.end, psf) ;
			psf->header.indx = position ;
			break ;

		case SEEK_CUR :
			if (psf->header.indx + position >= psf->header.len)
				psf_bump_header_allocation (psf, position) ;

			if (psf->header.indx + position < 0)
				break ;

			if (psf->header.indx >= psf->header.len)
			{	psf_fseek (psf, position, whence) ;
				return ;
				} ;

			if (psf->header.indx + position <= psf->header.end)
			{	psf->header.indx += position ;
				break ;
				} ;

			if (psf->header.indx + position > psf->header.len)
			{	/* Need to jump this without caching it. */
				psf->header.indx = psf->header.end ;
				psf_fseek (psf, position, SEEK_CUR) ;
				break ;
				} ;

			psf->header.end += psf_fread (psf->header.ptr + psf->header.end, 1, position - (psf->header.end - psf->header.indx), psf) ;
			psf->header.indx = psf->header.end ;
			break ;

		case SEEK_END :
		default :
			psf_log_printf (psf, "Bad whence param in header_seek().\n") ;
			break ;
		} ;

	return ;
} /* header_seek */

static int
header_gets (SF_PRIVATE *psf, char *ptr, int bufsize)
{	int		k ;

	if (psf->header.indx + bufsize >= psf->header.len && psf_bump_header_allocation (psf, bufsize))
		return 0 ;

	for (k = 0 ; k < bufsize - 1 ; k++)
	{	if (psf->header.indx < psf->header.end)
		{	ptr [k] = psf->header.ptr [psf->header.indx] ;
			psf->header.indx ++ ;
			}
		else
		{	psf->header.end += psf_fread (psf->header.ptr + psf->header.end, 1, 1, psf) ;
			ptr [k] = psf->header.ptr [psf->header.indx] ;
			psf->header.indx = psf->header.end ;
			} ;

		if (ptr [k] == '\n')
			break ;
		} ;

	ptr [k] = 0 ;

	return k ;
} /* header_gets */

int
psf_binheader_readf (SF_PRIVATE *psf, char const *format, ...)
{	va_list			argptr ;
	sf_count_t		*countptr, countdata ;
	unsigned char	*ucptr, sixteen_bytes [16] ;
	unsigned int 	*intptr, intdata ;
	unsigned short	*shortptr ;
	char			*charptr ;
	float			*floatptr ;
	double			*doubleptr ;
	char			c ;
	int				byte_count = 0, count = 0 ;

	if (! format)
		return psf_ftell (psf) ;

	va_start (argptr, format) ;

	while ((c = *format++))
	{
		if (psf->header.indx + 16 >= psf->header.len && psf_bump_header_allocation (psf, 16))
			return count ;

		switch (c)
		{	case 'e' : /* All conversions are now from LE to host. */
					psf->rwf_endian = SF_ENDIAN_LITTLE ;
					break ;

			case 'E' : /* All conversions are now from BE to host. */
					psf->rwf_endian = SF_ENDIAN_BIG ;
					break ;

			case 'm' : /* 4 byte marker value eg 'RIFF' */
					intptr = va_arg (argptr, unsigned int*) ;
					*intptr = 0 ;
					ucptr = (unsigned char*) intptr ;
					byte_count += header_read (psf, ucptr, sizeof (int)) ;
					*intptr = GET_MARKER (ucptr) ;
					break ;

			case 'h' :
					intptr = va_arg (argptr, unsigned int*) ;
					*intptr = 0 ;
					ucptr = (unsigned char*) intptr ;
					byte_count += header_read (psf, sixteen_bytes, sizeof (sixteen_bytes)) ;
					{	int k ;
						intdata = 0 ;
						for (k = 0 ; k < 16 ; k++)
							intdata ^= sixteen_bytes [k] << k ;
						}
					*intptr = intdata ;
					break ;

			case '1' :
					charptr = va_arg (argptr, char*) ;
					*charptr = 0 ;
					byte_count += header_read (psf, charptr, sizeof (char)) ;
					break ;

			case '2' : /* 2 byte value with the current endian-ness */
					shortptr = va_arg (argptr, unsigned short*) ;
					*shortptr = 0 ;
					ucptr = (unsigned char*) shortptr ;
					byte_count += header_read (psf, ucptr, sizeof (short)) ;
					if (psf->rwf_endian == SF_ENDIAN_BIG)
						*shortptr = GET_BE_SHORT (ucptr) ;
					else
						*shortptr = GET_LE_SHORT (ucptr) ;
					break ;

			case '3' : /* 3 byte value with the current endian-ness */
					intptr = va_arg (argptr, unsigned int*) ;
					*intptr = 0 ;
					byte_count += header_read (psf, sixteen_bytes, 3) ;
					if (psf->rwf_endian == SF_ENDIAN_BIG)
						*intptr = GET_BE_3BYTE (sixteen_bytes) ;
					else
						*intptr = GET_LE_3BYTE (sixteen_bytes) ;
					break ;

			case '4' : /* 4 byte value with the current endian-ness */
					intptr = va_arg (argptr, unsigned int*) ;
					*intptr = 0 ;
					ucptr = (unsigned char*) intptr ;
					byte_count += header_read (psf, ucptr, sizeof (int)) ;
					if (psf->rwf_endian == SF_ENDIAN_BIG)
						*intptr = psf_get_be32 (ucptr, 0) ;
					else
						*intptr = psf_get_le32 (ucptr, 0) ;
					break ;

			case '8' : /* 8 byte value with the current endian-ness */
					countptr = va_arg (argptr, sf_count_t *) ;
					*countptr = 0 ;
					byte_count += header_read (psf, sixteen_bytes, 8) ;
					if (psf->rwf_endian == SF_ENDIAN_BIG)
						countdata = psf_get_be64 (sixteen_bytes, 0) ;
					else
						countdata = psf_get_le64 (sixteen_bytes, 0) ;
					*countptr = countdata ;
					break ;

			case 'f' : /* Float conversion */
					floatptr = va_arg (argptr, float *) ;
					*floatptr = 0.0 ;
					byte_count += header_read (psf, floatptr, sizeof (float)) ;
					if (psf->rwf_endian == SF_ENDIAN_BIG)
						*floatptr = float32_be_read ((unsigned char*) floatptr) ;
					else
						*floatptr = float32_le_read ((unsigned char*) floatptr) ;
					break ;

			case 'd' : /* double conversion */
					doubleptr = va_arg (argptr, double *) ;
					*doubleptr = 0.0 ;
					byte_count += header_read (psf, doubleptr, sizeof (double)) ;
					if (psf->rwf_endian == SF_ENDIAN_BIG)
						*doubleptr = double64_be_read ((unsigned char*) doubleptr) ;
					else
						*doubleptr = double64_le_read ((unsigned char*) doubleptr) ;
					break ;

			case 's' :
					psf_log_printf (psf, "Format conversion 's' not implemented yet.\n") ;
					/*
					strptr = va_arg (argptr, char *) ;
					size   = strlen (strptr) + 1 ;
					size  += (size & 1) ;
					longdata = H2LE_32 (size) ;
					get_int (psf, longdata) ;
					memcpy (&(psf->header.ptr [psf->header.indx]), strptr, size) ;
					psf->header.indx += size ;
					*/
					break ;

			case 'b' : /* Raw bytes */
					charptr = va_arg (argptr, char*) ;
					count = va_arg (argptr, size_t) ;
					memset (charptr, 0, count) ;
					byte_count += header_read (psf, charptr, count) ;
					break ;

			case 'G' :
					charptr = va_arg (argptr, char*) ;
					count = va_arg (argptr, size_t) ;
					memset (charptr, 0, count) ;

					if (psf->header.indx + count >= psf->header.len && psf_bump_header_allocation (psf, count))
						return 0 ;

					byte_count += header_gets (psf, charptr, count) ;
					break ;

			case 'z' :
					psf_log_printf (psf, "Format conversion 'z' not implemented yet.\n") ;
					/*
					size    = va_arg (argptr, size_t) ;
					while (size)
					{	psf->header.ptr [psf->header.indx] = 0 ;
						psf->header.indx ++ ;
						size -- ;
						} ;
					*/
					break ;

			case 'p' :	/* Seek to position from start. */
					count = va_arg (argptr, size_t) ;
					header_seek (psf, count, SEEK_SET) ;
					byte_count = count ;
					break ;

			case 'j' :	/* Seek to position from current position. */
					count = va_arg (argptr, size_t) ;
					header_seek (psf, count, SEEK_CUR) ;
					byte_count += count ;
					break ;

			default :
				psf_log_printf (psf, "*** Invalid format specifier `%c'\n", c) ;
				psf->error = SFE_INTERNAL ;
				break ;
			} ;
		} ;

	va_end (argptr) ;

	return byte_count ;
} /* psf_binheader_readf */

/*-----------------------------------------------------------------------------------------------
*/

sf_count_t
psf_default_seek (SF_PRIVATE *psf, int UNUSED (mode), sf_count_t samples_from_start)
{	sf_count_t position, retval ;

	if (! (psf->blockwidth && psf->dataoffset >= 0))
	{	psf->error = SFE_BAD_SEEK ;
		return	PSF_SEEK_ERROR ;
		} ;

	if (! psf->sf.seekable)
	{	psf->error = SFE_NOT_SEEKABLE ;
		return	PSF_SEEK_ERROR ;
		} ;

	position = psf->dataoffset + psf->blockwidth * samples_from_start ;

	if ((retval = psf_fseek (psf, position, SEEK_SET)) != position)
	{	psf->error = SFE_SEEK_FAILED ;
		return PSF_SEEK_ERROR ;
		} ;

	return samples_from_start ;
} /* psf_default_seek */

/*-----------------------------------------------------------------------------------------------
*/

void
psf_hexdump (const void *ptr, int len)
{	const char *data ;
	char	ascii [17] ;
	int		k, m ;

	if ((data = ptr) == NULL)
		return ;
	if (len <= 0)
		return ;

	puts ("") ;
	for (k = 0 ; k < len ; k += 16)
	{	memset (ascii, ' ', sizeof (ascii)) ;

		printf ("%08X: ", k) ;
		for (m = 0 ; m < 16 && k + m < len ; m++)
		{	printf (m == 8 ? " %02X " : "%02X ", data [k + m] & 0xFF) ;
			ascii [m] = psf_isprint (data [k + m]) ? data [k + m] : '.' ;
			} ;

		if (m <= 8) printf (" ") ;
		for ( ; m < 16 ; m++) printf ("   ") ;

		ascii [16] = 0 ;
		printf (" %s\n", ascii) ;
		} ;

	puts ("") ;
} /* psf_hexdump */

void
psf_log_SF_INFO (SF_PRIVATE *psf)
{	psf_log_printf (psf, "---------------------------------\n") ;

	psf_log_printf (psf, " Sample rate :   %d\n", psf->sf.samplerate) ;
	if (psf->sf.frames == SF_COUNT_MAX)
		psf_log_printf (psf, " Frames      :   unknown\n") ;
	else
		psf_log_printf (psf, " Frames      :   %D\n", psf->sf.frames) ;
	psf_log_printf (psf, " Channels    :   %d\n", psf->sf.channels) ;

	psf_log_printf (psf, " Format      :   0x%X\n", psf->sf.format) ;
	psf_log_printf (psf, " Sections    :   %d\n", psf->sf.sections) ;
	psf_log_printf (psf, " Seekable    :   %s\n", psf->sf.seekable ? "TRUE" : "FALSE") ;

	psf_log_printf (psf, "---------------------------------\n") ;
} /* psf_dump_SFINFO */

/*========================================================================================
*/

void*
psf_memset (void *s, int c, sf_count_t len)
{	char	*ptr ;
	int 	setcount ;

	ptr = (char *) s ;

	while (len > 0)
	{	setcount = (len > 0x10000000) ? 0x10000000 : (int) len ;

		memset (ptr, c, setcount) ;

		ptr += setcount ;
		len -= setcount ;
		} ;

	return s ;
} /* psf_memset */


/*
** Clang refuses to do sizeof (SF_CUES_VAR (cue_count)) so we have to manually
** bodgy something up instead.
*/

typedef SF_CUES_VAR (0) SF_CUES_0 ;

/* calculate size of SF_CUES struct given number of cues */
#define SF_CUES_VAR_SIZE(count)	(sizeof (SF_CUES_0) + count * sizeof (SF_CUE_POINT))

/* calculate number of cues in SF_CUES struct given data size */
#define SF_CUES_COUNT(datasize) (((datasize) - sizeof (uint32_t)) / sizeof (SF_CUE_POINT))

SF_CUES *
psf_cues_alloc (uint32_t cue_count)
{	SF_CUES *pcues = calloc (1, SF_CUES_VAR_SIZE (cue_count)) ;

	pcues->cue_count = cue_count ;
	return pcues ;
} /* psf_cues_alloc */

SF_CUES *
psf_cues_dup (const void * ptr, size_t datasize)
{	const SF_CUES *pcues = ptr ;
	SF_CUES *pnew = NULL ;

	if (pcues->cue_count <= SF_CUES_COUNT (datasize))
	{	/* check that passed-in datasize is consistent with cue_count in passed-in SF_CUES struct */
		pnew = psf_cues_alloc (pcues->cue_count) ;
		memcpy (pnew, pcues, SF_CUES_VAR_SIZE (pcues->cue_count)) ;
	}

	return pnew ;
} /* psf_cues_dup */

void
psf_get_cues (SF_PRIVATE * psf, void * data, size_t datasize)
{
	if (psf->cues)
	{	uint32_t cue_count = SF_CUES_COUNT (datasize) ;

		cue_count = SF_MIN (cue_count, psf->cues->cue_count) ;
		memcpy (data, psf->cues, SF_CUES_VAR_SIZE (cue_count)) ;
		((SF_CUES*) data)->cue_count = cue_count ;
		} ;

	return ;
} /* psf_get_cues */


SF_INSTRUMENT *
psf_instrument_alloc (void)
{	SF_INSTRUMENT *instr ;

	instr = calloc (1, sizeof (SF_INSTRUMENT)) ;

	if (instr == NULL)
		return NULL ;

	/* Set non-zero default values. */
	instr->basenote = -1 ;
	instr->velocity_lo = -1 ;
	instr->velocity_hi = -1 ;
	instr->key_lo = -1 ;
	instr->key_hi = -1 ;

	return instr ;
} /* psf_instrument_alloc */

void
psf_sanitize_string (char * cptr, int len)
{
	do
	{
		len -- ;
		cptr [len] = psf_isprint (cptr [len]) ? cptr [len] : '.' ;
	}
	while (len > 0) ;
} /* psf_sanitize_string */

void
psf_get_date_str (char *str, int maxlen)
{	time_t		current ;
	struct tm	timedata, *tmptr ;

	time (&current) ;

#if defined (HAVE_GMTIME_R)
	/* If the re-entrant version is available, use it. */
	tmptr = gmtime_r (&current, &timedata) ;
#elif defined (HAVE_GMTIME)
	/* Otherwise use the standard one and copy the data to local storage. */
	tmptr = gmtime (&current) ;
	memcpy (&timedata, tmptr, sizeof (timedata)) ;
#else
	tmptr = NULL ;
#endif

	if (tmptr)
		snprintf (str, maxlen, "%4d-%02d-%02d %02d:%02d:%02d UTC",
			1900 + timedata.tm_year, timedata.tm_mon, timedata.tm_mday,
			timedata.tm_hour, timedata.tm_min, timedata.tm_sec) ;
	else
		snprintf (str, maxlen, "Unknown date") ;

	return ;
} /* psf_get_date_str */

int
subformat_to_bytewidth (int format)
{
	switch (format)
	{	case SF_FORMAT_PCM_U8 :
		case SF_FORMAT_PCM_S8 :
				return 1 ;
		case SF_FORMAT_PCM_16 :
				return 2 ;
		case SF_FORMAT_PCM_24 :
				return 3 ;
		case SF_FORMAT_PCM_32 :
		case SF_FORMAT_FLOAT :
				return 4 ;
		case SF_FORMAT_DOUBLE :
				return 8 ;
		} ;

	return 0 ;
} /* subformat_to_bytewidth */

int
s_bitwidth_to_subformat (int bits)
{	static int array [] =
	{	SF_FORMAT_PCM_S8, SF_FORMAT_PCM_16, SF_FORMAT_PCM_24, SF_FORMAT_PCM_32
		} ;

	if (bits < 8 || bits > 32)
		return 0 ;

	return array [((bits + 7) / 8) - 1] ;
} /* bitwidth_to_subformat */

int
u_bitwidth_to_subformat (int bits)
{	static int array [] =
	{	SF_FORMAT_PCM_U8, SF_FORMAT_PCM_16, SF_FORMAT_PCM_24, SF_FORMAT_PCM_32
		} ;

	if (bits < 8 || bits > 32)
		return 0 ;

	return array [((bits + 7) / 8) - 1] ;
} /* bitwidth_to_subformat */

/*
**	psf_rand_int32 : Not crypto quality, but more than adequate for things
**	like stream serial numbers in Ogg files or the unique_id field of the
**	SF_PRIVATE struct.
*/

int32_t
psf_rand_int32 (void)
{	static uint64_t value = 0 ;
	int k, count ;

	if (value == 0)
	{
#if HAVE_GETTIMEOFDAY
		struct timeval tv ;
		gettimeofday (&tv, NULL) ;
		value = tv.tv_sec + tv.tv_usec ;
#else
		value = time (NULL) ;
#endif
		} ;

	count = 4 + (value & 7) ;
	for (k = 0 ; k < count ; k++)
		value = (11117 * value + 211231) & 0x7fffffff ;

	return (int32_t) value ;
} /* psf_rand_int32 */

void
append_snprintf (char * dest, size_t maxlen, const char * fmt, ...)
{	size_t len = strlen (dest) ;

	if (len < maxlen)
	{	va_list ap ;

		va_start (ap, fmt) ;
		vsnprintf (dest + len, maxlen - len, fmt, ap) ;
		va_end (ap) ;
		} ;

	return ;
} /* append_snprintf */


void
psf_strlcpy_crlf (char *dest, const char *src, size_t destmax, size_t srcmax)
{	/* Must be minus 2 so it can still expand a single trailing '\n' or '\r'. */
	char * destend = dest + destmax - 2 ;
	const char * srcend = src + srcmax ;

	while (dest < destend && src < srcend)
	{	if ((src [0] == '\r' && src [1] == '\n') || (src [0] == '\n' && src [1] == '\r'))
		{	*dest++ = '\r' ;
			*dest++ = '\n' ;
			src += 2 ;
			continue ;
			} ;

		if (src [0] == '\r')
		{	*dest++ = '\r' ;
			*dest++ = '\n' ;
			src += 1 ;
			continue ;
			} ;

		if (src [0] == '\n')
		{	*dest++ = '\r' ;
			*dest++ = '\n' ;
			src += 1 ;
			continue ;
			} ;

		*dest++ = *src++ ;
		} ;

	/* Make sure dest is terminated. */
	*dest = 0 ;
} /* psf_strlcpy_crlf */

sf_count_t
psf_decode_frame_count (SF_PRIVATE *psf)
{	sf_count_t count, readlen, total = 0 ;
	BUF_UNION	ubuf ;

	/* If we're reading from a pipe or the file is too long, just return SF_COUNT_MAX. */
	if (psf_is_pipe (psf) || psf->datalength > 0x1000000)
		return SF_COUNT_MAX ;

	psf_fseek (psf, psf->dataoffset, SEEK_SET) ;

	readlen = ARRAY_LEN (ubuf.ibuf) / psf->sf.channels ;
	readlen *= psf->sf.channels ;

	while ((count = psf->read_int (psf, ubuf.ibuf, readlen)) > 0)
		total += count ;

	psf_fseek (psf, psf->dataoffset, SEEK_SET) ;

	return total / psf->sf.channels ;
} /* psf_decode_frame_count */

/*==============================================================================
*/

#define CASE_NAME(x)		case x : return #x ; break ;

const char *
str_of_major_format (int format)
{	switch (SF_CONTAINER (format))
	{	CASE_NAME (SF_FORMAT_WAV) ;
		CASE_NAME (SF_FORMAT_AIFF) ;
		CASE_NAME (SF_FORMAT_AU) ;
		CASE_NAME (SF_FORMAT_RAW) ;
		CASE_NAME (SF_FORMAT_PAF) ;
		CASE_NAME (SF_FORMAT_SVX) ;
		CASE_NAME (SF_FORMAT_NIST) ;
		CASE_NAME (SF_FORMAT_VOC) ;
		CASE_NAME (SF_FORMAT_IRCAM) ;
		CASE_NAME (SF_FORMAT_W64) ;
		CASE_NAME (SF_FORMAT_MAT4) ;
		CASE_NAME (SF_FORMAT_MAT5) ;
		CASE_NAME (SF_FORMAT_PVF) ;
		CASE_NAME (SF_FORMAT_XI) ;
		CASE_NAME (SF_FORMAT_HTK) ;
		CASE_NAME (SF_FORMAT_SDS) ;
		CASE_NAME (SF_FORMAT_AVR) ;
		CASE_NAME (SF_FORMAT_WAVEX) ;
		CASE_NAME (SF_FORMAT_SD2) ;
		CASE_NAME (SF_FORMAT_FLAC) ;
		CASE_NAME (SF_FORMAT_CAF) ;
		CASE_NAME (SF_FORMAT_WVE) ;
		CASE_NAME (SF_FORMAT_OGG) ;
		default :
			break ;
		} ;

	return "BAD_MAJOR_FORMAT" ;
} /* str_of_major_format */

const char *
str_of_minor_format (int format)
{	switch (SF_CODEC (format))
	{	CASE_NAME (SF_FORMAT_PCM_S8) ;
		CASE_NAME (SF_FORMAT_PCM_16) ;
		CASE_NAME (SF_FORMAT_PCM_24) ;
		CASE_NAME (SF_FORMAT_PCM_32) ;
		CASE_NAME (SF_FORMAT_PCM_U8) ;
		CASE_NAME (SF_FORMAT_FLOAT) ;
		CASE_NAME (SF_FORMAT_DOUBLE) ;
		CASE_NAME (SF_FORMAT_ULAW) ;
		CASE_NAME (SF_FORMAT_ALAW) ;
		CASE_NAME (SF_FORMAT_IMA_ADPCM) ;
		CASE_NAME (SF_FORMAT_MS_ADPCM) ;
		CASE_NAME (SF_FORMAT_GSM610) ;
		CASE_NAME (SF_FORMAT_VOX_ADPCM) ;
		CASE_NAME (SF_FORMAT_NMS_ADPCM_16) ;
		CASE_NAME (SF_FORMAT_NMS_ADPCM_24) ;
		CASE_NAME (SF_FORMAT_NMS_ADPCM_32) ;
		CASE_NAME (SF_FORMAT_G721_32) ;
		CASE_NAME (SF_FORMAT_G723_24) ;
		CASE_NAME (SF_FORMAT_G723_40) ;
		CASE_NAME (SF_FORMAT_DWVW_12) ;
		CASE_NAME (SF_FORMAT_DWVW_16) ;
		CASE_NAME (SF_FORMAT_DWVW_24) ;
		CASE_NAME (SF_FORMAT_DWVW_N) ;
		CASE_NAME (SF_FORMAT_DPCM_8) ;
		CASE_NAME (SF_FORMAT_DPCM_16) ;
		CASE_NAME (SF_FORMAT_VORBIS) ;
		default :
			break ;
		} ;

	return "BAD_MINOR_FORMAT" ;
} /* str_of_minor_format */

const char *
str_of_open_mode (int mode)
{	switch (mode)
	{	CASE_NAME (SFM_READ) ;
		CASE_NAME (SFM_WRITE) ;
		CASE_NAME (SFM_RDWR) ;

		default :
			break ;
		} ;

	return "BAD_MODE" ;
} /* str_of_open_mode */

const char *
str_of_endianness (int end)
{	switch (end)
	{	CASE_NAME (SF_ENDIAN_BIG) ;
		CASE_NAME (SF_ENDIAN_LITTLE) ;
		CASE_NAME (SF_ENDIAN_CPU) ;
		default :
			break ;
		} ;

	/* Zero length string for SF_ENDIAN_FILE. */
	return "" ;
} /* str_of_endianness */

/*==============================================================================
*/

void
psf_f2s_array (const float *src, short *dest, int count, int normalize)
{	float 			normfact ;

	normfact = normalize ? (1.0 * 0x7FFF) : 1.0 ;
	while (--count >= 0)
		dest [count] = lrintf (src [count] * normfact) ;

	return ;
} /* psf_f2s_array */

void
psf_f2s_clip_array (const float *src, short *dest, int count, int normalize)
{	float			normfact, scaled_value ;

	normfact = normalize ? (1.0 * 0x8000) : 1.0 ;

	while (--count >= 0)
	{	scaled_value = src [count] * normfact ;
		if (CPU_CLIPS_POSITIVE == 0 && scaled_value >= (1.0 * 0x7FFF))
		{	dest [count] = 0x7FFF ;
			continue ;
			} ;
		if (CPU_CLIPS_NEGATIVE == 0 && scaled_value <= (-8.0 * 0x1000))
		{	dest [count] = 0x8000 ;
			continue ;
			} ;

		dest [count] = lrintf (scaled_value) ;
		} ;

	return ;
} /* psf_f2s_clip_array */

void
psf_d2s_array (const double *src, short *dest, int count, int normalize)
{	double 			normfact ;

	normfact = normalize ? (1.0 * 0x7FFF) : 1.0 ;
	while (--count >= 0)
		dest [count] = lrint (src [count] * normfact) ;

	return ;
} /* psf_f2s_array */

void
psf_d2s_clip_array (const double *src, short *dest, int count, int normalize)
{	double			normfact, scaled_value ;

	normfact = normalize ? (1.0 * 0x8000) : 1.0 ;

	while (--count >= 0)
	{	scaled_value = src [count] * normfact ;
		if (CPU_CLIPS_POSITIVE == 0 && scaled_value >= (1.0 * 0x7FFF))
		{	dest [count] = 0x7FFF ;
			continue ;
			} ;
		if (CPU_CLIPS_NEGATIVE == 0 && scaled_value <= (-8.0 * 0x1000))
		{	dest [count] = 0x8000 ;
			continue ;
			} ;

		dest [count] = lrint (scaled_value) ;
		} ;

	return ;
} /* psf_d2s_clip_array */


void
psf_f2i_array (const float *src, int *dest, int count, int normalize)
{	float 			normfact ;

	normfact = normalize ? (1.0 * 0x7FFFFFFF) : 1.0 ;
	while (--count >= 0)
		dest [count] = lrintf (src [count] * normfact) ;

	return ;
} /* psf_f2i_array */

void
psf_f2i_clip_array (const float *src, int *dest, int count, int normalize)
{	float			normfact, scaled_value ;

	normfact = normalize ? (8.0 * 0x10000000) : 1.0 ;

	while (--count >= 0)
	{	scaled_value = src [count] * normfact ;
		if (CPU_CLIPS_POSITIVE == 0 && scaled_value >= (1.0 * 0x7FFFFFFF))
		{	dest [count] = 0x7FFFFFFF ;
			continue ;
			} ;
		if (CPU_CLIPS_NEGATIVE == 0 && scaled_value <= (-8.0 * 0x10000000))
		{	dest [count] = 0x80000000 ;
			continue ;
			} ;

		dest [count] = lrintf (scaled_value) ;
		} ;

	return ;
} /* psf_f2i_clip_array */

void
psf_d2i_array (const double *src, int *dest, int count, int normalize)
{	double 			normfact ;

	normfact = normalize ? (1.0 * 0x7FFFFFFF) : 1.0 ;
	while (--count >= 0)
		dest [count] = lrint (src [count] * normfact) ;

	return ;
} /* psf_f2i_array */

void
psf_d2i_clip_array (const double *src, int *dest, int count, int normalize)
{	double			normfact, scaled_value ;

	normfact = normalize ? (8.0 * 0x10000000) : 1.0 ;

	while (--count >= 0)
	{	scaled_value = src [count] * normfact ;
		if (CPU_CLIPS_POSITIVE == 0 && scaled_value >= (1.0 * 0x7FFFFFFF))
		{	dest [count] = 0x7FFFFFFF ;
			continue ;
			} ;
		if (CPU_CLIPS_NEGATIVE == 0 && scaled_value <= (-8.0 * 0x10000000))
		{	dest [count] = 0x80000000 ;
			continue ;
			} ;

		dest [count] = lrint (scaled_value) ;
		} ;

	return ;
} /* psf_d2i_clip_array */

FILE *
psf_open_tmpfile (char * fname, size_t fnamelen)
{	const char * tmpdir ;
	FILE * file ;

	if (OS_IS_WIN32)
		tmpdir = getenv ("TEMP") ;
	else
	{	tmpdir = getenv ("TMPDIR") ;
		tmpdir = tmpdir == NULL ? "/tmp" : tmpdir ;
		} ;

	if (tmpdir && access (tmpdir, R_OK | W_OK | X_OK) == 0)
	{	snprintf (fname, fnamelen, "%s/%x%x-alac.tmp", tmpdir, psf_rand_int32 (), psf_rand_int32 ()) ;
		if ((file = fopen (fname, "wb+")) != NULL)
			return file ;
		} ;

	snprintf (fname, fnamelen, "%x%x-alac.tmp", psf_rand_int32 (), psf_rand_int32 ()) ;
	if ((file = fopen (fname, "wb+")) != NULL)
		return file ;

	memset (fname, 0, fnamelen) ;
	return NULL ;
} /* psf_open_tmpfile */
