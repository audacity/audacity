/*
** Copyright (C) 1999-2009 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#include	"sfconfig.h"

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<limits.h>
#include	<math.h>

#include	"sndfile.h"
#include	"sfendian.h"
#include	"common.h"

#if CPU_IS_LITTLE_ENDIAN
	#define DOUBLE64_READ	double64_le_read
	#define DOUBLE64_WRITE	double64_le_write
#elif CPU_IS_BIG_ENDIAN
	#define DOUBLE64_READ	double64_be_read
	#define DOUBLE64_WRITE	double64_be_write
#endif

/* A 32 number which will not overflow when multiplied by sizeof (double). */
#define SENSIBLE_LEN	(0x8000000)

/*--------------------------------------------------------------------------------------------
**	Processor floating point capabilities. double64_get_capability () returns one of the
**	latter three values.
*/

enum
{	DOUBLE_UNKNOWN		= 0x00,
	DOUBLE_CAN_RW_LE	= 0x23,
	DOUBLE_CAN_RW_BE	= 0x34,
	DOUBLE_BROKEN_LE	= 0x45,
	DOUBLE_BROKEN_BE	= 0x56
} ;

/*--------------------------------------------------------------------------------------------
**	Prototypes for private functions.
*/

static sf_count_t		host_read_d2s	(SF_PRIVATE *psf, short *ptr, sf_count_t len) ;
static sf_count_t		host_read_d2i	(SF_PRIVATE *psf, int *ptr, sf_count_t len) ;
static sf_count_t		host_read_d2f	(SF_PRIVATE *psf, float *ptr, sf_count_t len) ;
static sf_count_t		host_read_d		(SF_PRIVATE *psf, double *ptr, sf_count_t len) ;

static sf_count_t		host_write_s2d	(SF_PRIVATE *psf, const short *ptr, sf_count_t len) ;
static sf_count_t		host_write_i2d	(SF_PRIVATE *psf, const int *ptr, sf_count_t len) ;
static sf_count_t		host_write_f2d	(SF_PRIVATE *psf, const float *ptr, sf_count_t len) ;
static sf_count_t		host_write_d	(SF_PRIVATE *psf, const double *ptr, sf_count_t len) ;

static void		double64_peak_update	(SF_PRIVATE *psf, const double *buffer, int count, sf_count_t indx) ;

static int		double64_get_capability	(SF_PRIVATE *psf) ;

static sf_count_t	replace_read_d2s	(SF_PRIVATE *psf, short *ptr, sf_count_t len) ;
static sf_count_t	replace_read_d2i	(SF_PRIVATE *psf, int *ptr, sf_count_t len) ;
static sf_count_t	replace_read_d2f	(SF_PRIVATE *psf, float *ptr, sf_count_t len) ;
static sf_count_t	replace_read_d	(SF_PRIVATE *psf, double *ptr, sf_count_t len) ;

static sf_count_t	replace_write_s2d	(SF_PRIVATE *psf, const short *ptr, sf_count_t len) ;
static sf_count_t	replace_write_i2d	(SF_PRIVATE *psf, const int *ptr, sf_count_t len) ;
static sf_count_t	replace_write_f2d	(SF_PRIVATE *psf, const float *ptr, sf_count_t len) ;
static sf_count_t	replace_write_d	(SF_PRIVATE *psf, const double *ptr, sf_count_t len) ;

static	void	d2bd_read (double *buffer, int count) ;
static	void	bd2d_write (double *buffer, int count) ;

/*--------------------------------------------------------------------------------------------
**	Exported functions.
*/

int
double64_init	(SF_PRIVATE *psf)
{	static int double64_caps ;

	double64_caps = double64_get_capability (psf) ;

	psf->blockwidth = sizeof (double) * psf->sf.channels ;

	if (psf->mode == SFM_READ || psf->mode == SFM_RDWR)
	{	switch (psf->endian + double64_caps)
		{	case (SF_ENDIAN_BIG + DOUBLE_CAN_RW_BE) :
					psf->data_endswap = SF_FALSE ;
					psf->read_short		= host_read_d2s ;
					psf->read_int		= host_read_d2i ;
					psf->read_float		= host_read_d2f ;
					psf->read_double	= host_read_d ;
					break ;

			case (SF_ENDIAN_LITTLE + DOUBLE_CAN_RW_LE) :
					psf->data_endswap = SF_FALSE ;
					psf->read_short		= host_read_d2s ;
					psf->read_int		= host_read_d2i ;
					psf->read_float		= host_read_d2f ;
					psf->read_double	= host_read_d ;
					break ;

			case (SF_ENDIAN_BIG + DOUBLE_CAN_RW_LE) :
					psf->data_endswap = SF_TRUE ;
					psf->read_short		= host_read_d2s ;
					psf->read_int		= host_read_d2i ;
					psf->read_float		= host_read_d2f ;
					psf->read_double	= host_read_d ;
					break ;

			case (SF_ENDIAN_LITTLE + DOUBLE_CAN_RW_BE) :
					psf->data_endswap = SF_TRUE ;
					psf->read_short		= host_read_d2s ;
					psf->read_int		= host_read_d2i ;
					psf->read_float		= host_read_d2f ;
					psf->read_double	= host_read_d ;
					break ;

			/* When the CPU is not IEEE compatible. */
			case (SF_ENDIAN_BIG + DOUBLE_BROKEN_BE) :
					psf->data_endswap = SF_FALSE ;
					psf->read_short		= replace_read_d2s ;
					psf->read_int		= replace_read_d2i ;
					psf->read_float		= replace_read_d2f ;
					psf->read_double	= replace_read_d ;
					break ;

			case (SF_ENDIAN_LITTLE + DOUBLE_BROKEN_LE) :
					psf->data_endswap = SF_FALSE ;
					psf->read_short		= replace_read_d2s ;
					psf->read_int		= replace_read_d2i ;
					psf->read_float		= replace_read_d2f ;
					psf->read_double	= replace_read_d ;
					break ;

			case (SF_ENDIAN_BIG + DOUBLE_BROKEN_LE) :
					psf->data_endswap = SF_TRUE ;
					psf->read_short		= replace_read_d2s ;
					psf->read_int		= replace_read_d2i ;
					psf->read_float		= replace_read_d2f ;
					psf->read_double	= replace_read_d ;
					break ;

			case (SF_ENDIAN_LITTLE + DOUBLE_BROKEN_BE) :
					psf->data_endswap = SF_TRUE ;
					psf->read_short		= replace_read_d2s ;
					psf->read_int		= replace_read_d2i ;
					psf->read_float		= replace_read_d2f ;
					psf->read_double	= replace_read_d ;
					break ;

			default : break ;
			} ;
		} ;

	if (psf->mode == SFM_WRITE || psf->mode == SFM_RDWR)
	{	switch (psf->endian + double64_caps)
		{	case (SF_ENDIAN_LITTLE + DOUBLE_CAN_RW_LE) :
					psf->data_endswap = SF_FALSE ;
					psf->write_short	= host_write_s2d ;
					psf->write_int		= host_write_i2d ;
					psf->write_float	= host_write_f2d ;
					psf->write_double	= host_write_d ;
					break ;

			case (SF_ENDIAN_BIG + DOUBLE_CAN_RW_BE) :
					psf->data_endswap = SF_FALSE ;
					psf->write_short	= host_write_s2d ;
					psf->write_int		= host_write_i2d ;
					psf->write_float	= host_write_f2d ;
					psf->write_double	= host_write_d ;
					break ;

			case (SF_ENDIAN_BIG + DOUBLE_CAN_RW_LE) :
					psf->data_endswap = SF_TRUE ;
					psf->write_short	= host_write_s2d ;
					psf->write_int		= host_write_i2d ;
					psf->write_float	= host_write_f2d ;
					psf->write_double	= host_write_d ;
					break ;

			case (SF_ENDIAN_LITTLE + DOUBLE_CAN_RW_BE) :
					psf->data_endswap = SF_TRUE ;
					psf->write_short	= host_write_s2d ;
					psf->write_int		= host_write_i2d ;
					psf->write_float	= host_write_f2d ;
					psf->write_double	= host_write_d ;
					break ;

			/* When the CPU is not IEEE compatible. */
			case (SF_ENDIAN_LITTLE + DOUBLE_BROKEN_LE) :
					psf->data_endswap = SF_FALSE ;
					psf->write_short	= replace_write_s2d ;
					psf->write_int		= replace_write_i2d ;
					psf->write_float	= replace_write_f2d ;
					psf->write_double	= replace_write_d ;
					break ;

			case (SF_ENDIAN_BIG + DOUBLE_BROKEN_BE) :
					psf->data_endswap = SF_FALSE ;
					psf->write_short	= replace_write_s2d ;
					psf->write_int		= replace_write_i2d ;
					psf->write_float	= replace_write_f2d ;
					psf->write_double	= replace_write_d ;
					break ;

			case (SF_ENDIAN_BIG + DOUBLE_BROKEN_LE) :
					psf->data_endswap = SF_TRUE ;
					psf->write_short	= replace_write_s2d ;
					psf->write_int		= replace_write_i2d ;
					psf->write_float	= replace_write_f2d ;
					psf->write_double	= replace_write_d ;
					break ;

			case (SF_ENDIAN_LITTLE + DOUBLE_BROKEN_BE) :
					psf->data_endswap = SF_TRUE ;
					psf->write_short	= replace_write_s2d ;
					psf->write_int		= replace_write_i2d ;
					psf->write_float	= replace_write_f2d ;
					psf->write_double	= replace_write_d ;
					break ;

			default : break ;
			} ;
		} ;

	if (psf->filelength > psf->dataoffset)
	{	psf->datalength = (psf->dataend > 0) ? psf->dataend - psf->dataoffset :
							psf->filelength - psf->dataoffset ;
		}
	else
		psf->datalength = 0 ;

	psf->sf.frames = psf->datalength / psf->blockwidth ;

	return 0 ;
} /* double64_init */

/*----------------------------------------------------------------------------
** From : http://www.hpcf.cam.ac.uk/fp_formats.html
**
** 64 bit double precision layout (big endian)
** 	  Sign				bit 0
** 	  Exponent			bits 1-11
** 	  Mantissa			bits 12-63
** 	  Exponent Offset	1023
**
**            double             single
**
** +INF     7FF0000000000000     7F800000
** -INF     FFF0000000000000     FF800000
**  NaN     7FF0000000000001     7F800001
**                to               to
**          7FFFFFFFFFFFFFFF     7FFFFFFF
**                and              and
**          FFF0000000000001     FF800001
**                to               to
**          FFFFFFFFFFFFFFFF     FFFFFFFF
** +OVER    7FEFFFFFFFFFFFFF     7F7FFFFF
** -OVER    FFEFFFFFFFFFFFFF     FF7FFFFF
** +UNDER   0010000000000000     00800000
** -UNDER   8010000000000000     80800000
*/

double
double64_be_read (unsigned char *cptr)
{	int		exponent, negative, upper, lower ;
	double	dvalue ;

	negative = (cptr [0] & 0x80) ? 1 : 0 ;
	exponent = ((cptr [0] & 0x7F) << 4) | ((cptr [1] >> 4) & 0xF) ;

	/* Might not have a 64 bit long, so load the mantissa into a double. */
	upper = (((cptr [1] & 0xF) << 24) | (cptr [2] << 16) | (cptr [3] << 8) | cptr [4]) ;
	lower = (cptr [5] << 16) | (cptr [6] << 8) | cptr [7] ;

	if (exponent == 0 && upper == 0 && lower == 0)
		return 0.0 ;

	dvalue = upper + lower / ((double) 0x1000000) ;
	dvalue += 0x10000000 ;

	exponent = exponent - 0x3FF ;

	dvalue = dvalue / ((double) 0x10000000) ;

	if (negative)
		dvalue *= -1 ;

	if (exponent > 0)
		dvalue *= pow (2.0, exponent) ;
	else if (exponent < 0)
		dvalue /= pow (2.0, abs (exponent)) ;

	return dvalue ;
} /* double64_be_read */

double
double64_le_read (unsigned char *cptr)
{	int		exponent, negative, upper, lower ;
	double	dvalue ;

	negative = (cptr [7] & 0x80) ? 1 : 0 ;
	exponent = ((cptr [7] & 0x7F) << 4) | ((cptr [6] >> 4) & 0xF) ;

	/* Might not have a 64 bit long, so load the mantissa into a double. */
	upper = ((cptr [6] & 0xF) << 24) | (cptr [5] << 16) | (cptr [4] << 8) | cptr [3] ;
	lower = (cptr [2] << 16) | (cptr [1] << 8) | cptr [0] ;

	if (exponent == 0 && upper == 0 && lower == 0)
		return 0.0 ;

	dvalue = upper + lower / ((double) 0x1000000) ;
	dvalue += 0x10000000 ;

	exponent = exponent - 0x3FF ;

	dvalue = dvalue / ((double) 0x10000000) ;

	if (negative)
		dvalue *= -1 ;

	if (exponent > 0)
		dvalue *= pow (2.0, exponent) ;
	else if (exponent < 0)
		dvalue /= pow (2.0, abs (exponent)) ;

	return dvalue ;
} /* double64_le_read */

void
double64_be_write (double in, unsigned char *out)
{	int		exponent, mantissa ;

	memset (out, 0, sizeof (double)) ;

	if (fabs (in) < 1e-30)
		return ;

	if (in < 0.0)
	{	in *= -1.0 ;
		out [0] |= 0x80 ;
		} ;

	in = frexp (in, &exponent) ;

	exponent += 1022 ;

	out [0] |= (exponent >> 4) & 0x7F ;
	out [1] |= (exponent << 4) & 0xF0 ;

	in *= 0x20000000 ;
	mantissa = lrint (floor (in)) ;

	out [1] |= (mantissa >> 24) & 0xF ;
	out [2] = (mantissa >> 16) & 0xFF ;
	out [3] = (mantissa >> 8) & 0xFF ;
	out [4] = mantissa & 0xFF ;

	in = fmod (in, 1.0) ;
	in *= 0x1000000 ;
	mantissa = lrint (floor (in)) ;

	out [5] = (mantissa >> 16) & 0xFF ;
	out [6] = (mantissa >> 8) & 0xFF ;
	out [7] = mantissa & 0xFF ;

	return ;
} /* double64_be_write */

void
double64_le_write (double in, unsigned char *out)
{	int		exponent, mantissa ;

	memset (out, 0, sizeof (double)) ;

	if (fabs (in) < 1e-30)
		return ;

	if (in < 0.0)
	{	in *= -1.0 ;
		out [7] |= 0x80 ;
		} ;

	in = frexp (in, &exponent) ;

	exponent += 1022 ;

	out [7] |= (exponent >> 4) & 0x7F ;
	out [6] |= (exponent << 4) & 0xF0 ;

	in *= 0x20000000 ;
	mantissa = lrint (floor (in)) ;

	out [6] |= (mantissa >> 24) & 0xF ;
	out [5] = (mantissa >> 16) & 0xFF ;
	out [4] = (mantissa >> 8) & 0xFF ;
	out [3] = mantissa & 0xFF ;

	in = fmod (in, 1.0) ;
	in *= 0x1000000 ;
	mantissa = lrint (floor (in)) ;

	out [2] = (mantissa >> 16) & 0xFF ;
	out [1] = (mantissa >> 8) & 0xFF ;
	out [0] = mantissa & 0xFF ;

	return ;
} /* double64_le_write */

/*==============================================================================================
**	Private functions.
*/

static void
double64_peak_update	(SF_PRIVATE *psf, const double *buffer, int count, sf_count_t indx)
{	int 	chan ;
	int		k, position ;
	float	fmaxval ;

	for (chan = 0 ; chan < psf->sf.channels ; chan++)
	{	fmaxval = fabs (buffer [chan]) ;
		position = 0 ;
		for (k = chan ; k < count ; k += psf->sf.channels)
			if (fmaxval < fabs (buffer [k]))
			{	fmaxval = fabs (buffer [k]) ;
				position = k ;
				} ;

		if (fmaxval > psf->peak_info->peaks [chan].value)
		{	psf->peak_info->peaks [chan].value = fmaxval ;
			psf->peak_info->peaks [chan].position = psf->write_current + indx + (position / psf->sf.channels) ;
			} ;
		} ;

	return ;
} /* double64_peak_update */

static int
double64_get_capability	(SF_PRIVATE *psf)
{	union
	{	double			d ;
		unsigned char	c [8] ;
	} data ;

	data.d = 1.234567890123456789 ; /* Some abitrary value. */

	if (! psf->ieee_replace)
	{	/* If this test is true ints and floats are compatible and little endian. */
		if (data.c [0] == 0xfb && data.c [1] == 0x59 && data.c [2] == 0x8c && data.c [3] == 0x42 &&
			data.c [4] == 0xca && data.c [5] == 0xc0 && data.c [6] == 0xf3 && data.c [7] == 0x3f)
			return DOUBLE_CAN_RW_LE ;

		/* If this test is true ints and floats are compatible and big endian. */
		if (data.c [0] == 0x3f && data.c [1] == 0xf3 && data.c [2] == 0xc0 && data.c [3] == 0xca &&
			data.c [4] == 0x42 && data.c [5] == 0x8c && data.c [6] == 0x59 && data.c [7] == 0xfb)
			return DOUBLE_CAN_RW_BE ;
		} ;

	/* Doubles are broken. Don't expect reading or writing to be fast. */
	psf_log_printf (psf, "Using IEEE replacement code for double.\n") ;

	return (CPU_IS_LITTLE_ENDIAN) ? DOUBLE_BROKEN_LE : DOUBLE_BROKEN_BE ;
} /* double64_get_capability */

/*=======================================================================================
*/

static void
d2s_array (const double *src, int count, short *dest, double scale)
{	while (--count >= 0)
	{	dest [count] = lrint (scale * src [count]) ;
		} ;
} /* d2s_array */

static void
d2s_clip_array (const double *src, int count, short *dest, double scale)
{	while (--count >= 0)
	{	double tmp = scale * src [count] ;

		if (CPU_CLIPS_POSITIVE == 0 && tmp > 32767.0)
			dest [count] = SHRT_MAX ;
		else if (CPU_CLIPS_NEGATIVE == 0 && tmp < -32768.0)
			dest [count] = SHRT_MIN ;
		else
			dest [count] = lrint (tmp) ;
		} ;
} /* d2s_clip_array */

static void
d2i_array (const double *src, int count, int *dest, double scale)
{	while (--count >= 0)
	{	dest [count] = lrint (scale * src [count]) ;
		} ;
} /* d2i_array */

static void
d2i_clip_array (const double *src, int count, int *dest, double scale)
{	while (--count >= 0)
	{	float tmp = scale * src [count] ;

		if (CPU_CLIPS_POSITIVE == 0 && tmp > (1.0 * INT_MAX))
			dest [count] = INT_MAX ;
		else if (CPU_CLIPS_NEGATIVE == 0 && tmp < (-1.0 * INT_MAX))
			dest [count] = INT_MIN ;
		else
			dest [count] = lrint (tmp) ;
		} ;
} /* d2i_clip_array */

static inline void
d2f_array (const double *src, int count, float *dest)
{	while (--count >= 0)
	{	dest [count] = src [count] ;
		} ;
} /* d2f_array */

static inline void
s2d_array (const short *src, double *dest, int count, double scale)
{	while (--count >= 0)
	{	dest [count] = scale * src [count] ;
		} ;
} /* s2d_array */

static inline void
i2d_array (const int *src, double *dest, int count, double scale)
{	while (--count >= 0)
	{	dest [count] = scale * src [count] ;
		} ;
} /* i2d_array */

static inline void
f2d_array (const float *src, double *dest, int count)
{	while (--count >= 0)
	{	dest [count] = src [count] ;
		} ;
} /* f2d_array */

/*----------------------------------------------------------------------------------------------
*/

static sf_count_t
host_read_d2s	(SF_PRIVATE *psf, short *ptr, sf_count_t len)
{	void		(*convert) (const double *, int, short *, double) ;
	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;
	double		scale ;

	convert = (psf->add_clipping) ? d2s_clip_array : d2s_array ;
	bufferlen = ARRAY_LEN (psf->u.dbuf) ;
	scale = (psf->float_int_mult == 0) ? 1.0 : 0x7FFF / psf->float_max ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.dbuf, sizeof (double), bufferlen, psf) ;

		if (psf->data_endswap == SF_TRUE)
			endswap_double_array (psf->u.dbuf, readcount) ;

		convert (psf->u.dbuf, readcount, ptr + total, scale) ;
		total += readcount ;
		len -= readcount ;
		if (readcount < bufferlen)
			break ;
		} ;

	return total ;
} /* host_read_d2s */

static sf_count_t
host_read_d2i	(SF_PRIVATE *psf, int *ptr, sf_count_t len)
{	void		(*convert) (const double *, int, int *, double) ;
	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;
	double		scale ;

	convert = (psf->add_clipping) ? d2i_clip_array : d2i_array ;
	bufferlen = ARRAY_LEN (psf->u.dbuf) ;
	scale = (psf->float_int_mult == 0) ? 1.0 : 0x7FFFFFFF / psf->float_max ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.dbuf, sizeof (double), bufferlen, psf) ;

		if (psf->data_endswap == SF_TRUE)
			endswap_double_array (psf->u.dbuf, bufferlen) ;

		convert (psf->u.dbuf, readcount, ptr + total, scale) ;
		total += readcount ;
		len -= readcount ;
		if (readcount < bufferlen)
			break ;
		} ;

	return total ;
} /* host_read_d2i */

static sf_count_t
host_read_d2f	(SF_PRIVATE *psf, float *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;

	bufferlen = ARRAY_LEN (psf->u.dbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.dbuf, sizeof (double), bufferlen, psf) ;

		if (psf->data_endswap == SF_TRUE)
			endswap_double_array (psf->u.dbuf, bufferlen) ;

		d2f_array (psf->u.dbuf, readcount, ptr + total) ;
		total += readcount ;
		len -= readcount ;
		if (readcount < bufferlen)
			break ;
		} ;

	return total ;
} /* host_read_d2f */

static sf_count_t
host_read_d	(SF_PRIVATE *psf, double *ptr, sf_count_t len)
{	int			bufferlen ;
	sf_count_t	readcount, total = 0 ;

	readcount = psf_fread (ptr, sizeof (double), len, psf) ;

	if (psf->data_endswap != SF_TRUE)
		return readcount ;

	/* If the read length was sensible, endswap output in one go. */
	if (readcount < SENSIBLE_LEN)
	{	endswap_double_array (ptr, readcount) ;
		return readcount ;
		} ;

	bufferlen = SENSIBLE_LEN ;
	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;

		endswap_double_array (ptr + total, bufferlen) ;

		total += bufferlen ;
		len -= bufferlen ;
		} ;

	return total ;
} /* host_read_d */

static sf_count_t
host_write_s2d	(SF_PRIVATE *psf, const short *ptr, sf_count_t len)
{	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;
	double		scale ;

	scale = (psf->scale_int_float == 0) ? 1.0 : 1.0 / 0x8000 ;
	bufferlen = ARRAY_LEN (psf->u.dbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;

		s2d_array (ptr + total, psf->u.dbuf, bufferlen, scale) ;

		if (psf->peak_info)
			double64_peak_update (psf, psf->u.dbuf, bufferlen, total / psf->sf.channels) ;

		if (psf->data_endswap == SF_TRUE)
			endswap_double_array (psf->u.dbuf, bufferlen) ;

		writecount = psf_fwrite (psf->u.dbuf, sizeof (double), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* host_write_s2d */

static sf_count_t
host_write_i2d	(SF_PRIVATE *psf, const int *ptr, sf_count_t len)
{	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;
	double		scale ;

	scale = (psf->scale_int_float == 0) ? 1.0 : 1.0 / (8.0 * 0x10000000) ;
	bufferlen = ARRAY_LEN (psf->u.dbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		i2d_array (ptr + total, psf->u.dbuf, bufferlen, scale) ;

		if (psf->peak_info)
			double64_peak_update (psf, psf->u.dbuf, bufferlen, total / psf->sf.channels) ;

		if (psf->data_endswap == SF_TRUE)
			endswap_double_array (psf->u.dbuf, bufferlen) ;

		writecount = psf_fwrite (psf->u.dbuf, sizeof (double), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* host_write_i2d */

static sf_count_t
host_write_f2d	(SF_PRIVATE *psf, const float *ptr, sf_count_t len)
{	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	bufferlen = ARRAY_LEN (psf->u.dbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		f2d_array (ptr + total, psf->u.dbuf, bufferlen) ;

		if (psf->peak_info)
			double64_peak_update (psf, psf->u.dbuf, bufferlen, total / psf->sf.channels) ;

		if (psf->data_endswap == SF_TRUE)
			endswap_double_array (psf->u.dbuf, bufferlen) ;

		writecount = psf_fwrite (psf->u.dbuf, sizeof (double), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* host_write_f2d */

static sf_count_t
host_write_d	(SF_PRIVATE *psf, const double *ptr, sf_count_t len)
{	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	if (psf->peak_info)
		double64_peak_update (psf, ptr, len, 0) ;

	if (psf->data_endswap != SF_TRUE)
		return psf_fwrite (ptr, sizeof (double), len, psf) ;

	bufferlen = ARRAY_LEN (psf->u.dbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;

		endswap_double_copy (psf->u.dbuf, ptr + total, bufferlen) ;

		writecount = psf_fwrite (psf->u.dbuf, sizeof (double), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* host_write_d */

/*=======================================================================================
*/

static sf_count_t
replace_read_d2s	(SF_PRIVATE *psf, short *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;
	double		scale ;

	bufferlen = ARRAY_LEN (psf->u.dbuf) ;
	scale = (psf->float_int_mult == 0) ? 1.0 : 0x7FFF / psf->float_max ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.dbuf, sizeof (double), bufferlen, psf) ;

		if (psf->data_endswap == SF_TRUE)
			endswap_double_array (psf->u.dbuf, bufferlen) ;

		d2bd_read (psf->u.dbuf, bufferlen) ;

		d2s_array (psf->u.dbuf, readcount, ptr + total, scale) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* replace_read_d2s */

static sf_count_t
replace_read_d2i	(SF_PRIVATE *psf, int *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;
	double		scale ;

	bufferlen = ARRAY_LEN (psf->u.dbuf) ;
	scale = (psf->float_int_mult == 0) ? 1.0 : 0x7FFFFFFF / psf->float_max ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.dbuf, sizeof (double), bufferlen, psf) ;

		if (psf->data_endswap == SF_TRUE)
			endswap_double_array (psf->u.dbuf, bufferlen) ;

		d2bd_read (psf->u.dbuf, bufferlen) ;

		d2i_array (psf->u.dbuf, readcount, ptr + total, scale) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* replace_read_d2i */

static sf_count_t
replace_read_d2f	(SF_PRIVATE *psf, float *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;

	bufferlen = ARRAY_LEN (psf->u.dbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.dbuf, sizeof (double), bufferlen, psf) ;

		if (psf->data_endswap == SF_TRUE)
			endswap_double_array (psf->u.dbuf, bufferlen) ;

		d2bd_read (psf->u.dbuf, bufferlen) ;

		memcpy (ptr + total, psf->u.dbuf, bufferlen * sizeof (double)) ;

		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* replace_read_d2f */

static sf_count_t
replace_read_d	(SF_PRIVATE *psf, double *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;

	/* FIXME : This is probably nowhere near optimal. */
	bufferlen = ARRAY_LEN (psf->u.dbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.dbuf, sizeof (double), bufferlen, psf) ;

		if (psf->data_endswap == SF_TRUE)
			endswap_double_array (psf->u.dbuf, readcount) ;

		d2bd_read (psf->u.dbuf, readcount) ;

		memcpy (ptr + total, psf->u.dbuf, readcount * sizeof (double)) ;

		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* replace_read_d */

static sf_count_t
replace_write_s2d	(SF_PRIVATE *psf, const short *ptr, sf_count_t len)
{	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;
	double		scale ;

	scale = (psf->scale_int_float == 0) ? 1.0 : 1.0 / 0x8000 ;
	bufferlen = ARRAY_LEN (psf->u.dbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		s2d_array (ptr + total, psf->u.dbuf, bufferlen, scale) ;

		if (psf->peak_info)
			double64_peak_update (psf, psf->u.dbuf, bufferlen, total / psf->sf.channels) ;

		bd2d_write (psf->u.dbuf, bufferlen) ;

		if (psf->data_endswap == SF_TRUE)
			endswap_double_array (psf->u.dbuf, bufferlen) ;

		writecount = psf_fwrite (psf->u.dbuf, sizeof (double), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* replace_write_s2d */

static sf_count_t
replace_write_i2d	(SF_PRIVATE *psf, const int *ptr, sf_count_t len)
{	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;
	double		scale ;

	scale = (psf->scale_int_float == 0) ? 1.0 : 1.0 / (8.0 * 0x10000000) ;
	bufferlen = ARRAY_LEN (psf->u.dbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		i2d_array (ptr + total, psf->u.dbuf, bufferlen, scale) ;

		if (psf->peak_info)
			double64_peak_update (psf, psf->u.dbuf, bufferlen, total / psf->sf.channels) ;

		bd2d_write (psf->u.dbuf, bufferlen) ;

		if (psf->data_endswap == SF_TRUE)
			endswap_double_array (psf->u.dbuf, bufferlen) ;

		writecount = psf_fwrite (psf->u.dbuf, sizeof (double), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* replace_write_i2d */

static sf_count_t
replace_write_f2d	(SF_PRIVATE *psf, const float *ptr, sf_count_t len)
{	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	bufferlen = ARRAY_LEN (psf->u.dbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		f2d_array (ptr + total, psf->u.dbuf, bufferlen) ;

		bd2d_write (psf->u.dbuf, bufferlen) ;

		if (psf->data_endswap == SF_TRUE)
			endswap_double_array (psf->u.dbuf, bufferlen) ;

		writecount = psf_fwrite (psf->u.dbuf, sizeof (double), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* replace_write_f2d */

static sf_count_t
replace_write_d	(SF_PRIVATE *psf, const double *ptr, sf_count_t len)
{	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	/* FIXME : This is probably nowhere near optimal. */
	if (psf->peak_info)
		double64_peak_update (psf, ptr, len, 0) ;

	bufferlen = ARRAY_LEN (psf->u.dbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;

		memcpy (psf->u.dbuf, ptr + total, bufferlen * sizeof (double)) ;

		bd2d_write (psf->u.dbuf, bufferlen) ;

		if (psf->data_endswap == SF_TRUE)
			endswap_double_array (psf->u.dbuf, bufferlen) ;

		writecount = psf_fwrite (psf->u.dbuf, sizeof (double), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* replace_write_d */

/*----------------------------------------------------------------------------------------------
*/

static void
d2bd_read (double *buffer, int count)
{	while (--count >= 0)
	{	buffer [count] = DOUBLE64_READ ((unsigned char *) (buffer + count)) ;
		} ;
} /* d2bd_read */

static void
bd2d_write (double *buffer, int count)
{	while (--count >= 0)
	{	DOUBLE64_WRITE (buffer [count], (unsigned char*) (buffer + count)) ;
		} ;
} /* bd2d_write */

