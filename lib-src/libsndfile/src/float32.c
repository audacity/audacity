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
	#define FLOAT32_READ	float32_le_read
	#define FLOAT32_WRITE	float32_le_write
#elif CPU_IS_BIG_ENDIAN
	#define FLOAT32_READ	float32_be_read
	#define FLOAT32_WRITE	float32_be_write
#endif

/*--------------------------------------------------------------------------------------------
**	Processor floating point capabilities. float32_get_capability () returns one of the
**	latter four values.
*/

enum
{	FLOAT_UNKNOWN		= 0x00,
	FLOAT_CAN_RW_LE		= 0x12,
	FLOAT_CAN_RW_BE		= 0x23,
	FLOAT_BROKEN_LE		= 0x34,
	FLOAT_BROKEN_BE		= 0x45
} ;

/*--------------------------------------------------------------------------------------------
**	Prototypes for private functions.
*/

static sf_count_t	host_read_f2s	(SF_PRIVATE *psf, short *ptr, sf_count_t len) ;
static sf_count_t	host_read_f2i	(SF_PRIVATE *psf, int *ptr, sf_count_t len) ;
static sf_count_t	host_read_f	(SF_PRIVATE *psf, float *ptr, sf_count_t len) ;
static sf_count_t	host_read_f2d	(SF_PRIVATE *psf, double *ptr, sf_count_t len) ;

static sf_count_t	host_write_s2f	(SF_PRIVATE *psf, const short *ptr, sf_count_t len) ;
static sf_count_t	host_write_i2f	(SF_PRIVATE *psf, const int *ptr, sf_count_t len) ;
static sf_count_t	host_write_f	(SF_PRIVATE *psf, const float *ptr, sf_count_t len) ;
static sf_count_t	host_write_d2f	(SF_PRIVATE *psf, const double *ptr, sf_count_t len) ;

static void		float32_peak_update	(SF_PRIVATE *psf, const float *buffer, int count, sf_count_t indx) ;

static sf_count_t	replace_read_f2s	(SF_PRIVATE *psf, short *ptr, sf_count_t len) ;
static sf_count_t	replace_read_f2i	(SF_PRIVATE *psf, int *ptr, sf_count_t len) ;
static sf_count_t	replace_read_f	(SF_PRIVATE *psf, float *ptr, sf_count_t len) ;
static sf_count_t	replace_read_f2d	(SF_PRIVATE *psf, double *ptr, sf_count_t len) ;

static sf_count_t	replace_write_s2f	(SF_PRIVATE *psf, const short *ptr, sf_count_t len) ;
static sf_count_t	replace_write_i2f	(SF_PRIVATE *psf, const int *ptr, sf_count_t len) ;
static sf_count_t	replace_write_f	(SF_PRIVATE *psf, const float *ptr, sf_count_t len) ;
static sf_count_t	replace_write_d2f	(SF_PRIVATE *psf, const double *ptr, sf_count_t len) ;

static	void	bf2f_array (float *buffer, int count) ;
static	void	f2bf_array (float *buffer, int count) ;

static int		float32_get_capability	(SF_PRIVATE *psf) ;

/*--------------------------------------------------------------------------------------------
**	Exported functions.
*/

int
float32_init	(SF_PRIVATE *psf)
{	static int float_caps ;

	float_caps = float32_get_capability (psf) ;

	psf->blockwidth = sizeof (float) * psf->sf.channels ;

	if (psf->mode == SFM_READ || psf->mode == SFM_RDWR)
	{	switch (psf->endian + float_caps)
		{	case (SF_ENDIAN_BIG + FLOAT_CAN_RW_BE) :
					psf->data_endswap = SF_FALSE ;
					psf->read_short		= host_read_f2s ;
					psf->read_int		= host_read_f2i ;
					psf->read_float		= host_read_f ;
					psf->read_double	= host_read_f2d ;
					break ;

			case (SF_ENDIAN_LITTLE + FLOAT_CAN_RW_LE) :
					psf->data_endswap = SF_FALSE ;
					psf->read_short		= host_read_f2s ;
					psf->read_int		= host_read_f2i ;
					psf->read_float		= host_read_f ;
					psf->read_double	= host_read_f2d ;
					break ;

			case (SF_ENDIAN_BIG + FLOAT_CAN_RW_LE) :
					psf->data_endswap = SF_TRUE ;
					psf->read_short		= host_read_f2s ;
					psf->read_int		= host_read_f2i ;
					psf->read_float		= host_read_f ;
					psf->read_double	= host_read_f2d ;
					break ;

			case (SF_ENDIAN_LITTLE + FLOAT_CAN_RW_BE) :
					psf->data_endswap = SF_TRUE ;
					psf->read_short		= host_read_f2s ;
					psf->read_int		= host_read_f2i ;
					psf->read_float		= host_read_f ;
					psf->read_double	= host_read_f2d ;
					break ;

			/* When the CPU is not IEEE compatible. */
			case (SF_ENDIAN_BIG + FLOAT_BROKEN_LE) :
					psf->data_endswap = SF_TRUE ;
					psf->read_short		= replace_read_f2s ;
					psf->read_int		= replace_read_f2i ;
					psf->read_float		= replace_read_f ;
					psf->read_double	= replace_read_f2d ;
					break ;

			case (SF_ENDIAN_LITTLE + FLOAT_BROKEN_LE) :
					psf->data_endswap = SF_FALSE ;
					psf->read_short		= replace_read_f2s ;
					psf->read_int		= replace_read_f2i ;
					psf->read_float		= replace_read_f ;
					psf->read_double	= replace_read_f2d ;
					break ;

			case (SF_ENDIAN_BIG + FLOAT_BROKEN_BE) :
					psf->data_endswap = SF_FALSE ;
					psf->read_short		= replace_read_f2s ;
					psf->read_int		= replace_read_f2i ;
					psf->read_float		= replace_read_f ;
					psf->read_double	= replace_read_f2d ;
					break ;

			case (SF_ENDIAN_LITTLE + FLOAT_BROKEN_BE) :
					psf->data_endswap = SF_TRUE ;
					psf->read_short		= replace_read_f2s ;
					psf->read_int		= replace_read_f2i ;
					psf->read_float		= replace_read_f ;
					psf->read_double	= replace_read_f2d ;
					break ;

			default : break ;
			} ;
		} ;

	if (psf->mode == SFM_WRITE || psf->mode == SFM_RDWR)
	{	switch (psf->endian + float_caps)
		{	case (SF_ENDIAN_LITTLE + FLOAT_CAN_RW_LE) :
					psf->data_endswap = SF_FALSE ;
					psf->write_short	= host_write_s2f ;
					psf->write_int		= host_write_i2f ;
					psf->write_float	= host_write_f ;
					psf->write_double	= host_write_d2f ;
					break ;

			case (SF_ENDIAN_BIG + FLOAT_CAN_RW_BE) :
					psf->data_endswap = SF_FALSE ;
					psf->write_short	= host_write_s2f ;
					psf->write_int		= host_write_i2f ;
					psf->write_float	= host_write_f ;
					psf->write_double	= host_write_d2f ;
					break ;

			case (SF_ENDIAN_BIG + FLOAT_CAN_RW_LE) :
					psf->data_endswap = SF_TRUE ;
					psf->write_short	= host_write_s2f ;
					psf->write_int		= host_write_i2f ;
					psf->write_float	= host_write_f ;
					psf->write_double	= host_write_d2f ;
					break ;

			case (SF_ENDIAN_LITTLE + FLOAT_CAN_RW_BE) :
					psf->data_endswap = SF_TRUE ;
					psf->write_short	= host_write_s2f ;
					psf->write_int		= host_write_i2f ;
					psf->write_float	= host_write_f ;
					psf->write_double	= host_write_d2f ;
					break ;

			/* When the CPU is not IEEE compatible. */
			case (SF_ENDIAN_BIG + FLOAT_BROKEN_LE) :
					psf->data_endswap = SF_TRUE ;
					psf->write_short	= replace_write_s2f ;
					psf->write_int		= replace_write_i2f ;
					psf->write_float	= replace_write_f ;
					psf->write_double	= replace_write_d2f ;
					break ;

			case (SF_ENDIAN_LITTLE + FLOAT_BROKEN_LE) :
					psf->data_endswap = SF_FALSE ;
					psf->write_short	= replace_write_s2f ;
					psf->write_int		= replace_write_i2f ;
					psf->write_float	= replace_write_f ;
					psf->write_double	= replace_write_d2f ;
					break ;

			case (SF_ENDIAN_BIG + FLOAT_BROKEN_BE) :
					psf->data_endswap = SF_FALSE ;
					psf->write_short	= replace_write_s2f ;
					psf->write_int		= replace_write_i2f ;
					psf->write_float	= replace_write_f ;
					psf->write_double	= replace_write_d2f ;
					break ;

			case (SF_ENDIAN_LITTLE + FLOAT_BROKEN_BE) :
					psf->data_endswap = SF_TRUE ;
					psf->write_short	= replace_write_s2f ;
					psf->write_int		= replace_write_i2f ;
					psf->write_float	= replace_write_f ;
					psf->write_double	= replace_write_d2f ;
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
} /* float32_init */

float
float32_be_read (unsigned char *cptr)
{	int		exponent, mantissa, negative ;
	float	fvalue ;

	negative = cptr [0] & 0x80 ;
	exponent = ((cptr [0] & 0x7F) << 1) | ((cptr [1] & 0x80) ? 1 : 0) ;
	mantissa = ((cptr [1] & 0x7F) << 16) | (cptr [2] << 8) | (cptr [3]) ;

	if (! (exponent || mantissa))
		return 0.0 ;

	mantissa |= 0x800000 ;
	exponent = exponent ? exponent - 127 : 0 ;

	fvalue = mantissa ? ((float) mantissa) / ((float) 0x800000) : 0.0 ;

	if (negative)
		fvalue *= -1 ;

	if (exponent > 0)
		fvalue *= pow (2.0, exponent) ;
	else if (exponent < 0)
		fvalue /= pow (2.0, abs (exponent)) ;

	return fvalue ;
} /* float32_be_read */

float
float32_le_read (unsigned char *cptr)
{	int		exponent, mantissa, negative ;
	float	fvalue ;

	negative = cptr [3] & 0x80 ;
	exponent = ((cptr [3] & 0x7F) << 1) | ((cptr [2] & 0x80) ? 1 : 0) ;
	mantissa = ((cptr [2] & 0x7F) << 16) | (cptr [1] << 8) | (cptr [0]) ;

	if (! (exponent || mantissa))
		return 0.0 ;

	mantissa |= 0x800000 ;
	exponent = exponent ? exponent - 127 : 0 ;

	fvalue = mantissa ? ((float) mantissa) / ((float) 0x800000) : 0.0 ;

	if (negative)
		fvalue *= -1 ;

	if (exponent > 0)
		fvalue *= pow (2.0, exponent) ;
	else if (exponent < 0)
		fvalue /= pow (2.0, abs (exponent)) ;

	return fvalue ;
} /* float32_le_read */

void
float32_le_write (float in, unsigned char *out)
{	int		exponent, mantissa, negative = 0 ;

	memset (out, 0, sizeof (int)) ;

	if (fabs (in) < 1e-30)
		return ;

	if (in < 0.0)
	{	in *= -1.0 ;
		negative = 1 ;
		} ;

	in = frexp (in, &exponent) ;

	exponent += 126 ;

	in *= (float) 0x1000000 ;
	mantissa = (((int) in) & 0x7FFFFF) ;

	if (negative)
		out [3] |= 0x80 ;

	if (exponent & 0x01)
		out [2] |= 0x80 ;

	out [0] = mantissa & 0xFF ;
	out [1] = (mantissa >> 8) & 0xFF ;
	out [2] |= (mantissa >> 16) & 0x7F ;
	out [3] |= (exponent >> 1) & 0x7F ;

	return ;
} /* float32_le_write */

void
float32_be_write (float in, unsigned char *out)
{	int		exponent, mantissa, negative = 0 ;

	memset (out, 0, sizeof (int)) ;

	if (fabs (in) < 1e-30)
		return ;

	if (in < 0.0)
	{	in *= -1.0 ;
		negative = 1 ;
		} ;

	in = frexp (in, &exponent) ;

	exponent += 126 ;

	in *= (float) 0x1000000 ;
	mantissa = (((int) in) & 0x7FFFFF) ;

	if (negative)
		out [0] |= 0x80 ;

	if (exponent & 0x01)
		out [1] |= 0x80 ;

	out [3] = mantissa & 0xFF ;
	out [2] = (mantissa >> 8) & 0xFF ;
	out [1] |= (mantissa >> 16) & 0x7F ;
	out [0] |= (exponent >> 1) & 0x7F ;

	return ;
} /* float32_be_write */

/*==============================================================================================
**	Private functions.
*/

static void
float32_peak_update	(SF_PRIVATE *psf, const float *buffer, int count, sf_count_t indx)
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
} /* float32_peak_update */

static int
float32_get_capability	(SF_PRIVATE *psf)
{	union
	{	float			f ;
		int				i ;
		unsigned char	c [4] ;
	} data ;

	data.f = (float) 1.23456789 ; /* Some abitrary value. */

	if (! psf->ieee_replace)
	{	/* If this test is true ints and floats are compatible and little endian. */
		if (data.c [0] == 0x52 && data.c [1] == 0x06 && data.c [2] == 0x9e && data.c [3] == 0x3f)
			return FLOAT_CAN_RW_LE ;

		/* If this test is true ints and floats are compatible and big endian. */
		if (data.c [3] == 0x52 && data.c [2] == 0x06 && data.c [1] == 0x9e && data.c [0] == 0x3f)
			return FLOAT_CAN_RW_BE ;
		} ;

	/* Floats are broken. Don't expect reading or writing to be fast. */
	psf_log_printf (psf, "Using IEEE replacement code for float.\n") ;

	return (CPU_IS_LITTLE_ENDIAN) ? FLOAT_BROKEN_LE : FLOAT_BROKEN_BE ;
} /* float32_get_capability */

/*=======================================================================================
*/

static void
f2s_array (const float *src, int count, short *dest, float scale)
{
	while (--count >= 0)
	{	dest [count] = lrintf (scale * src [count]) ;
		} ;
} /* f2s_array */

static void
f2s_clip_array (const float *src, int count, short *dest, float scale)
{	while (--count >= 0)
	{	float tmp = scale * src [count] ;

		if (CPU_CLIPS_POSITIVE == 0 && tmp > 32767.0)
			dest [count] = SHRT_MAX ;
		else if (CPU_CLIPS_NEGATIVE == 0 && tmp < 32768.0)
			dest [count] = SHRT_MIN ;
		else
			dest [count] = lrintf (tmp) ;
		} ;
} /* f2s_clip_array */

static inline void
f2i_array (const float *src, int count, int *dest, float scale)
{	while (--count >= 0)
	{	dest [count] = lrintf (scale * src [count]) ;
		} ;
} /* f2i_array */

static inline void
f2i_clip_array (const float *src, int count, int *dest, float scale)
{	while (--count >= 0)
	{	float tmp = scale * src [count] ;

		if (CPU_CLIPS_POSITIVE == 0 && tmp > (1.0 * INT_MAX))
			dest [count] = INT_MAX ;
		else if (CPU_CLIPS_NEGATIVE == 0 && tmp < (-1.0 * INT_MAX))
			dest [count] = INT_MIN ;
		else
			dest [count] = lrintf (tmp) ;
		} ;
} /* f2i_clip_array */

static inline void
f2d_array (const float *src, int count, double *dest)
{	while (--count >= 0)
	{	dest [count] = src [count] ;
		} ;
} /* f2d_array */

static inline void
s2f_array (const short *src, float *dest, int count, float scale)
{	while (--count >= 0)
	{	dest [count] = scale * src [count] ;
		} ;
} /* s2f_array */

static inline void
i2f_array (const int *src, float *dest, int count, float scale)
{	while (--count >= 0)
	{	dest [count] = scale * src [count] ;
		} ;
} /* i2f_array */

static inline void
d2f_array (const double *src, float *dest, int count)
{	while (--count >= 0)
	{	dest [count] = src [count] ;
		} ;
} /* d2f_array */

/*----------------------------------------------------------------------------------------------
*/

static sf_count_t
host_read_f2s	(SF_PRIVATE *psf, short *ptr, sf_count_t len)
{	void		(*convert) (const float *, int, short *, float) ;
	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;
	float		scale ;

	convert = (psf->add_clipping) ? f2s_clip_array : f2s_array ;
	bufferlen = ARRAY_LEN (psf->u.fbuf) ;
	scale = (psf->float_int_mult == 0) ? 1.0 : 0x7FFF / psf->float_max ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.fbuf, sizeof (float), bufferlen, psf) ;

/* Fix me : Need lef2s_array */
		if (psf->data_endswap == SF_TRUE)
			endswap_int_array (psf->u.ibuf, bufferlen) ;

		f2s_array (psf->u.fbuf, readcount, ptr + total, scale) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* host_read_f2s */

static sf_count_t
host_read_f2i	(SF_PRIVATE *psf, int *ptr, sf_count_t len)
{	void		(*convert) (const float *, int, int *, float) ;
	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;
	float		scale ;

	convert = (psf->add_clipping) ? f2i_clip_array : f2i_array ;
	bufferlen = ARRAY_LEN (psf->u.fbuf) ;
	scale = (psf->float_int_mult == 0) ? 1.0 : 0x7FFFFFFF / psf->float_max ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.fbuf, sizeof (float), bufferlen, psf) ;

		if (psf->data_endswap == SF_TRUE)
			endswap_int_array (psf->u.ibuf, bufferlen) ;

		convert (psf->u.fbuf, readcount, ptr + total, scale) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* host_read_f2i */

static sf_count_t
host_read_f	(SF_PRIVATE *psf, float *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;

	if (psf->data_endswap != SF_TRUE)
		return psf_fread (ptr, sizeof (float), len, psf) ;

	bufferlen = ARRAY_LEN (psf->u.fbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.fbuf, sizeof (float), bufferlen, psf) ;

		endswap_int_copy ((int*) (ptr + total), psf->u.ibuf, readcount) ;

		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* host_read_f */

static sf_count_t
host_read_f2d	(SF_PRIVATE *psf, double *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;

	bufferlen = ARRAY_LEN (psf->u.fbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.fbuf, sizeof (float), bufferlen, psf) ;

		if (psf->data_endswap == SF_TRUE)
			endswap_int_array (psf->u.ibuf, bufferlen) ;

/* Fix me : Need lef2d_array */
		f2d_array (psf->u.fbuf, readcount, ptr + total) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* host_read_f2d */

static sf_count_t
host_write_s2f	(SF_PRIVATE *psf, const short *ptr, sf_count_t len)
{	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;
	float		scale ;

/* Erik */
	scale = (psf->scale_int_float == 0) ? 1.0 : 1.0 / 0x8000 ;
	bufferlen = ARRAY_LEN (psf->u.fbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		s2f_array (ptr + total, psf->u.fbuf, bufferlen, scale) ;

		if (psf->peak_info)
			float32_peak_update (psf, psf->u.fbuf, bufferlen, total / psf->sf.channels) ;

		if (psf->data_endswap == SF_TRUE)
			endswap_int_array (psf->u.ibuf, bufferlen) ;

		writecount = psf_fwrite (psf->u.fbuf, sizeof (float), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* host_write_s2f */

static sf_count_t
host_write_i2f	(SF_PRIVATE *psf, const int *ptr, sf_count_t len)
{	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;
	float		scale ;

	scale = (psf->scale_int_float == 0) ? 1.0 : 1.0 / (8.0 * 0x10000000) ;
	bufferlen = ARRAY_LEN (psf->u.fbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		i2f_array (ptr + total, psf->u.fbuf, bufferlen, scale) ;

		if (psf->peak_info)
			float32_peak_update (psf, psf->u.fbuf, bufferlen, total / psf->sf.channels) ;

		if (psf->data_endswap == SF_TRUE)
			endswap_int_array (psf->u.ibuf, bufferlen) ;

		writecount = psf_fwrite (psf->u.fbuf, sizeof (float) , bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* host_write_i2f */

static sf_count_t
host_write_f	(SF_PRIVATE *psf, const float *ptr, sf_count_t len)
{	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	if (psf->peak_info)
		float32_peak_update (psf, ptr, len, 0) ;

	if (psf->data_endswap != SF_TRUE)
		return psf_fwrite (ptr, sizeof (float), len, psf) ;

	bufferlen = ARRAY_LEN (psf->u.fbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;

		endswap_int_copy (psf->u.ibuf, (const int*) (ptr + total), bufferlen) ;

		writecount = psf_fwrite (psf->u.fbuf, sizeof (float), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* host_write_f */

static sf_count_t
host_write_d2f	(SF_PRIVATE *psf, const double *ptr, sf_count_t len)
{	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	bufferlen = ARRAY_LEN (psf->u.fbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;

		d2f_array (ptr + total, psf->u.fbuf, bufferlen) ;

		if (psf->peak_info)
			float32_peak_update (psf, psf->u.fbuf, bufferlen, total / psf->sf.channels) ;

		if (psf->data_endswap == SF_TRUE)
			endswap_int_array (psf->u.ibuf, bufferlen) ;

		writecount = psf_fwrite (psf->u.fbuf, sizeof (float), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* host_write_d2f */

/*=======================================================================================
*/

static sf_count_t
replace_read_f2s	(SF_PRIVATE *psf, short *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;
	float		scale ;

	bufferlen = ARRAY_LEN (psf->u.fbuf) ;
	scale = (psf->float_int_mult == 0) ? 1.0 : 0x7FFF / psf->float_max ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.fbuf, sizeof (float), bufferlen, psf) ;

		if (psf->data_endswap == SF_TRUE)
			endswap_int_array (psf->u.ibuf, bufferlen) ;

		bf2f_array (psf->u.fbuf, bufferlen) ;

		f2s_array (psf->u.fbuf, readcount, ptr + total, scale) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* replace_read_f2s */

static sf_count_t
replace_read_f2i	(SF_PRIVATE *psf, int *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;
	float		scale ;

	bufferlen = ARRAY_LEN (psf->u.fbuf) ;
	scale = (psf->float_int_mult == 0) ? 1.0 : 0x7FFF / psf->float_max ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.fbuf, sizeof (float), bufferlen, psf) ;

		if (psf->data_endswap == SF_TRUE)
			endswap_int_array (psf->u.ibuf, bufferlen) ;

		bf2f_array (psf->u.fbuf, bufferlen) ;

		f2i_array (psf->u.fbuf, readcount, ptr + total, scale) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* replace_read_f2i */

static sf_count_t
replace_read_f	(SF_PRIVATE *psf, float *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;

	/* FIX THIS */

	bufferlen = ARRAY_LEN (psf->u.fbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.fbuf, sizeof (float), bufferlen, psf) ;

		if (psf->data_endswap == SF_TRUE)
			endswap_int_array (psf->u.ibuf, bufferlen) ;

		bf2f_array (psf->u.fbuf, bufferlen) ;

		memcpy (ptr + total, psf->u.fbuf, bufferlen * sizeof (float)) ;

		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* replace_read_f */

static sf_count_t
replace_read_f2d	(SF_PRIVATE *psf, double *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;

	bufferlen = ARRAY_LEN (psf->u.fbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.fbuf, sizeof (float), bufferlen, psf) ;

		if (psf->data_endswap == SF_TRUE)
			endswap_int_array (psf->u.ibuf, bufferlen) ;

		bf2f_array (psf->u.fbuf, bufferlen) ;

		f2d_array (psf->u.fbuf, readcount, ptr + total) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* replace_read_f2d */

static sf_count_t
replace_write_s2f	(SF_PRIVATE *psf, const short *ptr, sf_count_t len)
{	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;
	float		scale ;

	scale = (psf->scale_int_float == 0) ? 1.0 : 1.0 / 0x8000 ;
	bufferlen = ARRAY_LEN (psf->u.fbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		s2f_array (ptr + total, psf->u.fbuf, bufferlen, scale) ;

		if (psf->peak_info)
			float32_peak_update (psf, psf->u.fbuf, bufferlen, total / psf->sf.channels) ;

		f2bf_array (psf->u.fbuf, bufferlen) ;

		if (psf->data_endswap == SF_TRUE)
			endswap_int_array (psf->u.ibuf, bufferlen) ;

		writecount = psf_fwrite (psf->u.fbuf, sizeof (float), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* replace_write_s2f */

static sf_count_t
replace_write_i2f	(SF_PRIVATE *psf, const int *ptr, sf_count_t len)
{	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;
	float		scale ;

	scale = (psf->scale_int_float == 0) ? 1.0 : 1.0 / (8.0 * 0x10000000) ;
	bufferlen = ARRAY_LEN (psf->u.fbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		i2f_array (ptr + total, psf->u.fbuf, bufferlen, scale) ;

		if (psf->peak_info)
			float32_peak_update (psf, psf->u.fbuf, bufferlen, total / psf->sf.channels) ;

		f2bf_array (psf->u.fbuf, bufferlen) ;

		if (psf->data_endswap == SF_TRUE)
			endswap_int_array (psf->u.ibuf, bufferlen) ;

		writecount = psf_fwrite (psf->u.fbuf, sizeof (float), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* replace_write_i2f */

static sf_count_t
replace_write_f	(SF_PRIVATE *psf, const float *ptr, sf_count_t len)
{	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	/* FIX THIS */
	if (psf->peak_info)
		float32_peak_update (psf, ptr, len, 0) ;

	bufferlen = ARRAY_LEN (psf->u.fbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;

		memcpy (psf->u.fbuf, ptr + total, bufferlen * sizeof (float)) ;

		f2bf_array (psf->u.fbuf, bufferlen) ;

		if (psf->data_endswap == SF_TRUE)
			endswap_int_array (psf->u.ibuf, bufferlen) ;

		writecount = psf_fwrite (psf->u.fbuf, sizeof (float) , bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* replace_write_f */

static sf_count_t
replace_write_d2f	(SF_PRIVATE *psf, const double *ptr, sf_count_t len)
{	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	bufferlen = ARRAY_LEN (psf->u.fbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		d2f_array (ptr + total, psf->u.fbuf, bufferlen) ;

		if (psf->peak_info)
			float32_peak_update (psf, psf->u.fbuf, bufferlen, total / psf->sf.channels) ;

		f2bf_array (psf->u.fbuf, bufferlen) ;

		if (psf->data_endswap == SF_TRUE)
			endswap_int_array (psf->u.ibuf, bufferlen) ;

		writecount = psf_fwrite (psf->u.fbuf, sizeof (float), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* replace_write_d2f */

/*----------------------------------------------------------------------------------------------
*/

static void
bf2f_array (float *buffer, int count)
{	while (--count >= 0)
	{	buffer [count] = FLOAT32_READ ((unsigned char *) (buffer + count)) ;
		} ;
} /* bf2f_array */

static void
f2bf_array (float *buffer, int count)
{	while (--count >= 0)
	{	FLOAT32_WRITE (buffer [count], (unsigned char*) (buffer + count)) ;
		} ;
} /* f2bf_array */

