/*
** Copyright (C) 2002-2014 Erik de Castro Lopo <erikd@mega-nerd.com>
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

/*===========================================================================
** Delta Word Variable Width
**
** This decoder and encoder were implemented using information found in this
** document : http://home.swbell.net/rubywand/R011SNDFMTS.TXT
**
** According to the document, the algorithm "was invented 1991 by Magnus
** Lidstrom and is copyright 1993 by NuEdge Development".
*/

#include	"sfconfig.h"

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<math.h>

#include	"sndfile.h"
#include	"sfendian.h"
#include	"common.h"

typedef struct
{	int		bit_width, dwm_maxsize, max_delta, span ;
	int		samplecount ;
	int		bit_count, bits, last_delta_width, last_sample ;
	struct
	{	int				index, end ;
		unsigned char	buffer [256] ;
	} b ;
} DWVW_PRIVATE ;

/*============================================================================================
*/

static sf_count_t dwvw_read_s (SF_PRIVATE *psf, short *ptr, sf_count_t len) ;
static sf_count_t dwvw_read_i (SF_PRIVATE *psf, int *ptr, sf_count_t len) ;
static sf_count_t dwvw_read_f (SF_PRIVATE *psf, float *ptr, sf_count_t len) ;
static sf_count_t dwvw_read_d (SF_PRIVATE *psf, double *ptr, sf_count_t len) ;

static sf_count_t dwvw_write_s (SF_PRIVATE *psf, const short *ptr, sf_count_t len) ;
static sf_count_t dwvw_write_i (SF_PRIVATE *psf, const int *ptr, sf_count_t len) ;
static sf_count_t dwvw_write_f (SF_PRIVATE *psf, const float *ptr, sf_count_t len) ;
static sf_count_t dwvw_write_d (SF_PRIVATE *psf, const double *ptr, sf_count_t len) ;

static sf_count_t	dwvw_seek	(SF_PRIVATE *psf, int mode, sf_count_t offset) ;
static int	dwvw_close		(SF_PRIVATE *psf) ;
static int	dwvw_byterate	(SF_PRIVATE *psf) ;

static int	dwvw_decode_data (SF_PRIVATE *psf, DWVW_PRIVATE *pdwvw, int *ptr, int len) ;
static int	dwvw_decode_load_bits (SF_PRIVATE *psf, DWVW_PRIVATE *pdwvw, int bit_count) ;

static int	dwvw_encode_data (SF_PRIVATE *psf, DWVW_PRIVATE *pdwvw, const int *ptr, int len) ;
static void dwvw_encode_store_bits (SF_PRIVATE *psf, DWVW_PRIVATE *pdwvw, int data, int new_bits) ;
static void dwvw_read_reset (DWVW_PRIVATE *pdwvw) ;

/*============================================================================================
** DWVW initialisation function.
*/

int
dwvw_init (SF_PRIVATE *psf, int bitwidth)
{	DWVW_PRIVATE	*pdwvw ;

	if (psf->codec_data != NULL)
	{	psf_log_printf (psf, "*** psf->codec_data is not NULL.\n") ;
		return SFE_INTERNAL ;
		} ;

	if (bitwidth > 24)
		return SFE_DWVW_BAD_BITWIDTH ;

	if (psf->file.mode == SFM_RDWR)
		return SFE_BAD_MODE_RW ;

	if ((pdwvw = calloc (1, sizeof (DWVW_PRIVATE))) == NULL)
		return SFE_MALLOC_FAILED ;

	psf->codec_data = (void*) pdwvw ;
	pdwvw->bit_width 	= bitwidth ;
	dwvw_read_reset (pdwvw) ;

	if (psf->file.mode == SFM_READ)
	{	psf->read_short		= dwvw_read_s ;
		psf->read_int		= dwvw_read_i ;
		psf->read_float		= dwvw_read_f ;
		psf->read_double	= dwvw_read_d ;
		} ;

	if (psf->file.mode == SFM_WRITE)
	{	psf->write_short	= dwvw_write_s ;
		psf->write_int		= dwvw_write_i ;
		psf->write_float	= dwvw_write_f ;
		psf->write_double	= dwvw_write_d ;
		} ;

	psf->codec_close = dwvw_close ;
	psf->seek = dwvw_seek ;
	psf->byterate = dwvw_byterate ;

	if (psf->file.mode == SFM_READ)
	{	psf->sf.frames = psf_decode_frame_count (psf) ;
		dwvw_read_reset (pdwvw) ;
		} ;

	return 0 ;
} /* dwvw_init */

/*--------------------------------------------------------------------------------------------
*/

static int
dwvw_close (SF_PRIVATE *psf)
{	DWVW_PRIVATE *pdwvw ;

	if (psf->codec_data == NULL)
		return 0 ;
	pdwvw = (DWVW_PRIVATE*) psf->codec_data ;

	if (psf->file.mode == SFM_WRITE)
	{	static int last_values [12] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 } ;

		/* Write 8 zero samples to fully flush output. */
		dwvw_encode_data (psf, pdwvw, last_values, 12) ;

		/* Write the last buffer worth of data to disk. */
		psf_fwrite (pdwvw->b.buffer, 1, pdwvw->b.index, psf) ;

		if (psf->write_header)
			psf->write_header (psf, SF_TRUE) ;
		} ;

	return 0 ;
} /* dwvw_close */

static sf_count_t
dwvw_seek	(SF_PRIVATE *psf, int UNUSED (mode), sf_count_t offset)
{	DWVW_PRIVATE *pdwvw ;

	if (! psf->codec_data)
	{	psf->error = SFE_INTERNAL ;
		return PSF_SEEK_ERROR ;
		} ;

	pdwvw = (DWVW_PRIVATE*) psf->codec_data ;

	if (offset == 0)
	{	psf_fseek (psf, psf->dataoffset, SEEK_SET) ;
		dwvw_read_reset (pdwvw) ;
		return 0 ;
		} ;

	psf->error = SFE_BAD_SEEK ;
	return	PSF_SEEK_ERROR ;
} /* dwvw_seek */

static int
dwvw_byterate	(SF_PRIVATE *psf)
{
	if (psf->file.mode == SFM_READ)
		return (psf->datalength * psf->sf.samplerate) / psf->sf.frames ;

	return -1 ;
} /* dwvw_byterate */

/*==============================================================================
*/

static sf_count_t
dwvw_read_s (SF_PRIVATE *psf, short *ptr, sf_count_t len)
{	DWVW_PRIVATE *pdwvw ;
	BUF_UNION	ubuf ;
	int		*iptr ;
	int		k, bufferlen, readcount = 0, count ;
	sf_count_t	total = 0 ;

	if (! psf->codec_data)
		return 0 ;
	pdwvw = (DWVW_PRIVATE*) psf->codec_data ;

	iptr = ubuf.ibuf ;
	bufferlen = ARRAY_LEN (ubuf.ibuf) ;
	while (len > 0)
	{	readcount = (len >= bufferlen) ? bufferlen : len ;
		count = dwvw_decode_data (psf, pdwvw, iptr, readcount) ;
		for (k = 0 ; k < readcount ; k++)
			ptr [total + k] = iptr [k] >> 16 ;

		total += count ;
		len -= readcount ;
		if (count != readcount)
			break ;
		} ;

	return total ;
} /* dwvw_read_s */

static sf_count_t
dwvw_read_i (SF_PRIVATE *psf, int *ptr, sf_count_t len)
{	DWVW_PRIVATE *pdwvw ;
	int			readcount, count ;
	sf_count_t	total = 0 ;

	if (! psf->codec_data)
		return 0 ;
	pdwvw = (DWVW_PRIVATE*) psf->codec_data ;

	while (len > 0)
	{	readcount = (len > 0x10000000) ? 0x10000000 : (int) len ;

		count = dwvw_decode_data (psf, pdwvw, ptr, readcount) ;

		total += count ;
		len -= count ;

		if (count != readcount)
			break ;
		} ;

	return total ;
} /* dwvw_read_i */

static sf_count_t
dwvw_read_f (SF_PRIVATE *psf, float *ptr, sf_count_t len)
{	DWVW_PRIVATE *pdwvw ;
	BUF_UNION	ubuf ;
	int			*iptr ;
	int			k, bufferlen, readcount = 0, count ;
	sf_count_t	total = 0 ;
	float	normfact ;

	if (! psf->codec_data)
		return 0 ;
	pdwvw = (DWVW_PRIVATE*) psf->codec_data ;

	normfact = (psf->norm_float == SF_TRUE) ? 1.0 / ((float) 0x80000000) : 1.0 ;

	iptr = ubuf.ibuf ;
	bufferlen = ARRAY_LEN (ubuf.ibuf) ;
	while (len > 0)
	{	readcount = (len >= bufferlen) ? bufferlen : len ;
		count = dwvw_decode_data (psf, pdwvw, iptr, readcount) ;
		for (k = 0 ; k < readcount ; k++)
			ptr [total + k] = normfact * (float) (iptr [k]) ;

		total += count ;
		len -= readcount ;
		if (count != readcount)
			break ;
		} ;

	return total ;
} /* dwvw_read_f */

static sf_count_t
dwvw_read_d (SF_PRIVATE *psf, double *ptr, sf_count_t len)
{	DWVW_PRIVATE *pdwvw ;
	BUF_UNION	ubuf ;
	int			*iptr ;
	int			k, bufferlen, readcount = 0, count ;
	sf_count_t	total = 0 ;
	double 	normfact ;

	if (! psf->codec_data)
		return 0 ;
	pdwvw = (DWVW_PRIVATE*) psf->codec_data ;

	normfact = (psf->norm_double == SF_TRUE) ? 1.0 / ((double) 0x80000000) : 1.0 ;

	iptr = ubuf.ibuf ;
	bufferlen = ARRAY_LEN (ubuf.ibuf) ;
	while (len > 0)
	{	readcount = (len >= bufferlen) ? bufferlen : len ;
		count = dwvw_decode_data (psf, pdwvw, iptr, readcount) ;
		for (k = 0 ; k < readcount ; k++)
			ptr [total + k] = normfact * (double) (iptr [k]) ;

		total += count ;
		len -= readcount ;
		if (count != readcount)
			break ;
		} ;

	return total ;
} /* dwvw_read_d */

static int
dwvw_decode_data (SF_PRIVATE *psf, DWVW_PRIVATE *pdwvw, int *ptr, int len)
{	int	count ;
	int delta_width_modifier, delta_width, delta_negative, delta, sample ;

	/* Restore state from last decode call. */
	delta_width = pdwvw->last_delta_width ;
	sample = pdwvw->last_sample ;

	for (count = 0 ; count < len ; count++)
	{	/* If bit_count parameter is zero get the delta_width_modifier. */
		delta_width_modifier = dwvw_decode_load_bits (psf, pdwvw, -1) ;

		/* Check for end of input bit stream. Break loop if end. */
		if (delta_width_modifier < 0 || (pdwvw->b.end == 0 && count == 0))
			break ;

		if (delta_width_modifier && dwvw_decode_load_bits (psf, pdwvw, 1))
			delta_width_modifier = - delta_width_modifier ;

		/* Calculate the current word width. */
		delta_width = (delta_width + delta_width_modifier + pdwvw->bit_width) % pdwvw->bit_width ;

		/* Load the delta. */
		delta = 0 ;
		if (delta_width)
		{	delta = dwvw_decode_load_bits (psf, pdwvw, delta_width - 1) | (1 << (delta_width - 1)) ;
			delta_negative = dwvw_decode_load_bits (psf, pdwvw, 1) ;
			if (delta == pdwvw->max_delta - 1)
				delta += dwvw_decode_load_bits (psf, pdwvw, 1) ;
			if (delta_negative)
				delta = -delta ;
			} ;

		/* Calculate the sample */
		sample += delta ;

		if (sample >= pdwvw->max_delta)
			sample -= pdwvw->span ;
		else if (sample < - pdwvw->max_delta)
			sample += pdwvw->span ;

		/* Store the sample justifying to the most significant bit. */
		ptr [count] = arith_shift_left (sample, 32 - pdwvw->bit_width) ;

		if (pdwvw->b.end == 0 && pdwvw->bit_count == 0)
			break ;
		} ;

	pdwvw->last_delta_width = delta_width ;
	pdwvw->last_sample = sample ;

	pdwvw->samplecount += count ;

	return count ;
} /* dwvw_decode_data */

static int
dwvw_decode_load_bits (SF_PRIVATE *psf, DWVW_PRIVATE *pdwvw, int bit_count)
{	int output = 0, get_dwm = SF_FALSE ;

	/*
	**	Depending on the value of parameter bit_count, either get the
	**	required number of bits (ie bit_count > 0) or the
	**	delta_width_modifier (otherwise).
	*/

	if (bit_count < 0)
	{	get_dwm = SF_TRUE ;
		/* modify bit_count to ensure we have enought bits for finding dwm. */
		bit_count = pdwvw->dwm_maxsize ;
		} ;

	/* Load bits in bit reseviour. */
	while (pdwvw->bit_count < bit_count)
	{	if (pdwvw->b.index >= pdwvw->b.end)
		{	pdwvw->b.end = psf_fread (pdwvw->b.buffer, 1, sizeof (pdwvw->b.buffer), psf) ;
			pdwvw->b.index = 0 ;
			} ;

		/* Check for end of input stream. */
		if (bit_count < 8 && pdwvw->b.end == 0)
			return -1 ;

		pdwvw->bits = arith_shift_left (pdwvw->bits, 8) ;

		if (pdwvw->b.index < pdwvw->b.end)
		{	pdwvw->bits |= pdwvw->b.buffer [pdwvw->b.index] ;
			pdwvw->b.index ++ ;
			} ;
		pdwvw->bit_count += 8 ;
		} ;

	/* If asked to get bits do so. */
	if (! get_dwm)
	{	output = (pdwvw->bits >> (pdwvw->bit_count - bit_count)) & ((1 << bit_count) - 1) ;
		pdwvw->bit_count -= bit_count ;
		return output ;
		} ;

	/* Otherwise must have been asked to get delta_width_modifier. */
	while (output < (pdwvw->dwm_maxsize))
	{	pdwvw->bit_count -= 1 ;
		if (pdwvw->bits & (1 << pdwvw->bit_count))
			break ;
		output += 1 ;
		} ;

	return output ;
} /* dwvw_decode_load_bits */

static void
dwvw_read_reset (DWVW_PRIVATE *pdwvw)
{	int bitwidth = pdwvw->bit_width ;

	memset (pdwvw, 0, sizeof (DWVW_PRIVATE)) ;

	pdwvw->bit_width	= bitwidth ;
	pdwvw->dwm_maxsize	= bitwidth / 2 ;
	pdwvw->max_delta	= 1 << (bitwidth - 1) ;
	pdwvw->span			= 1 << bitwidth ;
} /* dwvw_read_reset */

static void
dwvw_encode_store_bits (SF_PRIVATE *psf, DWVW_PRIVATE *pdwvw, int data, int new_bits)
{	int 	byte ;

	/* Shift the bits into the resevoir. */
	pdwvw->bits = arith_shift_left (pdwvw->bits, new_bits) | (data & (arith_shift_left (1, new_bits) - 1)) ;
	pdwvw->bit_count += new_bits ;

	/* Transfer bit to buffer. */
	while (pdwvw->bit_count >= 8)
	{	byte = pdwvw->bits >> (pdwvw->bit_count - 	8) ;
		pdwvw->bit_count -= 8 ;
		pdwvw->b.buffer [pdwvw->b.index] = byte & 0xFF ;
		pdwvw->b.index ++ ;
		} ;

	if (pdwvw->b.index > SIGNED_SIZEOF (pdwvw->b.buffer) - 4)
	{	psf_fwrite (pdwvw->b.buffer, 1, pdwvw->b.index, psf) ;
		pdwvw->b.index = 0 ;
		} ;

	return ;
} /* dwvw_encode_store_bits */

#if 0
/* Debigging routine. */
static void
dump_bits (DWVW_PRIVATE *pdwvw)
{	int k, mask ;

	for (k = 0 ; k < 10 && k < pdwvw->b.index ; k++)
	{	mask = 0x80 ;
		while (mask)
		{	putchar (mask & pdwvw->b.buffer [k] ? '1' : '0') ;
			mask >>= 1 ;
			} ;
		putchar (' ') ;
		}

	for (k = pdwvw->bit_count - 1 ; k >= 0 ; k --)
		putchar (pdwvw->bits & (1 << k) ? '1' : '0') ;

	putchar ('\n') ;
} /* dump_bits */
#endif

#define HIGHEST_BIT(x, count)		\
			{	int y = x ;			\
				(count) = 0 ;		\
				while (y)			\
				{	(count) ++ ;	\
					y >>= 1 ;		\
					} ;				\
				} ;

static int
dwvw_encode_data (SF_PRIVATE *psf, DWVW_PRIVATE *pdwvw, const int *ptr, int len)
{	int	count ;
	int delta_width_modifier, delta, delta_negative, delta_width, extra_bit ;

	for (count = 0 ; count < len ; count++)
	{	delta = (ptr [count] >> (32 - pdwvw->bit_width)) - pdwvw->last_sample ;

		/* Calculate extra_bit if needed. */
		extra_bit = -1 ;
		delta_negative = 0 ;
		if (delta < -pdwvw->max_delta)
			delta = pdwvw->max_delta + (delta % pdwvw->max_delta) ;
		else if (delta == -pdwvw->max_delta)
		{	extra_bit = 1 ;
			delta_negative = 1 ;
			delta = pdwvw->max_delta - 1 ;
			}
		else if (delta > pdwvw->max_delta)
		{	delta_negative = 1 ;
			delta = pdwvw->span - delta ;
			delta = abs (delta) ;
			}
		else if (delta == pdwvw->max_delta)
		{	extra_bit = 1 ;
			delta = pdwvw->max_delta - 1 ;
			}
		else if (delta < 0)
		{	delta_negative = 1 ;
			delta = abs (delta) ;
			} ;

		if (delta == pdwvw->max_delta - 1 && extra_bit == -1)
			extra_bit = 0 ;

		/* Find width in bits of delta */
		HIGHEST_BIT (delta, delta_width) ;

		/* Calculate the delta_width_modifier */
		delta_width_modifier = (delta_width - pdwvw->last_delta_width) % pdwvw->bit_width ;
		if (delta_width_modifier > pdwvw->dwm_maxsize)
			delta_width_modifier -= pdwvw->bit_width ;
		if (delta_width_modifier < -pdwvw->dwm_maxsize)
			delta_width_modifier += pdwvw->bit_width ;

		/* Write delta_width_modifier zeros, followed by terminating '1'. */
		dwvw_encode_store_bits (psf, pdwvw, 0, abs (delta_width_modifier)) ;
		if (abs (delta_width_modifier) != pdwvw->dwm_maxsize)
			dwvw_encode_store_bits (psf, pdwvw, 1, 1) ;

		/*  Write delta_width_modifier sign. */
		if (delta_width_modifier < 0)
			dwvw_encode_store_bits (psf, pdwvw, 1, 1) ;
		if (delta_width_modifier > 0)
			dwvw_encode_store_bits (psf, pdwvw, 0, 1) ;

		/* Write delta and delta sign bit. */
		if (delta_width)
		{	dwvw_encode_store_bits (psf, pdwvw, delta, abs (delta_width) - 1) ;
			dwvw_encode_store_bits (psf, pdwvw, (delta_negative ? 1 : 0), 1) ;
			} ;

		/* Write extra bit!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
		if (extra_bit >= 0)
			dwvw_encode_store_bits (psf, pdwvw, extra_bit, 1) ;

		pdwvw->last_sample = ptr [count] >> (32 - pdwvw->bit_width) ;
		pdwvw->last_delta_width = delta_width ;
		} ;

	pdwvw->samplecount += count ;

	return count ;
} /* dwvw_encode_data */

static sf_count_t
dwvw_write_s (SF_PRIVATE *psf, const short *ptr, sf_count_t len)
{	DWVW_PRIVATE *pdwvw ;
	BUF_UNION	ubuf ;
	int		*iptr ;
	int		k, bufferlen, writecount = 0, count ;
	sf_count_t	total = 0 ;

	if (! psf->codec_data)
		return 0 ;
	pdwvw = (DWVW_PRIVATE*) psf->codec_data ;

	iptr = ubuf.ibuf ;
	bufferlen = ARRAY_LEN (ubuf.ibuf) ;
	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : len ;
		for (k = 0 ; k < writecount ; k++)
			iptr [k] = arith_shift_left (ptr [total + k], 16) ;
		count = dwvw_encode_data (psf, pdwvw, iptr, writecount) ;

		total += count ;
		len -= writecount ;
		if (count != writecount)
			break ;
		} ;

	return total ;
} /* dwvw_write_s */

static sf_count_t
dwvw_write_i (SF_PRIVATE *psf, const int *ptr, sf_count_t len)
{	DWVW_PRIVATE *pdwvw ;
	int			writecount, count ;
	sf_count_t	total = 0 ;

	if (! psf->codec_data)
		return 0 ;
	pdwvw = (DWVW_PRIVATE*) psf->codec_data ;

	while (len > 0)
	{	writecount = (len > 0x10000000) ? 0x10000000 : (int) len ;

		count = dwvw_encode_data (psf, pdwvw, ptr, writecount) ;

		total += count ;
		len -= count ;

		if (count != writecount)
			break ;
		} ;

	return total ;
} /* dwvw_write_i */

static sf_count_t
dwvw_write_f (SF_PRIVATE *psf, const float *ptr, sf_count_t len)
{	DWVW_PRIVATE *pdwvw ;
	BUF_UNION	ubuf ;
	int			*iptr ;
	int			k, bufferlen, writecount = 0, count ;
	sf_count_t	total = 0 ;
	float		normfact ;

	if (! psf->codec_data)
		return 0 ;
	pdwvw = (DWVW_PRIVATE*) psf->codec_data ;

	normfact = (psf->norm_float == SF_TRUE) ? (1.0 * 0x7FFFFFFF) : 1.0 ;

	iptr = ubuf.ibuf ;
	bufferlen = ARRAY_LEN (ubuf.ibuf) ;
	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : len ;
		for (k = 0 ; k < writecount ; k++)
			iptr [k] = lrintf (normfact * ptr [total + k]) ;
		count = dwvw_encode_data (psf, pdwvw, iptr, writecount) ;

		total += count ;
		len -= writecount ;
		if (count != writecount)
			break ;
		} ;

	return total ;
} /* dwvw_write_f */

static sf_count_t
dwvw_write_d (SF_PRIVATE *psf, const double *ptr, sf_count_t len)
{	DWVW_PRIVATE *pdwvw ;
	BUF_UNION	ubuf ;
	int			*iptr ;
	int			k, bufferlen, writecount = 0, count ;
	sf_count_t	total = 0 ;
	double 		normfact ;

	if (! psf->codec_data)
		return 0 ;
	pdwvw = (DWVW_PRIVATE*) psf->codec_data ;

	normfact = (psf->norm_double == SF_TRUE) ? (1.0 * 0x7FFFFFFF) : 1.0 ;

	iptr = ubuf.ibuf ;
	bufferlen = ARRAY_LEN (ubuf.ibuf) ;
	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : len ;
		for (k = 0 ; k < writecount ; k++)
			iptr [k] = lrint (normfact * ptr [total + k]) ;
		count = dwvw_encode_data (psf, pdwvw, iptr, writecount) ;

		total += count ;
		len -= writecount ;
		if (count != writecount)
			break ;
		} ;

	return total ;
} /* dwvw_write_d */

