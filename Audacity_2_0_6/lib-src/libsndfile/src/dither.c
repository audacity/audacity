/*
** Copyright (C) 2003-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#include	<stdlib.h>

#include	"sndfile.h"
#include	"sfendian.h"
#include	"common.h"

/*============================================================================
**	Rule number 1 is to only apply dither when going from a larger bitwidth
**	to a smaller bitwidth. This can happen on both read and write.
**
**	Need to apply dither on all conversions marked X below.
**
**	Dither on write:
**
**										Input
**					|	short		int			float		double
**			--------+-----------------------------------------------
**		O	8 bit	|	X			X			X			X
**		u	16 bit	|	none		X			X			X
**		t	24 bit	|	none		X			X			X
**		p	32 bit	|	none		none		X			X
**		u	float	|	none		none		none		none
**		t	double	|	none		none		none		none
**
**	Dither on read:
**
**										Input
**		O			|	8 bit	16 bit	24 bit	32 bit	float	double
**		u	--------+-------------------------------------------------
**		t	short	|	none	none	X		X		X		X
**		p	int		|	none	none	none	X		X		X
**		u	float	|	none	none	none	none	none	none
**		t	double	|	none	none	none	none	none	none
*/

#define	SFE_DITHER_BAD_PTR	666
#define	SFE_DITHER_BAD_TYPE	667

typedef struct
{	int			read_short_dither_bits, read_int_dither_bits ;
	int			write_short_dither_bits, write_int_dither_bits ;
	double		read_float_dither_scale, read_double_dither_bits ;
	double		write_float_dither_scale, write_double_dither_bits ;

	sf_count_t	(*read_short)	(SF_PRIVATE *psf, short *ptr, sf_count_t len) ;
	sf_count_t	(*read_int)		(SF_PRIVATE *psf, int *ptr, sf_count_t len) ;
	sf_count_t	(*read_float)	(SF_PRIVATE *psf, float *ptr, sf_count_t len) ;
	sf_count_t	(*read_double)	(SF_PRIVATE *psf, double *ptr, sf_count_t len) ;

	sf_count_t	(*write_short)	(SF_PRIVATE *psf, const short *ptr, sf_count_t len) ;
	sf_count_t	(*write_int)	(SF_PRIVATE *psf, const int *ptr, sf_count_t len) ;
	sf_count_t	(*write_float)	(SF_PRIVATE *psf, const float *ptr, sf_count_t len) ;
	sf_count_t	(*write_double)	(SF_PRIVATE *psf, const double *ptr, sf_count_t len) ;

	double buffer [SF_BUFFER_LEN / sizeof (double)] ;
} DITHER_DATA ;

static sf_count_t dither_read_short		(SF_PRIVATE *psf, short *ptr, sf_count_t len) ;
static sf_count_t dither_read_int		(SF_PRIVATE *psf, int *ptr, sf_count_t len) ;

static sf_count_t dither_write_short	(SF_PRIVATE *psf, const short *ptr, sf_count_t len) ;
static sf_count_t dither_write_int		(SF_PRIVATE *psf, const int *ptr, sf_count_t len) ;
static sf_count_t dither_write_float	(SF_PRIVATE *psf, const float *ptr, sf_count_t len) ;
static sf_count_t dither_write_double	(SF_PRIVATE *psf, const double *ptr, sf_count_t len) ;

int
dither_init (SF_PRIVATE *psf, int mode)
{	DITHER_DATA *pdither ;

	pdither = psf->dither ; /* This may be NULL. */

	/* Turn off dither on read. */
	if (mode == SFM_READ && psf->read_dither.type == SFD_NO_DITHER)
	{	if (pdither == NULL)
			return 0 ; /* Dither is already off, so just return. */

		if (pdither->read_short)
			psf->read_short = pdither->read_short ;
		if (pdither->read_int)
			psf->read_int = pdither->read_int ;
		if (pdither->read_float)
			psf->read_float = pdither->read_float ;
		if (pdither->read_double)
			psf->read_double = pdither->read_double ;
		return 0 ;
		} ;

	/* Turn off dither on write. */
	if (mode == SFM_WRITE && psf->write_dither.type == SFD_NO_DITHER)
	{	if (pdither == NULL)
			return 0 ; /* Dither is already off, so just return. */

		if (pdither->write_short)
			psf->write_short = pdither->write_short ;
		if (pdither->write_int)
			psf->write_int = pdither->write_int ;
		if (pdither->write_float)
			psf->write_float = pdither->write_float ;
		if (pdither->write_double)
			psf->write_double = pdither->write_double ;
		return 0 ;
		} ;

	/* Turn on dither on read if asked. */
	if (mode == SFM_READ && psf->read_dither.type != 0)
	{	if (pdither == NULL)
			pdither = psf->dither = calloc (1, sizeof (DITHER_DATA)) ;
		if (pdither == NULL)
			return SFE_MALLOC_FAILED ;

		switch (SF_CODEC (psf->sf.format))
		{	case SF_FORMAT_DOUBLE :
			case SF_FORMAT_FLOAT :
					pdither->read_int = psf->read_int ;
					psf->read_int = dither_read_int ;
					break ;

			case SF_FORMAT_PCM_32 :
			case SF_FORMAT_PCM_24 :
			case SF_FORMAT_PCM_16 :
			case SF_FORMAT_PCM_S8 :
			case SF_FORMAT_PCM_U8 :
					pdither->read_short = psf->read_short ;
					psf->read_short = dither_read_short ;
					break ;

			default : break ;
			} ;
		} ;

	/* Turn on dither on write if asked. */
	if (mode == SFM_WRITE && psf->write_dither.type != 0)
	{	if (pdither == NULL)
			pdither = psf->dither = calloc (1, sizeof (DITHER_DATA)) ;
		if (pdither == NULL)
			return SFE_MALLOC_FAILED ;

		switch (SF_CODEC (psf->sf.format))
		{	case SF_FORMAT_DOUBLE :
			case SF_FORMAT_FLOAT :
					pdither->write_int = psf->write_int ;
					psf->write_int = dither_write_int ;
					break ;

			case SF_FORMAT_PCM_32 :
			case SF_FORMAT_PCM_24 :
			case SF_FORMAT_PCM_16 :
			case SF_FORMAT_PCM_S8 :
			case SF_FORMAT_PCM_U8 :
					break ;

			default : break ;
			} ;

		pdither->write_short = psf->write_short ;
		psf->write_short = dither_write_short ;

		pdither->write_int = psf->write_int ;
		psf->write_int = dither_write_int ;

		pdither->write_float = psf->write_float ;
		psf->write_float = dither_write_float ;

		pdither->write_double = psf->write_double ;
		psf->write_double = dither_write_double ;
		} ;

	return 0 ;
} /* dither_init */

/*==============================================================================
*/

static void dither_short	(const short *in, short *out, int frames, int channels) ;
static void dither_int		(const int *in, int *out, int frames, int channels) ;

static void dither_float	(const float *in, float *out, int frames, int channels) ;
static void dither_double	(const double *in, double *out, int frames, int channels) ;

static sf_count_t
dither_read_short (SF_PRIVATE * UNUSED (psf), short * UNUSED (ptr), sf_count_t len)
{
	return len ;
} /* dither_read_short */

static sf_count_t
dither_read_int (SF_PRIVATE * UNUSED (psf), int * UNUSED (ptr), sf_count_t len)
{
	return len ;
} /* dither_read_int */

/*------------------------------------------------------------------------------
*/

static sf_count_t
dither_write_short	(SF_PRIVATE *psf, const short *ptr, sf_count_t len)
{	DITHER_DATA *pdither ;
	int			bufferlen, writecount, thiswrite ;
	sf_count_t	total = 0 ;

	if ((pdither = psf->dither) == NULL)
	{	psf->error = SFE_DITHER_BAD_PTR ;
		return 0 ;
		} ;

	switch (SF_CODEC (psf->sf.format))
	{	case SF_FORMAT_PCM_S8 :
		case SF_FORMAT_PCM_U8 :
		case SF_FORMAT_DPCM_8 :
				break ;

		default :
			return pdither->write_short (psf, ptr, len) ;
		} ;

	bufferlen = sizeof (pdither->buffer) / sizeof (short) ;

	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : (int) len ;
		writecount /= psf->sf.channels ;
		writecount *= psf->sf.channels ;

		dither_short (ptr, (short*) pdither->buffer, writecount / psf->sf.channels, psf->sf.channels) ;

		thiswrite = pdither->write_short (psf, (short*) pdither->buffer, writecount) ;
		total += thiswrite ;
		len -= thiswrite ;
		if (thiswrite < writecount)
			break ;
		} ;

	return total ;
} /* dither_write_short */

static sf_count_t
dither_write_int	(SF_PRIVATE *psf, const int *ptr, sf_count_t len)
{	DITHER_DATA *pdither ;
	int			bufferlen, writecount, thiswrite ;
	sf_count_t	total = 0 ;

	if ((pdither = psf->dither) == NULL)
	{	psf->error = SFE_DITHER_BAD_PTR ;
		return 0 ;
		} ;

	switch (SF_CODEC (psf->sf.format))
	{	case SF_FORMAT_PCM_S8 :
		case SF_FORMAT_PCM_U8 :
		case SF_FORMAT_PCM_16 :
		case SF_FORMAT_PCM_24 :
				break ;

		case SF_FORMAT_DPCM_8 :
		case SF_FORMAT_DPCM_16 :
				break ;

		default :
			return pdither->write_int (psf, ptr, len) ;
		} ;


	bufferlen = sizeof (pdither->buffer) / sizeof (int) ;

	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : (int) len ;
		writecount /= psf->sf.channels ;
		writecount *= psf->sf.channels ;

		dither_int (ptr, (int*) pdither->buffer, writecount / psf->sf.channels, psf->sf.channels) ;

		thiswrite = pdither->write_int (psf, (int*) pdither->buffer, writecount) ;
		total += thiswrite ;
		len -= thiswrite ;
		if (thiswrite < writecount)
			break ;
		} ;

	return total ;
} /* dither_write_int */

static sf_count_t
dither_write_float	(SF_PRIVATE *psf, const float *ptr, sf_count_t len)
{	DITHER_DATA *pdither ;
	int			bufferlen, writecount, thiswrite ;
	sf_count_t	total = 0 ;

	if ((pdither = psf->dither) == NULL)
	{	psf->error = SFE_DITHER_BAD_PTR ;
		return 0 ;
		} ;

	switch (SF_CODEC (psf->sf.format))
	{	case SF_FORMAT_PCM_S8 :
		case SF_FORMAT_PCM_U8 :
		case SF_FORMAT_PCM_16 :
		case SF_FORMAT_PCM_24 :
				break ;

		case SF_FORMAT_DPCM_8 :
		case SF_FORMAT_DPCM_16 :
				break ;

		default :
			return pdither->write_float (psf, ptr, len) ;
		} ;

	bufferlen = sizeof (pdither->buffer) / sizeof (float) ;

	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : (float) len ;
		writecount /= psf->sf.channels ;
		writecount *= psf->sf.channels ;

		dither_float (ptr, (float*) pdither->buffer, writecount / psf->sf.channels, psf->sf.channels) ;

		thiswrite = pdither->write_float (psf, (float*) pdither->buffer, writecount) ;
		total += thiswrite ;
		len -= thiswrite ;
		if (thiswrite < writecount)
			break ;
		} ;

	return total ;
} /* dither_write_float */

static sf_count_t
dither_write_double	(SF_PRIVATE *psf, const double *ptr, sf_count_t len)
{	DITHER_DATA *pdither ;
	int			bufferlen, writecount, thiswrite ;
	sf_count_t	total = 0 ;

	if ((pdither = psf->dither) == NULL)
	{	psf->error = SFE_DITHER_BAD_PTR ;
		return 0 ;
		} ;

	switch (SF_CODEC (psf->sf.format))
	{	case SF_FORMAT_PCM_S8 :
		case SF_FORMAT_PCM_U8 :
		case SF_FORMAT_PCM_16 :
		case SF_FORMAT_PCM_24 :
				break ;

		case SF_FORMAT_DPCM_8 :
		case SF_FORMAT_DPCM_16 :
				break ;

		default :
			return pdither->write_double (psf, ptr, len) ;
		} ;


	bufferlen = sizeof (pdither->buffer) / sizeof (double) ;

	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : (double) len ;
		writecount /= psf->sf.channels ;
		writecount *= psf->sf.channels ;

		dither_double (ptr, (double*) pdither->buffer, writecount / psf->sf.channels, psf->sf.channels) ;

		thiswrite = pdither->write_double (psf, (double*) pdither->buffer, writecount) ;
		total += thiswrite ;
		len -= thiswrite ;
		if (thiswrite < writecount)
			break ;
		} ;

	return total ;
} /* dither_write_double */

/*==============================================================================
*/

static void
dither_short (const short *in, short *out, int frames, int channels)
{	int ch, k ;

	for (ch = 0 ; ch < channels ; ch++)
		for (k = ch ; k < channels * frames ; k += channels)
			out [k] = in [k] ;

} /* dither_short */

static void
dither_int (const int *in, int *out, int frames, int channels)
{	int ch, k ;

	for (ch = 0 ; ch < channels ; ch++)
		for (k = ch ; k < channels * frames ; k += channels)
			out [k] = in [k] ;

} /* dither_int */

static void
dither_float (const float *in, float *out, int frames, int channels)
{	int ch, k ;

	for (ch = 0 ; ch < channels ; ch++)
		for (k = ch ; k < channels * frames ; k += channels)
			out [k] = in [k] ;

} /* dither_float */

static void
dither_double (const double *in, double *out, int frames, int channels)
{	int ch, k ;

	for (ch = 0 ; ch < channels ; ch++)
		for (k = ch ; k < channels * frames ; k += channels)
			out [k] = in [k] ;

} /* dither_double */

/*==============================================================================
*/
#if 0

/*
** Not made public because this (maybe) requires storage of state information.
**
** Also maybe need separate state info for each channel!!!!
*/

int
DO_NOT_USE_sf_dither_short (const SF_DITHER_INFO *dither, const short *in, short *out, int frames, int channels)
{	int ch, k ;

	if (! dither)
		return SFE_DITHER_BAD_PTR ;

	switch (dither->type & SFD_TYPEMASK)
	{	case SFD_WHITE :
		case SFD_TRIANGULAR_PDF :
				for (ch = 0 ; ch < channels ; ch++)
					for (k = ch ; k < channels * frames ; k += channels)
						out [k] = in [k] ;
				break ;

		default :
			return SFE_DITHER_BAD_TYPE ;
		} ;

	return 0 ;
} /* DO_NOT_USE_sf_dither_short */

int
DO_NOT_USE_sf_dither_int (const SF_DITHER_INFO *dither, const int *in, int *out, int frames, int channels)
{	int ch, k ;

	if (! dither)
		return SFE_DITHER_BAD_PTR ;

	switch (dither->type & SFD_TYPEMASK)
	{	case SFD_WHITE :
		case SFD_TRIANGULAR_PDF :
				for (ch = 0 ; ch < channels ; ch++)
					for (k = ch ; k < channels * frames ; k += channels)
						out [k] = in [k] ;
				break ;

		default :
			return SFE_DITHER_BAD_TYPE ;
		} ;

	return 0 ;
} /* DO_NOT_USE_sf_dither_int */

int
DO_NOT_USE_sf_dither_float (const SF_DITHER_INFO *dither, const float *in, float *out, int frames, int channels)
{	int ch, k ;

	if (! dither)
		return SFE_DITHER_BAD_PTR ;

	switch (dither->type & SFD_TYPEMASK)
	{	case SFD_WHITE :
		case SFD_TRIANGULAR_PDF :
				for (ch = 0 ; ch < channels ; ch++)
					for (k = ch ; k < channels * frames ; k += channels)
						out [k] = in [k] ;
				break ;

		default :
			return SFE_DITHER_BAD_TYPE ;
		} ;

	return 0 ;
} /* DO_NOT_USE_sf_dither_float */

int
DO_NOT_USE_sf_dither_double (const SF_DITHER_INFO *dither, const double *in, double *out, int frames, int channels)
{	int ch, k ;

	if (! dither)
		return SFE_DITHER_BAD_PTR ;

	switch (dither->type & SFD_TYPEMASK)
	{	case SFD_WHITE :
		case SFD_TRIANGULAR_PDF :
				for (ch = 0 ; ch < channels ; ch++)
					for (k = ch ; k < channels * frames ; k += channels)
						out [k] = in [k] ;
				break ;

		default :
			return SFE_DITHER_BAD_TYPE ;
		} ;

	return 0 ;
} /* DO_NOT_USE_sf_dither_double */

#endif

