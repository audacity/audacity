/*
** Copyright (C) 2002-2009 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#include	"sfendian.h"

#include	<stdlib.h>

#include	"sndfile.h"
#include	"common.h"

#define		INTERLEAVE_CHANNELS		6

typedef struct
{	double	buffer [SF_BUFFER_LEN / sizeof (double)] ;

	sf_count_t		channel_len ;

	sf_count_t		(*read_short)	(SF_PRIVATE*, short *ptr, sf_count_t len) ;
	sf_count_t		(*read_int)	(SF_PRIVATE*, int *ptr, sf_count_t len) ;
	sf_count_t		(*read_float)	(SF_PRIVATE*, float *ptr, sf_count_t len) ;
	sf_count_t		(*read_double)	(SF_PRIVATE*, double *ptr, sf_count_t len) ;

} INTERLEAVE_DATA ;



static sf_count_t	interleave_read_short	(SF_PRIVATE *psf, short *ptr, sf_count_t len) ;
static sf_count_t	interleave_read_int	(SF_PRIVATE *psf, int *ptr, sf_count_t len) ;
static sf_count_t	interleave_read_float	(SF_PRIVATE *psf, float *ptr, sf_count_t len) ;
static sf_count_t	interleave_read_double	(SF_PRIVATE *psf, double *ptr, sf_count_t len) ;

static sf_count_t	interleave_seek	(SF_PRIVATE*, int mode, sf_count_t samples_from_start) ;




int
interleave_init	(SF_PRIVATE *psf)
{	INTERLEAVE_DATA *pdata ;

	if (psf->mode != SFM_READ)
		return SFE_INTERLEAVE_MODE ;

	if (psf->interleave)
	{	psf_log_printf (psf, "*** Weird, already have interleave.\n") ;
		return 666 ;
		} ;

	/* Free this in sf_close() function. */
	if (! (pdata = malloc (sizeof (INTERLEAVE_DATA))))
		return SFE_MALLOC_FAILED ;

puts ("interleave_init") ;

	psf->interleave = pdata ;

	/* Save the existing methods. */
	pdata->read_short	= psf->read_short ;
	pdata->read_int		= psf->read_int ;
	pdata->read_float	= psf->read_float ;
	pdata->read_double	= psf->read_double ;

	pdata->channel_len = psf->sf.frames * psf->bytewidth ;

	/* Insert our new methods. */
	psf->read_short		= interleave_read_short ;
	psf->read_int		= interleave_read_int ;
	psf->read_float		= interleave_read_float ;
	psf->read_double	= interleave_read_double ;

	psf->seek = interleave_seek ;

	return 0 ;
} /* pcm_interleave_init */

/*------------------------------------------------------------------------------
*/

static sf_count_t
interleave_read_short	(SF_PRIVATE *psf, short *ptr, sf_count_t len)
{	INTERLEAVE_DATA *pdata ;
	sf_count_t	offset, templen ;
	int			chan, count, k ;
	short		*inptr, *outptr ;

	if (! (pdata = psf->interleave))
		return 0 ;

	inptr = (short*) pdata->buffer ;

	for (chan = 0 ; chan < psf->sf.channels ; chan++)
	{	outptr = ptr + chan ;

		offset = psf->dataoffset + chan * psf->bytewidth * psf->read_current ;

		if (psf_fseek (psf, offset, SEEK_SET) != offset)
		{	psf->error = SFE_INTERLEAVE_SEEK ;
			return 0 ;
			} ;

		templen = len / psf->sf.channels ;

		while (templen > 0)
		{	if (templen > SIGNED_SIZEOF (pdata->buffer) / SIGNED_SIZEOF (short))
				count = SIGNED_SIZEOF (pdata->buffer) / SIGNED_SIZEOF (short) ;
			else
				count = (int) templen ;

			if (pdata->read_short (psf, inptr, count) != count)
			{	psf->error = SFE_INTERLEAVE_READ ;
				return 0 ;
				} ;

			for (k = 0 ; k < count ; k++)
			{	*outptr = inptr [k] ;
				outptr += psf->sf.channels ;
				} ;

			templen -= count ;
			} ;
		} ;

	return len ;
} /* interleave_read_short */

static sf_count_t
interleave_read_int	(SF_PRIVATE *psf, int *ptr, sf_count_t len)
{	INTERLEAVE_DATA *pdata ;
	sf_count_t	offset, templen ;
	int			chan, count, k ;
	int		*inptr, *outptr ;

	if (! (pdata = psf->interleave))
		return 0 ;

	inptr = (int*) pdata->buffer ;

	for (chan = 0 ; chan < psf->sf.channels ; chan++)
	{	outptr = ptr + chan ;

		offset = psf->dataoffset + chan * psf->bytewidth * psf->read_current ;

		if (psf_fseek (psf, offset, SEEK_SET) != offset)
		{	psf->error = SFE_INTERLEAVE_SEEK ;
			return 0 ;
			} ;

		templen = len / psf->sf.channels ;

		while (templen > 0)
		{	if (templen > SIGNED_SIZEOF (pdata->buffer) / SIGNED_SIZEOF (int))
				count = SIGNED_SIZEOF (pdata->buffer) / SIGNED_SIZEOF (int) ;
			else
				count = (int) templen ;

			if (pdata->read_int (psf, inptr, count) != count)
			{	psf->error = SFE_INTERLEAVE_READ ;
				return 0 ;
				} ;

			for (k = 0 ; k < count ; k++)
			{	*outptr = inptr [k] ;
				outptr += psf->sf.channels ;
				} ;

			templen -= count ;
			} ;
		} ;

	return len ;
} /* interleave_read_int */

static sf_count_t
interleave_read_float	(SF_PRIVATE *psf, float *ptr, sf_count_t len)
{	INTERLEAVE_DATA *pdata ;
	sf_count_t	offset, templen ;
	int			chan, count, k ;
	float		*inptr, *outptr ;

	if (! (pdata = psf->interleave))
		return 0 ;

	inptr = (float*) pdata->buffer ;

	for (chan = 0 ; chan < psf->sf.channels ; chan++)
	{	outptr = ptr + chan ;

		offset = psf->dataoffset + pdata->channel_len * chan + psf->read_current * psf->bytewidth ;

/*-printf ("chan : %d     read_current : %6lld    offset : %6lld\n", chan, psf->read_current, offset) ;-*/

		if (psf_fseek (psf, offset, SEEK_SET) != offset)
		{	psf->error = SFE_INTERLEAVE_SEEK ;
/*-puts ("interleave_seek error") ; exit (1) ;-*/
			return 0 ;
			} ;

		templen = len / psf->sf.channels ;

		while (templen > 0)
		{	if (templen > SIGNED_SIZEOF (pdata->buffer) / SIGNED_SIZEOF (float))
				count = SIGNED_SIZEOF (pdata->buffer) / SIGNED_SIZEOF (float) ;
			else
				count = (int) templen ;

			if (pdata->read_float (psf, inptr, count) != count)
			{	psf->error = SFE_INTERLEAVE_READ ;
/*-puts ("interleave_read error") ; exit (1) ;-*/
				return 0 ;
				} ;

			for (k = 0 ; k < count ; k++)
			{	*outptr = inptr [k] ;
				outptr += psf->sf.channels ;
				} ;

			templen -= count ;
			} ;
		} ;

	return len ;
} /* interleave_read_float */

static sf_count_t
interleave_read_double	(SF_PRIVATE *psf, double *ptr, sf_count_t len)
{	INTERLEAVE_DATA *pdata ;
	sf_count_t	offset, templen ;
	int			chan, count, k ;
	double		*inptr, *outptr ;

	if (! (pdata = psf->interleave))
		return 0 ;

	inptr = (double*) pdata->buffer ;

	for (chan = 0 ; chan < psf->sf.channels ; chan++)
	{	outptr = ptr + chan ;

		offset = psf->dataoffset + chan * psf->bytewidth * psf->read_current ;

		if (psf_fseek (psf, offset, SEEK_SET) != offset)
		{	psf->error = SFE_INTERLEAVE_SEEK ;
			return 0 ;
			} ;

		templen = len / psf->sf.channels ;

		while (templen > 0)
		{	if (templen > SIGNED_SIZEOF (pdata->buffer) / SIGNED_SIZEOF (double))
				count = SIGNED_SIZEOF (pdata->buffer) / SIGNED_SIZEOF (double) ;
			else
				count = (int) templen ;

			if (pdata->read_double (psf, inptr, count) != count)
			{	psf->error = SFE_INTERLEAVE_READ ;
				return 0 ;
				} ;

			for (k = 0 ; k < count ; k++)
			{	*outptr = inptr [k] ;
				outptr += psf->sf.channels ;
				} ;

			templen -= count ;
			} ;
		} ;

	return len ;
} /* interleave_read_double */

/*------------------------------------------------------------------------------
*/

static sf_count_t
interleave_seek	(SF_PRIVATE *psf, int mode, sf_count_t samples_from_start)
{	psf = psf ;	mode = mode ;

	/*
	** Do nothing here. This is a place holder to prevent the default
	** seek function from being called.
	*/

	return samples_from_start ;
} /* interleave_seek */

