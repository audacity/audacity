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

#include "sfconfig.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "sndfile.h"
#include "sfendian.h"
#include "common.h"
#include "G72x/g72x.h"

/* This struct is private to the G72x code. */
struct g72x_state ;
typedef struct g72x_state G72x_STATE ;

typedef struct
{	/* Private data. Don't mess with it. */
	struct g72x_state * private ;

	/* Public data. Read only. */
	int				blocksize, samplesperblock, bytesperblock ;

	/* Public data. Read and write. */
	int				blocks_total, block_curr, sample_curr ;
	unsigned char	block	[G72x_BLOCK_SIZE] ;
	short			samples	[G72x_BLOCK_SIZE] ;
} G72x_PRIVATE ;

static	int	psf_g72x_decode_block (SF_PRIVATE *psf, G72x_PRIVATE *pg72x) ;
static	int	psf_g72x_encode_block (SF_PRIVATE *psf, G72x_PRIVATE *pg72x) ;

static	sf_count_t	g72x_read_s (SF_PRIVATE *psf, short *ptr, sf_count_t len) ;
static	sf_count_t	g72x_read_i (SF_PRIVATE *psf, int *ptr, sf_count_t len) ;
static	sf_count_t	g72x_read_f (SF_PRIVATE *psf, float *ptr, sf_count_t len) ;
static	sf_count_t	g72x_read_d (SF_PRIVATE *psf, double *ptr, sf_count_t len) ;

static	sf_count_t	g72x_write_s (SF_PRIVATE *psf, const short *ptr, sf_count_t len) ;
static	sf_count_t	g72x_write_i (SF_PRIVATE *psf, const int *ptr, sf_count_t len) ;
static	sf_count_t	g72x_write_f (SF_PRIVATE *psf, const float *ptr, sf_count_t len) ;
static	sf_count_t	g72x_write_d (SF_PRIVATE *psf, const double *ptr, sf_count_t len) ;

static	sf_count_t g72x_seek (SF_PRIVATE *psf, int mode, sf_count_t offset) ;

static	int	g72x_close (SF_PRIVATE *psf) ;


/*============================================================================================
** WAV G721 Reader initialisation function.
*/

int
g72x_init (SF_PRIVATE * psf)
{	G72x_PRIVATE	*pg72x ;
	int	bitspersample, bytesperblock, codec ;

	if (psf->codec_data != NULL)
	{	psf_log_printf (psf, "*** psf->codec_data is not NULL.\n") ;
		return SFE_INTERNAL ;
		} ;

	psf->sf.seekable = SF_FALSE ;

	if (psf->sf.channels != 1)
		return SFE_G72X_NOT_MONO ;

	if ((pg72x = calloc (1, sizeof (G72x_PRIVATE))) == NULL)
		return SFE_MALLOC_FAILED ;

	psf->codec_data = (void*) pg72x ;

	pg72x->block_curr = 0 ;
	pg72x->sample_curr = 0 ;

	switch (SF_CODEC (psf->sf.format))
	{	case SF_FORMAT_G721_32 :
				codec = G721_32_BITS_PER_SAMPLE ;
				bytesperblock = G721_32_BYTES_PER_BLOCK ;
				bitspersample = G721_32_BITS_PER_SAMPLE ;
				break ;

		case SF_FORMAT_G723_24:
				codec = G723_24_BITS_PER_SAMPLE ;
				bytesperblock = G723_24_BYTES_PER_BLOCK ;
				bitspersample = G723_24_BITS_PER_SAMPLE ;
				break ;

		case SF_FORMAT_G723_40:
				codec = G723_40_BITS_PER_SAMPLE ;
				bytesperblock = G723_40_BYTES_PER_BLOCK ;
				bitspersample = G723_40_BITS_PER_SAMPLE ;
				break ;

		default : return SFE_UNIMPLEMENTED ;
		} ;

	psf->blockwidth = psf->bytewidth = 1 ;

	psf->filelength = psf_get_filelen (psf) ;
	if (psf->filelength < psf->dataoffset)
		psf->filelength = psf->dataoffset ;

	psf->datalength = psf->filelength - psf->dataoffset ;
	if (psf->dataend > 0)
		psf->datalength -= psf->filelength - psf->dataend ;

	if (psf->mode == SFM_READ)
	{	pg72x->private = g72x_reader_init (codec, &(pg72x->blocksize), &(pg72x->samplesperblock)) ;
		if (pg72x->private == NULL)
			return SFE_MALLOC_FAILED ;

		pg72x->bytesperblock = bytesperblock ;

		psf->read_short		= g72x_read_s ;
		psf->read_int		= g72x_read_i ;
		psf->read_float		= g72x_read_f ;
		psf->read_double	= g72x_read_d ;

	 	psf->seek = g72x_seek ;

		if (psf->datalength % pg72x->blocksize)
		{	psf_log_printf (psf, "*** Odd psf->datalength (%D) should be a multiple of %d\n", psf->datalength, pg72x->blocksize) ;
			pg72x->blocks_total = (psf->datalength / pg72x->blocksize) + 1 ;
			}
		else
			pg72x->blocks_total = psf->datalength / pg72x->blocksize ;

		psf->sf.frames = pg72x->blocks_total * pg72x->samplesperblock ;

		psf_g72x_decode_block (psf, pg72x) ;
		}
	else if (psf->mode == SFM_WRITE)
	{	pg72x->private = g72x_writer_init (codec, &(pg72x->blocksize), &(pg72x->samplesperblock)) ;
		if (pg72x->private == NULL)
			return SFE_MALLOC_FAILED ;

		pg72x->bytesperblock = bytesperblock ;

		psf->write_short	= g72x_write_s ;
		psf->write_int		= g72x_write_i ;
		psf->write_float	= g72x_write_f ;
		psf->write_double	= g72x_write_d ;

		if (psf->datalength % pg72x->blocksize)
			pg72x->blocks_total = (psf->datalength / pg72x->blocksize) + 1 ;
		else
			pg72x->blocks_total = psf->datalength / pg72x->blocksize ;

		if (psf->datalength > 0)
			psf->sf.frames = (8 * psf->datalength) / bitspersample ;

		if ((psf->sf.frames * bitspersample) / 8 != psf->datalength)
			psf_log_printf (psf, "*** Warning : weird psf->datalength.\n") ;
		} ;

	psf->codec_close	= g72x_close ;

	return 0 ;
} /* g72x_init */

/*============================================================================================
** G721 Read Functions.
*/

static int
psf_g72x_decode_block (SF_PRIVATE *psf, G72x_PRIVATE *pg72x)
{	int	k ;

	pg72x->block_curr ++ ;
	pg72x->sample_curr = 0 ;

	if (pg72x->block_curr > pg72x->blocks_total)
	{	memset (pg72x->samples, 0, G72x_BLOCK_SIZE * sizeof (short)) ;
		return 1 ;
		} ;

	if ((k = psf_fread (pg72x->block, 1, pg72x->bytesperblock, psf)) != pg72x->bytesperblock)
		psf_log_printf (psf, "*** Warning : short read (%d != %d).\n", k, pg72x->bytesperblock) ;

	pg72x->blocksize = k ;
	g72x_decode_block (pg72x->private, pg72x->block, pg72x->samples) ;

	return 1 ;
} /* psf_g72x_decode_block */

static int
g72x_read_block (SF_PRIVATE *psf, G72x_PRIVATE *pg72x, short *ptr, int len)
{	int	count, total = 0, indx = 0 ;

	while (indx < len)
	{	if (pg72x->block_curr > pg72x->blocks_total)
		{	memset (&(ptr [indx]), 0, (len - indx) * sizeof (short)) ;
			return total ;
			} ;

		if (pg72x->sample_curr >= pg72x->samplesperblock)
			psf_g72x_decode_block (psf, pg72x) ;

		count = pg72x->samplesperblock - pg72x->sample_curr ;
		count = (len - indx > count) ? count : len - indx ;

		memcpy (&(ptr [indx]), &(pg72x->samples [pg72x->sample_curr]), count * sizeof (short)) ;
		indx += count ;
		pg72x->sample_curr += count ;
		total = indx ;
		} ;

	return total ;
} /* g72x_read_block */

static sf_count_t
g72x_read_s (SF_PRIVATE *psf, short *ptr, sf_count_t len)
{	G72x_PRIVATE 	*pg72x ;
	int			readcount, count ;
	sf_count_t	total = 0 ;

	if (psf->codec_data == NULL)
		return 0 ;
	pg72x = (G72x_PRIVATE*) psf->codec_data ;

	while (len > 0)
	{	readcount = (len > 0x10000000) ? 0x10000000 : (int) len ;

		count = g72x_read_block (psf, pg72x, ptr, readcount) ;

		total += count ;
		len -= count ;

		if (count != readcount)
			break ;
		} ;

	return total ;
} /* g72x_read_s */

static sf_count_t
g72x_read_i (SF_PRIVATE *psf, int *ptr, sf_count_t len)
{	G72x_PRIVATE *pg72x ;
	short		*sptr ;
	int			k, bufferlen, readcount = 0, count ;
	sf_count_t	total = 0 ;

	if (psf->codec_data == NULL)
		return 0 ;
	pg72x = (G72x_PRIVATE*) psf->codec_data ;

	sptr = psf->u.sbuf ;
	bufferlen = SF_BUFFER_LEN / sizeof (short) ;
	while (len > 0)
	{	readcount = (len >= bufferlen) ? bufferlen : len ;
		count = g72x_read_block (psf, pg72x, sptr, readcount) ;

		for (k = 0 ; k < readcount ; k++)
			ptr [total + k] = sptr [k] << 16 ;

		total += count ;
		len -= readcount ;
		if (count != readcount)
			break ;
		} ;

	return total ;
} /* g72x_read_i */

static sf_count_t
g72x_read_f (SF_PRIVATE *psf, float *ptr, sf_count_t len)
{	G72x_PRIVATE *pg72x ;
	short		*sptr ;
	int			k, bufferlen, readcount = 0, count ;
	sf_count_t	total = 0 ;
	float 		normfact ;

	if (psf->codec_data == NULL)
		return 0 ;
	pg72x = (G72x_PRIVATE*) psf->codec_data ;

	normfact = (psf->norm_float == SF_TRUE) ? 1.0 / ((float) 0x8000) : 1.0 ;

	sptr = psf->u.sbuf ;
	bufferlen = SF_BUFFER_LEN / sizeof (short) ;
	while (len > 0)
	{	readcount = (len >= bufferlen) ? bufferlen : len ;
		count = g72x_read_block (psf, pg72x, sptr, readcount) ;
		for (k = 0 ; k < readcount ; k++)
			ptr [total + k] = normfact * sptr [k] ;

		total += count ;
		len -= readcount ;
		if (count != readcount)
			break ;
		} ;

	return total ;
} /* g72x_read_f */

static sf_count_t
g72x_read_d (SF_PRIVATE *psf, double *ptr, sf_count_t len)
{	G72x_PRIVATE *pg72x ;
	short		*sptr ;
	int			k, bufferlen, readcount = 0, count ;
	sf_count_t	total = 0 ;
	double		normfact ;

	if (psf->codec_data == NULL)
		return 0 ;
	pg72x = (G72x_PRIVATE*) psf->codec_data ;

	normfact = (psf->norm_double == SF_TRUE) ? 1.0 / ((double) 0x8000) : 1.0 ;

	sptr = psf->u.sbuf ;
	bufferlen = SF_BUFFER_LEN / sizeof (short) ;
	while (len > 0)
	{	readcount = (len >= bufferlen) ? bufferlen : len ;
		count = g72x_read_block (psf, pg72x, sptr, readcount) ;
		for (k = 0 ; k < readcount ; k++)
			ptr [total + k] = normfact * (double) (sptr [k]) ;

		total += count ;
		len -= readcount ;
		if (count != readcount)
			break ;
		} ;

	return total ;
} /* g72x_read_d */

static sf_count_t
g72x_seek (SF_PRIVATE *psf, int UNUSED (mode), sf_count_t UNUSED (offset))
{
	psf_log_printf (psf, "seek unsupported\n") ;

	/*	No simple solution. To do properly, would need to seek
	**	to start of file and decode everything up to seek position.
	**	Maybe implement SEEK_SET to 0 only?
	*/
	return 0 ;

/*
**		G72x_PRIVATE	*pg72x ;
**		int			newblock, newsample, sample_curr ;
**
**		if (psf->codec_data == NULL)
**			return 0 ;
**		pg72x = (G72x_PRIVATE*) psf->codec_data ;
**
**		if (! (psf->datalength && psf->dataoffset))
**		{	psf->error = SFE_BAD_SEEK ;
**			return	PSF_SEEK_ERROR ;
**			} ;
**
**		sample_curr = (8 * psf->datalength) / G721_32_BITS_PER_SAMPLE ;
**
**		switch (whence)
**		{	case SEEK_SET :
**					if (offset < 0 || offset > sample_curr)
**					{	psf->error = SFE_BAD_SEEK ;
**						return	PSF_SEEK_ERROR ;
**						} ;
**					newblock  = offset / pg72x->samplesperblock ;
**					newsample = offset % pg72x->samplesperblock ;
**					break ;
**
**			case SEEK_CUR :
**					if (psf->current + offset < 0 || psf->current + offset > sample_curr)
**					{	psf->error = SFE_BAD_SEEK ;
**						return	PSF_SEEK_ERROR ;
**						} ;
**					newblock  = (8 * (psf->current + offset)) / pg72x->samplesperblock ;
**					newsample = (8 * (psf->current + offset)) % pg72x->samplesperblock ;
**					break ;
**
**			case SEEK_END :
**					if (offset > 0 || sample_curr + offset < 0)
**					{	psf->error = SFE_BAD_SEEK ;
**						return	PSF_SEEK_ERROR ;
**						} ;
**					newblock  = (sample_curr + offset) / pg72x->samplesperblock ;
**					newsample = (sample_curr + offset) % pg72x->samplesperblock ;
**					break ;
**
**			default :
**					psf->error = SFE_BAD_SEEK ;
**					return	PSF_SEEK_ERROR ;
**			} ;
**
**		if (psf->mode == SFM_READ)
**		{	psf_fseek (psf, psf->dataoffset + newblock * pg72x->blocksize, SEEK_SET) ;
**			pg72x->block_curr  = newblock ;
**			psf_g72x_decode_block (psf, pg72x) ;
**			pg72x->sample_curr = newsample ;
**			}
**		else
**		{	/+* What to do about write??? *+/
**			psf->error = SFE_BAD_SEEK ;
**			return	PSF_SEEK_ERROR ;
**			} ;
**
**		psf->current = newblock * pg72x->samplesperblock + newsample ;
**		return psf->current ;
**
*/
} /* g72x_seek */

/*==========================================================================================
** G72x Write Functions.
*/

static int
psf_g72x_encode_block (SF_PRIVATE *psf, G72x_PRIVATE *pg72x)
{	int k ;

	/* Encode the samples. */
	g72x_encode_block (pg72x->private, pg72x->samples, pg72x->block) ;

	/* Write the block to disk. */
	if ((k = psf_fwrite (pg72x->block, 1, pg72x->blocksize, psf)) != pg72x->blocksize)
		psf_log_printf (psf, "*** Warning : short write (%d != %d).\n", k, pg72x->blocksize) ;

	pg72x->sample_curr = 0 ;
	pg72x->block_curr ++ ;

	/* Set samples to zero for next block. */
	memset (pg72x->samples, 0, G72x_BLOCK_SIZE * sizeof (short)) ;

	return 1 ;
} /* psf_g72x_encode_block */

static int
g72x_write_block (SF_PRIVATE *psf, G72x_PRIVATE *pg72x, const short *ptr, int len)
{	int	count, total = 0, indx = 0 ;

	while (indx < len)
	{	count = pg72x->samplesperblock - pg72x->sample_curr ;

		if (count > len - indx)
			count = len - indx ;

		memcpy (&(pg72x->samples [pg72x->sample_curr]), &(ptr [indx]), count * sizeof (short)) ;
		indx += count ;
		pg72x->sample_curr += count ;
		total = indx ;

		if (pg72x->sample_curr >= pg72x->samplesperblock)
			psf_g72x_encode_block (psf, pg72x) ;
		} ;

	return total ;
} /* g72x_write_block */

static sf_count_t
g72x_write_s (SF_PRIVATE *psf, const short *ptr, sf_count_t len)
{	G72x_PRIVATE 	*pg72x ;
	int			writecount, count ;
	sf_count_t	total = 0 ;

	if (psf->codec_data == NULL)
		return 0 ;
	pg72x = (G72x_PRIVATE*) psf->codec_data ;

	while (len > 0)
	{	writecount = (len > 0x10000000) ? 0x10000000 : (int) len ;

		count = g72x_write_block (psf, pg72x, ptr, writecount) ;

		total += count ;
		len -= count ;
		if (count != writecount)
			break ;
		} ;

	return total ;
} /* g72x_write_s */

static sf_count_t
g72x_write_i (SF_PRIVATE *psf, const int *ptr, sf_count_t len)
{	G72x_PRIVATE *pg72x ;
	short		*sptr ;
	int			k, bufferlen, writecount = 0, count ;
	sf_count_t	total = 0 ;

	if (psf->codec_data == NULL)
		return 0 ;
	pg72x = (G72x_PRIVATE*) psf->codec_data ;

	sptr = psf->u.sbuf ;
	bufferlen = ((SF_BUFFER_LEN / psf->blockwidth) * psf->blockwidth) / sizeof (short) ;
	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : len ;
		for (k = 0 ; k < writecount ; k++)
			sptr [k] = ptr [total + k] >> 16 ;
		count = g72x_write_block (psf, pg72x, sptr, writecount) ;

		total += count ;
		len -= writecount ;
		if (count != writecount)
			break ;
		} ;
	return total ;
} /* g72x_write_i */

static sf_count_t
g72x_write_f (SF_PRIVATE *psf, const float *ptr, sf_count_t len)
{	G72x_PRIVATE *pg72x ;
	short		*sptr ;
	int			k, bufferlen, writecount = 0, count ;
	sf_count_t	total = 0 ;
	float		normfact ;

	if (psf->codec_data == NULL)
		return 0 ;
	pg72x = (G72x_PRIVATE*) psf->codec_data ;

	normfact = (psf->norm_float == SF_TRUE) ? (1.0 * 0x8000) : 1.0 ;

	sptr = psf->u.sbuf ;
	bufferlen = ((SF_BUFFER_LEN / psf->blockwidth) * psf->blockwidth) / sizeof (short) ;
	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : len ;
		for (k = 0 ; k < writecount ; k++)
			sptr [k] = lrintf (normfact * ptr [total + k]) ;
		count = g72x_write_block (psf, pg72x, sptr, writecount) ;

		total += count ;
		len -= writecount ;
		if (count != writecount)
			break ;
		} ;

	return total ;
} /* g72x_write_f */

static sf_count_t
g72x_write_d (SF_PRIVATE *psf, const double *ptr, sf_count_t len)
{	G72x_PRIVATE *pg72x ;
	short		*sptr ;
	int			k, bufferlen, writecount = 0, count ;
	sf_count_t	total = 0 ;
	double		normfact ;

	if (psf->codec_data == NULL)
		return 0 ;
	pg72x = (G72x_PRIVATE*) psf->codec_data ;

	normfact = (psf->norm_double == SF_TRUE) ? (1.0 * 0x8000) : 1.0 ;

	sptr = psf->u.sbuf ;
	bufferlen = ((SF_BUFFER_LEN / psf->blockwidth) * psf->blockwidth) / sizeof (short) ;
	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : len ;
		for (k = 0 ; k < writecount ; k++)
			sptr [k] = lrint (normfact * ptr [total + k]) ;
		count = g72x_write_block (psf, pg72x, sptr, writecount) ;

		total += count ;
		len -= writecount ;
		if (count != writecount)
			break ;
		} ;

	return total ;
} /* g72x_write_d */

static int
g72x_close (SF_PRIVATE *psf)
{	G72x_PRIVATE *pg72x ;

	pg72x = (G72x_PRIVATE*) psf->codec_data ;

	if (psf->mode == SFM_WRITE)
	{	/*	If a block has been partially assembled, write it out
		**	as the final block.
		*/

		if (pg72x->sample_curr && pg72x->sample_curr < G72x_BLOCK_SIZE)
			psf_g72x_encode_block (psf, pg72x) ;

		if (psf->write_header)
			psf->write_header (psf, SF_FALSE) ;
		} ;

	/* Only free the pointer allocated by g72x_(reader|writer)_init. */
	free (pg72x->private) ;

	return 0 ;
} /* g72x_close */

