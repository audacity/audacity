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

#include "sfconfig.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "sndfile.h"
#include "sfendian.h"
#include "common.h"
#include "wav_w64.h"
#include "GSM610/gsm.h"

#define	GSM610_BLOCKSIZE		33
#define	GSM610_SAMPLES			160

typedef struct gsm610_tag
{	int				blocks ;
	int				blockcount, samplecount ;
	int				samplesperblock, blocksize ;

	int				(*decode_block)	(SF_PRIVATE *psf, struct gsm610_tag *pgsm610) ;
	int				(*encode_block)	(SF_PRIVATE *psf, struct gsm610_tag *pgsm610) ;

	short			samples [WAV_W64_GSM610_SAMPLES] ;
	unsigned char	block [WAV_W64_GSM610_BLOCKSIZE] ;

	/* Damn I hate typedef-ed pointers; yes, gsm is a pointer type. */
	gsm				gsm_data ;
} GSM610_PRIVATE ;

static sf_count_t	gsm610_read_s	(SF_PRIVATE *psf, short *ptr, sf_count_t len) ;
static sf_count_t	gsm610_read_i	(SF_PRIVATE *psf, int *ptr, sf_count_t len) ;
static sf_count_t	gsm610_read_f	(SF_PRIVATE *psf, float *ptr, sf_count_t len) ;
static sf_count_t	gsm610_read_d	(SF_PRIVATE *psf, double *ptr, sf_count_t len) ;

static sf_count_t	gsm610_write_s	(SF_PRIVATE *psf, const short *ptr, sf_count_t len) ;
static sf_count_t	gsm610_write_i	(SF_PRIVATE *psf, const int *ptr, sf_count_t len) ;
static sf_count_t	gsm610_write_f	(SF_PRIVATE *psf, const float *ptr, sf_count_t len) ;
static sf_count_t	gsm610_write_d	(SF_PRIVATE *psf, const double *ptr, sf_count_t len) ;

static	int gsm610_read_block	(SF_PRIVATE *psf, GSM610_PRIVATE *pgsm610, short *ptr, int len) ;
static	int gsm610_write_block	(SF_PRIVATE *psf, GSM610_PRIVATE *pgsm610, const short *ptr, int len) ;

static	int	gsm610_decode_block	(SF_PRIVATE *psf, GSM610_PRIVATE *pgsm610) ;
static	int	gsm610_encode_block	(SF_PRIVATE *psf, GSM610_PRIVATE *pgsm610) ;

static	int	gsm610_wav_decode_block	(SF_PRIVATE *psf, GSM610_PRIVATE *pgsm610) ;
static	int	gsm610_wav_encode_block	(SF_PRIVATE *psf, GSM610_PRIVATE *pgsm610) ;

static sf_count_t	gsm610_seek	(SF_PRIVATE *psf, int mode, sf_count_t offset) ;

static int	gsm610_close	(SF_PRIVATE *psf) ;

/*============================================================================================
** WAV GSM610 initialisation function.
*/

int
gsm610_init	(SF_PRIVATE *psf)
{	GSM610_PRIVATE	*pgsm610 ;
	int		true_flag = 1 ;

	if (psf->codec_data != NULL)
	{	psf_log_printf (psf, "*** psf->codec_data is not NULL.\n") ;
		return SFE_INTERNAL ;
		} ;

	if (psf->file.mode == SFM_RDWR)
		return SFE_BAD_MODE_RW ;

	psf->sf.seekable = SF_FALSE ;

	if ((pgsm610 = calloc (1, sizeof (GSM610_PRIVATE))) == NULL)
		return SFE_MALLOC_FAILED ;

	psf->codec_data = pgsm610 ;

	memset (pgsm610, 0, sizeof (GSM610_PRIVATE)) ;

/*============================================================

Need separate gsm_data structs for encode and decode.

============================================================*/

	if ((pgsm610->gsm_data = gsm_create ()) == NULL)
		return SFE_MALLOC_FAILED ;

	switch (SF_CONTAINER (psf->sf.format))
	{	case SF_FORMAT_WAV :
		case SF_FORMAT_WAVEX :
		case SF_FORMAT_W64 :
			gsm_option (pgsm610->gsm_data, GSM_OPT_WAV49, &true_flag) ;

			pgsm610->encode_block = gsm610_wav_encode_block ;
			pgsm610->decode_block = gsm610_wav_decode_block ;

			pgsm610->samplesperblock = WAV_W64_GSM610_SAMPLES ;
			pgsm610->blocksize = WAV_W64_GSM610_BLOCKSIZE ;
			break ;

		case SF_FORMAT_AIFF :
		case SF_FORMAT_RAW :
			pgsm610->encode_block = gsm610_encode_block ;
			pgsm610->decode_block = gsm610_decode_block ;

			pgsm610->samplesperblock = GSM610_SAMPLES ;
			pgsm610->blocksize = GSM610_BLOCKSIZE ;
			break ;

		default :
			return SFE_INTERNAL ;
			break ;
		} ;

	if (psf->file.mode == SFM_READ)
	{	if (psf->datalength % pgsm610->blocksize == 0)
			pgsm610->blocks = psf->datalength / pgsm610->blocksize ;
		else if (psf->datalength % pgsm610->blocksize == 1 && pgsm610->blocksize == GSM610_BLOCKSIZE)
		{	/*
			**	Weird AIFF specific case.
			**	AIFF chunks must be at an even offset from the start of file and
			**	GSM610_BLOCKSIZE is odd which can result in an odd length SSND
			**	chunk. The SSND chunk then gets padded on write which means that
			**	when it is read the datalength is too big by 1.
			*/
			pgsm610->blocks = psf->datalength / pgsm610->blocksize ;
			}
		else
		{	psf_log_printf (psf, "*** Warning : data chunk seems to be truncated.\n") ;
			pgsm610->blocks = psf->datalength / pgsm610->blocksize + 1 ;
			} ;

		psf->sf.frames = pgsm610->samplesperblock * pgsm610->blocks ;

		psf_fseek (psf, psf->dataoffset, SEEK_SET) ;

		pgsm610->decode_block (psf, pgsm610) ;	/* Read first block. */

		psf->read_short		= gsm610_read_s ;
		psf->read_int		= gsm610_read_i ;
		psf->read_float		= gsm610_read_f ;
		psf->read_double	= gsm610_read_d ;
		} ;

	if (psf->file.mode == SFM_WRITE)
	{	pgsm610->blockcount = 0 ;
		pgsm610->samplecount = 0 ;

		psf->write_short	= gsm610_write_s ;
		psf->write_int		= gsm610_write_i ;
		psf->write_float	= gsm610_write_f ;
		psf->write_double	= gsm610_write_d ;
		} ;

	psf->codec_close = gsm610_close ;

	psf->seek = gsm610_seek ;

	psf->filelength = psf_get_filelen (psf) ;
	psf->datalength = psf->filelength - psf->dataoffset ;

	return 0 ;
} /* gsm610_init */

/*============================================================================================
** GSM 6.10 Read Functions.
*/

static int
gsm610_wav_decode_block	(SF_PRIVATE *psf, GSM610_PRIVATE *pgsm610)
{	int	k ;

	pgsm610->blockcount ++ ;
	pgsm610->samplecount = 0 ;

	if (pgsm610->blockcount > pgsm610->blocks)
	{	memset (pgsm610->samples, 0, WAV_W64_GSM610_SAMPLES * sizeof (short)) ;
		return 1 ;
		} ;

	if ((k = psf_fread (pgsm610->block, 1, WAV_W64_GSM610_BLOCKSIZE, psf)) != WAV_W64_GSM610_BLOCKSIZE)
		psf_log_printf (psf, "*** Warning : short read (%d != %d).\n", k, WAV_W64_GSM610_BLOCKSIZE) ;

	if (gsm_decode (pgsm610->gsm_data, pgsm610->block, pgsm610->samples) < 0)
	{	psf_log_printf (psf, "Error from WAV gsm_decode() on frame : %d\n", pgsm610->blockcount) ;
		return 0 ;
		} ;

	if (gsm_decode (pgsm610->gsm_data, pgsm610->block + (WAV_W64_GSM610_BLOCKSIZE + 1) / 2, pgsm610->samples + WAV_W64_GSM610_SAMPLES / 2) < 0)
	{	psf_log_printf (psf, "Error from WAV gsm_decode() on frame : %d.5\n", pgsm610->blockcount) ;
		return 0 ;
		} ;

	return 1 ;
} /* gsm610_wav_decode_block */

static int
gsm610_decode_block	(SF_PRIVATE *psf, GSM610_PRIVATE *pgsm610)
{	int	k ;

	pgsm610->blockcount ++ ;
	pgsm610->samplecount = 0 ;

	if (pgsm610->blockcount > pgsm610->blocks)
	{	memset (pgsm610->samples, 0, GSM610_SAMPLES * sizeof (short)) ;
		return 1 ;
		} ;

	if ((k = psf_fread (pgsm610->block, 1, GSM610_BLOCKSIZE, psf)) != GSM610_BLOCKSIZE)
		psf_log_printf (psf, "*** Warning : short read (%d != %d).\n", k, GSM610_BLOCKSIZE) ;

	if (gsm_decode (pgsm610->gsm_data, pgsm610->block, pgsm610->samples) < 0)
	{	psf_log_printf (psf, "Error from standard gsm_decode() on frame : %d\n", pgsm610->blockcount) ;
		return 0 ;
		} ;

	return 1 ;
} /* gsm610_decode_block */

static int
gsm610_read_block	(SF_PRIVATE *psf, GSM610_PRIVATE *pgsm610, short *ptr, int len)
{	int	count, total = 0, indx = 0 ;

	while (indx < len)
	{	if (pgsm610->blockcount >= pgsm610->blocks && pgsm610->samplecount >= pgsm610->samplesperblock)
		{	memset (&(ptr [indx]), 0, (len - indx) * sizeof (short)) ;
			return total ;
			} ;

		if (pgsm610->samplecount >= pgsm610->samplesperblock)
			pgsm610->decode_block (psf, pgsm610) ;

		count = pgsm610->samplesperblock - pgsm610->samplecount ;
		count = (len - indx > count) ? count : len - indx ;

		memcpy (&(ptr [indx]), &(pgsm610->samples [pgsm610->samplecount]), count * sizeof (short)) ;
		indx += count ;
		pgsm610->samplecount += count ;
		total = indx ;
		} ;

	return total ;
} /* gsm610_read_block */

static sf_count_t
gsm610_read_s	(SF_PRIVATE *psf, short *ptr, sf_count_t len)
{	GSM610_PRIVATE 	*pgsm610 ;
	int			readcount, count ;
	sf_count_t	total = 0 ;

	if (psf->codec_data == NULL)
		return 0 ;
	pgsm610 = (GSM610_PRIVATE*) psf->codec_data ;

	while (len > 0)
	{	readcount = (len > 0x10000000) ? 0x1000000 : (int) len ;

		count = gsm610_read_block (psf, pgsm610, ptr, readcount) ;

		total += count ;
		len -= count ;

		if (count != readcount)
			break ;
		} ;

	return total ;
} /* gsm610_read_s */

static sf_count_t
gsm610_read_i	(SF_PRIVATE *psf, int *ptr, sf_count_t len)
{	GSM610_PRIVATE *pgsm610 ;
	short		*sptr ;
	int			k, bufferlen, readcount = 0, count ;
	sf_count_t	total = 0 ;

	if (psf->codec_data == NULL)
		return 0 ;
	pgsm610 = (GSM610_PRIVATE*) psf->codec_data ;

	sptr = psf->u.sbuf ;
	bufferlen = ARRAY_LEN (psf->u.sbuf) ;
	while (len > 0)
	{	readcount = (len >= bufferlen) ? bufferlen : len ;
		count = gsm610_read_block (psf, pgsm610, sptr, readcount) ;
		for (k = 0 ; k < readcount ; k++)
			ptr [total + k] = sptr [k] << 16 ;

		total += count ;
		len -= readcount ;
		} ;
	return total ;
} /* gsm610_read_i */

static sf_count_t
gsm610_read_f	(SF_PRIVATE *psf, float *ptr, sf_count_t len)
{	GSM610_PRIVATE *pgsm610 ;
	short		*sptr ;
	int			k, bufferlen, readcount = 0, count ;
	sf_count_t	total = 0 ;
	float		normfact ;

	if (psf->codec_data == NULL)
		return 0 ;
	pgsm610 = (GSM610_PRIVATE*) psf->codec_data ;

	normfact = (psf->norm_float == SF_TRUE) ? 1.0 / ((float) 0x8000) : 1.0 ;

	sptr = psf->u.sbuf ;
	bufferlen = ARRAY_LEN (psf->u.sbuf) ;
	while (len > 0)
	{	readcount = (len >= bufferlen) ? bufferlen : len ;
		count = gsm610_read_block (psf, pgsm610, sptr, readcount) ;
		for (k = 0 ; k < readcount ; k++)
			ptr [total + k] = normfact * sptr [k] ;

		total += count ;
		len -= readcount ;
		} ;
	return total ;
} /* gsm610_read_f */

static sf_count_t
gsm610_read_d	(SF_PRIVATE *psf, double *ptr, sf_count_t len)
{	GSM610_PRIVATE *pgsm610 ;
	short		*sptr ;
	int			k, bufferlen, readcount = 0, count ;
	sf_count_t	total = 0 ;
	double		normfact ;

	normfact = (psf->norm_double == SF_TRUE) ? 1.0 / ((double) 0x8000) : 1.0 ;

	if (psf->codec_data == NULL)
		return 0 ;
	pgsm610 = (GSM610_PRIVATE*) psf->codec_data ;

	sptr = psf->u.sbuf ;
	bufferlen = ARRAY_LEN (psf->u.sbuf) ;
	while (len > 0)
	{	readcount = (len >= bufferlen) ? bufferlen : len ;
		count = gsm610_read_block (psf, pgsm610, sptr, readcount) ;
		for (k = 0 ; k < readcount ; k++)
			ptr [total + k] = normfact * sptr [k] ;

		total += count ;
		len -= readcount ;
		} ;
	return total ;
} /* gsm610_read_d */

static sf_count_t
gsm610_seek	(SF_PRIVATE *psf, int UNUSED (mode), sf_count_t offset)
{	GSM610_PRIVATE *pgsm610 ;
	int			newblock, newsample ;

	if (psf->codec_data == NULL)
		return 0 ;
	pgsm610 = (GSM610_PRIVATE*) psf->codec_data ;

	if (psf->dataoffset < 0)
	{	psf->error = SFE_BAD_SEEK ;
		return	PSF_SEEK_ERROR ;
		} ;

	if (offset == 0)
	{	int true_flag = 1 ;

		psf_fseek (psf, psf->dataoffset, SEEK_SET) ;
		pgsm610->blockcount = 0 ;

		gsm_init (pgsm610->gsm_data) ;
		if ((SF_CONTAINER (psf->sf.format)) == SF_FORMAT_WAV ||
				(SF_CONTAINER (psf->sf.format)) == SF_FORMAT_W64)
			gsm_option (pgsm610->gsm_data, GSM_OPT_WAV49, &true_flag) ;

		pgsm610->decode_block (psf, pgsm610) ;
		pgsm610->samplecount = 0 ;
		return 0 ;
		} ;

	if (offset < 0 || offset > pgsm610->blocks * pgsm610->samplesperblock)
	{	psf->error = SFE_BAD_SEEK ;
		return	PSF_SEEK_ERROR ;
		} ;

	newblock	= offset / pgsm610->samplesperblock ;
	newsample	= offset % pgsm610->samplesperblock ;

	if (psf->file.mode == SFM_READ)
	{	if (psf->read_current != newblock * pgsm610->samplesperblock + newsample)
		{	psf_fseek (psf, psf->dataoffset + newblock * pgsm610->samplesperblock, SEEK_SET) ;
			pgsm610->blockcount = newblock ;
			pgsm610->decode_block (psf, pgsm610) ;
			pgsm610->samplecount = newsample ;
			} ;

		return newblock * pgsm610->samplesperblock + newsample ;
		} ;

	/* What to do about write??? */
	psf->error = SFE_BAD_SEEK ;
	return	PSF_SEEK_ERROR ;
} /* gsm610_seek */

/*==========================================================================================
** GSM 6.10 Write Functions.
*/

static int
gsm610_encode_block	(SF_PRIVATE *psf, GSM610_PRIVATE *pgsm610)
{	int k ;

	/* Encode the samples. */
	gsm_encode (pgsm610->gsm_data, pgsm610->samples, pgsm610->block) ;

	/* Write the block to disk. */
	if ((k = psf_fwrite (pgsm610->block, 1, GSM610_BLOCKSIZE, psf)) != GSM610_BLOCKSIZE)
		psf_log_printf (psf, "*** Warning : short write (%d != %d).\n", k, GSM610_BLOCKSIZE) ;

	pgsm610->samplecount = 0 ;
	pgsm610->blockcount ++ ;

	/* Set samples to zero for next block. */
	memset (pgsm610->samples, 0, WAV_W64_GSM610_SAMPLES * sizeof (short)) ;

	return 1 ;
} /* gsm610_encode_block */

static int
gsm610_wav_encode_block	(SF_PRIVATE *psf, GSM610_PRIVATE *pgsm610)
{	int k ;

	/* Encode the samples. */
	gsm_encode (pgsm610->gsm_data, pgsm610->samples, pgsm610->block) ;
	gsm_encode (pgsm610->gsm_data, pgsm610->samples+WAV_W64_GSM610_SAMPLES/2, pgsm610->block+WAV_W64_GSM610_BLOCKSIZE/2) ;

	/* Write the block to disk. */
	if ((k = psf_fwrite (pgsm610->block, 1, WAV_W64_GSM610_BLOCKSIZE, psf)) != WAV_W64_GSM610_BLOCKSIZE)
		psf_log_printf (psf, "*** Warning : short write (%d != %d).\n", k, WAV_W64_GSM610_BLOCKSIZE) ;

	pgsm610->samplecount = 0 ;
	pgsm610->blockcount ++ ;

	/* Set samples to zero for next block. */
	memset (pgsm610->samples, 0, WAV_W64_GSM610_SAMPLES * sizeof (short)) ;

	return 1 ;
} /* gsm610_wav_encode_block */

static int
gsm610_write_block	(SF_PRIVATE *psf, GSM610_PRIVATE *pgsm610, const short *ptr, int len)
{	int		count, total = 0, indx = 0 ;

	while (indx < len)
	{	count = pgsm610->samplesperblock - pgsm610->samplecount ;

		if (count > len - indx)
			count = len - indx ;

		memcpy (&(pgsm610->samples [pgsm610->samplecount]), &(ptr [indx]), count * sizeof (short)) ;
		indx += count ;
		pgsm610->samplecount += count ;
		total = indx ;

		if (pgsm610->samplecount >= pgsm610->samplesperblock)
			pgsm610->encode_block (psf, pgsm610) ;
		} ;

	return total ;
} /* gsm610_write_block */

static sf_count_t
gsm610_write_s	(SF_PRIVATE *psf, const short *ptr, sf_count_t len)
{	GSM610_PRIVATE 	*pgsm610 ;
	int			writecount, count ;
	sf_count_t	total = 0 ;

	if (psf->codec_data == NULL)
		return 0 ;
	pgsm610 = (GSM610_PRIVATE*) psf->codec_data ;

	while (len > 0)
	{	writecount = (len > 0x10000000) ? 0x10000000 : (int) len ;

		count = gsm610_write_block (psf, pgsm610, ptr, writecount) ;

		total += count ;
		len -= count ;

		if (count != writecount)
			break ;
		} ;

	return total ;
} /* gsm610_write_s */

static sf_count_t
gsm610_write_i	(SF_PRIVATE *psf, const int *ptr, sf_count_t len)
{	GSM610_PRIVATE *pgsm610 ;
	short		*sptr ;
	int			k, bufferlen, writecount = 0, count ;
	sf_count_t	total = 0 ;

	if (psf->codec_data == NULL)
		return 0 ;
	pgsm610 = (GSM610_PRIVATE*) psf->codec_data ;

	sptr = psf->u.sbuf ;
	bufferlen = ARRAY_LEN (psf->u.sbuf) ;
	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : len ;
		for (k = 0 ; k < writecount ; k++)
			sptr [k] = ptr [total + k] >> 16 ;
		count = gsm610_write_block (psf, pgsm610, sptr, writecount) ;

		total += count ;
		len -= writecount ;
		} ;
	return total ;
} /* gsm610_write_i */

static sf_count_t
gsm610_write_f	(SF_PRIVATE *psf, const float *ptr, sf_count_t len)
{	GSM610_PRIVATE *pgsm610 ;
	short		*sptr ;
	int			k, bufferlen, writecount = 0, count ;
	sf_count_t	total = 0 ;
	float		normfact ;

	if (psf->codec_data == NULL)
		return 0 ;
	pgsm610 = (GSM610_PRIVATE*) psf->codec_data ;

	normfact = (psf->norm_float == SF_TRUE) ? (1.0 * 0x7FFF) : 1.0 ;

	sptr = psf->u.sbuf ;
	bufferlen = ARRAY_LEN (psf->u.sbuf) ;
	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : len ;
		for (k = 0 ; k < writecount ; k++)
			sptr [k] = lrintf (normfact * ptr [total + k]) ;
		count = gsm610_write_block (psf, pgsm610, sptr, writecount) ;

		total += count ;
		len -= writecount ;
		} ;
	return total ;
} /* gsm610_write_f */

static sf_count_t
gsm610_write_d	(SF_PRIVATE *psf, const double *ptr, sf_count_t len)
{	GSM610_PRIVATE *pgsm610 ;
	short		*sptr ;
	int			k, bufferlen, writecount = 0, count ;
	sf_count_t	total = 0 ;
	double		normfact ;

	if (psf->codec_data == NULL)
		return 0 ;
	pgsm610 = (GSM610_PRIVATE*) psf->codec_data ;

	normfact = (psf->norm_double == SF_TRUE) ? (1.0 * 0x7FFF) : 1.0 ;

	sptr = psf->u.sbuf ;
	bufferlen = ARRAY_LEN (psf->u.sbuf) ;
	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : len ;
		for (k = 0 ; k < writecount ; k++)
			sptr [k] = lrint (normfact * ptr [total + k]) ;
		count = gsm610_write_block (psf, pgsm610, sptr, writecount) ;

		total += count ;
		len -= writecount ;
		} ;
	return total ;
} /* gsm610_write_d */

static int
gsm610_close	(SF_PRIVATE *psf)
{	GSM610_PRIVATE *pgsm610 ;

	if (psf->codec_data == NULL)
		return 0 ;

	pgsm610 = (GSM610_PRIVATE*) psf->codec_data ;

	if (psf->file.mode == SFM_WRITE)
	{	/*	If a block has been partially assembled, write it out
		**	as the final block.
		*/

		if (pgsm610->samplecount && pgsm610->samplecount < pgsm610->samplesperblock)
			pgsm610->encode_block (psf, pgsm610) ;
		} ;

	if (pgsm610->gsm_data)
		gsm_destroy (pgsm610->gsm_data) ;

	return 0 ;
} /* gsm610_close */

