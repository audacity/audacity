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
#include	<math.h>

#include	"sndfile.h"
#include	"sfendian.h"
#include	"common.h"
#include	"wav_w64.h"

/* These required here because we write the header in this file. */

#define RIFF_MARKER	(MAKE_MARKER ('R', 'I', 'F', 'F'))
#define WAVE_MARKER	(MAKE_MARKER ('W', 'A', 'V', 'E'))
#define fmt_MARKER	(MAKE_MARKER ('f', 'm', 't', ' '))
#define fact_MARKER	(MAKE_MARKER ('f', 'a', 'c', 't'))
#define data_MARKER	(MAKE_MARKER ('d', 'a', 't', 'a'))

#define WAVE_FORMAT_MS_ADPCM	0x0002

typedef struct
{	int				channels, blocksize, samplesperblock, blocks, dataremaining ;
	int				blockcount ;
	sf_count_t		samplecount ;
	short			*samples ;
	unsigned char	*block ;
#if HAVE_FLEXIBLE_ARRAY
	short			dummydata [] ; /* ISO C99 struct flexible array. */
#else
	short			dummydata [0] ; /* This is a hack an might not work. */
#endif
} MSADPCM_PRIVATE ;

/*============================================================================================
** MS ADPCM static data and functions.
*/

static int AdaptationTable [] =
{	230, 230, 230, 230, 307, 409, 512, 614,
	768, 614, 512, 409, 307, 230, 230, 230
} ;

/* TODO : The first 7 coef's are are always hardcode and must
   appear in the actual WAVE file.  They should be read in
   in case a sound program added extras to the list. */

static int AdaptCoeff1 [MSADPCM_ADAPT_COEFF_COUNT] =
{	256, 512, 0, 192, 240, 460, 392
} ;

static int AdaptCoeff2 [MSADPCM_ADAPT_COEFF_COUNT] =
{	0, -256, 0, 64, 0, -208, -232
} ;

/*============================================================================================
**	MS ADPCM Block Layout.
**	======================
**	Block is usually 256, 512 or 1024 bytes depending on sample rate.
**	For a mono file, the block is laid out as follows:
**		byte	purpose
**		0		block predictor [0..6]
**		1,2		initial idelta (positive)
**		3,4		sample 1
**		5,6		sample 0
**		7..n	packed bytecodes
**
**	For a stereo file, the block is laid out as follows:
**		byte	purpose
**		0		block predictor [0..6] for left channel
**		1		block predictor [0..6] for right channel
**		2,3		initial idelta (positive) for left channel
**		4,5		initial idelta (positive) for right channel
**		6,7		sample 1 for left channel
**		8,9		sample 1 for right channel
**		10,11	sample 0 for left channel
**		12,13	sample 0 for right channel
**		14..n	packed bytecodes
*/

/*============================================================================================
** Static functions.
*/

static	int	msadpcm_decode_block	(SF_PRIVATE *psf, MSADPCM_PRIVATE *pms) ;
static sf_count_t msadpcm_read_block	(SF_PRIVATE *psf, MSADPCM_PRIVATE *pms, short *ptr, int len) ;

static	int	msadpcm_encode_block	(SF_PRIVATE *psf, MSADPCM_PRIVATE *pms) ;
static sf_count_t msadpcm_write_block	(SF_PRIVATE *psf, MSADPCM_PRIVATE *pms, const short *ptr, int len) ;

static sf_count_t	msadpcm_read_s	(SF_PRIVATE *psf, short *ptr, sf_count_t len) ;
static sf_count_t	msadpcm_read_i	(SF_PRIVATE *psf, int *ptr, sf_count_t len) ;
static sf_count_t	msadpcm_read_f	(SF_PRIVATE *psf, float *ptr, sf_count_t len) ;
static sf_count_t	msadpcm_read_d	(SF_PRIVATE *psf, double *ptr, sf_count_t len) ;

static sf_count_t	msadpcm_write_s	(SF_PRIVATE *psf, const short *ptr, sf_count_t len) ;
static sf_count_t	msadpcm_write_i	(SF_PRIVATE *psf, const int *ptr, sf_count_t len) ;
static sf_count_t	msadpcm_write_f	(SF_PRIVATE *psf, const float *ptr, sf_count_t len) ;
static sf_count_t	msadpcm_write_d	(SF_PRIVATE *psf, const double *ptr, sf_count_t len) ;

static sf_count_t msadpcm_seek	(SF_PRIVATE *psf, int mode, sf_count_t offset) ;
static	int	msadpcm_close	(SF_PRIVATE *psf) ;

static	void	choose_predictor (unsigned int channels, short *data, int *bpred, int *idelta) ;

/*============================================================================================
** MS ADPCM Read Functions.
*/

int
wav_w64_msadpcm_init	(SF_PRIVATE *psf, int blockalign, int samplesperblock)
{	MSADPCM_PRIVATE	*pms ;
	unsigned int	pmssize ;
	int				count ;

	if (psf->codec_data != NULL)
	{	psf_log_printf (psf, "*** psf->codec_data is not NULL.\n") ;
		return SFE_INTERNAL ;
		} ;

	if (psf->mode == SFM_WRITE)
		samplesperblock = 2 + 2 * (blockalign - 7 * psf->sf.channels) / psf->sf.channels ;

	pmssize = sizeof (MSADPCM_PRIVATE) + blockalign + 3 * psf->sf.channels * samplesperblock ;

	if (! (psf->codec_data = malloc (pmssize)))
		return SFE_MALLOC_FAILED ;
	pms = (MSADPCM_PRIVATE*) psf->codec_data ;
	memset (pms, 0, pmssize) ;

	pms->samples	= pms->dummydata ;
	pms->block		= (unsigned char*) (pms->dummydata + psf->sf.channels * samplesperblock) ;

	pms->channels	= psf->sf.channels ;
	pms->blocksize	= blockalign ;
	pms->samplesperblock = samplesperblock ;

	if (pms->blocksize == 0)
	{	psf_log_printf (psf, "*** Error : pms->blocksize should not be zero.\n") ;
		return SFE_INTERNAL ;
		} ;

	if (psf->mode == SFM_READ)
	{	pms->dataremaining	 = psf->datalength ;

		if (psf->datalength % pms->blocksize)
			pms->blocks = psf->datalength / pms->blocksize + 1 ;
		else
			pms->blocks = psf->datalength / pms->blocksize ;

		count = 2 * (pms->blocksize - 6 * pms->channels) / pms->channels ;
		if (pms->samplesperblock != count)
		{	psf_log_printf (psf, "*** Error : samplesperblock should be %d.\n", count) ;
			return SFE_INTERNAL ;
			} ;

		psf->sf.frames = (psf->datalength / pms->blocksize) * pms->samplesperblock ;

		psf_log_printf (psf, " bpred   idelta\n") ;

		msadpcm_decode_block (psf, pms) ;

		psf->read_short		= msadpcm_read_s ;
		psf->read_int		= msadpcm_read_i ;
		psf->read_float		= msadpcm_read_f ;
		psf->read_double	= msadpcm_read_d ;
		} ;

	if (psf->mode == SFM_WRITE)
	{	pms->samples = pms->dummydata ;

		pms->samplecount = 0 ;

		psf->write_short	= msadpcm_write_s ;
		psf->write_int		= msadpcm_write_i ;
		psf->write_float	= msadpcm_write_f ;
		psf->write_double	= msadpcm_write_d ;
		} ;

	psf->codec_close = msadpcm_close ;
	psf->seek = msadpcm_seek ;

	return 0 ;
} /* wav_w64_msadpcm_init */

static int
msadpcm_decode_block	(SF_PRIVATE *psf, MSADPCM_PRIVATE *pms)
{	int		chan, k, blockindx, sampleindx ;
	short	bytecode, bpred [2], chan_idelta [2] ;

    int predict ;
    int current ;
    int idelta ;

	pms->blockcount ++ ;
	pms->samplecount = 0 ;

	if (pms->blockcount > pms->blocks)
	{	memset (pms->samples, 0, pms->samplesperblock * pms->channels) ;
		return 1 ;
		} ;

	if ((k = psf_fread (pms->block, 1, pms->blocksize, psf)) != pms->blocksize)
		psf_log_printf (psf, "*** Warning : short read (%d != %d).\n", k, pms->blocksize) ;

	/* Read and check the block header. */

	if (pms->channels == 1)
	{	bpred [0] = pms->block [0] ;

		if (bpred [0] >= 7)
			psf_log_printf (psf, "MS ADPCM synchronisation error (%d).\n", bpred [0]) ;

		chan_idelta [0] = pms->block [1] | (pms->block [2] << 8) ;
		chan_idelta [1] = 0 ;

		psf_log_printf (psf, "(%d) (%d)\n", bpred [0], chan_idelta [0]) ;

		pms->samples [1] = pms->block [3] | (pms->block [4] << 8) ;
		pms->samples [0] = pms->block [5] | (pms->block [6] << 8) ;
		blockindx = 7 ;
		}
	else
	{	bpred [0] = pms->block [0] ;
		bpred [1] = pms->block [1] ;

		if (bpred [0] >= 7 || bpred [1] >= 7)
			psf_log_printf (psf, "MS ADPCM synchronisation error (%d %d).\n", bpred [0], bpred [1]) ;

		chan_idelta [0] = pms->block [2] | (pms->block [3] << 8) ;
		chan_idelta [1] = pms->block [4] | (pms->block [5] << 8) ;

		psf_log_printf (psf, "(%d, %d) (%d, %d)\n", bpred [0], bpred [1], chan_idelta [0], chan_idelta [1]) ;

		pms->samples [2] = pms->block [6] | (pms->block [7] << 8) ;
		pms->samples [3] = pms->block [8] | (pms->block [9] << 8) ;

		pms->samples [0] = pms->block [10] | (pms->block [11] << 8) ;
		pms->samples [1] = pms->block [12] | (pms->block [13] << 8) ;

		blockindx = 14 ;
		} ;

	/*--------------------------------------------------------
	This was left over from a time when calculations were done
	as ints rather than shorts. Keep this around as a reminder
	in case I ever find a file which decodes incorrectly.

    if (chan_idelta [0] & 0x8000)
		chan_idelta [0] -= 0x10000 ;
    if (chan_idelta [1] & 0x8000)
		chan_idelta [1] -= 0x10000 ;
	--------------------------------------------------------*/

	/* Pull apart the packed 4 bit samples and store them in their
	** correct sample positions.
	*/

	sampleindx = 2 * pms->channels ;
	while (blockindx < pms->blocksize)
	{	bytecode = pms->block [blockindx++] ;
  		pms->samples [sampleindx++] = (bytecode >> 4) & 0x0F ;
		pms->samples [sampleindx++] = bytecode & 0x0F ;
		} ;

	/* Decode the encoded 4 bit samples. */

	for (k = 2 * pms->channels ; k < (pms->samplesperblock * pms->channels) ; k ++)
	{	chan = (pms->channels > 1) ? (k % 2) : 0 ;

		bytecode = pms->samples [k] & 0xF ;

		/* Compute next Adaptive Scale Factor (ASF) */
		idelta = chan_idelta [chan] ;
		chan_idelta [chan] = (AdaptationTable [bytecode] * idelta) >> 8 ;	/* => / 256 => FIXED_POINT_ADAPTATION_BASE == 256 */
		if (chan_idelta [chan] < 16)
			chan_idelta [chan] = 16 ;
		if (bytecode & 0x8)
			bytecode -= 0x10 ;

    	predict = ((pms->samples [k - pms->channels] * AdaptCoeff1 [bpred [chan]])
					+ (pms->samples [k - 2 * pms->channels] * AdaptCoeff2 [bpred [chan]])) >> 8 ; /* => / 256 => FIXED_POINT_COEFF_BASE == 256 */
		current = (bytecode * idelta) + predict ;

		if (current > 32767)
			current = 32767 ;
		else if (current < -32768)
			current = -32768 ;

		pms->samples [k] = current ;
		} ;

	return 1 ;
} /* msadpcm_decode_block */

static sf_count_t
msadpcm_read_block	(SF_PRIVATE *psf, MSADPCM_PRIVATE *pms, short *ptr, int len)
{	int	count, total = 0, indx = 0 ;

	while (indx < len)
	{	if (pms->blockcount >= pms->blocks && pms->samplecount >= pms->samplesperblock)
		{	memset (&(ptr [indx]), 0, (size_t) ((len - indx) * sizeof (short))) ;
			return total ;
			} ;

		if (pms->samplecount >= pms->samplesperblock)
			msadpcm_decode_block (psf, pms) ;

		count = (pms->samplesperblock - pms->samplecount) * pms->channels ;
		count = (len - indx > count) ? count : len - indx ;

		memcpy (&(ptr [indx]), &(pms->samples [pms->samplecount * pms->channels]), count * sizeof (short)) ;
		indx += count ;
		pms->samplecount += count / pms->channels ;
		total = indx ;
		} ;

	return total ;
} /* msadpcm_read_block */

static sf_count_t
msadpcm_read_s	(SF_PRIVATE *psf, short *ptr, sf_count_t len)
{	MSADPCM_PRIVATE 	*pms ;
	int			readcount, count ;
	sf_count_t	total = 0 ;

	if (! psf->codec_data)
		return 0 ;
	pms = (MSADPCM_PRIVATE*) psf->codec_data ;

	while (len > 0)
	{	readcount = (len > 0x10000000) ? 0x10000000 : (int) len ;

		count = msadpcm_read_block (psf, pms, ptr, readcount) ;

		total += count ;
		len -= count ;
		if (count != readcount)
			break ;
		} ;

	return total ;
} /* msadpcm_read_s */

static sf_count_t
msadpcm_read_i	(SF_PRIVATE *psf, int *ptr, sf_count_t len)
{	MSADPCM_PRIVATE *pms ;
	short		*sptr ;
	int			k, bufferlen, readcount = 0, count ;
	sf_count_t	total = 0 ;

	if (! psf->codec_data)
		return 0 ;
	pms = (MSADPCM_PRIVATE*) psf->codec_data ;

	sptr = psf->u.sbuf ;
	bufferlen = ARRAY_LEN (psf->u.sbuf) ;
	while (len > 0)
	{	readcount = (len >= bufferlen) ? bufferlen : len ;
		count = msadpcm_read_block (psf, pms, sptr, readcount) ;
		for (k = 0 ; k < readcount ; k++)
			ptr [total + k] = sptr [k] << 16 ;
		total += count ;
		len -= readcount ;
		if (count != readcount)
			break ;
		} ;
	return total ;
} /* msadpcm_read_i */

static sf_count_t
msadpcm_read_f	(SF_PRIVATE *psf, float *ptr, sf_count_t len)
{	MSADPCM_PRIVATE *pms ;
	short		*sptr ;
	int			k, bufferlen, readcount = 0, count ;
	sf_count_t	total = 0 ;
	float		normfact ;

	if (! psf->codec_data)
		return 0 ;
	pms = (MSADPCM_PRIVATE*) psf->codec_data ;

	normfact = (psf->norm_float == SF_TRUE) ? 1.0 / ((float) 0x8000) : 1.0 ;
	sptr = psf->u.sbuf ;
	bufferlen = ARRAY_LEN (psf->u.sbuf) ;
	while (len > 0)
	{	readcount = (len >= bufferlen) ? bufferlen : len ;
		count = msadpcm_read_block (psf, pms, sptr, readcount) ;
		for (k = 0 ; k < readcount ; k++)
			ptr [total + k] = normfact * (float) (sptr [k]) ;
		total += count ;
		len -= readcount ;
		if (count != readcount)
			break ;
		} ;
	return total ;
} /* msadpcm_read_f */

static sf_count_t
msadpcm_read_d	(SF_PRIVATE *psf, double *ptr, sf_count_t len)
{	MSADPCM_PRIVATE *pms ;
	short		*sptr ;
	int			k, bufferlen, readcount = 0, count ;
	sf_count_t	total = 0 ;
	double 		normfact ;

	normfact = (psf->norm_double == SF_TRUE) ? 1.0 / ((double) 0x8000) : 1.0 ;

	if (! psf->codec_data)
		return 0 ;
	pms = (MSADPCM_PRIVATE*) psf->codec_data ;

	sptr = psf->u.sbuf ;
	bufferlen = ARRAY_LEN (psf->u.sbuf) ;
	while (len > 0)
	{	readcount = (len >= bufferlen) ? bufferlen : len ;
		count = msadpcm_read_block (psf, pms, sptr, readcount) ;
		for (k = 0 ; k < readcount ; k++)
			ptr [total + k] = normfact * (double) (sptr [k]) ;
		total += count ;
		len -= readcount ;
		if (count != readcount)
			break ;
		} ;
	return total ;
} /* msadpcm_read_d */

static sf_count_t
msadpcm_seek	(SF_PRIVATE *psf, int mode, sf_count_t offset)
{	MSADPCM_PRIVATE *pms ;
	int			newblock, newsample ;

	if (! psf->codec_data)
		return 0 ;
	pms = (MSADPCM_PRIVATE*) psf->codec_data ;

	if (psf->datalength < 0 || psf->dataoffset < 0)
	{	psf->error = SFE_BAD_SEEK ;
		return	PSF_SEEK_ERROR ;
		} ;

	if (offset == 0)
	{	psf_fseek (psf, psf->dataoffset, SEEK_SET) ;
		pms->blockcount = 0 ;
		msadpcm_decode_block (psf, pms) ;
		pms->samplecount = 0 ;
		return 0 ;
		} ;

	if (offset < 0 || offset > pms->blocks * pms->samplesperblock)
	{	psf->error = SFE_BAD_SEEK ;
		return	PSF_SEEK_ERROR ;
		} ;

	newblock	= offset / pms->samplesperblock ;
	newsample	= offset % pms->samplesperblock ;

	if (mode == SFM_READ)
	{	psf_fseek (psf, psf->dataoffset + newblock * pms->blocksize, SEEK_SET) ;
		pms->blockcount = newblock ;
		msadpcm_decode_block (psf, pms) ;
		pms->samplecount = newsample ;
		}
	else
	{	/* What to do about write??? */
		psf->error = SFE_BAD_SEEK ;
		return	PSF_SEEK_ERROR ;
		} ;

	return newblock * pms->samplesperblock + newsample ;
} /* msadpcm_seek */

/*==========================================================================================
** MS ADPCM Write Functions.
*/

void
msadpcm_write_adapt_coeffs	(SF_PRIVATE *psf)
{	int k ;

	for (k = 0 ; k < MSADPCM_ADAPT_COEFF_COUNT ; k++)
		psf_binheader_writef (psf, "22", AdaptCoeff1 [k], AdaptCoeff2 [k]) ;
} /* msadpcm_write_adapt_coeffs */

/*==========================================================================================
*/

static int
msadpcm_encode_block	(SF_PRIVATE *psf, MSADPCM_PRIVATE *pms)
{	unsigned int	blockindx ;
	unsigned char	byte ;
	int				chan, k, predict, bpred [2], idelta [2], errordelta, newsamp ;

	choose_predictor (pms->channels, pms->samples, bpred, idelta) ;

	/* Write the block header. */

	if (pms->channels == 1)
	{	pms->block [0]	= bpred [0] ;
		pms->block [1]	= idelta [0] & 0xFF ;
		pms->block [2]	= idelta [0] >> 8 ;
		pms->block [3]	= pms->samples [1] & 0xFF ;
		pms->block [4]	= pms->samples [1] >> 8 ;
		pms->block [5]	= pms->samples [0] & 0xFF ;
		pms->block [6]	= pms->samples [0] >> 8 ;

		blockindx = 7 ;
		byte = 0 ;

		/* Encode the samples as 4 bit. */

		for (k = 2 ; k < pms->samplesperblock ; k++)
		{	predict = (pms->samples [k-1] * AdaptCoeff1 [bpred [0]] + pms->samples [k-2] * AdaptCoeff2 [bpred [0]]) >> 8 ;
			errordelta = (pms->samples [k] - predict) / idelta [0] ;
			if (errordelta < -8)
				errordelta = -8 ;
			else if (errordelta > 7)
				errordelta = 7 ;
			newsamp = predict + (idelta [0] * errordelta) ;
			if (newsamp > 32767)
				newsamp = 32767 ;
			else if (newsamp < -32768)
				newsamp = -32768 ;
			if (errordelta < 0)
				errordelta += 0x10 ;

			byte = (byte << 4) | (errordelta & 0xF) ;
			if (k % 2)
			{	pms->block [blockindx++] = byte ;
				byte = 0 ;
				} ;

			idelta [0] = (idelta [0] * AdaptationTable [errordelta]) >> 8 ;
			if (idelta [0] < 16)
				idelta [0] = 16 ;
			pms->samples [k] = newsamp ;
			} ;
		}
	else
	{	/* Stereo file. */
		pms->block [0]	= bpred [0] ;
		pms->block [1]	= bpred [1] ;

		pms->block [2]	= idelta [0] & 0xFF ;
		pms->block [3]	= idelta [0] >> 8 ;
		pms->block [4]	= idelta [1] & 0xFF ;
		pms->block [5]	= idelta [1] >> 8 ;

		pms->block [6]	= pms->samples [2] & 0xFF ;
		pms->block [7]	= pms->samples [2] >> 8 ;
		pms->block [8]	= pms->samples [3] & 0xFF ;
		pms->block [9]	= pms->samples [3] >> 8 ;

		pms->block [10]	= pms->samples [0] & 0xFF ;
		pms->block [11]	= pms->samples [0] >> 8 ;
		pms->block [12]	= pms->samples [1] & 0xFF ;
		pms->block [13]	= pms->samples [1] >> 8 ;

		blockindx = 14 ;
		byte = 0 ;
		chan = 1 ;

		for (k = 4 ; k < 2 * pms->samplesperblock ; k++)
		{	chan = k & 1 ;

			predict = (pms->samples [k-2] * AdaptCoeff1 [bpred [chan]] + pms->samples [k-4] * AdaptCoeff2 [bpred [chan]]) >> 8 ;
			errordelta = (pms->samples [k] - predict) / idelta [chan] ;


			if (errordelta < -8)
				errordelta = -8 ;
			else if (errordelta > 7)
				errordelta = 7 ;
			newsamp = predict + (idelta [chan] * errordelta) ;
			if (newsamp > 32767)
				newsamp = 32767 ;
			else if (newsamp < -32768)
				newsamp = -32768 ;
			if (errordelta < 0)
				errordelta += 0x10 ;

			byte = (byte << 4) | (errordelta & 0xF) ;

			if (chan)
			{	pms->block [blockindx++] = byte ;
				byte = 0 ;
				} ;

			idelta [chan] = (idelta [chan] * AdaptationTable [errordelta]) >> 8 ;
			if (idelta [chan] < 16)
				idelta [chan] = 16 ;
			pms->samples [k] = newsamp ;
			} ;
		} ;

	/* Write the block to disk. */

	if ((k = psf_fwrite (pms->block, 1, pms->blocksize, psf)) != pms->blocksize)
		psf_log_printf (psf, "*** Warning : short write (%d != %d).\n", k, pms->blocksize) ;

	memset (pms->samples, 0, pms->samplesperblock * sizeof (short)) ;

	pms->blockcount ++ ;
	pms->samplecount = 0 ;

	return 1 ;
} /* msadpcm_encode_block */

static sf_count_t
msadpcm_write_block	(SF_PRIVATE *psf, MSADPCM_PRIVATE *pms, const short *ptr, int len)
{	int		count, total = 0, indx = 0 ;

	while (indx < len)
	{	count = (pms->samplesperblock - pms->samplecount) * pms->channels ;

		if (count > len - indx)
			count = len - indx ;

		memcpy (&(pms->samples [pms->samplecount * pms->channels]), &(ptr [total]), count * sizeof (short)) ;
		indx += count ;
		pms->samplecount += count / pms->channels ;
		total = indx ;

		if (pms->samplecount >= pms->samplesperblock)
			msadpcm_encode_block (psf, pms) ;
		} ;

	return total ;
} /* msadpcm_write_block */

static sf_count_t
msadpcm_write_s	(SF_PRIVATE *psf, const short *ptr, sf_count_t len)
{	MSADPCM_PRIVATE *pms ;
	int			writecount, count ;
	sf_count_t	total = 0 ;

	if (! psf->codec_data)
		return 0 ;
	pms = (MSADPCM_PRIVATE*) psf->codec_data ;

	while (len > 0)
	{	writecount = (len > 0x10000000) ? 0x10000000 : (int) len ;

		count = msadpcm_write_block (psf, pms, ptr, writecount) ;

		total += count ;
		len -= count ;
		if (count != writecount)
			break ;
		} ;

	return total ;
} /* msadpcm_write_s */

static sf_count_t
msadpcm_write_i	(SF_PRIVATE *psf, const int *ptr, sf_count_t len)
{	MSADPCM_PRIVATE *pms ;
	short		*sptr ;
	int			k, bufferlen, writecount, count ;
	sf_count_t	total = 0 ;

	if (! psf->codec_data)
		return 0 ;
	pms = (MSADPCM_PRIVATE*) psf->codec_data ;

	sptr = psf->u.sbuf ;
	bufferlen = ARRAY_LEN (psf->u.sbuf) ;
	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : len ;
		for (k = 0 ; k < writecount ; k++)
			sptr [k] = ptr [total + k] >> 16 ;
		count = msadpcm_write_block (psf, pms, sptr, writecount) ;
		total += count ;
		len -= writecount ;
		if (count != writecount)
			break ;
		} ;
	return total ;
} /* msadpcm_write_i */

static sf_count_t
msadpcm_write_f	(SF_PRIVATE *psf, const float *ptr, sf_count_t len)
{	MSADPCM_PRIVATE *pms ;
	short		*sptr ;
	int			k, bufferlen, writecount, count ;
	sf_count_t	total = 0 ;
	float		normfact ;

	if (! psf->codec_data)
		return 0 ;
	pms = (MSADPCM_PRIVATE*) psf->codec_data ;

	normfact = (psf->norm_float == SF_TRUE) ? (1.0 * 0x7FFF) : 1.0 ;

	sptr = psf->u.sbuf ;
	bufferlen = ARRAY_LEN (psf->u.sbuf) ;
	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : len ;
		for (k = 0 ; k < writecount ; k++)
			sptr [k] = lrintf (normfact * ptr [total + k]) ;
		count = msadpcm_write_block (psf, pms, sptr, writecount) ;
		total += count ;
		len -= writecount ;
		if (count != writecount)
			break ;
		} ;
	return total ;
} /* msadpcm_write_f */

static sf_count_t
msadpcm_write_d	(SF_PRIVATE *psf, const double *ptr, sf_count_t len)
{	MSADPCM_PRIVATE *pms ;
	short		*sptr ;
	int			k, bufferlen, writecount, count ;
	sf_count_t	total = 0 ;
	double 		normfact ;

	normfact = (psf->norm_double == SF_TRUE) ? (1.0 * 0x7FFF) : 1.0 ;

	if (! psf->codec_data)
		return 0 ;
	pms = (MSADPCM_PRIVATE*) psf->codec_data ;

	sptr = psf->u.sbuf ;
	bufferlen = ARRAY_LEN (psf->u.sbuf) ;
	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : len ;
		for (k = 0 ; k < writecount ; k++)
			sptr [k] = lrint (normfact * ptr [total + k]) ;
		count = msadpcm_write_block (psf, pms, sptr, writecount) ;
		total += count ;
		len -= writecount ;
		if (count != writecount)
			break ;
		} ;
	return total ;
} /* msadpcm_write_d */

/*========================================================================================
*/

static int
msadpcm_close	(SF_PRIVATE *psf)
{	MSADPCM_PRIVATE *pms ;

	pms = (MSADPCM_PRIVATE*) psf->codec_data ;

	if (psf->mode == SFM_WRITE)
	{	/*  Now we know static int for certain the length of the file we can
		**  re-write the header.
		*/

		if (pms->samplecount && pms->samplecount < pms->samplesperblock)
			msadpcm_encode_block (psf, pms) ;
		} ;

	return 0 ;
} /* msadpcm_close */

/*========================================================================================
** Static functions.
*/

/*----------------------------------------------------------------------------------------
**	Choosing the block predictor.
**	Each block requires a predictor and an idelta for each channel.
**	The predictor is in the range [0..6] which is an indx into the	two AdaptCoeff tables.
**	The predictor is chosen by trying all of the possible predictors on a small set of
**	samples at the beginning of the block. The predictor with the smallest average
**	abs (idelta) is chosen as the best predictor for this block.
**	The value of idelta is chosen to to give a 4 bit code value of +/- 4 (approx. half the
**	max. code value). If the average abs (idelta) is zero, the sixth predictor is chosen.
**	If the value of idelta is less then 16 it is set to 16.
**
**	Microsoft uses an IDELTA_COUNT (number of sample pairs used to choose best predictor)
**	value of 3. The best possible results would be obtained by using all the samples to
**	choose the predictor.
*/

#define		IDELTA_COUNT	3

static	void
choose_predictor (unsigned int channels, short *data, int *block_pred, int *idelta)
{	unsigned int	chan, k, bpred, idelta_sum, best_bpred, best_idelta ;

	for (chan = 0 ; chan < channels ; chan++)
	{	best_bpred = best_idelta = 0 ;

		for (bpred = 0 ; bpred < 7 ; bpred++)
		{	idelta_sum = 0 ;
			for (k = 2 ; k < 2 + IDELTA_COUNT ; k++)
				idelta_sum += abs (data [k * channels] - ((data [(k - 1) * channels] * AdaptCoeff1 [bpred] + data [(k - 2) * channels] * AdaptCoeff2 [bpred]) >> 8)) ;
			idelta_sum /= (4 * IDELTA_COUNT) ;

			if (bpred == 0 || idelta_sum < best_idelta)
			{	best_bpred = bpred ;
				best_idelta = idelta_sum ;
				} ;

			if (! idelta_sum)
			{	best_bpred = bpred ;
				best_idelta = 16 ;
				break ;
				} ;

			} ; /* for bpred ... */
		if (best_idelta < 16)
			best_idelta = 16 ;

		block_pred [chan]	= best_bpred ;
		idelta [chan]		= best_idelta ;
		} ;

	return ;
} /* choose_predictor */

