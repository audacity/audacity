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

typedef struct IMA_ADPCM_PRIVATE_tag
{	int 			(*decode_block) (SF_PRIVATE *psf, struct IMA_ADPCM_PRIVATE_tag *pima) ;
	int 			(*encode_block) (SF_PRIVATE *psf, struct IMA_ADPCM_PRIVATE_tag *pima) ;

	int				channels, blocksize, samplesperblock, blocks ;
	int				blockcount, samplecount ;
	int				previous [2] ;
	int				stepindx [2] ;
	unsigned char	*block ;
	short			*samples ;
#if HAVE_FLEXIBLE_ARRAY
	short			data	[] ; /* ISO C99 struct flexible array. */
#else
	short			data	[0] ; /* This is a hack and might not work. */
#endif
} IMA_ADPCM_PRIVATE ;

/*============================================================================================
** Predefined IMA ADPCM data.
*/

static int ima_indx_adjust [16] =
{	-1, -1, -1, -1,		/* +0 - +3, decrease the step size */
     2,  4,  6,  8,     /* +4 - +7, increase the step size */
    -1, -1, -1, -1,		/* -0 - -3, decrease the step size */
     2,  4,  6,  8,		/* -4 - -7, increase the step size */
} ;

static int ima_step_size [89] =
{	7, 8, 9, 10, 11, 12, 13, 14, 16, 17, 19, 21, 23, 25, 28, 31, 34, 37, 41, 45,
	50, 55, 60, 66, 73, 80, 88, 97, 107, 118, 130, 143, 157, 173, 190, 209, 230,
	253, 279, 307, 337, 371, 408, 449, 494, 544, 598, 658, 724, 796, 876, 963,
	1060, 1166, 1282, 1411, 1552, 1707, 1878, 2066, 2272, 2499, 2749, 3024, 3327,
	3660, 4026, 4428, 4871, 5358, 5894, 6484, 7132, 7845, 8630, 9493, 10442,
	11487, 12635, 13899, 15289, 16818, 18500, 20350, 22385, 24623, 27086, 29794,
	32767
} ;

static int ima_reader_init (SF_PRIVATE *psf, int blockalign, int samplesperblock) ;
static int ima_writer_init (SF_PRIVATE *psf, int blockalign) ;

static int ima_read_block (SF_PRIVATE *psf, IMA_ADPCM_PRIVATE *pima, short *ptr, int len) ;
static int ima_write_block (SF_PRIVATE *psf, IMA_ADPCM_PRIVATE *pima, const short *ptr, int len) ;

static sf_count_t ima_read_s (SF_PRIVATE *psf, short *ptr, sf_count_t len) ;
static sf_count_t ima_read_i (SF_PRIVATE *psf, int *ptr, sf_count_t len) ;
static sf_count_t ima_read_f (SF_PRIVATE *psf, float *ptr, sf_count_t len) ;
static sf_count_t ima_read_d (SF_PRIVATE *psf, double *ptr, sf_count_t len) ;

static sf_count_t ima_write_s (SF_PRIVATE *psf, const short *ptr, sf_count_t len) ;
static sf_count_t ima_write_i (SF_PRIVATE *psf, const int *ptr, sf_count_t len) ;
static sf_count_t ima_write_f (SF_PRIVATE *psf, const float *ptr, sf_count_t len) ;
static sf_count_t ima_write_d (SF_PRIVATE *psf, const double *ptr, sf_count_t len) ;

static sf_count_t	ima_seek	(SF_PRIVATE *psf, int mode, sf_count_t offset) ;

static int	ima_close	(SF_PRIVATE *psf) ;

static int wav_w64_ima_decode_block (SF_PRIVATE *psf, IMA_ADPCM_PRIVATE *pima) ;
static int wav_w64_ima_encode_block (SF_PRIVATE *psf, IMA_ADPCM_PRIVATE *pima) ;

/*-static int aiff_ima_reader_init (SF_PRIVATE *psf, int blockalign, int samplesperblock) ;-*/
static int aiff_ima_decode_block (SF_PRIVATE *psf, IMA_ADPCM_PRIVATE *pima) ;
static int aiff_ima_encode_block (SF_PRIVATE *psf, IMA_ADPCM_PRIVATE *pima) ;


/*============================================================================================
** IMA ADPCM Reader initialisation function.
*/

int
wav_w64_ima_init (SF_PRIVATE *psf, int blockalign, int samplesperblock)
{	int error ;

	if (psf->codec_data != NULL)
	{	psf_log_printf (psf, "*** psf->codec_data is not NULL.\n") ;
		return SFE_INTERNAL ;
		} ;

	if (psf->mode == SFM_RDWR)
		return SFE_BAD_MODE_RW ;

	if (psf->mode == SFM_READ)
		if ((error = ima_reader_init (psf, blockalign, samplesperblock)))
			return error ;

	if (psf->mode == SFM_WRITE)
		if ((error = ima_writer_init (psf, blockalign)))
			return error ;

	psf->codec_close = ima_close ;
	psf->seek = ima_seek ;

	return 0 ;
} /* wav_w64_ima_init */

int
aiff_ima_init (SF_PRIVATE *psf, int blockalign, int samplesperblock)
{	int error ;

	if (psf->mode == SFM_RDWR)
		return SFE_BAD_MODE_RW ;

	if (psf->mode == SFM_READ)
		if ((error = ima_reader_init (psf, blockalign, samplesperblock)))
			return error ;

	if (psf->mode == SFM_WRITE)
		if ((error = ima_writer_init (psf, blockalign)))
			return error ;

	psf->codec_close = ima_close ;

	return 0 ;
} /* aiff_ima_init */

static int
ima_close	(SF_PRIVATE *psf)
{	IMA_ADPCM_PRIVATE *pima ;

	pima = (IMA_ADPCM_PRIVATE*) psf->codec_data ;

	if (psf->mode == SFM_WRITE)
	{	/*	If a block has been partially assembled, write it out
		**	as the final block.
		*/
		if (pima->samplecount && pima->samplecount < pima->samplesperblock)
			pima->encode_block (psf, pima) ;

		psf->sf.frames = pima->samplesperblock * pima->blockcount / psf->sf.channels ;
		} ;

	return 0 ;
} /* ima_close */

/*============================================================================================
** IMA ADPCM Read Functions.
*/

static int
ima_reader_init (SF_PRIVATE *psf, int blockalign, int samplesperblock)
{	IMA_ADPCM_PRIVATE	*pima ;
	int		pimasize, count ;

	if (psf->mode != SFM_READ)
		return SFE_BAD_MODE_RW ;

	pimasize = sizeof (IMA_ADPCM_PRIVATE) + blockalign * psf->sf.channels + 3 * psf->sf.channels * samplesperblock ;

	if (! (pima = malloc (pimasize)))
		return SFE_MALLOC_FAILED ;

	psf->codec_data = (void*) pima ;

	memset (pima, 0, pimasize) ;

	pima->samples	= pima->data ;
	pima->block		= (unsigned char*) (pima->data + samplesperblock * psf->sf.channels) ;

	pima->channels			= psf->sf.channels ;
	pima->blocksize			= blockalign ;
	pima->samplesperblock	= samplesperblock ;

	psf->filelength = psf_get_filelen (psf) ;
	psf->datalength = (psf->dataend) ? psf->dataend - psf->dataoffset :
							psf->filelength - psf->dataoffset ;

    if (pima->blocksize == 0)
	{	psf_log_printf (psf, "*** Error : pima->blocksize should not be zero.\n") ;
		return SFE_INTERNAL ;
		} ;

	if (psf->datalength % pima->blocksize)
		pima->blocks = psf->datalength / pima->blocksize + 1 ;
	else
		pima->blocks = psf->datalength / pima->blocksize ;

	switch (SF_CONTAINER (psf->sf.format))
	{	case SF_FORMAT_WAV :
		case SF_FORMAT_W64 :
				count = 2 * (pima->blocksize - 4 * pima->channels) / pima->channels + 1 ;

				if (pima->samplesperblock != count)
				{	psf_log_printf (psf, "*** Error : samplesperblock should be %d.\n", count) ;
					return SFE_INTERNAL ;
					} ;

				pima->decode_block = wav_w64_ima_decode_block ;

				psf->sf.frames = pima->samplesperblock * pima->blocks ;
				break ;

		case SF_FORMAT_AIFF :
				psf_log_printf (psf, "still need to check block count\n") ;
				pima->decode_block = aiff_ima_decode_block ;
				psf->sf.frames = pima->samplesperblock * pima->blocks / pima->channels ;
				break ;

		default :
				psf_log_printf (psf, "ima_reader_init: bad psf->sf.format\n") ;
				return SFE_INTERNAL ;
				break ;
		} ;

	pima->decode_block (psf, pima) ;	/* Read first block. */

	psf->read_short		= ima_read_s ;
	psf->read_int		= ima_read_i ;
	psf->read_float		= ima_read_f ;
	psf->read_double	= ima_read_d ;

	return 0 ;
} /* ima_reader_init */

static int
aiff_ima_decode_block (SF_PRIVATE *psf, IMA_ADPCM_PRIVATE *pima)
{	unsigned char *blockdata ;
	int		chan, k, diff, bytecode ;
	short	step, stepindx, predictor, *sampledata ;

static int count = 0 ;
count ++ ;

	pima->blockcount += pima->channels ;
	pima->samplecount = 0 ;

	if (pima->blockcount > pima->blocks)
	{	memset (pima->samples, 0, pima->samplesperblock * pima->channels * sizeof (short)) ;
		return 1 ;
		} ;

	if ((k = psf_fread (pima->block, 1, pima->blocksize * pima->channels, psf)) != pima->blocksize * pima->channels)
		psf_log_printf (psf, "*** Warning : short read (%d != %d).\n", k, pima->blocksize) ;

	/* Read and check the block header. */
	for (chan = 0 ; chan < pima->channels ; chan++)
	{	blockdata = pima->block + chan * 34 ;
		sampledata = pima->samples + chan ;

		predictor = (blockdata [0] << 8) | (blockdata [1] & 0x80) ;
		stepindx = blockdata [1] & 0x7F ;

{
if (count < 5)
printf ("\nchan: %d    predictor: %d    stepindx: %d (%d)\n",
	chan, predictor, stepindx, ima_step_size [stepindx]) ;
}
		/* FIXME : Do this a better way. */
		if (stepindx < 0) stepindx = 0 ;
		else if (stepindx > 88)	stepindx = 88 ;

		/*
		**	Pull apart the packed 4 bit samples and store them in their
		**	correct sample positions.
		*/
		for (k = 0 ; k < pima->blocksize - 2 ; k++)
		{	bytecode = blockdata [k + 2] ;
			sampledata [pima->channels * (2 * k + 0)] = bytecode & 0xF ;
			sampledata [pima->channels * (2 * k + 1)] = (bytecode >> 4) & 0xF ;
			} ;

		/* Decode the encoded 4 bit samples. */
		for (k = 0 ; k < pima->samplesperblock ; k ++)
		{	step = ima_step_size [stepindx] ;

			bytecode = pima->samples [pima->channels * k + chan] ;

			stepindx += ima_indx_adjust [bytecode] ;

			if (stepindx < 0) stepindx = 0 ;
			else if (stepindx > 88) stepindx = 88 ;

			diff = step >> 3 ;
			if (bytecode & 1)	diff += step >> 2 ;
			if (bytecode & 2)	diff += step >> 1 ;
			if (bytecode & 4)	diff += step ;
			if (bytecode & 8)	diff = -diff ;

			predictor += diff ;

			pima->samples [pima->channels * k + chan] = predictor ;
			} ;
		} ;

if (count < 5)
{
	for (k = 0 ; k < 10 ; k++)
		printf ("% 7d,", pima->samples [k]) ;
	puts ("") ;
}

	return 1 ;
} /* aiff_ima_decode_block */

static int
aiff_ima_encode_block (SF_PRIVATE *psf, IMA_ADPCM_PRIVATE *pima)
{	int		chan, k, step, diff, vpdiff, blockindx, indx ;
	short	bytecode, mask ;

static int count = 0 ;
if (0 && count == 0)
{	pima->samples [0] = 0 ;
	printf ("blocksize : %d\n", pima->blocksize) ;
	printf ("pima->stepindx [0] : %d\n", pima->stepindx [0]) ;
	}
count ++ ;

	/* Encode the block header. */
	for (chan = 0 ; chan < pima->channels ; chan ++)
	{	blockindx = chan * pima->blocksize ;

		pima->block [blockindx] = (pima->samples [chan] >> 8) & 0xFF ;
		pima->block [blockindx + 1] = (pima->samples [chan] & 0x80) + (pima->stepindx [chan] & 0x7F) ;

		pima->previous [chan] = pima->samples [chan] ;
		} ;

	/* Encode second and later samples for every block as a 4 bit value. */
	for (k = pima->channels ; k < (pima->samplesperblock * pima->channels) ; k ++)
	{	chan = (pima->channels > 1) ? (k % 2) : 0 ;

		diff = pima->samples [k] - pima->previous [chan] ;

		bytecode = 0 ;
		step = ima_step_size [pima->stepindx [chan]] ;
		vpdiff = step >> 3 ;
		if (diff < 0)
		{	bytecode = 8 ;
			diff = -diff ;
			} ;
		mask = 4 ;
		while (mask)
		{	if (diff >= step)
			{	bytecode |= mask ;
				diff -= step ;
				vpdiff += step ;
				} ;
			step >>= 1 ;
			mask >>= 1 ;
			} ;

		if (bytecode & 8)
			pima->previous [chan] -= vpdiff ;
		else
			pima->previous [chan] += vpdiff ;

		if (pima->previous [chan] > 32767)
			pima->previous [chan] = 32767 ;
		else if (pima->previous [chan] < -32768)
			pima->previous [chan] = -32768 ;

		pima->stepindx [chan] += ima_indx_adjust [bytecode] ;
		if (pima->stepindx [chan] < 0)
			pima->stepindx [chan] = 0 ;
		else if (pima->stepindx [chan] > 88)
			pima->stepindx [chan] = 88 ;

		pima->samples [k] = bytecode ;
		} ;

	/* Pack the 4 bit encoded samples. */

	for (chan = 0 ; chan < pima->channels ; chan ++)
	{	for (indx = pima->channels ; indx < pima->channels * pima->samplesperblock ; indx += 2 * pima->channels)
		{	blockindx = chan * pima->blocksize + 2 + indx / 2 ;

if (0 && count ++ < 5)
	printf ("chan: %d    blockindx: %3d    indx: %3d\n", chan, blockindx, indx) ;

			pima->block [blockindx] = pima->samples [indx] & 0x0F ;
			pima->block [blockindx] |= (pima->samples [indx + pima->channels] << 4) & 0xF0 ;
			} ;
		} ;

	/* Write the block to disk. */

	if ((k = psf_fwrite (pima->block, 1, pima->channels * pima->blocksize, psf)) != pima->channels * pima->blocksize)
		psf_log_printf (psf, "*** Warning : short write (%d != %d).\n", k, pima->channels * pima->blocksize) ;

	memset (pima->samples, 0, pima->channels * pima->samplesperblock * sizeof (short)) ;
	pima->samplecount = 0 ;
	pima->blockcount ++ ;

	return 1 ;
} /* aiff_ima_encode_block */

static int
wav_w64_ima_decode_block (SF_PRIVATE *psf, IMA_ADPCM_PRIVATE *pima)
{	int		chan, k, current, blockindx, indx, indxstart, diff ;
	short	step, bytecode, stepindx [2] ;

	pima->blockcount ++ ;
	pima->samplecount = 0 ;

	if (pima->blockcount > pima->blocks)
	{	memset (pima->samples, 0, pima->samplesperblock * pima->channels * sizeof (short)) ;
		return 1 ;
		} ;

	if ((k = psf_fread (pima->block, 1, pima->blocksize, psf)) != pima->blocksize)
		psf_log_printf (psf, "*** Warning : short read (%d != %d).\n", k, pima->blocksize) ;

	/* Read and check the block header. */

	for (chan = 0 ; chan < pima->channels ; chan++)
	{	current = pima->block [chan*4] | (pima->block [chan*4+1] << 8) ;
		if (current & 0x8000)
			current -= 0x10000 ;

		stepindx [chan] = pima->block [chan*4+2] ;
		if (stepindx [chan] < 0)
			stepindx [chan] = 0 ;
		else if (stepindx [chan] > 88)
			stepindx [chan] = 88 ;

		if (pima->block [chan*4+3] != 0)
			psf_log_printf (psf, "IMA ADPCM synchronisation error.\n") ;

		pima->samples [chan] = current ;
		} ;

	/*
	**	Pull apart the packed 4 bit samples and store them in their
	**	correct sample positions.
	*/

	blockindx = 4 * pima->channels ;

	indxstart = pima->channels ;
	while (blockindx < pima->blocksize)
	{	for (chan = 0 ; chan < pima->channels ; chan++)
		{	indx = indxstart + chan ;
			for (k = 0 ; k < 4 ; k++)
			{	bytecode = pima->block [blockindx++] ;
				pima->samples [indx] = bytecode & 0x0F ;
				indx += pima->channels ;
				pima->samples [indx] = (bytecode >> 4) & 0x0F ;
				indx += pima->channels ;
				} ;
			} ;
		indxstart += 8 * pima->channels ;
		} ;

	/* Decode the encoded 4 bit samples. */

	for (k = pima->channels ; k < (pima->samplesperblock * pima->channels) ; k ++)
	{	chan = (pima->channels > 1) ? (k % 2) : 0 ;

		bytecode = pima->samples [k] & 0xF ;

		step = ima_step_size [stepindx [chan]] ;
		current = pima->samples [k - pima->channels] ;

		diff = step >> 3 ;
		if (bytecode & 1)
			diff += step >> 2 ;
		if (bytecode & 2)
			diff += step >> 1 ;
		if (bytecode & 4)
			diff += step ;
		if (bytecode & 8)
			diff = -diff ;

		current += diff ;

		if (current > 32767)
			current = 32767 ;
		else if (current < -32768)
			current = -32768 ;

		stepindx [chan] += ima_indx_adjust [bytecode] ;

		if (stepindx [chan] < 0)
			stepindx [chan] = 0 ;
		else if (stepindx [chan] > 88)
			stepindx [chan] = 88 ;

		pima->samples [k] = current ;
		} ;

	return 1 ;
} /* wav_w64_ima_decode_block */

static int
wav_w64_ima_encode_block (SF_PRIVATE *psf, IMA_ADPCM_PRIVATE *pima)
{	int		chan, k, step, diff, vpdiff, blockindx, indx, indxstart ;
	short	bytecode, mask ;

	/* Encode the block header. */
	for (chan = 0 ; chan < pima->channels ; chan++)
	{	pima->block [chan*4]	= pima->samples [chan] & 0xFF ;
		pima->block [chan*4+1]	= (pima->samples [chan] >> 8) & 0xFF ;

		pima->block [chan*4+2] = pima->stepindx [chan] ;
		pima->block [chan*4+3] = 0 ;

		pima->previous [chan] = pima->samples [chan] ;
		} ;

	/* Encode the samples as 4 bit. */

	for (k = pima->channels ; k < (pima->samplesperblock * pima->channels) ; k ++)
	{	chan = (pima->channels > 1) ? (k % 2) : 0 ;

		diff = pima->samples [k] - pima->previous [chan] ;

		bytecode = 0 ;
		step = ima_step_size [pima->stepindx [chan]] ;
		vpdiff = step >> 3 ;
		if (diff < 0)
		{	bytecode = 8 ;
			diff = -diff ;
			} ;
		mask = 4 ;
		while (mask)
		{	if (diff >= step)
			{	bytecode |= mask ;
				diff -= step ;
				vpdiff += step ;
				} ;
			step >>= 1 ;
			mask >>= 1 ;
			} ;

		if (bytecode & 8)
			pima->previous [chan] -= vpdiff ;
		else
			pima->previous [chan] += vpdiff ;

		if (pima->previous [chan] > 32767)
			pima->previous [chan] = 32767 ;
		else if (pima->previous [chan] < -32768)
			pima->previous [chan] = -32768 ;

		pima->stepindx [chan] += ima_indx_adjust [bytecode] ;
		if (pima->stepindx [chan] < 0)
			pima->stepindx [chan] = 0 ;
		else if (pima->stepindx [chan] > 88)
			pima->stepindx [chan] = 88 ;

		pima->samples [k] = bytecode ;
		} ;

	/* Pack the 4 bit encoded samples. */

	blockindx = 4 * pima->channels ;

	indxstart = pima->channels ;
	while (blockindx < pima->blocksize)
	{	for (chan = 0 ; chan < pima->channels ; chan++)
		{	indx = indxstart + chan ;
			for (k = 0 ; k < 4 ; k++)
			{	pima->block [blockindx] = pima->samples [indx] & 0x0F ;
				indx += pima->channels ;
				pima->block [blockindx] |= (pima->samples [indx] << 4) & 0xF0 ;
				indx += pima->channels ;
				blockindx ++ ;
				} ;
			} ;
		indxstart += 8 * pima->channels ;
		} ;

	/* Write the block to disk. */

	if ((k = psf_fwrite (pima->block, 1, pima->blocksize, psf)) != pima->blocksize)
		psf_log_printf (psf, "*** Warning : short write (%d != %d).\n", k, pima->blocksize) ;

	memset (pima->samples, 0, pima->samplesperblock * sizeof (short)) ;
	pima->samplecount = 0 ;
	pima->blockcount ++ ;

	return 1 ;
} /* wav_w64_ima_encode_block */

static int
ima_read_block (SF_PRIVATE *psf, IMA_ADPCM_PRIVATE *pima, short *ptr, int len)
{	int		count, total = 0, indx = 0 ;

	while (indx < len)
	{	if (pima->blockcount >= pima->blocks && pima->samplecount >= pima->samplesperblock)
		{	memset (&(ptr [indx]), 0, (size_t) ((len - indx) * sizeof (short))) ;
			return total ;
			} ;

		if (pima->samplecount >= pima->samplesperblock)
			pima->decode_block (psf, pima) ;

		count = (pima->samplesperblock - pima->samplecount) * pima->channels ;
		count = (len - indx > count) ? count : len - indx ;

		memcpy (&(ptr [indx]), &(pima->samples [pima->samplecount * pima->channels]), count * sizeof (short)) ;
		indx += count ;
		pima->samplecount += count / pima->channels ;
		total = indx ;
		} ;

	return total ;
} /* ima_read_block */

static sf_count_t
ima_read_s (SF_PRIVATE *psf, short *ptr, sf_count_t len)
{	IMA_ADPCM_PRIVATE 	*pima ;
	int			readcount, count ;
	sf_count_t	total = 0 ;

	if (! psf->codec_data)
		return 0 ;
	pima = (IMA_ADPCM_PRIVATE*) psf->codec_data ;

	while (len > 0)
	{	readcount = (len > 0x10000000) ? 0x10000000 : (int) len ;

		count = ima_read_block (psf, pima, ptr, readcount) ;

		total += count ;
		len -= count ;
		if (count != readcount)
			break ;
		} ;

	return total ;
} /* ima_read_s */

static sf_count_t
ima_read_i (SF_PRIVATE *psf, int *ptr, sf_count_t len)
{	IMA_ADPCM_PRIVATE *pima ;
	short		*sptr ;
	int			k, bufferlen, readcount, count ;
	sf_count_t	total = 0 ;

	if (! psf->codec_data)
		return 0 ;
	pima = (IMA_ADPCM_PRIVATE*) psf->codec_data ;

	sptr = psf->u.sbuf ;
	bufferlen = ARRAY_LEN (psf->u.sbuf) ;
	while (len > 0)
	{	readcount = (len >= bufferlen) ? bufferlen : (int) len ;
		count = ima_read_block (psf, pima, sptr, readcount) ;
		for (k = 0 ; k < readcount ; k++)
			ptr [total + k] = ((int) sptr [k]) << 16 ;
		total += count ;
		len -= readcount ;
		if (count != readcount)
			break ;
		} ;

	return total ;
} /* ima_read_i */

static sf_count_t
ima_read_f (SF_PRIVATE *psf, float *ptr, sf_count_t len)
{	IMA_ADPCM_PRIVATE *pima ;
	short		*sptr ;
	int			k, bufferlen, readcount, count ;
	sf_count_t	total = 0 ;
	float		normfact ;

	if (! psf->codec_data)
		return 0 ;
	pima = (IMA_ADPCM_PRIVATE*) psf->codec_data ;

	normfact = (psf->norm_float == SF_TRUE) ? 1.0 / ((float) 0x8000) : 1.0 ;

	sptr = psf->u.sbuf ;
	bufferlen = ARRAY_LEN (psf->u.sbuf) ;
	while (len > 0)
	{	readcount = (len >= bufferlen) ? bufferlen : (int) len ;
		count = ima_read_block (psf, pima, sptr, readcount) ;
		for (k = 0 ; k < readcount ; k++)
			ptr [total + k] = normfact * (float) (sptr [k]) ;
		total += count ;
		len -= readcount ;
		if (count != readcount)
			break ;
		} ;

	return total ;
} /* ima_read_f */

static sf_count_t
ima_read_d (SF_PRIVATE *psf, double *ptr, sf_count_t len)
{	IMA_ADPCM_PRIVATE *pima ;
	short		*sptr ;
	int			k, bufferlen, readcount, count ;
	sf_count_t	total = 0 ;
	double 		normfact ;

	if (! psf->codec_data)
		return 0 ;
	pima = (IMA_ADPCM_PRIVATE*) psf->codec_data ;

	normfact = (psf->norm_double == SF_TRUE) ? 1.0 / ((double) 0x8000) : 1.0 ;

	sptr = psf->u.sbuf ;
	bufferlen = ARRAY_LEN (psf->u.sbuf) ;
	while (len > 0)
	{	readcount = (len >= bufferlen) ? bufferlen : (int) len ;
		count = ima_read_block (psf, pima, sptr, readcount) ;
		for (k = 0 ; k < readcount ; k++)
			ptr [total + k] = normfact * (double) (sptr [k]) ;
		total += count ;
		len -= readcount ;
		if (count != readcount)
			break ;
		} ;

	return total ;
} /* ima_read_d */

static sf_count_t
ima_seek (SF_PRIVATE *psf, int mode, sf_count_t offset)
{	IMA_ADPCM_PRIVATE *pima ;
	int			newblock, newsample ;

	if (! psf->codec_data)
		return 0 ;
	pima = (IMA_ADPCM_PRIVATE*) psf->codec_data ;

	if (psf->datalength < 0 || psf->dataoffset < 0)
	{	psf->error = SFE_BAD_SEEK ;
		return PSF_SEEK_ERROR ;
		} ;

	if (offset == 0)
	{	psf_fseek (psf, psf->dataoffset, SEEK_SET) ;
		pima->blockcount = 0 ;
		pima->decode_block (psf, pima) ;
		pima->samplecount = 0 ;
		return 0 ;
		} ;

	if (offset < 0 || offset > pima->blocks * pima->samplesperblock)
	{	psf->error = SFE_BAD_SEEK ;
		return	PSF_SEEK_ERROR ;
		} ;

	newblock	= offset / pima->samplesperblock ;
	newsample	= offset % pima->samplesperblock ;

	if (mode == SFM_READ)
	{	psf_fseek (psf, psf->dataoffset + newblock * pima->blocksize, SEEK_SET) ;
		pima->blockcount = newblock ;
		pima->decode_block (psf, pima) ;
		pima->samplecount = newsample ;
		}
	else
	{	/* What to do about write??? */
		psf->error = SFE_BAD_SEEK ;
		return	PSF_SEEK_ERROR ;
		} ;

	return newblock * pima->samplesperblock + newsample ;
} /* ima_seek */

/*==========================================================================================
** IMA ADPCM Write Functions.
*/

static int
ima_writer_init (SF_PRIVATE *psf, int blockalign)
{	IMA_ADPCM_PRIVATE	*pima ;
	int					samplesperblock ;
	unsigned int 		pimasize ;

	if (psf->mode != SFM_WRITE)
		return SFE_BAD_MODE_RW ;

	samplesperblock = 2 * (blockalign - 4 * psf->sf.channels) / psf->sf.channels + 1 ;

	pimasize = sizeof (IMA_ADPCM_PRIVATE) + blockalign + 3 * psf->sf.channels * samplesperblock ;

	if ((pima = calloc (1, pimasize)) == NULL)
		return SFE_MALLOC_FAILED ;

	psf->codec_data = (void*) pima ;

	pima->channels			= psf->sf.channels ;
	pima->blocksize			= blockalign ;
	pima->samplesperblock	= samplesperblock ;

	pima->block		= (unsigned char*) pima->data ;
	pima->samples	= (short*) (pima->data + blockalign) ;

	pima->samplecount = 0 ;

	switch (SF_CONTAINER (psf->sf.format))
	{	case SF_FORMAT_WAV :
		case SF_FORMAT_W64 :
				pima->encode_block = wav_w64_ima_encode_block ;
				break ;

		case SF_FORMAT_AIFF :
				pima->encode_block = aiff_ima_encode_block ;
				break ;

		default :
				psf_log_printf (psf, "ima_reader_init: bad psf->sf.format\n") ;
				return SFE_INTERNAL ;
				break ;
		} ;

	psf->write_short	= ima_write_s ;
	psf->write_int		= ima_write_i ;
	psf->write_float	= ima_write_f ;
	psf->write_double	= ima_write_d ;

	return 0 ;
} /* ima_writer_init */

/*==========================================================================================
*/

static int
ima_write_block (SF_PRIVATE *psf, IMA_ADPCM_PRIVATE *pima, const short *ptr, int len)
{	int		count, total = 0, indx = 0 ;

	while (indx < len)
	{	count = (pima->samplesperblock - pima->samplecount) * pima->channels ;

		if (count > len - indx)
			count = len - indx ;

		memcpy (&(pima->samples [pima->samplecount * pima->channels]), &(ptr [total]), count * sizeof (short)) ;
		indx += count ;
		pima->samplecount += count / pima->channels ;
		total = indx ;

		if (pima->samplecount >= pima->samplesperblock)
			pima->encode_block (psf, pima) ;
		} ;

	return total ;
} /* ima_write_block */

static sf_count_t
ima_write_s (SF_PRIVATE *psf, const short *ptr, sf_count_t len)
{	IMA_ADPCM_PRIVATE 	*pima ;
	int			writecount, count ;
	sf_count_t	total = 0 ;

	if (! psf->codec_data)
		return 0 ;
	pima = (IMA_ADPCM_PRIVATE*) psf->codec_data ;

	while (len)
	{	writecount = (len > 0x10000000) ? 0x10000000 : (int) len ;

		count = ima_write_block (psf, pima, ptr, writecount) ;

		total += count ;
		len -= count ;
		if (count != writecount)
			break ;
		} ;

	return total ;
} /* ima_write_s */

static sf_count_t
ima_write_i (SF_PRIVATE *psf, const int *ptr, sf_count_t len)
{	IMA_ADPCM_PRIVATE *pima ;
	short		*sptr ;
	int			k, bufferlen, writecount, count ;
	sf_count_t	total = 0 ;

	if (! psf->codec_data)
		return 0 ;
	pima = (IMA_ADPCM_PRIVATE*) psf->codec_data ;

	sptr = psf->u.sbuf ;
	bufferlen = ARRAY_LEN (psf->u.sbuf) ;
	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : (int) len ;
		for (k = 0 ; k < writecount ; k++)
			sptr [k] = ptr [total + k] >> 16 ;
		count = ima_write_block (psf, pima, sptr, writecount) ;
		total += count ;
		len -= writecount ;
		if (count != writecount)
			break ;
		} ;

	return total ;
} /* ima_write_i */

static sf_count_t
ima_write_f (SF_PRIVATE *psf, const float *ptr, sf_count_t len)
{	IMA_ADPCM_PRIVATE *pima ;
	short		*sptr ;
	int			k, bufferlen, writecount, count ;
	sf_count_t	total = 0 ;
	float		normfact ;

	if (! psf->codec_data)
		return 0 ;
	pima = (IMA_ADPCM_PRIVATE*) psf->codec_data ;

	normfact = (psf->norm_float == SF_TRUE) ? (1.0 * 0x7FFF) : 1.0 ;

	sptr = psf->u.sbuf ;
	bufferlen = ARRAY_LEN (psf->u.sbuf) ;
	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : (int) len ;
		for (k = 0 ; k < writecount ; k++)
			sptr [k] = lrintf (normfact * ptr [total + k]) ;
		count = ima_write_block (psf, pima, sptr, writecount) ;
		total += count ;
		len -= writecount ;
		if (count != writecount)
			break ;
		} ;

	return total ;
} /* ima_write_f */

static sf_count_t
ima_write_d (SF_PRIVATE *psf, const double *ptr, sf_count_t len)
{	IMA_ADPCM_PRIVATE *pima ;
	short		*sptr ;
	int			k, bufferlen, writecount, count ;
	sf_count_t	total = 0 ;
	double 		normfact ;

	if (! psf->codec_data)
		return 0 ;
	pima = (IMA_ADPCM_PRIVATE*) psf->codec_data ;

	normfact = (psf->norm_double == SF_TRUE) ? (1.0 * 0x7FFF) : 1.0 ;

	sptr = psf->u.sbuf ;
	bufferlen = ARRAY_LEN (psf->u.sbuf) ;
	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : (int) len ;
		for (k = 0 ; k < writecount ; k++)
			sptr [k] = lrint (normfact * ptr [total + k]) ;
		count = ima_write_block (psf, pima, sptr, writecount) ;
		total += count ;
		len -= writecount ;
		if (count != writecount)
			break ;
		} ;

	return total ;
} /* ima_write_d */

