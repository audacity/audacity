/*
** Copyright (C) 1999-2017 Erik de Castro Lopo <erikd@mega-nerd.com>
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
#include <fcntl.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#include "sndfile.h"
#include "sfendian.h"
#include "common.h"

/*------------------------------------------------------------------------------
** Macros to handle big/little endian issues.
*/

#define FAP_MARKER	(MAKE_MARKER ('f', 'a', 'p', ' '))
#define PAF_MARKER	(MAKE_MARKER (' ', 'p', 'a', 'f'))

/*------------------------------------------------------------------------------
** Other defines.
*/

#define	PAF_HEADER_LENGTH 			2048

#define	PAF24_SAMPLES_PER_BLOCK		10
#define	PAF24_BLOCK_SIZE			32

/*------------------------------------------------------------------------------
** Typedefs.
*/

typedef	struct
{	int	version ;
	int	endianness ;
	int	samplerate ;
	int	format ;
	int	channels ;
	int	source ;
} PAF_FMT ;

typedef struct
{	int				max_blocks, channels, blocksize ;
	int				read_block, write_block, read_count, write_count ;
	sf_count_t		sample_count ;
	int				*samples ;
	int				*block ;
	int				data [] ; /* ISO C99 struct flexible array. */
} PAF24_PRIVATE ;

/*------------------------------------------------------------------------------
** Private static functions.
*/

static int paf24_init (SF_PRIVATE *psf) ;

static int	paf_read_header	(SF_PRIVATE *psf) ;
static int	paf_write_header (SF_PRIVATE *psf, int calc_length) ;

static sf_count_t paf24_read_s (SF_PRIVATE *psf, short *ptr, sf_count_t len) ;
static sf_count_t paf24_read_i (SF_PRIVATE *psf, int *ptr, sf_count_t len) ;
static sf_count_t paf24_read_f (SF_PRIVATE *psf, float *ptr, sf_count_t len) ;
static sf_count_t paf24_read_d (SF_PRIVATE *psf, double *ptr, sf_count_t len) ;

static sf_count_t paf24_write_s (SF_PRIVATE *psf, const short *ptr, sf_count_t len) ;
static sf_count_t paf24_write_i (SF_PRIVATE *psf, const int *ptr, sf_count_t len) ;
static sf_count_t paf24_write_f (SF_PRIVATE *psf, const float *ptr, sf_count_t len) ;
static sf_count_t paf24_write_d (SF_PRIVATE *psf, const double *ptr, sf_count_t len) ;

static sf_count_t paf24_seek (SF_PRIVATE *psf, int mode, sf_count_t offset) ;

enum
{	PAF_PCM_16 = 0,
	PAF_PCM_24 = 1,
	PAF_PCM_S8 = 2
} ;

/*------------------------------------------------------------------------------
** Public function.
*/

int
paf_open	(SF_PRIVATE *psf)
{	int		subformat, error, endian ;

 	psf->dataoffset = PAF_HEADER_LENGTH ;

	if (psf->file.mode == SFM_READ || (psf->file.mode == SFM_RDWR && psf->filelength > 0))
	{	if ((error = paf_read_header (psf)))
			return error ;
		} ;

	subformat = SF_CODEC (psf->sf.format) ;

	if (psf->file.mode == SFM_WRITE || psf->file.mode == SFM_RDWR)
	{	if ((SF_CONTAINER (psf->sf.format)) != SF_FORMAT_PAF)
			return	SFE_BAD_OPEN_FORMAT ;

		endian = SF_ENDIAN (psf->sf.format) ;

		/* PAF is by default big endian. */
		psf->endian = SF_ENDIAN_BIG ;

		if (endian == SF_ENDIAN_LITTLE || (CPU_IS_LITTLE_ENDIAN && (endian == SF_ENDIAN_CPU)))
			psf->endian = SF_ENDIAN_LITTLE ;

		if ((error = paf_write_header (psf, SF_FALSE)))
			return error ;

		psf->write_header = paf_write_header ;
		} ;

	switch (subformat)
	{	case SF_FORMAT_PCM_S8 :
					psf->bytewidth = 1 ;
					error = pcm_init (psf) ;
					break ;

		case SF_FORMAT_PCM_16 :
					psf->bytewidth = 2 ;
					error = pcm_init (psf) ;
					break ;

		case SF_FORMAT_PCM_24 :
					/* No bytewidth because of whacky 24 bit encoding. */
					error = paf24_init (psf) ;
					break ;

		default : return SFE_PAF_UNKNOWN_FORMAT ;
		} ;

	return error ;
} /* paf_open */

/*------------------------------------------------------------------------------
*/

static int
paf_read_header	(SF_PRIVATE *psf)
{	PAF_FMT		paf_fmt ;
	int			marker ;

	if (psf->filelength < PAF_HEADER_LENGTH)
		return SFE_PAF_SHORT_HEADER ;

	memset (&paf_fmt, 0, sizeof (paf_fmt)) ;
	psf_binheader_readf (psf, "pm", 0, &marker) ;

	psf_log_printf (psf, "Signature   : '%M'\n", marker) ;

	if (marker == PAF_MARKER)
	{	psf_binheader_readf (psf, "E444444", &(paf_fmt.version), &(paf_fmt.endianness),
			&(paf_fmt.samplerate), &(paf_fmt.format), &(paf_fmt.channels), &(paf_fmt.source)) ;
		}
	else if (marker == FAP_MARKER)
	{	psf_binheader_readf (psf, "e444444", &(paf_fmt.version), &(paf_fmt.endianness),
			&(paf_fmt.samplerate), &(paf_fmt.format), &(paf_fmt.channels), &(paf_fmt.source)) ;
		}
	else
		return SFE_PAF_NO_MARKER ;

	psf_log_printf (psf, "Version     : %d\n", paf_fmt.version) ;

	if (paf_fmt.version != 0)
	{	psf_log_printf (psf, "*** Bad version number. should be zero.\n") ;
		return SFE_PAF_VERSION ;
		} ;

	psf_log_printf (psf, "Sample Rate : %d\n", paf_fmt.samplerate) ;
	psf_log_printf (psf, "Channels    : %d\n", paf_fmt.channels) ;

	psf_log_printf (psf, "Endianness  : %d => ", paf_fmt.endianness) ;
	if (paf_fmt.endianness)
	{	psf_log_printf (psf, "Little\n", paf_fmt.endianness) ;
		psf->endian = SF_ENDIAN_LITTLE ;
		}
	else
	{	psf_log_printf (psf, "Big\n", paf_fmt.endianness) ;
		psf->endian = SF_ENDIAN_BIG ;
		} ;

	if (paf_fmt.channels < 1 || paf_fmt.channels > SF_MAX_CHANNELS)
		return SFE_PAF_BAD_CHANNELS ;

	psf->datalength = psf->filelength - psf->dataoffset ;

	psf_binheader_readf (psf, "p", (int) psf->dataoffset) ;

	psf->sf.samplerate	= paf_fmt.samplerate ;
	psf->sf.channels 	= paf_fmt.channels ;

	/* Only fill in type major. */
	psf->sf.format = SF_FORMAT_PAF ;

	psf_log_printf (psf, "Format      : %d => ", paf_fmt.format) ;

	/* PAF is by default big endian. */
	psf->sf.format |= paf_fmt.endianness ? SF_ENDIAN_LITTLE : SF_ENDIAN_BIG ;

	switch (paf_fmt.format)
	{	case PAF_PCM_S8 :
					psf_log_printf (psf, "8 bit linear PCM\n") ;
					psf->bytewidth = 1 ;

					psf->sf.format |= SF_FORMAT_PCM_S8 ;

					psf->blockwidth = psf->bytewidth * psf->sf.channels ;
					psf->sf.frames = psf->datalength / psf->blockwidth ;
					break ;

		case PAF_PCM_16 :
					psf_log_printf (psf, "16 bit linear PCM\n") ;
					psf->bytewidth = 2 ;

					psf->sf.format |= SF_FORMAT_PCM_16 ;

					psf->blockwidth = psf->bytewidth * psf->sf.channels ;
					psf->sf.frames = psf->datalength / psf->blockwidth ;
					break ;

		case PAF_PCM_24 :
					psf_log_printf (psf, "24 bit linear PCM\n") ;
					psf->bytewidth = 3 ;

					psf->sf.format |= SF_FORMAT_PCM_24 ;

					psf->blockwidth = 0 ;
					psf->sf.frames = PAF24_SAMPLES_PER_BLOCK * psf->datalength /
											(PAF24_BLOCK_SIZE * psf->sf.channels) ;
					break ;

		default :	psf_log_printf (psf, "Unknown\n") ;
					return SFE_PAF_UNKNOWN_FORMAT ;
					break ;
		} ;

	psf_log_printf (psf, "Source      : %d => ", paf_fmt.source) ;

	switch (paf_fmt.source)
	{	case 1 : psf_log_printf (psf, "Analog Recording\n") ;
					break ;
		case 2 : psf_log_printf (psf, "Digital Transfer\n") ;
					break ;
		case 3 : psf_log_printf (psf, "Multi-track Mixdown\n") ;
					break ;
		case 5 : psf_log_printf (psf, "Audio Resulting From DSP Processing\n") ;
					break ;
		default : psf_log_printf (psf, "Unknown\n") ;
					break ;
		} ;

	return 0 ;
} /* paf_read_header */

static int
paf_write_header (SF_PRIVATE *psf, int UNUSED (calc_length))
{	int			paf_format ;

	/* PAF header already written so no need to re-write. */
	if (psf_ftell (psf) >= PAF_HEADER_LENGTH)
		return 0 ;

	psf->dataoffset = PAF_HEADER_LENGTH ;

	switch (SF_CODEC (psf->sf.format))
	{	case SF_FORMAT_PCM_S8 :
					paf_format = PAF_PCM_S8 ;
					break ;

		case SF_FORMAT_PCM_16 :
					paf_format = PAF_PCM_16 ;
					break ;

		case SF_FORMAT_PCM_24 :
					paf_format = PAF_PCM_24 ;
					break ;

		default : return SFE_PAF_UNKNOWN_FORMAT ;
		} ;

	/* Reset the current header length to zero. */
	psf->header.ptr [0] = 0 ;
	psf->header.indx = 0 ;

	if (psf->endian == SF_ENDIAN_BIG)
	{	/* Marker, version, endianness, samplerate */
		psf_binheader_writef (psf, "Em444", BHWm (PAF_MARKER), BHW4 (0), BHW4 (0), BHW4 (psf->sf.samplerate)) ;
		/* format, channels, source */
		psf_binheader_writef (psf, "E444", BHW4 (paf_format), BHW4 (psf->sf.channels), BHW4 (0)) ;
		}
	else if (psf->endian == SF_ENDIAN_LITTLE)
	{	/* Marker, version, endianness, samplerate */
		psf_binheader_writef (psf, "em444", BHWm (FAP_MARKER), BHW4 (0), BHW4 (1), BHW4 (psf->sf.samplerate)) ;
		/* format, channels, source */
		psf_binheader_writef (psf, "e444", BHW4 (paf_format), BHW4 (psf->sf.channels), BHW4 (0)) ;
		} ;

	/* Zero fill to dataoffset. */
	psf_binheader_writef (psf, "z", BHWz ((size_t) (psf->dataoffset - psf->header.indx))) ;

	psf_fwrite (psf->header.ptr, psf->header.indx, 1, psf) ;

	return psf->error ;
} /* paf_write_header */

/*===============================================================================
**	24 bit PAF files have a really weird encoding.
**  For a mono file, 10 samples (each being 3 bytes) are packed into a 32 byte
**	block. The 8 ints in this 32 byte block are then endian swapped (as ints)
**	if necessary before being written to disk.
**  For a stereo file, blocks of 10 samples from the same channel are encoded
**  into 32 bytes as for the mono case. The 32 byte blocks are then interleaved
**	on disk.
**	Reading has to reverse the above process :-).
**	Weird!!!
**
**	The code below attempts to gain efficiency while maintaining readability.
*/

static int paf24_read_block (SF_PRIVATE *psf, PAF24_PRIVATE *ppaf24) ;
static int paf24_write_block (SF_PRIVATE *psf, PAF24_PRIVATE *ppaf24) ;
static int paf24_close (SF_PRIVATE *psf) ;


static int
paf24_init (SF_PRIVATE *psf)
{	PAF24_PRIVATE	*ppaf24 ;
	int	paf24size ;

	paf24size = sizeof (PAF24_PRIVATE) + psf->sf.channels *
					(PAF24_BLOCK_SIZE + PAF24_SAMPLES_PER_BLOCK * sizeof (int)) ;

	/*
	**	Not exatly sure why this needs to be here but the tests
	**	fail without it.
	*/
	psf->last_op = 0 ;

	if (! (psf->codec_data = calloc (1, paf24size)))
		return SFE_MALLOC_FAILED ;

	ppaf24 = (PAF24_PRIVATE*) psf->codec_data ;

	ppaf24->channels	= psf->sf.channels ;
	ppaf24->samples		= ppaf24->data ;
	ppaf24->block		= ppaf24->data + PAF24_SAMPLES_PER_BLOCK * ppaf24->channels ;

	ppaf24->blocksize = PAF24_BLOCK_SIZE * ppaf24->channels ;

	if (psf->file.mode == SFM_READ || psf->file.mode == SFM_RDWR)
	{	paf24_read_block (psf, ppaf24) ;	/* Read first block. */

		psf->read_short		= paf24_read_s ;
		psf->read_int		= paf24_read_i ;
		psf->read_float		= paf24_read_f ;
		psf->read_double	= paf24_read_d ;
		} ;

	if (psf->file.mode == SFM_WRITE || psf->file.mode == SFM_RDWR)
	{	psf->write_short	= paf24_write_s ;
		psf->write_int		= paf24_write_i ;
		psf->write_float	= paf24_write_f ;
		psf->write_double	= paf24_write_d ;
		} ;

	psf->seek	= paf24_seek ;
	psf->container_close	= paf24_close ;

	psf->filelength = psf_get_filelen (psf) ;
	psf->datalength = psf->filelength - psf->dataoffset ;

	if (psf->datalength % PAF24_BLOCK_SIZE)
	{	if (psf->file.mode == SFM_READ)
			psf_log_printf (psf, "*** Warning : file seems to be truncated.\n") ;
		ppaf24->max_blocks = psf->datalength / ppaf24->blocksize + 1 ;
		}
	else
		ppaf24->max_blocks = psf->datalength / ppaf24->blocksize ;

	ppaf24->read_block = 0 ;
	if (psf->file.mode == SFM_RDWR)
		ppaf24->write_block = ppaf24->max_blocks ;
	else
		ppaf24->write_block = 0 ;

	psf->sf.frames = PAF24_SAMPLES_PER_BLOCK * ppaf24->max_blocks ;
	ppaf24->sample_count = psf->sf.frames ;

	return 0 ;
} /* paf24_init */

static sf_count_t
paf24_seek (SF_PRIVATE *psf, int mode, sf_count_t offset)
{	PAF24_PRIVATE	*ppaf24 ;
	int				newblock, newsample ;

	if (psf->codec_data == NULL)
	{	psf->error = SFE_INTERNAL ;
		return PSF_SEEK_ERROR ;
		} ;

	ppaf24 = (PAF24_PRIVATE*) psf->codec_data ;

	if (mode == SFM_READ && ppaf24->write_count > 0)
		paf24_write_block (psf, ppaf24) ;

	newblock	= offset / PAF24_SAMPLES_PER_BLOCK ;
	newsample	= offset % PAF24_SAMPLES_PER_BLOCK ;

	switch (mode)
	{	case SFM_READ :
				if (psf->last_op == SFM_WRITE && ppaf24->write_count)
					paf24_write_block (psf, ppaf24) ;

				psf_fseek (psf, psf->dataoffset + newblock * ppaf24->blocksize, SEEK_SET) ;
				ppaf24->read_block = newblock ;
				paf24_read_block (psf, ppaf24) ;
				ppaf24->read_count = newsample ;
				break ;

		case SFM_WRITE :
				if (offset > ppaf24->sample_count)
				{	psf->error = SFE_BAD_SEEK ;
					return PSF_SEEK_ERROR ;
					} ;

				if (psf->last_op == SFM_WRITE && ppaf24->write_count)
					paf24_write_block (psf, ppaf24) ;

				psf_fseek (psf, psf->dataoffset + newblock * ppaf24->blocksize, SEEK_SET) ;
				ppaf24->write_block = newblock ;
				paf24_read_block (psf, ppaf24) ;
				ppaf24->write_count = newsample ;
				break ;

		default :
				psf->error = SFE_BAD_SEEK ;
				return PSF_SEEK_ERROR ;
		} ;

	return newblock * PAF24_SAMPLES_PER_BLOCK + newsample ;
} /* paf24_seek */

static int
paf24_close (SF_PRIVATE *psf)
{	PAF24_PRIVATE *ppaf24 ;

	if (psf->codec_data == NULL)
		return 0 ;

	ppaf24 = (PAF24_PRIVATE*) psf->codec_data ;

	if (psf->file.mode == SFM_WRITE || psf->file.mode == SFM_RDWR)
	{	if (ppaf24->write_count > 0)
			paf24_write_block (psf, ppaf24) ;
		} ;

	return 0 ;
} /* paf24_close */

/*---------------------------------------------------------------------------
*/
static int
paf24_read_block (SF_PRIVATE *psf, PAF24_PRIVATE *ppaf24)
{	int				k, channel ;
	unsigned char	*cptr ;

	ppaf24->read_block ++ ;
	ppaf24->read_count = 0 ;

	if (ppaf24->read_block * PAF24_SAMPLES_PER_BLOCK > ppaf24->sample_count)
	{	memset (ppaf24->samples, 0, PAF24_SAMPLES_PER_BLOCK * ppaf24->channels) ;
		return 1 ;
		} ;

	/* Read the block. */
	if ((k = psf_fread (ppaf24->block, 1, ppaf24->blocksize, psf)) != ppaf24->blocksize)
		psf_log_printf (psf, "*** Warning : short read (%d != %d).\n", k, ppaf24->blocksize) ;

	/* Do endian swapping if necessary. */
	if ((CPU_IS_BIG_ENDIAN && psf->endian == SF_ENDIAN_LITTLE) || (CPU_IS_LITTLE_ENDIAN && psf->endian == SF_ENDIAN_BIG))
		endswap_int_array (ppaf24->block, 8 * ppaf24->channels) ;

	/* Unpack block. */
	for (k = 0 ; k < PAF24_SAMPLES_PER_BLOCK * ppaf24->channels ; k++)
	{	channel = k % ppaf24->channels ;
		cptr = ((unsigned char *) ppaf24->block) + PAF24_BLOCK_SIZE * channel + 3 * (k / ppaf24->channels) ;
		ppaf24->samples [k] = (cptr [0] << 8) | (cptr [1] << 16) | (((unsigned) cptr [2]) << 24) ;
		} ;

	return 1 ;
} /* paf24_read_block */

static int
paf24_read (SF_PRIVATE *psf, PAF24_PRIVATE *ppaf24, int *ptr, int len)
{	int	count, total = 0 ;

	while (total < len)
	{	if (ppaf24->read_block * PAF24_SAMPLES_PER_BLOCK >= ppaf24->sample_count)
		{	memset (&(ptr [total]), 0, (len - total) * sizeof (int)) ;
			return total ;
			} ;

		if (ppaf24->read_count >= PAF24_SAMPLES_PER_BLOCK)
			paf24_read_block (psf, ppaf24) ;

		count = (PAF24_SAMPLES_PER_BLOCK - ppaf24->read_count) * ppaf24->channels ;
		count = (len - total > count) ? count : len - total ;

		memcpy (&(ptr [total]), &(ppaf24->samples [ppaf24->read_count * ppaf24->channels]), count * sizeof (int)) ;
		total += count ;
		ppaf24->read_count += count / ppaf24->channels ;
		} ;

	return total ;
} /* paf24_read */

static sf_count_t
paf24_read_s (SF_PRIVATE *psf, short *ptr, sf_count_t len)
{	BUF_UNION		ubuf ;
	PAF24_PRIVATE 	*ppaf24 ;
	int				*iptr ;
	int				k, bufferlen, readcount, count ;
	sf_count_t		total = 0 ;

	if (psf->codec_data == NULL)
		return 0 ;
	ppaf24 = (PAF24_PRIVATE*) psf->codec_data ;

	iptr = ubuf.ibuf ;
	bufferlen = ARRAY_LEN (ubuf.ibuf) ;
	while (len > 0)
	{	readcount = (len >= bufferlen) ? bufferlen : len ;
		count = paf24_read (psf, ppaf24, iptr, readcount) ;
		for (k = 0 ; k < readcount ; k++)
			ptr [total + k] = iptr [k] >> 16 ;
		total += count ;
		len -= readcount ;
		} ;
	return total ;
} /* paf24_read_s */

static sf_count_t
paf24_read_i (SF_PRIVATE *psf, int *ptr, sf_count_t len)
{	PAF24_PRIVATE *ppaf24 ;
	int				total ;

	if (psf->codec_data == NULL)
		return 0 ;
	ppaf24 = (PAF24_PRIVATE*) psf->codec_data ;

	total = paf24_read (psf, ppaf24, ptr, len) ;

	return total ;
} /* paf24_read_i */

static sf_count_t
paf24_read_f (SF_PRIVATE *psf, float *ptr, sf_count_t len)
{	BUF_UNION		ubuf ;
	PAF24_PRIVATE 	*ppaf24 ;
	int				*iptr ;
	int				k, bufferlen, readcount, count ;
	sf_count_t		total = 0 ;
	float			normfact ;

	if (psf->codec_data == NULL)
		return 0 ;
	ppaf24 = (PAF24_PRIVATE*) psf->codec_data ;

	normfact = (psf->norm_float == SF_TRUE) ? (1.0 / 0x80000000) : (1.0 / 0x100) ;

	iptr = ubuf.ibuf ;
	bufferlen = ARRAY_LEN (ubuf.ibuf) ;
	while (len > 0)
	{	readcount = (len >= bufferlen) ? bufferlen : len ;
		count = paf24_read (psf, ppaf24, iptr, readcount) ;
		for (k = 0 ; k < readcount ; k++)
			ptr [total + k] = normfact * iptr [k] ;
		total += count ;
		len -= readcount ;
		} ;
	return total ;
} /* paf24_read_f */

static sf_count_t
paf24_read_d (SF_PRIVATE *psf, double *ptr, sf_count_t len)
{	BUF_UNION		ubuf ;
	PAF24_PRIVATE 	*ppaf24 ;
	int				*iptr ;
	int				k, bufferlen, readcount, count ;
	sf_count_t		total = 0 ;
	double 			normfact ;

	if (psf->codec_data == NULL)
		return 0 ;
	ppaf24 = (PAF24_PRIVATE*) psf->codec_data ;

	normfact = (psf->norm_double == SF_TRUE) ? (1.0 / 0x80000000) : (1.0 / 0x100) ;

	iptr = ubuf.ibuf ;
	bufferlen = ARRAY_LEN (ubuf.ibuf) ;
	while (len > 0)
	{	readcount = (len >= bufferlen) ? bufferlen : len ;
		count = paf24_read (psf, ppaf24, iptr, readcount) ;
		for (k = 0 ; k < readcount ; k++)
			ptr [total + k] = normfact * iptr [k] ;
		total += count ;
		len -= readcount ;
		} ;
	return total ;
} /* paf24_read_d */

/*---------------------------------------------------------------------------
*/

static int
paf24_write_block (SF_PRIVATE *psf, PAF24_PRIVATE *ppaf24)
{	int				k, nextsample, channel ;
	unsigned char	*cptr ;

	/* First pack block. */

	if (CPU_IS_LITTLE_ENDIAN)
	{	for (k = 0 ; k < PAF24_SAMPLES_PER_BLOCK * ppaf24->channels ; k++)
		{	channel = k % ppaf24->channels ;
			cptr = ((unsigned char *) ppaf24->block) + PAF24_BLOCK_SIZE * channel + 3 * (k / ppaf24->channels) ;
			nextsample = ppaf24->samples [k] >> 8 ;
			cptr [0] = nextsample ;
			cptr [1] = nextsample >> 8 ;
			cptr [2] = nextsample >> 16 ;
			} ;

		/* Do endian swapping if necessary. */
		if (psf->endian == SF_ENDIAN_BIG)
			endswap_int_array (ppaf24->block, 8 * ppaf24->channels) ;
		}
	else if (CPU_IS_BIG_ENDIAN)
	{	/* This is correct. */
		for (k = 0 ; k < PAF24_SAMPLES_PER_BLOCK * ppaf24->channels ; k++)
		{	channel = k % ppaf24->channels ;
			cptr = ((unsigned char *) ppaf24->block) + PAF24_BLOCK_SIZE * channel + 3 * (k / ppaf24->channels) ;
			nextsample = ppaf24->samples [k] >> 8 ;
			cptr [0] = nextsample ;
			cptr [1] = nextsample >> 8 ;
			cptr [2] = nextsample >> 16 ;
			} ;
		if (psf->endian == SF_ENDIAN_LITTLE)
			endswap_int_array (ppaf24->block, 8 * ppaf24->channels) ;
		} ;

	/* Write block to disk. */
	if ((k = psf_fwrite (ppaf24->block, 1, ppaf24->blocksize, psf)) != ppaf24->blocksize)
		psf_log_printf (psf, "*** Warning : short write (%d != %d).\n", k, ppaf24->blocksize) ;

	if (ppaf24->sample_count < ppaf24->write_block * PAF24_SAMPLES_PER_BLOCK + ppaf24->write_count)
		ppaf24->sample_count = ppaf24->write_block * PAF24_SAMPLES_PER_BLOCK + ppaf24->write_count ;

	if (ppaf24->write_count == PAF24_SAMPLES_PER_BLOCK)
	{	ppaf24->write_block ++ ;
		ppaf24->write_count = 0 ;
		} ;

	return 1 ;
} /* paf24_write_block */

static int
paf24_write (SF_PRIVATE *psf, PAF24_PRIVATE *ppaf24, const int *ptr, int len)
{	int		count, total = 0 ;

	while (total < len)
	{	count = (PAF24_SAMPLES_PER_BLOCK - ppaf24->write_count) * ppaf24->channels ;

		if (count > len - total)
			count = len - total ;

		memcpy (&(ppaf24->samples [ppaf24->write_count * ppaf24->channels]), &(ptr [total]), count * sizeof (int)) ;
		total += count ;
		ppaf24->write_count += count / ppaf24->channels ;

		if (ppaf24->write_count >= PAF24_SAMPLES_PER_BLOCK)
			paf24_write_block (psf, ppaf24) ;
		} ;

	return total ;
} /* paf24_write */

static sf_count_t
paf24_write_s (SF_PRIVATE *psf, const short *ptr, sf_count_t len)
{	BUF_UNION		ubuf ;
	PAF24_PRIVATE 	*ppaf24 ;
	int				*iptr ;
	int				k, bufferlen, writecount = 0, count ;
	sf_count_t		total = 0 ;

	if (psf->codec_data == NULL)
		return 0 ;
	ppaf24 = (PAF24_PRIVATE*) psf->codec_data ;

	iptr = ubuf.ibuf ;
	bufferlen = ARRAY_LEN (ubuf.ibuf) ;
	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : len ;
		for (k = 0 ; k < writecount ; k++)
			iptr [k] = ptr [total + k] << 16 ;
		count = paf24_write (psf, ppaf24, iptr, writecount) ;
		total += count ;
		len -= writecount ;
		if (count != writecount)
			break ;
		} ;
	return total ;
} /* paf24_write_s */

static sf_count_t
paf24_write_i (SF_PRIVATE *psf, const int *ptr, sf_count_t len)
{	PAF24_PRIVATE 	*ppaf24 ;
	int				writecount, count ;
	sf_count_t		total = 0 ;

	if (psf->codec_data == NULL)
		return 0 ;
	ppaf24 = (PAF24_PRIVATE*) psf->codec_data ;

	while (len > 0)
	{	writecount = (len > 0x10000000) ? 0x10000000 : (int) len ;

		count = paf24_write (psf, ppaf24, ptr, writecount) ;

		total += count ;
		len -= count ;
		if (count != writecount)
			break ;
		} ;

	return total ;
} /* paf24_write_i */

static sf_count_t
paf24_write_f (SF_PRIVATE *psf, const float *ptr, sf_count_t len)
{	BUF_UNION		ubuf ;
	PAF24_PRIVATE 	*ppaf24 ;
	int				*iptr ;
	int				k, bufferlen, writecount = 0, count ;
	sf_count_t		total = 0 ;
	float			normfact ;

	if (psf->codec_data == NULL)
		return 0 ;
	ppaf24 = (PAF24_PRIVATE*) psf->codec_data ;

	normfact = (psf->norm_float == SF_TRUE) ? (1.0 * 0x7FFFFFFF) : (1.0 / 0x100) ;

	iptr = ubuf.ibuf ;
	bufferlen = ARRAY_LEN (ubuf.ibuf) ;
	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : len ;
		for (k = 0 ; k < writecount ; k++)
			iptr [k] = lrintf (normfact * ptr [total + k]) ;
		count = paf24_write (psf, ppaf24, iptr, writecount) ;
		total += count ;
		len -= writecount ;
		if (count != writecount)
			break ;
		} ;

	return total ;
} /* paf24_write_f */

static sf_count_t
paf24_write_d (SF_PRIVATE *psf, const double *ptr, sf_count_t len)
{	BUF_UNION		ubuf ;
	PAF24_PRIVATE 	*ppaf24 ;
	int				*iptr ;
	int				k, bufferlen, writecount = 0, count ;
	sf_count_t		total = 0 ;
	double			normfact ;

	if (psf->codec_data == NULL)
		return 0 ;
	ppaf24 = (PAF24_PRIVATE*) psf->codec_data ;

	normfact = (psf->norm_double == SF_TRUE) ? (1.0 * 0x7FFFFFFF) : (1.0 / 0x100) ;

	iptr = ubuf.ibuf ;
	bufferlen = ARRAY_LEN (ubuf.ibuf) ;
	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : len ;
		for (k = 0 ; k < writecount ; k++)
			iptr [k] = lrint (normfact * ptr [total+k]) ;
		count = paf24_write (psf, ppaf24, iptr, writecount) ;
		total += count ;
		len -= writecount ;
		if (count != writecount)
			break ;
		} ;

	return total ;
} /* paf24_write_d */

