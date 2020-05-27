/*
** Copyright (C) 2002-2017 Erik de Castro Lopo <erikd@mega-nerd.com>
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
*/

#define	SDS_DATA_OFFSET				0x15
#define SDS_BLOCK_SIZE				127

#define SDS_AUDIO_BYTES_PER_BLOCK	120

#define SDS_3BYTE_TO_INT_DECODE(x) (((x) & 0x7F) | (((x) & 0x7F00) >> 1) | (((x) & 0x7F0000) >> 2))
#define SDS_INT_TO_3BYTE_ENCODE(x) (((x) & 0x7F) | (((x) << 1) & 0x7F00) | (((x) << 2) & 0x7F0000))

/*------------------------------------------------------------------------------
** Typedefs.
*/

typedef struct tag_SDS_PRIVATE
{	int bitwidth, frames ;
	int	samplesperblock, total_blocks ;

	int (*reader) (SF_PRIVATE *psf, struct tag_SDS_PRIVATE *psds) ;
	int (*writer) (SF_PRIVATE *psf, struct tag_SDS_PRIVATE *psds) ;

	int read_block, read_count ;
	unsigned char read_data [SDS_BLOCK_SIZE] ;
	int	read_samples [SDS_BLOCK_SIZE / 2] ; /* Maximum samples per block */

	int write_block, write_count ;
	int total_written ;
	unsigned char write_data [SDS_BLOCK_SIZE] ;
	int	write_samples [SDS_BLOCK_SIZE / 2] ; /* Maximum samples per block */
} SDS_PRIVATE ;

/*------------------------------------------------------------------------------
** Private static functions.
*/

static int	sds_close	(SF_PRIVATE *psf) ;

static int	sds_write_header (SF_PRIVATE *psf, int calc_length) ;
static int	sds_read_header (SF_PRIVATE *psf, SDS_PRIVATE *psds) ;

static int	sds_init (SF_PRIVATE *psf, SDS_PRIVATE *psds) ;

static sf_count_t sds_read_s (SF_PRIVATE *psf, short *ptr, sf_count_t len) ;
static sf_count_t sds_read_i (SF_PRIVATE *psf, int *ptr, sf_count_t len) ;
static sf_count_t sds_read_f (SF_PRIVATE *psf, float *ptr, sf_count_t len) ;
static sf_count_t sds_read_d (SF_PRIVATE *psf, double *ptr, sf_count_t len) ;

static sf_count_t sds_write_s (SF_PRIVATE *psf, const short *ptr, sf_count_t len) ;
static sf_count_t sds_write_i (SF_PRIVATE *psf, const int *ptr, sf_count_t len) ;
static sf_count_t sds_write_f (SF_PRIVATE *psf, const float *ptr, sf_count_t len) ;
static sf_count_t sds_write_d (SF_PRIVATE *psf, const double *ptr, sf_count_t len) ;

static sf_count_t sds_seek (SF_PRIVATE *psf, int mode, sf_count_t offset) ;
static int sds_byterate (SF_PRIVATE * psf) ;

static int sds_2byte_read (SF_PRIVATE *psf, SDS_PRIVATE *psds) ;
static int sds_3byte_read (SF_PRIVATE *psf, SDS_PRIVATE *psds) ;
static int sds_4byte_read (SF_PRIVATE *psf, SDS_PRIVATE *psds) ;

static int sds_read (SF_PRIVATE *psf, SDS_PRIVATE *psds, int *iptr, int readcount) ;

static int sds_2byte_write (SF_PRIVATE *psf, SDS_PRIVATE *psds) ;
static int sds_3byte_write (SF_PRIVATE *psf, SDS_PRIVATE *psds) ;
static int sds_4byte_write (SF_PRIVATE *psf, SDS_PRIVATE *psds) ;

static int sds_write (SF_PRIVATE *psf, SDS_PRIVATE *psds, const int *iptr, int writecount) ;

/*------------------------------------------------------------------------------
** Public function.
*/

int
sds_open	(SF_PRIVATE *psf)
{	SDS_PRIVATE	*psds ;
	int			error = 0 ;

	/* Hmmmm, need this here to pass update_header_test. */
	psf->sf.frames = 0 ;

	if (! (psds = calloc (1, sizeof (SDS_PRIVATE))))
		return SFE_MALLOC_FAILED ;
	psf->codec_data = psds ;

	if (psf->file.mode == SFM_READ || (psf->file.mode == SFM_RDWR && psf->filelength > 0))
	{	if ((error = sds_read_header (psf, psds)))
			return error ;
		} ;

	if ((SF_CONTAINER (psf->sf.format)) != SF_FORMAT_SDS)
		return	SFE_BAD_OPEN_FORMAT ;

	if (psf->file.mode == SFM_WRITE || psf->file.mode == SFM_RDWR)
	{	if (sds_write_header (psf, SF_FALSE))
			return psf->error ;

		psf->write_header = sds_write_header ;

		psf_fseek (psf, SDS_DATA_OFFSET, SEEK_SET) ;
		} ;

	if ((error = sds_init (psf, psds)) != 0)
		return error ;

	psf->container_close = sds_close ;
	psf->seek = sds_seek ;
	psf->byterate = sds_byterate ;

	psf->blockwidth = 0 ;

	return error ;
} /* sds_open */

/*------------------------------------------------------------------------------
*/

static int
sds_close	(SF_PRIVATE *psf)
{
	if (psf->file.mode == SFM_WRITE || psf->file.mode == SFM_RDWR)
	{	SDS_PRIVATE *psds ;

		if ((psds = (SDS_PRIVATE *) psf->codec_data) == NULL)
		{	psf_log_printf (psf, "*** Bad psf->codec_data ptr.\n") ;
			return SFE_INTERNAL ;
			} ;

		if (psds->write_count > 0)
		{	memset (&(psds->write_data [psds->write_count]), 0, (psds->samplesperblock - psds->write_count) * sizeof (int)) ;
			psds->writer (psf, psds) ;
			} ;

		sds_write_header (psf, SF_TRUE) ;
		} ;

	return 0 ;
} /* sds_close */

static int
sds_init (SF_PRIVATE *psf, SDS_PRIVATE *psds)
{
	if (psds->bitwidth < 8 || psds->bitwidth > 28)
		return (psf->error = SFE_SDS_BAD_BIT_WIDTH) ;

	if (psds->bitwidth < 14)
	{	psds->reader = sds_2byte_read ;
		psds->writer = sds_2byte_write ;
		psds->samplesperblock = SDS_AUDIO_BYTES_PER_BLOCK / 2 ;
		}
	else if (psds->bitwidth < 21)
	{	psds->reader = sds_3byte_read ;
		psds->writer = sds_3byte_write ;
		psds->samplesperblock = SDS_AUDIO_BYTES_PER_BLOCK / 3 ;
		}
	else
	{	psds->reader = sds_4byte_read ;
		psds->writer = sds_4byte_write ;
		psds->samplesperblock = SDS_AUDIO_BYTES_PER_BLOCK / 4 ;
		} ;

	if (psf->file.mode == SFM_READ || psf->file.mode == SFM_RDWR)
	{	psf->read_short		= sds_read_s ;
		psf->read_int		= sds_read_i ;
		psf->read_float		= sds_read_f ;
		psf->read_double	= sds_read_d ;

		/* Read first block. */
		psds->reader (psf, psds) ;
		} ;

	if (psf->file.mode == SFM_WRITE || psf->file.mode == SFM_RDWR)
	{	psf->write_short	= sds_write_s ;
		psf->write_int		= sds_write_i ;
		psf->write_float	= sds_write_f ;
		psf->write_double	= sds_write_d ;
		} ;

	return 0 ;
} /* sds_init */

static int
sds_read_header (SF_PRIVATE *psf, SDS_PRIVATE *psds)
{	unsigned char	channel, bitwidth, loop_type, byte ;
	unsigned short	sample_no, marker ;
	unsigned int	samp_period, data_length, sustain_loop_start, sustain_loop_end ;
	int		bytesread, blockcount ;

	/* Set position to start of file to begin reading header. */
	bytesread = psf_binheader_readf (psf, "pE211", 0, &marker, &channel, &byte) ;

	if (marker != 0xF07E || byte != 0x01)
		return SFE_SDS_NOT_SDS ;

	bytesread += psf_binheader_readf (psf, "e2", &sample_no) ;
	sample_no = SDS_3BYTE_TO_INT_DECODE (sample_no) ;

	psf_log_printf (psf, "Midi Sample Dump Standard (.sds)\nF07E\n"
						" Midi Channel  : %d\n Sample Number : %d\n",
						channel, sample_no) ;

	bytesread += psf_binheader_readf (psf, "e13", &bitwidth, &samp_period) ;

	samp_period = SDS_3BYTE_TO_INT_DECODE (samp_period) ;

	psds->bitwidth = bitwidth ;

	if (psds->bitwidth > 1)
		psf_log_printf (psf, " Bit Width     : %d\n", psds->bitwidth) ;
	else
	{	psf_log_printf (psf, " Bit Width     : %d (should be > 1)\n", psds->bitwidth) ;
		return SFE_SDS_BAD_BIT_WIDTH ;
		} ;

	if (samp_period > 0)
	{	psf->sf.samplerate = 1000000000 / samp_period ;

		psf_log_printf (psf, " Sample Period : %d\n"
							" Sample Rate   : %d\n",
							samp_period, psf->sf.samplerate) ;
		}
	else
	{	psf->sf.samplerate = 16000 ;

		psf_log_printf (psf, " Sample Period : %d (should be > 0)\n"
							" Sample Rate   : %d (guessed)\n",
							samp_period, psf->sf.samplerate) ;
		} ;

	bytesread += psf_binheader_readf (psf, "e3331", &data_length, &sustain_loop_start, &sustain_loop_end, &loop_type) ;

	data_length = SDS_3BYTE_TO_INT_DECODE (data_length) ;

	psf->sf.frames = psds->frames = data_length ;

	sustain_loop_start = SDS_3BYTE_TO_INT_DECODE (sustain_loop_start) ;
	sustain_loop_end = SDS_3BYTE_TO_INT_DECODE (sustain_loop_end) ;

	psf_log_printf (psf, 	" Sustain Loop\n"
							"     Start     : %d\n"
							"     End       : %d\n"
							"     Loop Type : %d\n",
			sustain_loop_start, sustain_loop_end, loop_type) ;

	psf->dataoffset = SDS_DATA_OFFSET ;
	psf->datalength = psf->filelength - psf->dataoffset ;

	bytesread += psf_binheader_readf (psf, "1", &byte) ;
	if (byte != 0xF7)
		psf_log_printf (psf, "bad end : %X\n", byte & 0xFF) ;

	for (blockcount = 0 ; bytesread < psf->filelength ; blockcount++)
	{
		bytesread += psf_fread (&marker, 1, 2, psf) ;

		if (marker == 0)
			break ;

		psf_fseek (psf, SDS_BLOCK_SIZE - 2, SEEK_CUR) ;
		bytesread += SDS_BLOCK_SIZE - 2 ;
		} ;

	psf_log_printf (psf, "\nBlocks         : %d\n", blockcount) ;
	psds->total_blocks = blockcount ;

	psds->samplesperblock = SDS_AUDIO_BYTES_PER_BLOCK / ((psds->bitwidth + 6) / 7) ;
	psf_log_printf (psf, "Samples/Block  : %d\n", psds->samplesperblock) ;

	psf_log_printf (psf, "Frames         : %d\n", blockcount * psds->samplesperblock) ;

	/* Always Mono */
	psf->sf.channels = 1 ;
	psf->sf.sections = 1 ;

	/*
	** Lie to the user about PCM bit width. Always round up to
	** the next multiple of 8.
	*/
	switch ((psds->bitwidth + 7) / 8)
	{	case 1 :
			psf->sf.format = SF_FORMAT_SDS | SF_FORMAT_PCM_S8 ;
			break ;

		case 2 :
			psf->sf.format = SF_FORMAT_SDS | SF_FORMAT_PCM_16 ;
			break ;

		case 3 :
			psf->sf.format = SF_FORMAT_SDS | SF_FORMAT_PCM_24 ;
			break ;

		case 4 :
			psf->sf.format = SF_FORMAT_SDS | SF_FORMAT_PCM_32 ;
			break ;

		default :
			psf_log_printf (psf, "*** Weird byte width (%d)\n", (psds->bitwidth + 7) / 8) ;
			return SFE_SDS_BAD_BIT_WIDTH ;
		} ;

	psf_fseek (psf, SDS_DATA_OFFSET, SEEK_SET) ;

	return 0 ;
} /* sds_read_header */

static int
sds_write_header (SF_PRIVATE *psf, int calc_length)
{	SDS_PRIVATE *psds ;
	sf_count_t	current ;
	int samp_period, data_length, sustain_loop_start, sustain_loop_end ;
	unsigned char loop_type = 0 ;

	if ((psds = (SDS_PRIVATE *) psf->codec_data) == NULL)
	{	psf_log_printf (psf, "*** Bad psf->codec_data ptr.\n") ;
		return SFE_INTERNAL ;
		} ;

	if (psf->pipeoffset > 0)
		return 0 ;

	current = psf_ftell (psf) ;

	if (calc_length)
		psf->sf.frames = psds->total_written ;

	if (psds->write_count > 0)
	{	int current_count = psds->write_count ;
		int current_block = psds->write_block ;

		psds->writer (psf, psds) ;

		psf_fseek (psf, -1 * SDS_BLOCK_SIZE, SEEK_CUR) ;

		psds->write_count = current_count ;
		psds->write_block = current_block ;
		} ;

	/* Reset the current header length to zero. */
	psf->header.ptr [0] = 0 ;
	psf->header.indx = 0 ;

	if (psf->is_pipe == SF_FALSE)
		psf_fseek (psf, 0, SEEK_SET) ;

	psf_binheader_writef (psf, "E211", BHW2 (0xF07E), BHW1 (0), BHW1 (1)) ;

	switch (SF_CODEC (psf->sf.format))
	{	case SF_FORMAT_PCM_S8 :
				psds->bitwidth = 8 ;
				break ;
		case SF_FORMAT_PCM_16 :
				psds->bitwidth = 16 ;
				break ;
		case SF_FORMAT_PCM_24 :
				psds->bitwidth = 24 ;
				break ;
		default:
			return SFE_SDS_BAD_BIT_WIDTH ;
		} ;

	samp_period = SDS_INT_TO_3BYTE_ENCODE (1000000000 / psf->sf.samplerate) ;

	psf_binheader_writef (psf, "e213", BHW2 (0), BHW1 (psds->bitwidth), BHW3 (samp_period)) ;

	data_length			= SDS_INT_TO_3BYTE_ENCODE (psds->total_written) ;
	sustain_loop_start	= SDS_INT_TO_3BYTE_ENCODE (0) ;
	sustain_loop_end	= SDS_INT_TO_3BYTE_ENCODE (0) ;

	psf_binheader_writef (psf, "e33311", BHW3 (data_length), BHW3 (sustain_loop_start), BHW3 (sustain_loop_end), BHW1 (loop_type), BHW1 (0xF7)) ;

	/* Header construction complete so write it out. */
	psf_fwrite (psf->header.ptr, psf->header.indx, 1, psf) ;

	if (psf->error)
		return psf->error ;

	psf->dataoffset = psf->header.indx ;
	psf->datalength = psds->write_block * SDS_BLOCK_SIZE ;

	if (current > 0)
		psf_fseek (psf, current, SEEK_SET) ;

	return psf->error ;
} /* sds_write_header */


/*------------------------------------------------------------------------------
*/

static int
sds_2byte_read (SF_PRIVATE *psf, SDS_PRIVATE *psds)
{	unsigned char *ucptr, checksum ;
	unsigned int sample ;
	int 	k ;

	psds->read_block ++ ;
	psds->read_count = 0 ;

	if (psds->read_block * psds->samplesperblock > psds->frames)
	{	memset (psds->read_samples, 0, psds->samplesperblock * sizeof (int)) ;
		return 1 ;
		} ;

	if ((k = psf_fread (psds->read_data, 1, SDS_BLOCK_SIZE, psf)) != SDS_BLOCK_SIZE)
		psf_log_printf (psf, "*** Warning : short read (%d != %d).\n", k, SDS_BLOCK_SIZE) ;

	if (psds->read_data [0] != 0xF0)
	{	printf ("Error A : %02X\n", psds->read_data [0] & 0xFF) ;
		} ;

	checksum = psds->read_data [1] ;
	if (checksum != 0x7E)
	{	printf ("Error 1 : %02X\n", checksum & 0xFF) ;
		}

	for (k = 2 ; k <= SDS_BLOCK_SIZE - 3 ; k ++)
		checksum ^= psds->read_data [k] ;

	checksum &= 0x7F ;

	if (checksum != psds->read_data [SDS_BLOCK_SIZE - 2])
	{	psf_log_printf (psf, "Block %d : checksum is %02X should be %02X\n", psds->read_data [4], checksum, psds->read_data [SDS_BLOCK_SIZE - 2]) ;
		} ;

	ucptr = psds->read_data + 5 ;
	for (k = 0 ; k < 120 ; k += 2)
	{	sample = arith_shift_left (ucptr [k], 25) + arith_shift_left (ucptr [k + 1], 18) ;
		psds->read_samples [k / 2] = (int) (sample - 0x80000000) ;
		} ;

	return 1 ;
} /* sds_2byte_read */

static int
sds_3byte_read (SF_PRIVATE *psf, SDS_PRIVATE *psds)
{	unsigned char *ucptr, checksum ;
	unsigned int sample ;
	int 	k ;

	psds->read_block ++ ;
	psds->read_count = 0 ;

	if (psds->read_block * psds->samplesperblock > psds->frames)
	{	memset (psds->read_samples, 0, psds->samplesperblock * sizeof (int)) ;
		return 1 ;
		} ;

	if ((k = psf_fread (psds->read_data, 1, SDS_BLOCK_SIZE, psf)) != SDS_BLOCK_SIZE)
		psf_log_printf (psf, "*** Warning : short read (%d != %d).\n", k, SDS_BLOCK_SIZE) ;

	if (psds->read_data [0] != 0xF0)
	{	printf ("Error A : %02X\n", psds->read_data [0] & 0xFF) ;
		} ;

	checksum = psds->read_data [1] ;
	if (checksum != 0x7E)
	{	printf ("Error 1 : %02X\n", checksum & 0xFF) ;
		}

	for (k = 2 ; k <= SDS_BLOCK_SIZE - 3 ; k ++)
		checksum ^= psds->read_data [k] ;

	checksum &= 0x7F ;

	if (checksum != psds->read_data [SDS_BLOCK_SIZE - 2])
	{	psf_log_printf (psf, "Block %d : checksum is %02X should be %02X\n", psds->read_data [4], checksum, psds->read_data [SDS_BLOCK_SIZE - 2]) ;
		} ;

	ucptr = psds->read_data + 5 ;
	for (k = 0 ; k < 120 ; k += 3)
	{	sample = (((uint32_t) ucptr [k]) << 25) + (ucptr [k + 1] << 18) + (ucptr [k + 2] << 11) ;
		psds->read_samples [k / 3] = (int) (sample - 0x80000000) ;
		} ;

	return 1 ;
} /* sds_3byte_read */

static int
sds_4byte_read (SF_PRIVATE *psf, SDS_PRIVATE *psds)
{	unsigned char *ucptr, checksum ;
	uint32_t sample ;
	int 	k ;

	psds->read_block ++ ;
	psds->read_count = 0 ;

	if (psds->read_block * psds->samplesperblock > psds->frames)
	{	memset (psds->read_samples, 0, psds->samplesperblock * sizeof (int)) ;
		return 1 ;
		} ;

	if ((k = psf_fread (psds->read_data, 1, SDS_BLOCK_SIZE, psf)) != SDS_BLOCK_SIZE)
		psf_log_printf (psf, "*** Warning : short read (%d != %d).\n", k, SDS_BLOCK_SIZE) ;

	if (psds->read_data [0] != 0xF0)
	{	printf ("Error A : %02X\n", psds->read_data [0] & 0xFF) ;
		} ;

	checksum = psds->read_data [1] ;
	if (checksum != 0x7E)
	{	printf ("Error 1 : %02X\n", checksum & 0xFF) ;
		}

	for (k = 2 ; k <= SDS_BLOCK_SIZE - 3 ; k ++)
		checksum ^= psds->read_data [k] ;

	checksum &= 0x7F ;

	if (checksum != psds->read_data [SDS_BLOCK_SIZE - 2])
	{	psf_log_printf (psf, "Block %d : checksum is %02X should be %02X\n", psds->read_data [4], checksum, psds->read_data [SDS_BLOCK_SIZE - 2]) ;
		} ;

	ucptr = psds->read_data + 5 ;
	for (k = 0 ; k < 120 ; k += 4)
	{	sample = (((uint32_t) ucptr [k]) << 25) + (ucptr [k + 1] << 18) + (ucptr [k + 2] << 11) + (ucptr [k + 3] << 4) ;
		psds->read_samples [k / 4] = (int) (sample - 0x80000000) ;
		} ;

	return 1 ;
} /* sds_4byte_read */


static sf_count_t
sds_read_s (SF_PRIVATE *psf, short *ptr, sf_count_t len)
{	BUF_UNION	ubuf ;
	SDS_PRIVATE	*psds ;
	int			*iptr ;
	int			k, bufferlen, readcount, count ;
	sf_count_t	total = 0 ;

	if (psf->codec_data == NULL)
		return 0 ;
	psds = (SDS_PRIVATE*) psf->codec_data ;

	iptr = ubuf.ibuf ;
	bufferlen = ARRAY_LEN (ubuf.ibuf) ;
	while (len > 0)
	{	readcount = (len >= bufferlen) ? bufferlen : len ;
		count = sds_read (psf, psds, iptr, readcount) ;
		for (k = 0 ; k < readcount ; k++)
			ptr [total + k] = iptr [k] >> 16 ;
		total += count ;
		len -= readcount ;
		} ;

	return total ;
} /* sds_read_s */

static sf_count_t
sds_read_i (SF_PRIVATE *psf, int *ptr, sf_count_t len)
{	SDS_PRIVATE *psds ;
	int			total ;

	if (psf->codec_data == NULL)
		return 0 ;
	psds = (SDS_PRIVATE*) psf->codec_data ;

	total = sds_read (psf, psds, ptr, len) ;

	return total ;
} /* sds_read_i */

static sf_count_t
sds_read_f (SF_PRIVATE *psf, float *ptr, sf_count_t len)
{	BUF_UNION	ubuf ;
	SDS_PRIVATE	*psds ;
	int			*iptr ;
	int			k, bufferlen, readcount, count ;
	sf_count_t	total = 0 ;
	float		normfact ;

	if (psf->codec_data == NULL)
		return 0 ;
	psds = (SDS_PRIVATE*) psf->codec_data ;

	if (psf->norm_float == SF_TRUE)
		normfact = 1.0 / 0x80000000 ;
	else
		normfact = 1.0 / (1 << psds->bitwidth) ;

	iptr = ubuf.ibuf ;
	bufferlen = ARRAY_LEN (ubuf.ibuf) ;
	while (len > 0)
	{	readcount = (len >= bufferlen) ? bufferlen : len ;
		count = sds_read (psf, psds, iptr, readcount) ;
		for (k = 0 ; k < readcount ; k++)
			ptr [total + k] = normfact * iptr [k] ;
		total += count ;
		len -= readcount ;
		} ;

	return total ;
} /* sds_read_f */

static sf_count_t
sds_read_d (SF_PRIVATE *psf, double *ptr, sf_count_t len)
{	BUF_UNION	ubuf ;
	SDS_PRIVATE	*psds ;
	int			*iptr ;
	int			k, bufferlen, readcount, count ;
	sf_count_t	total = 0 ;
	double		normfact ;

	if (psf->codec_data == NULL)
		return 0 ;
	psds = (SDS_PRIVATE*) psf->codec_data ;

	if (psf->norm_double == SF_TRUE)
		normfact = 1.0 / 0x80000000 ;
	else
		normfact = 1.0 / (1 << psds->bitwidth) ;

	iptr = ubuf.ibuf ;
	bufferlen = ARRAY_LEN (ubuf.ibuf) ;
	while (len > 0)
	{	readcount = (len >= bufferlen) ? bufferlen : len ;
		count = sds_read (psf, psds, iptr, readcount) ;
		for (k = 0 ; k < readcount ; k++)
			ptr [total + k] = normfact * iptr [k] ;
		total += count ;
		len -= readcount ;
		} ;

	return total ;
} /* sds_read_d */

static int
sds_read (SF_PRIVATE *psf, SDS_PRIVATE *psds, int *ptr, int len)
{	int	count, total = 0 ;

	while (total < len)
	{	if (psds->read_block * psds->samplesperblock >= psds->frames)
		{	memset (&(ptr [total]), 0, (len - total) * sizeof (int)) ;
			return total ;
			} ;

		if (psds->read_count >= psds->samplesperblock)
			psds->reader (psf, psds) ;

		count = (psds->samplesperblock - psds->read_count) ;
		count = (len - total > count) ? count : len - total ;

		memcpy (&(ptr [total]), &(psds->read_samples [psds->read_count]), count * sizeof (int)) ;
		total += count ;
		psds->read_count += count ;
		} ;

	return total ;
} /* sds_read */

/*==============================================================================
*/

static sf_count_t
sds_seek (SF_PRIVATE *psf, int mode, sf_count_t seek_from_start)
{	SDS_PRIVATE	*psds ;
	sf_count_t	file_offset ;
	int			newblock, newsample ;

	if ((psds = psf->codec_data) == NULL)
	{	psf->error = SFE_INTERNAL ;
		return PSF_SEEK_ERROR ;
		} ;

	if (psf->datalength < 0 || psf->dataoffset < 0)
	{	psf->error = SFE_BAD_SEEK ;
		return PSF_SEEK_ERROR ;
		} ;

	if (seek_from_start < 0 || seek_from_start > psf->sf.frames)
	{	psf->error = SFE_BAD_SEEK ;
		return PSF_SEEK_ERROR ;
		} ;

	if (mode == SFM_READ && psds->write_count > 0)
		psds->writer (psf, psds) ;

	newblock = seek_from_start / psds->samplesperblock ;
	newsample = seek_from_start % psds->samplesperblock ;

	switch (mode)
	{	case SFM_READ :
			if (newblock > psds->total_blocks)
			{	psf->error = SFE_BAD_SEEK ;
				return PSF_SEEK_ERROR ;
				} ;

			file_offset = psf->dataoffset + newblock * SDS_BLOCK_SIZE ;

			if (psf_fseek (psf, file_offset, SEEK_SET) != file_offset)
			{	psf->error = SFE_SEEK_FAILED ;
				return PSF_SEEK_ERROR ;
				} ;

			psds->read_block = newblock ;
			psds->reader (psf, psds) ;
			psds->read_count = newsample ;
			break ;

		case SFM_WRITE :
			if (newblock > psds->total_blocks)
			{	psf->error = SFE_BAD_SEEK ;
				return PSF_SEEK_ERROR ;
				} ;

			file_offset = psf->dataoffset + newblock * SDS_BLOCK_SIZE ;

			if (psf_fseek (psf, file_offset, SEEK_SET) != file_offset)
			{	psf->error = SFE_SEEK_FAILED ;
				return PSF_SEEK_ERROR ;
				} ;

			psds->write_block = newblock ;
			psds->reader (psf, psds) ;
			psds->write_count = newsample ;
			break ;

		default :
			psf->error = SFE_BAD_SEEK ;
			return PSF_SEEK_ERROR ;
			break ;
		} ;

	return seek_from_start ;
} /* sds_seek */

static int
sds_byterate (SF_PRIVATE * psf)
{
	if (psf->file.mode == SFM_READ)
		return (psf->datalength * psf->sf.samplerate) / psf->sf.frames ;

	return -1 ;
} /* sds_byterate */

/*==============================================================================
*/

static int
sds_2byte_write (SF_PRIVATE *psf, SDS_PRIVATE *psds)
{	unsigned char *ucptr, checksum ;
	unsigned int sample ;
	int 	k ;

	psds->write_data [0] = 0xF0 ;
	psds->write_data [1] = 0x7E ;
	psds->write_data [2] = 0 ;							/* Channel number */
	psds->write_data [3] = 2 ;
	psds->write_data [4] = psds->write_block & 0x7F ;	/* Packet number */

	ucptr = psds->write_data + 5 ;
	for (k = 0 ; k < 120 ; k += 2)
	{	sample = psds->write_samples [k / 2] ;
		sample += 0x80000000 ;
		ucptr [k] = (sample >> 25) & 0x7F ;
		ucptr [k + 1] = (sample >> 18) & 0x7F ;
		} ;

	checksum = psds->write_data [1] ;
	for (k = 2 ; k <= SDS_BLOCK_SIZE - 3 ; k ++)
		checksum ^= psds->write_data [k] ;
	checksum &= 0x7F ;

	psds->write_data [SDS_BLOCK_SIZE - 2] = checksum ;
	psds->write_data [SDS_BLOCK_SIZE - 1] = 0xF7 ;

	if ((k = psf_fwrite (psds->write_data, 1, SDS_BLOCK_SIZE, psf)) != SDS_BLOCK_SIZE)
		psf_log_printf (psf, "*** Warning : psf_fwrite (%d != %d).\n", k, SDS_BLOCK_SIZE) ;

	psds->write_block ++ ;
	psds->write_count = 0 ;

	if (psds->write_block > psds->total_blocks)
		psds->total_blocks = psds->write_block ;
	psds->frames = psds->total_blocks * psds->samplesperblock ;

	return 1 ;
} /* sds_2byte_write */

static int
sds_3byte_write (SF_PRIVATE *psf, SDS_PRIVATE *psds)
{	unsigned char *ucptr, checksum ;
	unsigned int sample ;
	int 	k ;

	psds->write_data [0] = 0xF0 ;
	psds->write_data [1] = 0x7E ;
	psds->write_data [2] = 0 ;							/* Channel number */
	psds->write_data [3] = 2 ;
	psds->write_data [4] = psds->write_block & 0x7F ;	/* Packet number */

	ucptr = psds->write_data + 5 ;
	for (k = 0 ; k < 120 ; k += 3)
	{	sample = psds->write_samples [k / 3] ;
		sample += 0x80000000 ;
		ucptr [k] = (sample >> 25) & 0x7F ;
		ucptr [k + 1] = (sample >> 18) & 0x7F ;
		ucptr [k + 2] = (sample >> 11) & 0x7F ;
		} ;

	checksum = psds->write_data [1] ;
	for (k = 2 ; k <= SDS_BLOCK_SIZE - 3 ; k ++)
		checksum ^= psds->write_data [k] ;
	checksum &= 0x7F ;

	psds->write_data [SDS_BLOCK_SIZE - 2] = checksum ;
	psds->write_data [SDS_BLOCK_SIZE - 1] = 0xF7 ;

	if ((k = psf_fwrite (psds->write_data, 1, SDS_BLOCK_SIZE, psf)) != SDS_BLOCK_SIZE)
		psf_log_printf (psf, "*** Warning : psf_fwrite (%d != %d).\n", k, SDS_BLOCK_SIZE) ;

	psds->write_block ++ ;
	psds->write_count = 0 ;

	if (psds->write_block > psds->total_blocks)
		psds->total_blocks = psds->write_block ;
	psds->frames = psds->total_blocks * psds->samplesperblock ;

	return 1 ;
} /* sds_3byte_write */

static int
sds_4byte_write (SF_PRIVATE *psf, SDS_PRIVATE *psds)
{	unsigned char *ucptr, checksum ;
	unsigned int sample ;
	int 	k ;

	psds->write_data [0] = 0xF0 ;
	psds->write_data [1] = 0x7E ;
	psds->write_data [2] = 0 ;							/* Channel number */
	psds->write_data [3] = 2 ;
	psds->write_data [4] = psds->write_block & 0x7F ;	/* Packet number */

	ucptr = psds->write_data + 5 ;
	for (k = 0 ; k < 120 ; k += 4)
	{	sample = psds->write_samples [k / 4] ;
		sample += 0x80000000 ;
		ucptr [k] = (sample >> 25) & 0x7F ;
		ucptr [k + 1] = (sample >> 18) & 0x7F ;
		ucptr [k + 2] = (sample >> 11) & 0x7F ;
		ucptr [k + 3] = (sample >> 4) & 0x7F ;
		} ;

	checksum = psds->write_data [1] ;
	for (k = 2 ; k <= SDS_BLOCK_SIZE - 3 ; k ++)
		checksum ^= psds->write_data [k] ;
	checksum &= 0x7F ;

	psds->write_data [SDS_BLOCK_SIZE - 2] = checksum ;
	psds->write_data [SDS_BLOCK_SIZE - 1] = 0xF7 ;

	if ((k = psf_fwrite (psds->write_data, 1, SDS_BLOCK_SIZE, psf)) != SDS_BLOCK_SIZE)
		psf_log_printf (psf, "*** Warning : psf_fwrite (%d != %d).\n", k, SDS_BLOCK_SIZE) ;

	psds->write_block ++ ;
	psds->write_count = 0 ;

	if (psds->write_block > psds->total_blocks)
		psds->total_blocks = psds->write_block ;
	psds->frames = psds->total_blocks * psds->samplesperblock ;

	return 1 ;
} /* sds_4byte_write */

static sf_count_t
sds_write_s (SF_PRIVATE *psf, const short *ptr, sf_count_t len)
{	BUF_UNION	ubuf ;
	SDS_PRIVATE	*psds ;
	int			*iptr ;
	int			k, bufferlen, writecount, count ;
	sf_count_t	total = 0 ;

	if (psf->codec_data == NULL)
		return 0 ;
	psds = (SDS_PRIVATE*) psf->codec_data ;
	psds->total_written += len ;

	iptr = ubuf.ibuf ;
	bufferlen = ARRAY_LEN (ubuf.ibuf) ;
	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : len ;
		for (k = 0 ; k < writecount ; k++)
			iptr [k] = arith_shift_left (ptr [total + k], 16) ;
		count = sds_write (psf, psds, iptr, writecount) ;
		total += count ;
		len -= writecount ;
		} ;

	return total ;
} /* sds_write_s */

static sf_count_t
sds_write_i (SF_PRIVATE *psf, const int *ptr, sf_count_t len)
{	SDS_PRIVATE *psds ;
	int			total ;

	if (psf->codec_data == NULL)
		return 0 ;
	psds = (SDS_PRIVATE*) psf->codec_data ;
	psds->total_written += len ;

	total = sds_write (psf, psds, ptr, len) ;

	return total ;
} /* sds_write_i */

static sf_count_t
sds_write_f (SF_PRIVATE *psf, const float *ptr, sf_count_t len)
{	BUF_UNION	ubuf ;
	SDS_PRIVATE	*psds ;
	int			*iptr ;
	int			k, bufferlen, writecount, count ;
	sf_count_t	total = 0 ;
	float		normfact ;

	if (psf->codec_data == NULL)
		return 0 ;
	psds = (SDS_PRIVATE*) psf->codec_data ;
	psds->total_written += len ;

	if (psf->norm_float == SF_TRUE)
		normfact = 1.0 * 0x80000000 ;
	else
		normfact = 1.0 * (1 << psds->bitwidth) ;

	iptr = ubuf.ibuf ;
	bufferlen = ARRAY_LEN (ubuf.ibuf) ;
	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : len ;
		for (k = 0 ; k < writecount ; k++)
			iptr [k] = normfact * ptr [total + k] ;
		count = sds_write (psf, psds, iptr, writecount) ;
		total += count ;
		len -= writecount ;
		} ;

	return total ;
} /* sds_write_f */

static sf_count_t
sds_write_d (SF_PRIVATE *psf, const double *ptr, sf_count_t len)
{	BUF_UNION	ubuf ;
	SDS_PRIVATE	*psds ;
	int			*iptr ;
	int			k, bufferlen, writecount, count ;
	sf_count_t	total = 0 ;
	double		normfact ;

	if (psf->codec_data == NULL)
		return 0 ;
	psds = (SDS_PRIVATE*) psf->codec_data ;
	psds->total_written += len ;

	if (psf->norm_double == SF_TRUE)
		normfact = 1.0 * 0x80000000 ;
	else
		normfact = 1.0 * (1 << psds->bitwidth) ;

	iptr = ubuf.ibuf ;
	bufferlen = ARRAY_LEN (ubuf.ibuf) ;
	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : len ;
		for (k = 0 ; k < writecount ; k++)
			iptr [k] = normfact * ptr [total + k] ;
		count = sds_write (psf, psds, iptr, writecount) ;
		total += count ;
		len -= writecount ;
		} ;

	return total ;
} /* sds_write_d */

static int
sds_write (SF_PRIVATE *psf, SDS_PRIVATE *psds, const int *ptr, int len)
{	int	count, total = 0 ;

	while (total < len)
	{	count = psds->samplesperblock - psds->write_count ;
		if (count > len - total)
			count = len - total ;

		memcpy (&(psds->write_samples [psds->write_count]), &(ptr [total]), count * sizeof (int)) ;
		total += count ;
		psds->write_count += count ;

		if (psds->write_count >= psds->samplesperblock)
			psds->writer (psf, psds) ;
		} ;

	return total ;
} /* sds_write */

