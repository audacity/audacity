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

#include	"sfconfig.h"

#include	<stdio.h>
#include	<fcntl.h>
#include	<string.h>
#include	<ctype.h>

#include	"sndfile.h"
#include	"sfendian.h"
#include	"common.h"

/*------------------------------------------------------------------------------
** Macros to handle big/little endian issues.
*/

#define DOTSND_MARKER	(MAKE_MARKER ('.', 's', 'n', 'd'))
#define DNSDOT_MARKER	(MAKE_MARKER ('d', 'n', 's', '.'))

#define AU_DATA_OFFSET	24

/*------------------------------------------------------------------------------
** Known AU file encoding types.
*/

enum
{	AU_ENCODING_ULAW_8					= 1,	/* 8-bit u-law samples */
	AU_ENCODING_PCM_8					= 2,	/* 8-bit linear samples */
	AU_ENCODING_PCM_16					= 3,	/* 16-bit linear samples */
	AU_ENCODING_PCM_24					= 4,	/* 24-bit linear samples */
	AU_ENCODING_PCM_32					= 5,	/* 32-bit linear samples */

	AU_ENCODING_FLOAT					= 6,	/* floating-point samples */
	AU_ENCODING_DOUBLE					= 7,	/* double-precision float samples */
	AU_ENCODING_INDIRECT				= 8,	/* fragmented sampled data */
	AU_ENCODING_NESTED					= 9,	/* ? */
	AU_ENCODING_DSP_CORE				= 10,	/* DSP program */
	AU_ENCODING_DSP_DATA_8				= 11,	/* 8-bit fixed-point samples */
	AU_ENCODING_DSP_DATA_16				= 12,	/* 16-bit fixed-point samples */
	AU_ENCODING_DSP_DATA_24				= 13,	/* 24-bit fixed-point samples */
	AU_ENCODING_DSP_DATA_32				= 14,	/* 32-bit fixed-point samples */

	AU_ENCODING_DISPLAY					= 16,	/* non-audio display data */
	AU_ENCODING_MULAW_SQUELCH			= 17,	/* ? */
	AU_ENCODING_EMPHASIZED				= 18,	/* 16-bit linear with emphasis */
	AU_ENCODING_NEXT					= 19,	/* 16-bit linear with compression (NEXT) */
	AU_ENCODING_COMPRESSED_EMPHASIZED	= 20,	/* A combination of the two above */
	AU_ENCODING_DSP_COMMANDS			= 21,	/* Music Kit DSP commands */
	AU_ENCODING_DSP_COMMANDS_SAMPLES	= 22,	/* ? */

	AU_ENCODING_ADPCM_G721_32			= 23,	/* G721 32 kbs ADPCM - 4 bits per sample. */
	AU_ENCODING_ADPCM_G722				= 24,	/* G722 64 kbs ADPCM */
	AU_ENCODING_ADPCM_G723_24			= 25,	/* G723 24 kbs ADPCM - 3 bits per sample. */
	AU_ENCODING_ADPCM_G723_40			= 26,	/* G723 40 kbs ADPCM - 5 bits per sample. */

	AU_ENCODING_ALAW_8					= 27
} ;

/*------------------------------------------------------------------------------
** Typedefs.
*/

typedef	struct
{	int		dataoffset ;
	int		datasize ;
	int		encoding ;
    int		samplerate ;
    int		channels ;
} AU_FMT ;


/*------------------------------------------------------------------------------
** Private static functions.
*/

static	int		au_close		(SF_PRIVATE *psf) ;

static	int 	au_format_to_encoding	(int format) ;

static int		au_write_header (SF_PRIVATE *psf, int calc_length) ;
static int		au_read_header (SF_PRIVATE *psf) ;

/*------------------------------------------------------------------------------
** Public function.
*/

int
au_open	(SF_PRIVATE *psf)
{	int		subformat ;
	int		error = 0 ;

	if (psf->file.mode == SFM_READ || (psf->file.mode == SFM_RDWR && psf->filelength > 0))
	{	if ((error = au_read_header (psf)))
			return error ;
		} ;

	if ((SF_CONTAINER (psf->sf.format)) != SF_FORMAT_AU)
		return	SFE_BAD_OPEN_FORMAT ;

	subformat = SF_CODEC (psf->sf.format) ;

	if (psf->file.mode == SFM_WRITE || psf->file.mode == SFM_RDWR)
	{	psf->endian = SF_ENDIAN (psf->sf.format) ;
		if (CPU_IS_LITTLE_ENDIAN && psf->endian == SF_ENDIAN_CPU)
			psf->endian = SF_ENDIAN_LITTLE ;
		else if (psf->endian != SF_ENDIAN_LITTLE)
			psf->endian = SF_ENDIAN_BIG ;

		if (au_write_header (psf, SF_FALSE))
			return psf->error ;

		psf->write_header = au_write_header ;
		} ;

	psf->container_close = au_close ;

	psf->blockwidth = psf->bytewidth * psf->sf.channels ;

	switch (subformat)
	{	case SF_FORMAT_ULAW :	/* 8-bit Ulaw encoding. */
				ulaw_init (psf) ;
				break ;

		case SF_FORMAT_PCM_S8 :	/* 8-bit linear PCM. */
				error = pcm_init (psf) ;
				break ;

		case SF_FORMAT_PCM_16 :	/* 16-bit linear PCM. */
		case SF_FORMAT_PCM_24 :	/* 24-bit linear PCM */
		case SF_FORMAT_PCM_32 :	/* 32-bit linear PCM. */
				error = pcm_init (psf) ;
				break ;

		case SF_FORMAT_ALAW :	/* 8-bit Alaw encoding. */
				alaw_init (psf) ;
				break ;

		/* Lite remove start */
		case SF_FORMAT_FLOAT :	/* 32-bit floats. */
				error = float32_init (psf) ;
				break ;

		case SF_FORMAT_DOUBLE :	/* 64-bit double precision floats. */
				error = double64_init (psf) ;
				break ;

		case SF_FORMAT_G721_32 :
				error = g72x_init (psf) ;
				psf->sf.seekable = SF_FALSE ;
				break ;

		case SF_FORMAT_G723_24 :
				error = g72x_init (psf) ;
				psf->sf.seekable = SF_FALSE ;
				break ;

		case SF_FORMAT_G723_40 :
				error = g72x_init (psf) ;
				psf->sf.seekable = SF_FALSE ;
				break ;
		/* Lite remove end */

		default :	break ;
		} ;

	return error ;
} /* au_open */

/*------------------------------------------------------------------------------
*/

static int
au_close	(SF_PRIVATE *psf)
{
	if (psf->file.mode == SFM_WRITE || psf->file.mode == SFM_RDWR)
		au_write_header (psf, SF_TRUE) ;

	return 0 ;
} /* au_close */

static int
au_write_header (SF_PRIVATE *psf, int calc_length)
{	sf_count_t	current ;
	int			encoding, datalength ;

	if (psf->pipeoffset > 0)
		return 0 ;

	current = psf_ftell (psf) ;

	if (calc_length)
	{	psf->filelength = psf_get_filelen (psf) ;

		psf->datalength = psf->filelength - psf->dataoffset ;
		if (psf->dataend)
			psf->datalength -= psf->filelength - psf->dataend ;

		psf->sf.frames = psf->datalength / (psf->bytewidth * psf->sf.channels) ;
		} ;

	encoding = au_format_to_encoding (SF_CODEC (psf->sf.format)) ;
	if (! encoding)
		return (psf->error = SFE_BAD_OPEN_FORMAT) ;

	/* Reset the current header length to zero. */
	psf->header [0] = 0 ;
	psf->headindex = 0 ;

	/*
	** Only attempt to seek if we are not writng to a pipe. If we are
	** writing to a pipe we shouldn't be here anyway.
	*/
	if (psf->is_pipe == SF_FALSE)
		psf_fseek (psf, 0, SEEK_SET) ;

	/*
	**	AU format files allow a datalength value of -1 if the datalength
	**	is not know at the time the header is written.
	**	Also use this value of -1 if the datalength > 2 gigabytes.
	*/
	if (psf->datalength	< 0 || psf->datalength > 0x7FFFFFFF)
		datalength = -1 ;
	else
		datalength = (int) (psf->datalength & 0x7FFFFFFF) ;

	if (psf->endian == SF_ENDIAN_BIG)
	{	psf_binheader_writef (psf, "Em4", DOTSND_MARKER, AU_DATA_OFFSET) ;
		psf_binheader_writef (psf, "E4444", datalength, encoding, psf->sf.samplerate, psf->sf.channels) ;
		}
	else if (psf->endian == SF_ENDIAN_LITTLE)
	{	psf_binheader_writef (psf, "em4", DNSDOT_MARKER, AU_DATA_OFFSET) ;
		psf_binheader_writef (psf, "e4444", datalength, encoding, psf->sf.samplerate, psf->sf.channels) ;
		}
	else
		return (psf->error = SFE_BAD_OPEN_FORMAT) ;

	/* Header construction complete so write it out. */
	psf_fwrite (psf->header, psf->headindex, 1, psf) ;

	if (psf->error)
		return psf->error ;

	psf->dataoffset = psf->headindex ;

	if (current > 0)
		psf_fseek (psf, current, SEEK_SET) ;

	return psf->error ;
} /* au_write_header */

static int
au_format_to_encoding (int format)
{
	switch (format)
	{	case SF_FORMAT_PCM_S8 : 	return AU_ENCODING_PCM_8 ;
		case SF_FORMAT_PCM_16 :		return AU_ENCODING_PCM_16 ;
		case SF_FORMAT_PCM_24 : 	return AU_ENCODING_PCM_24 ;
		case SF_FORMAT_PCM_32 : 	return AU_ENCODING_PCM_32 ;

		case SF_FORMAT_FLOAT :		return AU_ENCODING_FLOAT ;
		case SF_FORMAT_DOUBLE :		return AU_ENCODING_DOUBLE ;

		case SF_FORMAT_ULAW :		return AU_ENCODING_ULAW_8 ;
		case SF_FORMAT_ALAW :		return AU_ENCODING_ALAW_8 ;

		case SF_FORMAT_G721_32 : 	return AU_ENCODING_ADPCM_G721_32 ;
		case SF_FORMAT_G723_24 :	return AU_ENCODING_ADPCM_G723_24 ;
		case SF_FORMAT_G723_40 :	return AU_ENCODING_ADPCM_G723_40 ;

		default : break ;
		} ;
	return 0 ;
} /* au_format_to_encoding */

static int
au_read_header (SF_PRIVATE *psf)
{	AU_FMT	au_fmt ;
	int		marker, dword ;

	memset (&au_fmt, 0, sizeof (au_fmt)) ;
	psf_binheader_readf (psf, "pm", 0, &marker) ;
	psf_log_printf (psf, "%M\n", marker) ;

	if (marker == DOTSND_MARKER)
	{	psf->endian = SF_ENDIAN_BIG ;

		psf_binheader_readf (psf, "E44444", &(au_fmt.dataoffset), &(au_fmt.datasize),
					&(au_fmt.encoding), &(au_fmt.samplerate), &(au_fmt.channels)) ;
		}
	else if (marker == DNSDOT_MARKER)
	{	psf->endian = SF_ENDIAN_LITTLE ;
		psf_binheader_readf (psf, "e44444", &(au_fmt.dataoffset), &(au_fmt.datasize),
					&(au_fmt.encoding), &(au_fmt.samplerate), &(au_fmt.channels)) ;
		}
	else
		return SFE_AU_NO_DOTSND ;

	psf_log_printf (psf, "  Data Offset : %d\n", au_fmt.dataoffset) ;

	if (psf->fileoffset > 0 && au_fmt.datasize == -1)
	{	psf_log_printf (psf, "  Data Size   : -1\n") ;
		return SFE_AU_EMBED_BAD_LEN ;
		} ;

	if (psf->fileoffset > 0)
	{	psf->filelength = au_fmt.dataoffset + au_fmt.datasize ;
		psf_log_printf (psf, "  Data Size   : %d\n", au_fmt.datasize) ;
		}
	else if (au_fmt.datasize == -1 || au_fmt.dataoffset + au_fmt.datasize == psf->filelength)
		psf_log_printf (psf, "  Data Size   : %d\n", au_fmt.datasize) ;
	else if (au_fmt.dataoffset + au_fmt.datasize < psf->filelength)
	{	psf->filelength = au_fmt.dataoffset + au_fmt.datasize ;
		psf_log_printf (psf, "  Data Size   : %d\n", au_fmt.datasize) ;
		}
	else
	{	dword = psf->filelength - au_fmt.dataoffset ;
		psf_log_printf (psf, "  Data Size   : %d (should be %d)\n", au_fmt.datasize, dword) ;
		au_fmt.datasize = dword ;
		} ;

 	psf->dataoffset = au_fmt.dataoffset ;
	psf->datalength = psf->filelength - psf->dataoffset ;

	if (psf_ftell (psf) < psf->dataoffset)
		psf_binheader_readf (psf, "j", psf->dataoffset - psf_ftell (psf)) ;

	psf->sf.samplerate	= au_fmt.samplerate ;
	psf->sf.channels 	= au_fmt.channels ;

	/* Only fill in type major. */
	if (psf->endian == SF_ENDIAN_BIG)
		psf->sf.format = SF_FORMAT_AU ;
	else if (psf->endian == SF_ENDIAN_LITTLE)
		psf->sf.format = SF_ENDIAN_LITTLE | SF_FORMAT_AU ;

	psf_log_printf (psf, "  Encoding    : %d => ", au_fmt.encoding) ;

	psf->sf.format = SF_ENDIAN (psf->sf.format) ;

	switch (au_fmt.encoding)
	{	case AU_ENCODING_ULAW_8 :
				psf->sf.format |= SF_FORMAT_AU | SF_FORMAT_ULAW ;
				psf->bytewidth = 1 ;	/* Before decoding */
				psf_log_printf (psf, "8-bit ISDN u-law\n") ;
				break ;

		case AU_ENCODING_PCM_8 :
				psf->sf.format |= SF_FORMAT_AU | SF_FORMAT_PCM_S8 ;
				psf->bytewidth = 1 ;
				psf_log_printf (psf, "8-bit linear PCM\n") ;
				break ;

		case AU_ENCODING_PCM_16 :
				psf->sf.format |= SF_FORMAT_AU | SF_FORMAT_PCM_16 ;
				psf->bytewidth = 2 ;
				psf_log_printf (psf, "16-bit linear PCM\n") ;
				break ;

		case AU_ENCODING_PCM_24 :
				psf->sf.format |= SF_FORMAT_AU | SF_FORMAT_PCM_24 ;
				psf->bytewidth = 3 ;
				psf_log_printf (psf, "24-bit linear PCM\n") ;
				break ;

		case AU_ENCODING_PCM_32 :
				psf->sf.format |= SF_FORMAT_AU | SF_FORMAT_PCM_32 ;
				psf->bytewidth = 4 ;
				psf_log_printf (psf, "32-bit linear PCM\n") ;
				break ;

		case AU_ENCODING_FLOAT :
				psf->sf.format |= SF_FORMAT_AU | SF_FORMAT_FLOAT ;
				psf->bytewidth = 4 ;
				psf_log_printf (psf, "32-bit float\n") ;
				break ;

		case AU_ENCODING_DOUBLE :
				psf->sf.format |= SF_FORMAT_AU | SF_FORMAT_DOUBLE ;
				psf->bytewidth = 8 ;
				psf_log_printf (psf, "64-bit double precision float\n") ;
				break ;

		case AU_ENCODING_ALAW_8 :
				psf->sf.format |= SF_FORMAT_AU | SF_FORMAT_ALAW ;
				psf->bytewidth = 1 ;	/* Before decoding */
				psf_log_printf (psf, "8-bit ISDN A-law\n") ;
				break ;

		case AU_ENCODING_ADPCM_G721_32 :
				psf->sf.format |= SF_FORMAT_AU | SF_FORMAT_G721_32 ;
				psf->bytewidth = 0 ;
				psf_log_printf (psf, "G721 32kbs ADPCM\n") ;
				break ;

		case AU_ENCODING_ADPCM_G723_24 :
				psf->sf.format |= SF_FORMAT_AU | SF_FORMAT_G723_24 ;
				psf->bytewidth = 0 ;
				psf_log_printf (psf, "G723 24kbs ADPCM\n") ;
				break ;

		case AU_ENCODING_ADPCM_G723_40 :
				psf->sf.format |= SF_FORMAT_AU | SF_FORMAT_G723_40 ;
				psf->bytewidth = 0 ;
				psf_log_printf (psf, "G723 40kbs ADPCM\n") ;
				break ;

		case AU_ENCODING_ADPCM_G722 :
				psf_log_printf (psf, "G722 64 kbs ADPCM (unsupported)\n") ;
				break ;

		case AU_ENCODING_NEXT :
				psf_log_printf (psf, "Weird NeXT encoding format (unsupported)\n") ;
				break ;

		default :
				psf_log_printf (psf, "Unknown!!\n") ;
				break ;
		} ;

	psf_log_printf (psf, "  Sample Rate : %d\n", au_fmt.samplerate) ;
	if (au_fmt.channels < 1)
	{	psf_log_printf (psf, "  Channels    : %d  **** should be >= 1\n", au_fmt.channels) ;
		return SFE_CHANNEL_COUNT_ZERO ;
		}
	else
		psf_log_printf (psf, "  Channels    : %d\n", au_fmt.channels) ;

	psf->blockwidth = psf->sf.channels * psf->bytewidth ;

	if (! psf->sf.frames && psf->blockwidth)
		psf->sf.frames = (psf->filelength - psf->dataoffset) / psf->blockwidth ;

	return 0 ;
} /* au_read_header */

