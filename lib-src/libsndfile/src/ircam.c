/*
** Copyright (C) 2001-2017 Erik de Castro Lopo <erikd@mega-nerd.com>
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

/* The IRCAM magic number is weird in that one byte in the number can have
** values of 0x1, 0x2, 0x03 or 0x04. Hence the need for a marker and a mask.
*/

#define IRCAM_BE_MASK		(MAKE_MARKER (0xFF, 0xFF, 0x00, 0xFF))
#define IRCAM_BE_MARKER		(MAKE_MARKER (0x64, 0xA3, 0x00, 0x00))

#define IRCAM_LE_MASK		(MAKE_MARKER (0xFF, 0x00, 0xFF, 0xFF))
#define IRCAM_LE_MARKER		(MAKE_MARKER (0x00, 0x00, 0xA3, 0x64))

#define IRCAM_02B_MARKER	(MAKE_MARKER (0x64, 0xA3, 0x02, 0x00))
#define IRCAM_03L_MARKER	(MAKE_MARKER (0x64, 0xA3, 0x03, 0x00))

#define IRCAM_DATA_OFFSET	(1024)

/*------------------------------------------------------------------------------
** Typedefs.
*/

enum
{	IRCAM_PCM_16	= 0x00002,
	IRCAM_FLOAT		= 0x00004,
	IRCAM_ALAW		= 0x10001,
	IRCAM_ULAW		= 0x20001,
	IRCAM_PCM_32	= 0x40004
} ;


/*------------------------------------------------------------------------------
** Private static functions.
*/

static	int		ircam_close			(SF_PRIVATE *psf) ;
static	int		ircam_write_header	(SF_PRIVATE *psf, int calc_length) ;
static	int		ircam_read_header	(SF_PRIVATE *psf) ;

static	int		get_encoding (int subformat) ;

static	const char*	get_encoding_str (int encoding) ;

/*------------------------------------------------------------------------------
** Public function.
*/

int
ircam_open	(SF_PRIVATE *psf)
{	int		subformat ;
	int		error = SFE_NO_ERROR ;

	if (psf->file.mode == SFM_READ || (psf->file.mode == SFM_RDWR && psf->filelength > 0))
	{	if ((error = ircam_read_header (psf)))
			return error ;
		} ;

	subformat = SF_CODEC (psf->sf.format) ;

	if (psf->file.mode == SFM_WRITE || psf->file.mode == SFM_RDWR)
	{	if ((SF_CONTAINER (psf->sf.format)) != SF_FORMAT_IRCAM)
			return	SFE_BAD_OPEN_FORMAT ;

		psf->endian = SF_ENDIAN (psf->sf.format) ;
		if (psf->endian == 0 || psf->endian == SF_ENDIAN_CPU)
			psf->endian = (CPU_IS_BIG_ENDIAN) ? SF_ENDIAN_BIG : SF_ENDIAN_LITTLE ;

		psf->dataoffset = IRCAM_DATA_OFFSET ;

		if ((error = ircam_write_header (psf, SF_FALSE)))
			return error ;

		psf->write_header = ircam_write_header ;
		} ;

	psf->container_close = ircam_close ;

	switch (subformat)
	{	case SF_FORMAT_ULAW :		/* 8-bit Ulaw encoding. */
				error = ulaw_init (psf) ;
				break ;

		case SF_FORMAT_ALAW :		/* 8-bit Alaw encoding. */
				error = alaw_init (psf) ;
				break ;

		case SF_FORMAT_PCM_16 :	/* 16-bit linear PCM. */
		case SF_FORMAT_PCM_32 :	/* 32-bit linear PCM. */
				error = pcm_init (psf) ;
				break ;

		case SF_FORMAT_FLOAT :	/* 32-bit linear PCM. */
				error = float32_init (psf) ;
				break ;

		default : break ;
		} ;

	return error ;
} /* ircam_open */

/*------------------------------------------------------------------------------
*/

static int
ircam_read_header	(SF_PRIVATE *psf)
{	unsigned int	marker, encoding ;
	float			samplerate ;
	int				error = SFE_NO_ERROR ;

	psf_binheader_readf (psf, "epmf44", 0, &marker, &samplerate, &(psf->sf.channels), &encoding) ;

	if (((marker & IRCAM_BE_MASK) != IRCAM_BE_MARKER) && ((marker & IRCAM_LE_MASK) != IRCAM_LE_MARKER))
	{	psf_log_printf (psf, "marker: 0x%X\n", marker) ;
		return SFE_IRCAM_NO_MARKER ;
		} ;

	psf->endian = SF_ENDIAN_LITTLE ;

	if (psf->sf.channels > SF_MAX_CHANNELS)
	{	psf_binheader_readf (psf, "Epmf44", 0, &marker, &samplerate, &(psf->sf.channels), &encoding) ;

		/* Sanity checking for endian-ness detection. */
		if (psf->sf.channels > SF_MAX_CHANNELS)
		{	psf_log_printf (psf, "marker: 0x%X\n", marker) ;
			return SFE_IRCAM_BAD_CHANNELS ;
			} ;

		psf->endian = SF_ENDIAN_BIG ;
		} ;

	psf_log_printf (psf, "marker: 0x%X\n", marker) ;

	psf->sf.samplerate = (int) samplerate ;

	psf_log_printf (psf,	"  Sample Rate : %d\n"
							"  Channels    : %d\n"
							"  Encoding    : %X => %s\n",
						psf->sf.samplerate, psf->sf.channels, encoding, get_encoding_str (encoding)) ;

	switch (encoding)
	{	case IRCAM_PCM_16 :
				psf->bytewidth = 2 ;
				psf->blockwidth = psf->sf.channels * psf->bytewidth ;

				psf->sf.format = SF_FORMAT_IRCAM | SF_FORMAT_PCM_16 ;
				break ;

		case IRCAM_PCM_32 :
				psf->bytewidth = 4 ;
				psf->blockwidth = psf->sf.channels * psf->bytewidth ;

				psf->sf.format = SF_FORMAT_IRCAM | SF_FORMAT_PCM_32 ;
				break ;

		case IRCAM_FLOAT :
				psf->bytewidth = 4 ;
				psf->blockwidth = psf->sf.channels * psf->bytewidth ;

				psf->sf.format = SF_FORMAT_IRCAM | SF_FORMAT_FLOAT ;
				break ;

		case IRCAM_ALAW :
				psf->bytewidth = 1 ;
				psf->blockwidth = psf->sf.channels * psf->bytewidth ;

				psf->sf.format = SF_FORMAT_IRCAM | SF_FORMAT_ALAW ;
				break ;

		case IRCAM_ULAW :
				psf->bytewidth = 1 ;
				psf->blockwidth = psf->sf.channels * psf->bytewidth ;

				psf->sf.format = SF_FORMAT_IRCAM | SF_FORMAT_ULAW ;
				break ;

		default :
				error = SFE_IRCAM_UNKNOWN_FORMAT ;
				break ;
		} ;

	if (psf->endian == SF_ENDIAN_BIG)
		psf->sf.format |= SF_ENDIAN_BIG ;
	else
		psf->sf.format |= SF_ENDIAN_LITTLE ;

	if (error)
		return error ;

	psf->dataoffset = IRCAM_DATA_OFFSET ;
	psf->datalength = psf->filelength - psf->dataoffset ;

	if (psf->sf.frames == 0 && psf->blockwidth)
		psf->sf.frames = psf->datalength / psf->blockwidth ;

	psf_log_printf (psf, "  Samples     : %d\n", psf->sf.frames) ;

	psf_binheader_readf (psf, "p", IRCAM_DATA_OFFSET) ;

	return 0 ;
} /* ircam_read_header */

static int
ircam_close	(SF_PRIVATE *psf)
{
	psf_log_printf (psf, "close\n") ;

	return 0 ;
} /* ircam_close */

static int
ircam_write_header (SF_PRIVATE *psf, int UNUSED (calc_length))
{	int			encoding ;
	float		samplerate ;
	sf_count_t	current ;

	if (psf->pipeoffset > 0)
		return 0 ;

	current = psf_ftell (psf) ;

	/* This also sets psf->endian. */
	encoding = get_encoding (SF_CODEC (psf->sf.format)) ;

	if (encoding == 0)
		return SFE_BAD_OPEN_FORMAT ;

	/* Reset the current header length to zero. */
	psf->header.ptr [0] = 0 ;
	psf->header.indx = 0 ;

	if (psf->is_pipe == SF_FALSE)
		psf_fseek (psf, 0, SEEK_SET) ;

	samplerate = psf->sf.samplerate ;

	switch (psf->endian)
	{	case SF_ENDIAN_BIG :
			psf_binheader_writef (psf, "Emf", BHWm (IRCAM_02B_MARKER), BHWf (samplerate)) ;
			psf_binheader_writef (psf, "E44", BHW4 (psf->sf.channels), BHW4 (encoding)) ;
			break ;

		case SF_ENDIAN_LITTLE :
			psf_binheader_writef (psf, "emf", BHWm (IRCAM_03L_MARKER), BHWf (samplerate)) ;
			psf_binheader_writef (psf, "e44", BHW4 (psf->sf.channels), BHW4 (encoding)) ;
			break ;

		default : return SFE_BAD_OPEN_FORMAT ;
		} ;

	psf_binheader_writef (psf, "z", BHWz ((size_t) (IRCAM_DATA_OFFSET - psf->header.indx))) ;

	/* Header construction complete so write it out. */
	psf_fwrite (psf->header.ptr, psf->header.indx, 1, psf) ;

	if (psf->error)
		return psf->error ;

	if (current > 0)
		psf_fseek (psf, current, SEEK_SET) ;

	return psf->error ;
} /* ircam_write_header */

static int
get_encoding (int subformat)
{	switch (subformat)
	{	case SF_FORMAT_PCM_16 :	return IRCAM_PCM_16 ;
		case SF_FORMAT_PCM_32 :	return IRCAM_PCM_32 ;

		case SF_FORMAT_FLOAT :	return IRCAM_FLOAT ;

		case SF_FORMAT_ULAW :	return IRCAM_ULAW ;
		case SF_FORMAT_ALAW :	return IRCAM_ALAW ;

		default : break ;
		} ;

	return 0 ;
} /* get_encoding */

static const char*
get_encoding_str (int encoding)
{	switch (encoding)
	{	case IRCAM_PCM_16	: return "16 bit PCM" ;
		case IRCAM_FLOAT	: return "32 bit float" ;
		case IRCAM_ALAW		: return "A law" ;
		case IRCAM_ULAW		: return "u law" ;
		case IRCAM_PCM_32	: return "32 bit PCM" ;
		} ;
	return "Unknown encoding" ;
} /* get_encoding_str */

