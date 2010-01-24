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

#include	"sfconfig.h"

#include	<stdio.h>
#include	<fcntl.h>
#include	<string.h>
#include	<ctype.h>
#include	<math.h>

#include	"sndfile.h"
#include	"sfendian.h"
#include	"common.h"

/*------------------------------------------------------------------------------
** Information on how to decode and encode this file was obtained in a PDF
** file which I found on http://www.wotsit.org/.
** Also did a lot of testing with GNU Octave but do not have access to
** Matlab (tm) and so could not test it there.
*/

/*------------------------------------------------------------------------------
** Macros to handle big/little endian issues.
*/

#define MAT4_BE_DOUBLE	(MAKE_MARKER (0, 0, 0x03, 0xE8))
#define MAT4_LE_DOUBLE	(MAKE_MARKER (0, 0, 0, 0))

#define MAT4_BE_FLOAT	(MAKE_MARKER (0, 0, 0x03, 0xF2))
#define MAT4_LE_FLOAT	(MAKE_MARKER (0x0A, 0, 0, 0))

#define MAT4_BE_PCM_32	(MAKE_MARKER (0, 0, 0x03, 0xFC))
#define MAT4_LE_PCM_32	(MAKE_MARKER (0x14, 0, 0, 0))

#define MAT4_BE_PCM_16	(MAKE_MARKER (0, 0, 0x04, 0x06))
#define MAT4_LE_PCM_16	(MAKE_MARKER (0x1E, 0, 0, 0))

/* Can't see any reason to ever implement this. */
#define MAT4_BE_PCM_U8	(MAKE_MARKER (0, 0, 0x04, 0x1A))
#define MAT4_LE_PCM_U8	(MAKE_MARKER (0x32, 0, 0, 0))

/*------------------------------------------------------------------------------
** Private static functions.
*/

static	int		mat4_close		(SF_PRIVATE *psf) ;

static	int 	mat4_format_to_encoding	(int format, int endian) ;

static int		mat4_write_header (SF_PRIVATE *psf, int calc_length) ;
static int		mat4_read_header (SF_PRIVATE *psf) ;

static const char * mat4_marker_to_str (int marker) ;

/*------------------------------------------------------------------------------
** Public function.
*/

int
mat4_open	(SF_PRIVATE *psf)
{	int		subformat, error = 0 ;

	if (psf->mode == SFM_READ || (psf->mode == SFM_RDWR && psf->filelength > 0))
	{	if ((error = mat4_read_header (psf)))
			return error ;
		} ;

	if ((SF_CONTAINER (psf->sf.format)) != SF_FORMAT_MAT4)
		return	SFE_BAD_OPEN_FORMAT ;

	subformat = SF_CODEC (psf->sf.format) ;

	if (psf->mode == SFM_WRITE || psf->mode == SFM_RDWR)
	{	if (psf->is_pipe)
			return SFE_NO_PIPE_WRITE ;

		psf->endian = SF_ENDIAN (psf->sf.format) ;
		if (CPU_IS_LITTLE_ENDIAN && (psf->endian == SF_ENDIAN_CPU || psf->endian == 0))
			psf->endian = SF_ENDIAN_LITTLE ;
		else if (CPU_IS_BIG_ENDIAN && (psf->endian == SF_ENDIAN_CPU || psf->endian == 0))
			psf->endian = SF_ENDIAN_BIG ;

		if ((error = mat4_write_header (psf, SF_FALSE)))
			return error ;

		psf->write_header = mat4_write_header ;
		} ;

	psf->container_close = mat4_close ;

	psf->blockwidth = psf->bytewidth * psf->sf.channels ;

	switch (subformat)
	{	case SF_FORMAT_PCM_16 :
		case SF_FORMAT_PCM_32 :
				error = pcm_init (psf) ;
				break ;

		case SF_FORMAT_FLOAT :
				error = float32_init (psf) ;
				break ;

		case SF_FORMAT_DOUBLE :
				error = double64_init (psf) ;
				break ;

		default : break ;
		} ;

	if (error)
		return error ;

	return error ;
} /* mat4_open */

/*------------------------------------------------------------------------------
*/

static int
mat4_close	(SF_PRIVATE *psf)
{
	if (psf->mode == SFM_WRITE || psf->mode == SFM_RDWR)
		mat4_write_header (psf, SF_TRUE) ;

	return 0 ;
} /* mat4_close */

/*------------------------------------------------------------------------------
*/

static int
mat4_write_header (SF_PRIVATE *psf, int calc_length)
{	sf_count_t	current ;
	int			encoding ;
	double		samplerate ;

	current = psf_ftell (psf) ;

	if (calc_length)
	{	psf->filelength = psf_get_filelen (psf) ;

		psf->datalength = psf->filelength - psf->dataoffset ;
		if (psf->dataend)
			psf->datalength -= psf->filelength - psf->dataend ;

		psf->sf.frames = psf->datalength / (psf->bytewidth * psf->sf.channels) ;
		} ;

	encoding = mat4_format_to_encoding (SF_CODEC (psf->sf.format), psf->endian) ;

	if (encoding == -1)
		return SFE_BAD_OPEN_FORMAT ;

	/* Reset the current header length to zero. */
	psf->header [0] = 0 ;
	psf->headindex = 0 ;
	psf_fseek (psf, 0, SEEK_SET) ;

	/* Need sample rate as a double for writing to the header. */
	samplerate = psf->sf.samplerate ;

	if (psf->endian == SF_ENDIAN_BIG)
	{	psf_binheader_writef (psf, "Em444", MAT4_BE_DOUBLE, 1, 1, 0) ;
		psf_binheader_writef (psf, "E4bd", 11, "samplerate", make_size_t (11), samplerate) ;
		psf_binheader_writef (psf, "tEm484", encoding, psf->sf.channels, psf->sf.frames, 0) ;
		psf_binheader_writef (psf, "E4b", 9, "wavedata", make_size_t (9)) ;
		}
	else if (psf->endian == SF_ENDIAN_LITTLE)
	{	psf_binheader_writef (psf, "em444", MAT4_LE_DOUBLE, 1, 1, 0) ;
		psf_binheader_writef (psf, "e4bd", 11, "samplerate", make_size_t (11), samplerate) ;
		psf_binheader_writef (psf, "tem484", encoding, psf->sf.channels, psf->sf.frames, 0) ;
		psf_binheader_writef (psf, "e4b", 9, "wavedata", make_size_t (9)) ;
		}
	else
		return SFE_BAD_OPEN_FORMAT ;

	/* Header construction complete so write it out. */
	psf_fwrite (psf->header, psf->headindex, 1, psf) ;

	if (psf->error)
		return psf->error ;

	psf->dataoffset = psf->headindex ;

	if (current > 0)
		psf_fseek (psf, current, SEEK_SET) ;

	return psf->error ;
} /* mat4_write_header */

static int
mat4_read_header (SF_PRIVATE *psf)
{	int		marker, rows, cols, imag ;
	unsigned namesize ;
	double	value ;
	const char *marker_str ;
	char	name [64] ;

	psf_binheader_readf (psf, "pm", 0, &marker) ;

	/* MAT4 file must start with a double for the samplerate. */
	if (marker == MAT4_BE_DOUBLE)
	{	psf->endian = psf->rwf_endian = SF_ENDIAN_BIG ;
		marker_str = "big endian double" ;
		}
	else if (marker == MAT4_LE_DOUBLE)
	{	psf->endian = psf->rwf_endian = SF_ENDIAN_LITTLE ;
		marker_str = "little endian double" ;
		}
	else
		return SFE_UNIMPLEMENTED ;

	psf_log_printf (psf, "GNU Octave 2.0 / MATLAB v4.2 format\nMarker : %s\n", marker_str) ;

	psf_binheader_readf (psf, "444", &rows, &cols, &imag) ;

	psf_log_printf (psf, " Rows  : %d\n Cols  : %d\n Imag  : %s\n", rows, cols, imag ? "True" : "False") ;

	psf_binheader_readf (psf, "4", &namesize) ;

	if (namesize >= SIGNED_SIZEOF (name))
		return SFE_MAT4_BAD_NAME ;

	psf_binheader_readf (psf, "b", name, namesize) ;
	name [namesize] = 0 ;

	psf_log_printf (psf, " Name  : %s\n", name) ;

	psf_binheader_readf (psf, "d", &value) ;

	LSF_SNPRINTF (psf->u.cbuf, sizeof (psf->u.cbuf), " Value : %f\n", value) ;
	psf_log_printf (psf, psf->u.cbuf) ;

	if ((rows != 1) || (cols != 1))
		return SFE_MAT4_NO_SAMPLERATE ;

	psf->sf.samplerate = lrint (value) ;

	/* Now write out the audio data. */

	psf_binheader_readf (psf, "m", &marker) ;

	psf_log_printf (psf, "Marker : %s\n", mat4_marker_to_str (marker)) ;

	psf_binheader_readf (psf, "444", &rows, &cols, &imag) ;

	psf_log_printf (psf, " Rows  : %d\n Cols  : %d\n Imag  : %s\n", rows, cols, imag ? "True" : "False") ;

	psf_binheader_readf (psf, "4", &namesize) ;

	if (namesize >= SIGNED_SIZEOF (name))
		return SFE_MAT4_BAD_NAME ;

	psf_binheader_readf (psf, "b", name, namesize) ;
	name [namesize] = 0 ;

	psf_log_printf (psf, " Name  : %s\n", name) ;

	psf->dataoffset = psf_ftell (psf) ;

	if (rows == 0 && cols == 0)
	{	psf_log_printf (psf, "*** Error : zero channel count.\n") ;
		return SFE_CHANNEL_COUNT_ZERO ;
		} ;

	psf->sf.channels	= rows ;
	psf->sf.frames		= cols ;

	psf->sf.format = psf->endian | SF_FORMAT_MAT4 ;
	switch (marker)
	{	case MAT4_BE_DOUBLE :
		case MAT4_LE_DOUBLE :
				psf->sf.format |= SF_FORMAT_DOUBLE ;
				psf->bytewidth = 8 ;
				break ;

		case MAT4_BE_FLOAT :
		case MAT4_LE_FLOAT :
				psf->sf.format |= SF_FORMAT_FLOAT ;
				psf->bytewidth = 4 ;
				break ;

		case MAT4_BE_PCM_32	:
		case MAT4_LE_PCM_32	:
				psf->sf.format |= SF_FORMAT_PCM_32 ;
				psf->bytewidth = 4 ;
				break ;

		case MAT4_BE_PCM_16	:
		case MAT4_LE_PCM_16	:
				psf->sf.format |= SF_FORMAT_PCM_16 ;
				psf->bytewidth = 2 ;
				break ;

		default :
				psf_log_printf (psf, "*** Error : Bad marker %08X\n", marker) ;
				return SFE_UNIMPLEMENTED ;
		} ;

	if ((psf->filelength - psf->dataoffset) < psf->sf.channels * psf->sf.frames * psf->bytewidth)
	{	psf_log_printf (psf, "*** File seems to be truncated. %D <--> %D\n",
				psf->filelength - psf->dataoffset, psf->sf.channels * psf->sf.frames * psf->bytewidth) ;
		}
	else if ((psf->filelength - psf->dataoffset) > psf->sf.channels * psf->sf.frames * psf->bytewidth)
		psf->dataend = psf->dataoffset + rows * cols * psf->bytewidth ;

	psf->datalength = psf->filelength - psf->dataoffset - psf->dataend ;

	psf->sf.sections = 1 ;

	return 0 ;
} /* mat4_read_header */

static int
mat4_format_to_encoding (int format, int endian)
{
	switch (format | endian)
	{	case (SF_FORMAT_PCM_16 | SF_ENDIAN_BIG) :
				return MAT4_BE_PCM_16 ;

		case (SF_FORMAT_PCM_16 | SF_ENDIAN_LITTLE) :
				return MAT4_LE_PCM_16 ;

		case (SF_FORMAT_PCM_32 | SF_ENDIAN_BIG) :
				return MAT4_BE_PCM_32 ;

		case (SF_FORMAT_PCM_32 | SF_ENDIAN_LITTLE) :
				return MAT4_LE_PCM_32 ;

		case (SF_FORMAT_FLOAT | SF_ENDIAN_BIG) :
				return MAT4_BE_FLOAT ;

		case (SF_FORMAT_FLOAT | SF_ENDIAN_LITTLE) :
				return MAT4_LE_FLOAT ;

		case (SF_FORMAT_DOUBLE | SF_ENDIAN_BIG) :
				return MAT4_BE_DOUBLE ;

		case (SF_FORMAT_DOUBLE | SF_ENDIAN_LITTLE) :
				return MAT4_LE_DOUBLE ;

		default : break ;
		} ;

	return -1 ;
} /* mat4_format_to_encoding */

static const char *
mat4_marker_to_str (int marker)
{	static char str [32] ;

	switch (marker)
	{
		case MAT4_BE_PCM_16	:	return "big endian 16 bit PCM" ;
		case MAT4_LE_PCM_16	:	return "little endian 16 bit PCM" ;

		case MAT4_BE_PCM_32	:	return "big endian 32 bit PCM" ;
		case MAT4_LE_PCM_32	:	return "little endian 32 bit PCM" ;


		case MAT4_BE_FLOAT :	return "big endian float" ;
		case MAT4_LE_FLOAT :	return "big endian float" ;

		case MAT4_BE_DOUBLE	:	return "big endian double" ;
		case MAT4_LE_DOUBLE	:	return "little endian double" ;
		} ;

	/* This is a little unsafe but is really only for debugging. */
	str [sizeof (str) - 1] = 0 ;
	LSF_SNPRINTF (str, sizeof (str) - 1, "%08X", marker) ;
	return str ;
} /* mat4_marker_to_str */

