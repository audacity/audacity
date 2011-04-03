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
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

#include "sndfile.h"
#include "sfendian.h"
#include "common.h"


/*------------------------------------------------------------------------------
 * Macros to handle big/little endian issues.
*/

#define FORM_MARKER	(MAKE_MARKER ('F', 'O', 'R', 'M'))
#define SVX8_MARKER	(MAKE_MARKER ('8', 'S', 'V', 'X'))
#define SV16_MARKER	(MAKE_MARKER ('1', '6', 'S', 'V'))
#define VHDR_MARKER	(MAKE_MARKER ('V', 'H', 'D', 'R'))
#define BODY_MARKER	(MAKE_MARKER ('B', 'O', 'D', 'Y'))

#define ATAK_MARKER	(MAKE_MARKER ('A', 'T', 'A', 'K'))
#define RLSE_MARKER	(MAKE_MARKER ('R', 'L', 'S', 'E'))

#define c_MARKER	(MAKE_MARKER ('(', 'c', ')', ' '))
#define NAME_MARKER	(MAKE_MARKER ('N', 'A', 'M', 'E'))
#define AUTH_MARKER	(MAKE_MARKER ('A', 'U', 'T', 'H'))
#define ANNO_MARKER	(MAKE_MARKER ('A', 'N', 'N', 'O'))
#define CHAN_MARKER	(MAKE_MARKER ('C', 'H', 'A', 'N'))

/*------------------------------------------------------------------------------
 * Typedefs for file chunks.
*/

typedef struct
{	unsigned int	oneShotHiSamples, repeatHiSamples, samplesPerHiCycle ;
	unsigned short	samplesPerSec ;
	unsigned char	octave, compression ;
	unsigned int	volume ;
} VHDR_CHUNK ;

enum {
	HAVE_FORM	= 0x01,

	HAVE_SVX	= 0x02,
	HAVE_VHDR	= 0x04,
	HAVE_BODY	= 0x08
} ;

/*------------------------------------------------------------------------------
 * Private static functions.
*/

static int	svx_close	(SF_PRIVATE *psf) ;
static int	svx_write_header (SF_PRIVATE *psf, int calc_length) ;
static int 	svx_read_header	(SF_PRIVATE *psf) ;

/*------------------------------------------------------------------------------
** Public function.
*/

int
svx_open	(SF_PRIVATE *psf)
{	int error ;

	if (psf->file.mode == SFM_READ || (psf->file.mode == SFM_RDWR && psf->filelength > 0))
	{	if ((error = svx_read_header (psf)))
			return error ;

		psf->endian = SF_ENDIAN_BIG ;			/* All SVX files are big endian. */

		psf->blockwidth = psf->sf.channels * psf->bytewidth ;
		if (psf->blockwidth)
			psf->sf.frames = psf->datalength / psf->blockwidth ;

		psf_fseek (psf, psf->dataoffset, SEEK_SET) ;
		} ;

	if (psf->file.mode == SFM_WRITE || psf->file.mode == SFM_RDWR)
	{	if (psf->is_pipe)
			return SFE_NO_PIPE_WRITE ;

		if ((SF_CONTAINER (psf->sf.format)) != SF_FORMAT_SVX)
			return	SFE_BAD_OPEN_FORMAT ;

		psf->endian = SF_ENDIAN (psf->sf.format) ;

		if (psf->endian == SF_ENDIAN_LITTLE || (CPU_IS_LITTLE_ENDIAN && psf->endian == SF_ENDIAN_CPU))
			return SFE_BAD_ENDIAN ;

		psf->endian = SF_ENDIAN_BIG ;			/* All SVX files are big endian. */

		error = svx_write_header (psf, SF_FALSE) ;
		if (error)
			return error ;

		psf->write_header = svx_write_header ;
		} ;

	psf->container_close = svx_close ;

	if ((error = pcm_init (psf)))
		return error ;

	return 0 ;
} /* svx_open */

/*------------------------------------------------------------------------------
*/

static int
svx_read_header	(SF_PRIVATE *psf)
{	VHDR_CHUNK		vhdr ;
	unsigned int	FORMsize, vhdrsize, dword, marker ;
	int				filetype = 0, parsestage = 0, done = 0 ;
	int 			bytecount = 0, channels ;

	if (psf->filelength > SF_PLATFORM_S64 (0xffffffff))
		psf_log_printf (psf, "Warning : filelength > 0xffffffff. This is bad!!!!\n") ;

	memset (&vhdr, 0, sizeof (vhdr)) ;
	psf_binheader_readf (psf, "p", 0) ;

	/* Set default number of channels. Modify later if necessary */
	psf->sf.channels = 1 ;

	psf->sf.format = SF_FORMAT_SVX ;

	while (! done)
	{	psf_binheader_readf (psf, "m", &marker) ;
		switch (marker)
		{	case FORM_MARKER :
					if (parsestage)
						return SFE_SVX_NO_FORM ;

					psf_binheader_readf (psf, "E4", &FORMsize) ;

					if (FORMsize != psf->filelength - 2 * sizeof (dword))
					{	dword = psf->filelength - 2 * sizeof (dword) ;
						psf_log_printf (psf, "FORM : %d (should be %d)\n", FORMsize, dword) ;
						FORMsize = dword ;
						}
					else
						psf_log_printf (psf, "FORM : %d\n", FORMsize) ;
					parsestage |= HAVE_FORM ;
					break ;

			case SVX8_MARKER :
			case SV16_MARKER :
					if (! (parsestage & HAVE_FORM))
						return SFE_SVX_NO_FORM ;
					filetype = marker ;
					psf_log_printf (psf, " %M\n", marker) ;
					parsestage |= HAVE_SVX ;
					break ;

			case VHDR_MARKER :
					if (! (parsestage & (HAVE_FORM | HAVE_SVX)))
						return SFE_SVX_NO_FORM ;

					psf_binheader_readf (psf, "E4", &vhdrsize) ;

					psf_log_printf (psf, " VHDR : %d\n", vhdrsize) ;

					psf_binheader_readf (psf, "E4442114", &(vhdr.oneShotHiSamples), &(vhdr.repeatHiSamples),
						&(vhdr.samplesPerHiCycle), &(vhdr.samplesPerSec), &(vhdr.octave), &(vhdr.compression),
						&(vhdr.volume)) ;

					psf_log_printf (psf, "  OneShotHiSamples  : %d\n", vhdr.oneShotHiSamples) ;
					psf_log_printf (psf, "  RepeatHiSamples   : %d\n", vhdr.repeatHiSamples) ;
					psf_log_printf (psf, "  samplesPerHiCycle : %d\n", vhdr.samplesPerHiCycle) ;
					psf_log_printf (psf, "  Sample Rate       : %d\n", vhdr.samplesPerSec) ;
					psf_log_printf (psf, "  Octave            : %d\n", vhdr.octave) ;

					psf_log_printf (psf, "  Compression       : %d => ", vhdr.compression) ;

					switch (vhdr.compression)
					{	case 0 : psf_log_printf (psf, "None.\n") ;
								break ;
						case 1 : psf_log_printf (psf, "Fibonacci delta\n") ;
								break ;
						case 2 : psf_log_printf (psf, "Exponential delta\n") ;
								break ;
						} ;

					psf_log_printf (psf, "  Volume            : %d\n", vhdr.volume) ;

					psf->sf.samplerate 	= vhdr.samplesPerSec ;

					if (filetype == SVX8_MARKER)
					{	psf->sf.format |= SF_FORMAT_PCM_S8 ;
						psf->bytewidth = 1 ;
						}
					else if (filetype == SV16_MARKER)
					{	psf->sf.format |= SF_FORMAT_PCM_16 ;
						psf->bytewidth = 2 ;
						} ;

					parsestage |= HAVE_VHDR ;
					break ;

			case BODY_MARKER :
					if (! (parsestage & HAVE_VHDR))
						return SFE_SVX_NO_BODY ;

					psf_binheader_readf (psf, "E4", &dword) ;
					psf->datalength = dword ;

					psf->dataoffset = psf_ftell (psf) ;
					if (psf->dataoffset < 0)
						return SFE_SVX_NO_BODY ;

					if (psf->datalength > psf->filelength - psf->dataoffset)
					{	psf_log_printf (psf, " BODY : %D (should be %D)\n", psf->datalength, psf->filelength - psf->dataoffset) ;
						psf->datalength = psf->filelength - psf->dataoffset ;
						}
					else
						psf_log_printf (psf, " BODY : %D\n", psf->datalength) ;

					parsestage |= HAVE_BODY ;

					if (! psf->sf.seekable)
						break ;

					psf_fseek (psf, psf->datalength, SEEK_CUR) ;
					break ;

			case NAME_MARKER :
					if (! (parsestage & HAVE_SVX))
						return SFE_SVX_NO_FORM ;

					psf_binheader_readf (psf, "E4", &dword) ;

					psf_log_printf (psf, " %M : %d\n", marker, dword) ;

					if (strlen (psf->file.name.c) != dword)
					{	if (dword > sizeof (psf->file.name.c) - 1)
							return SFE_SVX_BAD_NAME_LENGTH ;

						psf_binheader_readf (psf, "b", psf->file.name.c, dword) ;
						psf->file.name.c [dword] = 0 ;
						}
					else
						psf_binheader_readf (psf, "j", dword) ;
					break ;

			case ANNO_MARKER :
					if (! (parsestage & HAVE_SVX))
						return SFE_SVX_NO_FORM ;

					psf_binheader_readf (psf, "E4", &dword) ;

					psf_log_printf (psf, " %M : %d\n", marker, dword) ;

					psf_binheader_readf (psf, "j", dword) ;
					break ;

			case CHAN_MARKER :
					if (! (parsestage & HAVE_SVX))
						return SFE_SVX_NO_FORM ;

					psf_binheader_readf (psf, "E4", &dword) ;

					psf_log_printf (psf, " %M : %d\n", marker, dword) ;

					bytecount += psf_binheader_readf (psf, "E4", &channels) ;

					if (channels == 2 || channels == 4)
						psf_log_printf (psf, "  Channels : %d => mono\n", channels) ;
					else if (channels == 6)
					{	psf->sf.channels = 2 ;
						psf_log_printf (psf, "  Channels : %d => stereo\n", channels) ;
						}
					else
						psf_log_printf (psf, "  Channels : %d *** assuming mono\n", channels) ;

					psf_binheader_readf (psf, "j", dword - bytecount) ;
					break ;


			case AUTH_MARKER :
			case c_MARKER :
					if (! (parsestage & HAVE_SVX))
						return SFE_SVX_NO_FORM ;

					psf_binheader_readf (psf, "E4", &dword) ;

					psf_log_printf (psf, " %M : %d\n", marker, dword) ;

					psf_binheader_readf (psf, "j", dword) ;
					break ;

			default :
					if (psf_isprint ((marker >> 24) & 0xFF) && psf_isprint ((marker >> 16) & 0xFF)
						&& psf_isprint ((marker >> 8) & 0xFF) && psf_isprint (marker & 0xFF))
					{	psf_binheader_readf (psf, "E4", &dword) ;

						psf_log_printf (psf, "%M : %d (unknown marker)\n", marker, dword) ;

						psf_binheader_readf (psf, "j", dword) ;
						break ;
						} ;
					if ((dword = psf_ftell (psf)) & 0x03)
					{	psf_log_printf (psf, "  Unknown chunk marker at position %d. Resynching.\n", dword - 4) ;

						psf_binheader_readf (psf, "j", -3) ;
						break ;
						} ;
					psf_log_printf (psf, "*** Unknown chunk marker : %X. Exiting parser.\n", marker) ;
					done = 1 ;
			} ;	/* switch (marker) */

		if (! psf->sf.seekable && (parsestage & HAVE_BODY))
			break ;

		if (psf_ftell (psf) >= psf->filelength - SIGNED_SIZEOF (dword))
			break ;
		} ; /* while (1) */

	if (vhdr.compression)
		return SFE_SVX_BAD_COMP ;

	if (psf->dataoffset <= 0)
		return SFE_SVX_NO_DATA ;

	return 0 ;
} /* svx_read_header */

static int
svx_close (SF_PRIVATE *psf)
{
	if (psf->file.mode == SFM_WRITE || psf->file.mode == SFM_RDWR)
		svx_write_header (psf, SF_TRUE) ;

	return 0 ;
} /* svx_close */

static int
svx_write_header (SF_PRIVATE *psf, int calc_length)
{	static	char 	annotation	[] = "libsndfile by Erik de Castro Lopo\0\0\0" ;
	sf_count_t	current ;

	current = psf_ftell (psf) ;

	if (calc_length)
	{	psf->filelength = psf_get_filelen (psf) ;

		psf->datalength = psf->filelength - psf->dataoffset ;

		if (psf->dataend)
			psf->datalength -= psf->filelength - psf->dataend ;

		psf->sf.frames = psf->datalength / (psf->bytewidth * psf->sf.channels) ;
		} ;

	psf->header [0] = 0 ;
	psf->headindex = 0 ;
	psf_fseek (psf, 0, SEEK_SET) ;

	/* FORM marker and FORM size. */
	psf_binheader_writef (psf, "Etm8", FORM_MARKER, (psf->filelength < 8) ?
			psf->filelength * 0 : psf->filelength - 8) ;

	psf_binheader_writef (psf, "m", (psf->bytewidth == 1) ? SVX8_MARKER : SV16_MARKER) ;

	/* VHDR chunk. */
	psf_binheader_writef (psf, "Em4", VHDR_MARKER, sizeof (VHDR_CHUNK)) ;
	/* VHDR : oneShotHiSamples, repeatHiSamples, samplesPerHiCycle */
	psf_binheader_writef (psf, "E444", psf->sf.frames, 0, 0) ;
	/* VHDR : samplesPerSec, octave, compression */
	psf_binheader_writef (psf, "E211", psf->sf.samplerate, 1, 0) ;
	/* VHDR : volume */
	psf_binheader_writef (psf, "E4", (psf->bytewidth == 1) ? 0xFF : 0xFFFF) ;

	if (psf->sf.channels == 2)
		psf_binheader_writef (psf, "Em44", CHAN_MARKER, 4, 6) ;

	/* Filename and annotation strings. */
	psf_binheader_writef (psf, "Emsms", NAME_MARKER, psf->file.name.c, ANNO_MARKER, annotation) ;

	/* BODY marker and size. */
	psf_binheader_writef (psf, "Etm8", BODY_MARKER, (psf->datalength < 0) ?
			psf->datalength * 0 : psf->datalength) ;

	psf_fwrite (psf->header, psf->headindex, 1, psf) ;

	if (psf->error)
		return psf->error ;

	psf->dataoffset = psf->headindex ;

	if (current > 0)
		psf_fseek (psf, current, SEEK_SET) ;

	return psf->error ;
} /* svx_write_header */

