/*
** Copyright (C) 2008-2009 Erik de Castro Lopo <erikd@mega-nerd.com>
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

/*
**	This format documented at:
**	http://www.sr.se/utveckling/tu/bwf/prog/RF_64v1_4.pdf
*/

#include	"sfconfig.h"

#include	<stdio.h>
#include	<fcntl.h>
#include	<string.h>
#include	<ctype.h>

#include	"sndfile.h"
#include	"sfendian.h"
#include	"common.h"
#include	"wav_w64.h"

/*------------------------------------------------------------------------------
** Macros to handle big/little endian issues.
*/
#define	RF64_MARKER		MAKE_MARKER ('R', 'F', '6', '4')
#define	FFFF_MARKER		MAKE_MARKER (0xff, 0xff, 0xff, 0xff)
#define	WAVE_MARKER		MAKE_MARKER ('W', 'A', 'V', 'E')
#define	ds64_MARKER		MAKE_MARKER ('d', 's', '6', '4')
#define	fmt_MARKER		MAKE_MARKER ('f', 'm', 't', ' ')
#define	fact_MARKER		MAKE_MARKER ('f', 'a', 'c', 't')
#define	data_MARKER		MAKE_MARKER ('d', 'a', 't', 'a')

/*------------------------------------------------------------------------------
** Typedefs.
*/

/*------------------------------------------------------------------------------
** Private static functions.
*/

static int	rf64_read_header (SF_PRIVATE *psf) ;
static int	rf64_write_header (SF_PRIVATE *psf, int calc_length) ;
static int	rf64_close (SF_PRIVATE *psf) ;

/*------------------------------------------------------------------------------
** Public function.
*/

int
rf64_open (SF_PRIVATE *psf)
{	WAV_PRIVATE *wpriv ;
	int	subformat, error = 0 ;

	if ((wpriv = calloc (1, sizeof (WAV_PRIVATE))) == NULL)
		return SFE_MALLOC_FAILED ;
	psf->container_data = wpriv ;

	/* All RF64 files are little endian. */
	psf->endian = SF_ENDIAN_LITTLE ;


	if (psf->mode == SFM_READ || (psf->mode == SFM_RDWR && psf->filelength > 0))
	{	if ((error = rf64_read_header (psf)) != 0)
			return error ;
		} ;

	if ((psf->sf.format & SF_FORMAT_TYPEMASK) != SF_FORMAT_RF64)
		return	SFE_BAD_OPEN_FORMAT ;

	subformat = psf->sf.format & SF_FORMAT_SUBMASK ;

	if (psf->mode == SFM_WRITE || psf->mode == SFM_RDWR)
	{	if (psf->is_pipe)
			return SFE_NO_PIPE_WRITE ;

		psf->blockwidth = psf->bytewidth * psf->sf.channels ;

		if ((error = rf64_write_header (psf, SF_FALSE)))
			return error ;

		psf->write_header = rf64_write_header ;
		} ;

	psf->container_close = rf64_close ;

	switch (subformat)
	{	case SF_FORMAT_PCM_U8 :
		case SF_FORMAT_PCM_16 :
		case SF_FORMAT_PCM_24 :
		case SF_FORMAT_PCM_32 :
					error = pcm_init (psf) ;
					break ;

		case SF_FORMAT_ULAW :
					error = ulaw_init (psf) ;
					break ;

		case SF_FORMAT_ALAW :
					error = alaw_init (psf) ;
					break ;

		/* Lite remove start */
		case SF_FORMAT_FLOAT :
					error = float32_init (psf) ;
					break ;

		case SF_FORMAT_DOUBLE :
					error = double64_init (psf) ;
					break ;

		/* Lite remove end */

		default : 	return SFE_UNIMPLEMENTED ;
		} ;

	return error ;
} /* rf64_open */

/*------------------------------------------------------------------------------
*/

static int
rf64_read_header (SF_PRIVATE *psf)
{	WAV_PRIVATE *wpriv ;
	sf_count_t riff_size, data_size ;
	unsigned int size32 ;
	int marker, marks [2], error, done = 0 ;

	if ((wpriv = psf->container_data) == NULL)
		return SFE_INTERNAL ;

	/* Set position to start of file to begin reading header. */
	psf_binheader_readf (psf, "pmmm", 0, &marker, marks, marks + 1) ;
	if (marker != RF64_MARKER || marks [0] != FFFF_MARKER || marks [1] != WAVE_MARKER)
		return SFE_RF64_NOT_RF64 ;

	psf_log_printf (psf, "%M\n  %M\n", RF64_MARKER, WAVE_MARKER) ;

	while (! done)
	{	psf_binheader_readf (psf, "em4", &marker, &size32) ;

		switch (marker)
		{	case ds64_MARKER :
					psf_log_printf (psf, "%M : %u\n", marker, size32) ;

					psf_binheader_readf (psf, "888", &riff_size, &data_size, &psf->sf.frames) ;
					psf_log_printf (psf, "  Riff size : %D\n  Data size : %D\n  Frames    : %D\n",
											riff_size, data_size, psf->sf.frames) ;

					psf_binheader_readf (psf, "4", &size32) ;
					psf_log_printf (psf, "  Table len : %u\n", size32) ;

					psf_binheader_readf (psf, "jm4", size32 + 4, &marker, &size32) ;
					psf_log_printf (psf, "%M : %u\n", marker, size32) ;

					if ((error = wav_w64_read_fmt_chunk (psf, size32)) != 0)
						return error ;
					psf->sf.format = SF_FORMAT_RF64 | (psf->sf.format & SF_FORMAT_SUBMASK) ;
					break ;

			case data_MARKER :
					psf_log_printf (psf, "%M : %x\n", marker, size32) ;
					psf->dataoffset = psf->headindex ;
					done = SF_TRUE ;
					break ;

			default :
					if (isprint ((marker >> 24) & 0xFF) && isprint ((marker >> 16) & 0xFF)
						&& isprint ((marker >> 8) & 0xFF) && isprint (marker & 0xFF))
					{	psf_binheader_readf (psf, "4", &size32) ;
						psf_log_printf (psf, "*** %M : %d (unknown marker)\n", marker, size32) ;
						if (size32 < 8)
							done = SF_TRUE ;
						psf_binheader_readf (psf, "j", size32) ;
						break ;
						} ;
					if (psf_ftell (psf) & 0x03)
					{	psf_log_printf (psf, "  Unknown chunk marker at position %d. Resynching.\n", size32 - 4) ;
						psf_binheader_readf (psf, "j", -3) ;
						break ;
						} ;
					psf_log_printf (psf, "*** Unknown chunk marker (%X) at position %D. Exiting parser.\n", marker, psf_ftell (psf) - 4) ;
					done = SF_TRUE ;
				break ;
			} ;	/* switch (marker) */

		if (psf_ftell (psf) >= psf->filelength - SIGNED_SIZEOF (marker))
		{	psf_log_printf (psf, "End\n") ;
			break ;
			} ;
		} ;

	return 0 ;
} /* rf64_read_header */

/*  known WAVEFORMATEXTENSIBLE GUIDS  */
static const EXT_SUBFORMAT MSGUID_SUBTYPE_PCM =
{	0x00000001, 0x0000, 0x0010, {	0x80, 0x00, 0x00, 0xaa, 0x00, 0x38, 0x9b, 0x71 }
} ;

static const EXT_SUBFORMAT MSGUID_SUBTYPE_MS_ADPCM =
{	0x00000002, 0x0000, 0x0010, {	0x80, 0x00, 0x00, 0xaa, 0x00, 0x38, 0x9b, 0x71 }
} ;

static const EXT_SUBFORMAT MSGUID_SUBTYPE_IEEE_FLOAT =
{	0x00000003, 0x0000, 0x0010, {	0x80, 0x00, 0x00, 0xaa, 0x00, 0x38, 0x9b, 0x71 }
} ;

static const EXT_SUBFORMAT MSGUID_SUBTYPE_ALAW =
{	0x00000006, 0x0000, 0x0010, {	0x80, 0x00, 0x00, 0xaa, 0x00, 0x38, 0x9b, 0x71 }
} ;

static const EXT_SUBFORMAT MSGUID_SUBTYPE_MULAW =
{	0x00000007, 0x0000, 0x0010, {	0x80, 0x00, 0x00, 0xaa, 0x00, 0x38, 0x9b, 0x71 }
} ;

/*
** the next two are from
** http://dream.cs.bath.ac.uk/researchdev/wave-ex/bformat.html
*/
static const EXT_SUBFORMAT MSGUID_SUBTYPE_AMBISONIC_B_FORMAT_PCM =
{	0x00000001, 0x0721, 0x11d3, {	0x86, 0x44, 0xC8, 0xC1, 0xCA, 0x00, 0x00, 0x00 }
} ;

static const EXT_SUBFORMAT MSGUID_SUBTYPE_AMBISONIC_B_FORMAT_IEEE_FLOAT =
{	0x00000003, 0x0721, 0x11d3, {	0x86, 0x44, 0xC8, 0xC1, 0xCA, 0x00, 0x00, 0x00 }
} ;


static int
wavex_write_fmt_chunk (SF_PRIVATE *psf)
{	WAV_PRIVATE	*wpriv ;
	int subformat, fmt_size, add_fact_chunk = 0 ;

	if ((wpriv = psf->container_data) == NULL)
		return SFE_INTERNAL ;

	subformat = psf->sf.format & SF_FORMAT_SUBMASK ;

	/* initial section (same for all, it appears) */
	switch (subformat)
	{	case SF_FORMAT_PCM_U8 :
		case SF_FORMAT_PCM_16 :
		case SF_FORMAT_PCM_24 :
		case SF_FORMAT_PCM_32 :
		case SF_FORMAT_FLOAT :
		case SF_FORMAT_DOUBLE :
		case SF_FORMAT_ULAW :
		case SF_FORMAT_ALAW :
			fmt_size = 2 + 2 + 4 + 4 + 2 + 2 + 2 + 2 + 4 + 4 + 2 + 2 + 8 ;

			/* fmt : format, channels, samplerate */
			psf_binheader_writef (psf, "4224", fmt_size, WAVE_FORMAT_EXTENSIBLE, psf->sf.channels, psf->sf.samplerate) ;
			/*  fmt : bytespersec */
			psf_binheader_writef (psf, "4", psf->sf.samplerate * psf->bytewidth * psf->sf.channels) ;
			/*  fmt : blockalign, bitwidth */
			psf_binheader_writef (psf, "22", psf->bytewidth * psf->sf.channels, psf->bytewidth * 8) ;

			/* cbSize 22 is sizeof (WAVEFORMATEXTENSIBLE) - sizeof (WAVEFORMATEX) */
			psf_binheader_writef (psf, "2", 22) ;

			/* wValidBitsPerSample, for our use same as bitwidth as we use it fully */
			psf_binheader_writef (psf, "2", psf->bytewidth * 8) ;

			/* For an Ambisonic file set the channel mask to zero.
			** Otherwise use a default based on the channel count.
			*/
			if (wpriv->wavex_ambisonic != SF_AMBISONIC_NONE)
				psf_binheader_writef (psf, "4", 0) ;
			else
			{	/*
				** Ok some liberty is taken here to use the most commonly used channel masks
				** instead of "no mapping". If you really want to use "no mapping" for 8 channels and less
				** please don't use wavex. (otherwise we'll have to create a new SF_COMMAND)
				*/
				switch (psf->sf.channels)
				{	case 1 :	/* center channel mono */
						psf_binheader_writef (psf, "4", 0x4) ;
						break ;

					case 2 :	/* front left and right */
						psf_binheader_writef (psf, "4", 0x1 | 0x2) ;
						break ;

					case 4 :	/* Quad */
						psf_binheader_writef (psf, "4", 0x1 | 0x2 | 0x10 | 0x20) ;
						break ;

					case 6 :	/* 5.1 */
						psf_binheader_writef (psf, "4", 0x1 | 0x2 | 0x4 | 0x8 | 0x10 | 0x20) ;
						break ;

					case 8 :	/* 7.1 */
						psf_binheader_writef (psf, "4", 0x1 | 0x2 | 0x4 | 0x8 | 0x10 | 0x20 | 0x40 | 0x80) ;
						break ;

					default :	/* 0 when in doubt , use direct out, ie NO mapping*/
						psf_binheader_writef (psf, "4", 0x0) ;
						break ;
					} ;
				} ;
			break ;

		case SF_FORMAT_MS_ADPCM : /* Todo, GUID exists might have different header as per wav_write_header */
		default :
			return SFE_UNIMPLEMENTED ;
		} ;

	/* GUID section, different for each */

	switch (subformat)
	{	case SF_FORMAT_PCM_U8 :
		case SF_FORMAT_PCM_16 :
		case SF_FORMAT_PCM_24 :
		case SF_FORMAT_PCM_32 :
			wavex_write_guid (psf, wpriv->wavex_ambisonic == SF_AMBISONIC_NONE ?
						&MSGUID_SUBTYPE_PCM : &MSGUID_SUBTYPE_AMBISONIC_B_FORMAT_PCM) ;
			break ;

		case SF_FORMAT_FLOAT :
		case SF_FORMAT_DOUBLE :
			wavex_write_guid (psf, wpriv->wavex_ambisonic == SF_AMBISONIC_NONE ?
						&MSGUID_SUBTYPE_IEEE_FLOAT : &MSGUID_SUBTYPE_AMBISONIC_B_FORMAT_IEEE_FLOAT) ;
			break ;

		case SF_FORMAT_ULAW :
			wavex_write_guid (psf, &MSGUID_SUBTYPE_MULAW) ;
			break ;

		case SF_FORMAT_ALAW :
			wavex_write_guid (psf, &MSGUID_SUBTYPE_ALAW) ;
			break ;

		case SF_FORMAT_MS_ADPCM : /* todo, GUID exists */
			return SFE_UNIMPLEMENTED ;
			wavex_write_guid (psf, &MSGUID_SUBTYPE_MS_ADPCM) ;
			break ;

		default : return SFE_UNIMPLEMENTED ;
		} ;

	if (add_fact_chunk)
		psf_binheader_writef (psf, "tm48", fact_MARKER, 4, psf->sf.frames) ;

	return 0 ;
} /* wavex_write_fmt_chunk */


static int
rf64_write_header (SF_PRIVATE *psf, int calc_length)
{	sf_count_t	current ;
	int 		error = 0, has_data = SF_FALSE ;

	current = psf_ftell (psf) ;

	if (psf->dataoffset > 0 && current > psf->dataoffset)
		has_data = SF_TRUE ;

	if (calc_length)
	{	psf->filelength = psf_get_filelen (psf) ;

		psf->datalength = psf->filelength - psf->dataoffset ;

		if (psf->dataend)
			psf->datalength -= psf->filelength - psf->dataend ;

		if (psf->bytewidth > 0)
			psf->sf.frames = psf->datalength / (psf->bytewidth * psf->sf.channels) ;
		} ;

	/* Reset the current header length to zero. */
	psf->header [0] = 0 ;
	psf->headindex = 0 ;
	psf_fseek (psf, 0, SEEK_SET) ;

	psf_binheader_writef (psf, "em4m", RF64_MARKER, 0xffffffff, WAVE_MARKER) ;

	psf_binheader_writef (psf, "m488844", ds64_MARKER, 32, psf->filelength, psf->datalength, psf->sf.frames, 0, 0x005c0064) ;

	/* WAVE and 'fmt ' markers. */
	psf_binheader_writef (psf, "m", fmt_MARKER) ;

	/* Write the 'fmt ' chunk. */
	switch (psf->sf.format & SF_FORMAT_TYPEMASK)
	{	case SF_FORMAT_WAV :
				psf_log_printf (psf, "ooops SF_FORMAT_WAV\n") ;
				return SFE_UNIMPLEMENTED ;
				break ;

		case SF_FORMAT_WAVEX :
		case SF_FORMAT_RF64 :
				if ((error = wavex_write_fmt_chunk (psf)) != 0)
					return error ;
				break ;

		default :
				return SFE_UNIMPLEMENTED ;
		} ;

#if 0
	/* The LIST/INFO chunk. */
	if (psf->str_flags & SF_STR_LOCATE_START)
		wav_write_strings (psf, SF_STR_LOCATE_START) ;

	if (psf->peak_info != NULL && psf->peak_info->peak_loc == SF_PEAK_START)
	{	psf_binheader_writef (psf, "m4", PEAK_MARKER, WAV_PEAK_CHUNK_SIZE (psf->sf.channels)) ;
		psf_binheader_writef (psf, "44", 1, time (NULL)) ;
		for (k = 0 ; k < psf->sf.channels ; k++)
			psf_binheader_writef (psf, "ft8", (float) psf->peak_info->peaks [k].value, psf->peak_info->peaks [k].position) ;
		} ;

	if (psf->broadcast_info != NULL)
		wav_write_bext_chunk (psf) ;

	if (psf->instrument != NULL)
	{	int		tmp ;
		double	dtune = (double) (0x40000000) / 25.0 ;

		psf_binheader_writef (psf, "m4", smpl_MARKER, 9 * 4 + psf->instrument->loop_count * 6 * 4) ;
		psf_binheader_writef (psf, "44", 0, 0) ; /* Manufacturer zero is everyone */
		tmp = (int) (1.0e9 / psf->sf.samplerate) ; /* Sample period in nano seconds */
		psf_binheader_writef (psf, "44", tmp, psf->instrument->basenote) ;
		tmp = (unsigned int) (psf->instrument->detune * dtune + 0.5) ;
		psf_binheader_writef (psf, "4", tmp) ;
		psf_binheader_writef (psf, "44", 0, 0) ; /* SMTPE format */
		psf_binheader_writef (psf, "44", psf->instrument->loop_count, 0) ;

		for (tmp = 0 ; tmp < psf->instrument->loop_count ; tmp++)
		{	int type ;

			type = psf->instrument->loops [tmp].mode ;
			type = (type == SF_LOOP_FORWARD ? 0 : type==SF_LOOP_BACKWARD ? 2 : type == SF_LOOP_ALTERNATING ? 1 : 32) ;

			psf_binheader_writef (psf, "44", tmp, type) ;
			psf_binheader_writef (psf, "44", psf->instrument->loops [tmp].start, psf->instrument->loops [tmp].end) ;
			psf_binheader_writef (psf, "44", 0, psf->instrument->loops [tmp].count) ;
			} ;
		} ;

	if (psf->headindex + 8 < psf->dataoffset)
	{	/* Add PAD data if necessary. */
		k = psf->dataoffset - 16 - psf->headindex ;
		psf_binheader_writef (psf, "m4z", PAD_MARKER, k, make_size_t (k)) ;
		} ;

#endif

	psf_binheader_writef (psf, "m4", data_MARKER, 0xffffffff) ;

	psf_fwrite (psf->header, psf->headindex, 1, psf) ;
	if (psf->error)
		return psf->error ;

	if (has_data && psf->dataoffset != psf->headindex)
	{	printf ("Oooops : has_data && psf->dataoffset != psf->headindex\n") ;
		return psf->error = SFE_INTERNAL ;
		} ;

	psf->dataoffset = psf->headindex ;

	if (! has_data)
		psf_fseek (psf, psf->dataoffset, SEEK_SET) ;
	else if (current > 0)
		psf_fseek (psf, current, SEEK_SET) ;

	return psf->error ;
} /* rf64_write_header */

static int
rf64_close (SF_PRIVATE *psf)
{
	if (psf->mode == SFM_WRITE || psf->mode == SFM_RDWR)
	{	// rf64_write_tailer (psf) ;

		psf->write_header (psf, SF_TRUE) ;
		} ;

	return 0 ;
} /* rf64_close */
