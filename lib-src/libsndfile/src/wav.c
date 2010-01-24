/*
** Copyright (C) 1999-2009 Erik de Castro Lopo <erikd@mega-nerd.com>
** Copyright (C) 2004-2005 David Viens <davidv@plogue.com>
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
#include	<ctype.h>
#include	<time.h>

#include	"sndfile.h"
#include	"sfendian.h"
#include	"common.h"
#include	"wav_w64.h"

/*------------------------------------------------------------------------------
 * Macros to handle big/little endian issues.
 */

#define RIFF_MARKER	 (MAKE_MARKER ('R', 'I', 'F', 'F'))
#define RIFX_MARKER	 (MAKE_MARKER ('R', 'I', 'F', 'X'))
#define WAVE_MARKER	 (MAKE_MARKER ('W', 'A', 'V', 'E'))
#define fmt_MARKER	 (MAKE_MARKER ('f', 'm', 't', ' '))
#define data_MARKER	 (MAKE_MARKER ('d', 'a', 't', 'a'))
#define fact_MARKER	 (MAKE_MARKER ('f', 'a', 'c', 't'))
#define PEAK_MARKER	 (MAKE_MARKER ('P', 'E', 'A', 'K'))

#define cue_MARKER	 (MAKE_MARKER ('c', 'u', 'e', ' '))
#define LIST_MARKER	 (MAKE_MARKER ('L', 'I', 'S', 'T'))
#define slnt_MARKER	 (MAKE_MARKER ('s', 'l', 'n', 't'))
#define wavl_MARKER	 (MAKE_MARKER ('w', 'a', 'v', 'l'))
#define INFO_MARKER	 (MAKE_MARKER ('I', 'N', 'F', 'O'))
#define plst_MARKER	 (MAKE_MARKER ('p', 'l', 's', 't'))
#define adtl_MARKER	 (MAKE_MARKER ('a', 'd', 't', 'l'))
#define labl_MARKER	 (MAKE_MARKER ('l', 'a', 'b', 'l'))
#define ltxt_MARKER	 (MAKE_MARKER ('l', 't', 'x', 't'))
#define note_MARKER	 (MAKE_MARKER ('n', 'o', 't', 'e'))
#define smpl_MARKER	 (MAKE_MARKER ('s', 'm', 'p', 'l'))
#define bext_MARKER	 (MAKE_MARKER ('b', 'e', 'x', 't'))
#define iXML_MARKER	 (MAKE_MARKER ('i', 'X', 'M', 'L'))
#define levl_MARKER	 (MAKE_MARKER ('l', 'e', 'v', 'l'))
#define MEXT_MARKER	 (MAKE_MARKER ('M', 'E', 'X', 'T'))
#define DISP_MARKER	 (MAKE_MARKER ('D', 'I', 'S', 'P'))
#define acid_MARKER	 (MAKE_MARKER ('a', 'c', 'i', 'd'))
#define strc_MARKER	 (MAKE_MARKER ('s', 't', 'r', 'c'))
#define PAD_MARKER	 (MAKE_MARKER ('P', 'A', 'D', ' '))
#define afsp_MARKER	 (MAKE_MARKER ('a', 'f', 's', 'p'))
#define clm_MARKER	 (MAKE_MARKER ('c', 'l', 'm', ' '))
#define elmo_MARKER	 (MAKE_MARKER ('e', 'l', 'm', 'o'))
#define cart_MARKER	 (MAKE_MARKER ('c', 'a', 'r', 't'))

#define exif_MARKER	 (MAKE_MARKER ('e', 'x', 'i', 'f'))
#define ever_MARKER	 (MAKE_MARKER ('e', 'v', 'e', 'r'))
#define etim_MARKER	 (MAKE_MARKER ('e', 't', 'i', 'm'))
#define ecor_MARKER	 (MAKE_MARKER ('e', 'c', 'o', 'r'))
#define emdl_MARKER	 (MAKE_MARKER ('e', 'm', 'd', 'l'))
#define emnt_MARKER	 (MAKE_MARKER ('e', 'm', 'n', 't'))
#define erel_MARKER	 (MAKE_MARKER ('e', 'r', 'e', 'l'))
#define eucm_MARKER	 (MAKE_MARKER ('e', 'u', 'c', 'm'))

#define ISFT_MARKER	 (MAKE_MARKER ('I', 'S', 'F', 'T'))
#define ICRD_MARKER	 (MAKE_MARKER ('I', 'C', 'R', 'D'))
#define ICOP_MARKER	 (MAKE_MARKER ('I', 'C', 'O', 'P'))
#define IARL_MARKER	 (MAKE_MARKER ('I', 'A', 'R', 'L'))
#define IART_MARKER	 (MAKE_MARKER ('I', 'A', 'R', 'T'))
#define INAM_MARKER	 (MAKE_MARKER ('I', 'N', 'A', 'M'))
#define IENG_MARKER	 (MAKE_MARKER ('I', 'E', 'N', 'G'))
#define IART_MARKER	 (MAKE_MARKER ('I', 'A', 'R', 'T'))
#define ICOP_MARKER	 (MAKE_MARKER ('I', 'C', 'O', 'P'))
#define IPRD_MARKER	 (MAKE_MARKER ('I', 'P', 'R', 'D'))
#define ISRC_MARKER	 (MAKE_MARKER ('I', 'S', 'R', 'C'))
#define ISBJ_MARKER	 (MAKE_MARKER ('I', 'S', 'B', 'J'))
#define ICMT_MARKER	 (MAKE_MARKER ('I', 'C', 'M', 'T'))

/* Weird WAVPACK marker which can show up at the start of the DATA section. */
#define wvpk_MARKER (MAKE_MARKER ('w', 'v', 'p', 'k'))
#define OggS_MARKER (MAKE_MARKER ('O', 'g', 'g', 'S'))

#define WAV_PEAK_CHUNK_SIZE(ch) 	(2 * sizeof (int) + ch * (sizeof (float) + sizeof (int)))
#define WAV_BEXT_MIN_CHUNK_SIZE		602
#define WAV_BEXT_MAX_CHUNK_SIZE		(10 * 1024)

enum
{	HAVE_RIFF	= 0x01,
	HAVE_WAVE	= 0x02,
	HAVE_fmt	= 0x04,
	HAVE_fact	= 0x08,
	HAVE_PEAK	= 0x10,
	HAVE_data	= 0x20,
	HAVE_other	= 0x80000000
} ;



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


#if 0
/* maybe interesting one day to read the following through sf_read_raw */
/* http://www.bath.ac.uk/~masrwd/pvocex/pvocex.html */
static const EXT_SUBFORMAT MSGUID_SUBTYPE_PVOCEX =
{	0x8312B9C2, 0x2E6E, 0x11d4, {	0xA8, 0x24, 0xDE, 0x5B, 0x96, 0xC3, 0xAB, 0x21 }
} ;
#endif

/*------------------------------------------------------------------------------
** Private static functions.
*/

static int	wav_read_header	 (SF_PRIVATE *psf, int *blockalign, int *framesperblock) ;
static int	wav_write_header (SF_PRIVATE *psf, int calc_length) ;

static int	wav_write_tailer (SF_PRIVATE *psf) ;
static void wav_write_strings (SF_PRIVATE *psf, int location) ;
static int	wav_command (SF_PRIVATE *psf, int command, void *data, int datasize) ;
static int	wav_close (SF_PRIVATE *psf) ;

static int 	wav_subchunk_parse	 (SF_PRIVATE *psf, int chunk) ;
static int 	exif_subchunk_parse	 (SF_PRIVATE *psf, unsigned int length) ;
static int	wav_read_smpl_chunk (SF_PRIVATE *psf, unsigned int chunklen) ;
static int	wav_read_acid_chunk (SF_PRIVATE *psf, unsigned int chunklen) ;
static int	wav_read_bext_chunk (SF_PRIVATE *psf, unsigned int chunklen) ;
static int	wav_write_bext_chunk (SF_PRIVATE *psf) ;

/*------------------------------------------------------------------------------
** Public function.
*/

int
wav_open	 (SF_PRIVATE *psf)
{	WAV_PRIVATE * wpriv ;
	int	format, subformat, error, blockalign = 0, framesperblock = 0 ;

	if ((wpriv = calloc (1, sizeof (WAV_PRIVATE))) == NULL)
		return SFE_MALLOC_FAILED ;
	psf->container_data = wpriv ;

	wpriv->wavex_ambisonic = SF_AMBISONIC_NONE ;
	psf->str_flags = SF_STR_ALLOW_START | SF_STR_ALLOW_END ;

	if (psf->mode == SFM_READ || (psf->mode == SFM_RDWR && psf->filelength > 0))
	{	if ((error = wav_read_header (psf, &blockalign, &framesperblock)))
			return error ;
		} ;

	subformat = SF_CODEC (psf->sf.format) ;

	if (psf->mode == SFM_WRITE || psf->mode == SFM_RDWR)
	{	if (psf->is_pipe)
			return SFE_NO_PIPE_WRITE ;

		wpriv->wavex_ambisonic = SF_AMBISONIC_NONE ;

		format = SF_CONTAINER (psf->sf.format) ;
		if (format != SF_FORMAT_WAV && format != SF_FORMAT_WAVEX)
			return	SFE_BAD_OPEN_FORMAT ;

		psf->blockwidth = psf->bytewidth * psf->sf.channels ;

		/* RIFF WAVs are little-endian, RIFX WAVs are big-endian, default to little */
		psf->endian = SF_ENDIAN (psf->sf.format) ;
		if (CPU_IS_BIG_ENDIAN && psf->endian == SF_ENDIAN_CPU)
			psf->endian = SF_ENDIAN_BIG ;
		else if (psf->endian != SF_ENDIAN_BIG)
			psf->endian = SF_ENDIAN_LITTLE ;

		if (psf->mode != SFM_RDWR || psf->filelength < 44)
		{	psf->filelength = 0 ;
			psf->datalength = 0 ;
			psf->dataoffset = 0 ;
			psf->sf.frames = 0 ;
			} ;

		if (subformat == SF_FORMAT_IMA_ADPCM || subformat == SF_FORMAT_MS_ADPCM)
		{	blockalign = wav_w64_srate2blocksize (psf->sf.samplerate * psf->sf.channels) ;
			framesperblock = -1 ; /* Corrected later. */
			} ;

		/* By default, add the peak chunk to floating point files. Default behaviour
		** can be switched off using sf_command (SFC_SET_PEAK_CHUNK, SF_FALSE).
		*/
		if (psf->mode == SFM_WRITE && (subformat == SF_FORMAT_FLOAT || subformat == SF_FORMAT_DOUBLE))
		{	if ((psf->peak_info = peak_info_calloc (psf->sf.channels)) == NULL)
				return SFE_MALLOC_FAILED ;
			psf->peak_info->peak_loc = SF_PEAK_START ;
			} ;

		psf->write_header = wav_write_header ;
		} ;

	psf->container_close = wav_close ;
	psf->command = wav_command ;

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

		case SF_FORMAT_IMA_ADPCM :
					error = wav_w64_ima_init (psf, blockalign, framesperblock) ;
					break ;

		case SF_FORMAT_MS_ADPCM :
					error = wav_w64_msadpcm_init (psf, blockalign, framesperblock) ;
					break ;

		case SF_FORMAT_G721_32 :
					error = g72x_init (psf) ;
					break ;
		/* Lite remove end */

		case SF_FORMAT_GSM610 :
					error = gsm610_init (psf) ;
					break ;

		default : 	return SFE_UNIMPLEMENTED ;
		} ;

	if (psf->mode == SFM_WRITE || (psf->mode == SFM_RDWR && psf->filelength == 0))
		return psf->write_header (psf, SF_FALSE) ;

	return error ;
} /* wav_open */

/*=========================================================================
** Private functions.
*/

static int
wav_read_header	 (SF_PRIVATE *psf, int *blockalign, int *framesperblock)
{	WAV_PRIVATE	*wpriv ;
	WAV_FMT		*wav_fmt ;
	FACT_CHUNK	fact_chunk ;
	unsigned	dword = 0, marker, RIFFsize, done = 0 ;
	int			parsestage = 0, error, format = 0 ;
	char		*cptr ;

	if (psf->filelength > SF_PLATFORM_S64 (0xffffffff))
		psf_log_printf (psf, "Warning : filelength > 0xffffffff. This is bad!!!!\n") ;

	if ((wpriv = psf->container_data) == NULL)
		return SFE_INTERNAL ;
	wav_fmt = &wpriv->wav_fmt ;

	/* Set position to start of file to begin reading header. */
	psf_binheader_readf (psf, "p", 0) ;

	while (! done)
	{	psf_binheader_readf (psf, "m", &marker) ;

		switch (marker)
		{	case RIFF_MARKER :
			case RIFX_MARKER :
					if (parsestage)
						return SFE_WAV_NO_RIFF ;

					parsestage |= HAVE_RIFF ;

					/* RIFX signifies big-endian format for all header and data
					** to prevent lots of code copying here, we'll set the psf->rwf_endian
					** flag once here, and never specify endian-ness for all other header ops
					*/
					if (marker == RIFF_MARKER)
						psf->rwf_endian = SF_ENDIAN_LITTLE ;
					else
						psf->rwf_endian = SF_ENDIAN_BIG ;

					psf_binheader_readf (psf, "4", &RIFFsize) ;

					if (psf->fileoffset > 0 && psf->filelength > RIFFsize + 8)
					{	/* Set file length. */
						psf->filelength = RIFFsize + 8 ;
						if (marker == RIFF_MARKER)
							psf_log_printf (psf, "RIFF : %u\n", RIFFsize) ;
						else
							psf_log_printf (psf, "RIFX : %u\n", RIFFsize) ;
						}
					else if (psf->filelength < RIFFsize + 2 * SIGNED_SIZEOF (dword))
					{	if (marker == RIFF_MARKER)
							psf_log_printf (psf, "RIFF : %u (should be %D)\n", RIFFsize, psf->filelength - 2 * SIGNED_SIZEOF (dword)) ;
						else
							psf_log_printf (psf, "RIFX : %u (should be %D)\n", RIFFsize, psf->filelength - 2 * SIGNED_SIZEOF (dword)) ;

						RIFFsize = dword ;
						}
					else
					{	if (marker == RIFF_MARKER)
							psf_log_printf (psf, "RIFF : %u\n", RIFFsize) ;
						else
							psf_log_printf (psf, "RIFX : %u\n", RIFFsize) ;
					} ;
					break ;

			case WAVE_MARKER :
					if ((parsestage & HAVE_RIFF) != HAVE_RIFF)
						return SFE_WAV_NO_WAVE ;
					parsestage |= HAVE_WAVE ;

					psf_log_printf (psf, "WAVE\n") ;
					break ;

			case fmt_MARKER :
					if ((parsestage & (HAVE_RIFF | HAVE_WAVE)) != (HAVE_RIFF | HAVE_WAVE))
						return SFE_WAV_NO_FMT ;

					/* If this file has a SECOND fmt chunk, I don't want to know about it. */
					if (parsestage & HAVE_fmt)
						break ;

					parsestage |= HAVE_fmt ;

					psf_binheader_readf (psf, "4", &dword) ;
					psf_log_printf (psf, "fmt  : %d\n", dword) ;

					if ((error = wav_w64_read_fmt_chunk (psf, dword)))
						return error ;

					format = wav_fmt->format ;
					break ;

			case data_MARKER :
					if ((parsestage & (HAVE_RIFF | HAVE_WAVE | HAVE_fmt)) != (HAVE_RIFF | HAVE_WAVE | HAVE_fmt))
						return SFE_WAV_NO_DATA ;

					if (psf->mode == SFM_RDWR && (parsestage & HAVE_other) != 0)
						return SFE_RDWR_BAD_HEADER ;

					parsestage |= HAVE_data ;

					psf_binheader_readf (psf, "4", &dword) ;

					psf->datalength = dword ;
					psf->dataoffset = psf_ftell (psf) ;

					if (dword == 0 && RIFFsize == 8 && psf->filelength > 44)
					{	psf_log_printf (psf, "*** Looks like a WAV file which wasn't closed properly. Fixing it.\n") ;
						psf->datalength = psf->filelength - psf->dataoffset ;
						} ;

					if (psf->datalength > psf->filelength - psf->dataoffset)
					{	psf_log_printf (psf, "data : %D (should be %D)\n", psf->datalength, psf->filelength - psf->dataoffset) ;
						psf->datalength = psf->filelength - psf->dataoffset ;
						}
					else
						psf_log_printf (psf, "data : %D\n", psf->datalength) ;

					/* Only set dataend if there really is data at the end. */
					if (psf->datalength + psf->dataoffset < psf->filelength)
						psf->dataend = psf->datalength + psf->dataoffset ;

					if (format == WAVE_FORMAT_MS_ADPCM && psf->datalength % 2)
					{	psf->datalength ++ ;
						psf_log_printf (psf, "*** Data length odd. Increasing it by 1.\n") ;
						} ;

					if (! psf->sf.seekable)
						break ;

					/* Seek past data and continue reading header. */
					psf_fseek (psf, psf->datalength, SEEK_CUR) ;

					if (psf_ftell (psf) != psf->datalength + psf->dataoffset)
						psf_log_printf (psf, "*** psf_fseek past end error ***\n", dword, psf->dataoffset + psf->datalength) ;
					break ;

			case fact_MARKER :
					if ((parsestage & (HAVE_RIFF | HAVE_WAVE)) != (HAVE_RIFF | HAVE_WAVE))
						return SFE_WAV_BAD_FACT ;

					parsestage |= HAVE_fact ;

					if ((parsestage & HAVE_fmt) != HAVE_fmt)
						psf_log_printf (psf, "*** Should have 'fmt ' chunk before 'fact'\n") ;

					psf_binheader_readf (psf, "44", &dword, & (fact_chunk.frames)) ;

					if (dword > SIGNED_SIZEOF (fact_chunk))
						psf_binheader_readf (psf, "j", (int) (dword - SIGNED_SIZEOF (fact_chunk))) ;

					if (dword)
						psf_log_printf (psf, "%M : %d\n", marker, dword) ;
					else
						psf_log_printf (psf, "%M : %d (should not be zero)\n", marker, dword) ;

					psf_log_printf (psf, "  frames  : %d\n", fact_chunk.frames) ;
					break ;

			case PEAK_MARKER :
					if ((parsestage & (HAVE_RIFF | HAVE_WAVE | HAVE_fmt)) != (HAVE_RIFF | HAVE_WAVE | HAVE_fmt))
						return SFE_WAV_PEAK_B4_FMT ;

					parsestage |= HAVE_PEAK ;

					psf_binheader_readf (psf, "4", &dword) ;

					psf_log_printf (psf, "%M : %d\n", marker, dword) ;
					if (dword != WAV_PEAK_CHUNK_SIZE (psf->sf.channels))
					{	psf_binheader_readf (psf, "j", dword) ;
						psf_log_printf (psf, "*** File PEAK chunk size doesn't fit with number of channels (%d).\n", psf->sf.channels) ;
						return SFE_WAV_BAD_PEAK ;
						} ;

					if ((psf->peak_info = peak_info_calloc (psf->sf.channels)) == NULL)
						return SFE_MALLOC_FAILED ;

					/* read in rest of PEAK chunk. */
					psf_binheader_readf (psf, "44", & (psf->peak_info->version), & (psf->peak_info->timestamp)) ;

					if (psf->peak_info->version != 1)
						psf_log_printf (psf, "  version    : %d *** (should be version 1)\n", psf->peak_info->version) ;
					else
						psf_log_printf (psf, "  version    : %d\n", psf->peak_info->version) ;

					psf_log_printf (psf, "  time stamp : %d\n", psf->peak_info->timestamp) ;
					psf_log_printf (psf, "    Ch   Position       Value\n") ;

					cptr = psf->u.cbuf ;
					for (dword = 0 ; dword < (unsigned) psf->sf.channels ; dword++)
					{	float value ;
						unsigned int position ;
						psf_binheader_readf (psf, "f4", &value, &position) ;
						psf->peak_info->peaks [dword].value = value ;
						psf->peak_info->peaks [dword].position = position ;

						LSF_SNPRINTF (cptr, sizeof (psf->u.cbuf), "    %2d   %-12ld   %g\n",
								dword, (long) psf->peak_info->peaks [dword].position, psf->peak_info->peaks [dword].value) ;
						cptr [sizeof (psf->u.cbuf) - 1] = 0 ;
						psf_log_printf (psf, cptr) ;
						} ;

					psf->peak_info->peak_loc = ((parsestage & HAVE_data) == 0) ? SF_PEAK_START : SF_PEAK_END ;
					break ;

			case cue_MARKER :
					parsestage |= HAVE_other ;

					{	unsigned bytesread, cue_count ;
						int id, position, chunk_id, chunk_start, block_start, offset ;

						bytesread = psf_binheader_readf (psf, "44", &dword, &cue_count) ;
						bytesread -= 4 ; /* Remove bytes for first dword. */
						psf_log_printf (psf, "%M : %u\n", marker, dword) ;

						if (cue_count > 10)
						{	psf_log_printf (psf, "  Count : %d (skipping)\n", cue_count) ;
							psf_binheader_readf (psf, "j", cue_count * 24) ;
							break ;
							} ;

						psf_log_printf (psf, "  Count : %d\n", cue_count) ;

						while (cue_count)
						{	bytesread += psf_binheader_readf (psf, "444444", &id, &position,
									&chunk_id, &chunk_start, &block_start, &offset) ;
							psf_log_printf (psf, "   Cue ID : %2d"
												 "  Pos : %5u  Chunk : %M"
												 "  Chk Start : %d  Blk Start : %d"
												 "  Offset : %5d\n",
									id, position, chunk_id, chunk_start, block_start, offset) ;
							cue_count -- ;
							} ;

						if (bytesread != dword)
						{	psf_log_printf (psf, "**** Chunk size weirdness (%d != %d)\n", dword, bytesread) ;
							psf_binheader_readf (psf, "j", dword - bytesread) ;
							} ;
						} ;
					break ;

			case smpl_MARKER :
					parsestage |= HAVE_other ;

					psf_binheader_readf (psf, "4", &dword) ;
					psf_log_printf (psf, "smpl : %u\n", dword) ;

					if ((error = wav_read_smpl_chunk (psf, dword)))
						return error ;
					break ;

			case acid_MARKER :
					parsestage |= HAVE_other ;

					psf_binheader_readf (psf, "4", &dword) ;
					psf_log_printf (psf, "acid : %u\n", dword) ;

					if ((error = wav_read_acid_chunk (psf, dword)))
						return error ;
					break ;

			case INFO_MARKER :
			case LIST_MARKER :
					parsestage |= HAVE_other ;

					if ((error = wav_subchunk_parse (psf, marker)) != 0)
						return error ;
					break ;

			case bext_MARKER :
					/*
					The 'bext' chunk can actually be updated, so don't need to set this.
					parsestage |= HAVE_other ;
					*/
					psf_binheader_readf (psf, "4", &dword) ;
					if ((error = wav_read_bext_chunk (psf, dword)))
						return error ;
					break ;

			case PAD_MARKER :
					/*
					We can eat into a 'PAD ' chunk if we need to.
					parsestage |= HAVE_other ;
					*/
					psf_binheader_readf (psf, "4", &dword) ;
					psf_log_printf (psf, "%M : %u\n", marker, dword) ;
					dword += (dword & 1) ;
					psf_binheader_readf (psf, "j", dword) ;
					break ;

			case iXML_MARKER : /* See http://en.wikipedia.org/wiki/IXML */
			case strc_MARKER : /* Multiple of 32 bytes. */
			case afsp_MARKER :
			case clm_MARKER :
			case elmo_MARKER :
			case cart_MARKER :
			case levl_MARKER :
			case plst_MARKER :
			case DISP_MARKER :
			case MEXT_MARKER :
					parsestage |= HAVE_other ;

					psf_binheader_readf (psf, "4", &dword) ;
					psf_log_printf (psf, "%M : %u\n", marker, dword) ;
					dword += (dword & 1) ;
					psf_binheader_readf (psf, "j", dword) ;
					break ;

			default :
					parsestage |= HAVE_other ;
					if (isprint ((marker >> 24) & 0xFF) && isprint ((marker >> 16) & 0xFF)
						&& isprint ((marker >> 8) & 0xFF) && isprint (marker & 0xFF))
					{	psf_binheader_readf (psf, "4", &dword) ;
						psf_log_printf (psf, "*** %M : %d (unknown marker)\n", marker, dword) ;
						psf_binheader_readf (psf, "j", dword) ;
						break ;
						} ;
					if (psf_ftell (psf) & 0x03)
					{	psf_log_printf (psf, "  Unknown chunk marker at position %d. Resynching.\n", dword - 4) ;
						psf_binheader_readf (psf, "j", -3) ;
						break ;
						} ;
					psf_log_printf (psf, "*** Unknown chunk marker (%X) at position %D. Exiting parser.\n", marker, psf_ftell (psf) - 4) ;
					done = SF_TRUE ;
					break ;
			} ;	/* switch (dword) */

		if (! psf->sf.seekable && (parsestage & HAVE_data))
			break ;

		if (psf_ftell (psf) >= psf->filelength - SIGNED_SIZEOF (dword))
		{	psf_log_printf (psf, "End\n") ;
			break ;
			} ;
		} ; /* while (1) */

	if (! psf->dataoffset)
		return SFE_WAV_NO_DATA ;

	/* WAVs can be little or big endian */
	psf->endian = psf->rwf_endian ;

	psf_fseek (psf, psf->dataoffset, SEEK_SET) ;

	if (psf->is_pipe == 0)
	{	/*
		** Check for 'wvpk' at the start of the DATA section. Not able to
		** handle this.
		*/
		psf_binheader_readf (psf, "4", &marker) ;
		if (marker == wvpk_MARKER || marker == OggS_MARKER)
			return SFE_WAV_WVPK_DATA ;
		} ;

	/* Seek to start of DATA section. */
	psf_fseek (psf, psf->dataoffset, SEEK_SET) ;

	if (psf->blockwidth)
	{	if (psf->filelength - psf->dataoffset < psf->datalength)
			psf->sf.frames = (psf->filelength - psf->dataoffset) / psf->blockwidth ;
		else
			psf->sf.frames = psf->datalength / psf->blockwidth ;
		} ;

	switch (format)
	{	case WAVE_FORMAT_EXTENSIBLE :
			if (psf->sf.format == (SF_FORMAT_WAVEX | SF_FORMAT_MS_ADPCM))
			{	*blockalign = wav_fmt->msadpcm.blockalign ;
				*framesperblock = wav_fmt->msadpcm.samplesperblock ;
				} ;
			break ;

		case WAVE_FORMAT_PCM :
					psf->sf.format = SF_FORMAT_WAV | u_bitwidth_to_subformat (psf->bytewidth * 8) ;
					break ;

		case WAVE_FORMAT_MULAW :
		case IBM_FORMAT_MULAW :
					psf->sf.format = (SF_FORMAT_WAV | SF_FORMAT_ULAW) ;
					break ;

		case WAVE_FORMAT_ALAW :
		case IBM_FORMAT_ALAW :
					psf->sf.format = (SF_FORMAT_WAV | SF_FORMAT_ALAW) ;
					break ;

		case WAVE_FORMAT_MS_ADPCM :
					psf->sf.format = (SF_FORMAT_WAV | SF_FORMAT_MS_ADPCM) ;
					*blockalign = wav_fmt->msadpcm.blockalign ;
					*framesperblock = wav_fmt->msadpcm.samplesperblock ;
					break ;

		case WAVE_FORMAT_IMA_ADPCM :
					psf->sf.format = (SF_FORMAT_WAV | SF_FORMAT_IMA_ADPCM) ;
					*blockalign = wav_fmt->ima.blockalign ;
					*framesperblock = wav_fmt->ima.samplesperblock ;
					break ;

		case WAVE_FORMAT_GSM610 :
					psf->sf.format = (SF_FORMAT_WAV | SF_FORMAT_GSM610) ;
					break ;

		case WAVE_FORMAT_IEEE_FLOAT :
					psf->sf.format = SF_FORMAT_WAV ;
					psf->sf.format |= (psf->bytewidth == 8) ? SF_FORMAT_DOUBLE : SF_FORMAT_FLOAT ;
					break ;

		case WAVE_FORMAT_G721_ADPCM :
					psf->sf.format = SF_FORMAT_WAV | SF_FORMAT_G721_32 ;
					break ;

		default : return SFE_UNIMPLEMENTED ;
		} ;

	if (wpriv->fmt_is_broken)
		wav_w64_analyze (psf) ;

	/* Only set the format endian-ness if its non-standard big-endian. */
	if (psf->endian == SF_ENDIAN_BIG)
		psf->sf.format |= SF_ENDIAN_BIG ;

	return 0 ;
} /* wav_read_header */

static int
wav_write_fmt_chunk (SF_PRIVATE *psf)
{	int subformat, fmt_size, add_fact_chunk = 0 ;

	subformat = SF_CODEC (psf->sf.format) ;

	switch (subformat)
	{	case SF_FORMAT_PCM_U8 :
		case SF_FORMAT_PCM_16 :
		case SF_FORMAT_PCM_24 :
		case SF_FORMAT_PCM_32 :
					fmt_size = 2 + 2 + 4 + 4 + 2 + 2 ;

					/* fmt : format, channels, samplerate */
					psf_binheader_writef (psf, "4224", fmt_size, WAVE_FORMAT_PCM, psf->sf.channels, psf->sf.samplerate) ;
					/*  fmt : bytespersec */
					psf_binheader_writef (psf, "4", psf->sf.samplerate * psf->bytewidth * psf->sf.channels) ;
					/*  fmt : blockalign, bitwidth */
					psf_binheader_writef (psf, "22", psf->bytewidth * psf->sf.channels, psf->bytewidth * 8) ;
					break ;

		case SF_FORMAT_FLOAT :
		case SF_FORMAT_DOUBLE :
					fmt_size = 2 + 2 + 4 + 4 + 2 + 2 ;

					/* fmt : format, channels, samplerate */
					psf_binheader_writef (psf, "4224", fmt_size, WAVE_FORMAT_IEEE_FLOAT, psf->sf.channels, psf->sf.samplerate) ;
					/*  fmt : bytespersec */
					psf_binheader_writef (psf, "4", psf->sf.samplerate * psf->bytewidth * psf->sf.channels) ;
					/*  fmt : blockalign, bitwidth */
					psf_binheader_writef (psf, "22", psf->bytewidth * psf->sf.channels, psf->bytewidth * 8) ;

					add_fact_chunk = SF_TRUE ;
					break ;

		case SF_FORMAT_ULAW :
					fmt_size = 2 + 2 + 4 + 4 + 2 + 2 ;

					/* fmt : format, channels, samplerate */
					psf_binheader_writef (psf, "4224", fmt_size, WAVE_FORMAT_MULAW, psf->sf.channels, psf->sf.samplerate) ;
					/*  fmt : bytespersec */
					psf_binheader_writef (psf, "4", psf->sf.samplerate * psf->bytewidth * psf->sf.channels) ;
					/*  fmt : blockalign, bitwidth */
					psf_binheader_writef (psf, "22", psf->bytewidth * psf->sf.channels, 8) ;

					add_fact_chunk = SF_TRUE ;
					break ;

		case SF_FORMAT_ALAW :
					fmt_size = 2 + 2 + 4 + 4 + 2 + 2 ;

					/* fmt : format, channels, samplerate */
					psf_binheader_writef (psf, "4224", fmt_size, WAVE_FORMAT_ALAW, psf->sf.channels, psf->sf.samplerate) ;
					/*  fmt : bytespersec */
					psf_binheader_writef (psf, "4", psf->sf.samplerate * psf->bytewidth * psf->sf.channels) ;
					/*  fmt : blockalign, bitwidth */
					psf_binheader_writef (psf, "22", psf->bytewidth * psf->sf.channels, 8) ;

					add_fact_chunk = SF_TRUE ;
					break ;

		/* Lite remove start */
		case SF_FORMAT_IMA_ADPCM :
					{	int blockalign, framesperblock, bytespersec ;

						blockalign		= wav_w64_srate2blocksize (psf->sf.samplerate * psf->sf.channels) ;
						framesperblock	= 2 * (blockalign - 4 * psf->sf.channels) / psf->sf.channels + 1 ;
						bytespersec		= (psf->sf.samplerate * blockalign) / framesperblock ;

						/* fmt chunk. */
						fmt_size = 2 + 2 + 4 + 4 + 2 + 2 + 2 + 2 ;

						/* fmt : size, WAV format type, channels, samplerate, bytespersec */
						psf_binheader_writef (psf, "42244", fmt_size, WAVE_FORMAT_IMA_ADPCM,
									psf->sf.channels, psf->sf.samplerate, bytespersec) ;

						/* fmt : blockalign, bitwidth, extrabytes, framesperblock. */
						psf_binheader_writef (psf, "2222", blockalign, 4, 2, framesperblock) ;
						} ;

					add_fact_chunk = SF_TRUE ;
					break ;

		case SF_FORMAT_MS_ADPCM :
					{	int	blockalign, framesperblock, bytespersec, extrabytes ;

						blockalign		= wav_w64_srate2blocksize (psf->sf.samplerate * psf->sf.channels) ;
						framesperblock	= 2 + 2 * (blockalign - 7 * psf->sf.channels) / psf->sf.channels ;
						bytespersec		= (psf->sf.samplerate * blockalign) / framesperblock ;

						/* fmt chunk. */
						extrabytes	= 2 + 2 + MSADPCM_ADAPT_COEFF_COUNT * (2 + 2) ;
						fmt_size	= 2 + 2 + 4 + 4 + 2 + 2 + 2 + extrabytes ;

						/* fmt : size, WAV format type, channels. */
						psf_binheader_writef (psf, "422", fmt_size, WAVE_FORMAT_MS_ADPCM, psf->sf.channels) ;

						/* fmt : samplerate, bytespersec. */
						psf_binheader_writef (psf, "44", psf->sf.samplerate, bytespersec) ;

						/* fmt : blockalign, bitwidth, extrabytes, framesperblock. */
						psf_binheader_writef (psf, "22222", blockalign, 4, extrabytes, framesperblock, 7) ;

						msadpcm_write_adapt_coeffs (psf) ;
						} ;

					add_fact_chunk = SF_TRUE ;
					break ;


		case SF_FORMAT_G721_32 :
					/* fmt chunk. */
					fmt_size = 2 + 2 + 4 + 4 + 2 + 2 + 2 + 2 ;

					/* fmt : size, WAV format type, channels, samplerate, bytespersec */
					psf_binheader_writef (psf, "42244", fmt_size, WAVE_FORMAT_G721_ADPCM,
								psf->sf.channels, psf->sf.samplerate, psf->sf.samplerate * psf->sf.channels / 2) ;

					/* fmt : blockalign, bitwidth, extrabytes, auxblocksize. */
					psf_binheader_writef (psf, "2222", 64, 4, 2, 0) ;

					add_fact_chunk = SF_TRUE ;
					break ;

		/* Lite remove end */

		case SF_FORMAT_GSM610 :
					{	int	blockalign, framesperblock, bytespersec ;

						blockalign		= WAV_W64_GSM610_BLOCKSIZE ;
						framesperblock	= WAV_W64_GSM610_SAMPLES ;
						bytespersec		= (psf->sf.samplerate * blockalign) / framesperblock ;

						/* fmt chunk. */
						fmt_size = 2 + 2 + 4 + 4 + 2 + 2 + 2 + 2 ;

						/* fmt : size, WAV format type, channels. */
						psf_binheader_writef (psf, "422", fmt_size, WAVE_FORMAT_GSM610, psf->sf.channels) ;

						/* fmt : samplerate, bytespersec. */
						psf_binheader_writef (psf, "44", psf->sf.samplerate, bytespersec) ;

						/* fmt : blockalign, bitwidth, extrabytes, framesperblock. */
						psf_binheader_writef (psf, "2222", blockalign, 0, 2, framesperblock) ;
						} ;

					add_fact_chunk = SF_TRUE ;
					break ;

		default : 	return SFE_UNIMPLEMENTED ;
		} ;

	if (add_fact_chunk)
		psf_binheader_writef (psf, "tm48", fact_MARKER, 4, psf->sf.frames) ;

	return 0 ;
} /* wav_write_fmt_chunk */

static int
wavex_write_fmt_chunk (SF_PRIVATE *psf)
{	WAV_PRIVATE	*wpriv ;
	int subformat, fmt_size, add_fact_chunk = 0 ;

	if ((wpriv = psf->container_data) == NULL)
		return SFE_INTERNAL ;

	subformat = SF_CODEC (psf->sf.format) ;

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
			add_fact_chunk = SF_TRUE ;
			break ;

		case SF_FORMAT_ULAW :
			wavex_write_guid (psf, &MSGUID_SUBTYPE_MULAW) ;
			add_fact_chunk = SF_TRUE ;
			break ;

		case SF_FORMAT_ALAW :
			wavex_write_guid (psf, &MSGUID_SUBTYPE_ALAW) ;
			add_fact_chunk = SF_TRUE ;
			break ;

		case SF_FORMAT_MS_ADPCM : /* todo, GUID exists */
			return SFE_UNIMPLEMENTED ;
			wavex_write_guid (psf, &MSGUID_SUBTYPE_MS_ADPCM) ;
			add_fact_chunk = SF_TRUE ;
			break ;

		default : return SFE_UNIMPLEMENTED ;
		} ;

	if (add_fact_chunk)
		psf_binheader_writef (psf, "tm48", fact_MARKER, 4, psf->sf.frames) ;

	return 0 ;
} /* wavex_write_fmt_chunk */


static int
wav_write_header (SF_PRIVATE *psf, int calc_length)
{	sf_count_t	current ;
	int 		k, error, has_data = SF_FALSE ;

	current = psf_ftell (psf) ;

	if (current > psf->dataoffset)
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

	/*
	** RIFX signifies big-endian format for all header and data.
	** To prevent lots of code copying here, we'll set the psf->rwf_endian flag
	** once here, and never specify endian-ness for all other header operations.
	*/

	/* RIFF/RIFX marker, length, WAVE and 'fmt ' markers. */

	if (psf->endian == SF_ENDIAN_LITTLE)
		psf_binheader_writef (psf, "etm8", RIFF_MARKER, (psf->filelength < 8) ? 8 : psf->filelength - 8) ;
	else
		psf_binheader_writef (psf, "Etm8", RIFX_MARKER, (psf->filelength < 8) ? 8 : psf->filelength - 8) ;

	/* WAVE and 'fmt ' markers. */
	psf_binheader_writef (psf, "mm", WAVE_MARKER, fmt_MARKER) ;

	/* Write the 'fmt ' chunk. */
	switch (SF_CONTAINER (psf->sf.format))
	{	case SF_FORMAT_WAV :
				if ((error = wav_write_fmt_chunk (psf)) != 0)
					return error ;
				break ;

		case SF_FORMAT_WAVEX :
				if ((error = wavex_write_fmt_chunk (psf)) != 0)
					return error ;
				break ;

		default :
				return SFE_UNIMPLEMENTED ;
		} ;

	/* The LIST/INFO chunk. */
	if (psf->str_flags & SF_STR_LOCATE_START)
		wav_write_strings (psf, SF_STR_LOCATE_START) ;

	if (psf->peak_info != NULL && psf->peak_info->peak_loc == SF_PEAK_START)
	{	psf_binheader_writef (psf, "m4", PEAK_MARKER, WAV_PEAK_CHUNK_SIZE (psf->sf.channels)) ;
		psf_binheader_writef (psf, "44", 1, time (NULL)) ;
		for (k = 0 ; k < psf->sf.channels ; k++)
			psf_binheader_writef (psf, "ft8", (float) psf->peak_info->peaks [k].value, psf->peak_info->peaks [k].position) ;
		} ;

	if (psf->broadcast_var != NULL)
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

	if (psf->headindex + 16 < psf->dataoffset)
	{	/* Add PAD data if necessary. */
		k = psf->dataoffset - (psf->headindex + 16) ;
		psf_binheader_writef (psf, "m4z", PAD_MARKER, k, make_size_t (k)) ;
		} ;

	psf_binheader_writef (psf, "tm8", data_MARKER, psf->datalength) ;
	psf_fwrite (psf->header, psf->headindex, 1, psf) ;
	if (psf->error)
		return psf->error ;

	if (has_data && psf->dataoffset != psf->headindex)
	{	psf_log_printf (psf, "Oooops : has_data && psf->dataoffset != psf->headindex\n") ;
		return psf->error = SFE_INTERNAL ;
		} ;

	psf->dataoffset = psf->headindex ;

	if (! has_data)
		psf_fseek (psf, psf->dataoffset, SEEK_SET) ;
	else if (current > 0)
		psf_fseek (psf, current, SEEK_SET) ;

	return psf->error ;
} /* wav_write_header */


static int
wav_write_tailer (SF_PRIVATE *psf)
{	int		k ;

	/* Reset the current header buffer length to zero. */
	psf->header [0] = 0 ;
	psf->headindex = 0 ;

	if (psf->dataend > 0)
		psf_fseek (psf, psf->dataend, SEEK_SET) ;
	else
		psf->dataend = psf_fseek (psf, 0, SEEK_END) ;

	/* Add a PEAK chunk if requested. */
	if (psf->peak_info != NULL && psf->peak_info->peak_loc == SF_PEAK_END)
	{	psf_binheader_writef (psf, "m4", PEAK_MARKER, WAV_PEAK_CHUNK_SIZE (psf->sf.channels)) ;
		psf_binheader_writef (psf, "44", 1, time (NULL)) ;
		for (k = 0 ; k < psf->sf.channels ; k++)
			psf_binheader_writef (psf, "f4", psf->peak_info->peaks [k].value, psf->peak_info->peaks [k].position) ;
		} ;

	if (psf->str_flags & SF_STR_LOCATE_END)
		wav_write_strings (psf, SF_STR_LOCATE_END) ;

	/* Write the tailer. */
	if (psf->headindex > 0)
		psf_fwrite (psf->header, psf->headindex, 1, psf) ;

	return 0 ;
} /* wav_write_tailer */

static void
wav_write_strings (SF_PRIVATE *psf, int location)
{	int	k, prev_head_index, saved_head_index ;

	if (psf_location_string_count (psf, location) == 0)
		return ;

	prev_head_index = psf->headindex + 4 ;

	psf_binheader_writef (psf, "m4m", LIST_MARKER, 0xBADBAD, INFO_MARKER) ;

	for (k = 0 ; k < SF_MAX_STRINGS ; k++)
	{	if (psf->strings [k].type == 0)
			break ;
		if (psf->strings [k].type < 0 || psf->strings [k].flags != location)
			continue ;

		switch (psf->strings [k].type)
		{	case SF_STR_SOFTWARE :
				psf_binheader_writef (psf, "ms", ISFT_MARKER, psf->strings [k].str) ;
				break ;

			case SF_STR_TITLE :
				psf_binheader_writef (psf, "ms", INAM_MARKER, psf->strings [k].str) ;
				break ;

			case SF_STR_COPYRIGHT :
				psf_binheader_writef (psf, "ms", ICOP_MARKER, psf->strings [k].str) ;
				break ;

			case SF_STR_ARTIST :
				psf_binheader_writef (psf, "ms", IART_MARKER, psf->strings [k].str) ;
				break ;

			case SF_STR_COMMENT :
				psf_binheader_writef (psf, "ms", ICMT_MARKER, psf->strings [k].str) ;
				break ;

			case SF_STR_DATE :
				psf_binheader_writef (psf, "ms", ICRD_MARKER, psf->strings [k].str) ;
				break ;

			default :
				break ;
			} ;
		} ;

	saved_head_index = psf->headindex ;
	psf->headindex = prev_head_index ;
	psf_binheader_writef (psf, "4", saved_head_index - prev_head_index - 4) ;
	psf->headindex = saved_head_index ;

} /* wav_write_strings */

static int
wav_close (SF_PRIVATE *psf)
{
	if (psf->mode == SFM_WRITE || psf->mode == SFM_RDWR)
	{	wav_write_tailer (psf) ;

		if (psf->mode == SFM_RDWR)
		{	sf_count_t current = psf_ftell (psf) ;

			/*
			**	If the mode is RDWR and the current position is less than the
			**	filelength, truncate the file.
			*/

			if (current < psf->filelength)
			{	psf_ftruncate (psf, current) ;
				psf->filelength = current ;
				} ;
			} ;

		psf->write_header (psf, SF_TRUE) ;
		} ;

	return 0 ;
} /* wav_close */

static int
wav_command (SF_PRIVATE *psf, int command, void * UNUSED (data), int datasize)
{	WAV_PRIVATE	*wpriv ;

	if ((wpriv = psf->container_data) == NULL)
		return SFE_INTERNAL ;

	switch (command)
	{	case SFC_WAVEX_SET_AMBISONIC :
			if ((SF_CONTAINER (psf->sf.format)) == SF_FORMAT_WAVEX)
			{	if (datasize == SF_AMBISONIC_NONE)
					wpriv->wavex_ambisonic = SF_AMBISONIC_NONE ;
				else if (datasize == SF_AMBISONIC_B_FORMAT)
					wpriv->wavex_ambisonic = SF_AMBISONIC_B_FORMAT ;
				else
					return 0 ;
				} ;
			return wpriv->wavex_ambisonic ;

		case SFC_WAVEX_GET_AMBISONIC :
			return wpriv->wavex_ambisonic ;

		default :
			break ;
	} ;

	return 0 ;
} /* wav_command */

static int
wav_subchunk_parse (SF_PRIVATE *psf, int chunk)
{	sf_count_t	current_pos ;
	char		*cptr ;
	int 		dword, bytesread, length ;

	current_pos = psf_fseek (psf, 0, SEEK_CUR) ;

	bytesread = psf_binheader_readf (psf, "4", &length) ;

	if (length <= 8)
	{	/* This case is for broken files generated by PEAK. */
		psf_log_printf (psf, "%M : %d (weird length)\n", chunk, length) ;
		psf_binheader_readf (psf, "mj", &chunk, length - 4) ;
		psf_log_printf (psf, "  %M\n", chunk) ;
		return 0 ;
		} ;

	if (psf->headindex + length > SIGNED_SIZEOF (psf->header))
	{	psf_log_printf (psf, "%M : %d (too long)\n", chunk, length) ;
		psf_binheader_readf (psf, "j", length) ;
		return 0 ;
		} ;

	if (current_pos + length > psf->filelength)
	{	psf_log_printf (psf, "%M : %d (should be %d)\n", chunk, length, (int) (psf->filelength - current_pos)) ;
		length = psf->filelength - current_pos ;
		}
	else
		psf_log_printf (psf, "%M : %d\n", chunk, length) ;

	while (bytesread < length)
	{	bytesread += psf_binheader_readf (psf, "m", &chunk) ;

		switch (chunk)
		{	case adtl_MARKER :
			case INFO_MARKER :
					/* These markers don't contain anything. */
					psf_log_printf (psf, "  %M\n", chunk) ;
					break ;

			case data_MARKER :
					psf_log_printf (psf, "  %M inside a LIST block??? Backing out.\n", chunk) ;
					/* Jump back four bytes and return to caller. */
					psf_binheader_readf (psf, "j", -4) ;
					return 0 ;

			case ISFT_MARKER :
			case ICOP_MARKER :
			case IARL_MARKER :
			case IART_MARKER :
			case ICMT_MARKER :
			case ICRD_MARKER :
			case IENG_MARKER :

			case INAM_MARKER :
			case IPRD_MARKER :
			case ISBJ_MARKER :
			case ISRC_MARKER :
					bytesread += psf_binheader_readf (psf, "4", &dword) ;
					dword += (dword & 1) ;
					if (dword < 0 || dword > SIGNED_SIZEOF (psf->u.cbuf))
					{	psf_log_printf (psf, "  *** %M : %d (too big)\n", chunk, dword) ;
						psf_binheader_readf (psf, "j", dword) ;
						break ;
						} ;

					cptr = psf->u.cbuf ;
					psf_binheader_readf (psf, "b", cptr, dword) ;
					bytesread += dword ;
					if (dword > SIGNED_SIZEOF (psf->u.cbuf))
						cptr [sizeof (psf->u.cbuf) - 1] = 0 ;
					else
						cptr [dword > 0 ? dword : 0] = 0 ;
					psf_log_printf (psf, "    %M : %s\n", chunk, cptr) ;
					break ;

			case labl_MARKER :
					{	int mark_id ;

						bytesread += psf_binheader_readf (psf, "44", &dword, &mark_id) ;
						dword -= 4 ;
						dword += (dword & 1) ;
						if (dword < 1 || dword > SIGNED_SIZEOF (psf->u.cbuf))
						{	psf_log_printf (psf, "  *** %M : %d (too big)\n", chunk, dword) ;
							psf_binheader_readf (psf, "j", dword) ;
							break ;
							} ;

						cptr = psf->u.cbuf ;
						psf_binheader_readf (psf, "b", cptr, dword) ;
						bytesread += dword ;
						if (dword > SIGNED_SIZEOF (psf->u.cbuf))
							cptr [sizeof (psf->u.cbuf) - 1] = 0 ;
						else
							cptr [dword > 0 ? dword : 0] = 0 ;
						psf_log_printf (psf, "    %M : %d : %s\n", chunk, mark_id, cptr) ;
						} ;
					break ;


			case DISP_MARKER :
			case ltxt_MARKER :
			case note_MARKER :
					bytesread += psf_binheader_readf (psf, "4", &dword) ;
					dword += (dword & 1) ;
					psf_binheader_readf (psf, "j", dword) ;
					bytesread += dword ;
					psf_log_printf (psf, "    %M : %d\n", chunk, dword) ;
					break ;

			case exif_MARKER :
					psf_log_printf (psf, "  %M\n", chunk) ;
					bytesread += exif_subchunk_parse (psf, length - bytesread) ;
					break ;

			default :
					psf_binheader_readf (psf, "4", &dword) ;
					bytesread += sizeof (dword) ;
					dword += (dword & 1) ;
					psf_binheader_readf (psf, "j", dword) ;
					bytesread += dword ;
					psf_log_printf (psf, "    *** %M : %d\n", chunk, dword) ;
					if (dword > length)
						return 0 ;
					break ;
			} ;

		switch (chunk)
		{	case ISFT_MARKER :
					psf_store_string (psf, SF_STR_SOFTWARE, psf->u.cbuf) ;
					break ;
			case ICOP_MARKER :
					psf_store_string (psf, SF_STR_COPYRIGHT, psf->u.cbuf) ;
					break ;
			case INAM_MARKER :
					psf_store_string (psf, SF_STR_TITLE, psf->u.cbuf) ;
					break ;
			case IART_MARKER :
					psf_store_string (psf, SF_STR_ARTIST, psf->u.cbuf) ;
					break ;
			case ICMT_MARKER :
					psf_store_string (psf, SF_STR_COMMENT, psf->u.cbuf) ;
					break ;
			case ICRD_MARKER :
					psf_store_string (psf, SF_STR_DATE, psf->u.cbuf) ;
					break ;
			} ;
		} ;

	current_pos = psf_fseek (psf, 0, SEEK_CUR) - current_pos ;

	if (current_pos - 4 != length)
		psf_log_printf (psf, "**** Bad chunk length %d sbould be %D\n", length, current_pos - 4) ;

	return 0 ;
} /* wav_subchunk_parse */

static int
wav_read_smpl_chunk (SF_PRIVATE *psf, unsigned int chunklen)
{	unsigned int bytesread = 0, dword, sampler_data, loop_count ;
	unsigned int note, start, end, type = -1, count ;
	int j, k ;

	chunklen += (chunklen & 1) ;

	bytesread += psf_binheader_readf (psf, "4", &dword) ;
	psf_log_printf (psf, "  Manufacturer : %X\n", dword) ;

	bytesread += psf_binheader_readf (psf, "4", &dword) ;
	psf_log_printf (psf, "  Product      : %u\n", dword) ;

	bytesread += psf_binheader_readf (psf, "4", &dword) ;
	psf_log_printf (psf, "  Period       : %u nsec\n", dword) ;

	bytesread += psf_binheader_readf (psf, "4", &note) ;
	psf_log_printf (psf, "  Midi Note    : %u\n", note) ;

	bytesread += psf_binheader_readf (psf, "4", &dword) ;
	if (dword != 0)
	{	LSF_SNPRINTF (psf->u.cbuf, sizeof (psf->u.cbuf), "%f",
				 (1.0 * 0x80000000) / ((unsigned int) dword)) ;
		psf_log_printf (psf, "  Pitch Fract. : %s\n", psf->u.cbuf) ;
		}
	else
		psf_log_printf (psf, "  Pitch Fract. : 0\n") ;

	bytesread += psf_binheader_readf (psf, "4", &dword) ;
	psf_log_printf (psf, "  SMPTE Format : %u\n", dword) ;

	bytesread += psf_binheader_readf (psf, "4", &dword) ;
	LSF_SNPRINTF (psf->u.cbuf, sizeof (psf->u.cbuf), "%02d:%02d:%02d %02d",
		 (dword >> 24) & 0x7F, (dword >> 16) & 0x7F, (dword >> 8) & 0x7F, dword & 0x7F) ;
	psf_log_printf (psf, "  SMPTE Offset : %s\n", psf->u.cbuf) ;

	bytesread += psf_binheader_readf (psf, "4", &loop_count) ;
	psf_log_printf (psf, "  Loop Count   : %u\n", loop_count) ;

	/* Sampler Data holds the number of data bytes after the CUE chunks which
	** is not actually CUE data. Display value after CUE data.
	*/
	bytesread += psf_binheader_readf (psf, "4", &sampler_data) ;

	if ((psf->instrument = psf_instrument_alloc ()) == NULL)
		return SFE_MALLOC_FAILED ;

	psf->instrument->loop_count = loop_count ;

	for (j = 0 ; loop_count > 0 && chunklen - bytesread >= 24 ; j ++)
	{	bytesread += psf_binheader_readf (psf, "4", &dword) ;
		psf_log_printf (psf, "    Cue ID : %2u", dword) ;

		bytesread += psf_binheader_readf (psf, "4", &type) ;
		psf_log_printf (psf, "  Type : %2u", type) ;

		bytesread += psf_binheader_readf (psf, "4", &start) ;
		psf_log_printf (psf, "  Start : %5u", start) ;

		bytesread += psf_binheader_readf (psf, "4", &end) ;
		psf_log_printf (psf, "  End : %5u", end) ;

		bytesread += psf_binheader_readf (psf, "4", &dword) ;
		psf_log_printf (psf, "  Fraction : %5u", dword) ;

		bytesread += psf_binheader_readf (psf, "4", &count) ;
		psf_log_printf (psf, "  Count : %5u\n", count) ;

		if (j < ARRAY_LEN (psf->instrument->loops))
		{	psf->instrument->loops [j].start = start ;
			psf->instrument->loops [j].end = end ;
			psf->instrument->loops [j].count = count ;

			switch (type)
			{	case 0 :
					psf->instrument->loops [j].mode = SF_LOOP_FORWARD ;
					break ;
				case 1 :
					psf->instrument->loops [j].mode = SF_LOOP_ALTERNATING ;
					break ;
				case 2 :
					psf->instrument->loops [j].mode = SF_LOOP_BACKWARD ;
					break ;
				default:
					psf->instrument->loops [j].mode = SF_LOOP_NONE ;
					break ;
				} ;
			} ;

		loop_count -- ;
		} ;

	if (chunklen - bytesread == 0)
	{	if (sampler_data != 0)
			psf_log_printf (psf, "  Sampler Data : %u (should be 0)\n", sampler_data) ;
		else
			psf_log_printf (psf, "  Sampler Data : %u\n", sampler_data) ;
		}
	else
	{	if (sampler_data != chunklen - bytesread)
		{	psf_log_printf (psf, "  Sampler Data : %u (should have been %u)\n", sampler_data, chunklen - bytesread) ;
			sampler_data = chunklen - bytesread ;
			}
		else
			psf_log_printf (psf, "  Sampler Data : %u\n", sampler_data) ;

		psf_log_printf (psf, "      ") ;
		for (k = 0 ; k < (int) sampler_data ; k++)
		{	char ch ;

			if (k > 0 && (k % 20) == 0)
				psf_log_printf (psf, "\n      ") ;

			bytesread += psf_binheader_readf (psf, "1", &ch) ;
			psf_log_printf (psf, "%02X ", ch & 0xFF) ;
			} ;

		psf_log_printf (psf, "\n") ;
		} ;

	psf->instrument->basenote = note ;
	psf->instrument->gain = 1 ;
	psf->instrument->velocity_lo = psf->instrument->key_lo = 0 ;
	psf->instrument->velocity_hi = psf->instrument->key_hi = 127 ;

	return 0 ;
} /* wav_read_smpl_chunk */

/*
** The acid chunk goes a little something like this:
**
** 4 bytes          'acid'
** 4 bytes (int)     length of chunk starting at next byte
**
** 4 bytes (int)     type of file:
**        this appears to be a bit mask,however some combinations
**        are probably impossible and/or qualified as "errors"
**
**        0x01 On: One Shot         Off: Loop
**        0x02 On: Root note is Set Off: No root
**        0x04 On: Stretch is On,   Off: Strech is OFF
**        0x08 On: Disk Based       Off: Ram based
**        0x10 On: ??????????       Off: ????????? (Acidizer puts that ON)
**
** 2 bytes (short)      root note
**        if type 0x10 is OFF : [C,C#,(...),B] -> [0x30 to 0x3B]
**        if type 0x10 is ON  : [C,C#,(...),B] -> [0x3C to 0x47]
**         (both types fit on same MIDI pitch albeit different octaves, so who cares)
**
** 2 bytes (short)      ??? always set to 0x8000
** 4 bytes (float)      ??? seems to be always 0
** 4 bytes (int)        number of beats
** 2 bytes (short)      meter denominator   //always 4 in SF/ACID
** 2 bytes (short)      meter numerator     //always 4 in SF/ACID
**                      //are we sure about the order?? usually its num/denom
** 4 bytes (float)      tempo
**
*/

static int
wav_read_acid_chunk (SF_PRIVATE *psf, unsigned int chunklen)
{	unsigned int bytesread = 0 ;
	int	beats, flags ;
	short rootnote, q1, meter_denom, meter_numer ;
	float q2, tempo ;

	chunklen += (chunklen & 1) ;

	bytesread += psf_binheader_readf (psf, "422f", &flags, &rootnote, &q1, &q2) ;

	LSF_SNPRINTF (psf->u.cbuf, sizeof (psf->u.cbuf), "%f", q2) ;

	psf_log_printf (psf, "  Flags     : 0x%04x (%s,%s,%s,%s,%s)\n", flags,
			(flags & 0x01) ? "OneShot" : "Loop",
			(flags & 0x02) ? "RootNoteValid" : "RootNoteInvalid",
			(flags & 0x04) ? "StretchOn" : "StretchOff",
			(flags & 0x08) ? "DiskBased" : "RAMBased",
			(flags & 0x10) ? "??On" : "??Off") ;

	psf_log_printf (psf, "  Root note : 0x%x\n  ????      : 0x%04x\n  ????      : %s\n",
				rootnote, q1, psf->u.cbuf) ;

	bytesread += psf_binheader_readf (psf, "422f", &beats, &meter_denom, &meter_numer, &tempo) ;
	LSF_SNPRINTF (psf->u.cbuf, sizeof (psf->u.cbuf), "%f", tempo) ;
	psf_log_printf (psf, "  Beats     : %d\n  Meter     : %d/%d\n  Tempo     : %s\n",
				beats, meter_numer, meter_denom, psf->u.cbuf) ;

	psf_binheader_readf (psf, "j", chunklen - bytesread) ;

	if ((psf->loop_info = calloc (1, sizeof (SF_LOOP_INFO))) == NULL)
		return SFE_MALLOC_FAILED ;

	psf->loop_info->time_sig_num	= meter_numer ;
	psf->loop_info->time_sig_den	= meter_denom ;
	psf->loop_info->loop_mode		= (flags & 0x01) ? SF_LOOP_NONE : SF_LOOP_FORWARD ;
	psf->loop_info->num_beats		= beats ;
	psf->loop_info->bpm				= tempo ;
	psf->loop_info->root_key		= (flags & 0x02) ? rootnote : -1 ;

	return 0 ;
} /* wav_read_acid_chunk */

static int
wav_read_bext_chunk (SF_PRIVATE *psf, unsigned int chunksize)
{
	SF_BROADCAST_INFO* b ;
	unsigned int bytes = 0 ;

	if (chunksize < WAV_BEXT_MIN_CHUNK_SIZE)
	{	psf_log_printf (psf, "bext : %u (should be >= %d)\n", chunksize, WAV_BEXT_MIN_CHUNK_SIZE) ;
		psf_binheader_readf (psf, "j", chunksize) ;
		return 0 ;
		} ;

	if (chunksize > WAV_BEXT_MAX_CHUNK_SIZE)
	{	psf_log_printf (psf, "bext : %u (should be < %d)\n", chunksize, WAV_BEXT_MAX_CHUNK_SIZE) ;
		psf_binheader_readf (psf, "j", chunksize) ;
		return 0 ;
		} ;

	psf_log_printf (psf, "bext : %u\n", chunksize) ;

	if ((psf->broadcast_var = broadcast_var_alloc (chunksize + 128)) == NULL)
	{	psf->error = SFE_MALLOC_FAILED ;
		return psf->error ;
		} ;

	b = & psf->broadcast_var->binfo ;

	bytes += psf_binheader_readf (psf, "b", b->description, sizeof (b->description)) ;
	bytes += psf_binheader_readf (psf, "b", b->originator, sizeof (b->originator)) ;
	bytes += psf_binheader_readf (psf, "b", b->originator_reference, sizeof (b->originator_reference)) ;
	bytes += psf_binheader_readf (psf, "b", b->origination_date, sizeof (b->origination_date)) ;
	bytes += psf_binheader_readf (psf, "b", b->origination_time, sizeof (b->origination_time)) ;
	bytes += psf_binheader_readf (psf, "442", &b->time_reference_low, &b->time_reference_high, &b->version) ;
	bytes += psf_binheader_readf (psf, "bj", &b->umid, sizeof (b->umid), 190) ;

	if (chunksize > WAV_BEXT_MIN_CHUNK_SIZE)
	{	/* File has coding history data. */

		b->coding_history_size = chunksize - WAV_BEXT_MIN_CHUNK_SIZE ;

		/* We do not parse the coding history */
		bytes += psf_binheader_readf (psf, "b", b->coding_history, b->coding_history_size) ;
		} ;

	if (bytes < chunksize)
		psf_binheader_readf (psf, "j", chunksize - bytes) ;

	return 0 ;
} /* wav_read_bext_chunk */

static int
wav_write_bext_chunk (SF_PRIVATE *psf)
{	SF_BROADCAST_INFO *b ;

	if (psf->broadcast_var == NULL)
		return -1 ;

	b = & psf->broadcast_var->binfo ;

	psf_binheader_writef (psf, "m4", bext_MARKER, WAV_BEXT_MIN_CHUNK_SIZE + b->coding_history_size) ;

	/*
	**	Note that it is very important the the field widths of the SF_BROADCAST_INFO
	**	struct match those for the bext chunk fields.
	*/

	psf_binheader_writef (psf, "b", b->description, sizeof (b->description)) ;
	psf_binheader_writef (psf, "b", b->originator, sizeof (b->originator)) ;
	psf_binheader_writef (psf, "b", b->originator_reference, sizeof (b->originator_reference)) ;
	psf_binheader_writef (psf, "b", b->origination_date, sizeof (b->origination_date)) ;
	psf_binheader_writef (psf, "b", b->origination_time, sizeof (b->origination_time)) ;
	psf_binheader_writef (psf, "442", b->time_reference_low, b->time_reference_high, b->version) ;
	psf_binheader_writef (psf, "b", b->umid, sizeof (b->umid)) ;
	psf_binheader_writef (psf, "z", make_size_t (190)) ;

	if (b->coding_history_size > 0)
		psf_binheader_writef (psf, "b", b->coding_history, make_size_t (b->coding_history_size)) ;

	return 0 ;
} /* wav_write_bext_chunk */

static int
exif_fill_and_sink (SF_PRIVATE *psf, char* buf, size_t bufsz, size_t toread)
{
	size_t bytesread = 0 ;

	buf [0] = 0 ;
	bufsz -= 1 ;
	if (toread < bufsz)
		bufsz = toread ;
	bytesread = psf_binheader_readf (psf, "b", buf, bufsz) ;
	buf [bufsz] = 0 ;

	if (bytesread == bufsz && toread > bufsz)
		bytesread += psf_binheader_readf (psf, "j", toread - bufsz) ;

	return bytesread ;
} /* exif_fill_and_sink */

/*
** Exif specification for audio files, at JEITA CP-3451 Exif 2.2 section 5
** (Exif Audio File Specification) http://www.exif.org/Exif2-2.PDF
*/
static int
exif_subchunk_parse (SF_PRIVATE *psf, unsigned int length)
{
	unsigned marker, dword, vmajor = -1, vminor = -1, bytesread = 0 ;
	char buf [4096] ;

	while (bytesread < length)
	{
		bytesread += psf_binheader_readf (psf, "m", &marker) ;

		switch (marker)
		{
			case 0 : /* camera padding? */
				break ;

			case ever_MARKER :
				bytesread += psf_binheader_readf (psf, "j4", 4, &dword) ;
				vmajor = 10 * (((dword >> 24) & 0xff) - '0') + (((dword >> 16) & 0xff) - '0') ;
				vminor = 10 * (((dword >> 8) & 0xff) - '0') + ((dword & 0xff) - '0') ;
				psf_log_printf (psf, "    EXIF Version : %u.%02u\n", vmajor, vminor) ;
				break ;

			case emnt_MARKER : /* design information: null-terminated string */
			case emdl_MARKER : /* model name ; null-terminated string */
			case ecor_MARKER : /* manufacturer: null-terminated string */
			case etim_MARKER : /* creation time: null-terminated string in the format "hour:minute:second.subsecond" */
			case erel_MARKER : /* relation info: null-terminated string (filename) */
			case eucm_MARKER : /* user comment: 4-byte size follows, then possibly unicode data */
				psf_binheader_readf (psf, "4", &dword) ;
				bytesread += sizeof (dword) ;
				dword += (dword & 1) ;

				bytesread += exif_fill_and_sink (psf, buf, sizeof (buf), dword) ;

				/* BAD - don't know what's going on here -- maybe a bug in the camera */
				/* field should be NULL-terminated but there's no room for it with the reported number */
				/*  example output:     emdl : 8 (EX-Z1050) */
				if (marker == emdl_MARKER && dword == strlen (buf) /* should be >= strlen+1*/)
				{	psf_log_printf (psf, "    *** field size too small for string (sinking 2 bytes)\n") ;
					bytesread += psf_binheader_readf (psf, "j", 2) ;
					} ;

				psf_log_printf (psf, "    %M : %d (%s)\n", marker, dword, buf) ;
				if (dword > length)
					return bytesread ;
				break ;

			default :
				psf_log_printf (psf, "    *** %M (%d): -- ignored --\n", marker, marker) ;
				break ;
			} ;
		} ;

	return bytesread ;
} /* exif_subchunk_parse */

