/*
** Copyright (C) 1999-2009 Erik de Castro Lopo <erikd@mega-nerd.com>
** Copyright (C) 2005 David Viens <davidv@plogue.com>
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
#include <string.h>
#include <time.h>
#include <ctype.h>

#include "sndfile.h"
#include "sfendian.h"
#include "common.h"

/*------------------------------------------------------------------------------
 * Macros to handle big/little endian issues.
 */

#define FORM_MARKER		(MAKE_MARKER ('F', 'O', 'R', 'M'))
#define AIFF_MARKER		(MAKE_MARKER ('A', 'I', 'F', 'F'))
#define AIFC_MARKER		(MAKE_MARKER ('A', 'I', 'F', 'C'))
#define COMM_MARKER		(MAKE_MARKER ('C', 'O', 'M', 'M'))
#define SSND_MARKER		(MAKE_MARKER ('S', 'S', 'N', 'D'))
#define MARK_MARKER		(MAKE_MARKER ('M', 'A', 'R', 'K'))
#define INST_MARKER		(MAKE_MARKER ('I', 'N', 'S', 'T'))
#define APPL_MARKER		(MAKE_MARKER ('A', 'P', 'P', 'L'))

#define c_MARKER		(MAKE_MARKER ('(', 'c', ')', ' '))
#define NAME_MARKER		(MAKE_MARKER ('N', 'A', 'M', 'E'))
#define AUTH_MARKER		(MAKE_MARKER ('A', 'U', 'T', 'H'))
#define ANNO_MARKER		(MAKE_MARKER ('A', 'N', 'N', 'O'))
#define COMT_MARKER		(MAKE_MARKER ('C', 'O', 'M', 'T'))
#define FVER_MARKER		(MAKE_MARKER ('F', 'V', 'E', 'R'))
#define SFX_MARKER		(MAKE_MARKER ('S', 'F', 'X', '!'))

#define PEAK_MARKER		(MAKE_MARKER ('P', 'E', 'A', 'K'))
#define basc_MARKER		(MAKE_MARKER ('b', 'a', 's', 'c'))

/* Supported AIFC encodings.*/
#define NONE_MARKER		(MAKE_MARKER ('N', 'O', 'N', 'E'))
#define sowt_MARKER		(MAKE_MARKER ('s', 'o', 'w', 't'))
#define twos_MARKER		(MAKE_MARKER ('t', 'w', 'o', 's'))
#define raw_MARKER		(MAKE_MARKER ('r', 'a', 'w', ' '))
#define in24_MARKER		(MAKE_MARKER ('i', 'n', '2', '4'))
#define ni24_MARKER		(MAKE_MARKER ('4', '2', 'n', '1'))
#define in32_MARKER		(MAKE_MARKER ('i', 'n', '3', '2'))
#define ni32_MARKER		(MAKE_MARKER ('2', '3', 'n', 'i'))

#define fl32_MARKER		(MAKE_MARKER ('f', 'l', '3', '2'))
#define FL32_MARKER		(MAKE_MARKER ('F', 'L', '3', '2'))
#define fl64_MARKER		(MAKE_MARKER ('f', 'l', '6', '4'))
#define FL64_MARKER		(MAKE_MARKER ('F', 'L', '6', '4'))

#define ulaw_MARKER		(MAKE_MARKER ('u', 'l', 'a', 'w'))
#define ULAW_MARKER		(MAKE_MARKER ('U', 'L', 'A', 'W'))
#define alaw_MARKER		(MAKE_MARKER ('a', 'l', 'a', 'w'))
#define ALAW_MARKER		(MAKE_MARKER ('A', 'L', 'A', 'W'))

#define DWVW_MARKER		(MAKE_MARKER ('D', 'W', 'V', 'W'))
#define GSM_MARKER		(MAKE_MARKER ('G', 'S', 'M', ' '))
#define ima4_MARKER		(MAKE_MARKER ('i', 'm', 'a', '4'))

/*
**	This value is officially assigned to Mega Nerd Pty Ltd by Apple
**	Corportation as the Application marker for libsndfile.
**
**	See : http://developer.apple.com/faq/datatype.html
*/
#define m3ga_MARKER		(MAKE_MARKER ('m', '3', 'g', 'a'))

/* Unsupported AIFC encodings.*/

#define MAC3_MARKER		(MAKE_MARKER ('M', 'A', 'C', '3'))
#define MAC6_MARKER		(MAKE_MARKER ('M', 'A', 'C', '6'))
#define ADP4_MARKER		(MAKE_MARKER ('A', 'D', 'P', '4'))

/* Predfined chunk sizes. */
#define SIZEOF_AIFF_COMM		18
#define SIZEOF_AIFC_COMM_MIN	22
#define SIZEOF_AIFC_COMM		24
#define SIZEOF_SSND_CHUNK		8
#define SIZEOF_INST_CHUNK		20

/* Is it constant? */

/* AIFC/IMA4 defines. */
#define AIFC_IMA4_BLOCK_LEN				34
#define AIFC_IMA4_SAMPLES_PER_BLOCK		64

#define AIFF_PEAK_CHUNK_SIZE(ch)	(2 * sizeof (int) + ch * (sizeof (float) + sizeof (int)))

/*------------------------------------------------------------------------------
 * Typedefs for file chunks.
 */

enum
{	HAVE_FORM		= 0x01,
	HAVE_AIFF		= 0x02,
	HAVE_AIFC		= 0x04,
	HAVE_FVER		= 0x08,
	HAVE_COMM		= 0x10,
	HAVE_SSND		= 0x20
} ;

typedef struct
{	unsigned int	size ;
	short			numChannels ;
	unsigned int	numSampleFrames ;
	short			sampleSize ;
	unsigned char	sampleRate [10] ;
	unsigned int	encoding ;
	char			zero_bytes [2] ;
} COMM_CHUNK ;

typedef struct
{	unsigned int	offset ;
	unsigned int	blocksize ;
} SSND_CHUNK ;

typedef struct
{	short			playMode ;
	unsigned short	beginLoop ;
	unsigned short	endLoop ;
} INST_LOOP ;

typedef struct
{	char		baseNote ;		/* all notes are MIDI note numbers */
	char		detune ;		/* cents off, only -50 to +50 are significant */
	char		lowNote ;
	char		highNote ;
	char		lowVelocity ;	/* 1 to 127 */
	char		highVelocity ;	/* 1 to 127 */
	short		gain ;			/* in dB, 0 is normal */
	INST_LOOP	sustain_loop ;
	INST_LOOP	release_loop ;
} INST_CHUNK ;


enum
{	basc_SCALE_MINOR = 1,
	basc_SCALE_MAJOR,
	basc_SCALE_NEITHER,
	basc_SCALE_BOTH
} ;

enum
{	basc_TYPE_LOOP = 0,
	basc_TYPE_ONE_SHOT
} ;


typedef struct
{	unsigned int	version ;
	unsigned int	numBeats ;
	unsigned short	rootNote ;
	unsigned short	scaleType ;
	unsigned short	sigNumerator ;
	unsigned short	sigDenominator ;
	unsigned short	loopType ;
} basc_CHUNK ;

typedef struct
{	unsigned short	markerID ;
	unsigned int	position ;
} MARK_ID_POS ;

typedef struct
{	PRIV_CHUNK4 chunk4 ;

	sf_count_t	comm_offset ;
	sf_count_t	ssnd_offset ;
} AIFF_PRIVATE ;

/*------------------------------------------------------------------------------
 * Private static functions.
 */

static int	aiff_close (SF_PRIVATE *psf) ;

static int	tenbytefloat2int (unsigned char *bytes) ;
static void uint2tenbytefloat (unsigned int num, unsigned char *bytes) ;

static int	aiff_read_comm_chunk (SF_PRIVATE *psf, COMM_CHUNK *comm_fmt) ;

static int	aiff_read_header (SF_PRIVATE *psf, COMM_CHUNK *comm_fmt) ;

static int	aiff_write_header (SF_PRIVATE *psf, int calc_length) ;
static int	aiff_write_tailer (SF_PRIVATE *psf) ;
static void	aiff_write_strings (SF_PRIVATE *psf, int location) ;

static int	aiff_command (SF_PRIVATE *psf, int command, void *data, int datasize) ;

static const char *get_loop_mode_str (short mode) ;

static short get_loop_mode (short mode) ;

static int aiff_read_basc_chunk (SF_PRIVATE * psf, int) ;

static unsigned int marker_to_position (const MARK_ID_POS *m, unsigned short n, int marksize) ;

/*------------------------------------------------------------------------------
** Public function.
*/

int
aiff_open (SF_PRIVATE *psf)
{	COMM_CHUNK comm_fmt ;
	int error, subformat ;

	memset (&comm_fmt, 0, sizeof (comm_fmt)) ;

	subformat = SF_CODEC (psf->sf.format) ;

	if ((psf->container_data = calloc (1, sizeof (AIFF_PRIVATE))) == NULL)
		return SFE_MALLOC_FAILED ;

	if (psf->mode == SFM_READ || (psf->mode == SFM_RDWR && psf->filelength > 0))
	{	if ((error = aiff_read_header (psf, &comm_fmt)))
			return error ;
		psf_fseek (psf, psf->dataoffset, SEEK_SET) ;
		} ;

	if (psf->mode == SFM_WRITE || psf->mode == SFM_RDWR)
	{	if (psf->is_pipe)
			return SFE_NO_PIPE_WRITE ;

		if ((SF_CONTAINER (psf->sf.format)) != SF_FORMAT_AIFF)
			return SFE_BAD_OPEN_FORMAT ;

		if (psf->mode == SFM_WRITE && (subformat == SF_FORMAT_FLOAT || subformat == SF_FORMAT_DOUBLE))
		{	if ((psf->peak_info = peak_info_calloc (psf->sf.channels)) == NULL)
				return SFE_MALLOC_FAILED ;
			psf->peak_info->peak_loc = SF_PEAK_START ;
			} ;

		if (psf->mode != SFM_RDWR || psf->filelength < 40)
		{	psf->filelength = 0 ;
			psf->datalength = 0 ;
			psf->dataoffset = 0 ;
			psf->sf.frames = 0 ;
			} ;

		psf->str_flags = SF_STR_ALLOW_START | SF_STR_ALLOW_END ;

		if ((error = aiff_write_header (psf, SF_FALSE)))
			return error ;

		psf->write_header = aiff_write_header ;
		} ;

	psf->container_close = aiff_close ;
	psf->command = aiff_command ;

	switch (SF_CODEC (psf->sf.format))
	{	case SF_FORMAT_PCM_U8 :
				error = pcm_init (psf) ;
				break ;

		case SF_FORMAT_PCM_S8 :
				error = pcm_init (psf) ;
				break ;

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

		case SF_FORMAT_DWVW_12 :
				error = dwvw_init (psf, 12) ;
				break ;

		case SF_FORMAT_DWVW_16 :
				error = dwvw_init (psf, 16) ;
				break ;

		case SF_FORMAT_DWVW_24 :
				error = dwvw_init (psf, 24) ;
				break ;

		case SF_FORMAT_DWVW_N :
				if (psf->mode != SFM_READ)
				{	error = SFE_DWVW_BAD_BITWIDTH ;
					break ;
					} ;
				if (comm_fmt.sampleSize >= 8 && comm_fmt.sampleSize < 24)
				{	error = dwvw_init (psf, comm_fmt.sampleSize) ;
					psf->sf.frames = comm_fmt.numSampleFrames ;
					break ;
					} ;
				psf_log_printf (psf, "AIFC/DWVW : Bad bitwidth %d\n", comm_fmt.sampleSize) ;
				error = SFE_DWVW_BAD_BITWIDTH ;
				break ;

		case SF_FORMAT_IMA_ADPCM :
				/*
				**	IMA ADPCM encoded AIFF files always have a block length
				**	of 34 which decodes to 64 samples.
				*/
				error = aiff_ima_init (psf, AIFC_IMA4_BLOCK_LEN, AIFC_IMA4_SAMPLES_PER_BLOCK) ;
				break ;
		/* Lite remove end */

		case SF_FORMAT_GSM610 :
				error = gsm610_init (psf) ;
				break ;

		default : return SFE_UNIMPLEMENTED ;
		} ;


	return error ;
} /* aiff_open */

/*==========================================================================================
** Private functions.
*/

/* This function ought to check size */
static unsigned int
marker_to_position (const MARK_ID_POS *m, unsigned short n, int marksize)
{	int i ;

    for (i = 0 ; i < marksize ; i++)
		if (m [i].markerID == n)
			return m [i].position ;
    return 0 ;
} /* marker_to_position */

static int
aiff_read_header (SF_PRIVATE *psf, COMM_CHUNK *comm_fmt)
{	SSND_CHUNK	ssnd_fmt ;
	MARK_ID_POS *markstr = NULL ;
	AIFF_PRIVATE *paiff ;
	unsigned	marker, dword, FORMsize, SSNDsize, bytesread ;
	int			k, found_chunk = 0, done = 0, error = 0 ;
	char		*cptr, byte ;
	int			instr_found = 0, mark_found = 0, mark_count = 0 ;

	if (psf->filelength > SF_PLATFORM_S64 (0xffffffff))
		psf_log_printf (psf, "Warning : filelength > 0xffffffff. This is bad!!!!\n") ;

	if ((paiff = psf->container_data) == NULL)
		return SFE_INTERNAL ;

	paiff->comm_offset = 0 ;
	paiff->ssnd_offset = 0 ;

	/* Set position to start of file to begin reading header. */
	psf_binheader_readf (psf, "p", 0) ;

	memset (comm_fmt, 0, sizeof (COMM_CHUNK)) ;

	/* Until recently AIF* file were all BIG endian. */
	psf->endian = SF_ENDIAN_BIG ;

	/*	AIFF files can apparently have their chunks in any order. However, they
	**	must have a FORM chunk. Approach here is to read all the chunks one by
	**	one and then check for the mandatory chunks at the end.
	*/
	while (! done)
	{	psf_binheader_readf (psf, "m", &marker) ;

		if (psf->mode == SFM_RDWR && (found_chunk & HAVE_SSND))
			return SFE_AIFF_RW_SSND_NOT_LAST ;

		switch (marker)
		{	case FORM_MARKER :
					if (found_chunk)
						return SFE_AIFF_NO_FORM ;

					psf_binheader_readf (psf, "E4", &FORMsize) ;
					pchk4_store (&(paiff->chunk4), marker, psf->headindex - 8, FORMsize) ;

					if (psf->fileoffset > 0 && psf->filelength > FORMsize + 8)
					{	/* Set file length. */
						psf->filelength = FORMsize + 8 ;
						psf_log_printf (psf, "FORM : %u\n", FORMsize) ;
						}
					else if (FORMsize != psf->filelength - 2 * SIGNED_SIZEOF (dword))
					{	dword = psf->filelength - 2 * sizeof (dword) ;
						psf_log_printf (psf, "FORM : %u (should be %u)\n", FORMsize, dword) ;
						FORMsize = dword ;
						}
					else
						psf_log_printf (psf, "FORM : %u\n", FORMsize) ;
					found_chunk |= HAVE_FORM ;
					break ;

			case AIFC_MARKER :
			case AIFF_MARKER :
					if ((found_chunk & HAVE_FORM) == 0)
						return SFE_AIFF_AIFF_NO_FORM ;
					psf_log_printf (psf, " %M\n", marker) ;
					found_chunk |= (marker == AIFC_MARKER) ? (HAVE_AIFC | HAVE_AIFF) : HAVE_AIFF ;
					break ;

			case COMM_MARKER :
					paiff->comm_offset = psf_ftell (psf) - 4 ;
					error = aiff_read_comm_chunk (psf, comm_fmt) ;
					pchk4_store (&paiff->chunk4, marker, paiff->comm_offset, comm_fmt->size) ;


					psf->sf.samplerate = tenbytefloat2int (comm_fmt->sampleRate) ;
					psf->sf.frames = comm_fmt->numSampleFrames ;
					psf->sf.channels = comm_fmt->numChannels ;
					psf->bytewidth = BITWIDTH2BYTES (comm_fmt->sampleSize) ;

					if (error)
						return error ;

					found_chunk |= HAVE_COMM ;
					break ;

			case PEAK_MARKER :
					/* Must have COMM chunk before PEAK chunk. */
					if ((found_chunk & (HAVE_FORM | HAVE_AIFF | HAVE_COMM)) != (HAVE_FORM | HAVE_AIFF | HAVE_COMM))
						return SFE_AIFF_PEAK_B4_COMM ;

					psf_binheader_readf (psf, "E4", &dword) ;
					pchk4_store (&paiff->chunk4, marker, psf->headindex - 8, dword) ;

					psf_log_printf (psf, "%M : %d\n", marker, dword) ;
					if (dword != AIFF_PEAK_CHUNK_SIZE (psf->sf.channels))
					{	psf_binheader_readf (psf, "j", dword) ;
						psf_log_printf (psf, "*** File PEAK chunk too big.\n") ;
						return SFE_WAV_BAD_PEAK ;
						} ;

					if ((psf->peak_info = peak_info_calloc (psf->sf.channels)) == NULL)
						return SFE_MALLOC_FAILED ;

					/* read in rest of PEAK chunk. */
					psf_binheader_readf (psf, "E44", &(psf->peak_info->version), &(psf->peak_info->timestamp)) ;

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

						psf_binheader_readf (psf, "Ef4", &value, &position) ;
						psf->peak_info->peaks [dword].value = value ;
						psf->peak_info->peaks [dword].position = position ;

						LSF_SNPRINTF (cptr, sizeof (psf->u.scbuf), "    %2d   %-12ld   %g\n",
								dword, (long) psf->peak_info->peaks [dword].position, psf->peak_info->peaks [dword].value) ;
						cptr [sizeof (psf->u.scbuf) - 1] = 0 ;
						psf_log_printf (psf, cptr) ;
						} ;

					psf->peak_info->peak_loc = ((found_chunk & HAVE_SSND) == 0) ? SF_PEAK_START : SF_PEAK_END ;
					break ;

			case SSND_MARKER :
					if ((found_chunk & HAVE_AIFC) && (found_chunk & HAVE_FVER) == 0)
						psf_log_printf (psf, "*** Valid AIFC files should have an FVER chunk.\n") ;

					paiff->ssnd_offset = psf_ftell (psf) - 4 ;
					psf_binheader_readf (psf, "E444", &SSNDsize, &(ssnd_fmt.offset), &(ssnd_fmt.blocksize)) ;
					pchk4_store (&paiff->chunk4, marker, paiff->ssnd_offset, SSNDsize) ;

					psf->datalength = SSNDsize - sizeof (ssnd_fmt) ;
					psf->dataoffset = psf_ftell (psf) ;

					if (psf->datalength > psf->filelength - psf->dataoffset || psf->datalength < 0)
					{	psf_log_printf (psf, " SSND : %u (should be %D)\n", SSNDsize, psf->filelength - psf->dataoffset + sizeof (SSND_CHUNK)) ;
						psf->datalength = psf->filelength - psf->dataoffset ;
						}
					else
						psf_log_printf (psf, " SSND : %u\n", SSNDsize) ;

					if (ssnd_fmt.offset == 0 || psf->dataoffset + ssnd_fmt.offset == ssnd_fmt.blocksize)
					{	psf_log_printf (psf, "  Offset     : %u\n", ssnd_fmt.offset) ;
						psf_log_printf (psf, "  Block Size : %u\n", ssnd_fmt.blocksize) ;

						psf->dataoffset += ssnd_fmt.offset ;
						psf->datalength -= ssnd_fmt.offset ;
						}
					else
					{	psf_log_printf (psf, "  Offset     : %u (Should be zero)\n", ssnd_fmt.offset) ;
						psf_log_printf (psf, "  Block Size : %u ???\n", ssnd_fmt.blocksize) ;
						} ;

					/* Only set dataend if there really is data at the end. */
					if (psf->datalength + psf->dataoffset < psf->filelength)
						psf->dataend = psf->datalength + psf->dataoffset ;

					found_chunk |= HAVE_SSND ;

					if (! psf->sf.seekable)
						break ;

					/* Seek to end of SSND chunk. */
					psf_fseek (psf, psf->dataoffset + psf->datalength + (SSNDsize & 1), SEEK_SET) ;
					break ;

			case c_MARKER :
					psf_binheader_readf (psf, "E4", &dword) ;
					pchk4_store (&paiff->chunk4, marker, psf_ftell (psf) - 8, dword) ;
					if (dword == 0)
						break ;
					if (dword > SIGNED_SIZEOF (psf->u.scbuf) - 1)
					{	psf_log_printf (psf, " %M : %d (too big)\n", marker, dword) ;
						return SFE_INTERNAL ;
						} ;

					cptr = psf->u.cbuf ;
					psf_binheader_readf (psf, "b", cptr, dword + (dword & 1)) ;
					if (dword > SIGNED_SIZEOF (psf->u.cbuf))
						cptr [sizeof (psf->u.cbuf) - 1] = 0 ;
					else
						cptr [dword > 0 ? dword : 0] = 0 ;

					psf_sanitize_string (cptr, dword) ;

					psf_log_printf (psf, " %M : %s\n", marker, cptr) ;
					psf_store_string (psf, SF_STR_COPYRIGHT, cptr) ;
					break ;

			case AUTH_MARKER :
					psf_binheader_readf (psf, "E4", &dword) ;
					pchk4_store (&paiff->chunk4, marker, psf_ftell (psf) - 8, dword) ;
					if (dword == 0)
						break ;
					if (dword > SIGNED_SIZEOF (psf->u.scbuf) - 1)
					{	psf_log_printf (psf, " %M : %d (too big)\n", marker, dword) ;
						return SFE_INTERNAL ;
						} ;

					cptr = psf->u.cbuf ;
					psf_binheader_readf (psf, "b", cptr, dword + (dword & 1)) ;
					cptr [dword > 0 ? dword : 0] = 0 ;
					psf_log_printf (psf, " %M : %s\n", marker, cptr) ;
					psf_store_string (psf, SF_STR_ARTIST, cptr) ;
					break ;

			case COMT_MARKER :
				{	unsigned short count, id, len ;
					unsigned int timestamp ;

					psf_binheader_readf (psf, "E42", &dword, &count) ;
					pchk4_store (&paiff->chunk4, marker, psf_ftell (psf) - 8, dword) ;
					psf_log_printf (psf, " %M : %d\n  count  : %d\n", marker, dword, count) ;
					dword += (dword & 1) ;
					if (dword == 0)
						break ;
					dword -= 2 ;

					for (k = 0 ; k < count ; k++)
					{	dword -= psf_binheader_readf (psf, "E422", &timestamp, &id, &len) ;
						psf_log_printf (psf, "   time   : 0x%x\n   marker : %x\n   length : %d\n", timestamp, id, len) ;

						if (len + 1 > SIGNED_SIZEOF (psf->u.scbuf))
						{	psf_log_printf (psf, "\nError : string length (%d) too big.\n", len) ;
							return SFE_INTERNAL ;
							} ;

						cptr = psf->u.cbuf ;
						dword -= psf_binheader_readf (psf, "b", cptr, len) ;
						cptr [len] = 0 ;
						psf_log_printf (psf, "   string : %s\n", cptr) ;
						} ;

					if (dword > 0)
						psf_binheader_readf (psf, "j", dword) ;
					} ;
					break ;

			case APPL_MARKER :
				{	unsigned appl_marker ;

					psf_binheader_readf (psf, "E4", &dword) ;
					pchk4_store (&paiff->chunk4, marker, psf_ftell (psf) - 8, dword) ;
					if (dword == 0)
						break ;
					if (dword >= SIGNED_SIZEOF (psf->u.scbuf) - 1)
					{	psf_log_printf (psf, " %M : %d (too big, skipping)\n", marker, dword) ;
						psf_binheader_readf (psf, "j", dword + (dword & 1)) ;
						break ;
						} ;

					if (dword < 4)
					{	psf_log_printf (psf, " %M : %d (too small, skipping)\n", marker, dword) ;
						psf_binheader_readf (psf, "j", dword + (dword & 1)) ;
						break ;
						} ;

					cptr = psf->u.cbuf ;
					psf_binheader_readf (psf, "mb", &appl_marker, cptr, dword + (dword & 1) - 4) ;
					cptr [dword > 0 ? dword : 0] = 0 ;

					for (k = 0 ; k < (int) dword ; k++)
						if (! isprint (cptr [k]))
						{	cptr [k] = 0 ;
							break ;
							} ;

					psf_log_printf (psf, " %M : %d\n  AppSig : %M\n  Name   : %s\n", marker, dword, appl_marker, cptr) ;
					psf_store_string (psf, SF_STR_SOFTWARE, cptr) ;
					} ;
					break ;

			case NAME_MARKER :
					psf_binheader_readf (psf, "E4", &dword) ;
					pchk4_store (&paiff->chunk4, marker, psf_ftell (psf) - 8, dword) ;
					if (dword == 0)
						break ;
					if (dword > SIGNED_SIZEOF (psf->u.scbuf) - 2)
					{	psf_log_printf (psf, " %M : %d (too big)\n", marker, dword) ;
						return SFE_INTERNAL ;
						} ;

					cptr = psf->u.cbuf ;
					psf_binheader_readf (psf, "b", cptr, dword + (dword & 1)) ;
					cptr [dword > 0 ? dword : 0] = 0 ;
					psf_log_printf (psf, " %M : %s\n", marker, cptr) ;
					psf_store_string (psf, SF_STR_TITLE, cptr) ;
					break ;

			case ANNO_MARKER :
					psf_binheader_readf (psf, "E4", &dword) ;
					pchk4_store (&paiff->chunk4, marker, psf_ftell (psf) - 8, dword) ;
					if (dword == 0)
						break ;
					if (dword > SIGNED_SIZEOF (psf->u.scbuf) - 2)
					{	psf_log_printf (psf, " %M : %d (too big)\n", marker, dword) ;
						return SFE_INTERNAL ;
						} ;

					cptr = psf->u.cbuf ;
					psf_binheader_readf (psf, "b", cptr, dword + (dword & 1)) ;
					cptr [dword > 0 ? dword : 0] = 0 ;
					psf_log_printf (psf, " %M : %s\n", marker, cptr) ;
					psf_store_string (psf, SF_STR_COMMENT, cptr) ;
					break ;

			case INST_MARKER :
					psf_binheader_readf (psf, "E4", &dword) ;
					pchk4_store (&paiff->chunk4, marker, psf_ftell (psf) - 8, dword) ;
					if (dword != SIZEOF_INST_CHUNK)
					{	psf_log_printf (psf, " %M : %d (should be %d)\n", marker, dword, SIZEOF_INST_CHUNK) ;
						psf_binheader_readf (psf, "j", dword) ;
						break ;
						} ;
					psf_log_printf (psf, " %M : %d\n", marker, dword) ;
					{	unsigned char bytes [6] ;
						short gain ;

						if (psf->instrument == NULL && (psf->instrument = psf_instrument_alloc ()) == NULL)
							return SFE_MALLOC_FAILED ;

						psf_binheader_readf (psf, "b", bytes, 6) ;
						psf_log_printf (psf, "  Base Note : %u\n  Detune    : %u\n"
											"  Low  Note : %u\n  High Note : %u\n"
											"  Low  Vel. : %u\n  High Vel. : %u\n",
											bytes [0], bytes [1], bytes [2], bytes [3], bytes [4], bytes [5]) ;
						psf->instrument->basenote = bytes [0] ;
						psf->instrument->detune = bytes [1] ;
						psf->instrument->key_lo = bytes [2] ;
						psf->instrument->key_hi = bytes [3] ;
						psf->instrument->velocity_lo = bytes [4] ;
						psf->instrument->velocity_hi = bytes [5] ;
						psf_binheader_readf (psf, "E2", &gain) ;
						psf->instrument->gain = gain ;
						psf_log_printf (psf, "  Gain (dB) : %d\n", gain) ;
						} ;
					{	short	mode ; /* 0 - no loop, 1 - forward looping, 2 - backward looping */
						const char	*loop_mode ;
						unsigned short begin, end ;

						psf_binheader_readf (psf, "E222", &mode, &begin, &end) ;
						loop_mode = get_loop_mode_str (mode) ;
						mode = get_loop_mode (mode) ;
						if (mode == SF_LOOP_NONE)
						{	psf->instrument->loop_count = 0 ;
							psf->instrument->loops [0].mode = SF_LOOP_NONE ;
							}
						else
						{	psf->instrument->loop_count = 1 ;
							psf->instrument->loops [0].mode = SF_LOOP_FORWARD ;
							psf->instrument->loops [0].start = begin ;
							psf->instrument->loops [0].end = end ;
							psf->instrument->loops [0].count = 0 ;
							} ;
						psf_log_printf (psf, "  Sustain\n   mode  : %d => %s\n   begin : %u\n   end   : %u\n",
											mode, loop_mode, begin, end) ;
						psf_binheader_readf (psf, "E222", &mode, &begin, &end) ;
						loop_mode = get_loop_mode_str (mode) ;
						mode = get_loop_mode (mode) ;
						if (mode == SF_LOOP_NONE)
							psf->instrument->loops [1].mode = SF_LOOP_NONE ;
						else
						{	psf->instrument->loop_count += 1 ;
							psf->instrument->loops [1].mode = SF_LOOP_FORWARD ;
							psf->instrument->loops [1].start = begin ;
							psf->instrument->loops [1].end = end ;
							psf->instrument->loops [1].count = 0 ;
							} ;
						psf_log_printf (psf, "  Release\n   mode  : %d => %s\n   begin : %u\n   end   : %u\n",
										mode, loop_mode, begin, end) ;
						} ;
					instr_found++ ;
					break ;

			case basc_MARKER :
					psf_binheader_readf (psf, "E4", &dword) ;
					pchk4_store (&paiff->chunk4, marker, psf_ftell (psf) - 8, dword) ;
					psf_log_printf (psf, " basc : %u\n", dword) ;

					if ((error = aiff_read_basc_chunk (psf, dword)))
						return error ;
					break ;

			case MARK_MARKER :
					psf_binheader_readf (psf, "E4", &dword) ;
					pchk4_store (&paiff->chunk4, marker, psf_ftell (psf) - 8, dword) ;
					psf_log_printf (psf, " %M : %d\n", marker, dword) ;
					{	unsigned short mark_id, n = 0 ;
						unsigned char pstr_len ;
						unsigned int position ;

						bytesread = psf_binheader_readf (psf, "E2", &n) ;
						mark_count = n ;
						markstr = calloc (mark_count, sizeof (MARK_ID_POS)) ;
						psf_log_printf (psf, "  Count : %d\n", mark_count) ;

						for (n = 0 ; n < mark_count && bytesread < dword ; n++)
						{	bytesread += psf_binheader_readf (psf, "E241", &mark_id, &position, &pstr_len) ;
							psf_log_printf (psf, "   Mark ID  : %u\n   Position : %u\n", mark_id, position) ;

							pstr_len += (pstr_len & 1) ? 0 : 1 ;

							bytesread += psf_binheader_readf (psf, "b", psf->u.scbuf, pstr_len) ;
							psf->u.scbuf [pstr_len] = 0 ;
							psf_log_printf (psf, "   Name     : %s\n", psf->u.scbuf) ;

							markstr [n].markerID = mark_id ;
							markstr [n].position = position ;
							/*
							**	TODO if psf->u.scbuf is equal to
							**	either Beg_loop, Beg loop or beg loop and spam
							**	if (psf->instrument == NULL && (psf->instrument = psf_instrument_alloc ()) == NULL)
							**		return SFE_MALLOC_FAILED ;
							*/
							} ;
						} ;
					mark_found++ ;
					psf_binheader_readf (psf, "j", dword - bytesread) ;
					break ;

			case FVER_MARKER :
					found_chunk |= HAVE_FVER ;
					/* Fall through to next case. */

			case SFX_MARKER :
					psf_binheader_readf (psf, "E4", &dword) ;
					pchk4_store (&paiff->chunk4, marker, psf_ftell (psf) - 8, dword) ;
					psf_log_printf (psf, " %M : %d\n", marker, dword) ;

					psf_binheader_readf (psf, "j", dword) ;
					break ;

			case NONE_MARKER :
					/* Fix for broken AIFC files with incorrect COMM chunk length. */
					psf_binheader_readf (psf, "1", &byte) ;
					dword = byte ;
					psf_binheader_readf (psf, "j", dword) ;
					break ;

			default :
					if (isprint ((marker >> 24) & 0xFF) && isprint ((marker >> 16) & 0xFF)
						&& isprint ((marker >> 8) & 0xFF) && isprint (marker & 0xFF))
					{	psf_binheader_readf (psf, "E4", &dword) ;
						psf_log_printf (psf, " %M : %d (unknown marker)\n", marker, dword) ;

						psf_binheader_readf (psf, "j", dword) ;
						break ;
						} ;
					if ((dword = psf_ftell (psf)) & 0x03)
					{	psf_log_printf (psf, "  Unknown chunk marker %X at position %d. Resyncing.\n", marker, dword - 4) ;

						psf_binheader_readf (psf, "j", -3) ;
						break ;
						} ;
					psf_log_printf (psf, "*** Unknown chunk marker %X at position %D. Exiting parser.\n", marker, psf_ftell (psf)) ;
					done = 1 ;
					break ;
			} ;	/* switch (marker) */

		if ((! psf->sf.seekable) && (found_chunk & HAVE_SSND))
			break ;

		if (psf_ftell (psf) >= psf->filelength - (2 * SIGNED_SIZEOF (dword)))
			break ;
		} ; /* while (1) */

	if (instr_found && mark_found)
	{	int j ;

		for (j = 0 ; j<psf->instrument->loop_count ; j ++)
		{	if (j < ARRAY_LEN (psf->instrument->loops))
			{	psf->instrument->loops [j].start = marker_to_position (markstr, psf->instrument->loops [j].start, mark_count) ;
				psf->instrument->loops [j].end = marker_to_position (markstr, psf->instrument->loops [j].end, mark_count) ;
				psf->instrument->loops [j].mode = SF_LOOP_FORWARD ;
				} ;
  			} ;
		} ;

	if (markstr)
		free (markstr) ;

	if (! (found_chunk & HAVE_FORM))
		return SFE_AIFF_NO_FORM ;

	if (! (found_chunk & HAVE_AIFF))
		return SFE_AIFF_COMM_NO_FORM ;

	if (! (found_chunk & HAVE_COMM))
		return SFE_AIFF_SSND_NO_COMM ;

	if (! psf->dataoffset)
		return SFE_AIFF_NO_DATA ;

	return 0 ;
} /* aiff_read_header */

static int
aiff_close (SF_PRIVATE *psf)
{
	if (psf->mode == SFM_WRITE || psf->mode == SFM_RDWR)
	{	aiff_write_tailer (psf) ;
		aiff_write_header (psf, SF_TRUE) ;
		} ;

	return 0 ;
} /* aiff_close */

static int
aiff_read_comm_chunk (SF_PRIVATE *psf, COMM_CHUNK *comm_fmt)
{	int error = 0, bytesread, subformat ;

	psf->u.scbuf [0] = 0 ;

	bytesread = psf_binheader_readf (psf, "E4", &(comm_fmt->size)) ;

	/* The COMM chunk has an int aligned to an odd word boundary. Some
	** procesors are not able to deal with this (ie bus fault) so we have
	** to take special care.
	*/
	comm_fmt->size += comm_fmt->size & 1 ;

	bytesread +=
	psf_binheader_readf (psf, "E242b", &(comm_fmt->numChannels), &(comm_fmt->numSampleFrames),
			&(comm_fmt->sampleSize), &(comm_fmt->sampleRate), SIGNED_SIZEOF (comm_fmt->sampleRate)) ;

	if (comm_fmt->size > 0x10000 && (comm_fmt->size & 0xffff) == 0)
	{	psf_log_printf (psf, " COMM : %d (0x%x) *** should be ", comm_fmt->size, comm_fmt->size) ;
		comm_fmt->size = ENDSWAP_INT (comm_fmt->size) ;
		psf_log_printf (psf, "%d (0x%x)\n", comm_fmt->size, comm_fmt->size) ;
		}
	else
		psf_log_printf (psf, " COMM : %d\n", comm_fmt->size) ;

	if (comm_fmt->size == SIZEOF_AIFF_COMM)
		comm_fmt->encoding = NONE_MARKER ;
	else if (comm_fmt->size == SIZEOF_AIFC_COMM_MIN)
		bytesread += psf_binheader_readf (psf, "Em", &(comm_fmt->encoding)) ;
	else if (comm_fmt->size >= SIZEOF_AIFC_COMM)
	{	unsigned char encoding_len ;
		unsigned read_len ;

		bytesread += psf_binheader_readf (psf, "Em1", &(comm_fmt->encoding), &encoding_len) ;

		comm_fmt->size = SF_MIN (sizeof (psf->u.scbuf), make_size_t (comm_fmt->size)) ;
		memset (psf->u.scbuf, 0, comm_fmt->size) ;
		read_len = comm_fmt->size - SIZEOF_AIFC_COMM + 1 ;
		bytesread += psf_binheader_readf (psf, "b", psf->u.scbuf, read_len) ;
		psf->u.scbuf [read_len + 1] = 0 ;
		} ;

	psf_log_printf (psf, "  Sample Rate : %d\n", tenbytefloat2int (comm_fmt->sampleRate)) ;
	psf_log_printf (psf, "  Frames      : %u%s\n", comm_fmt->numSampleFrames, (comm_fmt->numSampleFrames == 0 && psf->filelength > 104) ? " (Should not be 0)" : "") ;
	psf_log_printf (psf, "  Channels    : %d\n", comm_fmt->numChannels) ;

	/* Found some broken 'fl32' files with comm.samplesize == 16. Fix it here. */
	if ((comm_fmt->encoding == fl32_MARKER || comm_fmt->encoding == FL32_MARKER) && comm_fmt->sampleSize != 32)
	{	psf_log_printf (psf, "  Sample Size : %d (should be 32)\n", comm_fmt->sampleSize) ;
		comm_fmt->sampleSize = 32 ;
		}
	else if ((comm_fmt->encoding == fl64_MARKER || comm_fmt->encoding == FL64_MARKER) && comm_fmt->sampleSize != 64)
	{	psf_log_printf (psf, "  Sample Size : %d (should be 64)\n", comm_fmt->sampleSize) ;
		comm_fmt->sampleSize = 64 ;
		}
	else
		psf_log_printf (psf, "  Sample Size : %d\n", comm_fmt->sampleSize) ;

	subformat = s_bitwidth_to_subformat (comm_fmt->sampleSize) ;

	psf->endian = SF_ENDIAN_BIG ;

	switch (comm_fmt->encoding)
	{	case NONE_MARKER :
				psf->sf.format = (SF_FORMAT_AIFF | subformat) ;
				break ;

		case twos_MARKER :
		case in24_MARKER :
		case in32_MARKER :
				psf->sf.format = (SF_ENDIAN_BIG | SF_FORMAT_AIFF | subformat) ;
				break ;

		case sowt_MARKER :
		case ni24_MARKER :
		case ni32_MARKER :
				psf->endian = SF_ENDIAN_LITTLE ;
				psf->sf.format = (SF_ENDIAN_LITTLE | SF_FORMAT_AIFF | subformat) ;
				break ;

		case fl32_MARKER :
		case FL32_MARKER :
				psf->sf.format = (SF_FORMAT_AIFF | SF_FORMAT_FLOAT) ;
				break ;

		case ulaw_MARKER :
		case ULAW_MARKER :
				psf->sf.format = (SF_FORMAT_AIFF | SF_FORMAT_ULAW) ;
				break ;

		case alaw_MARKER :
		case ALAW_MARKER :
				psf->sf.format = (SF_FORMAT_AIFF | SF_FORMAT_ALAW) ;
				break ;

		case fl64_MARKER :
		case FL64_MARKER :
				psf->sf.format = (SF_FORMAT_AIFF | SF_FORMAT_DOUBLE) ;
				break ;

		case raw_MARKER :
				psf->sf.format = (SF_FORMAT_AIFF | SF_FORMAT_PCM_U8) ;
				break ;

		case DWVW_MARKER :
				psf->sf.format = SF_FORMAT_AIFF ;
				switch (comm_fmt->sampleSize)
				{	case 12 :
						psf->sf.format |= SF_FORMAT_DWVW_12 ;
						break ;
					case 16 :
						psf->sf.format |= SF_FORMAT_DWVW_16 ;
						break ;
					case 24 :
						psf->sf.format |= SF_FORMAT_DWVW_24 ;
						break ;

					default :
						psf->sf.format |= SF_FORMAT_DWVW_N ;
						break ;
					} ;
				break ;

		case GSM_MARKER :
				psf->sf.format = SF_FORMAT_AIFF ;
				psf->sf.format = (SF_FORMAT_AIFF | SF_FORMAT_GSM610) ;
				break ;


		case ima4_MARKER :
				psf->endian = SF_ENDIAN_BIG ;
				psf->sf.format = (SF_FORMAT_AIFF | SF_FORMAT_IMA_ADPCM) ;
				break ;

		default :
			psf_log_printf (psf, "AIFC : Unimplemented format : %M\n", comm_fmt->encoding) ;
			error = SFE_UNIMPLEMENTED ;
		} ;

	if (! psf->u.scbuf [0])
		psf_log_printf (psf, "  Encoding    : %M\n", comm_fmt->encoding) ;
	else
		psf_log_printf (psf, "  Encoding    : %M => %s\n", comm_fmt->encoding, psf->u.scbuf) ;

	return error ;
} /* aiff_read_comm_chunk */


/*==========================================================================================
*/

static int
aiff_rewrite_header (SF_PRIVATE *psf, AIFF_PRIVATE * paiff)
{
	/* Assuming here that the header has already been written and just
	** needs to be corrected for new data length. That means that we
	** only change the length fields of the FORM and SSND chunks ;
	** everything else can be skipped over.
	*/
	int k, ch ;
	int comm_size, comm_frames ;

	psf_fseek (psf, 0, SEEK_SET) ;
	psf_fread (psf->header, psf->dataoffset, 1, psf) ;

	psf->headindex = 0 ;

	for (k = 0 ; k < paiff->chunk4.count ; k++)
	{	switch (paiff->chunk4.l [k].chunk)
		{	case FORM_MARKER :
				psf_binheader_writef (psf, "Etm8", FORM_MARKER, psf->filelength - 8) ;
				break ;

			case COMM_MARKER :
				psf->headindex = paiff->chunk4.l [k].offset ;
				comm_frames = psf->sf.frames ;
				comm_size = paiff->chunk4.l [k].len ;
				psf_binheader_writef (psf, "Em42t4", COMM_MARKER, comm_size, psf->sf.channels, comm_frames) ;
				break ;

			case SSND_MARKER :
				psf->headindex = paiff->chunk4.l [k].offset ;
				psf_binheader_writef (psf, "Etm8", SSND_MARKER, psf->datalength + SIZEOF_SSND_CHUNK) ;
				break ;

			case PEAK_MARKER :
				psf->headindex = paiff->chunk4.l [k].offset ;
				psf_binheader_writef (psf, "Em4", PEAK_MARKER, AIFF_PEAK_CHUNK_SIZE (psf->sf.channels)) ;
				psf_binheader_writef (psf, "E44", 1, time (NULL)) ;
				for (ch = 0 ; ch < psf->sf.channels ; ch++)
					psf_binheader_writef (psf, "Eft8", (float) psf->peak_info->peaks [ch].value, psf->peak_info->peaks [ch].position) ;
				break ;

			default :
				/* There are a whole bunch of chunks we should just ignore. */
				break ;
			} ;
		} ;

	/* Header mangling complete so write it out. */
	psf_fseek (psf, 0, SEEK_SET) ;
	psf_fwrite (psf->header, psf->headindex, 1, psf) ;

	return 0 ;
} /* aiff_rewrite_header */

static int
aiff_write_header (SF_PRIVATE *psf, int calc_length)
{	sf_count_t		current ;
	AIFF_PRIVATE	*paiff ;
	unsigned char	comm_sample_rate [10], comm_zero_bytes [2] = { 0, 0 } ;
	unsigned int	comm_type, comm_size, comm_encoding, comm_frames = 0 ;
	int				k, endian, has_data = SF_FALSE ;
	short			bit_width ;

	if ((paiff = psf->container_data) == NULL)
		return SFE_INTERNAL ;

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

	if (psf->mode == SFM_RDWR && psf->dataoffset > 0 && paiff->chunk4.count > 0)
	{	int err = aiff_rewrite_header (psf, paiff) ;
		if (current > 0)
			psf_fseek (psf, current, SEEK_SET) ;
		return err ;
		} ;

	endian = SF_ENDIAN (psf->sf.format) ;
	if (CPU_IS_LITTLE_ENDIAN && endian == SF_ENDIAN_CPU)
		endian = SF_ENDIAN_LITTLE ;

	/* Standard value here. */
	bit_width = psf->bytewidth * 8 ;
	comm_frames = (psf->sf.frames > 0xFFFFFFFF) ? 0xFFFFFFFF : psf->sf.frames ;

	switch (SF_CODEC (psf->sf.format))
	{	case SF_FORMAT_PCM_S8 :
		case SF_FORMAT_PCM_16 :
		case SF_FORMAT_PCM_24 :
		case SF_FORMAT_PCM_32 :
				switch (endian)
				{	case SF_ENDIAN_BIG :
							psf->endian = SF_ENDIAN_BIG ;
							comm_type = AIFC_MARKER ;
							comm_size = SIZEOF_AIFC_COMM ;
							comm_encoding = twos_MARKER ;
							break ;

					case SF_ENDIAN_LITTLE :
							psf->endian = SF_ENDIAN_LITTLE ;
							comm_type = AIFC_MARKER ;
							comm_size = SIZEOF_AIFC_COMM ;
							comm_encoding = sowt_MARKER ;
							break ;

					default : /* SF_ENDIAN_FILE */
							psf->endian = SF_ENDIAN_BIG ;
							comm_type = AIFF_MARKER ;
							comm_size = SIZEOF_AIFF_COMM ;
							comm_encoding = 0 ;
							break ;
					} ;
				break ;

		case SF_FORMAT_FLOAT :					/* Big endian floating point. */
				psf->endian = SF_ENDIAN_BIG ;
				comm_type = AIFC_MARKER ;
				comm_size = SIZEOF_AIFC_COMM ;
				comm_encoding = FL32_MARKER ;	/* Use 'FL32' because its easier to read. */
				break ;

		case SF_FORMAT_DOUBLE :					/* Big endian double precision floating point. */
				psf->endian = SF_ENDIAN_BIG ;
				comm_type = AIFC_MARKER ;
				comm_size = SIZEOF_AIFC_COMM ;
				comm_encoding = FL64_MARKER ;	/* Use 'FL64' because its easier to read. */
				break ;

		case SF_FORMAT_ULAW :
				psf->endian = SF_ENDIAN_BIG ;
				comm_type = AIFC_MARKER ;
				comm_size = SIZEOF_AIFC_COMM ;
				comm_encoding = ulaw_MARKER ;
				break ;

		case SF_FORMAT_ALAW :
				psf->endian = SF_ENDIAN_BIG ;
				comm_type = AIFC_MARKER ;
				comm_size = SIZEOF_AIFC_COMM ;
				comm_encoding = alaw_MARKER ;
				break ;

		case SF_FORMAT_PCM_U8 :
				psf->endian = SF_ENDIAN_BIG ;
				comm_type = AIFC_MARKER ;
				comm_size = SIZEOF_AIFC_COMM ;
				comm_encoding = raw_MARKER ;
				break ;

		case SF_FORMAT_DWVW_12 :
				psf->endian = SF_ENDIAN_BIG ;
				comm_type = AIFC_MARKER ;
				comm_size = SIZEOF_AIFC_COMM ;
				comm_encoding = DWVW_MARKER ;

				/* Override standard value here.*/
				bit_width = 12 ;
				break ;

		case SF_FORMAT_DWVW_16 :
				psf->endian = SF_ENDIAN_BIG ;
				comm_type = AIFC_MARKER ;
				comm_size = SIZEOF_AIFC_COMM ;
				comm_encoding = DWVW_MARKER ;

				/* Override standard value here.*/
				bit_width = 16 ;
				break ;

		case SF_FORMAT_DWVW_24 :
				psf->endian = SF_ENDIAN_BIG ;
				comm_type = AIFC_MARKER ;
				comm_size = SIZEOF_AIFC_COMM ;
				comm_encoding = DWVW_MARKER ;

				/* Override standard value here.*/
				bit_width = 24 ;
				break ;

		case SF_FORMAT_GSM610 :
				psf->endian = SF_ENDIAN_BIG ;
				comm_type = AIFC_MARKER ;
				comm_size = SIZEOF_AIFC_COMM ;
				comm_encoding = GSM_MARKER ;

				/* Override standard value here.*/
				bit_width = 16 ;
				break ;

		case SF_FORMAT_IMA_ADPCM :
				psf->endian = SF_ENDIAN_BIG ;
				comm_type = AIFC_MARKER ;
				comm_size = SIZEOF_AIFC_COMM ;
				comm_encoding = ima4_MARKER ;

				/* Override standard value here.*/
				bit_width = 16 ;
				comm_frames = psf->sf.frames / AIFC_IMA4_SAMPLES_PER_BLOCK ;
				break ;

		default : return SFE_BAD_OPEN_FORMAT ;
		} ;

	/* Reset the current header length to zero. */
	psf->header [0] = 0 ;
	psf->headindex = 0 ;
	psf_fseek (psf, 0, SEEK_SET) ;

	psf_binheader_writef (psf, "Etm8", FORM_MARKER, psf->filelength - 8) ;

	/* Write AIFF/AIFC marker and COM chunk. */
	if (comm_type == AIFC_MARKER)
		/* AIFC must have an FVER chunk. */
		psf_binheader_writef (psf, "Emm44", comm_type, FVER_MARKER, 4, 0xA2805140) ;
	else
		psf_binheader_writef (psf, "Em", comm_type) ;

	paiff->comm_offset = psf->headindex - 8 ;

	memset (comm_sample_rate, 0, sizeof (comm_sample_rate)) ;
	uint2tenbytefloat (psf->sf.samplerate, comm_sample_rate) ;

	psf_binheader_writef (psf, "Em42t42", COMM_MARKER, comm_size, psf->sf.channels, comm_frames, bit_width) ;
	psf_binheader_writef (psf, "b", comm_sample_rate, sizeof (comm_sample_rate)) ;

	/* AIFC chunks have some extra data. */
	if (comm_type == AIFC_MARKER)
		psf_binheader_writef (psf, "mb", comm_encoding, comm_zero_bytes, sizeof (comm_zero_bytes)) ;

	if (psf->instrument != NULL)
	{	MARK_ID_POS	m [4] ;
		INST_CHUNK ch ;
		unsigned short ct = 0 ;

		memset (m, 0, sizeof (m)) ;
		memset (&ch, 0, sizeof (ch)) ;

		ch.baseNote = psf->instrument->basenote ;
		ch.detune = psf->instrument->detune ;
		ch.lowNote = psf->instrument->key_lo ;
		ch.highNote = psf->instrument->key_hi ;
		ch.lowVelocity = psf->instrument->velocity_lo ;
		ch.highVelocity = psf->instrument->velocity_hi ;
		ch.gain = psf->instrument->gain ;
		if (psf->instrument->loops [0].mode != SF_LOOP_NONE)
		{	ch.sustain_loop.playMode = 1 ;
			ch.sustain_loop.beginLoop = ct ;
			m [0].markerID = ct++ ;
			m [0].position = psf->instrument->loops [0].start ;
			ch.sustain_loop.endLoop = ct ;
			m [1].markerID = ct++ ;
			m [1].position = psf->instrument->loops [0].end ;
			} ;
		if (psf->instrument->loops [1].mode != SF_LOOP_NONE)
		{	ch.release_loop.playMode = 1 ;
			ch.release_loop.beginLoop = ct ;
			m [2].markerID = ct++ ;
			m [2].position = psf->instrument->loops [1].start ;
			ch.release_loop.endLoop = ct ;
			m [3].markerID = ct++ ;
			m [3].position = psf->instrument->loops [1].end ;
			}
		else
		{	ch.release_loop.playMode = 0 ;
			ch.release_loop.beginLoop = 0 ;
			ch.release_loop.endLoop = 0 ;
			} ;

		psf_binheader_writef (psf, "Em4111111", INST_MARKER, SIZEOF_INST_CHUNK, ch.baseNote, ch.detune,
						ch.lowNote, ch.highNote, ch.lowVelocity, ch.highVelocity) ;
		psf_binheader_writef (psf, "2222222", ch.gain, ch.sustain_loop.playMode,
						ch.sustain_loop.beginLoop, ch.sustain_loop.endLoop, ch.release_loop.playMode,
						ch.release_loop.beginLoop, ch.release_loop.endLoop) ;

		if (ct == 2)
			psf_binheader_writef (psf, "Em42241b241b",
					MARK_MARKER, 2 + 2 * (2 + 4 + 1 + 9), 2,
					m [0].markerID, m [0].position, 8, "beg loop", make_size_t (9),
					m [1].markerID, m [1].position, 8, "end loop", make_size_t (9)) ;
		else if (ct == 4)
			psf_binheader_writef (psf, "Em42 241b 241b 241b 241b",
					MARK_MARKER, 2 + 4 * (2 + 4 + 1 + 9), 4,
					m [0].markerID, m [0].position, 8, "beg loop", make_size_t (9),
					m [1].markerID, m [1].position, 8, "end loop", make_size_t (9),
					m [2].markerID, m [2].position, 8, "beg loop", make_size_t (9),
					m [3].markerID, m [3].position, 8, "end loop", make_size_t (9)) ;
		} ;

	if (psf->str_flags & SF_STR_LOCATE_START)
		aiff_write_strings (psf, SF_STR_LOCATE_START) ;

	if (psf->peak_info != NULL && psf->peak_info->peak_loc == SF_PEAK_START)
	{	psf_binheader_writef (psf, "Em4", PEAK_MARKER, AIFF_PEAK_CHUNK_SIZE (psf->sf.channels)) ;
		psf_binheader_writef (psf, "E44", 1, time (NULL)) ;
		for (k = 0 ; k < psf->sf.channels ; k++)
			psf_binheader_writef (psf, "Eft8", (float) psf->peak_info->peaks [k].value, psf->peak_info->peaks [k].position) ;
		} ;

	/* Write SSND chunk. */
	paiff->ssnd_offset = psf->headindex ;
	psf_binheader_writef (psf, "Etm844", SSND_MARKER, psf->datalength + SIZEOF_SSND_CHUNK, 0, 0) ;

	/* Header construction complete so write it out. */
	psf_fwrite (psf->header, psf->headindex, 1, psf) ;

	if (psf->error)
		return psf->error ;

	if (has_data && psf->dataoffset != psf->headindex)
		return psf->error = SFE_INTERNAL ;

	psf->dataoffset = psf->headindex ;

	if (! has_data)
		psf_fseek (psf, psf->dataoffset, SEEK_SET) ;
	else if (current > 0)
		psf_fseek (psf, current, SEEK_SET) ;

	return psf->error ;
} /* aiff_write_header */

static int
aiff_write_tailer (SF_PRIVATE *psf)
{	int		k ;

	/* Reset the current header length to zero. */
	psf->header [0] = 0 ;
	psf->headindex = 0 ;

	psf->dataend = psf_fseek (psf, 0, SEEK_END) ;

	/* Make sure tailer data starts at even byte offset. Pad if necessary. */
	if (psf->dataend % 2 == 1)
	{	psf_fwrite (psf->header, 1, 1, psf) ;
		psf->dataend ++ ;
		} ;

	if (psf->peak_info != NULL && psf->peak_info->peak_loc == SF_PEAK_END)
	{	psf_binheader_writef (psf, "Em4", PEAK_MARKER, AIFF_PEAK_CHUNK_SIZE (psf->sf.channels)) ;
		psf_binheader_writef (psf, "E44", 1, time (NULL)) ;
		for (k = 0 ; k < psf->sf.channels ; k++)
			psf_binheader_writef (psf, "Eft8", (float) psf->peak_info->peaks [k].value, psf->peak_info->peaks [k].position) ;
		} ;

	if (psf->str_flags & SF_STR_LOCATE_END)
		aiff_write_strings (psf, SF_STR_LOCATE_END) ;

	/* Write the tailer. */
	if (psf->headindex > 0)
		psf_fwrite (psf->header, psf->headindex, 1, psf) ;

	return 0 ;
} /* aiff_write_tailer */

static void
aiff_write_strings (SF_PRIVATE *psf, int location)
{	int	k, slen ;

	for (k = 0 ; k < SF_MAX_STRINGS ; k++)
	{	if (psf->strings [k].type == 0)
			break ;

		if (psf->strings [k].flags != location)
			continue ;

		switch (psf->strings [k].type)
		{	case SF_STR_SOFTWARE :
				slen = strlen (psf->strings [k].str) ;
				psf_binheader_writef (psf, "Em4mb", APPL_MARKER, slen + 4, m3ga_MARKER, psf->strings [k].str, make_size_t (slen + (slen & 1))) ;
				break ;

			case SF_STR_TITLE :
				psf_binheader_writef (psf, "EmS", NAME_MARKER, psf->strings [k].str) ;
				break ;

			case SF_STR_COPYRIGHT :
				psf_binheader_writef (psf, "EmS", c_MARKER, psf->strings [k].str) ;
				break ;

			case SF_STR_ARTIST :
				psf_binheader_writef (psf, "EmS", AUTH_MARKER, psf->strings [k].str) ;
				break ;

			case SF_STR_COMMENT :
				psf_binheader_writef (psf, "EmS", ANNO_MARKER, psf->strings [k].str) ;
				break ;

			/*
			case SF_STR_DATE :
				psf_binheader_writef (psf, "Ems", ICRD_MARKER, psf->strings [k].str) ;
				break ;
			*/
			} ;
		} ;

	return ;
} /* aiff_write_strings */

static int
aiff_command (SF_PRIVATE * UNUSED (psf), int UNUSED (command), void * UNUSED (data), int UNUSED (datasize))
{
	return 0 ;
} /* aiff_command */

static const char*
get_loop_mode_str (short mode)
{	switch (mode)
	{	case 0 : return "none" ;
		case 1 : return "forward" ;
		case 2 : return "backward" ;
		} ;

	return "*** unknown" ;
} /* get_loop_mode_str */

static short
get_loop_mode (short mode)
{	switch (mode)
	{	case 0 : return SF_LOOP_NONE ;
		case 1 : return SF_LOOP_FORWARD ;
		case 2 : return SF_LOOP_BACKWARD ;
		} ;

	return SF_LOOP_NONE ;
} /* get_loop_mode */

/*==========================================================================================
**	Rough hack at converting from 80 bit IEEE float in AIFF header to an int and
**	back again. It assumes that all sample rates are between 1 and 800MHz, which
**	should be OK as other sound file formats use a 32 bit integer to store sample
**	rate.
**	There is another (probably better) version in the source code to the SoX but it
**	has a copyright which probably prevents it from being allowable as GPL/LGPL.
*/

static int
tenbytefloat2int (unsigned char *bytes)
{	int val = 3 ;

	if (bytes [0] & 0x80)	/* Negative number. */
		return 0 ;

	if (bytes [0] <= 0x3F)	/* Less than 1. */
		return 1 ;

	if (bytes [0] > 0x40)	/* Way too big. */
		return 0x4000000 ;

	if (bytes [0] == 0x40 && bytes [1] > 0x1C) /* Too big. */
		return 800000000 ;

	/* Ok, can handle it. */

	val = (bytes [2] << 23) | (bytes [3] << 15) | (bytes [4] << 7) | (bytes [5] >> 1) ;

	val >>= (29 - bytes [1]) ;

	return val ;
} /* tenbytefloat2int */

static void
uint2tenbytefloat (unsigned int num, unsigned char *bytes)
{	unsigned int mask = 0x40000000 ;
	int	count ;

	if (num <= 1)
	{	bytes [0] = 0x3F ;
		bytes [1] = 0xFF ;
		bytes [2] = 0x80 ;
		return ;
		} ;

	bytes [0] = 0x40 ;

	if (num >= mask)
	{	bytes [1] = 0x1D ;
		return ;
		} ;

	for (count = 0 ; count <= 32 ; count ++)
	{	if (num & mask)
			break ;
		mask >>= 1 ;
		} ;

	num <<= count + 1 ;
	bytes [1] = 29 - count ;
	bytes [2] = (num >> 24) & 0xFF ;
	bytes [3] = (num >> 16) & 0xFF ;
	bytes [4] = (num >> 8) & 0xFF ;
	bytes [5] = num & 0xFF ;

} /* uint2tenbytefloat */

static int
aiff_read_basc_chunk (SF_PRIVATE * psf, int datasize)
{	const char * type_str ;
	basc_CHUNK bc ;
	int count ;

	count = psf_binheader_readf (psf, "E442", &bc.version, &bc.numBeats, &bc.rootNote) ;
	count += psf_binheader_readf (psf, "E222", &bc.scaleType, &bc.sigNumerator, &bc.sigDenominator) ;
	count += psf_binheader_readf (psf, "E2j", &bc.loopType, datasize - sizeof (bc)) ;

	psf_log_printf (psf, "  Version ? : %u\n  Num Beats : %u\n  Root Note : 0x%x\n",
						bc.version, bc.numBeats, bc.rootNote) ;

	switch (bc.scaleType)
	{	case basc_SCALE_MINOR :
				type_str = "MINOR" ;
				break ;
		case basc_SCALE_MAJOR :
				type_str = "MAJOR" ;
				break ;
		case basc_SCALE_NEITHER :
				type_str = "NEITHER" ;
				break ;
		case basc_SCALE_BOTH :
				type_str = "BOTH" ;
				break ;
		default :
				type_str = "!!WRONG!!" ;
				break ;
		} ;

	psf_log_printf (psf, "  ScaleType : 0x%x (%s)\n", bc.scaleType, type_str) ;
	psf_log_printf (psf, "  Time Sig  : %d/%d\n", bc.sigNumerator, bc.sigDenominator) ;

	switch (bc.loopType)
	{	case basc_TYPE_ONE_SHOT :
				type_str = "One Shot" ;
				break ;
		case basc_TYPE_LOOP :
				type_str = "Loop" ;
				break ;
		default:
				type_str = "!!WRONG!!" ;
				break ;
		} ;

	psf_log_printf (psf, "  Loop Type : 0x%x (%s)\n", bc.loopType, type_str) ;

	if ((psf->loop_info = calloc (1, sizeof (SF_LOOP_INFO))) == NULL)
		return SFE_MALLOC_FAILED ;

	psf->loop_info->time_sig_num	= bc.sigNumerator ;
	psf->loop_info->time_sig_den	= bc.sigDenominator ;
	psf->loop_info->loop_mode		= (bc.loopType == basc_TYPE_ONE_SHOT) ? SF_LOOP_NONE : SF_LOOP_FORWARD ;
	psf->loop_info->num_beats		= bc.numBeats ;

	/* Can always be recalculated from other known fields. */
	psf->loop_info->bpm = (1.0 / psf->sf.frames) * psf->sf.samplerate
							* ((bc.numBeats * 4.0) / bc.sigDenominator) * 60.0 ;
	psf->loop_info->root_key = bc.rootNote ;

	if (count < datasize)
		psf_binheader_readf (psf, "j", datasize - count) ;

	return 0 ;
} /* aiff_read_basc_chunk */

