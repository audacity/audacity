/*
** Copyright (C) 1999-2018 Erik de Castro Lopo <erikd@mega-nerd.com>
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
#include <inttypes.h>

#include "sndfile.h"
#include "sfendian.h"
#include "common.h"
#include "chanmap.h"

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
#define CHAN_MARKER		(MAKE_MARKER ('C', 'H', 'A', 'N'))

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
{	uint32_t	size ;
	int16_t		numChannels ;
	uint32_t	numSampleFrames ;
	int16_t		sampleSize ;
	uint8_t		sampleRate [10] ;
	uint32_t	encoding ;
	char			zero_bytes [2] ;
} COMM_CHUNK ;

typedef struct
{	uint32_t	offset ;
	uint32_t	blocksize ;
} SSND_CHUNK ;

typedef struct
{	int16_t		playMode ;
	uint16_t	beginLoop ;
	uint16_t	endLoop ;
} INST_LOOP ;

typedef struct
{	int8_t		baseNote ;		/* all notes are MIDI note numbers */
	int8_t		detune ;		/* cents off, only -50 to +50 are significant */
	int8_t		lowNote ;
	int8_t		highNote ;
	int8_t		lowVelocity ;	/* 1 to 127 */
	int8_t		highVelocity ;	/* 1 to 127 */
	int16_t		gain ;			/* in dB, 0 is normal */
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
{	uint32_t	version ;
	uint32_t	numBeats ;
	uint16_t	rootNote ;
	uint16_t	scaleType ;
	uint16_t	sigNumerator ;
	uint16_t	sigDenominator ;
	uint16_t	loopType ;
} basc_CHUNK ;

typedef struct
{	uint16_t	markerID ;
	uint32_t	position ;
} MARK_ID_POS ;

typedef struct
{	sf_count_t	comm_offset ;
	sf_count_t	ssnd_offset ;

	int32_t		chanmap_tag ;

	MARK_ID_POS *markstr ;
} AIFF_PRIVATE ;

/*------------------------------------------------------------------------------
 * Private static functions.
 */

static int	aiff_close (SF_PRIVATE *psf) ;

static int	tenbytefloat2int (uint8_t *bytes) ;
static void uint2tenbytefloat (uint32_t num, uint8_t *bytes) ;

static int	aiff_read_comm_chunk (SF_PRIVATE *psf, COMM_CHUNK *comm_fmt) ;

static int	aiff_read_header (SF_PRIVATE *psf, COMM_CHUNK *comm_fmt) ;

static int	aiff_write_header (SF_PRIVATE *psf, int calc_length) ;
static int	aiff_write_tailer (SF_PRIVATE *psf) ;
static void	aiff_write_strings (SF_PRIVATE *psf, int location) ;

static int	aiff_command (SF_PRIVATE *psf, int command, void *data, int datasize) ;

static const char *get_loop_mode_str (int16_t mode) ;

static int16_t get_loop_mode (int16_t mode) ;

static int aiff_read_basc_chunk (SF_PRIVATE * psf, int) ;

static int aiff_read_chanmap (SF_PRIVATE * psf, unsigned dword) ;

static uint32_t marker_to_position (const MARK_ID_POS *m, uint16_t n, int marksize) ;

static int aiff_set_chunk (SF_PRIVATE *psf, const SF_CHUNK_INFO * chunk_info) ;
static SF_CHUNK_ITERATOR * aiff_next_chunk_iterator (SF_PRIVATE *psf, SF_CHUNK_ITERATOR * iterator) ;
static int aiff_get_chunk_size (SF_PRIVATE *psf, const SF_CHUNK_ITERATOR * iterator, SF_CHUNK_INFO * chunk_info) ;
static int aiff_get_chunk_data (SF_PRIVATE *psf, const SF_CHUNK_ITERATOR * iterator, SF_CHUNK_INFO * chunk_info) ;

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

	if (psf->file.mode == SFM_READ || (psf->file.mode == SFM_RDWR && psf->filelength > 0))
	{	if ((error = aiff_read_header (psf, &comm_fmt)))
			return error ;

		psf->next_chunk_iterator = aiff_next_chunk_iterator ;
		psf->get_chunk_size = aiff_get_chunk_size ;
		psf->get_chunk_data = aiff_get_chunk_data ;

		psf_fseek (psf, psf->dataoffset, SEEK_SET) ;
		} ;

	if (psf->file.mode == SFM_WRITE || psf->file.mode == SFM_RDWR)
	{	if (psf->is_pipe)
			return SFE_NO_PIPE_WRITE ;

		if ((SF_CONTAINER (psf->sf.format)) != SF_FORMAT_AIFF)
			return SFE_BAD_OPEN_FORMAT ;

		if (psf->file.mode == SFM_WRITE && (subformat == SF_FORMAT_FLOAT || subformat == SF_FORMAT_DOUBLE))
		{	if ((psf->peak_info = peak_info_calloc (psf->sf.channels)) == NULL)
				return SFE_MALLOC_FAILED ;
			psf->peak_info->peak_loc = SF_PEAK_START ;
			} ;

		if (psf->file.mode != SFM_RDWR || psf->filelength < 40)
		{	psf->filelength = 0 ;
			psf->datalength = 0 ;
			psf->dataoffset = 0 ;
			psf->sf.frames = 0 ;
			} ;

		psf->strings.flags = SF_STR_ALLOW_START | SF_STR_ALLOW_END ;

		if ((error = aiff_write_header (psf, SF_FALSE)))
			return error ;

		psf->write_header	= aiff_write_header ;
		psf->set_chunk		= aiff_set_chunk ;
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
				if (psf->sf.frames > comm_fmt.numSampleFrames)
					psf->sf.frames = comm_fmt.numSampleFrames ;
				break ;

		case SF_FORMAT_DWVW_16 :
				error = dwvw_init (psf, 16) ;
				if (psf->sf.frames > comm_fmt.numSampleFrames)
					psf->sf.frames = comm_fmt.numSampleFrames ;
				break ;

		case SF_FORMAT_DWVW_24 :
				error = dwvw_init (psf, 24) ;
				if (psf->sf.frames > comm_fmt.numSampleFrames)
					psf->sf.frames = comm_fmt.numSampleFrames ;
				break ;

		case SF_FORMAT_DWVW_N :
				if (psf->file.mode != SFM_READ)
				{	error = SFE_DWVW_BAD_BITWIDTH ;
					break ;
					} ;
				if (comm_fmt.sampleSize >= 8 && comm_fmt.sampleSize < 24)
				{	error = dwvw_init (psf, comm_fmt.sampleSize) ;
					if (psf->sf.frames > comm_fmt.numSampleFrames)
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
				if (psf->sf.frames > comm_fmt.numSampleFrames)
					psf->sf.frames = comm_fmt.numSampleFrames ;
				break ;

		default : return SFE_UNIMPLEMENTED ;
		} ;

	if (psf->file.mode != SFM_WRITE && psf->sf.frames - comm_fmt.numSampleFrames != 0)
	{	psf_log_printf (psf,
			"*** Frame count read from 'COMM' chunk (%u) not equal to frame count\n"
			"*** calculated from length of 'SSND' chunk (%u).\n",
			comm_fmt.numSampleFrames, (uint32_t) psf->sf.frames) ;
		} ;

	return error ;
} /* aiff_open */

/*==========================================================================================
** Private functions.
*/

/* This function ought to check size */
static uint32_t
marker_to_position (const MARK_ID_POS *m, uint16_t n, int marksize)
{	int i ;

	for (i = 0 ; i < marksize ; i++)
		if (m [i].markerID == n)
			return m [i].position ;
	return 0 ;
} /* marker_to_position */

static int
aiff_read_header (SF_PRIVATE *psf, COMM_CHUNK *comm_fmt)
{	SSND_CHUNK	ssnd_fmt ;
	AIFF_PRIVATE *paiff ;
	BUF_UNION	ubuf ;
	uint32_t	chunk_size = 0, FORMsize, SSNDsize, bytesread, mark_count = 0 ;
	int			k, found_chunk = 0, done = 0, error = 0 ;
	char		*cptr ;
	int			instr_found = 0, mark_found = 0 ;

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
	{	unsigned	marker ;
		size_t jump = chunk_size & 1 ;

		marker = chunk_size = 0 ;
		psf_binheader_readf (psf, "Ejm4", jump, &marker, &chunk_size) ;
		if (marker == 0)
		{	sf_count_t pos = psf_ftell (psf) ;
			psf_log_printf (psf, "Have 0 marker at position %D (0x%x).\n", pos, pos) ;
			break ;
			} ;

		if (psf->file.mode == SFM_RDWR && (found_chunk & HAVE_SSND))
			return SFE_AIFF_RW_SSND_NOT_LAST ;

		psf_store_read_chunk_u32 (&psf->rchunks, marker, psf_ftell (psf), chunk_size) ;

		switch (marker)
		{	case FORM_MARKER :
					if (found_chunk)
						return SFE_AIFF_NO_FORM ;

					FORMsize = chunk_size ;

					found_chunk |= HAVE_FORM ;
					psf_binheader_readf (psf, "m", &marker) ;
					switch (marker)
					{	case AIFC_MARKER :
						case AIFF_MARKER :
							found_chunk |= (marker == AIFC_MARKER) ? (HAVE_AIFC | HAVE_AIFF) : HAVE_AIFF ;
							break ;
						default :
							break ;
						} ;

					if (psf->fileoffset > 0 && psf->filelength > FORMsize + 8)
					{	/* Set file length. */
						psf->filelength = FORMsize + 8 ;
						psf_log_printf (psf, "FORM : %u\n %M\n", FORMsize, marker) ;
						}
					else if (FORMsize != psf->filelength - 2 * SIGNED_SIZEOF (chunk_size))
					{	chunk_size = psf->filelength - 2 * sizeof (chunk_size) ;
						psf_log_printf (psf, "FORM : %u (should be %u)\n %M\n", FORMsize, chunk_size, marker) ;
						FORMsize = chunk_size ;
						}
					else
						psf_log_printf (psf, "FORM : %u\n %M\n", FORMsize, marker) ;
					/* Set this to 0, so we don't jump a byte when parsing the next marker. */
					chunk_size = 0 ;
					break ;


			case COMM_MARKER :
					paiff->comm_offset = psf_ftell (psf) - 8 ;
					chunk_size += chunk_size & 1 ;
					comm_fmt->size = chunk_size ;
					if ((error = aiff_read_comm_chunk (psf, comm_fmt)) != 0)
						return error ;

					found_chunk |= HAVE_COMM ;
					break ;

			case PEAK_MARKER :
					/* Must have COMM chunk before PEAK chunk. */
					if ((found_chunk & (HAVE_FORM | HAVE_AIFF | HAVE_COMM)) != (HAVE_FORM | HAVE_AIFF | HAVE_COMM))
						return SFE_AIFF_PEAK_B4_COMM ;

					psf_log_printf (psf, "%M : %d\n", marker, chunk_size) ;
					if (chunk_size != AIFF_PEAK_CHUNK_SIZE (psf->sf.channels))
					{	psf_binheader_readf (psf, "j", chunk_size) ;
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

					cptr = ubuf.cbuf ;
					for (k = 0 ; k < psf->sf.channels ; k++)
					{	float value ;
						uint32_t position ;

						psf_binheader_readf (psf, "Ef4", &value, &position) ;
						psf->peak_info->peaks [k].value = value ;
						psf->peak_info->peaks [k].position = position ;

						snprintf (cptr, sizeof (ubuf.scbuf), "    %2d   %-12" PRId64 "   %g\n",
								k, psf->peak_info->peaks [k].position, psf->peak_info->peaks [k].value) ;
						cptr [sizeof (ubuf.scbuf) - 1] = 0 ;
						psf_log_printf (psf, "%s", cptr) ;
						} ;

					psf->peak_info->peak_loc = ((found_chunk & HAVE_SSND) == 0) ? SF_PEAK_START : SF_PEAK_END ;
					break ;

			case SSND_MARKER :
					if ((found_chunk & HAVE_AIFC) && (found_chunk & HAVE_FVER) == 0)
						psf_log_printf (psf, "*** Valid AIFC files should have an FVER chunk.\n") ;

					paiff->ssnd_offset = psf_ftell (psf) - 8 ;
					SSNDsize = chunk_size ;
					psf_binheader_readf (psf, "E44", &(ssnd_fmt.offset), &(ssnd_fmt.blocksize)) ;

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
					{	psf_log_printf (psf, "  Offset     : %u\n", ssnd_fmt.offset) ;
						psf_log_printf (psf, "  Block Size : %u ???\n", ssnd_fmt.blocksize) ;
						psf->dataoffset += ssnd_fmt.offset ;
						psf->datalength -= ssnd_fmt.offset ;
						} ;

					/* Only set dataend if there really is data at the end. */
					if (psf->datalength + psf->dataoffset < psf->filelength)
						psf->dataend = psf->datalength + psf->dataoffset ;

					found_chunk |= HAVE_SSND ;

					if (! psf->sf.seekable)
						break ;

					/* Seek to end of SSND chunk. */
					psf_fseek (psf, psf->dataoffset + psf->datalength, SEEK_SET) ;
					break ;

			case c_MARKER :
					if (chunk_size == 0)
						break ;
					if (chunk_size >= SIGNED_SIZEOF (ubuf.scbuf))
					{	psf_log_printf (psf, " %M : %d (too big)\n", marker, chunk_size) ;
						return SFE_INTERNAL ;
						} ;

					cptr = ubuf.cbuf ;
					psf_binheader_readf (psf, "b", cptr, chunk_size + (chunk_size & 1)) ;
					cptr [chunk_size] = 0 ;

					psf_sanitize_string (cptr, chunk_size) ;

					psf_log_printf (psf, " %M : %s\n", marker, cptr) ;
					psf_store_string (psf, SF_STR_COPYRIGHT, cptr) ;
					chunk_size += chunk_size & 1 ;
					break ;

			case AUTH_MARKER :
					if (chunk_size == 0)
						break ;
					if (chunk_size >= SIGNED_SIZEOF (ubuf.scbuf) - 1)
					{	psf_log_printf (psf, " %M : %d (too big)\n", marker, chunk_size) ;
						return SFE_INTERNAL ;
						} ;

					cptr = ubuf.cbuf ;
					psf_binheader_readf (psf, "b", cptr, chunk_size + (chunk_size & 1)) ;
					cptr [chunk_size] = 0 ;
					psf_log_printf (psf, " %M : %s\n", marker, cptr) ;
					psf_store_string (psf, SF_STR_ARTIST, cptr) ;
					chunk_size += chunk_size & 1 ;
					break ;

			case COMT_MARKER :
				{	uint16_t count, id, len ;
					uint32_t timestamp, bytes ;

					if (chunk_size == 0)
						break ;
					bytes = chunk_size ;
					bytes -= psf_binheader_readf (psf, "E2", &count) ;
					psf_log_printf (psf, " %M : %d\n  count  : %d\n", marker, chunk_size, count) ;

					for (k = 0 ; k < count ; k++)
					{	bytes -= psf_binheader_readf (psf, "E422", &timestamp, &id, &len) ;
						psf_log_printf (psf, "   time   : 0x%x\n   marker : %x\n   length : %d\n", timestamp, id, len) ;

						if (len + 1 > SIGNED_SIZEOF (ubuf.scbuf))
						{	psf_log_printf (psf, "\nError : string length (%d) too big.\n", len) ;
							return SFE_INTERNAL ;
							} ;

						cptr = ubuf.cbuf ;
						bytes -= psf_binheader_readf (psf, "b", cptr, len) ;
						cptr [len] = 0 ;
						psf_log_printf (psf, "   string : %s\n", cptr) ;
						} ;

					if (bytes > 0)
						psf_binheader_readf (psf, "j", bytes) ;
					} ;
					break ;

			case APPL_MARKER :
				{	unsigned appl_marker ;

					if (chunk_size == 0)
						break ;
					if (chunk_size >= SIGNED_SIZEOF (ubuf.scbuf) - 1)
					{	psf_log_printf (psf, " %M : %u (too big, skipping)\n", marker, chunk_size) ;
						psf_binheader_readf (psf, "j", chunk_size + (chunk_size & 1)) ;
						break ;
						} ;

					if (chunk_size < 4)
					{	psf_log_printf (psf, " %M : %d (too small, skipping)\n", marker, chunk_size) ;
						psf_binheader_readf (psf, "j", chunk_size + (chunk_size & 1)) ;
						break ;
						} ;

					cptr = ubuf.cbuf ;
					psf_binheader_readf (psf, "mb", &appl_marker, cptr, chunk_size + (chunk_size & 1) - 4) ;
					cptr [chunk_size] = 0 ;

					for (k = 0 ; k < (int) chunk_size ; k++)
						if (! psf_isprint (cptr [k]))
						{	cptr [k] = 0 ;
							break ;
							} ;

					psf_log_printf (psf, " %M : %d\n  AppSig : %M\n  Name   : %s\n", marker, chunk_size, appl_marker, cptr) ;
					psf_store_string (psf, SF_STR_SOFTWARE, cptr) ;
					chunk_size += chunk_size & 1 ;
					} ;
					break ;

			case NAME_MARKER :
					if (chunk_size == 0)
						break ;
					if (chunk_size >= SIGNED_SIZEOF (ubuf.scbuf) - 2)
					{	psf_log_printf (psf, " %M : %d (too big)\n", marker, chunk_size) ;
						return SFE_INTERNAL ;
						} ;

					cptr = ubuf.cbuf ;
					psf_binheader_readf (psf, "b", cptr, chunk_size + (chunk_size & 1)) ;
					cptr [chunk_size] = 0 ;
					psf_log_printf (psf, " %M : %s\n", marker, cptr) ;
					psf_store_string (psf, SF_STR_TITLE, cptr) ;
					chunk_size += chunk_size & 1 ;
					break ;

			case ANNO_MARKER :
					if (chunk_size == 0)
						break ;
					if (chunk_size >= SIGNED_SIZEOF (ubuf.scbuf) - 2)
					{	psf_log_printf (psf, " %M : %d (too big)\n", marker, chunk_size) ;
						return SFE_INTERNAL ;
						} ;

					cptr = ubuf.cbuf ;
					psf_binheader_readf (psf, "b", cptr, chunk_size + (chunk_size & 1)) ;
					cptr [chunk_size] = 0 ;
					psf_log_printf (psf, " %M : %s\n", marker, cptr) ;
					psf_store_string (psf, SF_STR_COMMENT, cptr) ;
					chunk_size += chunk_size & 1 ;
					break ;

			case INST_MARKER :
					if (chunk_size != SIZEOF_INST_CHUNK)
					{	psf_log_printf (psf, " %M : %d (should be %d)\n", marker, chunk_size, SIZEOF_INST_CHUNK) ;
						psf_binheader_readf (psf, "j", chunk_size) ;
						break ;
						} ;
					psf_log_printf (psf, " %M : %d\n", marker, chunk_size) ;
					{	uint8_t bytes [6] ;
						int16_t gain ;

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
					{	int16_t	mode ; /* 0 - no loop, 1 - forward looping, 2 - backward looping */
						const char	*loop_mode ;
						uint16_t begin, end ;

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
					psf_log_printf (psf, " basc : %u\n", chunk_size) ;

					if ((error = aiff_read_basc_chunk (psf, chunk_size)))
						return error ;
					break ;

			case MARK_MARKER :
					psf_log_printf (psf, " %M : %d\n", marker, chunk_size) ;
					{	uint16_t mark_id, n = 0 ;
						uint32_t position ;

						bytesread = psf_binheader_readf (psf, "E2", &n) ;
						mark_count = n ;
						psf_log_printf (psf, "  Count : %u\n", mark_count) ;
						if (paiff->markstr != NULL)
						{	psf_log_printf (psf, "*** Second MARK chunk found. Throwing away the first.\n") ;
							free (paiff->markstr) ;
							} ;
						paiff->markstr = calloc (mark_count, sizeof (MARK_ID_POS)) ;
						if (paiff->markstr == NULL)
							return SFE_MALLOC_FAILED ;

						if (mark_count > 2500) /* 2500 is close to the largest number of cues possible because of block sizes */
						{	psf_log_printf (psf, "  More than 2500 markers, skipping!\n") ;
							psf_binheader_readf (psf, "j", chunk_size - bytesread) ;
							break ;
						} ;

						if ((psf->cues = psf_cues_alloc (mark_count)) == NULL)
							return SFE_MALLOC_FAILED ;

						for (n = 0 ; n < mark_count && bytesread < chunk_size ; n++)
						{	uint32_t pstr_len ;
							uint8_t ch ;

							bytesread += psf_binheader_readf (psf, "E241", &mark_id, &position, &ch) ;
							psf_log_printf (psf, "   Mark ID  : %u\n   Position : %u\n", mark_id, position) ;

							psf->cues->cue_points [n].indx = mark_id ;
							psf->cues->cue_points [n].position = 0 ;
							psf->cues->cue_points [n].fcc_chunk = MAKE_MARKER ('d', 'a', 't', 'a') ; /* always data */
							psf->cues->cue_points [n].chunk_start = 0 ;
							psf->cues->cue_points [n].block_start = 0 ;
							psf->cues->cue_points [n].sample_offset = position ;

							pstr_len = (ch & 1) ? ch : ch + 1 ;

							if (pstr_len < sizeof (ubuf.scbuf) - 1)
							{	bytesread += psf_binheader_readf (psf, "b", ubuf.scbuf, pstr_len) ;
								ubuf.scbuf [pstr_len] = 0 ;
								}
							else
							{	uint32_t read_len = pstr_len - (sizeof (ubuf.scbuf) - 1) ;
								bytesread += psf_binheader_readf (psf, "bj", ubuf.scbuf, read_len, pstr_len - read_len) ;
								ubuf.scbuf [sizeof (ubuf.scbuf) - 1] = 0 ;
								}

							psf_log_printf (psf, "   Name     : %s\n", ubuf.scbuf) ;

							psf_strlcpy (psf->cues->cue_points [n].name, sizeof (psf->cues->cue_points [n].name), ubuf.cbuf) ;

							paiff->markstr [n].markerID = mark_id ;
							paiff->markstr [n].position = position ;
							/*
							**	TODO if ubuf.scbuf is equal to
							**	either Beg_loop, Beg loop or beg loop and spam
							**	if (psf->instrument == NULL && (psf->instrument = psf_instrument_alloc ()) == NULL)
							**		return SFE_MALLOC_FAILED ;
							*/
							} ;
						} ;
					mark_found++ ;
					psf_binheader_readf (psf, "j", chunk_size - bytesread) ;
					break ;

			case FVER_MARKER :
					found_chunk |= HAVE_FVER ;
					/* Falls through. */

			case SFX_MARKER :
					psf_log_printf (psf, " %M : %d\n", marker, chunk_size) ;
					psf_binheader_readf (psf, "j", chunk_size) ;
					break ;

			case NONE_MARKER :
					/* Fix for broken AIFC files with incorrect COMM chunk length. */
					chunk_size = (chunk_size >> 24) - 3 ;
					psf_log_printf (psf, " %M : %d\n", marker, chunk_size) ;
					psf_binheader_readf (psf, "j", make_size_t (chunk_size)) ;
					break ;

			case CHAN_MARKER :
					if (chunk_size < 12)
					{	psf_log_printf (psf, " %M : %d (should be >= 12)\n", marker, chunk_size) ;
						psf_binheader_readf (psf, "j", chunk_size) ;
						break ;
						}

					psf_log_printf (psf, " %M : %d\n", marker, chunk_size) ;

					if ((error = aiff_read_chanmap (psf, chunk_size)))
						return error ;
					break ;

			default :
					if (chunk_size >= 0xffff0000)
					{	done = SF_TRUE ;
						psf_log_printf (psf, "*** Unknown chunk marker (%X) at position %D with length %u. Exiting parser.\n", marker, psf_ftell (psf) - 8, chunk_size) ;
						break ;
						} ;

					if (psf_isprint ((marker >> 24) & 0xFF) && psf_isprint ((marker >> 16) & 0xFF)
						&& psf_isprint ((marker >> 8) & 0xFF) && psf_isprint (marker & 0xFF))
					{	psf_log_printf (psf, " %M : %u (unknown marker)\n", marker, chunk_size) ;

						psf_binheader_readf (psf, "j", chunk_size) ;
						break ;
						} ;

					if (psf_ftell (psf) & 0x03)
					{	psf_log_printf (psf, "  Unknown chunk marker at position %D. Resynching.\n", psf_ftell (psf) - 8) ;
						psf_binheader_readf (psf, "j", -3) ;
						break ;
						} ;
					psf_log_printf (psf, "*** Unknown chunk marker %X at position %D. Exiting parser.\n", marker, psf_ftell (psf)) ;
					done = SF_TRUE ;
					break ;
			} ;	/* switch (marker) */

		if (chunk_size >= psf->filelength)
		{	psf_log_printf (psf, "*** Chunk size %u > file length %D. Exiting parser.\n", chunk_size, psf->filelength) ;
			break ;
			} ;

		if ((! psf->sf.seekable) && (found_chunk & HAVE_SSND))
			break ;

		if (psf_ftell (psf) >= psf->filelength - (2 * SIGNED_SIZEOF (int32_t)))
			break ;
		} ; /* while (1) */

	if (instr_found && mark_found)
	{	int ji, str_index ;
		/* Next loop will convert markers to loop positions for internal handling */
		for (ji = 0 ; ji < psf->instrument->loop_count ; ji ++)
		{	if (ji < ARRAY_LEN (psf->instrument->loops))
			{	psf->instrument->loops [ji].start = marker_to_position (paiff->markstr, psf->instrument->loops [ji].start, mark_count) ;
				psf->instrument->loops [ji].end = marker_to_position (paiff->markstr, psf->instrument->loops [ji].end, mark_count) ;
				psf->instrument->loops [ji].mode = SF_LOOP_FORWARD ;
				} ;
			} ;

		/* The markers that correspond to loop positions can now be removed from cues struct */
		if (psf->cues->cue_count > (uint32_t) (psf->instrument->loop_count * 2))
		{	uint32_t j ;

			for (j = 0 ; j < psf->cues->cue_count - (uint32_t) (psf->instrument->loop_count * 2) ; j ++)
			{	/* This simply copies the information in cues above loop positions and writes it at current count instead */
				psf->cues->cue_points [j].indx = psf->cues->cue_points [j + psf->instrument->loop_count * 2].indx ;
				psf->cues->cue_points [j].position = psf->cues->cue_points [j + psf->instrument->loop_count * 2].position ;
				psf->cues->cue_points [j].fcc_chunk = psf->cues->cue_points [j + psf->instrument->loop_count * 2].fcc_chunk ;
				psf->cues->cue_points [j].chunk_start = psf->cues->cue_points [j + psf->instrument->loop_count * 2].chunk_start ;
				psf->cues->cue_points [j].block_start = psf->cues->cue_points [j + psf->instrument->loop_count * 2].block_start ;
				psf->cues->cue_points [j].sample_offset = psf->cues->cue_points [j + psf->instrument->loop_count * 2].sample_offset ;
				for (str_index = 0 ; str_index < 256 ; str_index++)
					psf->cues->cue_points [j].name [str_index] = psf->cues->cue_points [j + psf->instrument->loop_count * 2].name [str_index] ;
				} ;
			psf->cues->cue_count -= psf->instrument->loop_count * 2 ;
			} else
			{	/* All the cues were in fact loop positions so we can actually remove the cues altogether */
				free (psf->cues) ;
				psf->cues = NULL ;
				}
		} ;

	if (psf->sf.channels < 1)
		return SFE_CHANNEL_COUNT_ZERO ;

	if (psf->sf.channels > SF_MAX_CHANNELS)
		return SFE_CHANNEL_COUNT ;

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
{	AIFF_PRIVATE *paiff = psf->container_data ;

	if (paiff != NULL && paiff->markstr != NULL)
	{	free (paiff->markstr) ;
		paiff->markstr = NULL ;
		} ;

	if (psf->file.mode == SFM_WRITE || psf->file.mode == SFM_RDWR)
	{	aiff_write_tailer (psf) ;
		aiff_write_header (psf, SF_TRUE) ;
		} ;

	return 0 ;
} /* aiff_close */

static int
aiff_read_comm_chunk (SF_PRIVATE *psf, COMM_CHUNK *comm_fmt)
{	BUF_UNION	ubuf ;
	int subformat, samplerate ;

	ubuf.scbuf [0] = 0 ;

	/* The COMM chunk has an int aligned to an odd word boundary. Some
	** procesors are not able to deal with this (ie bus fault) so we have
	** to take special care.
	*/

	psf_binheader_readf (psf, "E242b", &(comm_fmt->numChannels), &(comm_fmt->numSampleFrames),
				&(comm_fmt->sampleSize), &(comm_fmt->sampleRate), SIGNED_SIZEOF (comm_fmt->sampleRate)) ;

	if (comm_fmt->size > 0x10000 && (comm_fmt->size & 0xffff) == 0)
	{	psf_log_printf (psf, " COMM : %d (0x%x) *** should be ", comm_fmt->size, comm_fmt->size) ;
		comm_fmt->size = ENDSWAP_32 (comm_fmt->size) ;
		psf_log_printf (psf, "%d (0x%x)\n", comm_fmt->size, comm_fmt->size) ;
		}
	else
		psf_log_printf (psf, " COMM : %d\n", comm_fmt->size) ;

	if (comm_fmt->size == SIZEOF_AIFF_COMM)
		comm_fmt->encoding = NONE_MARKER ;
	else if (comm_fmt->size == SIZEOF_AIFC_COMM_MIN)
		psf_binheader_readf (psf, "Em", &(comm_fmt->encoding)) ;
	else if (comm_fmt->size >= SIZEOF_AIFC_COMM)
	{	uint8_t encoding_len ;
		unsigned read_len ;

		psf_binheader_readf (psf, "Em1", &(comm_fmt->encoding), &encoding_len) ;

		comm_fmt->size = SF_MIN (sizeof (ubuf.scbuf), make_size_t (comm_fmt->size)) ;
		memset (ubuf.scbuf, 0, comm_fmt->size) ;
		read_len = comm_fmt->size - SIZEOF_AIFC_COMM + 1 ;
		psf_binheader_readf (psf, "b", ubuf.scbuf, read_len) ;
		ubuf.scbuf [read_len + 1] = 0 ;
		} ;

	samplerate = tenbytefloat2int (comm_fmt->sampleRate) ;

	psf_log_printf (psf, "  Sample Rate : %d\n", samplerate) ;
	psf_log_printf (psf, "  Frames      : %u%s\n", comm_fmt->numSampleFrames, (comm_fmt->numSampleFrames == 0 && psf->filelength > 104) ? " (Should not be 0)" : "") ;

	if (comm_fmt->numChannels < 1 || comm_fmt->numChannels > SF_MAX_CHANNELS)
	{	psf_log_printf (psf, "  Channels    : %d (should be >= 1 and < %d)\n", comm_fmt->numChannels, SF_MAX_CHANNELS) ;
		return SFE_CHANNEL_COUNT_BAD ;
		} ;

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

	psf->sf.samplerate = samplerate ;
	psf->sf.frames = comm_fmt->numSampleFrames ;
	psf->sf.channels = comm_fmt->numChannels ;
	psf->bytewidth = BITWIDTH2BYTES (comm_fmt->sampleSize) ;

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
				psf->sf.format = (SF_FORMAT_AIFF | SF_FORMAT_GSM610) ;
				break ;


		case ima4_MARKER :
				psf->endian = SF_ENDIAN_BIG ;
				psf->sf.format = (SF_FORMAT_AIFF | SF_FORMAT_IMA_ADPCM) ;
				break ;

		default :
			psf_log_printf (psf, "AIFC : Unimplemented format : %M\n", comm_fmt->encoding) ;
			return SFE_UNIMPLEMENTED ;
		} ;

	if (! ubuf.scbuf [0])
		psf_log_printf (psf, "  Encoding    : %M\n", comm_fmt->encoding) ;
	else
		psf_log_printf (psf, "  Encoding    : %M => %s\n", comm_fmt->encoding, ubuf.scbuf) ;

	return 0 ;
} /* aiff_read_comm_chunk */


/*==========================================================================================
*/

static void
aiff_rewrite_header (SF_PRIVATE *psf)
{
	/* Assuming here that the header has already been written and just
	** needs to be corrected for new data length. That means that we
	** only change the length fields of the FORM and SSND chunks ;
	** everything else can be skipped over.
	*/
	int k, ch, comm_size, comm_frames ;

	psf_fseek (psf, 0, SEEK_SET) ;
	psf_fread (psf->header.ptr, psf->dataoffset, 1, psf) ;

	psf->header.indx = 0 ;

	/* FORM chunk. */
	psf_binheader_writef (psf, "Etm8", BHWm (FORM_MARKER), BHW8 (psf->filelength - 8)) ;

	/* COMM chunk. */
	if ((k = psf_find_read_chunk_m32 (&psf->rchunks, COMM_MARKER)) >= 0)
	{	psf->header.indx = psf->rchunks.chunks [k].offset - 8 ;
		comm_frames = psf->sf.frames ;
		comm_size = psf->rchunks.chunks [k].len ;
		psf_binheader_writef (psf, "Em42t4", BHWm (COMM_MARKER), BHW4 (comm_size), BHW2 (psf->sf.channels), BHW4 (comm_frames)) ;
		} ;

	/* PEAK chunk. */
	if ((k = psf_find_read_chunk_m32 (&psf->rchunks, PEAK_MARKER)) >= 0)
	{	psf->header.indx = psf->rchunks.chunks [k].offset - 8 ;
		psf_binheader_writef (psf, "Em4", BHWm (PEAK_MARKER), BHW4 (AIFF_PEAK_CHUNK_SIZE (psf->sf.channels))) ;
		psf_binheader_writef (psf, "E44", BHW4 (1), BHW4 (time (NULL))) ;
		for (ch = 0 ; ch < psf->sf.channels ; ch++)
			psf_binheader_writef (psf, "Eft8", BHWf ((float) psf->peak_info->peaks [ch].value), BHW8 (psf->peak_info->peaks [ch].position)) ;
		} ;


	/* SSND chunk. */
	if ((k = psf_find_read_chunk_m32 (&psf->rchunks, SSND_MARKER)) >= 0)
	{	psf->header.indx = psf->rchunks.chunks [k].offset - 8 ;
		psf_binheader_writef (psf, "Etm8", BHWm (SSND_MARKER), BHW8 (psf->datalength + SIZEOF_SSND_CHUNK)) ;
		} ;

	/* Header mangling complete so write it out. */
	psf_fseek (psf, 0, SEEK_SET) ;
	psf_fwrite (psf->header.ptr, psf->header.indx, 1, psf) ;

	return ;
} /* aiff_rewrite_header */

static int
aiff_write_header (SF_PRIVATE *psf, int calc_length)
{	sf_count_t		current ;
	AIFF_PRIVATE	*paiff ;
	uint8_t	comm_sample_rate [10], comm_zero_bytes [2] = { 0, 0 } ;
	uint32_t	comm_type, comm_size, comm_encoding, comm_frames = 0, uk ;
	int				k, endian, has_data = SF_FALSE ;
	int16_t			bit_width ;

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

	if (psf->file.mode == SFM_RDWR && psf->dataoffset > 0 && psf->rchunks.count > 0)
	{	aiff_rewrite_header (psf) ;
		if (current > 0)
			psf_fseek (psf, current, SEEK_SET) ;
		return 0 ;
		} ;

	endian = SF_ENDIAN (psf->sf.format) ;
	if (CPU_IS_LITTLE_ENDIAN && endian == SF_ENDIAN_CPU)
		endian = SF_ENDIAN_LITTLE ;

	/* Standard value here. */
	bit_width = psf->bytewidth * 8 ;
	comm_frames = (psf->sf.frames > 0xFFFFFFFF) ? 0xFFFFFFFF : psf->sf.frames ;

	switch (SF_CODEC (psf->sf.format) | endian)
	{	case SF_FORMAT_PCM_S8 | SF_ENDIAN_BIG :
			psf->endian = SF_ENDIAN_BIG ;
			comm_type = AIFC_MARKER ;
			comm_size = SIZEOF_AIFC_COMM ;
			comm_encoding = twos_MARKER ;
			break ;

		case SF_FORMAT_PCM_S8 | SF_ENDIAN_LITTLE :
			psf->endian = SF_ENDIAN_LITTLE ;
			comm_type = AIFC_MARKER ;
			comm_size = SIZEOF_AIFC_COMM ;
			comm_encoding = sowt_MARKER ;
			break ;

		case SF_FORMAT_PCM_16 | SF_ENDIAN_BIG :
			psf->endian = SF_ENDIAN_BIG ;
			comm_type = AIFC_MARKER ;
			comm_size = SIZEOF_AIFC_COMM ;
			comm_encoding = twos_MARKER ;
			break ;

		case SF_FORMAT_PCM_16 | SF_ENDIAN_LITTLE :
			psf->endian = SF_ENDIAN_LITTLE ;
			comm_type = AIFC_MARKER ;
			comm_size = SIZEOF_AIFC_COMM ;
			comm_encoding = sowt_MARKER ;
			break ;

		case SF_FORMAT_PCM_24 | SF_ENDIAN_BIG :
			psf->endian = SF_ENDIAN_BIG ;
			comm_type = AIFC_MARKER ;
			comm_size = SIZEOF_AIFC_COMM ;
			comm_encoding = in24_MARKER ;
			break ;

		case SF_FORMAT_PCM_24 | SF_ENDIAN_LITTLE :
			psf->endian = SF_ENDIAN_LITTLE ;
			comm_type = AIFC_MARKER ;
			comm_size = SIZEOF_AIFC_COMM ;
			comm_encoding = ni24_MARKER ;
			break ;

		case SF_FORMAT_PCM_32 | SF_ENDIAN_BIG :
			psf->endian = SF_ENDIAN_BIG ;
			comm_type = AIFC_MARKER ;
			comm_size = SIZEOF_AIFC_COMM ;
			comm_encoding = in32_MARKER ;
			break ;

		case SF_FORMAT_PCM_32 | SF_ENDIAN_LITTLE :
			psf->endian = SF_ENDIAN_LITTLE ;
			comm_type = AIFC_MARKER ;
			comm_size = SIZEOF_AIFC_COMM ;
			comm_encoding = ni32_MARKER ;
			break ;

		case SF_FORMAT_PCM_S8 :			/* SF_ENDIAN_FILE */
		case SF_FORMAT_PCM_16 :
		case SF_FORMAT_PCM_24 :
		case SF_FORMAT_PCM_32 :
			psf->endian = SF_ENDIAN_BIG ;
			comm_type = AIFF_MARKER ;
			comm_size = SIZEOF_AIFF_COMM ;
			comm_encoding = 0 ;
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
	psf->header.ptr [0] = 0 ;
	psf->header.indx = 0 ;
	psf_fseek (psf, 0, SEEK_SET) ;

	psf_binheader_writef (psf, "Etm8", BHWm (FORM_MARKER), BHW8 (psf->filelength - 8)) ;

	/* Write AIFF/AIFC marker and COM chunk. */
	if (comm_type == AIFC_MARKER)
		/* AIFC must have an FVER chunk. */
		psf_binheader_writef (psf, "Emm44", BHWm (comm_type), BHWm (FVER_MARKER), BHW4 (4), BHW4 (0xA2805140)) ;
	else
		psf_binheader_writef (psf, "Em", BHWm (comm_type)) ;

	paiff->comm_offset = psf->header.indx - 8 ;

	memset (comm_sample_rate, 0, sizeof (comm_sample_rate)) ;
	uint2tenbytefloat (psf->sf.samplerate, comm_sample_rate) ;

	psf_binheader_writef (psf, "Em42t42", BHWm (COMM_MARKER), BHW4 (comm_size), BHW2 (psf->sf.channels), BHW4 (comm_frames), BHW2 (bit_width)) ;
	psf_binheader_writef (psf, "b", BHWv (comm_sample_rate), BHWz (sizeof (comm_sample_rate))) ;

	/* AIFC chunks have some extra data. */
	if (comm_type == AIFC_MARKER)
		psf_binheader_writef (psf, "mb", BHWm (comm_encoding), BHWv (comm_zero_bytes), BHWz (sizeof (comm_zero_bytes))) ;

	if (psf->channel_map && paiff->chanmap_tag)
		psf_binheader_writef (psf, "Em4444", BHWm (CHAN_MARKER), BHW4 (12), BHW4 (paiff->chanmap_tag), BHW4 (0), BHW4 (0)) ;

	/* Check if there's a INST chunk to write */
	if (psf->instrument != NULL && psf->cues != NULL)
	{	/* Huge chunk of code removed here because it had egregious errors that were
		** not detected by either the compiler or the tests. It was found when updating
		** the way psf_binheader_writef works.
		*/
		}
	else if (psf->instrument == NULL && psf->cues != NULL)
	{	/* There are cues but no loops */
		uint32_t idx ;
		int totalStringLength = 0, stringLength ;

		/* Here we count how many bytes will the pascal strings need */
		for (idx = 0 ; idx < psf->cues->cue_count ; idx++)
		{	stringLength = strlen (psf->cues->cue_points [idx].name) + 1 ; /* We'll count the first byte also of every pascal string */
			totalStringLength += stringLength + (stringLength % 2 == 0 ? 0 : 1) ;
			} ;

		psf_binheader_writef (psf, "Em42",
			BHWm (MARK_MARKER), BHW4 (2 + psf->cues->cue_count * (2 + 4) + totalStringLength), BHW2 (psf->cues->cue_count)) ;

		for (idx = 0 ; idx < psf->cues->cue_count ; idx++)
			psf_binheader_writef (psf, "E24p", BHW2 (psf->cues->cue_points [idx].indx), BHW4 (psf->cues->cue_points [idx].sample_offset), BHWp (psf->cues->cue_points [idx].name)) ;
		} ;

	if (psf->strings.flags & SF_STR_LOCATE_START)
		aiff_write_strings (psf, SF_STR_LOCATE_START) ;

	if (psf->peak_info != NULL && psf->peak_info->peak_loc == SF_PEAK_START)
	{	psf_binheader_writef (psf, "Em4", BHWm (PEAK_MARKER), BHW4 (AIFF_PEAK_CHUNK_SIZE (psf->sf.channels))) ;
		psf_binheader_writef (psf, "E44", BHW4 (1), BHW4 (time (NULL))) ;
		for (k = 0 ; k < psf->sf.channels ; k++)
			psf_binheader_writef (psf, "Eft8", BHWf ((float) psf->peak_info->peaks [k].value), BHW8 (psf->peak_info->peaks [k].position)) ;
		} ;

	/* Write custom headers. */
	for (uk = 0 ; uk < psf->wchunks.used ; uk++)
		psf_binheader_writef (psf, "Em4b", BHWm (psf->wchunks.chunks [uk].mark32), BHW4 (psf->wchunks.chunks [uk].len), BHWv (psf->wchunks.chunks [uk].data), BHWz (psf->wchunks.chunks [uk].len)) ;

	/* Write SSND chunk. */
	paiff->ssnd_offset = psf->header.indx ;
	psf_binheader_writef (psf, "Etm844", BHWm (SSND_MARKER), BHW8 (psf->datalength + SIZEOF_SSND_CHUNK), BHW4 (0), BHW4 (0)) ;

	/* Header construction complete so write it out. */
	psf_fwrite (psf->header.ptr, psf->header.indx, 1, psf) ;

	if (psf->error)
		return psf->error ;

	if (has_data && psf->dataoffset != psf->header.indx)
		return psf->error = SFE_INTERNAL ;

	psf->dataoffset = psf->header.indx ;

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
	psf->header.ptr [0] = 0 ;
	psf->header.indx = 0 ;

	psf->dataend = psf_fseek (psf, 0, SEEK_END) ;

	/* Make sure tailer data starts at even byte offset. Pad if necessary. */
	if (psf->dataend % 2 == 1)
	{	psf_fwrite (psf->header.ptr, 1, 1, psf) ;
		psf->dataend ++ ;
		} ;

	if (psf->peak_info != NULL && psf->peak_info->peak_loc == SF_PEAK_END)
	{	psf_binheader_writef (psf, "Em4", BHWm (PEAK_MARKER), BHW4 (AIFF_PEAK_CHUNK_SIZE (psf->sf.channels))) ;
		psf_binheader_writef (psf, "E44", BHW4 (1), BHW4 (time (NULL))) ;
		for (k = 0 ; k < psf->sf.channels ; k++)
			psf_binheader_writef (psf, "Eft8", BHWf ((float) psf->peak_info->peaks [k].value), BHW8 (psf->peak_info->peaks [k].position)) ;
		} ;

	if (psf->strings.flags & SF_STR_LOCATE_END)
		aiff_write_strings (psf, SF_STR_LOCATE_END) ;

	/* Write the tailer. */
	if (psf->header.indx > 0)
		psf_fwrite (psf->header.ptr, psf->header.indx, 1, psf) ;

	return 0 ;
} /* aiff_write_tailer */

static void
aiff_write_strings (SF_PRIVATE *psf, int location)
{	int	k, slen ;

	for (k = 0 ; k < SF_MAX_STRINGS ; k++)
	{	if (psf->strings.data [k].type == 0)
			break ;

		if (psf->strings.data [k].flags != location)
			continue ;

		switch (psf->strings.data [k].type)
		{	case SF_STR_SOFTWARE :
				slen = strlen (psf->strings.storage + psf->strings.data [k].offset) ;
				psf_binheader_writef (psf, "Em4mb", BHWm (APPL_MARKER), BHW4 (slen + 4), BHWm (m3ga_MARKER), BHWv (psf->strings.storage + psf->strings.data [k].offset), BHWz (slen + (slen & 1))) ;
				break ;

			case SF_STR_TITLE :
				psf_binheader_writef (psf, "EmS", BHWm (NAME_MARKER), BHWS (psf->strings.storage + psf->strings.data [k].offset)) ;
				break ;

			case SF_STR_COPYRIGHT :
				psf_binheader_writef (psf, "EmS", BHWm (c_MARKER), BHWS (psf->strings.storage + psf->strings.data [k].offset)) ;
				break ;

			case SF_STR_ARTIST :
				psf_binheader_writef (psf, "EmS", BHWm (AUTH_MARKER), BHWS (psf->strings.storage + psf->strings.data [k].offset)) ;
				break ;

			case SF_STR_COMMENT :
				psf_binheader_writef (psf, "EmS", BHWm (ANNO_MARKER), BHWS (psf->strings.storage + psf->strings.data [k].offset)) ;
				break ;

			/*
			case SF_STR_DATE :
				psf_binheader_writef (psf, "Ems", BHWm (ICRD_MARKER), BHWs (psf->strings.data [k].str)) ;
				break ;
			*/
			} ;
		} ;

	return ;
} /* aiff_write_strings */

static int
aiff_command (SF_PRIVATE * psf, int command, void * UNUSED (data), int UNUSED (datasize))
{	AIFF_PRIVATE	*paiff ;

	if ((paiff = psf->container_data) == NULL)
		return SFE_INTERNAL ;

	switch (command)
	{	case SFC_SET_CHANNEL_MAP_INFO :
			paiff->chanmap_tag = aiff_caf_find_channel_layout_tag (psf->channel_map, psf->sf.channels) ;
			return (paiff->chanmap_tag != 0) ;

		default :
			break ;
	} ;

	return 0 ;
} /* aiff_command */

static const char*
get_loop_mode_str (int16_t mode)
{	switch (mode)
	{	case 0 : return "none" ;
		case 1 : return "forward" ;
		case 2 : return "backward" ;
		} ;

	return "*** unknown" ;
} /* get_loop_mode_str */

static int16_t
get_loop_mode (int16_t mode)
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
tenbytefloat2int (uint8_t *bytes)
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
uint2tenbytefloat (uint32_t num, uint8_t *bytes)
{	uint32_t mask = 0x40000000 ;
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

	for (count = 0 ; count < 32 ; count ++)
	{	if (num & mask)
			break ;
		mask >>= 1 ;
		} ;

	num = count < 31 ? num << (count + 1) : 0 ;
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


static int
aiff_read_chanmap (SF_PRIVATE * psf, unsigned dword)
{	const AIFF_CAF_CHANNEL_MAP * map_info ;
	unsigned channel_bitmap, channel_decriptions, bytesread ;
	int layout_tag ;

	bytesread = psf_binheader_readf (psf, "444", &layout_tag, &channel_bitmap, &channel_decriptions) ;

	if ((map_info = aiff_caf_of_channel_layout_tag (layout_tag)) == NULL)
		return 0 ;

	psf_log_printf (psf, "  Tag    : %x\n", layout_tag) ;
	if (map_info)
		psf_log_printf (psf, "  Layout : %s\n", map_info->name) ;

	if (bytesread < dword)
		psf_binheader_readf (psf, "j", dword - bytesread) ;

	if (map_info->channel_map != NULL)
	{	size_t chanmap_size = SF_MIN (psf->sf.channels, layout_tag & 0xffff) * sizeof (psf->channel_map [0]) ;

		free (psf->channel_map) ;

		if ((psf->channel_map = malloc (chanmap_size)) == NULL)
			return SFE_MALLOC_FAILED ;

		memcpy (psf->channel_map, map_info->channel_map, chanmap_size) ;
		} ;

	return 0 ;
} /* aiff_read_chanmap */

/*==============================================================================
*/

static int
aiff_set_chunk (SF_PRIVATE *psf, const SF_CHUNK_INFO * chunk_info)
{	return psf_save_write_chunk (&psf->wchunks, chunk_info) ;
} /* aiff_set_chunk */

static SF_CHUNK_ITERATOR *
aiff_next_chunk_iterator (SF_PRIVATE *psf, SF_CHUNK_ITERATOR * iterator)
{	return psf_next_chunk_iterator (&psf->rchunks, iterator) ;
} /* aiff_next_chunk_iterator */

static int
aiff_get_chunk_size (SF_PRIVATE *psf, const SF_CHUNK_ITERATOR * iterator, SF_CHUNK_INFO * chunk_info)
{	int indx ;

	if ((indx = psf_find_read_chunk_iterator (&psf->rchunks, iterator)) < 0)
		return SFE_UNKNOWN_CHUNK ;

	chunk_info->datalen = psf->rchunks.chunks [indx].len ;

	return SFE_NO_ERROR ;
} /* aiff_get_chunk_size */

static int
aiff_get_chunk_data (SF_PRIVATE *psf, const SF_CHUNK_ITERATOR * iterator, SF_CHUNK_INFO * chunk_info)
{	sf_count_t pos ;
	int indx ;

	if ((indx = psf_find_read_chunk_iterator (&psf->rchunks, iterator)) < 0)
		return SFE_UNKNOWN_CHUNK ;

	if (chunk_info->data == NULL)
		return SFE_BAD_CHUNK_DATA_PTR ;

	chunk_info->id_size = psf->rchunks.chunks [indx].id_size ;
	memcpy (chunk_info->id, psf->rchunks.chunks [indx].id, sizeof (chunk_info->id) / sizeof (*chunk_info->id)) ;

	pos = psf_ftell (psf) ;
	psf_fseek (psf, psf->rchunks.chunks [indx].offset, SEEK_SET) ;
	psf_fread (chunk_info->data, SF_MIN (chunk_info->datalen, psf->rchunks.chunks [indx].len), 1, psf) ;
	psf_fseek (psf, pos, SEEK_SET) ;

	return SFE_NO_ERROR ;
} /* aiff_get_chunk_data */
