/*
** Copyright (C) 2005-2017 Erik de Castro Lopo <erikd@mega-nerd.com>
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
#include	<math.h>
#include	<inttypes.h>

#include	"sndfile.h"
#include	"sfendian.h"
#include	"common.h"
#include	"chanmap.h"

/*------------------------------------------------------------------------------
** Macros to handle big/little endian issues.
*/

#define aac_MARKER		MAKE_MARKER ('a', 'a', 'c', ' ')
#define alac_MARKER		MAKE_MARKER ('a', 'l', 'a', 'c')
#define alaw_MARKER		MAKE_MARKER ('a', 'l', 'a', 'w')
#define caff_MARKER		MAKE_MARKER ('c', 'a', 'f', 'f')
#define chan_MARKER		MAKE_MARKER ('c', 'h', 'a', 'n')
#define data_MARKER		MAKE_MARKER ('d', 'a', 't', 'a')
#define desc_MARKER		MAKE_MARKER ('d', 'e', 's', 'c')
#define edct_MARKER		MAKE_MARKER ('e', 'd', 'c', 't')
#define free_MARKER		MAKE_MARKER ('f', 'r', 'e', 'e')
#define ima4_MARKER		MAKE_MARKER ('i', 'm', 'a', '4')
#define info_MARKER		MAKE_MARKER ('i', 'n', 'f', 'o')
#define inst_MARKER		MAKE_MARKER ('i', 'n', 's', 't')
#define kuki_MARKER		MAKE_MARKER ('k', 'u', 'k', 'i')
#define lpcm_MARKER		MAKE_MARKER ('l', 'p', 'c', 'm')
#define mark_MARKER		MAKE_MARKER ('m', 'a', 'r', 'k')
#define midi_MARKER		MAKE_MARKER ('m', 'i', 'd', 'i')
#define mp1_MARKER		MAKE_MARKER ('.', 'm', 'p', '1')
#define mp2_MARKER		MAKE_MARKER ('.', 'm', 'p', '2')
#define mp3_MARKER		MAKE_MARKER ('.', 'm', 'p', '3')
#define ovvw_MARKER		MAKE_MARKER ('o', 'v', 'v', 'w')
#define pakt_MARKER		MAKE_MARKER ('p', 'a', 'k', 't')
#define peak_MARKER		MAKE_MARKER ('p', 'e', 'a', 'k')
#define regn_MARKER		MAKE_MARKER ('r', 'e', 'g', 'n')
#define strg_MARKER		MAKE_MARKER ('s', 't', 'r', 'g')
#define umid_MARKER		MAKE_MARKER ('u', 'm', 'i', 'd')
#define uuid_MARKER		MAKE_MARKER ('u', 'u', 'i', 'd')
#define ulaw_MARKER		MAKE_MARKER ('u', 'l', 'a', 'w')
#define MAC3_MARKER		MAKE_MARKER ('M', 'A', 'C', '3')
#define MAC6_MARKER		MAKE_MARKER ('M', 'A', 'C', '6')

#define CAF_PEAK_CHUNK_SIZE(ch) 	((int) (sizeof (int) + ch * (sizeof (float) + 8)))

#define SFE_CAF_NOT_CAF	666
#define SFE_CAF_NO_DESC	667
#define SFE_CAF_BAD_PEAK 668

/*------------------------------------------------------------------------------
** Typedefs.
*/

typedef struct
{	uint8_t		srate [8] ;
	uint32_t	fmt_id ;
	uint32_t	fmt_flags ;
	uint32_t	pkt_bytes ;
	uint32_t	frames_per_packet ;
	uint32_t	channels_per_frame ;
	uint32_t	bits_per_chan ;
} DESC_CHUNK ;

typedef struct
{	int			chanmap_tag ;

	ALAC_DECODER_INFO	alac ;
} CAF_PRIVATE ;

/*------------------------------------------------------------------------------
** Private static functions.
*/

static int	caf_close (SF_PRIVATE *psf) ;
static int	caf_read_header (SF_PRIVATE *psf) ;
static int	caf_write_header (SF_PRIVATE *psf, int calc_length) ;
static int	caf_write_tailer (SF_PRIVATE *psf) ;
static int	caf_command (SF_PRIVATE *psf, int command, void *data, int datasize) ;
static int	caf_read_chanmap (SF_PRIVATE * psf, sf_count_t chunk_size) ;
static int	caf_read_strings (SF_PRIVATE * psf, sf_count_t chunk_size) ;
static void	caf_write_strings (SF_PRIVATE * psf, int location) ;


static int caf_set_chunk (SF_PRIVATE *psf, const SF_CHUNK_INFO * chunk_info) ;
static SF_CHUNK_ITERATOR * caf_next_chunk_iterator (SF_PRIVATE *psf, SF_CHUNK_ITERATOR * iterator) ;
static int caf_get_chunk_size (SF_PRIVATE *psf, const SF_CHUNK_ITERATOR * iterator, SF_CHUNK_INFO * chunk_info) ;
static int caf_get_chunk_data (SF_PRIVATE *psf, const SF_CHUNK_ITERATOR * iterator, SF_CHUNK_INFO * chunk_info) ;

/*------------------------------------------------------------------------------
** Public function.
*/

int
caf_open (SF_PRIVATE *psf)
{	CAF_PRIVATE * pcaf ;
	int	subformat, format, error = 0 ;

	if ((psf->container_data = calloc (1, sizeof (CAF_PRIVATE))) == NULL)
		return SFE_MALLOC_FAILED ;

	pcaf = psf->container_data ;

	if (psf->file.mode == SFM_READ || (psf->file.mode == SFM_RDWR && psf->filelength > 0))
	{	if ((error = caf_read_header (psf)))
			return error ;

		psf->next_chunk_iterator = caf_next_chunk_iterator ;
		psf->get_chunk_size = caf_get_chunk_size ;
		psf->get_chunk_data = caf_get_chunk_data ;
		} ;

	subformat = SF_CODEC (psf->sf.format) ;

	if (psf->file.mode == SFM_WRITE || psf->file.mode == SFM_RDWR)
	{	if (psf->is_pipe)
			return SFE_NO_PIPE_WRITE ;

		format = SF_CONTAINER (psf->sf.format) ;
		if (format != SF_FORMAT_CAF)
			return	SFE_BAD_OPEN_FORMAT ;

		psf->blockwidth = psf->bytewidth * psf->sf.channels ;

		if (psf->file.mode != SFM_RDWR || psf->filelength < 44)
		{	psf->filelength = 0 ;
			psf->datalength = 0 ;
			psf->dataoffset = 0 ;
			psf->sf.frames = 0 ;
			} ;

		psf->strings.flags = SF_STR_ALLOW_START | SF_STR_ALLOW_END ;

		/*
		**	By default, add the peak chunk to floating point files. Default behaviour
		**	can be switched off using sf_command (SFC_SET_PEAK_CHUNK, SF_FALSE).
		*/
		if (psf->file.mode == SFM_WRITE && (subformat == SF_FORMAT_FLOAT || subformat == SF_FORMAT_DOUBLE))
		{	if ((psf->peak_info = peak_info_calloc (psf->sf.channels)) == NULL)
				return SFE_MALLOC_FAILED ;
			psf->peak_info->peak_loc = SF_PEAK_START ;
			} ;

		if ((error = caf_write_header (psf, SF_FALSE)) != 0)
			return error ;

		psf->write_header	= caf_write_header ;
		psf->set_chunk		= caf_set_chunk ;
		} ;

	psf->container_close = caf_close ;
	psf->command = caf_command ;

	switch (subformat)
	{	case SF_FORMAT_PCM_S8 :
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

		case SF_FORMAT_ALAC_16 :
		case SF_FORMAT_ALAC_20 :
		case SF_FORMAT_ALAC_24 :
		case SF_FORMAT_ALAC_32 :
					if (psf->file.mode == SFM_READ)
						/* Only pass the ALAC_DECODER_INFO in read mode. */
						error = alac_init (psf, &pcaf->alac) ;
					else
						error = alac_init (psf, NULL) ;
					break ;

		/* Lite remove end */

		default :
			return SFE_UNSUPPORTED_ENCODING ;
		} ;

	return error ;
} /* caf_open */

static int
caf_close (SF_PRIVATE *psf)
{
	if (psf->file.mode == SFM_WRITE || psf->file.mode == SFM_RDWR)
	{	caf_write_tailer (psf) ;
		caf_write_header (psf, SF_TRUE) ;
		} ;

	return 0 ;
} /* caf_close */

static int
caf_command (SF_PRIVATE * psf, int command, void * UNUSED (data), int UNUSED (datasize))
{	CAF_PRIVATE	*pcaf ;

	if ((pcaf = psf->container_data) == NULL)
		return SFE_INTERNAL ;

	switch (command)
	{	case SFC_SET_CHANNEL_MAP_INFO :
			pcaf->chanmap_tag = aiff_caf_find_channel_layout_tag (psf->channel_map, psf->sf.channels) ;
			return (pcaf->chanmap_tag != 0) ;

		default :
			break ;
	} ;

	return 0 ;
} /* caf_command */

/*------------------------------------------------------------------------------
*/

static int
decode_desc_chunk (SF_PRIVATE *psf, const DESC_CHUNK *desc)
{	int format = SF_FORMAT_CAF ;

	psf->sf.channels = desc->channels_per_frame ;

	if (desc->fmt_id == alac_MARKER)
	{	CAF_PRIVATE	*pcaf ;

		if ((pcaf = psf->container_data) != NULL)
		{	switch (desc->fmt_flags)
			{	case 1 :
					pcaf->alac.bits_per_sample = 16 ;
					format |= SF_FORMAT_ALAC_16 ;
					break ;
				case 2 :
					pcaf->alac.bits_per_sample = 20 ;
					format |= SF_FORMAT_ALAC_20 ;
					break ;
				case 3 :
					pcaf->alac.bits_per_sample = 24 ;
					format |= SF_FORMAT_ALAC_24 ;
					break ;
				case 4 :
					pcaf->alac.bits_per_sample = 32 ;
					format |= SF_FORMAT_ALAC_32 ;
					break ;
				default :
					psf_log_printf (psf, "Bad ALAC format flag value of %d\n", desc->fmt_flags) ;
				} ;

			pcaf->alac.frames_per_packet = desc->frames_per_packet ;
			} ;

		return format ;
		} ;

	format |= psf->endian == SF_ENDIAN_LITTLE ? SF_ENDIAN_LITTLE : 0 ;

	if (desc->fmt_id == lpcm_MARKER && desc->fmt_flags & 1)
	{	/* Floating point data. */
		if (desc->bits_per_chan == 32 && desc->pkt_bytes == 4 * desc->channels_per_frame)
		{	psf->bytewidth = 4 ;
			return format | SF_FORMAT_FLOAT ;
			} ;
		if (desc->bits_per_chan == 64 && desc->pkt_bytes == 8 * desc->channels_per_frame)
		{	psf->bytewidth = 8 ;
			return format | SF_FORMAT_DOUBLE ;
			} ;
		} ;

	if (desc->fmt_id == lpcm_MARKER && (desc->fmt_flags & 1) == 0)
	{	/* Integer data. */
		if (desc->bits_per_chan == 32 && desc->pkt_bytes == 4 * desc->channels_per_frame)
		{	psf->bytewidth = 4 ;
			return format | SF_FORMAT_PCM_32 ;
			} ;
		if (desc->bits_per_chan == 24 && desc->pkt_bytes == 3 * desc->channels_per_frame)
		{	psf->bytewidth = 3 ;
			return format | SF_FORMAT_PCM_24 ;
			} ;
		if (desc->bits_per_chan == 16 && desc->pkt_bytes == 2 * desc->channels_per_frame)
		{	psf->bytewidth = 2 ;
			return format | SF_FORMAT_PCM_16 ;
			} ;
		if (desc->bits_per_chan == 8 && desc->pkt_bytes == 1 * desc->channels_per_frame)
		{	psf->bytewidth = 1 ;
			return format | SF_FORMAT_PCM_S8 ;
			} ;
		} ;

	if (desc->fmt_id == alaw_MARKER && desc->bits_per_chan == 8)
	{	psf->bytewidth = 1 ;
		return format | SF_FORMAT_ALAW ;
		} ;

	if (desc->fmt_id == ulaw_MARKER && desc->bits_per_chan == 8)
	{	psf->bytewidth = 1 ;
		return format | SF_FORMAT_ULAW ;
		} ;

	psf_log_printf (psf, "**** Unknown format identifier.\n") ;

	return 0 ;
} /* decode_desc_chunk */

static int
caf_read_header (SF_PRIVATE *psf)
{	CAF_PRIVATE	*pcaf ;
	BUF_UNION	ubuf ;
	DESC_CHUNK desc ;
	sf_count_t chunk_size ;
	double srate ;
	short version, flags ;
	int marker, k, have_data = 0, error ;

	if ((pcaf = psf->container_data) == NULL)
		return SFE_INTERNAL ;

	memset (&desc, 0, sizeof (desc)) ;

	/* Set position to start of file to begin reading header. */
	psf_binheader_readf (psf, "pmE2E2", 0, &marker, &version, &flags) ;
	psf_log_printf (psf, "%M\n  Version : %d\n  Flags   : %x\n", marker, version, flags) ;
	if (marker != caff_MARKER)
		return SFE_CAF_NOT_CAF ;

	psf_binheader_readf (psf, "mE8b", &marker, &chunk_size, ubuf.ucbuf, 8) ;
	srate = double64_be_read (ubuf.ucbuf) ;
	snprintf (ubuf.cbuf, sizeof (ubuf.cbuf), "%5.3f", srate) ;
	psf_log_printf (psf, "%M : %D\n  Sample rate  : %s\n", marker, chunk_size, ubuf.cbuf) ;
	if (marker != desc_MARKER)
		return SFE_CAF_NO_DESC ;

	if (chunk_size < SIGNED_SIZEOF (DESC_CHUNK))
	{	psf_log_printf (psf, "**** Chunk size too small. Should be > 32 bytes.\n") ;
		return SFE_MALFORMED_FILE ;
		} ;

	psf->sf.samplerate = lrint (srate) ;

	psf_binheader_readf (psf, "mE44444", &desc.fmt_id, &desc.fmt_flags, &desc.pkt_bytes, &desc.frames_per_packet,
			&desc.channels_per_frame, &desc.bits_per_chan) ;
	psf_log_printf (psf, "  Format id    : %M\n  Format flags : %x\n  Bytes / packet   : %u\n"
			"  Frames / packet  : %u\n  Channels / frame : %u\n  Bits / channel   : %u\n",
			desc.fmt_id, desc.fmt_flags, desc.pkt_bytes, desc.frames_per_packet, desc.channels_per_frame, desc.bits_per_chan) ;

	if (desc.channels_per_frame > SF_MAX_CHANNELS)
	{	psf_log_printf (psf, "**** Bad channels per frame value %u.\n", desc.channels_per_frame) ;
		return SFE_MALFORMED_FILE ;
		} ;

	if (chunk_size > SIGNED_SIZEOF (DESC_CHUNK))
		psf_binheader_readf (psf, "j", (int) (chunk_size - sizeof (DESC_CHUNK))) ;

	psf->sf.channels = desc.channels_per_frame ;

	while (1)
	{	marker = 0 ;
		chunk_size = 0 ;

		psf_binheader_readf (psf, "mE8", &marker, &chunk_size) ;
		if (marker == 0)
		{	sf_count_t pos = psf_ftell (psf) ;
			psf_log_printf (psf, "Have 0 marker at position %D (0x%x).\n", pos, pos) ;
			break ;
			} ;
		if (chunk_size < 0)
		{	psf_log_printf (psf, "%M : %D *** Should be >= 0 ***\n", marker, chunk_size) ;
			break ;
			} ;
		if (chunk_size > psf->filelength)
			break ;

		psf_store_read_chunk_u32 (&psf->rchunks, marker, psf_ftell (psf), chunk_size) ;

		switch (marker)
		{	case peak_MARKER :
				psf_log_printf (psf, "%M : %D\n", marker, chunk_size) ;
				if (chunk_size != CAF_PEAK_CHUNK_SIZE (psf->sf.channels))
				{	psf_binheader_readf (psf, "j", make_size_t (chunk_size)) ;
					psf_log_printf (psf, "*** File PEAK chunk %D should be %d.\n", chunk_size, CAF_PEAK_CHUNK_SIZE (psf->sf.channels)) ;
					return SFE_CAF_BAD_PEAK ;
					} ;

				if ((psf->peak_info = peak_info_calloc (psf->sf.channels)) == NULL)
					return SFE_MALLOC_FAILED ;

				/* read in rest of PEAK chunk. */
				psf_binheader_readf (psf, "E4", & (psf->peak_info->edit_number)) ;
				psf_log_printf (psf, "  edit count : %d\n", psf->peak_info->edit_number) ;

				psf_log_printf (psf, "     Ch   Position       Value\n") ;
				for (k = 0 ; k < psf->sf.channels ; k++)
				{	sf_count_t position ;
					float value ;

					psf_binheader_readf (psf, "Ef8", &value, &position) ;
					psf->peak_info->peaks [k].value = value ;
					psf->peak_info->peaks [k].position = position ;

					snprintf (ubuf.cbuf, sizeof (ubuf.cbuf), "    %2d   %-12" PRId64 "   %g\n", k, position, value) ;
					psf_log_printf (psf, ubuf.cbuf) ;
					} ;

				psf->peak_info->peak_loc = SF_PEAK_START ;
				break ;

			case chan_MARKER :
				if (chunk_size < 12)
				{	psf_log_printf (psf, "%M : %D (should be >= 12)\n", marker, chunk_size) ;
					psf_binheader_readf (psf, "j", make_size_t (chunk_size)) ;
					break ;
					}

				psf_log_printf (psf, "%M : %D\n", marker, chunk_size) ;

				if ((error = caf_read_chanmap (psf, chunk_size)))
					return error ;
				break ;

			case free_MARKER :
				psf_log_printf (psf, "%M : %D\n", marker, chunk_size) ;
				psf_binheader_readf (psf, "j", make_size_t (chunk_size)) ;
				break ;

			case data_MARKER :
				psf_binheader_readf (psf, "E4", &k) ;
				if (chunk_size == -1)
				{	psf_log_printf (psf, "%M : -1\n") ;
					chunk_size = psf->filelength - psf->header.indx ;
					}
				else if (psf->filelength > 0 && chunk_size > psf->filelength - psf->header.indx + 10)
				{	psf_log_printf (psf, "%M : %D (should be %D)\n", marker, chunk_size, psf->filelength - psf->header.indx - 8) ;
					psf->datalength = psf->filelength - psf->header.indx - 8 ;
					}
				else
				{	psf_log_printf (psf, "%M : %D\n", marker, chunk_size) ;
					/* Subtract the 4 bytes of the 'edit' field above. */
					psf->datalength = chunk_size - 4 ;
					} ;

				psf_log_printf (psf, "  edit : %u\n", k) ;

				psf->dataoffset = psf->header.indx ;
				if (psf->datalength + psf->dataoffset < psf->filelength)
					psf->dataend = psf->datalength + psf->dataoffset ;

				psf_binheader_readf (psf, "j", make_size_t (psf->datalength)) ;
				have_data = 1 ;
				break ;

			case kuki_MARKER :
				psf_log_printf (psf, "%M : %D\n", marker, chunk_size) ;
				pcaf->alac.kuki_offset = psf_ftell (psf) - 12 ;
				psf_binheader_readf (psf, "j", make_size_t (chunk_size)) ;
				break ;

			case pakt_MARKER :
				if (chunk_size < 24)
				{	psf_log_printf (psf, "%M : %D (should be > 24)\n", marker, chunk_size) ;
					return SFE_MALFORMED_FILE ;
					}
				else if (chunk_size > psf->filelength - psf->header.indx)
				{	psf_log_printf (psf, "%M : %D (should be < %D)\n", marker, chunk_size, psf->filelength - psf->header.indx) ;
					return SFE_MALFORMED_FILE ;
					}
				else
					psf_log_printf (psf, "%M : %D\n", marker, chunk_size) ;

				psf_binheader_readf (psf, "E8844", &pcaf->alac.packets, &pcaf->alac.valid_frames,
									&pcaf->alac.priming_frames, &pcaf->alac.remainder_frames) ;

				psf_log_printf (psf,
						"  Packets          : %D\n"
						"  Valid frames     : %D\n"
						"  Priming frames   : %d\n"
						"  Remainder frames : %d\n",
						pcaf->alac.packets, pcaf->alac.valid_frames, pcaf->alac.priming_frames,
						pcaf->alac.remainder_frames
						) ;

				if (pcaf->alac.packets == 0 && pcaf->alac.valid_frames == 0
							&& pcaf->alac.priming_frames == 0 && pcaf->alac.remainder_frames == 0)
					psf_log_printf (psf, "*** 'pakt' chunk header is all zero.\n") ;

				pcaf->alac.pakt_offset = psf_ftell (psf) - 12 ;
				psf_binheader_readf (psf, "j", make_size_t (chunk_size) - 24) ;
				break ;

			case info_MARKER :
				if (chunk_size < 4)
				{	psf_log_printf (psf, "%M : %D (should be > 4)\n", marker, chunk_size) ;
					return SFE_MALFORMED_FILE ;
					}
				else if (chunk_size > psf->filelength - psf->header.indx)
				{	psf_log_printf (psf, "%M : %D (should be < %D)\n", marker, chunk_size, psf->filelength - psf->header.indx) ;
					return SFE_MALFORMED_FILE ;
					} ;
				psf_log_printf (psf, "%M : %D\n", marker, chunk_size) ;
				if (chunk_size > 4)
					caf_read_strings (psf, chunk_size - 4) ;
				break ;

			default :
				psf_log_printf (psf, "%M : %D (skipped)\n", marker, chunk_size) ;
				psf_binheader_readf (psf, "j", make_size_t (chunk_size)) ;
				break ;
			} ;

		if (marker != data_MARKER && chunk_size >= 0xffffff00)
			break ;

		if (! psf->sf.seekable && have_data)
			break ;

		if (psf_ftell (psf) >= psf->filelength - SIGNED_SIZEOF (chunk_size))
		{	psf_log_printf (psf, "End\n") ;
			break ;
			} ;
		} ;

	if (have_data == 0)
	{	psf_log_printf (psf, "**** Error, could not find 'data' chunk.\n") ;
		return SFE_MALFORMED_FILE ;
		} ;

	psf->endian = (desc.fmt_flags & 2) ? SF_ENDIAN_LITTLE : SF_ENDIAN_BIG ;

	psf_fseek (psf, psf->dataoffset, SEEK_SET) ;

	if ((psf->sf.format = decode_desc_chunk (psf, &desc)) == 0)
		return SFE_UNSUPPORTED_ENCODING ;

	if (psf->bytewidth > 0)
		psf->sf.frames = psf->datalength / psf->bytewidth ;

	return 0 ;
} /* caf_read_header */

/*------------------------------------------------------------------------------
*/

static int
caf_write_header (SF_PRIVATE *psf, int calc_length)
{	BUF_UNION	ubuf ;
	CAF_PRIVATE	*pcaf ;
	DESC_CHUNK desc ;
	sf_count_t current ;
	uint32_t uk ;
	int subformat, append_free_block = SF_TRUE ;

	if ((pcaf = psf->container_data) == NULL)
		return SFE_INTERNAL ;

	memset (&desc, 0, sizeof (desc)) ;

	current = psf_ftell (psf) ;

	if (calc_length)
	{	psf->filelength = psf_get_filelen (psf) ;

		psf->datalength = psf->filelength - psf->dataoffset ;

		if (psf->dataend)
			psf->datalength -= psf->filelength - psf->dataend ;

		if (psf->bytewidth > 0)
			psf->sf.frames = psf->datalength / (psf->bytewidth * psf->sf.channels) ;
		} ;

	/* Reset the current header length to zero. */
	psf->header.ptr [0] = 0 ;
	psf->header.indx = 0 ;
	psf_fseek (psf, 0, SEEK_SET) ;

	/* 'caff' marker, version and flags. */
	psf_binheader_writef (psf, "Em22", BHWm (caff_MARKER), BHW2 (1), BHW2 (0)) ;

	/* 'desc' marker and chunk size. */
	psf_binheader_writef (psf, "Em8", BHWm (desc_MARKER), BHW8 ((sf_count_t) (sizeof (DESC_CHUNK)))) ;

 	double64_be_write (1.0 * psf->sf.samplerate, ubuf.ucbuf) ;
	psf_binheader_writef (psf, "b", BHWv (ubuf.ucbuf), BHWz (8)) ;

	subformat = SF_CODEC (psf->sf.format) ;

	psf->endian = SF_ENDIAN (psf->sf.format) ;

	if (CPU_IS_BIG_ENDIAN && (psf->endian == 0 || psf->endian == SF_ENDIAN_CPU))
		psf->endian = SF_ENDIAN_BIG ;
	else if (CPU_IS_LITTLE_ENDIAN && (psf->endian == SF_ENDIAN_LITTLE || psf->endian == SF_ENDIAN_CPU))
		psf->endian = SF_ENDIAN_LITTLE ;

	if (psf->endian == SF_ENDIAN_LITTLE)
		desc.fmt_flags = 2 ;
	else
		psf->endian = SF_ENDIAN_BIG ;

	/* initial section (same for all, it appears) */
	switch (subformat)
	{	case SF_FORMAT_PCM_S8 :
			desc.fmt_id = lpcm_MARKER ;
			psf->bytewidth = 1 ;
			desc.pkt_bytes = psf->bytewidth * psf->sf.channels ;
			desc.frames_per_packet = 1 ;
			desc.channels_per_frame = psf->sf.channels ;
			desc.bits_per_chan = 8 ;
			break ;

		case SF_FORMAT_PCM_16 :
			desc.fmt_id = lpcm_MARKER ;
			psf->bytewidth = 2 ;
			desc.pkt_bytes = psf->bytewidth * psf->sf.channels ;
			desc.frames_per_packet = 1 ;
			desc.channels_per_frame = psf->sf.channels ;
			desc.bits_per_chan = 16 ;
			break ;

		case SF_FORMAT_PCM_24 :
			psf->bytewidth = 3 ;
			desc.pkt_bytes = psf->bytewidth * psf->sf.channels ;
			desc.frames_per_packet = 1 ;
			desc.channels_per_frame = psf->sf.channels ;
			desc.bits_per_chan = 24 ;
			desc.fmt_id = lpcm_MARKER ;
			break ;

		case SF_FORMAT_PCM_32 :
			desc.fmt_id = lpcm_MARKER ;
			psf->bytewidth = 4 ;
			desc.pkt_bytes = psf->bytewidth * psf->sf.channels ;
			desc.frames_per_packet = 1 ;
			desc.channels_per_frame = psf->sf.channels ;
			desc.bits_per_chan = 32 ;
			break ;

		case SF_FORMAT_FLOAT :
			desc.fmt_id = lpcm_MARKER ;
			desc.fmt_flags |= 1 ;
			psf->bytewidth = 4 ;
			desc.pkt_bytes = psf->bytewidth * psf->sf.channels ;
			desc.frames_per_packet = 1 ;
			desc.channels_per_frame = psf->sf.channels ;
			desc.bits_per_chan = 32 ;
			break ;

		case SF_FORMAT_DOUBLE :
			desc.fmt_id = lpcm_MARKER ;
			desc.fmt_flags |= 1 ;
			psf->bytewidth = 8 ;
			desc.pkt_bytes = psf->bytewidth * psf->sf.channels ;
			desc.frames_per_packet = 1 ;
			desc.channels_per_frame = psf->sf.channels ;
			desc.bits_per_chan = 64 ;
			break ;

		case SF_FORMAT_ALAW :
			desc.fmt_id = alaw_MARKER ;
			psf->bytewidth = 1 ;
			desc.pkt_bytes = psf->bytewidth * psf->sf.channels ;
			desc.frames_per_packet = 1 ;
			desc.channels_per_frame = psf->sf.channels ;
			desc.bits_per_chan = 8 ;
			break ;

		case SF_FORMAT_ULAW :
			desc.fmt_id = ulaw_MARKER ;
			psf->bytewidth = 1 ;
			desc.pkt_bytes = psf->bytewidth * psf->sf.channels ;
			desc.frames_per_packet = 1 ;
			desc.channels_per_frame = psf->sf.channels ;
			desc.bits_per_chan = 8 ;
			break ;

		case SF_FORMAT_ALAC_16 :
		case SF_FORMAT_ALAC_20 :
		case SF_FORMAT_ALAC_24 :
		case SF_FORMAT_ALAC_32 :
			desc.fmt_id = alac_MARKER ;
			desc.pkt_bytes = psf->bytewidth * psf->sf.channels ;
			desc.channels_per_frame = psf->sf.channels ;
			alac_get_desc_chunk_items (subformat, &desc.fmt_flags, &desc.frames_per_packet) ;
			append_free_block = SF_FALSE ;
			break ;

		default :
			return SFE_UNIMPLEMENTED ;
		} ;

	psf_binheader_writef (psf, "mE44444", BHWm (desc.fmt_id), BHW4 (desc.fmt_flags), BHW4 (desc.pkt_bytes), BHW4 (desc.frames_per_packet), BHW4 (desc.channels_per_frame), BHW4 (desc.bits_per_chan)) ;

	caf_write_strings (psf, SF_STR_LOCATE_START) ;

	if (psf->peak_info != NULL)
	{	int k ;
		psf_binheader_writef (psf, "Em84", BHWm (peak_MARKER), BHW8 ((sf_count_t) CAF_PEAK_CHUNK_SIZE (psf->sf.channels)), BHW4 (psf->peak_info->edit_number)) ;
		for (k = 0 ; k < psf->sf.channels ; k++)
			psf_binheader_writef (psf, "Ef8", BHWf ((float) psf->peak_info->peaks [k].value), BHW8 (psf->peak_info->peaks [k].position)) ;
		} ;

	if (psf->channel_map && pcaf->chanmap_tag)
		psf_binheader_writef (psf, "Em8444", BHWm (chan_MARKER), BHW8 ((sf_count_t) 12), BHW4 (pcaf->chanmap_tag), BHW4 (0), BHW4 (0)) ;

	/* Write custom headers. */
	for (uk = 0 ; uk < psf->wchunks.used ; uk++)
		psf_binheader_writef (psf, "m44b", BHWm ((int) psf->wchunks.chunks [uk].mark32), BHW4 (0), BHW4 (psf->wchunks.chunks [uk].len), BHWv (psf->wchunks.chunks [uk].data), BHWz (psf->wchunks.chunks [uk].len)) ;

	if (append_free_block)
	{	/* Add free chunk so that the actual audio data starts at a multiple 0x1000. */
		sf_count_t free_len = 0x1000 - psf->header.indx - 16 - 12 ;
		while (free_len < 0)
			free_len += 0x1000 ;
		psf_binheader_writef (psf, "Em8z", BHWm (free_MARKER), BHW8 (free_len), BHWz (free_len)) ;
		} ;

	psf_binheader_writef (psf, "Em84", BHWm (data_MARKER), BHW8 (psf->datalength + 4), BHW4 (0)) ;

	psf_fwrite (psf->header.ptr, psf->header.indx, 1, psf) ;
	if (psf->error)
		return psf->error ;

	psf->dataoffset = psf->header.indx ;
	if (current < psf->dataoffset)
		psf_fseek (psf, psf->dataoffset, SEEK_SET) ;
	else if (current > 0)
		psf_fseek (psf, current, SEEK_SET) ;

	return psf->error ;
} /* caf_write_header */

static int
caf_write_tailer (SF_PRIVATE *psf)
{
	/* Reset the current header buffer length to zero. */
	psf->header.ptr [0] = 0 ;
	psf->header.indx = 0 ;

	if (psf->bytewidth > 0 && psf->sf.seekable == SF_TRUE)
	{	psf->datalength = psf->sf.frames * psf->bytewidth * psf->sf.channels ;
		psf->dataend = psf->dataoffset + psf->datalength ;
		} ;

	if (psf->dataend > 0)
		psf_fseek (psf, psf->dataend, SEEK_SET) ;
	else
		psf->dataend = psf_fseek (psf, 0, SEEK_END) ;

	if (psf->dataend & 1)
		psf_binheader_writef (psf, "z", BHWz (1)) ;

	if (psf->strings.flags & SF_STR_LOCATE_END)
		caf_write_strings (psf, SF_STR_LOCATE_END) ;

	/* Write the tailer. */
	if (psf->header.indx > 0)
		psf_fwrite (psf->header.ptr, psf->header.indx, 1, psf) ;

	return 0 ;
} /* caf_write_tailer */

static int
caf_read_chanmap (SF_PRIVATE * psf, sf_count_t chunk_size)
{	const AIFF_CAF_CHANNEL_MAP * map_info ;
	unsigned channel_bitmap, channel_decriptions, bytesread ;
	int layout_tag ;

	bytesread = psf_binheader_readf (psf, "E444", &layout_tag, &channel_bitmap, &channel_decriptions) ;

	map_info = aiff_caf_of_channel_layout_tag (layout_tag) ;

	psf_log_printf (psf, "  Tag    : %x\n", layout_tag) ;
	if (map_info)
		psf_log_printf (psf, "  Layout : %s\n", map_info->name) ;

	if (bytesread < chunk_size)
		psf_binheader_readf (psf, "j", chunk_size - bytesread) ;

	if (map_info && map_info->channel_map != NULL)
	{	size_t chanmap_size = SF_MIN (psf->sf.channels, layout_tag & 0xff) * sizeof (psf->channel_map [0]) ;

		free (psf->channel_map) ;

		if ((psf->channel_map = malloc (chanmap_size)) == NULL)
			return SFE_MALLOC_FAILED ;

		memcpy (psf->channel_map, map_info->channel_map, chanmap_size) ;
		} ;

	return 0 ;
} /* caf_read_chanmap */


static uint32_t
string_hash32 (const char * str)
{	uint32_t hash = 0x87654321 ;

	while (str [0])
	{	hash = hash * 333 + str [0] ;
		str ++ ;
		} ;

	return hash ;
} /* string_hash32 */

static int
caf_read_strings (SF_PRIVATE * psf, sf_count_t chunk_size)
{	char *buf ;
	char *key, *value ;
	uint32_t count, hash ;

	if ((buf = malloc (chunk_size + 1)) == NULL)
		return (psf->error = SFE_MALLOC_FAILED) ;

	psf_binheader_readf (psf, "E4b", &count, buf, make_size_t (chunk_size)) ;
	psf_log_printf (psf, " count: %u\n", count) ;

	/* Force terminate `buf` to make sure. */
	buf [chunk_size] = 0 ;

	for (key = buf ; key < buf + chunk_size ; )
	{	value = key + strlen (key) + 1 ;
		if (value > buf + chunk_size)
			break ;
		psf_log_printf (psf, "   %-12s : %s\n", key, value) ;

		hash = string_hash32 (key) ;
		switch (hash)
		{	case 0xC4861943 : /* 'title' */
				psf_store_string (psf, SF_STR_TITLE, value) ;
				break ;
			case 0xAD47A394 : /* 'software' */
				psf_store_string (psf, SF_STR_SOFTWARE, value) ;
				break ;
			case 0x5D178E2A : /* 'copyright' */
				psf_store_string (psf, SF_STR_COPYRIGHT, value) ;
				break ;
			case 0x60E4D0C8 : /* 'artist' */
				psf_store_string (psf, SF_STR_ARTIST, value) ;
				break ;
			case 0x83B5D16A : /* 'genre' */
				psf_store_string (psf, SF_STR_GENRE, value) ;
				break ;
			case 0x15E5FC88 : /* 'comment' */
			case 0x7C297D5B : /* 'comments' */
				psf_store_string (psf, SF_STR_COMMENT, value) ;
				break ;
			case 0x24A7C347 : /* 'tracknumber' */
				psf_store_string (psf, SF_STR_TRACKNUMBER, value) ;
				break ;
			case 0x50A31EB7 : /* 'date' */
				psf_store_string (psf, SF_STR_DATE, value) ;
				break ;
			case 0x6583545A : /* 'album' */
				psf_store_string (psf, SF_STR_ALBUM, value) ;
				break ;
			case 0xE7C64B6C : /* 'license' */
				psf_store_string (psf, SF_STR_LICENSE, value) ;
				break ;
			default :
				psf_log_printf (psf, " Unhandled hash 0x%x : /* '%s' */\n", hash, key) ;
				break ;
			} ;

		key = value + strlen (value) + 1 ;
		} ;

	free (buf) ;

	return 0 ;
} /* caf_read_strings */

struct put_buffer
{	uint32_t index ;
	char s [16 * 1024] ;
} ;

static uint32_t
put_key_value (struct put_buffer * buf, const char * key, const char * value)
{	uint32_t written ;

	if (buf->index + strlen (key) + strlen (value) + 2 > sizeof (buf->s))
		return 0 ;

	written = snprintf (buf->s + buf->index, sizeof (buf->s) - buf->index, "%s%c%s%c", key, 0, value, 0) ;

	if (buf->index + written >= sizeof (buf->s))
		return 0 ;

	buf->index += written ;
	return 1 ;
} /* put_key_value */

static void
caf_write_strings (SF_PRIVATE * psf, int location)
{	struct put_buffer buf ;
 	const char * cptr ;
	uint32_t k, string_count = 0 ;

	memset (&buf, 0, sizeof (buf)) ;

	for (k = 0 ; k < SF_MAX_STRINGS ; k++)
	{	if (psf->strings.data [k].type == 0)
			break ;

		if (psf->strings.data [k].flags != location)
			continue ;

		if ((cptr = psf_get_string (psf, psf->strings.data [k].type)) == NULL)
			continue ;

		switch (psf->strings.data [k].type)
		{	case SF_STR_TITLE :
				string_count += put_key_value (&buf, "title", cptr) ;
				break ;
			case SF_STR_COPYRIGHT :
				string_count += put_key_value (&buf, "copyright", cptr) ;
				break ;
			case SF_STR_SOFTWARE :
				string_count += put_key_value (&buf, "software", cptr) ;
				break ;
			case SF_STR_ARTIST :
				string_count += put_key_value (&buf, "artist", cptr) ;
				break ;
			case SF_STR_COMMENT :
				string_count += put_key_value (&buf, "comment", cptr) ;
				break ;
			case SF_STR_DATE :
				string_count += put_key_value (&buf, "date", cptr) ;
				break ;
			case SF_STR_ALBUM :
				string_count += put_key_value (&buf, "album", cptr) ;
				break ;
			case SF_STR_LICENSE :
				string_count += put_key_value (&buf, "license", cptr) ;
				break ;
			case SF_STR_TRACKNUMBER :
				string_count += put_key_value (&buf, "tracknumber", cptr) ;
				break ;
			case SF_STR_GENRE :
				string_count += put_key_value (&buf, "genre", cptr) ;
				break ;

			default :
				break ;
			} ;
		} ;

	if (string_count == 0 || buf.index == 0)
		return ;

	psf_binheader_writef (psf, "Em84b", BHWm (info_MARKER), BHW8 (buf.index + 4), BHW4 (string_count), BHWv (buf.s), BHWz (buf.index)) ;
} /* caf_write_strings */

/*==============================================================================
*/

static int
caf_set_chunk (SF_PRIVATE *psf, const SF_CHUNK_INFO * chunk_info)
{	return psf_save_write_chunk (&psf->wchunks, chunk_info) ;
} /* caf_set_chunk */

static SF_CHUNK_ITERATOR *
caf_next_chunk_iterator (SF_PRIVATE *psf, SF_CHUNK_ITERATOR * iterator)
{	return psf_next_chunk_iterator (&psf->rchunks, iterator) ;
} /* caf_next_chunk_iterator */

static int
caf_get_chunk_size (SF_PRIVATE *psf, const SF_CHUNK_ITERATOR * iterator, SF_CHUNK_INFO * chunk_info)
{	int indx ;

	if ((indx = psf_find_read_chunk_iterator (&psf->rchunks, iterator)) < 0)
		return SFE_UNKNOWN_CHUNK ;

	chunk_info->datalen = psf->rchunks.chunks [indx].len ;

	return SFE_NO_ERROR ;
} /* caf_get_chunk_size */

static int
caf_get_chunk_data (SF_PRIVATE *psf, const SF_CHUNK_ITERATOR * iterator, SF_CHUNK_INFO * chunk_info)
{	int indx ;
	sf_count_t pos ;

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
} /* caf_get_chunk_data */
