/*
** Copyright (C) 2005-2009 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#include	"sndfile.h"
#include	"sfendian.h"
#include	"common.h"

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
{	unsigned char srate [8] ;
	unsigned int fmt_id ;
	unsigned int fmt_flags ;
	unsigned int pkt_bytes ;
	unsigned int pkt_frames ;
	unsigned int channels_per_frame ;
	unsigned int bits_per_chan ;
} DESC_CHUNK ;

/*------------------------------------------------------------------------------
** Private static functions.
*/

static int	caf_close (SF_PRIVATE *psf) ;
static int	caf_read_header (SF_PRIVATE *psf) ;
static int	caf_write_header (SF_PRIVATE *psf, int calc_length) ;

/*------------------------------------------------------------------------------
** Public function.
*/

int
caf_open (SF_PRIVATE *psf)
{	int	subformat, format, error = 0 ;

	if (psf->mode == SFM_READ || (psf->mode == SFM_RDWR && psf->filelength > 0))
	{	if ((error = caf_read_header (psf)))
			return error ;
		} ;

	subformat = SF_CODEC (psf->sf.format) ;

	if (psf->mode == SFM_WRITE || psf->mode == SFM_RDWR)
	{	if (psf->is_pipe)
			return SFE_NO_PIPE_WRITE ;

		format = SF_CONTAINER (psf->sf.format) ;
		if (format != SF_FORMAT_CAF)
			return	SFE_BAD_OPEN_FORMAT ;

		psf->blockwidth = psf->bytewidth * psf->sf.channels ;

		if (psf->mode != SFM_RDWR || psf->filelength < 44)
		{	psf->filelength = 0 ;
			psf->datalength = 0 ;
			psf->dataoffset = 0 ;
			psf->sf.frames = 0 ;
			} ;

		psf->str_flags = SF_STR_ALLOW_START ;

		/*
		**	By default, add the peak chunk to floating point files. Default behaviour
		**	can be switched off using sf_command (SFC_SET_PEAK_CHUNK, SF_FALSE).
		*/
		if (psf->mode == SFM_WRITE && (subformat == SF_FORMAT_FLOAT || subformat == SF_FORMAT_DOUBLE))
		{	if ((psf->peak_info = peak_info_calloc (psf->sf.channels)) == NULL)
				return SFE_MALLOC_FAILED ;
			psf->peak_info->peak_loc = SF_PEAK_START ;
			} ;

		if ((error = caf_write_header (psf, SF_FALSE)) != 0)
			return error ;

		psf->write_header = caf_write_header ;
		} ;

	psf->container_close = caf_close ;
	/*psf->command = caf_command ;*/

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
		/* Lite remove end */

		default :
			return SFE_UNSUPPORTED_ENCODING ;
		} ;

	return error ;
} /* caf_open */

static int
caf_close (SF_PRIVATE *psf)
{
	if (psf->mode == SFM_WRITE || psf->mode == SFM_RDWR)
		caf_write_header (psf, SF_TRUE) ;

	return 0 ;
} /* caf_close */

/*------------------------------------------------------------------------------
*/

static int
decode_desc_chunk (SF_PRIVATE *psf, const DESC_CHUNK *desc)
{	int format ;

	psf->sf.channels = desc->channels_per_frame ;

	format = SF_FORMAT_CAF | (psf->endian == SF_ENDIAN_LITTLE ? SF_ENDIAN_LITTLE : 0) ;

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

	if ((desc->fmt_flags & 1) != 0)
	{	psf_log_printf (psf, "**** Ooops, 'desc' chunk suggests float data, but other info invalid.\n") ;
		return 0 ;
		} ;

	if (desc->fmt_id == lpcm_MARKER)
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

	return 0 ;
} /* decode_desc_chunk */

static int
caf_read_header (SF_PRIVATE *psf)
{	DESC_CHUNK desc ;
	sf_count_t chunk_size ;
	double srate ;
	short version, flags ;
	int marker, k, have_data = 0 ;

	memset (&desc, 0, sizeof (desc)) ;

	/* Set position to start of file to begin reading header. */
	psf_binheader_readf (psf, "pmE2E2", 0, &marker, &version, &flags) ;
	psf_log_printf (psf, "%M\n  Version : %d\n  Flags   : %x\n", marker, version, flags) ;
	if (marker != caff_MARKER)
		return SFE_CAF_NOT_CAF ;

	psf_binheader_readf (psf, "mE8b", &marker, &chunk_size, psf->u.ucbuf, 8) ;
	srate = double64_be_read (psf->u.ucbuf) ;
	LSF_SNPRINTF (psf->u.cbuf, sizeof (psf->u.cbuf), "%5.3f", srate) ;
	psf_log_printf (psf, "%M : %D\n  Sample rate  : %s\n", marker, chunk_size, psf->u.cbuf) ;
	if (marker != desc_MARKER)
		return SFE_CAF_NO_DESC ;

	if (chunk_size < SIGNED_SIZEOF (DESC_CHUNK))
	{	psf_log_printf (psf, "**** Chunk size too small. Should be > 32 bytes.\n") ;
		return SFE_MALFORMED_FILE ;
		} ;

	psf->sf.samplerate = lrint (srate) ;

	psf_binheader_readf (psf, "mE44444", &desc.fmt_id, &desc.fmt_flags, &desc.pkt_bytes, &desc.pkt_frames,
			&desc.channels_per_frame, &desc.bits_per_chan) ;
	psf_log_printf (psf, "  Format id    : %M\n  Format flags : %x\n  Bytes / packet   : %u\n"
			"  Frames / packet  : %u\n  Channels / frame : %u\n  Bits / channel   : %u\n",
			desc.fmt_id, desc.fmt_flags, desc.pkt_bytes, desc.pkt_frames, desc.channels_per_frame, desc.bits_per_chan) ;

	if (chunk_size > SIGNED_SIZEOF (DESC_CHUNK))
		psf_binheader_readf (psf, "j", (int) (chunk_size - sizeof (DESC_CHUNK))) ;

	psf->sf.channels = desc.channels_per_frame ;

	while (have_data == 0 && psf_ftell (psf) < psf->filelength - SIGNED_SIZEOF (marker))
	{	psf_binheader_readf (psf, "mE8", &marker, &chunk_size) ;

		switch (marker)
		{	case peak_MARKER :
				psf_log_printf (psf, "%M : %D\n", marker, chunk_size) ;
				if (chunk_size != CAF_PEAK_CHUNK_SIZE (psf->sf.channels))
				{	psf_binheader_readf (psf, "j", (int) chunk_size) ;
					psf_log_printf (psf, "*** File PEAK chunk %D should be %d.\n", chunk_size, CAF_PEAK_CHUNK_SIZE (psf->sf.channels)) ;
					return SFE_CAF_BAD_PEAK ;
					} ;

				if ((psf->peak_info = peak_info_calloc (psf->sf.channels)) == NULL)
					return SFE_MALLOC_FAILED ;

				/* read in rest of PEAK chunk. */
				psf_binheader_readf (psf, "E4", & (psf->peak_info->edit_number)) ;
				psf_log_printf (psf, "  edit count : %d\n", psf->peak_info->edit_number) ;

				psf_log_printf (psf, "     Ch   Position      Value\n") ;
				for (k = 0 ; k < psf->sf.channels ; k++)
				{	sf_count_t position ;
					float value ;

					psf_binheader_readf (psf, "Ef8", &value, &position) ;
					psf->peak_info->peaks [k].value = value ;
					psf->peak_info->peaks [k].position = position ;

					LSF_SNPRINTF (psf->u.cbuf, sizeof (psf->u.cbuf), "    %2d   %-12ld   %g\n", k, (long) position, value) ;
					psf_log_printf (psf, psf->u.cbuf) ;
					} ;

				psf->peak_info->peak_loc = SF_PEAK_START ;
				break ;

			case free_MARKER :
				psf_log_printf (psf, "%M : %D\n", marker, chunk_size) ;
				psf_binheader_readf (psf, "j", (int) chunk_size) ;
				break ;

			case data_MARKER :
				psf_log_printf (psf, "%M : %D\n", marker, chunk_size) ;
				psf_binheader_readf (psf, "E4", &k) ;
				psf_log_printf (psf, "  edit : %u\n", k) ;
				have_data = 1 ;
				break ;

			default :
				psf_log_printf (psf, " %M : %D (skipped)\n", marker, chunk_size) ;
				psf_binheader_readf (psf, "j", (int) chunk_size) ;
				break ;
			} ;
		} ;

	if (have_data == 0)
	{	psf_log_printf (psf, "**** Error, could not find 'data' chunk.\n") ;
		return SFE_MALFORMED_FILE ;
		} ;

	psf_log_printf (psf, "End\n") ;

	psf->dataoffset = psf_ftell (psf) ;
	psf->datalength = psf->filelength - psf->dataoffset ;
	psf->endian = (desc.fmt_flags & 2) ? SF_ENDIAN_LITTLE : SF_ENDIAN_BIG ;

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
{	DESC_CHUNK desc ;
	sf_count_t current, free_len ;
	int subformat ;

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
	psf->header [0] = 0 ;
	psf->headindex = 0 ;
	psf_fseek (psf, 0, SEEK_SET) ;

	/* 'caff' marker, version and flags. */
	psf_binheader_writef (psf, "Em22", caff_MARKER, 1, 0) ;

	/* 'desc' marker and chunk size. */
	psf_binheader_writef (psf, "Em8", desc_MARKER, (sf_count_t) (sizeof (DESC_CHUNK))) ;

 	double64_be_write (1.0 * psf->sf.samplerate, psf->u.ucbuf) ;
	psf_binheader_writef (psf, "b", psf->u.ucbuf, make_size_t (8)) ;

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
			desc.pkt_frames = 1 ;
			desc.channels_per_frame = psf->sf.channels ;
			desc.bits_per_chan = 8 ;
			break ;

		case SF_FORMAT_PCM_16 :
			desc.fmt_id = lpcm_MARKER ;
			psf->bytewidth = 2 ;
			desc.pkt_bytes = psf->bytewidth * psf->sf.channels ;
			desc.pkt_frames = 1 ;
			desc.channels_per_frame = psf->sf.channels ;
			desc.bits_per_chan = 16 ;
			break ;

		case SF_FORMAT_PCM_24 :
			psf->bytewidth = 3 ;
			desc.pkt_bytes = psf->bytewidth * psf->sf.channels ;
			desc.pkt_frames = 1 ;
			desc.channels_per_frame = psf->sf.channels ;
			desc.bits_per_chan = 24 ;
			desc.fmt_id = lpcm_MARKER ;
			break ;

		case SF_FORMAT_PCM_32 :
			desc.fmt_id = lpcm_MARKER ;
			psf->bytewidth = 4 ;
			desc.pkt_bytes = psf->bytewidth * psf->sf.channels ;
			desc.pkt_frames = 1 ;
			desc.channels_per_frame = psf->sf.channels ;
			desc.bits_per_chan = 32 ;
			break ;

		case SF_FORMAT_FLOAT :
			desc.fmt_id = lpcm_MARKER ;
			desc.fmt_flags |= 1 ;
			psf->bytewidth = 4 ;
			desc.pkt_bytes = psf->bytewidth * psf->sf.channels ;
			desc.pkt_frames = 1 ;
			desc.channels_per_frame = psf->sf.channels ;
			desc.bits_per_chan = 32 ;
			break ;

		case SF_FORMAT_DOUBLE :
			desc.fmt_id = lpcm_MARKER ;
			desc.fmt_flags |= 1 ;
			psf->bytewidth = 8 ;
			desc.pkt_bytes = psf->bytewidth * psf->sf.channels ;
			desc.pkt_frames = 1 ;
			desc.channels_per_frame = psf->sf.channels ;
			desc.bits_per_chan = 64 ;
			break ;

		case SF_FORMAT_ALAW :
			desc.fmt_id = alaw_MARKER ;
			psf->bytewidth = 1 ;
			desc.pkt_bytes = psf->bytewidth * psf->sf.channels ;
			desc.pkt_frames = 1 ;
			desc.channels_per_frame = psf->sf.channels ;
			desc.bits_per_chan = 8 ;
			break ;

		case SF_FORMAT_ULAW :
			desc.fmt_id = ulaw_MARKER ;
			psf->bytewidth = 1 ;
			desc.pkt_bytes = psf->bytewidth * psf->sf.channels ;
			desc.pkt_frames = 1 ;
			desc.channels_per_frame = psf->sf.channels ;
			desc.bits_per_chan = 8 ;
			break ;

		default :
			return SFE_UNIMPLEMENTED ;
		} ;

	psf_binheader_writef (psf, "mE44444", desc.fmt_id, desc.fmt_flags, desc.pkt_bytes, desc.pkt_frames, desc.channels_per_frame, desc.bits_per_chan) ;

#if 0
	if (psf->str_flags & SF_STR_LOCATE_START)
		caf_write_strings (psf, SF_STR_LOCATE_START) ;
#endif

	if (psf->peak_info != NULL)
	{	int k ;
		psf_binheader_writef (psf, "Em84", peak_MARKER, (sf_count_t) CAF_PEAK_CHUNK_SIZE (psf->sf.channels), psf->peak_info->edit_number) ;
		for (k = 0 ; k < psf->sf.channels ; k++)
			psf_binheader_writef (psf, "Ef8", (float) psf->peak_info->peaks [k].value, psf->peak_info->peaks [k].position) ;
		} ;

	/* Add free chunk so that the actual audio data starts at a multiple 0x1000. */
	free_len = 0x1000 - psf->headindex - 16 - 12 ;
	while (free_len < 0)
		free_len += 0x1000 ;
	psf_binheader_writef (psf, "Em8z", free_MARKER, free_len, (int) free_len) ;

	psf_binheader_writef (psf, "Em84", data_MARKER, psf->datalength, 0) ;

	psf_fwrite (psf->header, psf->headindex, 1, psf) ;
	if (psf->error)
		return psf->error ;

	psf->dataoffset = psf->headindex ;
	if (current < psf->dataoffset)
		psf_fseek (psf, psf->dataoffset, SEEK_SET) ;
	else if (current > 0)
		psf_fseek (psf, current, SEEK_SET) ;

	return psf->error ;
} /* caf_write_header */

