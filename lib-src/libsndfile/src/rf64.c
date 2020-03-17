/*
** Copyright (C) 2008-2018 Erik de Castro Lopo <erikd@mega-nerd.com>
** Copyright (C) 2009      Uli Franke <cls@nebadje.org>
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
**
**	But this may be a better reference:
**	http://www.ebu.ch/CMSimages/en/tec_doc_t3306-2007_tcm6-42570.pdf
*/

#include	"sfconfig.h"

#include	<stdio.h>
#include	<fcntl.h>
#include	<string.h>
#include	<ctype.h>

#include	"sndfile.h"
#include	"sfendian.h"
#include	"common.h"
#include	"wavlike.h"

/*------------------------------------------------------------------------------
** Macros to handle big/little endian issues.
*/
#define	RF64_MARKER		MAKE_MARKER ('R', 'F', '6', '4')
#define	RIFF_MARKER		MAKE_MARKER ('R', 'I', 'F', 'F')
#define	JUNK_MARKER		MAKE_MARKER ('J', 'U', 'N', 'K')
#define	FFFF_MARKER		MAKE_MARKER (0xff, 0xff, 0xff, 0xff)
#define	WAVE_MARKER		MAKE_MARKER ('W', 'A', 'V', 'E')
#define	ds64_MARKER		MAKE_MARKER ('d', 's', '6', '4')
#define	fmt_MARKER		MAKE_MARKER ('f', 'm', 't', ' ')
#define	fact_MARKER		MAKE_MARKER ('f', 'a', 'c', 't')
#define	data_MARKER		MAKE_MARKER ('d', 'a', 't', 'a')

#define bext_MARKER		MAKE_MARKER ('b', 'e', 'x', 't')
#define cart_MARKER		MAKE_MARKER ('c', 'a', 'r', 't')
#define OggS_MARKER		MAKE_MARKER ('O', 'g', 'g', 'S')
#define wvpk_MARKER		MAKE_MARKER ('w', 'v', 'p', 'k')
#define LIST_MARKER		MAKE_MARKER ('L', 'I', 'S', 'T')

/*
** The file size limit in bytes below which we can, if requested, write this
** file as a RIFF/WAVE file.
*/

#define RIFF_DOWNGRADE_BYTES	((sf_count_t) 0xffffffff)

/*------------------------------------------------------------------------------
** Private static functions.
*/

static int	rf64_read_header (SF_PRIVATE *psf, int *blockalign, int *framesperblock) ;
static int	rf64_write_header (SF_PRIVATE *psf, int calc_length) ;
static int	rf64_write_tailer (SF_PRIVATE *psf) ;
static int	rf64_close (SF_PRIVATE *psf) ;
static int	rf64_command (SF_PRIVATE *psf, int command, void * UNUSED (data), int datasize) ;

static int rf64_set_chunk (SF_PRIVATE *psf, const SF_CHUNK_INFO * chunk_info) ;
static SF_CHUNK_ITERATOR * rf64_next_chunk_iterator (SF_PRIVATE *psf, SF_CHUNK_ITERATOR * iterator) ;
static int rf64_get_chunk_size (SF_PRIVATE *psf, const SF_CHUNK_ITERATOR * iterator, SF_CHUNK_INFO * chunk_info) ;
static int rf64_get_chunk_data (SF_PRIVATE *psf, const SF_CHUNK_ITERATOR * iterator, SF_CHUNK_INFO * chunk_info) ;

/*------------------------------------------------------------------------------
** Public function.
*/

int
rf64_open (SF_PRIVATE *psf)
{	WAVLIKE_PRIVATE *wpriv ;
	int	subformat, error = 0 ;
	int blockalign, framesperblock ;

	if ((wpriv = calloc (1, sizeof (WAVLIKE_PRIVATE))) == NULL)
		return SFE_MALLOC_FAILED ;
	psf->container_data = wpriv ;
	wpriv->wavex_ambisonic = SF_AMBISONIC_NONE ;

	/* All RF64 files are little endian. */
	psf->endian = SF_ENDIAN_LITTLE ;

	psf->strings.flags = SF_STR_ALLOW_START | SF_STR_ALLOW_END ;

	if (psf->file.mode == SFM_READ || (psf->file.mode == SFM_RDWR && psf->filelength > 0))
	{	if ((error = rf64_read_header (psf, &blockalign, &framesperblock)) != 0)
			return error ;

		psf->next_chunk_iterator = rf64_next_chunk_iterator ;
		psf->get_chunk_size = rf64_get_chunk_size ;
		psf->get_chunk_data = rf64_get_chunk_data ;
		} ;

	if ((psf->sf.format & SF_FORMAT_TYPEMASK) != SF_FORMAT_RF64)
		return	SFE_BAD_OPEN_FORMAT ;

	subformat = psf->sf.format & SF_FORMAT_SUBMASK ;

	if (psf->file.mode == SFM_WRITE || psf->file.mode == SFM_RDWR)
	{	if (psf->is_pipe)
			return SFE_NO_PIPE_WRITE ;

		psf->blockwidth = psf->bytewidth * psf->sf.channels ;

		if ((error = rf64_write_header (psf, SF_FALSE)))
			return error ;

		psf->write_header = rf64_write_header ;
		psf->set_chunk = rf64_set_chunk ;
		} ;

	psf->container_close = rf64_close ;
	psf->command = rf64_command ;

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
enum
{	HAVE_ds64	= 0x01,
	HAVE_fmt	= 0x02,
	HAVE_bext	= 0x04,
	HAVE_data	= 0x08,
	HAVE_cart	= 0x10,
	HAVE_PEAK	= 0x20,
	HAVE_other	= 0x40
} ;

#define HAVE_CHUNK(CHUNK)	((parsestage & CHUNK) != 0)

static int
rf64_read_header (SF_PRIVATE *psf, int *blockalign, int *framesperblock)
{	WAVLIKE_PRIVATE	*wpriv ;
	WAV_FMT		*wav_fmt ;
	sf_count_t riff_size = 0, frame_count = 0, ds64_datalength = 0 ;
	uint32_t marks [2], marker, chunk_size, parsestage = 0 ;
	int error, done = 0, format = 0 ;

	if ((wpriv = psf->container_data) == NULL)
		return SFE_INTERNAL ;
	wav_fmt = &wpriv->wav_fmt ;

	/* Set position to start of file to begin reading header. */
	psf_binheader_readf (psf, "pmmm", 0, &marker, marks, marks + 1) ;
	if (marker != RF64_MARKER || marks [1] != WAVE_MARKER)
		return SFE_RF64_NOT_RF64 ;

	if (marks [0] == FFFF_MARKER)
		psf_log_printf (psf, "%M\n  %M\n", RF64_MARKER, WAVE_MARKER) ;
	else
		psf_log_printf (psf, "%M : 0x%x (should be 0xFFFFFFFF)\n  %M\n", RF64_MARKER, WAVE_MARKER) ;

	while (NOT (done))
	{
		marker = chunk_size = 0 ;
		psf_binheader_readf (psf, "em4", &marker, &chunk_size) ;

		if (marker == 0)
		{	sf_count_t pos = psf_ftell (psf) ;
			psf_log_printf (psf, "Have 0 marker at position %D (0x%x).\n", pos, pos) ;
			break ;
			} ;

		psf_store_read_chunk_u32 (&psf->rchunks, marker, psf_ftell (psf), chunk_size) ;

		switch (marker)
		{	case ds64_MARKER :
				if (parsestage & HAVE_ds64)
				{	psf_log_printf (psf, "*** Second 'ds64' chunk?\n") ;
					break ;
					} ;

				{	unsigned int table_len, bytesread ;

					/* Read ds64 sizes (3 8-byte words). */
					bytesread = psf_binheader_readf (psf, "888", &riff_size, &ds64_datalength, &frame_count) ;

					/* Read table length. */
					bytesread += psf_binheader_readf (psf, "4", &table_len) ;
					/* Skip table for now. (this was "table_len + 4", why?) */
					bytesread += psf_binheader_readf (psf, "j", table_len) ;

					if (chunk_size == bytesread)
						psf_log_printf (psf, "%M : %u\n", marker, chunk_size) ;
					else if (chunk_size >= bytesread + 4)
					{	unsigned int next ;
						psf_binheader_readf (psf, "m", &next) ;
						if (next == fmt_MARKER)
						{	psf_log_printf (psf, "%M : %u (should be %u)\n", marker, chunk_size, bytesread) ;
							psf_binheader_readf (psf, "j", -4) ;
							}
						else
						{	psf_log_printf (psf, "%M : %u\n", marker, chunk_size) ;
							psf_binheader_readf (psf, "j", chunk_size - bytesread - 4) ;
							} ;
						} ;

					if (psf->filelength != riff_size + 8)
						psf_log_printf (psf, "  Riff size : %D (should be %D)\n", riff_size, psf->filelength - 8) ;
					else
						psf_log_printf (psf, "  Riff size : %D\n", riff_size) ;

					psf_log_printf (psf, "  Data size : %D\n", ds64_datalength) ;

					psf_log_printf (psf, "  Frames    : %D\n", frame_count) ;
					psf_log_printf (psf, "  Table length : %u\n", table_len) ;

					} ;
				parsestage |= HAVE_ds64 ;
				break ;

			case fmt_MARKER:
					psf_log_printf (psf, "%M : %u\n", marker, chunk_size) ;
					if ((error = wavlike_read_fmt_chunk (psf, chunk_size)) != 0)
						return error ;
					format = wav_fmt->format ;
					parsestage |= HAVE_fmt ;
					break ;

			case bext_MARKER :
					if ((error = wavlike_read_bext_chunk (psf, chunk_size)) != 0)
						return error ;
					parsestage |= HAVE_bext ;
					break ;

			case cart_MARKER :
					if ((error = wavlike_read_cart_chunk (psf, chunk_size)) != 0)
						return error ;
					parsestage |= HAVE_cart ;
					break ;

			case INFO_MARKER :
			case LIST_MARKER :
					if ((error = wavlike_subchunk_parse (psf, marker, chunk_size)) != 0)
						return error ;
					parsestage |= HAVE_other ;
					break ;

			case PEAK_MARKER :
					if ((parsestage & (HAVE_ds64 | HAVE_fmt)) != (HAVE_ds64 | HAVE_fmt))
						return SFE_RF64_PEAK_B4_FMT ;

					parsestage |= HAVE_PEAK ;

					psf_log_printf (psf, "%M : %u\n", marker, chunk_size) ;
					if ((error = wavlike_read_peak_chunk (psf, chunk_size)) != 0)
						return error ;
					psf->peak_info->peak_loc = ((parsestage & HAVE_data) == 0) ? SF_PEAK_START : SF_PEAK_END ;
					break ;

			case data_MARKER :
				/* see wav for more sophisticated parsing -> implement state machine with parsestage */

				if (HAVE_CHUNK (HAVE_ds64))
				{	if (chunk_size == 0xffffffff)
						psf_log_printf (psf, "%M : 0x%x\n", marker, chunk_size) ;
					else
						psf_log_printf (psf, "%M : 0x%x (should be 0xffffffff\n", marker, chunk_size) ;
					psf->datalength = ds64_datalength ;
					}
				else
				{	if (chunk_size == 0xffffffff)
					{	psf_log_printf (psf, "%M : 0x%x\n", marker, chunk_size) ;
						psf_log_printf (psf, "  *** Data length not specified no 'ds64' chunk.\n") ;
						}
					else
					{	psf_log_printf (psf, "%M : 0x%x\n**** Weird, RF64 file without a 'ds64' chunk and no valid 'data' size.\n", marker, chunk_size) ;
						psf->datalength = chunk_size ;
						} ;
					} ;

				psf->dataoffset = psf_ftell (psf) ;

				if (psf->dataoffset > 0)
				{	if (chunk_size == 0 && riff_size == 8 && psf->filelength > 44)
					{	psf_log_printf (psf, "  *** Looks like a WAV file which wasn't closed properly. Fixing it.\n") ;
						psf->datalength = psf->filelength - psf->dataoffset ;
						} ;

					/* Only set dataend if there really is data at the end. */
					if (psf->datalength + psf->dataoffset < psf->filelength)
						psf->dataend = psf->datalength + psf->dataoffset ;

					if (NOT (psf->sf.seekable) || psf->dataoffset < 0)
						break ;

					/* Seek past data and continue reading header. */
					psf_fseek (psf, psf->datalength, SEEK_CUR) ;

					if (psf_ftell (psf) != psf->datalength + psf->dataoffset)
						psf_log_printf (psf, "  *** psf_fseek past end error ***\n") ;
					} ;
				break ;

			case JUNK_MARKER :
			case PAD_MARKER :
				psf_log_printf (psf, "%M : %d\n", marker, chunk_size) ;
				psf_binheader_readf (psf, "j", chunk_size) ;
				break ;

			default :
					if (chunk_size >= 0xffff0000)
					{	psf_log_printf (psf, "*** Unknown chunk marker (%X) at position %D with length %u. Exiting parser.\n", marker, psf_ftell (psf) - 8, chunk_size) ;
						done = SF_TRUE ;
						break ;
						} ;

					if (isprint ((marker >> 24) & 0xFF) && isprint ((marker >> 16) & 0xFF)
						&& isprint ((marker >> 8) & 0xFF) && isprint (marker & 0xFF))
					{	psf_log_printf (psf, "*** %M : %d (unknown marker)\n", marker, chunk_size) ;
						psf_binheader_readf (psf, "j", chunk_size) ;
						break ;
						} ;
					if (psf_ftell (psf) & 0x03)
					{	psf_log_printf (psf, "  Unknown chunk marker at position 0x%x. Resynching.\n", chunk_size - 4) ;
						psf_binheader_readf (psf, "j", -3) ;
						break ;
						} ;
					psf_log_printf (psf, "*** Unknown chunk marker (0x%X) at position 0x%X. Exiting parser.\n", marker, psf_ftell (psf) - 4) ;
					done = SF_TRUE ;
					break ;
			} ;	/* switch (marker) */

		/* The 'data' chunk, a chunk size of 0xffffffff means that the 'data' chunk size
		** is actually given by the ds64_datalength field.
		*/
		if (marker != data_MARKER && chunk_size >= psf->filelength)
		{	psf_log_printf (psf, "*** Chunk size %u > file length %D. Exiting parser.\n", chunk_size, psf->filelength) ;
			break ;
			} ;

		if (psf_ftell (psf) >= psf->filelength - SIGNED_SIZEOF (marker))
		{	psf_log_printf (psf, "End\n") ;
			break ;
			} ;
		} ;

	if (psf->dataoffset <= 0)
		return SFE_RF64_NO_DATA ;

	if (psf->sf.channels < 1)
		return SFE_CHANNEL_COUNT_ZERO ;

	if (psf->sf.channels > SF_MAX_CHANNELS)
		return SFE_CHANNEL_COUNT ;

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

	if (frame_count != psf->sf.frames)
		psf_log_printf (psf, "*** Calculated frame count %d does not match value from 'ds64' chunk of %d.\n", psf->sf.frames, frame_count) ;

	switch (format)
	{
		case WAVE_FORMAT_EXTENSIBLE :

			/* with WAVE_FORMAT_EXTENSIBLE the psf->sf.format field is already set. We just have to set the major to rf64 */
			psf->sf.format = (psf->sf.format & ~SF_FORMAT_TYPEMASK) | SF_FORMAT_RF64 ;

			if (psf->sf.format == (SF_FORMAT_WAVEX | SF_FORMAT_MS_ADPCM))
			{	*blockalign = wav_fmt->msadpcm.blockalign ;
				*framesperblock = wav_fmt->msadpcm.samplesperblock ;
				} ;
			break ;

		case WAVE_FORMAT_PCM :
					psf->sf.format = SF_FORMAT_RF64 | u_bitwidth_to_subformat (psf->bytewidth * 8) ;
					break ;

		case WAVE_FORMAT_MULAW :
		case IBM_FORMAT_MULAW :
					psf->sf.format = (SF_FORMAT_RF64 | SF_FORMAT_ULAW) ;
					break ;

		case WAVE_FORMAT_ALAW :
		case IBM_FORMAT_ALAW :
					psf->sf.format = (SF_FORMAT_RF64 | SF_FORMAT_ALAW) ;
					break ;

		case WAVE_FORMAT_MS_ADPCM :
					psf->sf.format = (SF_FORMAT_RF64 | SF_FORMAT_MS_ADPCM) ;
					*blockalign = wav_fmt->msadpcm.blockalign ;
					*framesperblock = wav_fmt->msadpcm.samplesperblock ;
					break ;

		case WAVE_FORMAT_IMA_ADPCM :
					psf->sf.format = (SF_FORMAT_RF64 | SF_FORMAT_IMA_ADPCM) ;
					*blockalign = wav_fmt->ima.blockalign ;
					*framesperblock = wav_fmt->ima.samplesperblock ;
					break ;

		case WAVE_FORMAT_GSM610 :
					psf->sf.format = (SF_FORMAT_RF64 | SF_FORMAT_GSM610) ;
					break ;

		case WAVE_FORMAT_IEEE_FLOAT :
					psf->sf.format = SF_FORMAT_RF64 ;
					psf->sf.format |= (psf->bytewidth == 8) ? SF_FORMAT_DOUBLE : SF_FORMAT_FLOAT ;
					break ;

		case WAVE_FORMAT_G721_ADPCM :
					psf->sf.format = SF_FORMAT_RF64 | SF_FORMAT_G721_32 ;
					break ;

		default : return SFE_UNIMPLEMENTED ;
		} ;

	if (wpriv->fmt_is_broken)
		wavlike_analyze (psf) ;

	/* Only set the format endian-ness if its non-standard big-endian. */
	if (psf->endian == SF_ENDIAN_BIG)
		psf->sf.format |= SF_ENDIAN_BIG ;

	return 0 ;
} /* rf64_read_header */

/*  known WAVEFORMATEXTENSIBLE GUIDS  */
static const EXT_SUBFORMAT MSGUID_SUBTYPE_PCM =
{	0x00000001, 0x0000, 0x0010, {	0x80, 0x00, 0x00, 0xaa, 0x00, 0x38, 0x9b, 0x71 }
} ;

#if 0
static const EXT_SUBFORMAT MSGUID_SUBTYPE_MS_ADPCM =
{	0x00000002, 0x0000, 0x0010, {	0x80, 0x00, 0x00, 0xaa, 0x00, 0x38, 0x9b, 0x71 }
} ;
#endif

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
rf64_write_fmt_chunk (SF_PRIVATE *psf)
{	WAVLIKE_PRIVATE	*wpriv ;
	int subformat, fmt_size ;

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
			psf_binheader_writef (psf, "4224", BHW4 (fmt_size), BHW2 (WAVE_FORMAT_EXTENSIBLE), BHW2 (psf->sf.channels), BHW4 (psf->sf.samplerate)) ;
			/*  fmt : bytespersec */
			psf_binheader_writef (psf, "4", BHW4 (psf->sf.samplerate * psf->bytewidth * psf->sf.channels)) ;
			/*  fmt : blockalign, bitwidth */
			psf_binheader_writef (psf, "22", BHW2 (psf->bytewidth * psf->sf.channels), BHW2 (psf->bytewidth * 8)) ;

			/* cbSize 22 is sizeof (WAVEFORMATEXTENSIBLE) - sizeof (WAVEFORMATEX) */
			psf_binheader_writef (psf, "2", BHW2 (22)) ;

			/* wValidBitsPerSample, for our use same as bitwidth as we use it fully */
			psf_binheader_writef (psf, "2", BHW2 (psf->bytewidth * 8)) ;

			/* For an Ambisonic file set the channel mask to zero.
			** Otherwise use a default based on the channel count.
			*/
			if (wpriv->wavex_ambisonic != SF_AMBISONIC_NONE)
				psf_binheader_writef (psf, "4", BHW4 (0)) ;
			else if (wpriv->wavex_channelmask != 0)
				psf_binheader_writef (psf, "4", BHW4 (wpriv->wavex_channelmask)) ;
			else
			{	/*
				** Ok some liberty is taken here to use the most commonly used channel masks
				** instead of "no mapping". If you really want to use "no mapping" for 8 channels and less
				** please don't use wavex. (otherwise we'll have to create a new SF_COMMAND)
				*/
				switch (psf->sf.channels)
				{	case 1 :	/* center channel mono */
						psf_binheader_writef (psf, "4", BHW4 (0x4)) ;
						break ;

					case 2 :	/* front left and right */
						psf_binheader_writef (psf, "4", BHW4 (0x1 | 0x2)) ;
						break ;

					case 4 :	/* Quad */
						psf_binheader_writef (psf, "4", BHW4 (0x1 | 0x2 | 0x10 | 0x20)) ;
						break ;

					case 6 :	/* 5.1 */
						psf_binheader_writef (psf, "4", BHW4 (0x1 | 0x2 | 0x4 | 0x8 | 0x10 | 0x20)) ;
						break ;

					case 8 :	/* 7.1 */
						psf_binheader_writef (psf, "4", BHW4 (0x1 | 0x2 | 0x4 | 0x8 | 0x10 | 0x20 | 0x40 | 0x80)) ;
						break ;

					default :	/* 0 when in doubt , use direct out, ie NO mapping*/
						psf_binheader_writef (psf, "4", BHW4 (0x0)) ;
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
			wavlike_write_guid (psf, wpriv->wavex_ambisonic == SF_AMBISONIC_NONE ?
						&MSGUID_SUBTYPE_PCM : &MSGUID_SUBTYPE_AMBISONIC_B_FORMAT_PCM) ;
			break ;

		case SF_FORMAT_FLOAT :
		case SF_FORMAT_DOUBLE :
			wavlike_write_guid (psf, wpriv->wavex_ambisonic == SF_AMBISONIC_NONE ?
						&MSGUID_SUBTYPE_IEEE_FLOAT : &MSGUID_SUBTYPE_AMBISONIC_B_FORMAT_IEEE_FLOAT) ;
			break ;

		case SF_FORMAT_ULAW :
			wavlike_write_guid (psf, &MSGUID_SUBTYPE_MULAW) ;
			break ;

		case SF_FORMAT_ALAW :
			wavlike_write_guid (psf, &MSGUID_SUBTYPE_ALAW) ;
			break ;

		default : return SFE_UNIMPLEMENTED ;
		} ;

	return 0 ;
} /* rf64_write_fmt_chunk */


static int
rf64_write_header (SF_PRIVATE *psf, int calc_length)
{	sf_count_t	current, pad_size ;
	int 		error = 0, has_data = SF_FALSE, add_fact_chunk = 0 ;
	WAVLIKE_PRIVATE	*wpriv ;

	if ((wpriv = psf->container_data) == NULL)
		return SFE_INTERNAL ;

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
	psf->header.ptr [0] = 0 ;
	psf->header.indx = 0 ;
	psf_fseek (psf, 0, SEEK_SET) ;

	if (wpriv->rf64_downgrade && psf->filelength < RIFF_DOWNGRADE_BYTES)
	{	psf_binheader_writef (psf, "etm8m", BHWm (RIFF_MARKER), BHW8 ((psf->filelength < 8) ? 8 : psf->filelength - 8), BHWm (WAVE_MARKER)) ;
		psf_binheader_writef (psf, "m4z", BHWm (JUNK_MARKER), BHW4 (24), BHWz (24)) ;
		add_fact_chunk = 1 ;
		}
	else
	{	psf_binheader_writef (psf, "em4m", BHWm (RF64_MARKER), BHW4 (0xffffffff), BHWm (WAVE_MARKER)) ;
		/* Currently no table. */
		psf_binheader_writef (psf, "m48884", BHWm (ds64_MARKER), BHW4 (28), BHW8 (psf->filelength - 8), BHW8 (psf->datalength), BHW8 (psf->sf.frames), BHW4 (0)) ;
		} ;

	/* WAVE and 'fmt ' markers. */
	psf_binheader_writef (psf, "m", BHWm (fmt_MARKER)) ;

	/* Write the 'fmt ' chunk. */
	switch (psf->sf.format & SF_FORMAT_TYPEMASK)
	{	case SF_FORMAT_WAV :
				psf_log_printf (psf, "ooops SF_FORMAT_WAV\n") ;
				return SFE_UNIMPLEMENTED ;
				break ;

		case SF_FORMAT_WAVEX :
		case SF_FORMAT_RF64 :
				if ((error = rf64_write_fmt_chunk (psf)) != 0)
					return error ;
				if (add_fact_chunk)
					psf_binheader_writef (psf, "tm48", BHWm (fact_MARKER), BHW4 (4), BHW8 (psf->sf.frames)) ;
				break ;

		default :
				return SFE_UNIMPLEMENTED ;
		} ;

	if (psf->broadcast_16k != NULL)
		wavlike_write_bext_chunk (psf) ;

	if (psf->cart_16k != NULL)
		wavlike_write_cart_chunk (psf) ;

	/* The LIST/INFO chunk. */
	if (psf->strings.flags & SF_STR_LOCATE_START)
		wavlike_write_strings (psf, SF_STR_LOCATE_START) ;

	if (psf->peak_info != NULL && psf->peak_info->peak_loc == SF_PEAK_START)
		wavlike_write_peak_chunk (psf) ;

	/* Write custom headers. */
	if (psf->wchunks.used > 0)
		wavlike_write_custom_chunks (psf) ;

#if 0
	if (psf->instrument != NULL)
	{	int		tmp ;
		double	dtune = (double) (0x40000000) / 25.0 ;

		psf_binheader_writef (psf, "m4", BHWm (smpl_MARKER), BHW4 (9 * 4 + psf->instrument->loop_count * 6 * 4)) ;
		psf_binheader_writef (psf, "44", BHW4 (0), BHW4 (0)) ; /* Manufacturer zero is everyone */
		tmp = (int) (1.0e9 / psf->sf.samplerate) ; /* Sample period in nano seconds */
		psf_binheader_writef (psf, "44", BHW4 (tmp), BHW4 (psf->instrument->basenote)) ;
		tmp = (unsigned int) (psf->instrument->detune * dtune + 0.5) ;
		psf_binheader_writef (psf, "4", BHW4 (tmp)) ;
		psf_binheader_writef (psf, "44", BHW4 (0), BHW4 (0)) ; /* SMTPE format */
		psf_binheader_writef (psf, "44", BHW4 (psf->instrument->loop_count), BHW4 (0)) ;

		for (tmp = 0 ; tmp < psf->instrument->loop_count ; tmp++)
		{	int type ;

			type = psf->instrument->loops [tmp].mode ;
			type = (type == SF_LOOP_FORWARD ? 0 : type == SF_LOOP_BACKWARD ? 2 : type == SF_LOOP_ALTERNATING ? 1 : 32) ;

			psf_binheader_writef (psf, "44", BHW4 (tmp), BHW4 (type)) ;
			psf_binheader_writef (psf, "44", BHW4 (psf->instrument->loops [tmp].start), BHW4 (psf->instrument->loops [tmp].end)) ;
			psf_binheader_writef (psf, "44", BHW4 (0), BHW4 (psf->instrument->loops [tmp].count)) ;
			} ;
		} ;

#endif

	/* Padding may be needed if string data sizes change. */
	pad_size = psf->dataoffset - 16 - psf->header.indx ;
	if (pad_size >= 0)
		psf_binheader_writef (psf, "m4z", BHWm (PAD_MARKER), BHW4 ((unsigned int) pad_size), BHWz (pad_size)) ;

	if (wpriv->rf64_downgrade && (psf->filelength < RIFF_DOWNGRADE_BYTES))
		psf_binheader_writef (psf, "tm8", BHWm (data_MARKER), BHW8 (psf->datalength)) ;
	else
		psf_binheader_writef (psf, "m4", BHWm (data_MARKER), BHW4 (0xffffffff)) ;

	psf_fwrite (psf->header.ptr, psf->header.indx, 1, psf) ;
	if (psf->error)
		return psf->error ;

	if (has_data && psf->dataoffset != psf->header.indx)
	{	psf_log_printf (psf, "Oooops : has_data && psf->dataoffset != psf->header.indx\n") ;
		return psf->error = SFE_INTERNAL ;
		} ;

	psf->dataoffset = psf->header.indx ;

	if (NOT (has_data))
		psf_fseek (psf, psf->dataoffset, SEEK_SET) ;
	else if (current > 0)
		psf_fseek (psf, current, SEEK_SET) ;

	return psf->error ;
} /* rf64_write_header */

static int
rf64_write_tailer (SF_PRIVATE *psf)
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
		wavlike_write_strings (psf, SF_STR_LOCATE_END) ;

	/* Write the tailer. */
	if (psf->header.indx > 0)
		psf_fwrite (psf->header.ptr, psf->header.indx, 1, psf) ;

	return 0 ;
} /* rf64_write_tailer */

static int
rf64_close (SF_PRIVATE *psf)
{
	if (psf->file.mode == SFM_WRITE || psf->file.mode == SFM_RDWR)
	{	rf64_write_tailer (psf) ;
		rf64_write_header (psf, SF_TRUE) ;
		} ;

	return 0 ;
} /* rf64_close */

static int
rf64_command (SF_PRIVATE *psf, int command, void * UNUSED (data), int datasize)
{	WAVLIKE_PRIVATE	*wpriv ;

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

		case SFC_SET_CHANNEL_MAP_INFO :
			wpriv->wavex_channelmask = wavlike_gen_channel_mask (psf->channel_map, psf->sf.channels) ;
			return (wpriv->wavex_channelmask != 0) ;

		case SFC_RF64_AUTO_DOWNGRADE :
			if (psf->have_written == 0)
				wpriv->rf64_downgrade = datasize ? SF_TRUE : SF_FALSE ;
			return wpriv->rf64_downgrade ;

		default :
			break ;
	} ;

	return 0 ;
} /* rf64_command */

static int
rf64_set_chunk (SF_PRIVATE *psf, const SF_CHUNK_INFO * chunk_info)
{	return psf_save_write_chunk (&psf->wchunks, chunk_info) ;
} /* rf64_set_chunk */

static SF_CHUNK_ITERATOR *
rf64_next_chunk_iterator (SF_PRIVATE *psf, SF_CHUNK_ITERATOR * iterator)
{	return psf_next_chunk_iterator (&psf->rchunks, iterator) ;
} /* rf64_next_chunk_iterator */

static int
rf64_get_chunk_size (SF_PRIVATE *psf, const SF_CHUNK_ITERATOR * iterator, SF_CHUNK_INFO * chunk_info)
{	int indx ;

	if ((indx = psf_find_read_chunk_iterator (&psf->rchunks, iterator)) < 0)
		return SFE_UNKNOWN_CHUNK ;

	chunk_info->datalen = psf->rchunks.chunks [indx].len ;

	return SFE_NO_ERROR ;
} /* rf64_get_chunk_size */

static int
rf64_get_chunk_data (SF_PRIVATE *psf, const SF_CHUNK_ITERATOR * iterator, SF_CHUNK_INFO * chunk_info)
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
} /* rf64_get_chunk_data */
