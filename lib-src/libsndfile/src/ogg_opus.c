/*
** Copyright (C) 2013-2019 Erik de Castro Lopo <erikd@mega-nerd.com>
** Copyright (C) 2018 Arthur Taylor <art@ified.ca>
**
** This program is free software ; you can redistribute it and/or modify
** it under the terms of the GNU Lesser General Public License as published by
** the Free Software Foundation ; either version 2.1 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY ; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
** GNU Lesser General Public License for more details.
**
** You should have received a copy of the GNU Lesser General Public License
** along with this program ; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

/*
** This file contains code based on OpusFile and Opus-Tools, both by
** Xiph.Org. COPYING from each is identical and is as follows:
**
** Copyright (c) 1994-2013 Xiph.Org Foundation and contributors
**
** Redistribution and use in source and binary forms, with or without
** modification, are permitted provided that the following conditions
** are met:
**
** - Redistributions of source code must retain the above copyright
** notice, this list of conditions and the following disclaimer.
**
** - Redistributions in binary form must reproduce the above copyright
** notice, this list of conditions and the following disclaimer in the
** documentation and/or other materials provided with the distribution.
**
** - Neither the name of the Xiph.Org Foundation nor the names of its
** contributors may be used to endorse or promote products derived from
** this software without specific prior written permission.
**
** THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
** ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
** LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
** A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE FOUNDATION
** OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
** SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
** LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
** DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
** THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
** (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
** OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

/*
** TODO:
**  - Channel mapping modification / reporting
**     - connect psf->channel_map and Opus channel mapping somehow?
**  - Gain parameters and their mappings
*/

/*
** Opus Sample, Frame, and Samples/Channel Terminology
**
** libsndfile refers to one PCM value as a 'sample,' and a group of samples of
** the same sample time, one for each channel, as a 'frame.' This differs from
** Opus, which has no corresponding name for sample, and refers to a group of
** PCM values, one per channel (aka libsndfile frames) as 'samples.'
** Further, Opus has an object called a 'frame' that is made up of multiple
** Opus-samples.
** All this means that one has to be careful with what is meant by each term.
** In an attempt to avoid ambiguity, this file adopts the following terms:
**  - Samples shall refer to discrete PCM values, regardless of any channel
**    considerations. This is the same as what libsndfile calls samples.
**  - Samples/channel shall refer to groups of samples, one for each channel.
**    This is what Opus calles samples, and what libsndfile calles frames. It
**    has the advantage that its name is also the formula to calculate it.
**
**
** Opus vs OggOpus
**
** In this file a distinction is made between Opus and OggOpus. Opus refers to
** the codec alone, support for which is by libopus. OggOpus refers to an Opus
** payload encapsulated in an Ogg stream. This is also know as an "Opus file."
** The OggOpus spec includes information on header and granule position
** interpretation, which is outside of the scope of the Opus spec. As such, an
** attempt here is made to refer to either Opus or OggOpus depending on which
** spec is being referenced. See https://wiki.xiph.org/OggOpus
**
**
** Opus Sample Rates
**
** Opus only supports a fixed number of sample rates: 48kHz, 24kHz, 16kHz,
** 12kHz, 8kHz. Audio may be decoded or encoded at any of these rates,
** independent of the rate it was encoded at or to be decoded at respectively.
** Other sample rates must be converted to one of these rates.
**
** As 44.1kHz (CD sample rate) and 22.5kHz are popular sample rates, and to
** support any other sample rate there may be, the Opus header includes a field
** to save the input (original) sample rate before converting it to a supported
** one. Implementations are recommended by the Opus spec to do a sample rate
** conversion at encode, but decode at 48kHz if outputting to hardware, or do
** the reverse sample rate conversion if outputting to file.
**
** Heretofore libsndfile does not contain a sample rate converter, so doing the
** sample rate conversion is not supported. Instead audio must be provided by
** the user at a supported rate. However, the input sample rate field can be
** set and retrieved by the user using sf_command(). At decode we choose to
** decode at the lowest valid rate that is greater than or equal to the input
** sample rate.
**
**
** OggOpus Granule Positions
**
** Ogg streams include a strictly increasing granule position value. The
** interpretation of this value is dependent on the payload type. For Opus
** streams the granule position is the count of samples in the stream when
** encoding/decoding at 48kHz. Note that the actual position of the output
** sample relative to the granule position is offset by the preskip amount.
** That is, if a packet ends with a granule position of x, the last sample
** output when decoding is actually sample (x - preskip).
**
** Further, to allow for clipping off of the front of a stream without
** rewriting all following granule positions, an Opus stream granule position
** may be offset by a constant amount. This amount is evident by comparing the
** granule position of the first page of an Opus stream on which an audio
** packet completes is greater than the sum of the samples of all audio
** packets completed on the page. Only the first such page is allows to have an
** 'excessive' granule position, and only if it is not also the last page of
** the stream (e_o_s bit is not set.)
**
** The granule position is an unsigned 64-bit integer, with the special value
** of UINT64_MAX/-1 being treated as invalid. However, as not all platforms
** support unsigned 64-bit integers, libOgg uses signed 64-bit integers for the
** granule position.
**
** Remembering that signed integer overflow/underflow is explicitly undefined
** in C, and as we already assume support for unsigned 64-bit integers, the
** easiest way to deal with this problem is to modify granule positions as
** unsigned integers.
*/


#include "sfconfig.h"

#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <math.h>

#if HAVE_UNISTD_H
#include <unistd.h>
#else
#include "sf_unistd.h"
#endif

#include "sndfile.h"
#include "sfendian.h"
#include "common.h"

#if HAVE_EXTERNAL_XIPH_LIBS

#include <ogg/ogg.h>
#include <opus/opus.h>
#include <opus/opus_multistream.h>

#include "ogg.h"
#include "ogg_vcomment.h"

#define OGG_OPUS_COMMENT_PAD (512) /* Same as oggenc default */
#define OGG_OPUS_PAGE_LATENCY (1000 * 48) /* 1 second */

/*
** Opus packets can be any multiple of 2.5ms (at 48kHz). We use the recommended
** default for non-realtime of 20ms. While longer packets reduce the overhead
** data somewhat, it also decreases the quality.
*/
#define OGG_OPUS_ENCODE_PACKET_LEN(samplerate) ((20 * (samplerate)) / 1000)

/*
** How long does it take for a decoder to converge (avoiding flush on seek.
*/
#define OGG_OPUS_PREROLL (80 * 48) /* 80 milliseconds */

typedef struct
{	int version ;

	/* Number of channels, 1...255 */
	int channels ;

	/* Encoder latency, the amount to skip before valid data comes out. */
	int preskip ;

	/* The sample rate of a the encoded source, as it may have been converted. */
	int input_samplerate ;

	/* 'baked-in' gain to apply, dB S7.8 format. Should be zero when possible. */
	int16_t gain ;

	/* Channel mapping type. See OggOpus spec */
	int channel_mapping ;

	/* The rest is only used if channel_mapping != 0 */
	/* How many streams are there? */
	int nb_streams ;

	/* How man of those streams are coupled? (aka stereo) */
	int nb_coupled ;

	/* Mapping of opus streams to output channels */
	unsigned char stream_map [255] ;
} OpusHeader ;

typedef struct
{	uint32_t serialno ;
	OpusHeader header ;

	/* Granule position before the current packet */
	uint64_t pkt_pos ;

	/* Granule position at the end of the current page (encode: last completed) */
	uint64_t pg_pos ;

	/* integer coefficient of (current sample rate) / 48000Hz */
	int sr_factor ;

	/* Current position in buffer expressed as samples/channel */
	int loc ;

	/* Current data fill (decode) or target (encode) of buffer expressed in samples/channel */
	int len ;

	/* Size of the buffer storage, in sizeof (float) * channels */
	int buffersize ;

	/* Samples, either decoded from a packet, or assembling for encode. */
	float *buffer ;

	union {
		/* decode only members */
		struct {
			OpusMSDecoder *state ;
			uint64_t gp_start ;
			uint64_t gp_end ;
			sf_count_t last_offset ;
		} decode ;

		/* encode only members */
		struct {
			OpusMSEncoder *state ;

			/* How many Ogg page segments are in Ogg page currently being assembled. */
			int last_segments ;

			int bitrate ;

			/* Least significant bit of the source (aka bitwidth) */
			int lsb ;
			int lsb_last ;
		} encode ;
	} u ;
} OPUS_PRIVATE ;

/*-----------------------------------------------------------------------------------------------
** Private function prototypes.
*/

static int			ogg_opus_close (SF_PRIVATE *psf) ;
static void			opus_print_header (SF_PRIVATE *psf, OpusHeader *h) ;
static int			opus_read_header_packet (SF_PRIVATE *psf, OpusHeader *h, ogg_packet *opacket) ;
static int			ogg_opus_read_header (SF_PRIVATE * psf) ;
static int			ogg_opus_setup_decoder (SF_PRIVATE *psf, int input_samplerate) ;

static int			ogg_opus_setup_encoder (SF_PRIVATE *psf, OGG_PRIVATE *odata, OPUS_PRIVATE *oopus) ;
static int			ogg_opus_write_header (SF_PRIVATE * psf, int calc_length) ;
static void			ogg_opus_flush (SF_PRIVATE *psf) ;
static int			ogg_opus_unpack_next_page (SF_PRIVATE *psf, OGG_PRIVATE *odata, OPUS_PRIVATE *oopus) ;
static int			ogg_opus_calculate_page_duration (OGG_PRIVATE *odata) ;
static int			ogg_opus_read_refill (SF_PRIVATE *psf, OGG_PRIVATE *odata, OPUS_PRIVATE *oopus) ;
static int			ogg_opus_write_out (SF_PRIVATE *psf, OGG_PRIVATE *odata, OPUS_PRIVATE *oopus) ;

static sf_count_t	ogg_opus_read_s (SF_PRIVATE *psf, short *ptr, sf_count_t len) ;
static sf_count_t	ogg_opus_read_i (SF_PRIVATE *psf, int *ptr, sf_count_t len) ;
static sf_count_t	ogg_opus_read_f (SF_PRIVATE *psf, float *ptr, sf_count_t len) ;
static sf_count_t	ogg_opus_read_d (SF_PRIVATE *psf, double *ptr, sf_count_t len) ;

static sf_count_t	ogg_opus_write_s (SF_PRIVATE *psf, const short *ptr, sf_count_t len) ;
static sf_count_t	ogg_opus_write_i (SF_PRIVATE *psf, const int *ptr, sf_count_t len) ;
static sf_count_t	ogg_opus_write_f (SF_PRIVATE *psf, const float *ptr, sf_count_t len) ;
static sf_count_t	ogg_opus_write_d (SF_PRIVATE *psf, const double *ptr, sf_count_t len) ;

static sf_count_t	ogg_opus_seek (SF_PRIVATE *psf, int mode, sf_count_t offset) ;
static sf_count_t	ogg_opus_seek_null_read (SF_PRIVATE *psf, sf_count_t offset) ;
static sf_count_t	ogg_opus_seek_manual (SF_PRIVATE *psf, uint64_t target_gp) ;
static int			ogg_opus_seek_page_search (SF_PRIVATE *psf, uint64_t target_gp) ;

static int			ogg_opus_analyze_file (SF_PRIVATE *psf) ;
static int			ogg_opus_command (SF_PRIVATE *psf, int command, void *data, int datasize) ;
static int			ogg_opus_byterate (SF_PRIVATE *psf) ;

/*-----------------------------------------------------------------------------------------------
*/

static vorbiscomment_ident opustags_ident =	{ "OpusTags", 8 } ;

/*-----------------------------------------------------------------------------------------------
** Exported functions.
*/

int
ogg_opus_open (SF_PRIVATE *psf)
{	OGG_PRIVATE* odata = psf->container_data ;
	OPUS_PRIVATE* oopus = calloc (1, sizeof (OPUS_PRIVATE)) ;
	int	error = 0 ;

	if (odata == NULL)
	{	psf_log_printf (psf, "%s : odata is NULL???\n", __func__) ;
		return SFE_INTERNAL ;
		} ;

	psf->codec_data = oopus ;
	if (oopus == NULL)
		return SFE_MALLOC_FAILED ;

	if (psf->file.mode == SFM_RDWR)
		return SFE_BAD_MODE_RW ;

	psf_log_printf (psf, "Opus library version: %s\n", opus_get_version_string ()) ;

	psf->codec_close = ogg_opus_close ;
	if (psf->file.mode == SFM_READ)
	{	if ((error = ogg_opus_read_header (psf)))
			return error ;
		if ((error = ogg_opus_analyze_file (psf)))
			return error ;

		psf->read_short		= ogg_opus_read_s ;
		psf->read_int		= ogg_opus_read_i ;
		psf->read_float		= ogg_opus_read_f ;
		psf->read_double	= ogg_opus_read_d ;
		} ;

	if (psf->file.mode == SFM_WRITE)
	{	if ((error = ogg_opus_setup_encoder (psf, odata, oopus)))
			return error ;

		psf->write_header	= ogg_opus_write_header ;
		psf->write_short	= ogg_opus_write_s ;
		psf->write_int		= ogg_opus_write_i ;
		psf->write_float	= ogg_opus_write_f ;
		psf->write_double	= ogg_opus_write_d ;

		psf->sf.frames = SF_COUNT_MAX ; /* Unknown really */
		psf->strings.flags = SF_STR_ALLOW_START ;
		psf->datalength = 0 ;
		psf->dataoffset = 0 ; /* will be updated */
		} ;

	psf->seek = ogg_opus_seek ;
	psf->command = ogg_opus_command ;
	psf->byterate = ogg_opus_byterate ;
	psf->sf.format = SF_FORMAT_OGG | SF_FORMAT_OPUS ;

	return error ;
} /* ogg_opus_open */

/*==============================================================================
** Private functions.
*/

static int
ogg_opus_close (SF_PRIVATE *psf)
{	OGG_PRIVATE *odata = (OGG_PRIVATE *) psf->container_data ;
	OPUS_PRIVATE *oopus = (OPUS_PRIVATE *) psf->codec_data ;

	if (!oopus)
		return 0 ;

	if (psf->file.mode == SFM_WRITE)
	{	if (psf->have_written)
			ogg_opus_flush (psf) ;
		else {
			/* Write a header... it is expected. */
			ogg_opus_write_header (psf, 0) ;
			} ;
		ogg_packet_clear (&odata->opacket) ;
		if (oopus->u.encode.state)
		{	opus_multistream_encoder_destroy (oopus->u.encode.state) ;
			oopus->u.encode.state = NULL ;
			} ;
		}
	else if (psf->file.mode == SFM_READ)
	{	if (oopus->u.decode.state)
		{	opus_multistream_decoder_destroy (oopus->u.decode.state) ;
			oopus->u.decode.state = NULL ;
			} ;
		} ;

	psf->codec_data = NULL ;
	if (oopus->buffer)
		free (oopus->buffer) ;
	free (oopus) ;

	return 0 ;
} /* ogg_opus_close */

static void
opus_print_header (SF_PRIVATE *psf, OpusHeader *h)
{	psf_log_printf (psf, "Opus Header Metadata\n") ;
	psf_log_printf (psf, "  OggOpus version  : %d\n", h->version) ;
	psf_log_printf (psf, "  Channels         : %d\n", h->channels) ;
	psf_log_printf (psf, "  Preskip          : %d samples @48kHz\n", h->preskip) ;
	psf_log_printf (psf, "  Input Samplerate : %d Hz\n", h->input_samplerate) ;
	psf_log_printf (psf, "  Gain             : %d.%d\n", arith_shift_right (h->gain & 0xF0, 8), h->gain & 0x0F) ;
	psf_log_printf (psf, "  Channel Mapping  : ") ;
	switch (h->channel_mapping)
	{	case 0 :	psf_log_printf (psf, "0 (mono or stereo)\n") ; break ;
		case 1 :	psf_log_printf (psf, "1 (surround, AC3 channel order)\n") ; break ;
		case 255 :	psf_log_printf (psf, "255 (no channel order)\n") ; break ;
		default :	psf_log_printf (psf, "%d (unknown or unsupported)\n", h->channel_mapping) ; break ;
		} ;

	if (h->channel_mapping > 0)
	{	int i ;
		psf_log_printf (psf, "   streams total   : %d\n", h->nb_streams) ;
		psf_log_printf (psf, "   streams coupled : %d\n", h->nb_coupled) ;
		psf_log_printf (psf, "    stream mapping : [") ;
		for (i = 0 ; i < h->channels - 1 ; i++)
			psf_log_printf (psf, "%d,", h->stream_map [i]) ;
		psf_log_printf (psf, "%d]\n", h->stream_map [i]) ;
		} ;
} /* opus_print_header */

static int
opus_read_header_packet (SF_PRIVATE *psf, OpusHeader *h, ogg_packet *opacket)
{	int count, i ;

	/*
	** Opus headers are 19 bytes, in the case of type 0 channel mapping,
	** or 19 + 2 + (1 * channel count) bytes for other channel mappings, to a
	** maximum of 276 (255 channels).
	*/

	if (opacket->bytes < 19 || opacket->bytes > 276)
		return SFE_MALFORMED_FILE ;

	if (memcmp (opacket->packet, "OpusHead", 8) != 0)
		return SFE_MALFORMED_FILE ;

	/*
	** Copy the header page into the binheader so we can use binheader
	** functions to safely unpack it.
	*/
	count = psf_binheader_writef (psf, "ob", BHWo (0), BHWv (opacket->packet), BHWz (opacket->bytes)) ;
	psf->header.end = count ;

	count = psf_binheader_readf (psf, "ep1", 8, &h->version) ;
	if (! (h->version == 1 || h->version == 0))
	{	psf_log_printf (psf, "Opus : Unknown / unsupported embedding scheme version: %d.\n", h->version) ;
		return SFE_UNIMPLEMENTED ;
		} ;

	count += psf_binheader_readf (psf, "e12421", &h->channels, &h->preskip,
		&h->input_samplerate, &h->gain, &h->channel_mapping) ;

	if (h->channel_mapping == 0)
	{	if (h->channels > 2)
			return SFE_MALFORMED_FILE ;

		/*
		** Setup the stream mapping, so we can use the multistream decoder,
		** rather than have to deal with two decoder pointer types
		*/
		h->nb_streams = 1 ;
		h->nb_coupled = h->channels - 1 ;
		h->stream_map [0] = 0 ;
		h->stream_map [1] = 1 ;
		}
	else
	{	if (opacket->bytes < 19 + 2 + h->channels)
			return SFE_MALFORMED_FILE ;

		if (h->channel_mapping == 1 && h->channels > 8)
			return SFE_MALFORMED_FILE ;

		count += psf_binheader_readf (psf, "11", &h->nb_streams, &h->nb_coupled) ;

		if (h->nb_streams < 1 ||
			h->nb_coupled > h->nb_streams ||
			h->nb_coupled + h->nb_streams > 255)
			return SFE_MALFORMED_FILE ;

		for (i = 0 ; i < h->channels ; i++)
		{	count += psf_binheader_readf (psf, "1", &(h->stream_map [i])) ;
			if (h->stream_map [i] > h->nb_streams + h->nb_coupled && h->stream_map [i] != 255)
				return SFE_MALFORMED_FILE ;
			} ;
		} ;

	if (count != opacket->bytes)
	{	/* OggOpus spec mandates that this is a hard error. */
		psf_log_printf (psf, "Opus : Error, extra data in Ogg Opus header.\n") ;
		return SFE_MALFORMED_FILE ;
		} ;

	opus_print_header (psf, h) ;

	return 0 ;
} /* ogg_opus_read_header_packet */

static int
ogg_opus_read_header (SF_PRIVATE *psf)
{	OGG_PRIVATE *odata = (OGG_PRIVATE *) psf->container_data ;
	OPUS_PRIVATE *oopus = (OPUS_PRIVATE *) psf->codec_data ;
	int error ;

	/*
	** First page is already loaded by the ogg container code when it
	** classified the stream, no need to re-load it now.
	*/

	if (ogg_page_packets (&odata->opage) != 1 || !ogg_page_bos (&odata->opage))
		return SFE_MALFORMED_FILE ;

	oopus->serialno = ogg_page_serialno (&odata->opage) ;
	if ((error = opus_read_header_packet (psf, &oopus->header, &odata->opacket)))
		return error ;

	/*
	** The comment header MUST be next. It is one packet, that packet MUST begin
	** on the second page of the stream, but it MAY span multiple pages.
	*/

	while (ogg_stream_packetout (&odata->ostream, &odata->opacket) != 1)
	{	if (ogg_stream_next_page (psf, odata) != 1)
		{	/* out of data... technically that's malformed. */
			return psf->error ? psf->error : SFE_MALFORMED_FILE ;
			} ;
		} ;

	if ((error = vorbiscomment_read_tags (psf, &odata->opacket, &opustags_ident)))
		return error ;

	return ogg_opus_setup_decoder (psf, oopus->header.input_samplerate) ;
} /* ogg_opus_read_header */

static int
ogg_opus_setup_decoder (SF_PRIVATE *psf, int input_samplerate)
{	OPUS_PRIVATE *oopus = (OPUS_PRIVATE *) psf->codec_data ;
	OpusMSDecoder *decoder ;
	int sr_factor ;
	int error ;

	/*
	** Decide what sample rate to decode at. We choose the lowest valid rate
	** that is greater or equal to the original rate.
	**
	** Opus documentation recommends always decoding at 48000Hz if the file is
	** being decoded for playback, since most hardware will resample it back to
	** 48000Hz anyways. We don't know if that's true, maybe the user is
	** decoding for editing or transcoding purposes.
	*/
	if (input_samplerate > 24000)
		sr_factor = 1 ;
	else if (input_samplerate > 16000)
		sr_factor = 2 ;
	else if (input_samplerate > 12000)
		sr_factor = 3 ;
	else if (input_samplerate > 8000)
		sr_factor = 4 ;
	else
		sr_factor = 6 ;

	decoder = opus_multistream_decoder_create (
		48000 / sr_factor,
		oopus->header.channels,
		oopus->header.nb_streams,
		oopus->header.nb_coupled,
		oopus->header.stream_map,
		&error) ;

	if (error != OPUS_OK)
	{	psf_log_printf (psf, "Opus : Failed to create multistream decoder: %s\n",
			opus_strerror (error)) ;
		return SFE_INTERNAL ;
		}

	/*
	** Replace the decoder, if one was already initialized (see
	** SFC_GET_ORIGINAL_SAMPLERATE)
	*/
	if (oopus->u.decode.state)
		opus_multistream_decoder_destroy (oopus->u.decode.state) ;
	oopus->u.decode.state = decoder ;

	oopus->sr_factor = sr_factor ;
	psf->sf.samplerate = 48000 / sr_factor ;
	psf->sf.channels = oopus->header.channels ;
	oopus->loc = oopus->len = 0 ;

	/*
	** The Opus decoder can do our gain for us. The OggOpus header contains a
	** gain field. This field, unlike various gain-related tags, is intended to
	** be a perminent baked-in gain applied before any user-configurable gain
	** (eg replay-gain.) This is so the gain of track can be set without having
	** to re-encode.
	**
	** Both the header.gain field and the parameter are in the Q7.8 format.
	**
	** TODO: Make this configurable? Include other gain sources too?
	*/
	opus_multistream_decoder_ctl (oopus->u.decode.state, OPUS_SET_GAIN (oopus->header.gain)) ;

	/*
	** Opus packets can vary in length, with the legal values being 2.5, 5, 10,
	** 20, 40 or 60ms. The recommended default for non-realtime is 20ms. As
	** such, allocate a buffer of that size now, we'll realloc later if a
	** larger one is needed.
	**
	** buffersize is expressed in samples/channel, as that is what opus_decode
	** expects.
	*/
	if (oopus->buffer)
	{	free (oopus->buffer) ;
		oopus->buffer = NULL ;
		} ;
	oopus->buffersize = 20 * psf->sf.samplerate / 1000 ;
	oopus->buffer = malloc (sizeof (float) * psf->sf.channels * oopus->buffersize) ;
	if (oopus->buffer == NULL)
		return SFE_MALLOC_FAILED ;

	return 0 ;
} /* ogg_opus_setup_decoder */

static int
ogg_opus_setup_encoder (SF_PRIVATE *psf, OGG_PRIVATE *odata, OPUS_PRIVATE *oopus)
{	int error ;

	switch (psf->sf.samplerate)
	{	case 8000 :
		case 12000 :
		case 16000 :
		case 24000 :
		case 48000 :
			oopus->sr_factor = 48000 / psf->sf.samplerate ;
			break ;
		default :
			return SFE_OPUS_BAD_SAMPLERATE ;
		} ;

	if (psf->sf.channels <= 2)
	{	oopus->header.channel_mapping = 0 ;
		oopus->header.nb_streams = 1 ;
		oopus->header.nb_coupled = psf->sf.channels - 1 ;
		oopus->header.stream_map [0] = 0 ;
		oopus->header.stream_map [1] = 1 ;

		oopus->u.encode.state = opus_multistream_encoder_create (
			psf->sf.samplerate,
			psf->sf.channels,
			oopus->header.nb_streams,
			oopus->header.nb_coupled,
			oopus->header.stream_map,
			OPUS_APPLICATION_AUDIO,
			&error) ;
		}
	else
	{	if (psf->sf.channels <= 8)
		{	/* Use Vorbis/AC3 channel mappings for surround. */
			oopus->header.channel_mapping = 1 ;
			}
		else
		{	/* There is no channel mapping, just audio, in parallel, good luck */
			oopus->header.channel_mapping = 255 ;
			}

		oopus->u.encode.state = opus_multistream_surround_encoder_create (
			psf->sf.samplerate,
			psf->sf.channels,
			oopus->header.channel_mapping,
			&oopus->header.nb_streams,
			&oopus->header.nb_coupled,
			oopus->header.stream_map,
			OPUS_APPLICATION_AUDIO,
			&error) ;
		}

	if (error != OPUS_OK)
	{	psf_log_printf (psf, "Opus : Error, opus_multistream_encoder_create returned %s\n", opus_strerror (error)) ;
		return SFE_BAD_OPEN_FORMAT ;
		} ;

	opus_multistream_encoder_ctl (oopus->u.encode.state, OPUS_GET_BITRATE (&oopus->u.encode.bitrate)) ;
	psf_log_printf (psf, "Encoding at target bitrate of %dbps\n", oopus->u.encode.bitrate) ;

	/* TODO: Make configurable? */
	error = opus_multistream_encoder_ctl (oopus->u.encode.state, OPUS_SET_COMPLEXITY (10)) ;
	if (error != OPUS_OK)
	{	/* Non-fatal */
		psf_log_printf (psf, "Opus : OPUS_SET_COMPLEXITY returned: %s\n", opus_strerror (error)) ;
		}

	/*
	** Get the encoder delay. This can vary depending on implementation and
	** encoder configuration.
	** GOTCHA: This returns the preskip at the encoder samplerate, not the
	** granulepos rate of 48000Hz needed for header.preskip.
	*/
	error = opus_multistream_encoder_ctl (oopus->u.encode.state, OPUS_GET_LOOKAHEAD (&oopus->header.preskip)) ;
	if (error != OPUS_OK)
	{	psf_log_printf (psf, "Opus : OPUS_GET_LOOKAHEAD returned: %s\n", opus_strerror (error)) ;
		return SFE_BAD_OPEN_FORMAT ;
		} ;
	oopus->header.preskip *= oopus->sr_factor ;

	oopus->len = OGG_OPUS_ENCODE_PACKET_LEN (psf->sf.samplerate) ;
	oopus->buffer = malloc (sizeof (float) * psf->sf.channels * oopus->len) ;
	if (oopus->buffer == NULL)
		return SFE_MALLOC_FAILED ;

	/*
	** Set up the resident ogg packet structure, ready for writing into.
	** 1275 * 3 + 7 bytes of packet per stream is from opusenc from opus-tools
	*/
	ogg_packet_clear (&odata->opacket) ;
	oopus->buffersize = (1275 * 3 + 7) * oopus->header.nb_streams ;
	odata->opacket.packet = malloc (oopus->buffersize) ;
	odata->opacket.packetno = 2 ;
	if (odata->opacket.packet == NULL)
		return SFE_MALLOC_FAILED ;

	oopus->serialno = psf_rand_int32 () ;
	ogg_stream_init (&odata->ostream, oopus->serialno) ;

	return 0 ;
} /* ogg_opus_setup_encoder */

static int
ogg_opus_write_header (SF_PRIVATE *psf, int UNUSED (calc_length))
{	OGG_PRIVATE *odata = (OGG_PRIVATE *) psf->container_data ;
	OPUS_PRIVATE *oopus = (OPUS_PRIVATE *) psf->codec_data ;
	int nn ;
	ogg_packet op ;

	oopus->header.version = 1 ;
	oopus->header.channels = psf->sf.channels ;

	/* FIXME: Allow the user to set this ?! */
	oopus->header.gain = 0 ;

	if (psf->dataoffset > 0)
	{	if (psf->have_written)
		{	/*
			** Might be possible to deal with this, but it's difficult as we
			** have to take Ogg Page header sizes in to account, not just
			** packet sizes.
			*/
			return SFE_UNIMPLEMENTED ;
			}
		if (psf_is_pipe (psf))
			return SFE_NOT_SEEKABLE ;
		if (psf_fseek (psf, 0, SEEK_SET) < 0)
			return SFE_SEEK_FAILED ;
		ogg_stream_reset_serialno (&odata->ostream, oopus->serialno) ;
		psf->dataoffset = 0 ;
		}
	else
		opus_print_header (psf, &oopus->header) ;

	psf->header.ptr [0] = 0 ;
	psf->header.indx = 0 ;

	/* Opus Header Marker */
	psf_binheader_writef (psf, "eb", BHWv ("OpusHead"), BHWz (8)) ;

	/* Ogg Embedding scheme version, Channel Count, Preskip Samples */
	psf_binheader_writef (psf, "e112", BHW1 (oopus->header.version), BHW1 (psf->sf.channels), BHW2 (oopus->header.preskip)) ;

	/*
	** If an original samplerate has not been set by the user command
	** SFC_SET_ORIGINAL_SAMPLERATE, write the current samplerate.
	*/
	if (oopus->header.input_samplerate)
		psf_binheader_writef (psf, "e4", BHW4 (oopus->header.input_samplerate)) ;
	else
		psf_binheader_writef (psf, "e4", BHW4 (psf->sf.samplerate)) ;

	/* Input Sample Rate, Gain (S7.8 format), Channel Mapping Type */
	psf_binheader_writef (psf, "e21", BHW2 (oopus->header.gain), BHW1 (oopus->header.channel_mapping)) ;

	/* Channel mappings, required if not using type 0 (mono/stereo) */
	if (oopus->header.channel_mapping > 0)
	{	psf_binheader_writef (psf, "11", BHW1 (oopus->header.nb_streams), BHW1 (oopus->header.nb_coupled)) ;
		for (nn = 0 ; nn < oopus->header.channels ; nn++)
			psf_binheader_writef (psf, "1", BHW1 (oopus->header.stream_map [nn])) ;
		} ;

	op.packet = psf->header.ptr ;
	op.bytes = psf->header.indx ;
	op.b_o_s = 1 ;
	op.e_o_s = 0 ;
	op.granulepos = 0 ;
	op.packetno = 1 ;

	/* The first page MUST only contain the header, so flush it out now */
	ogg_stream_packetin (&odata->ostream, &op) ;
	for ( ; (nn = ogg_stream_flush (&odata->ostream, &odata->opage)) ; )
	{	if (! (nn = ogg_write_page (psf, &odata->opage)))
		{	psf_log_printf (psf, "Opus : Failed to write header!\n") ;
			if (psf->error)
				return psf->error ;
			return SFE_INTERNAL ;
			} ;
		psf->dataoffset += nn ;
		}

	/*
	** Metadata Tags (manditory)
	**
	** All tags must be in one packet, which may span pages, and these pages
	** must not contain any other packets, so flush. The vendor string should
	** be the libopus library version, as it is doing the actual encoding. We
	** put the libsndfile identifier in the ENCODER tag.
	**
	** See: https://wiki.xiph.org/VorbisComment#ENCODER
	*/
	vorbiscomment_write_tags (psf, &op, &opustags_ident, opus_get_version_string (), - (OGG_OPUS_COMMENT_PAD)) ;
	op.packetno = 2 ;
	ogg_stream_packetin (&odata->ostream, &op) ;
	for ( ; (nn = ogg_stream_flush (&odata->ostream, &odata->opage)) ; )
	{	if (! (nn = ogg_write_page (psf, &odata->opage)))
		{	psf_log_printf (psf, "Opus : Failed to write comments!\n") ;
			if (psf->error)
				return psf->error ;
			return SFE_INTERNAL ;
			} ;
		psf->dataoffset += nn ;
		}

	return 0 ;
} /* ogg_opus_write_header */

static void
ogg_opus_flush (SF_PRIVATE *psf)
{	OGG_PRIVATE *odata = (OGG_PRIVATE *) psf->container_data ;
	OPUS_PRIVATE *oopus = (OPUS_PRIVATE *) psf->codec_data ;
	uint64_t last_granulepos ;
	int nbytes ;
	int len ;
	int last_packet ;

	/*
	** Need to flush both samples waiting for a complete packet and samples
	** currently 'inside' the encoder because of its latency. In the case of
	** the latter, we need to encode an equivalent amount of silence to push
	** them out.
	**
	** Note that the last packet's granule position might be less than the
	** total number of samples completed in it. This is how Ogg embedded Opus
	** encodes the amount of appended padding to truncate for gapless playback.
	*/

	last_granulepos = oopus->pkt_pos + (oopus->sr_factor * oopus->loc) + oopus->header.preskip ;
	last_packet = SF_FALSE ;
	memset (&(oopus->buffer [oopus->loc * psf->sf.channels]), 0, sizeof (float) * psf->sf.channels * (oopus->len - oopus->loc)) ;

	for (last_packet = SF_FALSE ; last_packet == SF_FALSE ; )
	{	oopus->pkt_pos += oopus->len * oopus->sr_factor ;
		if (oopus->pkt_pos >= last_granulepos)
		{	last_packet = SF_TRUE ;
			/*
			** Try to shorten the last packet to the smallest valid packet size
			** to minimize padding samples.
			*/
			len = (oopus->len * oopus->sr_factor) - (oopus->pkt_pos - last_granulepos) ;
			if (len <= 120) /* 2.5 ms */
				len = 120 / oopus->sr_factor ;
			else if (len <= 240) /* 5 ms */
				len = 240 / oopus->sr_factor ;
			else if (len <= 480) /* 10 ms */
				len = 480 / oopus->sr_factor ;
			else
				len = oopus->len ;
			}
		else
			len = oopus->len ;

		nbytes = opus_multistream_encode_float (oopus->u.encode.state, oopus->buffer,
					len, odata->opacket.packet, oopus->buffersize) ;

		if (nbytes < 0)
		{	psf_log_printf (psf, "Opus : opus_multistream_encode_float returned: %s\n", opus_strerror (nbytes)) ;
			break ;
			}

		odata->opacket.bytes = nbytes ;
		odata->opacket.packetno++ ;
		if (last_packet)
		{	odata->opacket.granulepos = (ogg_int64_t) last_granulepos ;
			odata->opacket.e_o_s = 1 ;
			}
		else
			odata->opacket.granulepos = (ogg_int64_t) oopus->pkt_pos ;

		ogg_stream_packetin (&odata->ostream, &odata->opacket) ;
		while (ogg_stream_pageout (&odata->ostream, &odata->opage))
			ogg_write_page (psf, &odata->opage) ;
		} ;

	while (ogg_stream_flush (&odata->ostream, &odata->opage))
		ogg_write_page (psf, &odata->opage) ;
} /* ogg_opus_flush */

static int
ogg_opus_calculate_page_duration (OGG_PRIVATE *odata)
{	int i, samples, duration ;
	ogg_packet *ppkt ;

	duration = 0 ;
	for (i = 0 , ppkt = odata->pkt ; i < odata->pkt_len ; i++, ppkt++)
	{	/* Use 48kHz to get the sample count for use with granule positions. */
		samples = opus_packet_get_nb_samples (ppkt->packet, ppkt->bytes, 48000) ;
		if (samples > 0)
			duration += samples ;
		} ;
	return duration ;
} /* ogg_opus_calculate_page_duration */

static int
ogg_opus_unpack_next_page (SF_PRIVATE *psf, OGG_PRIVATE *odata, OPUS_PRIVATE *oopus)
{	int nn ;

	nn = ogg_stream_unpack_page (psf, odata) ;

	if (nn == 1)
	{	oopus->pkt_pos = oopus->pg_pos ;
		oopus->pg_pos = odata->pkt [odata->pkt_len - 1].granulepos ;
		}
	else if (nn == 2)
	{	uint64_t gp, last_page ;

		/* Found a hole. Need to recalculated pkt_pos from pg_pos */
		last_page = oopus->pg_pos ;
		oopus->pg_pos = odata->pkt [odata->pkt_len - 1].granulepos ;
		gp = ogg_opus_calculate_page_duration (odata) ;
		oopus->pkt_pos = oopus->pg_pos - gp ;
		psf_log_printf (psf, "Opus : Hole found appears to be of length %d samples.\n",
				(oopus->pkt_pos - last_page) / oopus->sr_factor) ;
		/*
		** Could save the hole size here, and have ogg_opus_read_refill()
		** do packet loss concealment until the hole is gone, but libopus does
		** PLC by generating white-noise for the duration of the hole. That is
		** the correct thing for use in telephony, but it isn't generally
		** appropriate here. It actually sounds better with no PLC, as the
		** lapped nature of full-width Opus means the two edges of the hole
		** will be blended together.
		*/
		return 1 ;
		}

	return nn ;
} /* ogg_opus_unpack_next_page */

static int
ogg_opus_read_refill (SF_PRIVATE *psf, OGG_PRIVATE *odata, OPUS_PRIVATE *oopus)
{	uint64_t pkt_granulepos ;
	int nn, nsamp ;
	ogg_packet *ppkt ;

	if (odata->pkt_indx == odata->pkt_len)
	{	nn = ogg_opus_unpack_next_page (psf, odata, oopus) ;
		if (nn <= 0)
			return nn ;
		}

	if (odata->pkt_indx == odata->pkt_len)
		return 0 ;

	ppkt = odata->pkt + odata->pkt_indx ;
	nsamp = opus_multistream_decode_float (oopus->u.decode.state,
				ppkt->packet, ppkt->bytes, oopus->buffer, oopus->buffersize, 0) ;

	if (nsamp == OPUS_BUFFER_TOO_SMALL)
	{	nsamp = opus_packet_get_nb_samples (ppkt->packet, ppkt->bytes, psf->sf.samplerate) ;
		psf_log_printf (psf, "Growing decode buffer to hold %d samples from %d\n",
			nsamp, oopus->buffersize) ;
		if (nsamp > 5760)
		{	psf_log_printf (psf, "Packet is larger than maximum allowable of 120ms!? Skipping.\n") ;
			return 0 ;
			} ;
		oopus->buffersize = nsamp ;

		free (oopus->buffer) ;
		oopus->buffer = NULL ;
		oopus->buffer = malloc (sizeof (float) * oopus->buffersize * psf->sf.channels) ;
		if (oopus->buffer == NULL)
		{	psf->error = SFE_MALLOC_FAILED ;
			oopus->buffersize = 0 ;
			return -1 ;
			} ;

		nsamp = opus_multistream_decode_float (oopus->u.decode.state,
				ppkt->packet, ppkt->bytes, oopus->buffer, oopus->buffersize, 0) ;
		} ;
	odata->pkt_indx ++ ;

	if (nsamp < 0)
	{	psf_log_printf (psf, "Opus : opus_multistream_decode returned: %s\n",
			opus_strerror (nsamp)) ;
		psf->error = SFE_INTERNAL ;
		return nsamp ;
		} ;

	/*
	** Check for if this decoded packet is the last of the stream, in
	** which case a page granule position which is shorter than the
	** sample count of all packets in the page indicates that the last
	** samples are padding and should be dropped.
	*/
	pkt_granulepos = oopus->pkt_pos + (nsamp * oopus->sr_factor) ;
	if (pkt_granulepos <= oopus->pg_pos)
	{	oopus->len = nsamp ;
		}
	else
	{	if (ogg_page_eos (&odata->opage))
		{	/*
			** Possible for pg_pos < pkt_pos if there is a trailing
			** packet. It's not supposed to happen, but could.
			*/
			oopus->len = SF_MAX ((int) (oopus->pg_pos - oopus->pkt_pos) / oopus->sr_factor, 0) ;
			}
		else
		{	/*
			** From https://wiki.xiph.org/OggOpus#Granule_Position
			**    A decoder MUST reject as invalid any stream where the granule
			**    position is smaller than the number of samples contained in
			**    packets that complete on the first page with a completed
			**    packet, unless that page has the 'end of stream' flag set. It
			**    MAY defer this action until it decodes the last packet
			**    completed on that page.
			*/
			psf_log_printf (psf, "Opus : Mid-strem page's granule position %d is less than total samples of %d\n", oopus->pg_pos, pkt_granulepos) ;
			psf->error = SFE_MALFORMED_FILE ;
			return -1 ;
			} ;
		} ;

	if (oopus->len > oopus->buffersize)
	{	free (oopus->buffer) ;
		oopus->buffersize = oopus->len ;
		oopus->buffer = malloc (sizeof (float) * oopus->buffersize * psf->sf.channels) ;
		if (oopus->buffer == NULL)
		{	psf->error = SFE_MALLOC_FAILED ;
			oopus->buffersize = 0 ;
			return -1 ;
			} ;
		} ;

	/*
	** Check for if this decoded packet contains samples from before the pre-
	** skip point, indicating that these samples are padding to get the decoder
	** to converge and should be dropped.
	*/
	if (oopus->pkt_pos < (unsigned) oopus->header.preskip)
		oopus->loc = SF_MIN ((oopus->header.preskip - (int) oopus->pkt_pos) / oopus->sr_factor, oopus->len) ;
	else
		oopus->loc = 0 ;

	oopus->pkt_pos = pkt_granulepos ;

	return nsamp ;
} /* ogg_opus_read_refill */

static int
ogg_opus_write_out (SF_PRIVATE *psf, OGG_PRIVATE *odata, OPUS_PRIVATE *oopus)
{	int nbytes ;

	if (oopus->u.encode.lsb != oopus->u.encode.lsb_last)
		opus_multistream_encoder_ctl (oopus->u.encode.state, OPUS_SET_LSB_DEPTH (oopus->u.encode.lsb)) ;

	nbytes = opus_multistream_encode_float (oopus->u.encode.state,
		oopus->buffer, oopus->len,
		odata->opacket.packet, oopus->buffersize) ;

	if (nbytes < 0)
	{	psf_log_printf (psf, "Opus : Error, opus_multistream_encode_float returned: %s\n", opus_strerror (nbytes)) ;
		psf->error = SFE_INTERNAL ;
		return nbytes ;
		} ;

	oopus->u.encode.last_segments += (nbytes + 255) / 255 ;
	oopus->pkt_pos += oopus->len * oopus->sr_factor ;
	odata->opacket.bytes = nbytes ;
	odata->opacket.granulepos = oopus->pkt_pos ;
	odata->opacket.packetno++ ;

	/*
	** Decide whether to flush the Ogg page *before* adding the new packet to
	** it. Check both for if there is more than 1 second of audio (our default
	** Ogg page latency) or if adding the packet would cause a continued page,
	** in which case we might as well make a new page anyways.
	*/
	for ( ; ; )
	{	if (oopus->pkt_pos - oopus->pg_pos >= OGG_OPUS_PAGE_LATENCY || oopus->u.encode.last_segments >= 255)
			nbytes = ogg_stream_flush_fill (&odata->ostream, &odata->opage, 255 * 255) ;
		else
			nbytes = ogg_stream_pageout_fill (&odata->ostream, &odata->opage, 255 * 255) ;
		if (nbytes > 0)
		{	/*
			** LibOgg documentation is noted as being bad by it's author. Ogg
			** page header byte 26 is the segment count.
			*/
			oopus->u.encode.last_segments -= odata->opage.header [26] ;
			oopus->pg_pos = oopus->pkt_pos ;
			ogg_write_page (psf, &odata->opage) ;
			}
		else
			break ;
		} ;

	ogg_stream_packetin (&odata->ostream, &odata->opacket) ;
	oopus->loc = 0 ;
	oopus->u.encode.lsb_last = oopus->u.encode.lsb ;
	oopus->u.encode.lsb = 0 ;

	return 1 ;
} /* ogg_opus_write_out */

static sf_count_t
ogg_opus_read_s (SF_PRIVATE *psf, short *ptr, sf_count_t len)
{	OGG_PRIVATE *odata = (OGG_PRIVATE *) psf->container_data ;
	OPUS_PRIVATE *oopus = (OPUS_PRIVATE *) psf->codec_data ;
	sf_count_t total = 0 ;
	sf_count_t readlen, i ;
	float *iptr ;

	while (total < len)
	{	if (oopus->loc == oopus->len)
		{	if (ogg_opus_read_refill (psf, odata, oopus) <= 0)
				return total ;
			} ;

		readlen = SF_MIN (len - total, (sf_count_t) (oopus->len - oopus->loc) * psf->sf.channels) ;
		if (readlen > 0)
		{	iptr = oopus->buffer + oopus->loc * psf->sf.channels ;
			i = total ;
			total += readlen ;

			if (psf->float_int_mult)
			{	float inverse = 1.0 / psf->float_max ;
				for ( ; i < total ; i++)
				{	ptr [i] = lrintf (((*(iptr++)) * inverse) * 32767.0f) ;
					} ;
				}
			else
			{	for ( ; i < total ; i++)
				{	ptr [i] = lrintf ((*(iptr++)) * 32767.0f) ;
					} ;
				} ;
			oopus->loc += (readlen / psf->sf.channels) ;
			} ;
		} ;
	return total ;
} /* ogg_opus_read_s */

static sf_count_t
ogg_opus_read_i (SF_PRIVATE *psf, int *ptr, sf_count_t len)
{	OGG_PRIVATE *odata = (OGG_PRIVATE *) psf->container_data ;
	OPUS_PRIVATE *oopus = (OPUS_PRIVATE *) psf->codec_data ;
	sf_count_t total = 0 ;
	sf_count_t readlen, i ;
	float *iptr ;

	while (total < len)
	{	if (oopus->loc == oopus->len)
		{	if (ogg_opus_read_refill (psf, odata, oopus) <= 0)
				return total ;
			} ;

		readlen = SF_MIN (len - total, (sf_count_t) (oopus->len - oopus->loc) * psf->sf.channels) ;
		if (readlen > 0)
		{	iptr = oopus->buffer + oopus->loc * psf->sf.channels ;
			i = total ;
			total += readlen ;

			if (psf->float_int_mult)
			{	float inverse = 1.0 / psf->float_max ;
				for ( ; i < total ; i++)
				{	ptr [i] = lrintf (((*(iptr++)) * inverse) * 2147483647.0f) ;
					}
				}
			else
			{	for ( ; i < total ; i++)
				{	ptr [i] = lrintf ((*(iptr++)) * 2147483647.0f) ;
					}
				} ;
			oopus->loc += (readlen / psf->sf.channels) ;
			} ;
		} ;
	return total ;
} /* ogg_opus_read_i */

static sf_count_t
ogg_opus_read_f (SF_PRIVATE *psf, float *ptr, sf_count_t len)
{	OGG_PRIVATE *odata = (OGG_PRIVATE *) psf->container_data ;
	OPUS_PRIVATE *oopus = (OPUS_PRIVATE *) psf->codec_data ;
	sf_count_t total = 0 ;
	sf_count_t readlen ;

	while (total < len)
	{	if (oopus->loc == oopus->len)
		{	if (ogg_opus_read_refill (psf, odata, oopus) <= 0)
				return total ;
			} ;

		readlen = SF_MIN (len - total, (sf_count_t) (oopus->len - oopus->loc) * psf->sf.channels) ;
		if (readlen > 0)
		{	memcpy (&(ptr [total]), &(oopus->buffer [oopus->loc * psf->sf.channels]), sizeof (float) * readlen) ;
			total += readlen ;
			oopus->loc += (readlen / psf->sf.channels) ;
			} ;
		} ;
	return total ;
} /* ogg_opus_read_f */

static sf_count_t
ogg_opus_read_d (SF_PRIVATE *psf, double *ptr, sf_count_t len)
{	OGG_PRIVATE *odata = (OGG_PRIVATE *) psf->container_data ;
	OPUS_PRIVATE *oopus = (OPUS_PRIVATE *) psf->codec_data ;
	sf_count_t total = 0 ;
	sf_count_t readlen, i ;
	float *fptr ;

	while (total < len)
	{	if (oopus->loc >= oopus->len)
		{	if (ogg_opus_read_refill (psf, odata, oopus) <= 0)
				return total ;
			} ;

		readlen = SF_MIN (len - total, (sf_count_t) (oopus->len - oopus->loc) * psf->sf.channels) ;

		if (readlen > 0)
		{	fptr = oopus->buffer + oopus->loc * psf->sf.channels ;
			i = total ;
			total += readlen ;
			for ( ; i < total ; i++)
			{	ptr [i] = *fptr++ ;
				} ;
			oopus->loc += readlen / psf->sf.channels ;
			} ;
		} ;
	return total ;
} /* ogg_opus_read_d */

static sf_count_t
ogg_opus_write_s (SF_PRIVATE *psf, const short *ptr, sf_count_t len)
{	OGG_PRIVATE *odata = (OGG_PRIVATE *) psf->container_data ;
	OPUS_PRIVATE *oopus = (OPUS_PRIVATE *) psf->codec_data ;
	sf_count_t total, i ;
	int writelen ;
	float *optr ;

	if (oopus->u.encode.lsb < 16)
		oopus->u.encode.lsb = 16 ;

	for (total = 0 ; total < len ; )
	{	if (oopus->loc >= oopus->len)
		{	/* Need to encode the buffer */
			if (ogg_opus_write_out (psf, odata, oopus) <= 0)
				return total ;
			} ;

		writelen = SF_MIN (len - total, (sf_count_t) (oopus->len - oopus->loc) * psf->sf.channels) ;
		if (writelen)
		{	optr = oopus->buffer + oopus->loc * psf->sf.channels ;
			i = total ;
			total += writelen ;
			for ( ; i < total ; i++)
			{	*optr++ = (float) (ptr [i]) / 32767.0f ;
				}
			oopus->loc += (writelen / psf->sf.channels) ;
			} ;
		} ;
	return total ;
} /* ogg_opus_write_s */

static sf_count_t
ogg_opus_write_i (SF_PRIVATE *psf, const int *ptr, sf_count_t len)
{	OGG_PRIVATE *odata = (OGG_PRIVATE *) psf->container_data ;
	OPUS_PRIVATE *oopus = (OPUS_PRIVATE *) psf->codec_data ;
	sf_count_t total, i ;
	int writelen ;
	float *optr ;

	if (oopus->u.encode.lsb < 24)
		oopus->u.encode.lsb = 24 ;

	for (total = 0 ; total < len ; )
	{	if (oopus->loc >= oopus->len)
		{	/* Need to encode the buffer */
			if (ogg_opus_write_out (psf, odata, oopus) <= 0)
				return total ;
			} ;

		writelen = SF_MIN (len - total, (sf_count_t) (oopus->len - oopus->loc) * psf->sf.channels) ;
		if (writelen)
		{	optr = oopus->buffer + oopus->loc * psf->sf.channels ;
			i = total ;
			total += writelen ;
			for ( ; i < total ; i++)
			{	*optr++ = (float) (ptr [i]) / 2147483647.0f ;
				} ;
			oopus->loc += (writelen / psf->sf.channels) ;
			} ;
		} ;
	return total ;
} /* ogg_opus_write_i */

static sf_count_t
ogg_opus_write_f (SF_PRIVATE *psf, const float *ptr, sf_count_t len)
{	OGG_PRIVATE *odata = (OGG_PRIVATE *) psf->container_data ;
	OPUS_PRIVATE *oopus = (OPUS_PRIVATE *) psf->codec_data ;
	sf_count_t total ;
	int writelen ;

	if (oopus->u.encode.lsb < 24)
		oopus->u.encode.lsb = 24 ;

	for (total = 0 ; total < len ; )
	{	if (oopus->loc >= oopus->len)
		{	/* Need to encode the buffer */
			if (ogg_opus_write_out (psf, odata, oopus) <= 0)
				return total ;
			} ;

		writelen = SF_MIN (len - total, (sf_count_t) (oopus->len - oopus->loc) * psf->sf.channels) ;
		if (writelen)
		{	memcpy (&(oopus->buffer [oopus->loc * psf->sf.channels]), &(ptr [total]), sizeof (float) * writelen) ;
			total += writelen ;
			oopus->loc += (writelen / psf->sf.channels) ;
			} ;
		} ;
	return total ;
} /* ogg_opus_write_f */

static sf_count_t
ogg_opus_write_d (SF_PRIVATE *psf, const double *ptr, sf_count_t len)
{	OGG_PRIVATE *odata = (OGG_PRIVATE *) psf->container_data ;
	OPUS_PRIVATE *oopus = (OPUS_PRIVATE *) psf->codec_data ;
	sf_count_t total, i ;
	int writelen ;
	float *optr ;

	if (oopus->u.encode.lsb < 24)
		oopus->u.encode.lsb = 24 ;

	for (total = 0 ; total < len ; )
	{	if (oopus->loc >= oopus->len)
		{	/* Need to encode the buffer */
			if (ogg_opus_write_out (psf, odata, oopus) <= 0)
				return total ;
			} ;

		writelen = SF_MIN (len - total, (sf_count_t) (oopus->len - oopus->loc) * psf->sf.channels) ;
		if (writelen)
		{	optr = oopus->buffer + oopus->loc * psf->sf.channels ;
			i = total ;
			total += writelen ;
			for ( ; i < total ; i++)
			{	*optr++ = (float) (ptr [i]) ;
				} ;
			oopus->loc += (writelen / psf->sf.channels) ;
			} ;
		} ;
	return total ;
} /* ogg_opus_write_d */

static int
ogg_opus_analyze_file (SF_PRIVATE *psf)
{	OGG_PRIVATE *odata = (OGG_PRIVATE *) psf->container_data ;
	OPUS_PRIVATE *oopus = (OPUS_PRIVATE *) psf->codec_data ;
	uint64_t gp ;
	sf_count_t saved_offset, last_page ;
	int error ;

	psf->sf.sections = 1 ;
	psf->sf.frames	= SF_COUNT_MAX ;
	oopus->u.decode.gp_end = (uint64_t) -1 ;
	oopus->u.decode.last_offset = SF_COUNT_MAX ;

	psf->dataoffset = ogg_sync_ftell (psf) ;
	if (psf->filelength != SF_COUNT_MAX)
		psf->datalength = psf->filelength - psf->dataoffset ;
	else
		psf->datalength = SF_COUNT_MAX ;

	/*
	** Calculate the start granule position offset
	**
	** OggOpus streams are allowed to start with a granule position other than
	** zero. This allows for cutting the beginning off of streams without
	** having to modify all following granule positions, or for recording/
	** joining a live stream in the middle. To figure out the offset, we need
	** to sum up how many samples are in all the packets that complete in the
	** page and subtract it from the page granule position.
	**
	** If this is the last page of the steam (EOS set), this is not possible,
	** as the granule position may be /less/ than the number of samples, to
	** indicate how many samples are end-padding. In this case the granule
	** position offset of the file must be 0, as otherwise it is considered
	** malformed.
	*/
	error = ogg_opus_unpack_next_page (psf, odata, oopus) ;
	if (error < 0 && psf->error)
		return psf->error ;

	gp = ogg_opus_calculate_page_duration (odata) ;
	if (gp <= 0)
	{	psf_log_printf (psf, "Opus : Page duration of zero!\n") ;
		return SFE_MALFORMED_FILE ;
		} ;

	if (!ogg_page_eos (&odata->opage))
	{	if (gp > oopus->pg_pos)
		{	psf_log_printf (psf, "Opus : First data page's granule position is less than total number of samples on the page!\n") ;
			return SFE_MALFORMED_FILE ;
			}
		oopus->pkt_pos = oopus->pg_pos - gp ;
		}
	else if (gp < oopus->pg_pos)
	{	psf_log_printf (psf, "Opus : First data page is also the last, and granule position has an (ambigious) offset.\n") ;
		return SFE_MALFORMED_FILE ;
		} ;
	oopus->u.decode.gp_start = oopus->pkt_pos ;

	if (!psf->sf.seekable)
		return 0 ;

	/*
	** Find the last page and fetch the last granule position.
	** First, save were we are now.
	*/
	saved_offset = ogg_sync_ftell (psf) ;

	/* This uses the sync page buffer, the stream page buffer is untouched. */
	last_page = ogg_sync_last_page_before (psf, odata, &oopus->u.decode.gp_end, psf->filelength, oopus->serialno) ;
	if (last_page > 0)
	{	if (!ogg_page_eos (&odata->opage))
			psf_log_printf (psf, "Ogg : Last page lacks an end-of-stream bit.\n") ;
		if (last_page + odata->opage.header_len + odata->opage.body_len < psf->filelength)
			psf_log_printf (psf, "Ogg : Junk after the last page.\n") ;
		oopus->u.decode.last_offset = last_page ;

		if (oopus->u.decode.gp_end != (uint64_t) -1)
		{	psf->sf.frames = (oopus->u.decode.gp_end - oopus->u.decode.gp_start
				- oopus->header.preskip) / oopus->sr_factor ;
			} ;
	}

	/* Go back to where we left off. */
	ogg_sync_fseek (psf, saved_offset, SEEK_SET) ;
	return 0 ;
} /* ogg_opus_analyze_file */

/*
** ogg_opus_seek_null_read
**
** Decode samples, doing nothing with them, until the desired granule position
** is reached.
*/
static sf_count_t
ogg_opus_seek_null_read (SF_PRIVATE *psf, sf_count_t offset)
{	OGG_PRIVATE *odata = (OGG_PRIVATE *) psf->container_data ;
	OPUS_PRIVATE *oopus = (OPUS_PRIVATE *) psf->codec_data ;
	sf_count_t total ;
	sf_count_t readlen ;

	total = oopus->pkt_pos / oopus->sr_factor ;
	total += oopus->loc ;

	for ( ; total < offset ; )
	{	if (oopus->loc == oopus->len)
		{	if (ogg_opus_read_refill (psf, odata, oopus) <= 0)
				return total ;
			/*
			** Ignore pre-skip skipping. The preskip was accounted for in the
			** arugment to offset, so we need to count it.
			*/
			oopus->loc = 0 ;
			} ;

		readlen = SF_MIN ((int) (offset - total), (oopus->len - oopus->loc)) ;
		if (readlen > 0)
		{	total += readlen ;
			oopus->loc += readlen ;
			} ;
		} ;
	return total ;
} /* ogg_opus_seek_null_read */

/*
** Search within the file for the page with the highest granule position at or
** before our target.
*/
static int
ogg_opus_seek_page_search (SF_PRIVATE *psf, uint64_t target_gp)
{	OGG_PRIVATE *odata = (OGG_PRIVATE *) psf->container_data ;
	OPUS_PRIVATE *oopus = (OPUS_PRIVATE *) psf->codec_data ;
	uint64_t pcm_start ;
	uint64_t pcm_end ;
	uint64_t best_gp ;
	sf_count_t begin ;
	sf_count_t end ;
	int ret ;

	best_gp = pcm_start = oopus->u.decode.gp_start ;
	pcm_end = oopus->u.decode.gp_end ;
	begin = psf->dataoffset ;

	/* Adjust the target to give time to converge. */
	if (target_gp >= OGG_OPUS_PREROLL)
		target_gp -= OGG_OPUS_PREROLL ;
	if (target_gp < pcm_start)
		target_gp = pcm_start ;

	/* Seek to beginning special case */
	if (target_gp < pcm_start + (uint64_t) oopus->header.preskip)
		end = begin ;
	else
		end = oopus->u.decode.last_offset ;

	ogg_stream_seek_page_search (psf, odata, target_gp, pcm_start, pcm_end, &best_gp, begin, end) ;

	oopus->loc = 0 ;
	oopus->len = 0 ;
	if ((ret = ogg_opus_unpack_next_page (psf, odata, oopus)) != 1)
		return ret ;
	oopus->pkt_pos = best_gp ;
	opus_multistream_decoder_ctl (oopus->u.decode.state, OPUS_RESET_STATE) ;
	/* Gain decoder settings survive resets. */

	return 0 ;
} /* ogg_opus_seek_page_search */

static sf_count_t
ogg_opus_seek_manual (SF_PRIVATE *psf, uint64_t target_gp)
{	OGG_PRIVATE *odata = (OGG_PRIVATE *) psf->container_data ;
	OPUS_PRIVATE *oopus = (OPUS_PRIVATE *) psf->codec_data ;
	sf_count_t pos ;
	int nn ;

	if (target_gp > OGG_OPUS_PREROLL)
		target_gp -= OGG_OPUS_PREROLL ;
	if (target_gp < oopus->pg_pos)
		target_gp = oopus->pg_pos ;

	if (oopus->pg_pos > target_gp)
	{	ogg_stream_reset (&odata->ostream) ;
		pos = ogg_sync_fseek (psf, psf->dataoffset, SEEK_SET) ;
		if (pos < 0)
			return pos ;
		oopus->pg_pos = oopus->u.decode.gp_start ;
		opus_multistream_decoder_ctl (oopus->u.decode.state, OPUS_RESET_STATE) ;
		} ;

	while (oopus->pg_pos < target_gp)
	{	nn = ogg_opus_unpack_next_page (psf, odata, oopus) ;
		if (nn <= 0)
			return nn ;
		} ;

	return 1 ;
} /* ogg_opus_seek_manual */

static sf_count_t
ogg_opus_seek (SF_PRIVATE *psf, int mode, sf_count_t offset)
{	OPUS_PRIVATE *oopus = (OPUS_PRIVATE *) psf->codec_data ;
	uint64_t target_gp ;
	uint64_t current ;
	int ret ;

	/* Only support seeking in read mode. */
	if (mode != SFM_READ || psf->file.mode != SFM_READ)
	{	psf->error = SFE_BAD_SEEK ;
		return PSF_SEEK_ERROR ;
		} ;

	current = oopus->pkt_pos + oopus->loc * oopus->sr_factor ;
	/*
	** Remember, there are preskip granulepos worth of samples at the front of
	** the stream which are bunk. Also, granule positions can be offset.
	*/
	target_gp = offset * oopus->sr_factor + oopus->u.decode.gp_start + oopus->header.preskip ;

	if (oopus->u.decode.gp_end == (uint64_t) -1)
	{	/*
		** Don't know the end of the file. Could be a chained file we don't yet
		** support. Oh well, just do it manually.
		*/
		ogg_opus_seek_manual (psf, target_gp) ;
		}
	else
	{	/*
		** Avoid seeking in the file if where we want is just ahead or exactly
		** were we are. To avoid needing to flush the decoder we choose pre-
		** roll plus 10ms.
		*/
		if (target_gp < current || target_gp - current > OGG_OPUS_PREROLL + 10 * 48)
		{	ret = ogg_opus_seek_page_search (psf, target_gp) ;
			if (ret < 0)
			{	/*
				** Page seek failed, what to do? Could be bad data. We can
				** either fall-back to manual seeking or bail. Manaul seeking
				** from the beginning has the advantage of finding where the
				** file goes bad.
				*/
				ret = ogg_opus_seek_manual (psf, target_gp) ;
				if (ret < 0)
				{	/*
					** If were here, and there is no error, we can be pretty
					** sure that it's the file that is to blame.
					*/
					if (!psf->error)
						psf->error = SFE_MALFORMED_FILE ;
					return ret ;
					} ;
				} ;
			} ;
		} ;

	/*
	** We've seeked or skipped through pages until just before our target,
	** now decode until we hit it.
	*/
	offset = ogg_opus_seek_null_read (psf, target_gp / oopus->sr_factor) ;
	return offset - ((oopus->header.preskip + oopus->u.decode.gp_start) / oopus->sr_factor) ;

} /* ogg_opus_seek */

static int
ogg_opus_command (SF_PRIVATE *psf, int command, void *data, int datasize)
{	OGG_PRIVATE *odata = (OGG_PRIVATE *) psf->container_data ;
	OPUS_PRIVATE *oopus = (OPUS_PRIVATE *) psf->codec_data ;
	double quality ;
	int error ;

	switch (command)
	{	case SFC_SET_CHANNEL_MAP_INFO :
			/* TODO: figure this out */
			break ;

		case SFC_SET_COMPRESSION_LEVEL :
			/*
			** Argument: double, range 0.0 (lest compressed, best quality) to
			** 1.0 (most compressed, worst quality)
			*/
			if (data == NULL || datasize != SIGNED_SIZEOF (double))
				return SFE_BAD_COMMAND_PARAM ;

			/* Usable bitrate range is  [6, 256] kbps per channel. */
			quality = *((double *) data) ;
			oopus->u.encode.bitrate = (int) (((1.0 - quality) * (250000.0)) + 6000.0) * psf->sf.channels ;
			if (opus_multistream_encoder_ctl (oopus->u.encode.state, OPUS_SET_BITRATE (oopus->u.encode.bitrate)) == OPUS_OK)
			{	psf_log_printf (psf, "User changed encoding target bitrate to %dbps\n", oopus->u.encode.bitrate) ;
				return SF_TRUE ;
				}
			psf_log_printf (psf, "Failed to set user encoding target bitrate of %dbps\n", oopus->u.encode.bitrate) ;
			return SF_FALSE ;
			break ;

		case SFC_SET_ORIGINAL_SAMPLERATE :
			if (data == NULL || datasize != SIGNED_SIZEOF (int))
				return SFE_BAD_COMMAND_PARAM ;
			/*
			** Only allow changing the input samplerate if at the beginning
			** of the stream, because while it might be possible to change
			** samplerate mid-decode, or to re-write the header for encode,
			** ain't nobody got time to implement and test that.
			*/
			if (psf->file.mode == SFM_WRITE)
			{	if (psf->have_written)
					return SF_FALSE ;
				oopus->header.input_samplerate = *((int *) data) ;
				}
			else {
				if (oopus->pkt_pos > oopus->u.decode.gp_start || oopus->loc > 0)
					return SF_FALSE ;
				if ((error = ogg_opus_setup_decoder (psf, *((int *) data))))
					return error ;
				odata->pkt_indx = 0 ;
				/* Adjust file frames count. */
				if (oopus->u.decode.gp_end != (uint64_t) -1)
					psf->sf.frames = (oopus->u.decode.gp_end - oopus->u.decode.gp_start
						- oopus->header.preskip) / oopus->sr_factor ;
				} ;
			return SF_TRUE ;

		case SFC_GET_ORIGINAL_SAMPLERATE :
			if (data == NULL || datasize != SIGNED_SIZEOF (int))
				return SFE_BAD_COMMAND_PARAM ;
			*((int *) data) = oopus->header.input_samplerate ;
			return SF_TRUE ;

		default :
			break ;
	}

	return SF_FALSE ;
} /* ogg_opus_command */

static int
ogg_opus_byterate (SF_PRIVATE *psf)
{	OGG_PRIVATE *odata = (OGG_PRIVATE *) psf->container_data ;
	OPUS_PRIVATE *oopus = (OPUS_PRIVATE *) psf->codec_data ;

	if (psf->file.mode == SFM_READ)
	{	if (odata->pkt_indx == odata->pkt_len)
		{	if (ogg_opus_unpack_next_page (psf, odata, oopus) < 0)
				return -1 ;
			} ;

		if (odata->pkt_indx < odata->pkt_len)
		{	ogg_packet *ppkt = &odata->pkt [odata->pkt_indx] ;
			return (ppkt->bytes * 8000) / opus_packet_get_nb_samples (ppkt->packet, ppkt->bytes, 8000) ;
			} ;

		if (psf->datalength != SF_COUNT_MAX)
			return (psf->datalength * psf->sf.samplerate) / psf->sf.frames ;
		} ;

	if (psf->file.mode == SFM_WRITE && oopus->u.encode.state != NULL)
		return (oopus->u.encode.bitrate + 7) / 8 ;

	return -1 ;
} /* ogg_opus_byterate */

#else /* HAVE_EXTERNAL_XIPH_LIBS */

int
ogg_opus_open (SF_PRIVATE *psf)
{
	psf_log_printf (psf, "This version of libsndfile was compiled without Ogg/Opus support.\n") ;
	return SFE_UNIMPLEMENTED ;
} /* ogg_opus_open */

#endif
