/*
** Copyright (C) 2008-2016 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#if (ENABLE_EXPERIMENTAL_CODE && HAVE_EXTERNAL_XIPH_LIBS)

#include <ogg/ogg.h>

#include <speex/speex.h>
#include <speex/speex_stereo.h>
#include <speex/speex_header.h>
#include <speex/speex_callbacks.h>

#include "ogg.h"

#define	OGG_SPX_READ_SIZE	200

typedef struct
{	SpeexBits bits ;

	int32_t serialno ;

	int frame_size, granule_frame_size, nframes ;
	int force_mode ;

	SpeexStereoState stereo ;
	SpeexHeader header ;

	void * state ;
} SPX_PRIVATE ;

static int	spx_read_header (SF_PRIVATE * psf) ;
static int	spx_close (SF_PRIVATE *psf) ;
static void *spx_header_read (SF_PRIVATE * psf, ogg_packet *op, spx_int32_t enh_enabled, int force_mode) ;
static void spx_print_comments (const char *comments, int length) ;

int
ogg_speex_open (SF_PRIVATE *psf)
{	OGG_PRIVATE* odata = psf->container_data ;
	SPX_PRIVATE* spx = calloc (1, sizeof (SPX_PRIVATE)) ;
	int	error = 0 ;

	if (odata == NULL)
	{	psf_log_printf (psf, "%s : odata is NULL???\n", __func__) ;
		return SFE_INTERNAL ;
		} ;

	psf->codec_data = spx ;
	if (spx == NULL)
		return SFE_MALLOC_FAILED ;

	if (psf->file.mode == SFM_RDWR)
		return SFE_BAD_MODE_RW ;

	if (psf->file.mode == SFM_READ)
	{	/* Call this here so it only gets called once, so no memory is leaked. */
		ogg_sync_init (&odata->osync) ;

		if ((error = spx_read_header (psf)))
			return error ;

#if 0
		psf->read_short		= spx_read_s ;
		psf->read_int		= spx_read_i ;
		psf->read_float		= spx_read_f ;
		psf->read_double	= spx_read_d ;
		psf->sf.frames		= spx_length (psf) ;
#endif
		} ;

	psf->codec_close = spx_close ;

	if (psf->file.mode == SFM_WRITE)
	{
#if 0
		/* Set the default spx quality here. */
		vdata->quality = 0.4 ;

		psf->write_header	= spx_write_header ;
		psf->write_short	= spx_write_s ;
		psf->write_int		= spx_write_i ;
		psf->write_float	= spx_write_f ;
		psf->write_double	= spx_write_d ;
#endif

		psf->sf.frames = SF_COUNT_MAX ; /* Unknown really */
		psf->strings.flags = SF_STR_ALLOW_START ;
		} ;

	psf->bytewidth = 1 ;
	psf->blockwidth = psf->bytewidth * psf->sf.channels ;

#if 0
	psf->seek = spx_seek ;
	psf->command = spx_command ;
#endif

	/* FIXME, FIXME, FIXME : Hack these here for now and correct later. */
	psf->sf.format = SF_FORMAT_OGG | SF_FORMAT_SPEEX ;
	psf->sf.sections = 1 ;

	psf->datalength = 1 ;
	psf->dataoffset = 0 ;
	/* End FIXME. */

	return error ;
} /* ogg_speex_open */

#define le_short (x)	(x)

static int
spx_read_header (SF_PRIVATE * psf)
{	static SpeexStereoState STEREO_INIT = SPEEX_STEREO_STATE_INIT ;

	OGG_PRIVATE* odata = psf->container_data ;
	SPX_PRIVATE* spx = psf->codec_data ;

	ogg_int64_t page_granule = 0 ;
	int stream_init = 0 ;
	int	page_nb_packets = 0 ;
	int packet_count = 0 ;
	int enh_enabled = 1 ;
	int force_mode = -1 ;
	char * data ;
	int nb_read ;
	int lookahead ;

printf ("%s %d\n", __func__, __LINE__) ;

	psf_log_printf (psf, "Speex header\n") ;
	odata->eos = 0 ;

	/* Reset ogg stuff which has already been used in src/ogg.c. */
	ogg_stream_reset (&odata->ostream) ;
	ogg_sync_reset (&odata->osync) ;

	/* Seek to start of stream. */
	psf_fseek (psf, 0, SEEK_SET) ;

	/* Initialize. */
	ogg_sync_init (&odata->osync) ;
	speex_bits_init (&spx->bits) ;

	/* Set defaults. */
	psf->sf.channels = -1 ;
	psf->sf.samplerate = 0 ;
	spx->stereo = STEREO_INIT ;

	/* Get a pointer to the ogg buffer and read data into it. */
	data = ogg_sync_buffer (&odata->osync, OGG_SPX_READ_SIZE) ;
	nb_read = psf_fread (data, 1, OGG_SPX_READ_SIZE, psf) ;
	ogg_sync_wrote (&odata->osync, nb_read) ;

	/* Now we chew on Ogg packets. */
	while (ogg_sync_pageout (&odata->osync, &odata->opage) == 1)
	{	if (stream_init == 0)
		{	ogg_stream_init (&odata->ostream, ogg_page_serialno (&odata->opage)) ;
			stream_init = 1 ;
			} ;

		if (ogg_page_serialno (&odata->opage) != odata->ostream.serialno)
		{	/* so all streams are read. */
			ogg_stream_reset_serialno (&odata->ostream, ogg_page_serialno (&odata->opage)) ;
			} ;

		/*Add page to the bitstream*/
		ogg_stream_pagein (&odata->ostream, &odata->opage) ;
		page_granule = ogg_page_granulepos (&odata->opage) ;
		page_nb_packets = ogg_page_packets (&odata->opage) ;

		/*Extract all available packets*/
		while (odata->eos == 0 && ogg_stream_packetout (&odata->ostream, &odata->opacket) == 1)
		{	if (odata->opacket.bytes >= 8 && memcmp (odata->opacket.packet, "Speex   ", 8) == 0)
			{	spx->serialno = odata->ostream.serialno ;
				} ;

			if (spx->serialno == -1 || odata->ostream.serialno != spx->serialno)
				break ;

			if (packet_count == 0)
			{	spx->state = spx_header_read (psf, &odata->opacket, enh_enabled, force_mode) ;
				if (! spx->state)
					break ;

				speex_decoder_ctl (spx->state, SPEEX_GET_LOOKAHEAD, &lookahead) ;
				if (spx->nframes == 0)
					spx->nframes = 1 ;
				}
			else if (packet_count == 1)
			{	spx_print_comments ((const char*) odata->opacket.packet, odata->opacket.bytes) ;
				}
			else if (packet_count < 2 + spx->header.extra_headers)
			{	/* Ignore extra headers */
				}
			packet_count ++ ;
			} ;
		} ;

	psf_log_printf (psf, "End\n") ;

	psf_log_printf (psf, "packet_count %d\n", packet_count) ;
	psf_log_printf (psf, "page_nb_packets %d\n", page_nb_packets) ;
	psf_log_printf (psf, "page_granule %lld\n", page_granule) ;

	return 0 ;
} /* spx_read_header */

static int
spx_close (SF_PRIVATE *psf)
{	SPX_PRIVATE* spx = psf->codec_data ;

	if (spx->state)
		speex_decoder_destroy (spx->state) ;

	if (spx)
		speex_bits_destroy (&spx->bits) ;

	return 0 ;
} /* spx_close */



static void *
spx_header_read (SF_PRIVATE * psf, ogg_packet *op, spx_int32_t enh_enabled, int force_mode)
{	SPX_PRIVATE* spx = psf->codec_data ;
	void *st ;
	const SpeexMode *mode ;
	SpeexHeader *tmp_header ;
	int modeID ;
	SpeexCallback callback ;

	tmp_header = speex_packet_to_header ((char*) op->packet, op->bytes) ;
	if (tmp_header == NULL)
	{	psf_log_printf (psf, "Cannot read Speex header\n") ;
		return NULL ;
		} ;

	memcpy (&spx->header, tmp_header, sizeof (spx->header)) ;
	free (tmp_header) ;
	tmp_header = NULL ;

	if (spx->header.mode >= SPEEX_NB_MODES || spx->header.mode < 0)
	{	psf_log_printf (psf, "Mode number %d does not (yet/any longer) exist in this version\n", spx->header.mode) ;
		return NULL ;
		} ;

	modeID = spx->header.mode ;
	if (force_mode != -1)
		modeID = force_mode ;

	mode = speex_lib_get_mode (modeID) ;

	if (spx->header.speex_version_id > 1)
	{	psf_log_printf (psf, "This file was encoded with Speex bit-stream version %d, which I don't know how to decode\n", spx->header.speex_version_id) ;
		return NULL ;
		} ;

	if (mode->bitstream_version < spx->header.mode_bitstream_version)
	{	psf_log_printf (psf, "The file was encoded with a newer version of Speex. You need to upgrade in order to play it.\n") ;
		return NULL ;
		} ;

	if (mode->bitstream_version > spx->header.mode_bitstream_version)
	{	psf_log_printf (psf, "The file was encoded with an older version of Speex. You would need to downgrade the version in order to play it.\n") ;
		return NULL ;
		} ;

	st = speex_decoder_init (mode) ;
	if (!st)
	{	psf_log_printf (psf, "Decoder initialization failed.\n") ;
		return NULL ;
		} ;

	speex_decoder_ctl (st, SPEEX_SET_ENH, &enh_enabled) ;
	speex_decoder_ctl (st, SPEEX_GET_FRAME_SIZE, &spx->frame_size) ;
	spx->granule_frame_size = spx->frame_size ;

	if (!psf->sf.samplerate)
		psf->sf.samplerate = spx->header.rate ;
	/* Adjust rate if --force-* options are used */
	if (force_mode != -1)
	{	if (spx->header.mode < force_mode)
		{	psf->sf.samplerate <<= (force_mode - spx->header.mode) ;
			spx->granule_frame_size >>= (force_mode - spx->header.mode) ;
			} ;
		if (spx->header.mode > force_mode)
		{	psf->sf.samplerate >>= (spx->header.mode - force_mode) ;
			spx->granule_frame_size <<= (spx->header.mode - force_mode) ;
			} ;
		} ;

	speex_decoder_ctl (st, SPEEX_SET_SAMPLING_RATE, &psf->sf.samplerate) ;

	spx->nframes = spx->header.frames_per_packet ;

	if (psf->sf.channels == -1)
		psf->sf.channels = spx->header.nb_channels ;

	if (! (psf->sf.channels == 1))
	{	psf->sf.channels = 2 ;
		callback.callback_id = SPEEX_INBAND_STEREO ;
		callback.func = speex_std_stereo_request_handler ;
		callback.data = &spx->stereo ;
		speex_decoder_ctl (st, SPEEX_SET_HANDLER, &callback) ;
		} ;

	spx->header.speex_version [sizeof (spx->header.speex_version) - 1] = 0 ;

	psf_log_printf (psf, "  Encoder ver   : %s\n  Frames/packet : %d\n",
					spx->header.speex_version, spx->header.frames_per_packet) ;

	if (spx->header.bitrate > 0)
		psf_log_printf (psf, "  Bit rate	  : %d\n", spx->header.bitrate) ;

	psf_log_printf (psf, "  Sample rate   : %d\n  Mode		  : %s\n  VBR		   : %s\n  Channels	  : %d\n",
					psf->sf.samplerate, mode->modeName, (spx->header.vbr ? "yes" : "no"), psf->sf.channels) ;

	psf_log_printf (psf, "  Extra headers : %d\n", spx->header.extra_headers) ;

	return st ;
} /* spx_header_read */


static void
spx_print_comments (const char *c, int length)
{
	const char *end ;
	int len, i, nb_fields ;

printf ("%s %d\n", __func__, __LINE__) ;
	if (length < 8)
	{	fprintf (stderr, "Invalid/corrupted comments\n") ;
		return ;
		}
	end = c + length ;
	len = readint (c, 0) ;
	c += 4 ;
	if (len < 0 || c + len > end)
	{	fprintf (stderr, "Invalid/corrupted comments\n") ;
		return ;
		}
	(void) fwrite (c, 1, len, stderr) ;
	c += len ;
	fprintf (stderr, "\n") ;
	if (c + 4 > end)
	{	fprintf (stderr, "Invalid/corrupted comments\n") ;
		return ;
		}
	nb_fields = readint (c, 0) ;
	c += 4 ;
	for (i = 0 ; i < nb_fields ; i++)
	{	if (c + 4 > end)
		{	fprintf (stderr, "Invalid/corrupted comments\n") ;
			return ;
			} ;
		len = readint (c, 0) ;
		c += 4 ;
		if (len < 0 || c + len > end)
		{	fprintf (stderr, "Invalid/corrupted comments\n") ;
			return ;
			}
		(void) fwrite (c, 1, len, stderr) ;
		c += len ;
		fprintf (stderr, "\n") ;
		} ;
	return ;
} /* spx_print_comments */


/*
encoded_speex_frames = (frames_per_packet * Packets)
					 = 1 * 272
					 = 272

audio_samples = encoded_speex_frames * frame_size
			  = 272 * 640
			  = 174080

duration = audio_samples / rate
		 = 174080 / 44100
		 = 3.947
*/

#else /* ENABLE_EXPERIMENTAL_CODE && HAVE_EXTERNAL_XIPH_LIBS */

int
ogg_speex_open (SF_PRIVATE *psf)
{
	psf_log_printf (psf, "This version of libsndfile was compiled without Ogg/Speex support.\n") ;
	return SFE_UNIMPLEMENTED ;
} /* ogg_speex_open */

#endif
