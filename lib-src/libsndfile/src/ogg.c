/*
** Copyright (C) 2002-2019 Erik de Castro Lopo <erikd@mega-nerd.com>
** Copyright (C) 2007 John ffitch
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

#include "ogg.h"

#define OGG_SYNC_READ_SIZE (2048)
#define OGG_PAGE_SIZE_MAX (65307)
#define OGG_CHUNK_SIZE (65536)
#define OGG_CHUNK_SIZE_MAX (1024*1024)

/*
 * The Ogg container may seem overly complicated, particularly when used for a
 * on-disk audio file format. This is probably because Ogg is designed with
 * streaming rather than storage as a priority, and can handle multiple codec
 * payloads multiplexed together, then possibly chained on top of that.
 * Ogg achieves its goals well, but it does lend to a bit of a learning curve,
 * with many internal structures to push data around in compared to most sound
 * file formats which only have a header and raw data.
 *
 * See
 *  - [https://xiph.org/ogg/doc/oggstream.html]
 *  - [https://xiph.org/ogg/doc/framing.html]
 *
 * libogg Memory Management
 * ===========================================================================
 *
 * libOgg's memory management is documented in code, not in headers or external
 * documentation. What follows is not an attempt to completely document it, but
 * an explanation of the basics.
 *
 * libOgg has two data structures which allocate and manage data buffers: The
 * ogg_sync_state structure and the ogg_stream_state structure. The remaining
 * structures of ogg_page and ogg_packet are views into the buffers managed by
 * the previous structures.
 *
 * ogg_sync_state is used for reading purposes. It takes a physical bitstream
 * and searches for, validates, and returns complete Ogg Pages. The
 * ogg_sync_state buffers the returned page data, holding at most one
 * complete page at a time. A returned Ogg page remains valid until any
 * operation other than ogg_sync_check() is called.
 *
 * ogg_stream_state is used for both reading and writing. For reading, the
 * contents of an ogg_page is copied into the stream state. This data is
 * buffered to be split or joined as necessary into complete ogg_packets. If,
 * after copying an ogg_page into an ogg_stream_state, packets are available to
 * be read, then all of those packets remain in memory and valid until either
 * the ogg_stream_state is reset, destroyed, or a new ogg_page is read into it.
 * As the maximum number of packets an Ogg Page may contain is 255, at most 255
 * packets may be available from an ogg_stream_state at one time.
 *
 * For writing, the life cycle of a buffer pointed to by a ogg_packet is the
 * responsibility of the caller. Packets written into an ogg_stream_state are
 * buffered until a complete page is ready for writing. Pages for writing out
 * remain in the ogg_stream_state's buffer and valid until either the
 * ogg_stream_state is reset, cleared, destroyed. Writing another packet into
 * the ogg_stream_state might also invalidate such pages, but writing in
 * packets when a page is ready to be written out is a caller bug anyways.
 */

/*-----------------------------------------------------------------------------------------------
** Private function prototypes.
*/

static int		ogg_close (SF_PRIVATE *psf) ;
static int		ogg_stream_classify (SF_PRIVATE *psf, OGG_PRIVATE * odata) ;
static int		ogg_page_classify (SF_PRIVATE * psf, const ogg_page * og) ;
static uint64_t	ogg_page_search_do_rescale (uint64_t x, uint64_t from, uint64_t to) ;
static void		ogg_page_search_continued_data (OGG_PRIVATE *odata, ogg_page *page) ;

/*-----------------------------------------------------------------------------------------------
** Exported functions.
*/

int
ogg_read_first_page (SF_PRIVATE *psf, OGG_PRIVATE *odata)
{	int ret ;
	char *buffer ;

	/*
	** The ogg standard requires that the first pages of a physical ogg
	** bitstream be only the first pages of each logical bitstream. These
	** pages MUST have the Beginning-Of-Stream bit set, and must contain
	** only the stream's relevant header. Currently we only load the first
	** page and check that it contains a codec we support as supporting
	** multiplexed streams (video+audio(en)+audio(fs)+subtitles, etc) is
	** beyond the scope of this library.
	*/

	ret = ogg_sync_fseek (psf, psf->header.indx, SEEK_SET) ;
	if (ret < 0)
		return SFE_NOT_SEEKABLE ;

	buffer = ogg_sync_buffer (&odata->osync, psf->header.indx) ;
	memcpy (buffer, psf->header.ptr, psf->header.indx) ;
	ogg_sync_wrote (&odata->osync, psf->header.indx) ;

	ret = ogg_sync_next_page (psf, &odata->opage, SF_MAX (0l, 4096 - psf->header.indx), NULL) ;

	/* Have we simply run out of data?  If so, we're done. */
	if (ret == 0)
		return 0 ;
	if (ret < 0)
		return psf->error ;

	if (!ogg_page_bos (&odata->opage))
	{	/*
		** Error case. Either must not be an Ogg bitstream, or is in the
		** middle of a bitstream (live capture), or in the middle of a
		** bitstream and no complete page was in the buffer.
		*/
		psf_log_printf (psf, "Input does not appear to be the start of an Ogg bitstream.\n") ;
		return SFE_MALFORMED_FILE ;
		} ;

	/*
	**	Get the serial number and set up the rest of decode.
	**	Serialno first ; use it to set up a logical stream.
	*/
	ogg_stream_reset_serialno (&odata->ostream, ogg_page_serialno (&odata->opage)) ;

	if (ogg_stream_pagein (&odata->ostream, &odata->opage) < 0)
	{	/* Error ; stream version mismatch perhaps. */
		psf_log_printf (psf, "Error reading first page of Ogg bitstream data\n") ;
		return SFE_MALFORMED_FILE ;
		} ;

	if (ogg_stream_packetout (&odata->ostream, &odata->opacket) != 1)
	{	/* No page? */
		psf_log_printf (psf, "Error reading initial header page packet.\n") ;
		return SFE_MALFORMED_FILE ;
		} ;

	return 0 ;
} /* ogg_read_first_page */

int
ogg_write_page (SF_PRIVATE *psf, ogg_page *page)
{	int bytes ;

	bytes = psf_fwrite (page->header, 1, page->header_len, psf) ;
	bytes += psf_fwrite (page->body, 1, page->body_len, psf) ;

	return bytes == page->header_len + page->body_len ;
} /* ogg_write_page */

sf_count_t
ogg_sync_ftell (SF_PRIVATE *psf)
{	OGG_PRIVATE* odata = (OGG_PRIVATE *) psf->container_data ;
	sf_count_t position ;

	position = psf_ftell (psf) ;
	if (position >= 0)
	{	/* success */
		if (position < odata->osync.fill)
		{	/* Really, this should be an assert. */
			psf->error = SFE_INTERNAL ;
			return -1 ;
			}
		position += (sf_count_t) (odata->osync.returned - odata->osync.fill) ;
		}

	return position ;
} /* ogg_sync_ftell */

sf_count_t
ogg_sync_fseek (SF_PRIVATE *psf, sf_count_t offset, int whence)
{	OGG_PRIVATE* odata = (OGG_PRIVATE *) psf->container_data ;
	sf_count_t ret ;

	ret = psf_fseek (psf, offset, whence) ;
	if (ret >= 0)
	{	/* success */
		odata->eos = 0 ;
		ogg_sync_reset (&odata->osync) ;
		}

	return ret ;
} /* ogg_sync_fseek */

int
ogg_sync_next_page (SF_PRIVATE * psf, ogg_page *og, sf_count_t readmax, sf_count_t *offset)
{	OGG_PRIVATE* odata = (OGG_PRIVATE *) psf->container_data ;
	sf_count_t position, nb_read, read_ret ;
	unsigned char *buffer ;
	int synced ;
	int report_hole = 0 ;

	for (position = 0 ; readmax <= 0 || readmax > position ; )
	{	synced = ogg_sync_pageseek (&odata->osync, og) ;
		if (synced < 0)
		{	/*
			** Skipped -synced bytes before finding the start of a page.
			** If seeking, we have just landed in the middle of a page.
			** Otherwise, warn about junk in the bitstream.
			** Page might not yet be ready, hence the continue.
			*/
			if (!offset)
				report_hole = 1 ;
			position -= synced ;
			continue ;
			} ;

		if (report_hole)
		{	psf_log_printf (psf, "Ogg : Skipped %d bytes looking for the next page. Corrupted bitstream?!\n", position) ;
			report_hole = 0 ;
			} ;

		if (synced > 0)
		{	/* Have a page */
			if (offset)
				*offset += position ;
			return og->header_len + og->body_len ;
			} ;

		/*
		** Else readmax == 0, Out of data. Try to read more in without
		** invalidating our boundary (readmax) constraint.
		*/
		if (readmax == 0)
			return 0 ;
		if (readmax > 0)
			nb_read = SF_MIN ((sf_count_t) OGG_SYNC_READ_SIZE, readmax - position) ;
		else
			nb_read = OGG_SYNC_READ_SIZE ;
		buffer = (unsigned char *) ogg_sync_buffer (&odata->osync, nb_read) ;
		read_ret = psf_fread (buffer, 1, nb_read, psf) ;
		if (read_ret == 0)
			return psf->error ? -1 : 0 ;
		ogg_sync_wrote (&odata->osync, read_ret) ;
		} ;
	return 0 ;
} /* ogg_sync_next_page */

int
ogg_stream_next_page (SF_PRIVATE *psf, OGG_PRIVATE *odata)
{	int nn ;

	if (odata->eos)
		return 0 ;

	for ( ; ; )
	{	nn = ogg_sync_next_page (psf, &odata->opage, -1, NULL) ;
		if (nn == 0)
		{	psf_log_printf (psf, "Ogg : File ended unexpectedly without an End-Of-Stream flag set.\n") ;
			odata->eos = 1 ;
			}
		if (nn <= 0)
			return nn ;

		if (ogg_page_serialno (&odata->opage) == odata->ostream.serialno)
			break ;
		} ;

	if (ogg_page_eos (&odata->opage))
		odata->eos = 1 ;

	if (ogg_stream_pagein (&odata->ostream, &odata->opage) < 0)
	{	psf->error = SFE_INTERNAL ;
		return -1 ;
		}

	return 1 ;
} /* ogg_stream_next_page */

int
ogg_stream_unpack_page (SF_PRIVATE *psf, OGG_PRIVATE *odata)
{	int nn ;
	int i ;
	int found_hole = 0 ;
	ogg_packet *ppkt = odata->pkt ;

	odata->pkt_indx = 0 ;
	nn = ogg_stream_packetout (&odata->ostream, ppkt) ;
	if (nn == 0)
	{	/*
		** Steam is out of packets. Read in more pages until there is one, or
		** the stream ends, or an error occurs.
		*/
		for ( ; nn == 0 ; nn = ogg_stream_packetout (&odata->ostream, ppkt))
		{	nn = ogg_stream_next_page (psf, odata) ;
			if (nn <= 0)
			{	odata->pkt_len = 0 ;
				return nn ;
				}
			}
		/*
		** In the case of the for loop exiting because
		** ogg_stream_packetout() == -1, fall-through.
		*/
		}

	if (nn == -1)
	{	/*
		** libOgg found a hole. That is, the next packet found was out of
		** sequence. As such, "flush" the hole marker by removing the invalid
		** packet, as the valid packets are queued behind it.
		*/
		psf_log_printf (psf, "Ogg : Warning, libogg reports a hole at %d bytes.\n", ogg_sync_ftell (psf)) ;
		nn = ogg_stream_packetout (&odata->ostream, ppkt) ;
		found_hole = 1 ;
		}

	/*
	** Unpack all the packets on the page. It is undocumented (like much of
	** libOgg behavior) but all packets from a page read into the stream are
	** guarenteed to remain valid in memory until a new page is read into the
	** stream.
	*/
	for (i = 1 ; ; i++)
	{	/* Not an off-by-one, there are 255 not 256 packets max. */
		if (i == 255)
		{	if (ogg_stream_packetpeek (&odata->ostream, NULL) == 1)
			{	psf->error = SFE_INTERNAL ;
				return -1 ;
				}
			break ;
			}
		if (ogg_stream_packetout (&odata->ostream, ++ ppkt) != 1)
			break ;
		}
	odata->pkt_len = i ;

	/* 1 = ok, 2 = ok, and found a hole. */
	return 1 + found_hole ;
} /* ogg_stream_unpack_page */

sf_count_t
ogg_sync_last_page_before (SF_PRIVATE *psf, OGG_PRIVATE *odata, uint64_t *gp_out, sf_count_t offset, int32_t serialno)
{	sf_count_t begin, end, original_end, chunk_size, ret ;
	sf_count_t position = 0 ;
	uint64_t gp = -1 ;
	int left_link ;

	/* Based on code from Xiph.org's Opusfile */

	original_end = end = begin = offset ;
	offset = -1 ;
	chunk_size = OGG_CHUNK_SIZE ;
	do
	{	begin = SF_MAX (begin - chunk_size, (sf_count_t) 0) ;
		position = ogg_sync_fseek (psf, begin, SEEK_SET) ;
		if (position < 0)
			return position ;
		left_link = 0 ;
		while (position < end)
		{	ret = ogg_sync_next_page (psf, &odata->opage, end - position, &position) ;
			if (ret <= 0)
				return -1 ;
			if (ogg_page_serialno (&odata->opage) == serialno)
			{	uint64_t page_gp = ogg_page_granulepos (&odata->opage) ;
				if (page_gp != (uint64_t) -1)
				{	offset = position ;
					gp = page_gp ;
					}
				}
			else
				left_link = 1 ;
			position += ret ;
			}

		if ((left_link || !begin) && offset < 0)
		{	psf->error = SFE_MALFORMED_FILE ;
			return -1 ;
			}

		chunk_size = SF_MIN (2 * chunk_size, (sf_count_t) OGG_CHUNK_SIZE_MAX) ;
		end = SF_MIN (begin + OGG_PAGE_SIZE_MAX - 1, original_end) ;
		}
	while (offset < 0) ;

	*gp_out = gp ;
	return offset ;
} /* ogg_sync_last_page_before */

int
ogg_stream_seek_page_search (SF_PRIVATE *psf, OGG_PRIVATE *odata, uint64_t target_gp, uint64_t pcm_start, uint64_t pcm_end, uint64_t *best_gp, sf_count_t begin, sf_count_t end)
{	ogg_page page ;
	uint64_t gp ;
	sf_count_t d0, d1, d2 ;
	sf_count_t best ;
	sf_count_t best_start ;
	sf_count_t boundary ;
	sf_count_t next_boundary ;
	sf_count_t page_offset = -1 ;
	sf_count_t seek_pos = -1 ;
	sf_count_t bisect ;
	sf_count_t chunk_size ;
	int buffering = SF_FALSE ;
	int force_bisect = SF_FALSE ;
	int ret ;
	int has_packets ;

	*best_gp = pcm_start ;
	best = best_start = begin ;
	boundary = end ;

	ogg_stream_reset_serialno (&odata->ostream, odata->ostream.serialno) ;

	/*
	** This code is based on op_pcm_seek_page() from Opusfile, which is in turn
	** based on "new search algorithm by Nicholas Vinen" from libvorbisfile.
	*/

	d2 = d1 = d0 = end - begin ;
	while (begin < end)
	{	/*
		** Figure out if and where to try and seek in the file.
		*/
		if (end - begin < OGG_CHUNK_SIZE)
			bisect = begin ;
		else
		{	/* Update the interval size history */
			d0 = d1 >> 1 ;
			d1 = d2 >> 1 ;
			d2 = (end - begin) >> 1 ;
			if (force_bisect == SF_TRUE)
				bisect = begin + ((end - begin) >> 1) ;
			else
			{	/* Take a decent guess. */
				bisect = begin + ogg_page_search_do_rescale (target_gp - pcm_start, pcm_end - pcm_start, end - begin) ;
				}
			if (bisect - OGG_CHUNK_SIZE < begin)
				bisect = begin ;
			else
				bisect -= OGG_CHUNK_SIZE ;
			force_bisect = SF_FALSE ;
			}

		/*
		** Avoid an actual fseek if we can (common for final iterations.)
		*/
		if (seek_pos != bisect)
		{	if (buffering == SF_TRUE)
				ogg_stream_reset (&odata->ostream) ;
			buffering = SF_FALSE ;
			page_offset = -1 ;
			seek_pos = ogg_sync_fseek (psf, bisect, SEEK_SET) ;
			if (seek_pos < 0)
				return seek_pos ;
			}

		chunk_size = OGG_CHUNK_SIZE ;
		next_boundary = boundary ;

		/*
		** Scan forward, figure out where we landed.
		** The ideal case is we see a page that ends before our target followed
		** by a page that ends after our target.
		** If we are too far before or after, breaking out will bisect what we
		** have found so far.
		*/
		while (begin < end)
		{	ret = ogg_sync_next_page (psf, &page, boundary - seek_pos, &seek_pos) ;
			if (ret <= 0)
				return ret ;
			page_offset = seek_pos ;
			if (ret == 0)
			{	/*
				** There are no more pages in this interval from our stream
				** with a granulepos less than our target.
				*/
				if (bisect <= begin + 1)
				{	/* Scanned the whole interval, so we are done. */
					end = begin ;
					}
				else
				{	/*
					** Otherwise, back up one chunk. First discard any data
					** from a continued packet.
					*/
					if (buffering)
						ogg_stream_reset (&odata->ostream) ;
					buffering = SF_FALSE ;
					bisect = SF_MAX (bisect - chunk_size, begin) ;
					seek_pos = ogg_sync_fseek (psf, bisect, SEEK_SET) ;
					if (seek_pos < 0)
						return seek_pos ;
					/* Bump up the chunk size. */
					chunk_size = SF_MIN (2 * chunk_size, (sf_count_t) OGG_CHUNK_SIZE_MAX) ;
					/*
					** If we did find a page from another stream or without a
					** timestamp, don't read past it.
					*/
					boundary = next_boundary ;
					}
				continue ;
				}

			/* Found a page. Advance seek_pos past it */
			seek_pos += page.header_len + page.body_len ;
			/*
			** Save the offset of the first page we found after the seek,
			** regardless of the stream it came from or whether or not it has a
			** timestamp.
			*/
			next_boundary = SF_MIN (page_offset, next_boundary) ;

			/* If not from our stream, continue. */
			if (odata->ostream.serialno != (uint32_t) ogg_page_serialno (&page))
				continue ;

			/*
			** The Ogg spec says that a page with a granule pos of -1 must not
			** contain and packets which complete, but the lack of biconditional
			** wording means that /technically/ a packet which does not complete
			** any packets can have a granule pos other than -1. To make matters
			** worse, older versions of libogg did just that.
			*/
			has_packets = ogg_page_packets (&page) > 0 ;
			gp = has_packets ? ogg_page_granulepos (&page) : -1 ;
			if (gp == (uint64_t) -1)
			{	if (buffering == SF_TRUE)
				{	if (!has_packets)
						ogg_stream_pagein (&odata->ostream, &page) ;
					else
					{	/*
						** If packets did end on this page, but we still didn't
						** have a valid granule position (in violation of the
						** spec!), stop buffering continued packet data.
						** Otherwise we might continue past the packet we
						** actually wanted.
						*/
						ogg_stream_reset (&odata->ostream) ;
						buffering = SF_FALSE ;
						}
					}
				continue ;
				}

			if (gp < target_gp)
			{	/*
				** We found a page that ends before our target. Advance to
				** the raw offset of the next page.
				*/
				begin = seek_pos ;
				if (pcm_start > gp || pcm_end < gp)
					break ;
				/* Save the byte offset of after this page. */
				best = best_start = begin ;
				if (buffering)
					ogg_stream_reset (&odata->ostream) ;
				/* Check to see if the last packet continues. */
				if (page.header [27 + page.header [26] - 1] == 255)
				{	ogg_page_search_continued_data (odata, &page) ;
					/*
					** If we have a continued packet, remember the offset of
					** this page's start, so that if we do wind up having to
					** seek back here later, we can prime the stream with the
					** continued packet data. With no continued packet, we
					** remember the end of the page.
					*/
					best_start = page_offset ;
					} ;
				/*
				** Then force buffering on, so that if a packet starts (but
				** does not end) on the next page, we still avoid the extra
				** seek back.
				*/
				buffering = SF_TRUE ;
				*best_gp = pcm_start = gp ;
				if (target_gp - gp > 48000)
				{	/* Out by over a second. Try another bisection. */
					break ;
					}
				/* Otherwise, keep scanning forward (do NOT use begin+1). */
				bisect = begin ;
				}
			else
			{	/*
				** Found a page that ends after our target. If we had just
				** scanned the whole interval before we found it, we're good.
				*/
				if (bisect <= begin + 1)
					end = begin ;
				else
				{	end = bisect ;
					/*
					** In later iterations, don't read past the first page we
					** found.
					*/
					boundary = next_boundary ;
					/*
					** If we're not making much progress shrinking the interval
					** size, start forcing straight bisection to limit the
					** worst case.
					*/
					force_bisect = end - begin > d0 * 2 ? SF_TRUE : SF_FALSE ;
					/*
					** Don't let pcm_end get out of range! That could happen
					** with an invalid timestamp.
					*/
					if (pcm_end > gp && pcm_start <= gp)
						pcm_end = gp ;
					}
				break ;
				}
			}
		}

	/*
	** If we are buffering, the page we want is currently buffered in the
	** Ogg stream structure, or in the Ogg page which has not been submitted.
	** If not, we need to seek back and load it again.
	*/
	if (buffering == SF_FALSE)
	{	if (best_start != page_offset)
		{	page_offset = -1 ;
			seek_pos = ogg_sync_fseek (psf, best_start, SEEK_SET) ;
			if (seek_pos < 0)
				return seek_pos ;
			}
		if (best_start < best)
		{	if (page_offset < 0)
			{	ret = ogg_sync_next_page (psf, &page, -1, &seek_pos) ;
				if (seek_pos != best_start)
					return -1 ;
				}
			ogg_page_search_continued_data (odata, &page) ;
			page_offset = -1 ;
			}
		} ;

	if (page_offset >= 0)
		ogg_stream_pagein (&odata->ostream, &page) ;

	return 0 ;
} /* ogg_stream_seek_page_search */

int
ogg_open (SF_PRIVATE *psf)
{	OGG_PRIVATE* odata = calloc (1, sizeof (OGG_PRIVATE)) ;
	sf_count_t pos = psf_ftell (psf) ;
	int	error = 0 ;

	psf->container_data = odata ;
	psf->container_close = ogg_close ;

	if (psf->file.mode == SFM_RDWR)
		return SFE_BAD_MODE_RW ;

	if (psf->file.mode == SFM_READ)
		if ((error = ogg_stream_classify (psf, odata)) != 0)
			return error ;

	if (SF_ENDIAN (psf->sf.format) != 0)
		return SFE_BAD_ENDIAN ;

	switch (psf->sf.format)
	{	case SF_FORMAT_OGG | SF_FORMAT_VORBIS :
			return ogg_vorbis_open (psf) ;

		case SF_FORMAT_OGGFLAC :
			/* Reset everything to an initial state. */
			ogg_sync_clear (&odata->osync) ;
			ogg_stream_clear (&odata->ostream) ;
			psf_fseek (psf, pos, SEEK_SET) ;
			free (psf->container_data) ;
			psf->container_data = NULL ;
			psf->container_close = NULL ;
			return flac_open (psf) ;

		case SF_FORMAT_OGG | SF_FORMAT_OPUS :
			return ogg_opus_open (psf) ;

#if ENABLE_EXPERIMENTAL_CODE
		case SF_FORMAT_OGG | SF_FORMAT_SPEEX :
			return ogg_speex_open (psf) ;

		case SF_FORMAT_OGG | SF_FORMAT_PCM_16 :
		case SF_FORMAT_OGG | SF_FORMAT_PCM_24 :
			return ogg_pcm_open (psf) ;
#endif

		default :
			break ;
		} ;

	psf_log_printf (psf, "%s : bad psf->sf.format 0x%x.\n", __func__, psf->sf.format) ;
	return SFE_INTERNAL ;
} /* ogg_open */

/*==============================================================================
** Private functions.
*/

static int
ogg_close (SF_PRIVATE *psf)
{	OGG_PRIVATE* odata = psf->container_data ;

	ogg_sync_clear (&odata->osync) ;
	ogg_stream_clear (&odata->ostream) ;

	return 0 ;
} /* ogg_close */

static int
ogg_stream_classify (SF_PRIVATE *psf, OGG_PRIVATE* odata)
{	int error ;

	/* Call this here so it only gets called once, so no memory is leaked. */
	ogg_sync_init (&odata->osync) ;
	ogg_stream_init (&odata->ostream, 0) ;

	/* Load the first page in the physical bitstream. */
	if ((error = ogg_read_first_page (psf, odata)) != 0)
		return error ;

	odata->codec = ogg_page_classify (psf, &odata->opage) ;

	switch (odata->codec)
	{	case OGG_VORBIS :
			psf->sf.format = SF_FORMAT_OGG | SF_FORMAT_VORBIS ;
			return 0 ;

		case OGG_FLAC :
		case OGG_FLAC0 :
			psf->sf.format = SF_FORMAT_OGGFLAC ;
			return 0 ;

		case OGG_SPEEX :
			psf->sf.format = SF_FORMAT_OGG | SF_FORMAT_SPEEX ;
			return 0 ;

		case OGG_OPUS :
			psf->sf.format = SF_FORMAT_OGG | SF_FORMAT_OPUS ;
			return 0 ;

		case OGG_PCM :
			psf_log_printf (psf, "Detected Ogg/PCM data. This is not supported yet.\n") ;
			return SFE_UNIMPLEMENTED ;

		default :
			break ;
		} ;

	psf_log_printf (psf, "This Ogg bitstream contains some uknown data type.\n") ;
	return SFE_UNIMPLEMENTED ;
} /* ogg_stream_classify */

/*==============================================================================
*/

static struct
{	const char *str, *name ;
	int len, codec ;
} codec_lookup [] =
{	{	"Annodex",		"Annodex",	8, OGG_ANNODEX },
	{	"AnxData",		"AnxData",	7, OGG_ANXDATA },
	{	"\177FLAC",		"Flac1",	5, OGG_FLAC },
	{	"fLaC",			"Flac0",	4, OGG_FLAC0 },
	{	"PCM     ",		"PCM",		8, OGG_PCM },
	{	"Speex",		"Speex",	5, OGG_SPEEX },
	{	"\001vorbis",	"Vorbis",	7, OGG_VORBIS },
	{	"OpusHead",		"Opus",		8, OGG_OPUS },
} ;

static int
ogg_page_classify (SF_PRIVATE * psf, const ogg_page * og)
{	int k, len ;

	for (k = 0 ; k < ARRAY_LEN (codec_lookup) ; k++)
	{	if (codec_lookup [k].len > og->body_len)
			continue ;

		if (memcmp (og->body, codec_lookup [k].str, codec_lookup [k].len) == 0)
		{	psf_log_printf (psf, "Ogg stream data : %s\n", codec_lookup [k].name) ;
			psf_log_printf (psf, "Stream serialno : %u\n", (uint32_t) ogg_page_serialno (og)) ;
			return codec_lookup [k].codec ;
			} ;
		} ;

	len = og->body_len < 8 ? og->body_len : 8 ;

	psf_log_printf (psf, "Ogg_stream data : '") ;
	for (k = 0 ; k < len ; k++)
		psf_log_printf (psf, "%c", isprint (og->body [k]) ? og->body [k] : '.') ;
	psf_log_printf (psf, "'     ") ;
	for (k = 0 ; k < len ; k++)
		psf_log_printf (psf, " %02x", og->body [k] & 0xff) ;
	psf_log_printf (psf, "\n") ;

	return 0 ;
} /* ogg_page_classify */

/*
** Scale x from the range [0, from] to the range [0, to]
*/
static uint64_t
ogg_page_search_do_rescale (uint64_t x, uint64_t from, uint64_t to)
{	uint64_t frac ;
	uint64_t ret ;
	int i ;

	/* I should have paid more attention in CSc 349A: Numerical Analysis */
	if (x >= from)
		return to ;
	if (x == 0)
		return 0 ;
	frac = 0 ;
	for (i = 0 ; i < 63 ; i++)
	{	frac <<= 1 ;
		if (x >= from >> 1)
		{	x -= from - x ;
			frac |= 1 ;
			}
		else
			x <<= 1 ;
		}
	ret = 0 ;
	for (i = 0 ; i < 63 ; i++)
	{	if (frac & 1)
			ret = (ret & to & 1) + (ret >> 1) + (to >> 1) ;
		else
			ret >>= 1 ;
		frac >>= 1 ;
		}
	return ret ;
} /* ogg_page_search_do_rescale */

static void
ogg_page_search_continued_data (OGG_PRIVATE *odata, ogg_page *page)
{	ogg_stream_pagein (&odata->ostream, page) ;
	while (ogg_stream_packetout (&odata->ostream, &odata->opacket)) ;
} /* ogg_page_search_continued_data */

#else /* HAVE_EXTERNAL_XIPH_LIBS */

int
ogg_open	(SF_PRIVATE *psf)
{
	psf_log_printf (psf, "This version of libsndfile was compiled without Ogg/Vorbis support.\n") ;
	return SFE_UNIMPLEMENTED ;
} /* ogg_open */

#endif
