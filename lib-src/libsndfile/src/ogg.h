/*
** Copyright (C) 2008-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#ifndef SF_SRC_OGG_H
#define SF_SRC_OGG_H

enum
{	OGG_ANNODEX = 300,
	OGG_ANXDATA,
	OGG_FLAC,
	OGG_FLAC0,
	OGG_PCM,
	OGG_SPEEX,
	OGG_VORBIS,
	OGG_OPUS,
} ;

typedef struct
{	/* Sync and verify incoming physical bitstream */
	ogg_sync_state osync ;
	/* Take physical pages, weld into a logical stream of packets */
	ogg_stream_state ostream ;
	/* One Ogg bitstream page. Codec packets are inside */
	ogg_page opage ;
	/* One raw packet of data for decode */
	ogg_packet opacket ;

	/* Unpacked packets. 255 is max there can ever be in one page. */
	ogg_packet pkt [255] ;
	/* How many packets */
	int pkt_len ;
	/* Current packet */
	int pkt_indx ;

	int eos ;
	int codec ;
} OGG_PRIVATE ;


#define readint(buf, base) (((buf [base + 3] << 24) & 0xff000000) | \
								((buf [base + 2] <<16) & 0xff0000) | \
								((buf [base + 1] << 8) & 0xff00) | \
								(buf [base] & 0xff))

int	ogg_read_first_page	(SF_PRIVATE *, OGG_PRIVATE *) ;

/*
** Write the whole Ogg page out. Convenience function as the ogg_page struct
** splits header and body data into separate buffers.
*/
int	ogg_write_page	(SF_PRIVATE *, ogg_page *) ;

/*
** Wrapper around psf_ftell() that returns the current offset in the file after
** the most recent page that has been returned by ogg_sync_pageout().
*/
sf_count_t ogg_sync_ftell (SF_PRIVATE *) ;

/*
** Wrapper around psf_fseek() that on success resets the ogg_sync_state struct
** so that it doesn't get corrupted.
*/
sf_count_t ogg_sync_fseek (SF_PRIVATE *, sf_count_t offset, int whence) ;

/*
** Get the next page from the physical bitstream, reading in data as necessary.
** Pays no attention to Ogg BOS/EOS markers or stream serial numbers.
** The page is buffered in the ogg_sync_state struct, (replacing any other
** buffered there) and also returned in *og. readmax sets a boundary for how
** many bytes more may be read from the file, use already buffered only, or
** unlimited reading in the case of a positive, zero or negative argument
** respectively. If a pointer to a sf_count_t is passed in offset, then it will
** be incremented by how many bytes were skipped to find the next page header.
** (Useful for seeking, normally zero.) Returns the page size in bytes on
** success, 0 on out-of-data (be it end of file or readmax reached) and -1 on
** error with psf->error set appropriately.
*/
int	ogg_sync_next_page (SF_PRIVATE * psf, ogg_page *og, sf_count_t readmax, sf_count_t *offset) ;

/*
** Load the last page of a stream before the provided file offset. Searches the
** physical bitstream, and selects a page of the passed serialno. The page
** found is loaded in the sync buffer and exposed in odata->opage, and not
** loaded into the ogg_stream_state. If found, the granulepos is returned in
** *gp_out. Returns the file offset *before* the last page on success, or -1 on
** error, setting psf->error as appropriate.
*/
sf_count_t ogg_sync_last_page_before (SF_PRIVATE *psf, OGG_PRIVATE *odata, uint64_t *gp_out, sf_count_t offset, int32_t serialno) ;

/*
** Load the next page from the virtual bitstream, reading data as necessary.
** Reads in pages from the physical bitstream, skipping pages until one of the
** virtual bitstream of interest is found, and then feeds it into the
** ogg_stream_state of odata->ostream, where it is buffered. Heeds EOS markers.
** Returns 1 on success, 0 on end of stream, and -1 on fatal error.
*/
int ogg_stream_next_page (SF_PRIVATE * psf, OGG_PRIVATE *odata) ;

/*
** Loads the next page using ogg_stream_next_page() and unpacks all packets
** into the array odata->pkt, updating odata->pkt_len and setting
** odata->pkt_indx to 0. Returns 1 if okay, 2 if okay but a hole was found
** in the bitstream, 0 if on end of stream, and -1 on fatal error.
*/
int ogg_stream_unpack_page (SF_PRIVATE *psf, OGG_PRIVATE *odata) ;

/*
** Seek within the Ogg virtual bitstream for a page containing target_gp.
** Preforms a bisection search. If not found exactly, the best result is
** returned in *best_gp. Found page is loaded into the virtual bitstream,
** ready for unpacking. Arguments pcm_start and pcm_end are the highest and
** lowest granule positions of the file. begin and end are the file offsets.
*/
int ogg_stream_seek_page_search (SF_PRIVATE *psf, OGG_PRIVATE *odata,
								uint64_t target_gp, uint64_t pcm_start, uint64_t pcm_end,
								uint64_t *best_gp, sf_count_t begin, sf_count_t end) ;

#endif /* SF_SRC_OGG_H */
