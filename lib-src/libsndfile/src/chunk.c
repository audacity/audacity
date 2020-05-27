/*
** Copyright (C) 2008-2016 Erik de Castro Lopo <erikd@mega-nerd.com>
** Copyright (C) 2012 IOhannes m zmoelnig, IEM <zmoelnig@iem.at>
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

#include	<stdlib.h>
#include	<string.h>

#include	"sndfile.h"
#include	"sfendian.h"
#include	"common.h"

static int64_t
hash_of_str (const char * str)
{	int64_t marker = 0 ;
	int k ;

	for (k = 0 ; str [k] ; k++)
		marker = marker * 0x7f + ((const uint8_t *) str) [k] ;

	return marker ;
} /* hash_of_str */

SF_CHUNK_ITERATOR *
psf_get_chunk_iterator (SF_PRIVATE * psf, const char * marker_str)
{	const READ_CHUNKS * pchk = &psf->rchunks ;
	int idx ;

	if (marker_str)
		idx = psf_find_read_chunk_str (pchk, marker_str) ;
	else
		idx = pchk->used > 0 ? 0 : -1 ;

	if (idx < 0)
		return NULL ;

	if (psf->iterator == NULL)
	{	psf->iterator = calloc (1, sizeof (SF_CHUNK_ITERATOR)) ;
		if (psf->iterator == NULL)
			return NULL ;
		} ;

	psf->iterator->sndfile = (SNDFILE *) psf ;

	if (marker_str)
	{	int64_t hash ;
		size_t marker_len ;
		union
		{	uint32_t marker ;
			char str [5] ;
		} u ;

		snprintf (u.str, sizeof (u.str), "%s", marker_str) ;

		marker_len = strlen (marker_str) ;
		if (marker_len > 64)
			marker_len = 64 ;

		hash = marker_len > 4 ? hash_of_str (marker_str) : u.marker ;

		memcpy (psf->iterator->id, marker_str, marker_len) ;
		psf->iterator->id_size = marker_len ;
		psf->iterator->hash = hash ;
		}

	psf->iterator->current = idx ;

	return psf->iterator ;
} /* psf_get_chunk_iterator */

SF_CHUNK_ITERATOR *
psf_next_chunk_iterator (const READ_CHUNKS * pchk , SF_CHUNK_ITERATOR * iterator)
{	uint64_t hash = iterator->hash ;
	uint32_t k ;

	iterator->current++ ;

	if (hash)
	{	for (k = iterator->current ; k < pchk->used ; k++)
			if (pchk->chunks [k].hash == hash)
			{	iterator->current = k ;
				return iterator ;
				}
		}
	else if (iterator->current < pchk->used)
		return iterator ;

	/* No match, clear iterator and return NULL */
	memset (iterator, 0, sizeof (*iterator)) ;
	return NULL ;
} /* psf_next_chunk_iterator */

static int
psf_store_read_chunk (READ_CHUNKS * pchk, const READ_CHUNK * rchunk)
{	if (pchk->count == 0)
	{	pchk->used = 0 ;
		pchk->count = 20 ;
		pchk->chunks = calloc (pchk->count, sizeof (READ_CHUNK)) ;
		}
	else if (pchk->used > pchk->count)
		return SFE_INTERNAL ;
	else if (pchk->used == pchk->count)
	{	READ_CHUNK * old_ptr = pchk->chunks ;
		int new_count = 3 * (pchk->count + 1) / 2 ;

		pchk->chunks = realloc (old_ptr, new_count * sizeof (READ_CHUNK)) ;
		if (pchk->chunks == NULL)
		{	pchk->chunks = old_ptr ;
			return SFE_MALLOC_FAILED ;
			} ;
		pchk->count = new_count ;
		} ;

	pchk->chunks [pchk->used] = *rchunk ;

	pchk->used ++ ;

	return SFE_NO_ERROR ;
} /* psf_store_read_chunk */

int
psf_store_read_chunk_u32 (READ_CHUNKS * pchk, uint32_t marker, sf_count_t offset, uint32_t len)
{	READ_CHUNK rchunk ;

	memset (&rchunk, 0, sizeof (rchunk)) ;

	rchunk.hash = marker ;
	rchunk.mark32 = marker ;
	rchunk.offset = offset ;
	rchunk.len = len ;

	rchunk.id_size = 4 ;
	memcpy (rchunk.id, &marker, rchunk.id_size) ;

	return psf_store_read_chunk (pchk, &rchunk) ;
} /* psf_store_read_chunk_u32 */

int
psf_find_read_chunk_str (const READ_CHUNKS * pchk, const char * marker_str)
{	uint64_t hash ;
	uint32_t k ;
	union
	{	uint32_t marker ;
		char str [5] ;
	} u ;

	snprintf (u.str, sizeof (u.str), "%s", marker_str) ;

	hash = strlen (marker_str) > 4 ? hash_of_str (marker_str) : u.marker ;

	for (k = 0 ; k < pchk->used ; k++)
		if (pchk->chunks [k].hash == hash)
			return k ;

	return -1 ;
} /* psf_find_read_chunk_str */

int
psf_find_read_chunk_m32 (const READ_CHUNKS * pchk, uint32_t marker)
{	uint32_t k ;

	for (k = 0 ; k < pchk->used ; k++)
		if (pchk->chunks [k].mark32 == marker)
			return k ;

	return -1 ;
} /* psf_find_read_chunk_m32 */
int
psf_find_read_chunk_iterator (const READ_CHUNKS * pchk, const SF_CHUNK_ITERATOR * marker)
{	if (marker->current < pchk->used)
		return marker->current ;

	return -1 ;
} /* psf_find_read_chunk_iterator */

int
psf_store_read_chunk_str (READ_CHUNKS * pchk, const char * marker_str, sf_count_t offset, uint32_t len)
{	READ_CHUNK rchunk ;
	union
	{	uint32_t marker ;
		char str [5] ;
	} u ;
	size_t marker_len ;

	memset (&rchunk, 0, sizeof (rchunk)) ;
	snprintf (u.str, sizeof (u.str), "%s", marker_str) ;

	marker_len = strlen (marker_str) ;

	rchunk.hash = marker_len > 4 ? hash_of_str (marker_str) : u.marker ;
	rchunk.mark32 = u.marker ;
	rchunk.offset = offset ;
	rchunk.len = len ;

	rchunk.id_size = marker_len > 64 ? 64 : marker_len ;
	memcpy (rchunk.id, marker_str, rchunk.id_size) ;

	return psf_store_read_chunk (pchk, &rchunk) ;
} /* psf_store_read_chunk_str */

int
psf_save_write_chunk (WRITE_CHUNKS * pchk, const SF_CHUNK_INFO * chunk_info)
{	union
	{	uint32_t marker ;
		char str [5] ;
		/* Update snprintf() format string below when changing this */
	} u ;
	uint32_t len ;

	if (pchk->count == 0)
	{	pchk->used = 0 ;
		pchk->count = 20 ;
		pchk->chunks = calloc (pchk->count, sizeof (WRITE_CHUNK)) ;
		}
	else if (pchk->used >= pchk->count)
	{	WRITE_CHUNK * old_ptr = pchk->chunks ;
		int new_count = 3 * (pchk->count + 1) / 2 ;

		pchk->chunks = realloc (old_ptr, new_count * sizeof (WRITE_CHUNK)) ;
		if (pchk->chunks == NULL)
		{	pchk->chunks = old_ptr ;
			return SFE_MALLOC_FAILED ;
			} ;
		} ;

	len = chunk_info->datalen ;
	while (len & 3) len ++ ;

	snprintf (u.str, sizeof (u.str), "%.4s", chunk_info->id) ;

	pchk->chunks [pchk->used].hash = strlen (chunk_info->id) > 4 ? hash_of_str (chunk_info->id) : u.marker ;
	pchk->chunks [pchk->used].mark32 = u.marker ;
	pchk->chunks [pchk->used].len = len ;
	pchk->chunks [pchk->used].data = psf_memdup (chunk_info->data, chunk_info->datalen) ;

	pchk->used ++ ;

	return SFE_NO_ERROR ;
} /* psf_save_write_chunk */

