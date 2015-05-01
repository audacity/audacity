/* flac - Command-line FLAC encoder/decoder
 * Copyright (C) 2000-2009  Josh Coalson
 * Copyright (C) 2011-2014  Xiph.Org Foundation
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifndef flac__foreign_metadata_h
#define flac__foreign_metadata_h

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include "FLAC/metadata.h"
#include "utils.h"
#include "share/compat.h"

/* WATCHOUT: these enums are used to index internal arrays */
typedef enum {
	FOREIGN_BLOCK_TYPE__AIFF = 0, /* for AIFF and AIFF-C */
	FOREIGN_BLOCK_TYPE__RIFF = 1, /* for WAVE and RF64 */
	FOREIGN_BLOCK_TYPE__WAVE64 = 2  /* only for Sony's flavor */
} foreign_block_type_t;

typedef struct {
	/* for encoding, this will be the offset in the WAVE/AIFF file of the chunk */
	/* for decoding, this will be the offset in the FLAC file of the chunk data inside the APPLICATION block */
	FLAC__off_t offset;
	/* size is the actual size in bytes of the chunk to be stored/recreated. */
	/* It includes the 8 bytes of chunk type and size, and any padding byte for alignment. */
	/* For 'data'/'SSND' chunks, the size does not include the actual sound or padding bytes */
	/* because these are not stored, they are recreated from the compressed FLAC stream. */
	/* So for RIFF 'data', size is 8, and for AIFF 'SSND', size is 8 + 8 + ssnd_offset_size */
	/* 32 bit size is OK because we only care about the non-sound data and FLAC metadata */
	/* only supports a few megs anyway. */
	FLAC__uint32 size;
} foreign_block_t;

typedef struct {
	foreign_block_type_t type; /* currently we don't support multiple foreign types in a stream (and maybe never will) */
	foreign_block_t *blocks;
	size_t num_blocks;
	size_t format_block; /* block number of 'fmt ' or 'COMM' chunk */
	size_t audio_block; /* block number of 'data' or 'SSND' chunk */
	FLAC__bool is_rf64; /* always false if type!=RIFF */
	FLAC__uint32 ssnd_offset_size; /* 0 if type!=AIFF */
} foreign_metadata_t;

foreign_metadata_t *flac__foreign_metadata_new(foreign_block_type_t type);

void flac__foreign_metadata_delete(foreign_metadata_t *fm);

FLAC__bool flac__foreign_metadata_read_from_aiff(foreign_metadata_t *fm, const char *filename, const char **error);
FLAC__bool flac__foreign_metadata_read_from_wave(foreign_metadata_t *fm, const char *filename, const char **error);
FLAC__bool flac__foreign_metadata_read_from_wave64(foreign_metadata_t *fm, const char *filename, const char **error);
FLAC__bool flac__foreign_metadata_write_to_flac(foreign_metadata_t *fm, const char *infilename, const char *outfilename, const char **error);

FLAC__bool flac__foreign_metadata_read_from_flac(foreign_metadata_t *fm, const char *filename, const char **error);
FLAC__bool flac__foreign_metadata_write_to_iff(foreign_metadata_t *fm, const char *infilename, const char *outfilename, FLAC__off_t offset1, FLAC__off_t offset2, FLAC__off_t offset3, const char **error);

#endif
