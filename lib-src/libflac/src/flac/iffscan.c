/* iffscan - Simple AIFF/RIFF chunk scanner
 * Copyright (C) 2007-2009  Josh Coalson
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

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "share/compat.h"
#include "foreign_metadata.h"

static FLAC__uint32 unpack32be_(const FLAC__byte *b)
{
	return ((FLAC__uint32)b[0]<<24) + ((FLAC__uint32)b[1]<<16) + ((FLAC__uint32)b[2]<<8) + (FLAC__uint32)b[3];
}

static FLAC__uint32 unpack32le_(const FLAC__byte *b)
{
	return (FLAC__uint32)b[0] + ((FLAC__uint32)b[1]<<8) + ((FLAC__uint32)b[2]<<16) + ((FLAC__uint32)b[3]<<24);
}

static FLAC__uint64 unpack64le_(const FLAC__byte *b)
{
	return (FLAC__uint64)b[0] + ((FLAC__uint64)b[1]<<8) + ((FLAC__uint64)b[2]<<16) + ((FLAC__uint64)b[3]<<24) + ((FLAC__uint64)b[4]<<32) + ((FLAC__uint64)b[5]<<40) + ((FLAC__uint64)b[6]<<48) + ((FLAC__uint64)b[7]<<56);
}

static FLAC__uint32 unpack32_(const FLAC__byte *b, foreign_block_type_t type)
{
	if(type == FOREIGN_BLOCK_TYPE__AIFF)
		return unpack32be_(b);
	else
		return unpack32le_(b);
}

int main(int argc, char *argv[])
{
	FILE *f;
	char buf[36];
	foreign_metadata_t *fm;
	const char *fn, *error;
	size_t i;
	FLAC__uint32 size;

#ifdef _WIN32
	if (get_utf8_argv(&argc, &argv) != 0) {
		fprintf(stderr, "ERROR: failed to convert command line parameters to UTF-8\n");
		return 1;
	}
#endif

	if(argc != 2) {
		flac_fprintf(stderr, "usage: %s { file.wav | file.aif }\n", argv[0]);
		return 1;
	}
	fn = argv[1];
	if(0 == (f = flac_fopen(fn, "rb")) || fread(buf, 1, 4, f) != 4) {
		flac_fprintf(stderr, "ERROR opening %s for reading\n", fn);
		return 1;
	}
	fclose(f);
	if(0 == (fm = flac__foreign_metadata_new(memcmp(buf, "RIFF", 4) && memcmp(buf, "RF64", 4)? FOREIGN_BLOCK_TYPE__AIFF : FOREIGN_BLOCK_TYPE__RIFF))) {
		flac_fprintf(stderr, "ERROR: out of memory\n");
		return 1;
	}
	if(fm->type == FOREIGN_BLOCK_TYPE__AIFF) {
		if(!flac__foreign_metadata_read_from_aiff(fm, fn, &error)) {
			flac_fprintf(stderr, "ERROR reading chunks from %s: %s\n", fn, error);
			return 1;
		}
	}
	else {
		if(!flac__foreign_metadata_read_from_wave(fm, fn, &error)) {
			flac_fprintf(stderr, "ERROR reading chunks from %s: %s\n", fn, error);
			return 1;
		}
	}
	if(0 == (f = flac_fopen(fn, "rb"))) {
		flac_fprintf(stderr, "ERROR opening %s for reading\n", fn);
		return 1;
	}
	for(i = 0; i < fm->num_blocks; i++) {
		if(fseeko(f, fm->blocks[i].offset, SEEK_SET) < 0) {
			flac_fprintf(stderr, "ERROR seeking in %s\n", fn);
			return 1;
		}
		if(fread(buf, 1, i==0?12:8, f) != (i==0?12:8)) {
			flac_fprintf(stderr, "ERROR reading %s\n", fn);
			return 1;
		}
		size = unpack32_((const FLAC__byte*)buf+4, fm->type);
		printf("block:[%c%c%c%c] size=%08x=(%10u)", buf[0], buf[1], buf[2], buf[3], size, size);
		if(i == 0)
			printf(" type:[%c%c%c%c]", buf[8], buf[9], buf[10], buf[11]);
		else if(fm->type == FOREIGN_BLOCK_TYPE__AIFF && i == fm->audio_block)
			printf(" offset size=%08x=(%10u)", fm->ssnd_offset_size, fm->ssnd_offset_size);
		else if(fm->type == FOREIGN_BLOCK_TYPE__RIFF && i == 1 && !memcmp(buf, "ds64", 4)) {
			if(fread(buf+8, 1, 36-8, f) != 36-8) {
				flac_fprintf(stderr, "ERROR reading %s\n", fn);
				return 1;
			}
			printf(" RIFF size=%016" PRIx64 "=(" PRIu64 ")", unpack64le_(buf+8), unpack64le_(buf+8));
			printf(" data size=%016" PRIx64 "=(" PRIu64 ")", unpack64le_(buf+16), unpack64le_(buf+16));
			printf(" sample count=%016" PRIx64 "=(" PRIu64 ")", unpack64le_(buf+24), unpack64le_(buf+24));
			printf(" table size=%08x=(%u)", unpack32le_(buf+32), unpack32le_(buf+32));
		}
		printf("\n");
	}
	fclose(f);
	flac__foreign_metadata_delete(fm);
	return 0;
}
