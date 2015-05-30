/* flac - Command-line FLAC encoder/decoder
 * Copyright (C) 2002-2009  Josh Coalson
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

#ifndef flac__utils_h
#define flac__utils_h

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include "FLAC/ordinals.h"
#include "FLAC/format.h" /* for FLAC__StreamMetadata_CueSheet */
#include <stdio.h> /* for FILE */

typedef enum { FORMAT_RAW, FORMAT_WAVE, FORMAT_WAVE64, FORMAT_RF64, FORMAT_AIFF, FORMAT_AIFF_C, FORMAT_FLAC, FORMAT_OGGFLAC } FileFormat;

typedef struct {
	FLAC__bool is_relative; /* i.e. specification string started with + or - */
	FLAC__bool value_is_samples;
	union {
		double seconds;
		FLAC__int64 samples;
	} value;
} utils__SkipUntilSpecification;

typedef struct {
	FLAC__bool has_start_point, has_end_point;
	unsigned start_track, start_index;
	unsigned end_track, end_index;
} utils__CueSpecification;

#ifdef FLAC__VALGRIND_TESTING
size_t flac__utils_fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream);
#else
#define flac__utils_fwrite fwrite
#endif

extern int flac__utils_verbosity_;
void flac__utils_printf(FILE *stream, int level, const char *format, ...);

int get_console_width(void);
size_t strlen_console(const char *text);
void stats_new_file(void);
void stats_clear(void);
void stats_print_name(int level, const char *name);
void stats_print_info(int level, const char *format, ...);

FLAC__bool flac__utils_parse_skip_until_specification(const char *s, utils__SkipUntilSpecification *spec);
void flac__utils_canonicalize_skip_until_specification(utils__SkipUntilSpecification *spec, unsigned sample_rate);

FLAC__bool flac__utils_parse_cue_specification(const char *s, utils__CueSpecification *spec);
void flac__utils_canonicalize_cue_specification(const utils__CueSpecification *cue_spec, const FLAC__StreamMetadata_CueSheet *cuesheet, FLAC__uint64 total_samples, utils__SkipUntilSpecification *skip_spec, utils__SkipUntilSpecification *until_spec);

FLAC__bool flac__utils_set_channel_mask_tag(FLAC__StreamMetadata *object, FLAC__uint32 channel_mask);
FLAC__bool flac__utils_get_channel_mask_tag(const FLAC__StreamMetadata *object, FLAC__uint32 *channel_mask);

#endif
