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

#ifndef flac__decode_h
#define flac__decode_h

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include "analyze.h"
#include "foreign_metadata.h"
#include "utils.h"
#include "share/replaygain_synthesis.h"


typedef struct {
	FLAC__bool apply;
	FLAC__bool use_album_gain; /* false => use track gain */
	enum { RGSS_LIMIT__NONE, RGSS_LIMIT__PEAK, RGSS_LIMIT__HARD} limiter;
	NoiseShaping noise_shaping;
	double preamp;
} replaygain_synthesis_spec_t;

typedef struct {
	FLAC__bool treat_warnings_as_errors;
	FLAC__bool continue_through_decode_errors;
	replaygain_synthesis_spec_t replaygain_synthesis_spec;
#if FLAC__HAS_OGG
	FLAC__bool is_ogg;
	FLAC__bool use_first_serial_number;
	long serial_number;
#endif
	utils__SkipUntilSpecification skip_specification;
	utils__SkipUntilSpecification until_specification;
	FLAC__bool has_cue_specification;
	utils__CueSpecification cue_specification;
	FLAC__bool channel_map_none; /* --channel-map=none specified, eventually will expand to take actual channel map */

	FileFormat format;
	union {
		struct {
			FLAC__bool is_big_endian;
			FLAC__bool is_unsigned_samples;
		} raw;
		struct {
			foreign_metadata_t *foreign_metadata; /* NULL unless --keep-foreign-metadata requested */
		} iff;
	} format_options;
} decode_options_t;

/* outfile == 0 => test only */
int flac__decode_file(const char *infilename, const char *outfilename, FLAC__bool analysis_mode, analysis_options aopts, decode_options_t options);

#endif
