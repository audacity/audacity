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

#ifndef flac__analyze_h
#define flac__analyze_h

typedef struct {
	FLAC__bool do_residual_text;
	FLAC__bool do_residual_gnuplot;
} analysis_options;

void flac__analyze_init(analysis_options aopts);
void flac__analyze_frame(const FLAC__Frame *frame, unsigned frame_number, FLAC__uint64 frame_offset, unsigned frame_bytes, analysis_options aopts, FILE *fout);
void flac__analyze_finish(analysis_options aopts);

#endif
