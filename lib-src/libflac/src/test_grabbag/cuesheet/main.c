/* test_cuesheet - Simple tester for cuesheet routines in grabbag
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

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "FLAC/assert.h"
#include "FLAC/metadata.h"
#include "share/grabbag.h"

static int do_cuesheet(const char *infilename, unsigned sample_rate, FLAC__bool is_cdda, FLAC__uint64 lead_out_offset)
{
	FILE *fin, *fout;
	const char *error_message;
	char tmpfilename[4096];
	unsigned last_line_read;
	FLAC__StreamMetadata *cuesheet;

	FLAC__ASSERT(strlen(infilename) + 2 < sizeof(tmpfilename));

	/*
	 * pass 1
	 */
	if(0 == strcmp(infilename, "-")) {
		fin = stdin;
	}
	else if(0 == (fin = flac_fopen(infilename, "r"))) {
		fprintf(stderr, "can't open file %s for reading: %s\n", infilename, strerror(errno));
		return 255;
	}
	if(0 != (cuesheet = grabbag__cuesheet_parse(fin, &error_message, &last_line_read, sample_rate, is_cdda, lead_out_offset))) {
		if(fin != stdin)
			fclose(fin);
	}
	else {
		printf("pass1: parse error, line %u: \"%s\"\n", last_line_read, error_message);
		if(fin != stdin)
			fclose(fin);
		return 1;
	}
	if(!FLAC__metadata_object_cuesheet_is_legal(cuesheet, is_cdda, &error_message)) {
		printf("pass1: illegal cuesheet: \"%s\"\n", error_message);
		FLAC__metadata_object_delete(cuesheet);
		return 1;
	}
	flac_snprintf(tmpfilename, sizeof (tmpfilename), "%s.1", infilename);
	if(0 == (fout = flac_fopen(tmpfilename, "w"))) {
		fprintf(stderr, "can't open file %s for writing: %s\n", tmpfilename, strerror(errno));
		FLAC__metadata_object_delete(cuesheet);
		return 255;
	}
	grabbag__cuesheet_emit(fout, cuesheet, "\"dummy.wav\" WAVE");
	FLAC__metadata_object_delete(cuesheet);
	fclose(fout);

	/*
	 * pass 2
	 */
	if(0 == (fin = flac_fopen(tmpfilename, "r"))) {
		fprintf(stderr, "can't open file %s for reading: %s\n", tmpfilename, strerror(errno));
		return 255;
	}
	if(0 != (cuesheet = grabbag__cuesheet_parse(fin, &error_message, &last_line_read, sample_rate, is_cdda, lead_out_offset))) {
		if(fin != stdin)
			fclose(fin);
	}
	else {
		printf("pass2: parse error, line %u: \"%s\"\n", last_line_read, error_message);
		if(fin != stdin)
			fclose(fin);
		return 1;
	}
	if(!FLAC__metadata_object_cuesheet_is_legal(cuesheet, is_cdda, &error_message)) {
		printf("pass2: illegal cuesheet: \"%s\"\n", error_message);
		FLAC__metadata_object_delete(cuesheet);
		return 1;
	}
	flac_snprintf(tmpfilename, sizeof (tmpfilename), "%s.2", infilename);
	if(0 == (fout = flac_fopen(tmpfilename, "w"))) {
		fprintf(stderr, "can't open file %s for writing: %s\n", tmpfilename, strerror(errno));
		FLAC__metadata_object_delete(cuesheet);
		return 255;
	}
	grabbag__cuesheet_emit(fout, cuesheet, "\"dummy.wav\" WAVE");
	FLAC__metadata_object_delete(cuesheet);
	fclose(fout);

	return 0;
}

int main(int argc, char *argv[])
{
	FLAC__uint64 lead_out_offset;
	unsigned sample_rate = 48000;
	FLAC__bool is_cdda = false;
	const char *usage = "usage: test_cuesheet cuesheet_file lead_out_offset [ [ sample_rate ] cdda ]\n";

	if(argc > 1 && 0 == strcmp(argv[1], "-h")) {
		puts(usage);
		return 0;
	}

	if(argc < 3 || argc > 5) {
		fputs(usage, stderr);
		return 255;
	}

	lead_out_offset = (FLAC__uint64)strtoul(argv[2], 0, 10);
	if(argc >= 4) {
		sample_rate = (unsigned)atoi(argv[3]);
		if(argc >= 5) {
			if(0 == strcmp(argv[4], "cdda"))
				is_cdda = true;
			else {
				fputs(usage, stderr);
				return 255;
			}
		}
	}

	return do_cuesheet(argv[1], sample_rate, is_cdda, lead_out_offset);
}
