/* metaflac - Command-line FLAC metadata editor
 * Copyright (C) 2001-2009  Josh Coalson
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

#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "utils.h"
#include "FLAC/assert.h"
#include "share/alloc.h"
#include "share/safe_str.h"
#include "share/utf8.h"
#include "share/compat.h"

void die(const char *message)
{
	FLAC__ASSERT(0 != message);
	flac_fprintf(stderr, "ERROR: %s\n", message);
	exit(1);
}

#ifdef FLAC__VALGRIND_TESTING
size_t local_fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream)
{
	size_t ret = fwrite(ptr, size, nmemb, stream);
	if(!ferror(stream))
		fflush(stream);
	return ret;
}
#endif

char *local_strdup(const char *source)
{
	char *ret;
	FLAC__ASSERT(0 != source);
	if(0 == (ret = strdup(source)))
		die("out of memory during strdup()");
	return ret;
}

void local_strcat(char **dest, const char *source)
{
	size_t ndest, nsource;

	FLAC__ASSERT(0 != dest);
	FLAC__ASSERT(0 != source);

	ndest = *dest? strlen(*dest) : 0;
	nsource = strlen(source);

	if(nsource == 0)
		return;

	*dest = safe_realloc_add_3op_(*dest, ndest, /*+*/nsource, /*+*/1);
	if(0 == *dest)
		die("out of memory growing string");
	safe_strncpy((*dest)+ndest, source, nsource + 1);
}

static inline int local_isprint(int c)
{
	if (c < 32) return 0;
	return isprint(c);
}

void hexdump(const char *filename, const FLAC__byte *buf, unsigned bytes, const char *indent)
{
	unsigned i, left = bytes;
	const FLAC__byte *b = buf;

	for(i = 0; i < bytes; i += 16) {
		flac_printf("%s%s", filename? filename:"", filename? ":":"");
		printf("%s%08X: "
			"%02X %02X %02X %02X %02X %02X %02X %02X "
			"%02X %02X %02X %02X %02X %02X %02X %02X "
			"%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c\n",
			indent, i,
			left >  0? (unsigned char)b[ 0] : 0,
			left >  1? (unsigned char)b[ 1] : 0,
			left >  2? (unsigned char)b[ 2] : 0,
			left >  3? (unsigned char)b[ 3] : 0,
			left >  4? (unsigned char)b[ 4] : 0,
			left >  5? (unsigned char)b[ 5] : 0,
			left >  6? (unsigned char)b[ 6] : 0,
			left >  7? (unsigned char)b[ 7] : 0,
			left >  8? (unsigned char)b[ 8] : 0,
			left >  9? (unsigned char)b[ 9] : 0,
			left > 10? (unsigned char)b[10] : 0,
			left > 11? (unsigned char)b[11] : 0,
			left > 12? (unsigned char)b[12] : 0,
			left > 13? (unsigned char)b[13] : 0,
			left > 14? (unsigned char)b[14] : 0,
			left > 15? (unsigned char)b[15] : 0,
			(left >  0) ? (local_isprint(b[ 0]) ? b[ 0] : '.') : ' ',
			(left >  1) ? (local_isprint(b[ 1]) ? b[ 1] : '.') : ' ',
			(left >  2) ? (local_isprint(b[ 2]) ? b[ 2] : '.') : ' ',
			(left >  3) ? (local_isprint(b[ 3]) ? b[ 3] : '.') : ' ',
			(left >  4) ? (local_isprint(b[ 4]) ? b[ 4] : '.') : ' ',
			(left >  5) ? (local_isprint(b[ 5]) ? b[ 5] : '.') : ' ',
			(left >  6) ? (local_isprint(b[ 6]) ? b[ 6] : '.') : ' ',
			(left >  7) ? (local_isprint(b[ 7]) ? b[ 7] : '.') : ' ',
			(left >  8) ? (local_isprint(b[ 8]) ? b[ 8] : '.') : ' ',
			(left >  9) ? (local_isprint(b[ 9]) ? b[ 9] : '.') : ' ',
			(left > 10) ? (local_isprint(b[10]) ? b[10] : '.') : ' ',
			(left > 11) ? (local_isprint(b[11]) ? b[11] : '.') : ' ',
			(left > 12) ? (local_isprint(b[12]) ? b[12] : '.') : ' ',
			(left > 13) ? (local_isprint(b[13]) ? b[13] : '.') : ' ',
			(left > 14) ? (local_isprint(b[14]) ? b[14] : '.') : ' ',
			(left > 15) ? (local_isprint(b[15]) ? b[15] : '.') : ' '
		);
		left -= 16;
		b += 16;
   }
}

void print_error_with_chain_status(FLAC__Metadata_Chain *chain, const char *format, ...)
{
	const FLAC__Metadata_ChainStatus status = FLAC__metadata_chain_status(chain);
	va_list args;

	FLAC__ASSERT(0 != format);

	va_start(args, format);

	(void) flac_vfprintf(stderr, format, args);

	va_end(args);

	flac_fprintf(stderr, ", status = \"%s\"\n", FLAC__Metadata_ChainStatusString[status]);

	if(status == FLAC__METADATA_CHAIN_STATUS_ERROR_OPENING_FILE) {
		flac_fprintf(stderr, "\n"
			"The FLAC file could not be opened.  Most likely the file does not exist\n"
			"or is not readable.\n"
		);
	}
	else if(status == FLAC__METADATA_CHAIN_STATUS_NOT_A_FLAC_FILE) {
		flac_fprintf(stderr, "\n"
			"The file does not appear to be a FLAC file.\n"
		);
	}
	else if(status == FLAC__METADATA_CHAIN_STATUS_NOT_WRITABLE) {
		flac_fprintf(stderr, "\n"
			"The FLAC file does not have write permissions.\n"
		);
	}
	else if(status == FLAC__METADATA_CHAIN_STATUS_BAD_METADATA) {
		flac_fprintf(stderr, "\n"
			"The metadata to be written does not conform to the FLAC metadata\n"
			"specifications.\n"
		);
	}
	else if(status == FLAC__METADATA_CHAIN_STATUS_READ_ERROR) {
		flac_fprintf(stderr, "\n"
			"There was an error while reading the FLAC file.\n"
		);
	}
	else if(status == FLAC__METADATA_CHAIN_STATUS_WRITE_ERROR) {
		flac_fprintf(stderr, "\n"
			"There was an error while writing FLAC file; most probably the disk is\n"
			"full.\n"
		);
	}
	else if(status == FLAC__METADATA_CHAIN_STATUS_UNLINK_ERROR) {
		flac_fprintf(stderr, "\n"
			"There was an error removing the temporary FLAC file.\n"
		);
	}
}

FLAC__bool parse_vorbis_comment_field(const char *field_ref, char **field, char **name, char **value, unsigned *length, const char **violation)
{
	static const char * const violations[] = {
		"field name contains invalid character",
		"field contains no '=' character"
	};

	char *p, *q, *s;

	if(0 != field)
		*field = local_strdup(field_ref);

	s = local_strdup(field_ref);

	if(0 == (p = strchr(s, '='))) {
		free(s);
		*violation = violations[1];
		return false;
	}
	*p++ = '\0';

	for(q = s; *q; q++) {
		if(*q < 0x20 || *q > 0x7d || *q == 0x3d) {
			free(s);
			*violation = violations[0];
			return false;
		}
	}

	*name = local_strdup(s);
	*value = local_strdup(p);
	*length = strlen(p);

	free(s);
	return true;
}

void write_vc_field(const char *filename, const FLAC__StreamMetadata_VorbisComment_Entry *entry, FLAC__bool raw, FILE *f)
{
	if(0 != entry->entry) {
		if(filename)
			flac_fprintf(f, "%s:", filename);

		if(!raw) {
			/*
			 * WATCHOUT: comments that contain an embedded null will
			 * be truncated by utf_decode().
			 */
#ifdef _WIN32 /* if we are outputting to console, we need to use proper print functions to show unicode characters */
			if (f == stdout || f == stderr) {
				flac_fprintf(f, "%s", entry->entry);
			} else {
#endif
			char *converted;

			if(utf8_decode((const char *)entry->entry, &converted) >= 0) {
				(void) local_fwrite(converted, 1, strlen(converted), f);
				free(converted);
			}
			else {
				(void) local_fwrite(entry->entry, 1, entry->length, f);
			}
#ifdef _WIN32
			}
#endif
		}
		else {
			(void) local_fwrite(entry->entry, 1, entry->length, f);
		}
	}

	putc('\n', f);
}

void write_vc_fields(const char *filename, const char *field_name, const FLAC__StreamMetadata_VorbisComment_Entry entry[], unsigned num_entries, FLAC__bool raw, FILE *f)
{
	unsigned i;
	const unsigned field_name_length = (0 != field_name)? strlen(field_name) : 0;

	for(i = 0; i < num_entries; i++) {
		if(0 == field_name || FLAC__metadata_object_vorbiscomment_entry_matches(entry[i], field_name, field_name_length))
			write_vc_field(filename, entry + i, raw, f);
	}
}
