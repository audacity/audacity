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

#include "options.h"

FLAC__bool do_shorthand_operation__picture(const char *filename, FLAC__Metadata_Chain *chain, const Operation *operation, FLAC__bool *needs_write);
FLAC__bool do_shorthand_operation__cuesheet(const char *filename, FLAC__Metadata_Chain *chain, const Operation *operation, FLAC__bool *needs_write);
FLAC__bool do_shorthand_operation__add_seekpoints(const char *filename, FLAC__Metadata_Chain *chain, const char *specification, FLAC__bool *needs_write);
FLAC__bool do_shorthand_operation__streaminfo(const char *filename, FLAC__bool prefix_with_filename, FLAC__Metadata_Chain *chain, const Operation *operation, FLAC__bool *needs_write);
FLAC__bool do_shorthand_operation__vorbis_comment(const char *filename, FLAC__bool prefix_with_filename, FLAC__Metadata_Chain *chain, const Operation *operation, FLAC__bool *needs_write, FLAC__bool raw);
