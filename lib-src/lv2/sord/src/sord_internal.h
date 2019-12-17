/*
  Copyright 2011-2015 David Robillard <http://drobilla.net>

  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*/

#ifndef SORD_SORD_INTERNAL_H
#define SORD_SORD_INTERNAL_H

#include <stddef.h>
#include <stdint.h>

#include "sord/sord.h"

#if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 5)
#    define SORD_UNREACHABLE() __builtin_unreachable()
#else
#    define SORD_UNREACHABLE() assert(false)
#endif

/** Resource node metadata */
typedef struct {
	size_t refs_as_obj;  ///< References as a quad object
} SordResourceMetadata;

/** Literal node metadata */
typedef struct {
	SordNode* datatype;  ///< Optional literal data type URI
	char      lang[16];  ///< Optional language tag
} SordLiteralMetadata;

/** Node */
struct SordNodeImpl {
	SerdNode node;  ///< Serd node
	size_t   refs;  ///< Reference count (# of containing quads)
	union {
		SordResourceMetadata res;
		SordLiteralMetadata  lit;
	} meta;
};

#endif /* SORD_SORD_INTERNAL_H */
