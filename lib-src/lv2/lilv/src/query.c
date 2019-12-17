/*
  Copyright 2007-2019 David Robillard <http://drobilla.net>

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

#include "lilv_internal.h"

#include "lilv/lilv.h"
#include "sord/sord.h"
#include "zix/tree.h"

#include <stdlib.h>
#include <string.h>

typedef enum {
	LILV_LANG_MATCH_NONE,     ///< Language does not match at all
	LILV_LANG_MATCH_PARTIAL,  ///< Partial (language, but not country) match
	LILV_LANG_MATCH_EXACT     ///< Exact (language and country) match
} LilvLangMatch;

static LilvLangMatch
lilv_lang_matches(const char* a, const char* b)
{
	if (!strcmp(a, b)) {
		return LILV_LANG_MATCH_EXACT;
	}

	const char*  a_dash     = strchr(a, '-');
	const size_t a_lang_len = a_dash ? (size_t)(a_dash - a) : strlen(a);
	const char*  b_dash     = strchr(b, '-');
	const size_t b_lang_len = b_dash ? (size_t)(b_dash - b) : strlen(b);

	if (a_lang_len == b_lang_len && !strncmp(a, b, a_lang_len)) {
		return LILV_LANG_MATCH_PARTIAL;
	}

	return LILV_LANG_MATCH_NONE;
}

static LilvNodes*
lilv_nodes_from_stream_objects_i18n(LilvWorld*    world,
                                    SordIter*     stream,
                                    SordQuadIndex field)
{
	LilvNodes*      values  = lilv_nodes_new();
	const SordNode* nolang  = NULL;  // Untranslated value
	const SordNode* partial = NULL;  // Partial language match
	char*           syslang = lilv_get_lang();
	FOREACH_MATCH(stream) {
		const SordNode* value = sord_iter_get_node(stream, field);
		if (sord_node_get_type(value) == SORD_LITERAL) {
			const char*   lang = sord_node_get_language(value);
			LilvLangMatch lm   = LILV_LANG_MATCH_NONE;
			if (lang) {
				lm = (syslang)
					? lilv_lang_matches(lang, syslang)
					: LILV_LANG_MATCH_PARTIAL;
			} else {
				nolang = value;
				if (!syslang) {
					lm = LILV_LANG_MATCH_EXACT;
				}
			}

			if (lm == LILV_LANG_MATCH_EXACT) {
				// Exact language match, add to results
				zix_tree_insert((ZixTree*)values,
				                lilv_node_new_from_node(world, value),
				                NULL);
			} else if (lm == LILV_LANG_MATCH_PARTIAL) {
				// Partial language match, save in case we find no exact
				partial = value;
			}
		} else {
			zix_tree_insert((ZixTree*)values,
			                lilv_node_new_from_node(world, value),
			                NULL);
		}
	}
	sord_iter_free(stream);
	free(syslang);

	if (lilv_nodes_size(values) > 0) {
		return values;
	}

	const SordNode* best = nolang;
	if (syslang && partial) {
		// Partial language match for system language
		best = partial;
	} else if (!best) {
		// No languages matches at all, and no untranslated value
		// Use any value, if possible
		best = partial;
	}

	if (best) {
		zix_tree_insert(
			(ZixTree*)values, lilv_node_new_from_node(world, best), NULL);
	} else {
		// No matches whatsoever
		lilv_nodes_free(values);
		values = NULL;
	}

	return values;
}

LilvNodes*
lilv_nodes_from_stream_objects(LilvWorld*    world,
                               SordIter*     stream,
                               SordQuadIndex field)
{
	if (sord_iter_end(stream)) {
		sord_iter_free(stream);
		return NULL;
	} else if (world->opt.filter_language) {
		return lilv_nodes_from_stream_objects_i18n(world, stream, field);
	} else {
		LilvNodes* values = lilv_nodes_new();
		FOREACH_MATCH(stream) {
			const SordNode* value = sord_iter_get_node(stream, field);
			LilvNode*       node  = lilv_node_new_from_node(world, value);
			if (node) {
				zix_tree_insert((ZixTree*)values, node, NULL);
			}
		}
		sord_iter_free(stream);
		return values;
	}
}
