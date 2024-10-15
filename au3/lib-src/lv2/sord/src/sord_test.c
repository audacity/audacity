/*
  Copyright 2011-2016 David Robillard <http://drobilla.net>

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

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sord/sord.h"

static const int      DIGITS        = 3;
static const unsigned n_objects_per = 2;

static int n_expected_errors = 0;

typedef struct {
	SordQuad query;
	int      expected_num_results;
} QueryTest;

#define USTR(s) ((const uint8_t*)(s))

static SordNode*
uri(SordWorld* world, int num)
{
	if (num == 0) {
		return 0;
	}

	char  str[]   = "eg:000";
	char* uri_num = str + 3;  // First `0'
	snprintf(uri_num, DIGITS + 1, "%0*d", DIGITS, num);
	return sord_new_uri(world, (const uint8_t*)str);
}

static int
test_fail(const char* fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	fprintf(stderr, "error: ");
	vfprintf(stderr, fmt, args);
	va_end(args);
	return 1;
}

static int
generate(SordWorld* world,
         SordModel* sord,
         size_t     n_quads,
         SordNode*  graph)
{
	fprintf(stderr, "Generating %zu (S P *) quads with %u objects each\n",
	        n_quads, n_objects_per);

	for (size_t i = 0; i < n_quads; ++i) {
		int num = (i * n_objects_per) + 1;

		SordNode* ids[2 + n_objects_per];
		for (unsigned j = 0; j < 2 + n_objects_per; ++j) {
			ids[j] = uri(world, num++);
		}

		for (unsigned j = 0; j < n_objects_per; ++j) {
			SordQuad tup = { ids[0], ids[1], ids[2 + j], graph };
			if (!sord_add(sord, tup)) {
				return test_fail("Fail: Failed to add quad\n");
			}
		}

		for (unsigned j = 0; j < 2 + n_objects_per; ++j) {
			sord_node_free(world, ids[j]);
		}
	}

	// Add some literals

	// (98 4 "hello") and (98 4 "hello"^^<5>)
	SordQuad tup = { 0, 0, 0, 0 };
	tup[0] = uri(world, 98);
	tup[1] = uri(world, 4);
	tup[2] = sord_new_literal(world, 0, USTR("hello"), NULL);
	tup[3] = graph;
	sord_add(sord, tup);
	sord_node_free(world, (SordNode*)tup[2]);
	tup[2] = sord_new_literal(world, uri(world, 5), USTR("hello"), NULL);
	if (!sord_add(sord, tup)) {
		return test_fail("Failed to add typed literal\n");
	}

	// (96 4 "hello"^^<4>) and (96 4 "hello"^^<5>)
	tup[0] = uri(world, 96);
	tup[1] = uri(world, 4);
	tup[2] = sord_new_literal(world, uri(world, 4), USTR("hello"), NULL);
	tup[3] = graph;
	sord_add(sord, tup);
	sord_node_free(world, (SordNode*)tup[2]);
	tup[2] = sord_new_literal(world, uri(world, 5), USTR("hello"), NULL);
	if (!sord_add(sord, tup)) {
		return test_fail("Failed to add typed literal\n");
	}

	// (94 5 "hello") and (94 5 "hello"@en-gb)
	tup[0] = uri(world, 94);
	tup[1] = uri(world, 5);
	tup[2] = sord_new_literal(world, 0, USTR("hello"), NULL);
	tup[3] = graph;
	sord_add(sord, tup);
	sord_node_free(world, (SordNode*)tup[2]);
	tup[2] = sord_new_literal(world, NULL, USTR("hello"), "en-gb");
	if (!sord_add(sord, tup)) {
		return test_fail("Failed to add literal with language\n");
	}

	// (92 6 "hello"@en-us) and (92 5 "hello"@en-gb)
	tup[0] = uri(world, 92);
	tup[1] = uri(world, 6);
	tup[2] = sord_new_literal(world, 0, USTR("hello"), "en-us");
	tup[3] = graph;
	sord_add(sord, tup);
	sord_node_free(world, (SordNode*)tup[2]);
	tup[2] = sord_new_literal(world, NULL, USTR("hello"), "en-gb");
	if (!sord_add(sord, tup)) {
		return test_fail("Failed to add literal with language\n");
	}

	sord_node_free(world, (SordNode*)tup[0]);
	sord_node_free(world, (SordNode*)tup[2]);
	tup[0] = uri(world, 14);
	tup[2] = sord_new_literal(world, 0, USTR("bonjour"), "fr");
	sord_add(sord, tup);
	sord_node_free(world, (SordNode*)tup[2]);
	tup[2] = sord_new_literal(world, 0, USTR("salut"), "fr");
	sord_add(sord, tup);

	// Attempt to add some duplicates
	if (sord_add(sord, tup)) {
		return test_fail("Fail: Successfully added duplicate quad\n");
	}
	if (sord_add(sord, tup)) {
		return test_fail("Fail: Successfully added duplicate quad\n");
	}

	// Add a blank node subject
	sord_node_free(world, (SordNode*)tup[0]);
	tup[0] = sord_new_blank(world, USTR("ablank"));
	sord_add(sord, tup);

	sord_node_free(world, (SordNode*)tup[1]);
	sord_node_free(world, (SordNode*)tup[2]);
	tup[1] = uri(world, 6);
	tup[2] = uri(world, 7);
	sord_add(sord, tup);
	sord_node_free(world, (SordNode*)tup[0]);
	sord_node_free(world, (SordNode*)tup[1]);
	sord_node_free(world, (SordNode*)tup[2]);

	return EXIT_SUCCESS;
}

#define TUP_FMT "(%6s %6s %6s)"
#define TUP_FMT_ARGS(t) \
	((t)[0] ? sord_node_get_string((t)[0]) : USTR("*")), \
		((t)[1] ? sord_node_get_string((t)[1]) : USTR("*")), \
		((t)[2] ? sord_node_get_string((t)[2]) : USTR("*"))

static int
test_read(SordWorld* world, SordModel* sord, SordNode* g,
          const size_t n_quads)
{
	int ret = EXIT_SUCCESS;

	SordQuad id;

	SordIter* iter = sord_begin(sord);
	if (sord_iter_get_model(iter) != sord) {
		return test_fail("Fail: Iterator has incorrect sord pointer\n");
	}

	for (; !sord_iter_end(iter); sord_iter_next(iter)) {
		sord_iter_get(iter, id);
	}

	// Attempt to increment past end
	if (!sord_iter_next(iter)) {
		return test_fail("Fail: Successfully incremented past end\n");
	}

	sord_iter_free(iter);

	const uint8_t* s = USTR("hello");
	SordNode* plain_hello = sord_new_literal(world, 0, s, NULL);
	SordNode* type4_hello = sord_new_literal(world, uri(world, 4), s, NULL);
	SordNode* type5_hello = sord_new_literal(world, uri(world, 5), s, NULL);
	SordNode* gb_hello    = sord_new_literal(world, NULL, s, "en-gb");
	SordNode* us_hello    = sord_new_literal(world, NULL, s, "en-us");

#define NUM_PATTERNS 18

	QueryTest patterns[NUM_PATTERNS] = {
		{ { 0, 0, 0 }, (int)(n_quads * n_objects_per) + 12 },
		{ { uri(world, 1), 0, 0 }, 2 },
		{ { uri(world, 9), uri(world, 9), uri(world, 9) }, 0 },
		{ { uri(world, 1), uri(world, 2), uri(world, 4) }, 1 },
		{ { uri(world, 3), uri(world, 4), uri(world, 0) }, 2 },
		{ { uri(world, 0), uri(world, 2), uri(world, 4) }, 1 },
		{ { uri(world, 0), uri(world, 0), uri(world, 4) }, 1 },
		{ { uri(world, 1), uri(world, 0), uri(world, 0) }, 2 },
		{ { uri(world, 1), uri(world, 0), uri(world, 4) }, 1 },
		{ { uri(world, 0), uri(world, 2), uri(world, 0) }, 2 },
		{ { uri(world, 98), uri(world, 4), plain_hello  }, 1 },
		{ { uri(world, 98), uri(world, 4), type5_hello  }, 1 },
		{ { uri(world, 96), uri(world, 4), type4_hello  }, 1 },
		{ { uri(world, 96), uri(world, 4), type5_hello  }, 1 },
		{ { uri(world, 94), uri(world, 5), plain_hello  }, 1 },
		{ { uri(world, 94), uri(world, 5), gb_hello     }, 1 },
		{ { uri(world, 92), uri(world, 6), gb_hello     }, 1 },
		{ { uri(world, 92), uri(world, 6), us_hello     }, 1 } };

	SordQuad match = { uri(world, 1), uri(world, 2), uri(world, 4), g };
	if (!sord_contains(sord, match)) {
		return test_fail("Fail: No match for " TUP_FMT "\n",
		                 TUP_FMT_ARGS(match));
	}

	SordQuad nomatch = { uri(world, 1), uri(world, 2), uri(world, 9), g };
	if (sord_contains(sord, nomatch)) {
		return test_fail("Fail: False match for " TUP_FMT "\n",
		                 TUP_FMT_ARGS(nomatch));
	}

	if (sord_get(sord, NULL, NULL, uri(world, 3), g)) {
		return test_fail("Fail: Get *,*,3 succeeded\n");
	} else if (!sord_node_equals(
		           sord_get(sord, uri(world, 1), uri(world, 2), NULL, g),
		           uri(world, 3))) {
		return test_fail("Fail: Get 1,2,* != 3\n");
	} else if (!sord_node_equals(
		           sord_get(sord, uri(world, 1), NULL, uri(world, 3), g),
		           uri(world, 2))) {
		return test_fail("Fail: Get 1,*,3 != 2\n");
	} else if (!sord_node_equals(
		           sord_get(sord, NULL, uri(world, 2), uri(world, 3), g),
		           uri(world, 1))) {
		return test_fail("Fail: Get *,2,3 != 1\n");
	}

	for (unsigned i = 0; i < NUM_PATTERNS; ++i) {
		QueryTest test = patterns[i];
		SordQuad  pat = { test.query[0], test.query[1], test.query[2], g };
		fprintf(stderr, "Query " TUP_FMT "... ", TUP_FMT_ARGS(pat));

		iter = sord_find(sord, pat);
		int num_results = 0;
		for (; !sord_iter_end(iter); sord_iter_next(iter)) {
			sord_iter_get(iter, id);
			++num_results;
			if (!sord_quad_match(pat, id)) {
				sord_iter_free(iter);
				return test_fail(
					"Fail: Query result " TUP_FMT " does not match pattern\n",
					TUP_FMT_ARGS(id));
			}
		}
		sord_iter_free(iter);
		if (num_results != test.expected_num_results) {
			return test_fail("Fail: Expected %d results, got %d\n",
			                 test.expected_num_results, num_results);
		}
		fprintf(stderr, "OK (%u matches)\n", test.expected_num_results);
	}

	// Query blank node subject
	SordQuad pat = { sord_new_blank(world, USTR("ablank")), 0, 0 };
	if (!pat[0]) {
		return test_fail("Blank node subject lost\n");
	}
	fprintf(stderr, "Query " TUP_FMT "... ", TUP_FMT_ARGS(pat));
	iter = sord_find(sord, pat);
	int num_results = 0;
	for (; !sord_iter_end(iter); sord_iter_next(iter)) {
		sord_iter_get(iter, id);
		++num_results;
		if (!sord_quad_match(pat, id)) {
			sord_iter_free(iter);
			return test_fail(
				"Fail: Query result " TUP_FMT " does not match pattern\n",
				TUP_FMT_ARGS(id));
		}
	}
	fprintf(stderr, "OK\n");
	sord_node_free(world, (SordNode*)pat[0]);
	sord_iter_free(iter);
	if (num_results != 2) {
		return test_fail("Blank node subject query failed\n");
	}

	// Test nested queries
	fprintf(stderr, "Nested Queries... ");
	const SordNode* last_subject = 0;
	iter = sord_search(sord, NULL, NULL, NULL, NULL);
	for (; !sord_iter_end(iter); sord_iter_next(iter)) {
		sord_iter_get(iter, id);
		if (id[0] == last_subject) {
			continue;
		}

		SordQuad  subpat          = { id[0], 0, 0 };
		SordIter* subiter         = sord_find(sord, subpat);
		uint64_t  num_sub_results = 0;
		if (sord_iter_get_node(subiter, SORD_SUBJECT) != id[0]) {
			return test_fail("Fail: Incorrect initial submatch\n");
		}
		for (; !sord_iter_end(subiter); sord_iter_next(subiter)) {
			SordQuad subid;
			sord_iter_get(subiter, subid);
			if (!sord_quad_match(subpat, subid)) {
				sord_iter_free(iter);
				sord_iter_free(subiter);
				return test_fail(
					"Fail: Nested query result does not match pattern\n");
			}
			++num_sub_results;
		}
		sord_iter_free(subiter);
		if (num_sub_results != n_objects_per) {
			return test_fail(
				"Fail: Nested query " TUP_FMT " failed"
				" (%d results, expected %d)\n",
				TUP_FMT_ARGS(subpat), num_sub_results, n_objects_per);
		}

		uint64_t count = sord_count(sord, id[0], 0, 0, 0);
		if (count != num_sub_results) {
			return test_fail("Fail: Query " TUP_FMT " sord_count() %d"
			                 "does not match result count %d\n",
			                 TUP_FMT_ARGS(subpat), count, num_sub_results);
		}

		last_subject = id[0];
	}
	fprintf(stderr, "OK\n\n");
	sord_iter_free(iter);

	return ret;
}

static SerdStatus
unexpected_error(void* handle, const SerdError* error)
{
	fprintf(stderr, "unexpected error: ");
	vfprintf(stderr, error->fmt, *error->args);
	return SERD_SUCCESS;
}

static SerdStatus
expected_error(void* handle, const SerdError* error)
{
	fprintf(stderr, "expected error: ");
	vfprintf(stderr, error->fmt, *error->args);
	++n_expected_errors;
	return SERD_SUCCESS;
}

static int
finished(SordWorld* world, SordModel* sord, int status)
{
	sord_free(sord);
	sord_world_free(world);
	return status;
}

int
main(int argc, char** argv)
{
	static const size_t n_quads = 300;

	sord_free(NULL);  // Shouldn't crash

	SordWorld* world = sord_world_new();


	// Attempt to create invalid URI
	fprintf(stderr, "expected ");
	SordNode* bad_uri = sord_new_uri(world, USTR("noscheme"));
	if (bad_uri) {
		return test_fail("Successfully created invalid URI \"noscheme\"\n");
	}
	sord_node_free(world, bad_uri);

	sord_world_set_error_sink(world, expected_error, NULL);

	// Attempt to create invalid CURIE
	SerdNode  base = serd_node_from_string(SERD_URI, USTR("http://example.org/"));
	SerdEnv*  env  = serd_env_new(&base);
	SerdNode  sbad = serd_node_from_string(SERD_CURIE, USTR("bad:"));
	SordNode* bad  = sord_node_from_serd_node(world, env, &sbad, NULL, NULL);
	if (bad) {
		return test_fail("Successfully created CURIE with bad namespace\n");
	}
	sord_node_free(world, bad);
	serd_env_free(env);

	// Attempt to create node from garbage
	SerdNode junk = SERD_NODE_NULL;
	junk.type = (SerdType)1234;
	if (sord_node_from_serd_node(world, env, &junk, NULL, NULL)) {
		return test_fail("Successfully created node from garbage serd node\n");
	}

	// Attempt to create NULL node
	SordNode* nil_node = sord_node_from_serd_node(
		world, NULL, &SERD_NODE_NULL, NULL, NULL);
	if (nil_node) {
		return test_fail("Successfully created NULL node\n");
	}
	sord_node_free(world, nil_node);

	// Check node flags are set properly
	SordNode* with_newline = sord_new_literal(world, NULL, USTR("a\nb"), NULL);
	if (!(sord_node_get_flags(with_newline) & SERD_HAS_NEWLINE)) {
		return test_fail("Newline flag not set\n");
	}
	SordNode* with_quote = sord_new_literal(world, NULL, USTR("a\"b"), NULL);
	if (!(sord_node_get_flags(with_quote) & SERD_HAS_QUOTE)) {
		return test_fail("Quote flag not set\n");
	}

	// Create with minimal indexing
	SordModel* sord = sord_new(world, SORD_SPO, false);
	generate(world, sord, n_quads, NULL);

	if (test_read(world, sord, NULL, n_quads)) {
		sord_free(sord);
		sord_world_free(world);
		return EXIT_FAILURE;
	}

	// Check adding tuples with NULL fields fails
	sord_world_set_error_sink(world, expected_error, NULL);
	const size_t initial_num_quads = sord_num_quads(sord);
	SordQuad tup = { 0, 0, 0, 0};
	if (sord_add(sord, tup)) {
		return test_fail("Added NULL tuple\n");
	}
	tup[0] = uri(world, 1);
	if (sord_add(sord, tup)) {
		return test_fail("Added tuple with NULL P and O\n");
	}
	tup[1] = uri(world, 2);
	if (sord_add(sord, tup)) {
		return test_fail("Added tuple with NULL O\n");
	}

	if (sord_num_quads(sord) != initial_num_quads) {
		return test_fail("Num quads %zu != %zu\n",
		                 sord_num_quads(sord), initial_num_quads);
	}

	// Check adding tuples with an active iterator fails
	SordIter* iter = sord_begin(sord);
	tup[2] = uri(world, 3);
	if (sord_add(sord, tup)) {
		return test_fail("Added tuple with active iterator\n");
	}

	// Check removing tuples with several active iterator fails
	SordIter* iter2 = sord_begin(sord);
	if (!sord_erase(sord, iter)) {
		return test_fail("Erased tuple with several active iterators\n");
	}
	n_expected_errors = 0;
	sord_remove(sord, tup);
	if (n_expected_errors != 1) {
		return test_fail("Removed tuple with several active iterators\n");
	}
	sord_iter_free(iter);
	sord_iter_free(iter2);

	sord_world_set_error_sink(world, unexpected_error, NULL);

	// Check interning merges equivalent values
	SordNode* uri_id   = sord_new_uri(world, USTR("http://example.org"));
	SordNode* blank_id = sord_new_blank(world, USTR("testblank"));
	SordNode* lit_id   = sord_new_literal(world, uri_id, USTR("hello"), NULL);
	if (sord_node_get_type(uri_id) != SORD_URI) {
		return test_fail("URI node has incorrect type\n");
	} else if (sord_node_get_type(blank_id) != SORD_BLANK) {
		return test_fail("Blank node has incorrect type\n");
	} else if (sord_node_get_type(lit_id) != SORD_LITERAL) {
		return test_fail("Literal node has incorrect type\n");
	}

	const size_t initial_num_nodes = sord_num_nodes(world);

	SordNode* uri_id2   = sord_new_uri(world, USTR("http://example.org"));
	SordNode* blank_id2 = sord_new_blank(world, USTR("testblank"));
	SordNode* lit_id2   = sord_new_literal(world, uri_id, USTR("hello"), NULL);
	if (uri_id2 != uri_id || !sord_node_equals(uri_id2, uri_id)) {
		fprintf(stderr, "Fail: URI interning failed (duplicates)\n");
		return finished(world, sord, EXIT_FAILURE);
	} else if (blank_id2 != blank_id
	           || !sord_node_equals(blank_id2, blank_id)) {
		fprintf(stderr, "Fail: Blank node interning failed (duplicates)\n");
		return finished(world, sord, EXIT_FAILURE);
	} else if (lit_id2 != lit_id || !sord_node_equals(lit_id2, lit_id)) {
		fprintf(stderr, "Fail: Literal interning failed (duplicates)\n");
		return finished(world, sord, EXIT_FAILURE);
	}

	if (sord_num_nodes(world) != initial_num_nodes) {
		return test_fail("Num nodes %zu != %zu\n",
		                 sord_num_nodes(world), initial_num_nodes);
	}

	const uint8_t ni_hao[] = { 0xE4, 0xBD, 0xA0, 0xE5, 0xA5, 0xBD, 0 };
	SordNode*     chello   = sord_new_literal(world, NULL, ni_hao, "cmn");

	// Test literal length
	size_t         n_bytes;
	size_t         n_chars;
	const uint8_t* str = sord_node_get_string_counted(lit_id2, &n_bytes);
	if (strcmp((const char*)str, "hello")) {
		return test_fail("Literal node corrupt\n");
	} else if (n_bytes != strlen("hello")) {
		return test_fail("ASCII literal byte count incorrect\n");
	}

	str = sord_node_get_string_measured(lit_id2, &n_bytes, &n_chars);
	if (n_bytes != strlen("hello") || n_chars != strlen("hello")) {
		return test_fail("ASCII literal measured length incorrect\n");
	} else if (strcmp((const char*)str, "hello")) {
		return test_fail("ASCII literal string incorrect\n");
	}

	str = sord_node_get_string_measured(chello, &n_bytes, &n_chars);
	if (n_bytes != 6) {
		return test_fail("Multi-byte literal byte count incorrect\n");
	} else if (n_chars != 2) {
		return test_fail("Multi-byte literal character count incorrect\n");
	} else if (strcmp((const char*)str, (const char*)ni_hao)) {
		return test_fail("Multi-byte literal string incorrect\n");
	}

	// Check interning doesn't clash non-equivalent values
	SordNode* uri_id3   = sord_new_uri(world, USTR("http://example.orgX"));
	SordNode* blank_id3 = sord_new_blank(world, USTR("testblankX"));
	SordNode* lit_id3   = sord_new_literal(world, uri_id, USTR("helloX"), NULL);
	if (uri_id3 == uri_id || sord_node_equals(uri_id3, uri_id)) {
		fprintf(stderr, "Fail: URI interning failed (clash)\n");
		return finished(world, sord, EXIT_FAILURE);
	} else if (blank_id3 == blank_id || sord_node_equals(blank_id3, blank_id)) {
		fprintf(stderr, "Fail: Blank node interning failed (clash)\n");
		return finished(world, sord, EXIT_FAILURE);
	} else if (lit_id3 == lit_id || sord_node_equals(lit_id3, lit_id)) {
		fprintf(stderr, "Fail: Literal interning failed (clash)\n");
		return finished(world, sord, EXIT_FAILURE);
	}

	// Check literal interning
	SordNode* lit4 = sord_new_literal(world, NULL, USTR("hello"), NULL);
	SordNode* lit5 = sord_new_literal(world, uri_id2, USTR("hello"), NULL);
	SordNode* lit6 = sord_new_literal(world, NULL, USTR("hello"), "en-ca");
	if (lit4 == lit5 || sord_node_equals(lit4, lit5)
	    || lit4 == lit6 || sord_node_equals(lit4, lit6)
	    || lit5 == lit6 || sord_node_equals(lit5, lit6)) {
		fprintf(stderr, "Fail: Literal interning failed (type/lang clash)\n");
		return finished(world, sord, EXIT_FAILURE);
	}

	// Check relative URI construction
	SordNode* reluri = sord_new_relative_uri(
		world, USTR("a/b"), USTR("http://example.org/"));
	if (strcmp((const char*)sord_node_get_string(reluri),
	           "http://example.org/a/b")) {
		fprintf(stderr, "Fail: Bad relative URI constructed: <%s>\n",
		        sord_node_get_string(reluri));
		return finished(world, sord, EXIT_FAILURE);
	}
	SordNode* reluri2 = sord_new_relative_uri(
		world, USTR("http://drobilla.net/"), USTR("http://example.org/"));
	if (strcmp((const char*)sord_node_get_string(reluri2),
	           "http://drobilla.net/")) {
		fprintf(stderr, "Fail: Bad relative URI constructed: <%s>\n",
		        sord_node_get_string(reluri));
		return finished(world, sord, EXIT_FAILURE);
	}

	// Check comparison with NULL
	sord_node_free(world, uri_id);
	sord_node_free(world, blank_id);
	sord_node_free(world, lit_id);
	sord_node_free(world, uri_id2);
	sord_node_free(world, blank_id2);
	sord_node_free(world, lit_id2);
	sord_node_free(world, uri_id3);
	sord_node_free(world, blank_id3);
	sord_node_free(world, lit_id3);
	sord_free(sord);

	static const char* const index_names[6] = {
		"spo", "sop", "ops", "osp", "pso", "pos"
	};

	for (int i = 0; i < 6; ++i) {
		sord = sord_new(world, (1 << i), false);
		printf("Testing Index `%s'\n", index_names[i]);
		generate(world, sord, n_quads, 0);
		if (test_read(world, sord, 0, n_quads)) {
			return finished(world, sord, EXIT_FAILURE);
		}
		sord_free(sord);
	}

	static const char* const graph_index_names[6] = {
		"gspo", "gsop", "gops", "gosp", "gpso", "gpos"
	};

	for (int i = 0; i < 6; ++i) {
		sord = sord_new(world, (1 << i), true);
		printf("Testing Index `%s'\n", graph_index_names[i]);
		SordNode* graph = uri(world, 42);
		generate(world, sord, n_quads, graph);
		if (test_read(world, sord, graph, n_quads)) {
			return finished(world, sord, EXIT_FAILURE);
		}
		sord_free(sord);
	}

	// Test removing
	sord = sord_new(world, SORD_SPO, true);
	tup[0] = uri(world, 1);
	tup[1] = uri(world, 2);
	tup[2] = sord_new_literal(world, 0, USTR("hello"), NULL);
	tup[3] = 0;
	sord_add(sord, tup);
	if (!sord_ask(sord, tup[0], tup[1], tup[2], tup[3])) {
		fprintf(stderr, "Failed to add tuple\n");
		return finished(world, sord, EXIT_FAILURE);
	}
	sord_node_free(world, (SordNode*)tup[2]);
	tup[2] = sord_new_literal(world, 0, USTR("hi"), NULL);
	sord_add(sord, tup);
	sord_remove(sord, tup);
	if (sord_num_quads(sord) != 1) {
		fprintf(stderr, "Remove failed (%zu quads, expected 1)\n",
		        sord_num_quads(sord));
		return finished(world, sord, EXIT_FAILURE);
	}

	iter = sord_find(sord, tup);
	if (!sord_iter_end(iter)) {
		fprintf(stderr, "Found removed tuple\n");
		return finished(world, sord, EXIT_FAILURE);
	}
	sord_iter_free(iter);

	// Test double remove (silent success)
	sord_remove(sord, tup);

	// Load a couple graphs
	SordNode* graph42 = uri(world, 42);
	SordNode* graph43 = uri(world, 43);
	generate(world, sord, 1, graph42);
	generate(world, sord, 1, graph43);

	// Remove one graph via iterator
	SerdStatus st;
	iter = sord_search(sord, NULL, NULL, NULL, graph43);
	while (!sord_iter_end(iter)) {
		if ((st = sord_erase(sord, iter))) {
			fprintf(stderr, "Remove by iterator failed (%s)\n",
			        serd_strerror(st));
			return finished(world, sord, EXIT_FAILURE);
		}
	}
	sord_iter_free(iter);

	// Erase the first tuple (an element in the default graph)
	iter = sord_begin(sord);
	if (sord_erase(sord, iter)) {
		return test_fail("Failed to erase begin iterator on non-empty model\n");
	}
	sord_iter_free(iter);

	// Ensure only the other graph is left
	SordQuad quad;
	SordQuad pat = { 0, 0, 0, graph42 };
	for (iter = sord_begin(sord); !sord_iter_end(iter); sord_iter_next(iter)) {
		sord_iter_get(iter, quad);
		if (!sord_quad_match(quad, pat)) {
			fprintf(stderr, "Graph removal via iteration failed\n");
			return finished(world, sord, EXIT_FAILURE);
		}
	}
	sord_iter_free(iter);

	// Load file into two separate graphs
	sord_free(sord);
	sord = sord_new(world, SORD_SPO, true);
	env  = serd_env_new(&base);
	SordNode*   graph1 = sord_new_uri(world, USTR("http://example.org/graph1"));
	SordNode*   graph2 = sord_new_uri(world, USTR("http://example.org/graph2"));
	SerdReader* reader = sord_new_reader(sord, env, SERD_TURTLE, graph1);
	if ((st = serd_reader_read_string(reader, USTR("<s> <p> <o> .")))) {
		fprintf(stderr, "Failed to read string (%s)\n", serd_strerror(st));
		return finished(world, sord, EXIT_FAILURE);
	}
	serd_reader_free(reader);
	reader = sord_new_reader(sord, env, SERD_TURTLE, graph2);
	if ((st = serd_reader_read_string(reader, USTR("<s> <p> <o> .")))) {
		fprintf(stderr, "Failed to re-read string (%s)\n", serd_strerror(st));
		return finished(world, sord, EXIT_FAILURE);
	}
	serd_reader_free(reader);
	serd_env_free(env);

	// Ensure we only see triple once
	size_t n_triples = 0;
	for (iter = sord_begin(sord); !sord_iter_end(iter); sord_iter_next(iter)) {
		fprintf(stderr, "%s %s %s %s\n",
		        sord_node_get_string(sord_iter_get_node(iter, SORD_SUBJECT)),
		        sord_node_get_string(sord_iter_get_node(iter, SORD_PREDICATE)),
		        sord_node_get_string(sord_iter_get_node(iter, SORD_OBJECT)),
		        sord_node_get_string(sord_iter_get_node(iter, SORD_GRAPH)));

		++n_triples;
	}
	sord_iter_free(iter);
	if (n_triples != 1) {
		fprintf(stderr, "Found duplicate triple\n");
		return finished(world, sord, EXIT_FAILURE);
	}

	// Test SPO iteration on an SOP indexed store
	sord_free(sord);
	sord = sord_new(world, SORD_SOP, false);
	generate(world, sord, 1, graph42);
	for (iter = sord_begin(sord); !sord_iter_end(iter); sord_iter_next(iter)) {
		++n_triples;
	}
	sord_iter_free(iter);

	return finished(world, sord, EXIT_SUCCESS);
}
