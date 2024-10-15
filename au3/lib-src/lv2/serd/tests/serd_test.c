/*
  Copyright 2011-2017 David Robillard <http://drobilla.net>

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

#undef NDEBUG

#include <assert.h>
#include <float.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "serd/serd.h"

#define USTR(s) ((const uint8_t*)(s))

#ifndef INFINITY
#    define INFINITY (DBL_MAX + DBL_MAX)
#endif
#ifndef NAN
#    define NAN (INFINITY - INFINITY)
#endif

static void
test_strtod(double dbl, double max_delta)
{
	char buf[1024];
	snprintf(buf, sizeof(buf), "%f", dbl);

	char* endptr = NULL;
	const double out = serd_strtod(buf, &endptr);

	const double diff = fabs(out - dbl);
	assert(diff <= max_delta);
}

static SerdStatus
count_prefixes(void* handle, const SerdNode* name, const SerdNode* uri)
{
	(void)name;
	(void)uri;

	++*(int*)handle;
	return SERD_SUCCESS;
}

typedef struct {
	int             n_statements;
	const SerdNode* graph;
} ReaderTest;

static SerdStatus
test_sink(void*              handle,
          SerdStatementFlags flags,
          const SerdNode*    graph,
          const SerdNode*    subject,
          const SerdNode*    predicate,
          const SerdNode*    object,
          const SerdNode*    object_datatype,
          const SerdNode*    object_lang)
{
	(void)flags;
	(void)subject;
	(void)predicate;
	(void)object;
	(void)object_datatype;
	(void)object_lang;

	ReaderTest* rt = (ReaderTest*)handle;
	++rt->n_statements;
	rt->graph = graph;
	return SERD_SUCCESS;
}

static void
test_file_uri(const char* hostname,
              const char* path,
              bool        escape,
              const char* expected_uri,
              const char* expected_path)
{
	if (!expected_path) {
		expected_path = path;
	}

	SerdNode node = serd_node_new_file_uri(
		USTR(path), USTR(hostname), 0, escape);

	uint8_t* out_hostname = NULL;
	uint8_t* out_path     = serd_file_uri_parse(node.buf, &out_hostname);
	assert(!strcmp((const char*)node.buf, expected_uri));
	assert((hostname && out_hostname) || (!hostname && !out_hostname));
	assert(!strcmp((const char*)out_path, (const char*)expected_path));

	serd_free(out_path);
	serd_free(out_hostname);
	serd_node_free(&node);
}

int
main(void)
{
#define MAX       1000000
#define NUM_TESTS 1000
	for (int i = 0; i < NUM_TESTS; ++i) {
		double dbl = rand() % MAX;
		dbl += (rand() % MAX) / (double)MAX;

		test_strtod(dbl, 1 / (double)MAX);
	}

	const double expt_test_nums[] = {
		2.0E18, -5e19, +8e20, 2e+24, -5e-5, 8e0, 9e-0, 2e+0
	};

	const char* expt_test_strs[] = {
		"02e18", "-5e019", "+8e20", "2E+24", "-5E-5", "8E0", "9e-0", " 2e+0"
	};

	for (unsigned i = 0; i < sizeof(expt_test_nums) / sizeof(double); ++i) {
		const double num   = serd_strtod(expt_test_strs[i], NULL);
		const double delta = fabs(num - expt_test_nums[i]);
		assert(delta <= DBL_EPSILON);
	}

	// Test serd_node_new_decimal

	const double dbl_test_nums[] = {
		0.0, 9.0, 10.0, .01, 2.05, -16.00001, 5.000000005, 0.0000000001, NAN, INFINITY
	};

	const char* dbl_test_strs[] = {
		"0.0", "9.0", "10.0", "0.01", "2.05", "-16.00001", "5.00000001", "0.0", NULL, NULL
	};

	for (unsigned i = 0; i < sizeof(dbl_test_nums) / sizeof(double); ++i) {
		SerdNode   node = serd_node_new_decimal(dbl_test_nums[i], 8);
		const bool pass = (node.buf && dbl_test_strs[i])
			? !strcmp((const char*)node.buf, (const char*)dbl_test_strs[i])
			: ((const char*)node.buf == dbl_test_strs[i]);
		assert(pass);
		const size_t len = node.buf ? strlen((const char*)node.buf) : 0;
		assert(node.n_bytes == len && node.n_chars == len);
		serd_node_free(&node);
	}

	// Test serd_node_new_integer

	const long int_test_nums[] = {
		0, -0, -23, 23, -12340, 1000, -1000
	};

	const char* int_test_strs[] = {
		"0", "0", "-23", "23", "-12340", "1000", "-1000"
	};

	for (unsigned i = 0; i < sizeof(int_test_nums) / sizeof(double); ++i) {
		SerdNode node = serd_node_new_integer(int_test_nums[i]);
		assert(!strcmp((const char*)node.buf, (const char*)int_test_strs[i]));
		const size_t len = strlen((const char*)node.buf);
		assert(node.n_bytes == len && node.n_chars == len);
		serd_node_free(&node);
	}

	// Test serd_node_new_blob
	for (size_t size = 0; size < 256; ++size) {
		uint8_t* data = (uint8_t*)malloc(size);
		for (size_t i = 0; i < size; ++i) {
			data[i] = (uint8_t)(rand() % 256);
		}

		SerdNode blob = serd_node_new_blob(data, size, size % 5);

		assert(blob.n_bytes == blob.n_chars);
		assert(blob.n_bytes == strlen((const char*)blob.buf));

		size_t   out_size;
		uint8_t* out = (uint8_t*)serd_base64_decode(
			blob.buf, blob.n_bytes, &out_size);
		assert(out_size == size);

		for (size_t i = 0; i < size; ++i) {
			assert(out[i] == data[i]);
		}

		serd_node_free(&blob);
		serd_free(out);
		free(data);
	}

	// Test serd_strlen

	const uint8_t str[] = { '"', '5', 0xE2, 0x82, 0xAC, '"', '\n', 0 };

	size_t        n_bytes;
	SerdNodeFlags flags;
	size_t        len = serd_strlen(str, &n_bytes, &flags);
	assert(len == 5 && n_bytes == 7 &&
	       flags == (SERD_HAS_QUOTE | SERD_HAS_NEWLINE));
	len = serd_strlen(str, NULL, &flags);
	assert(len == 5);

	assert(serd_strlen(str, &n_bytes, NULL) == 5);

	// Test serd_strerror

	const uint8_t* msg = NULL;
	assert(!strcmp((const char*)(msg = serd_strerror(SERD_SUCCESS)), "Success"));
	for (int i = SERD_FAILURE; i <= SERD_ERR_INTERNAL; ++i) {
		msg = serd_strerror((SerdStatus)i);
		assert(strcmp((const char*)msg, "Success"));
	}
	msg = serd_strerror((SerdStatus)-1);

	// Test serd_uri_to_path

	const uint8_t* uri = (const uint8_t*)"file:///home/user/foo.ttl";
	assert(!strcmp((const char*)serd_uri_to_path(uri), "/home/user/foo.ttl"));

	uri = (const uint8_t*)"file://localhost/home/user/foo.ttl";
	assert(!strcmp((const char*)serd_uri_to_path(uri), "/home/user/foo.ttl"));

	uri = (const uint8_t*)"file:illegal/file/uri";
	assert(!serd_uri_to_path(uri));

	uri = (const uint8_t*)"file:///c:/awful/system";
	assert(!strcmp((const char*)serd_uri_to_path(uri), "c:/awful/system"));

	uri = (const uint8_t*)"file:///c:awful/system";
	assert(!strcmp((const char*)serd_uri_to_path(uri), "/c:awful/system"));

	uri = (const uint8_t*)"file:///0/1";
	assert(!strcmp((const char*)serd_uri_to_path(uri), "/0/1"));

	uri = (const uint8_t*)"C:\\Windows\\Sucks";
	assert(!strcmp((const char*)serd_uri_to_path(uri), "C:\\Windows\\Sucks"));

	uri = (const uint8_t*)"C|/Windows/Sucks";
	assert(!strcmp((const char*)serd_uri_to_path(uri), "C|/Windows/Sucks"));

	// Test file URI escaping and parsing

	test_file_uri(NULL, "C:/My 100%", true,
	              "file:///C:/My%20100%%", NULL);
	test_file_uri("ahost", "C:\\Pointless Space", true,
	              "file://ahost/C:/Pointless%20Space", "C:/Pointless Space");
	test_file_uri(NULL, "/foo/bar", true,
	              "file:///foo/bar", NULL);
	test_file_uri("bhost", "/foo/bar", true,
	              "file://bhost/foo/bar", NULL);
	test_file_uri(NULL, "a/relative path", false,
	              "a/relative path", NULL);
	test_file_uri(NULL, "a/relative <path>", true,
	              "a/relative%20%3Cpath%3E", NULL);

	// Test tolerance of parsing junk URI escapes

	uint8_t* out_path = serd_file_uri_parse(USTR("file:///foo/%0Xbar"), NULL);
	assert(!strcmp((const char*)out_path, "/foo/bar"));
	serd_free(out_path);

	// Test serd_node_equals

	const uint8_t replacement_char_str[] = { 0xEF, 0xBF, 0xBD, 0 };
	SerdNode lhs = serd_node_from_string(SERD_LITERAL, replacement_char_str);
	SerdNode rhs = serd_node_from_string(SERD_LITERAL, USTR("123"));
	assert(!serd_node_equals(&lhs, &rhs));

	SerdNode qnode = serd_node_from_string(SERD_CURIE, USTR("foo:bar"));
	assert(!serd_node_equals(&lhs, &qnode));
	assert(serd_node_equals(&lhs, &lhs));

	SerdNode null_copy = serd_node_copy(&SERD_NODE_NULL);
	assert(serd_node_equals(&SERD_NODE_NULL, &null_copy));

	// Test serd_node_from_string

	SerdNode node = serd_node_from_string(SERD_LITERAL, (const uint8_t*)"hello\"");
	assert(node.n_bytes == 6 && node.n_chars == 6 &&
	       node.flags == SERD_HAS_QUOTE &&
	       !strcmp((const char*)node.buf, "hello\""));

	node = serd_node_from_string(SERD_URI, NULL);
	assert(serd_node_equals(&node, &SERD_NODE_NULL));

	// Test serd_node_from_substring

	SerdNode empty = serd_node_from_substring(SERD_LITERAL, NULL, 32);
	assert(!empty.buf && !empty.n_bytes && !empty.n_chars && !empty.flags &&
	       !empty.type);

	SerdNode a_b = serd_node_from_substring(SERD_LITERAL, USTR("a\"bc"), 3);
	assert(a_b.n_bytes == 3 && a_b.n_chars == 3 &&
	       a_b.flags == SERD_HAS_QUOTE &&
	       !strncmp((const char*)a_b.buf, "a\"b", 3));

	a_b = serd_node_from_substring(SERD_LITERAL, USTR("a\"bc"), 10);
	assert(a_b.n_bytes == 4 && a_b.n_chars == 4 &&
	       a_b.flags == SERD_HAS_QUOTE &&
	       !strncmp((const char*)a_b.buf, "a\"bc", 4));

	// Test serd_node_new_uri_from_string

	SerdNode nonsense = serd_node_new_uri_from_string(NULL, NULL, NULL);
	assert(nonsense.type == SERD_NOTHING);

	SerdURI base_uri;
	SerdNode base = serd_node_new_uri_from_string(USTR("http://example.org/"),
	                                              NULL, &base_uri);
	SerdNode nil = serd_node_new_uri_from_string(NULL, &base_uri, NULL);
	SerdNode nil2 = serd_node_new_uri_from_string(USTR(""), &base_uri, NULL);
	assert(nil.type == SERD_URI);
	assert(!strcmp((const char*)nil.buf, (const char*)base.buf));
	assert(nil2.type == SERD_URI);
	assert(!strcmp((const char*)nil2.buf, (const char*)base.buf));
	serd_node_free(&nil);
	serd_node_free(&nil2);

	// Test serd_node_new_relative_uri
	SerdNode abs = serd_node_from_string(SERD_URI, USTR("http://example.org/foo/bar"));
	SerdURI  abs_uri;
	serd_uri_parse(abs.buf, &abs_uri);

	SerdURI  rel_uri;
	SerdNode rel = serd_node_new_relative_uri(&abs_uri, &base_uri, NULL, &rel_uri);
	assert(!strcmp((const char*)rel.buf, "/foo/bar"));

	SerdNode up = serd_node_new_relative_uri(&base_uri, &abs_uri, NULL, NULL);
	assert(!strcmp((const char*)up.buf, "../"));

	SerdNode noup = serd_node_new_relative_uri(&base_uri, &abs_uri, &abs_uri, NULL);
	assert(!strcmp((const char*)noup.buf, "http://example.org/"));

	SerdNode x = serd_node_from_string(SERD_URI, USTR("http://example.org/foo/x"));
	SerdURI  x_uri;
	serd_uri_parse(x.buf, &x_uri);

	SerdNode x_rel = serd_node_new_relative_uri(&x_uri, &abs_uri, &abs_uri, NULL);
	assert(!strcmp((const char*)x_rel.buf, "x"));

	serd_node_free(&x_rel);
	serd_node_free(&noup);
	serd_node_free(&up);
	serd_node_free(&rel);
	serd_node_free(&base);

	// Test SerdEnv

	SerdNode u   = serd_node_from_string(SERD_URI, USTR("http://example.org/foo"));
	SerdNode b   = serd_node_from_string(SERD_CURIE, USTR("invalid"));
	SerdNode c   = serd_node_from_string(SERD_CURIE, USTR("eg.2:b"));
	SerdEnv* env = serd_env_new(NULL);
	serd_env_set_prefix_from_strings(env, USTR("eg.2"), USTR("http://example.org/"));

	assert(serd_env_set_base_uri(env, NULL));
	assert(serd_env_set_base_uri(env, &node));
	assert(serd_node_equals(serd_env_get_base_uri(env, NULL), &node));

	SerdChunk prefix, suffix;
	assert(serd_env_expand(env, &b, &prefix, &suffix));

	SerdNode xnode = serd_env_expand_node(env, &node);
	assert(serd_node_equals(&xnode, &SERD_NODE_NULL));

	SerdNode xu = serd_env_expand_node(env, &u);
	assert(!strcmp((const char*)xu.buf, "http://example.org/foo"));
	serd_node_free(&xu);

	SerdNode badpre = serd_node_from_string(SERD_CURIE, USTR("hm:what"));
	SerdNode xbadpre = serd_env_expand_node(env, &badpre);
	assert(serd_node_equals(&xbadpre, &SERD_NODE_NULL));

	SerdNode xc = serd_env_expand_node(env, &c);
	assert(!strcmp((const char*)xc.buf, "http://example.org/b"));
	serd_node_free(&xc);

	assert(serd_env_set_prefix(env, &SERD_NODE_NULL, &SERD_NODE_NULL));

	const SerdNode lit = serd_node_from_string(SERD_LITERAL, USTR("hello"));
	assert(serd_env_set_prefix(env, &b, &lit));

	int n_prefixes = 0;
	serd_env_set_prefix_from_strings(env, USTR("eg.2"), USTR("http://example.org/"));
	serd_env_foreach(env, count_prefixes, &n_prefixes);
	assert(n_prefixes == 1);

	SerdNode shorter_uri = serd_node_from_string(SERD_URI, USTR("urn:foo"));
	SerdNode prefix_name;
	assert(!serd_env_qualify(env, &shorter_uri, &prefix_name, &suffix));

	// Test SerdReader and SerdWriter

	const char* path = "serd_test.ttl";
	FILE* fd = fopen(path, "wb");
	assert(fd);

	SerdWriter* writer = serd_writer_new(
		SERD_TURTLE, (SerdStyle)0, env, NULL, serd_file_sink, fd);
	assert(writer);

	serd_writer_chop_blank_prefix(writer, USTR("tmp"));
	serd_writer_chop_blank_prefix(writer, NULL);

	assert(serd_writer_set_base_uri(writer, &lit));
	assert(serd_writer_set_prefix(writer, &lit, &lit));
	assert(serd_writer_end_anon(writer, NULL));
	assert(serd_writer_get_env(writer) == env);

	uint8_t buf[] = { 0x80, 0, 0, 0, 0 };
	SerdNode s = serd_node_from_string(SERD_URI, USTR(""));
	SerdNode p = serd_node_from_string(SERD_URI, USTR("http://example.org/pred"));
	SerdNode o = serd_node_from_string(SERD_LITERAL, buf);

	// Write 3 invalid statements (should write nothing)
	const SerdNode* junk[][5] = { { &s, &p, NULL, NULL, NULL },
	                              { &s, NULL, &o, NULL, NULL },
	                              { NULL, &p, &o, NULL, NULL },
	                              { &s, &p, &SERD_NODE_NULL, NULL, NULL },
	                              { &s, &SERD_NODE_NULL, &o, NULL, NULL },
	                              { &SERD_NODE_NULL, &p, &o, NULL, NULL },
	                              { &s, &o, &o, NULL, NULL },
	                              { &o, &p, &o, NULL, NULL },
	                              { &s, &p, &SERD_NODE_NULL, NULL, NULL },
	                              { NULL, NULL, NULL, NULL, NULL } };
	for (unsigned i = 0; i < sizeof(junk) / (sizeof(SerdNode*) * 5); ++i) {
		assert(serd_writer_write_statement(
			       writer, 0, NULL,
			       junk[i][0], junk[i][1], junk[i][2], junk[i][3], junk[i][4]));
	}

	const SerdNode t = serd_node_from_string(SERD_URI, USTR("urn:Type"));
	const SerdNode l = serd_node_from_string(SERD_LITERAL, USTR("en"));
	const SerdNode* good[][5] = { { &s, &p, &o, NULL, NULL },
	                              { &s, &p, &o, &SERD_NODE_NULL, &SERD_NODE_NULL },
	                              { &s, &p, &o, &t, NULL },
	                              { &s, &p, &o, NULL, &l },
	                              { &s, &p, &o, &t, &l },
	                              { &s, &p, &o, &t, &SERD_NODE_NULL },
	                              { &s, &p, &o, &SERD_NODE_NULL, &l },
	                              { &s, &p, &o, NULL, &SERD_NODE_NULL },
	                              { &s, &p, &o, &SERD_NODE_NULL, NULL },
	                              { &s, &p, &o, &SERD_NODE_NULL, NULL } };
	for (unsigned i = 0; i < sizeof(good) / (sizeof(SerdNode*) * 5); ++i) {
		assert(!serd_writer_write_statement(
			       writer, 0, NULL,
			       good[i][0], good[i][1], good[i][2], good[i][3], good[i][4]));
	}

	// Write statements with bad UTF-8 (should be replaced)
	const uint8_t bad_str[] = { 0xFF, 0x90, 'h', 'i', 0 };
	SerdNode      bad_lit   = serd_node_from_string(SERD_LITERAL, bad_str);
	SerdNode      bad_uri   = serd_node_from_string(SERD_URI, bad_str);
	assert(!serd_writer_write_statement(writer, 0, NULL,
	                                    &s, &p, &bad_lit, NULL, NULL));
	assert(!serd_writer_write_statement(writer, 0, NULL,
	                                    &s, &p, &bad_uri, NULL, NULL));

	// Write 1 valid statement
	o = serd_node_from_string(SERD_LITERAL, USTR("hello"));
	assert(!serd_writer_write_statement(writer, 0, NULL,
	                                    &s, &p, &o, NULL, NULL));

	serd_writer_free(writer);

	// Test chunk sink
	SerdChunk chunk = { NULL, 0 };
	writer = serd_writer_new(
		SERD_TURTLE, (SerdStyle)0, env, NULL, serd_chunk_sink, &chunk);

	o = serd_node_from_string(SERD_URI, USTR("http://example.org/base"));
	assert(!serd_writer_set_base_uri(writer, &o));

	serd_writer_free(writer);
	uint8_t* out = serd_chunk_sink_finish(&chunk);

	assert(!strcmp((const char*)out, "@base <http://example.org/base> .\n"));
	serd_free(out);

	// Rewind and test reader
	fseek(fd, 0, SEEK_SET);

	ReaderTest* rt     = (ReaderTest*)calloc(1, sizeof(ReaderTest));
	SerdReader* reader = serd_reader_new(
		SERD_TURTLE, rt, free,
		NULL, NULL, test_sink, NULL);
	assert(reader);
	assert(serd_reader_get_handle(reader) == rt);

	SerdNode g = serd_node_from_string(SERD_URI, USTR("http://example.org/"));
	serd_reader_set_default_graph(reader, &g);
	serd_reader_add_blank_prefix(reader, USTR("tmp"));
	serd_reader_add_blank_prefix(reader, NULL);

	assert(serd_reader_read_file(reader, USTR("http://notafile")));
	assert(serd_reader_read_file(reader, USTR("file:///better/not/exist")));
	assert(serd_reader_read_file(reader, USTR("file://")));

	const SerdStatus st = serd_reader_read_file(reader, USTR(path));
	assert(!st);
	assert(rt->n_statements == 13);
	assert(rt->graph && rt->graph->buf &&
	       !strcmp((const char*)rt->graph->buf, "http://example.org/"));

	assert(serd_reader_read_string(reader, USTR("This isn't Turtle at all.")));

	serd_reader_free(reader);
	fclose(fd);

	serd_env_free(env);

	printf("Success\n");
	return 0;
}
