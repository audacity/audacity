/*
  Copyright 2011-2012 David Robillard <http://drobilla.net>

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

#include <float.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "serd/serd.h"

#define USTR(s) ((const uint8_t*)(s))

#ifdef _WIN32
#    define INFINITY (DBL_MAX + DBL_MAX)
#    define NAN      (INFINITY - INFINITY)
#endif

static int
failure(const char* fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	fprintf(stderr, "error: ");
	vfprintf(stderr, fmt, args);
	va_end(args);
	return 1;
}

static bool
test_strtod(double dbl, double max_delta)
{
	char buf[1024];
	snprintf(buf, sizeof(buf), "%lf", dbl);

	char* endptr = NULL;
	const double out = serd_strtod(buf, &endptr);

	const double diff = fabs(out - dbl);
	if (diff > max_delta) {
		return !failure("Parsed %lf != %lf (delta %lf)\n", dbl, out, diff);
	}
	return true;
}

static SerdStatus
count_prefixes(void* handle, const SerdNode* name, const SerdNode* uri)
{
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
	ReaderTest* rt = (ReaderTest*)handle;
	++rt->n_statements;
	rt->graph = graph;
	return SERD_SUCCESS;
}

int
main(void)
{
#define MAX       1000000
#define NUM_TESTS 1000
	for (int i = 0; i < NUM_TESTS; ++i) {
		double dbl = rand() % MAX;
		dbl += (rand() % MAX) / (double)MAX;

		if (!test_strtod(dbl, 1 / (double)MAX)) {
			return 1;
		}
	}

	const double expt_test_nums[] = {
		2.0E18, -5e19, +8e20, 2e+34, -5e-5, 8e0, 9e-0, 2e+0
	};

	const char* expt_test_strs[] = {
		"02e18", "-5e019", "+8e20", "2E+34", "-5E-5", "8E0", "9e-0", " 2e+0"
	};

	for (unsigned i = 0; i < sizeof(expt_test_nums) / sizeof(double); ++i) {
		const double num   = serd_strtod(expt_test_strs[i], NULL);
		const double delta = fabs(num - expt_test_nums[i]);
		if (delta > DBL_EPSILON) {
			return failure("Parsed `%s' %lf != %lf (delta %lf)\n",
			               expt_test_strs[i], num, expt_test_nums[i], delta);
		}
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
		if (!pass) {
			return failure("Serialised `%s' != %s\n",
			               node.buf, dbl_test_strs[i]);
		}
		const size_t len = node.buf ? strlen((const char*)node.buf) : 0;
		if (node.n_bytes != len || node.n_chars != len) {
			return failure("Length %zu,%zu != %zu\n",
			               node.n_bytes, node.n_chars, len);
		}
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
		if (strcmp((const char*)node.buf, (const char*)int_test_strs[i])) {
			return failure("Serialised `%s' != %s\n",
			               node.buf, int_test_strs[i]);
		}
		const size_t len = strlen((const char*)node.buf);
		if (node.n_bytes != len || node.n_chars != len) {
			return failure("Length %zu,%zu != %zu\n",
			               node.n_bytes, node.n_chars, len);
		}
		serd_node_free(&node);
	}

	// Test serd_node_new_blob
	for (size_t size = 0; size < 256; ++size) {
		uint8_t* data = (uint8_t*)malloc(size);
		for (size_t i = 0; i < size; ++i) {
			data[i] = (uint8_t)(rand() % 256);
		}

		SerdNode blob = serd_node_new_blob(data, size, size % 5);

		if (blob.n_bytes != blob.n_chars) {
			return failure("Blob %zu bytes != %zu chars\n",
			               blob.n_bytes, blob.n_chars);
		}

		size_t   out_size;
		uint8_t* out = (uint8_t*)serd_base64_decode(
			blob.buf, blob.n_bytes, &out_size);
		if (out_size != size) {
			return failure("Blob size %zu != %zu\n", out_size, size);
		}

		for (size_t i = 0; i < size; ++i) {
			if (out[i] != data[i]) {
				return failure("Corrupt blob at byte %zu\n", i);
			}
		}

		serd_node_free(&blob);
		free(out);
		free(data);
	}

	// Test serd_strlen

	const uint8_t str[] = { '"', '5', 0xE2, 0x82, 0xAC, '"', '\n', 0 };

	size_t        n_bytes;
	SerdNodeFlags flags;
	size_t        len = serd_strlen(str, &n_bytes, &flags);
	if (len != 5 || n_bytes != 7
	    || flags != (SERD_HAS_QUOTE|SERD_HAS_NEWLINE)) {
		return failure("Bad serd_strlen(%s) len=%zu n_bytes=%zu flags=%u\n",
		        str, len, n_bytes, flags);
	}
	len = serd_strlen(str, NULL, &flags);
	if (len != 5) {
		return failure("Bad serd_strlen(%s) len=%zu flags=%u\n",
		        str, len, flags);
	}

	// Test serd_strerror

	const uint8_t* msg = NULL;
	if (strcmp((const char*)(msg = serd_strerror(SERD_SUCCESS)), "Success")) {
		return failure("Bad message `%s' for SERD_SUCCESS\n", msg);
	}
	for (int i = SERD_FAILURE; i <= SERD_ERR_INTERNAL; ++i) {
		msg = serd_strerror((SerdStatus)i);
		if (!strcmp((const char*)msg, "Success")) {
			return failure("Bad message `%s' for (SerdStatus)%d\n", msg, i);
		}
	}
	msg = serd_strerror((SerdStatus)-1);

	// Test serd_uri_to_path

	const uint8_t* uri = (const uint8_t*)"file:///home/user/foo.ttl";
	if (strcmp((const char*)serd_uri_to_path(uri), "/home/user/foo.ttl")) {
		return failure("Bad path %s for %s\n", serd_uri_to_path(uri), uri);
	}
	uri = (const uint8_t*)"file://localhost/home/user/foo.ttl";
	if (strcmp((const char*)serd_uri_to_path(uri), "/home/user/foo.ttl")) {
		return failure("Bad path %s for %s\n", serd_uri_to_path(uri), uri);
	}
	uri = (const uint8_t*)"file:illegal/file/uri";
	if (serd_uri_to_path(uri)) {
		return failure("Converted invalid URI `%s' to path `%s'\n",
		        uri, serd_uri_to_path(uri));
	}
	uri = (const uint8_t*)"file:///c:/awful/system";
	if (strcmp((const char*)serd_uri_to_path(uri), "c:/awful/system")) {
		return failure("Bad path %s for %s\n", serd_uri_to_path(uri), uri);
	}
	uri = (const uint8_t*)"file:///c:awful/system";
	if (strcmp((const char*)serd_uri_to_path(uri), "/c:awful/system")) {
		return failure("Bad path %s for %s\n", serd_uri_to_path(uri), uri);
	}
	uri = (const uint8_t*)"file:///0/1";
	if (strcmp((const char*)serd_uri_to_path(uri), "/0/1")) {
		return failure("Bad path %s for %s\n", serd_uri_to_path(uri), uri);
	}
	uri = (const uint8_t*)"C:\\Windows\\Sucks";
	if (strcmp((const char*)serd_uri_to_path(uri), "C:\\Windows\\Sucks")) {
		return failure("Bad path %s for %s\n", serd_uri_to_path(uri), uri);
	}
	uri = (const uint8_t*)"C|/Windows/Sucks";
	if (strcmp((const char*)serd_uri_to_path(uri), "C|/Windows/Sucks")) {
		return failure("Bad path %s for %s\n", serd_uri_to_path(uri), uri);
	}

	// Test serd_node_new_file_uri and serd_file_uri_parse
	SerdURI        furi;
	const uint8_t* path_str  = USTR("C:/My 100%");
	SerdNode       file_node = serd_node_new_file_uri(path_str, 0, &furi, true);
	uint8_t*       hostname  = NULL;
	uint8_t*       out_path  = serd_file_uri_parse(file_node.buf, &hostname);
	if (strcmp((const char*)file_node.buf, "file:///C:/My%20100%%")) {
		return failure("Bad URI %s\n", file_node.buf);
	} else if (hostname) {
		return failure("hostname `%s' shouldn't exist\n", hostname);
	} else if (strcmp((const char*)path_str, (const char*)out_path)) {
		return failure("path=>URI=>path failure %s => %s => %s\n",
		               path_str, file_node.buf, out_path);
	}
	free(out_path);
	serd_node_free(&file_node);

	path_str  = USTR("C:\\Pointless Space");
	file_node = serd_node_new_file_uri(path_str, USTR("pwned"), 0, true);
	hostname  = NULL;
	out_path  = serd_file_uri_parse(file_node.buf, &hostname);
	if (strcmp((const char*)file_node.buf, "file://pwned/C:/Pointless%20Space")) {
		return failure("Bad URI %s\n", file_node.buf);
	} else if (!hostname || strcmp((const char*)hostname, "pwned")) {
		return failure("Bad hostname `%s'\n", hostname);
	} else if (strcmp((const char*)out_path, "C:/Pointless Space")) {
		return failure("path=>URI=>path failure %s => %s => %s\n",
		               path_str, file_node.buf, out_path);
	}
	free(hostname);
	free(out_path);
	serd_node_free(&file_node);

	path_str  = USTR("/foo/bar");
	file_node = serd_node_new_file_uri(path_str, 0, 0, true);
	hostname  = NULL;
	out_path  = serd_file_uri_parse(file_node.buf, &hostname);
	if (strcmp((const char*)file_node.buf, "file:///foo/bar")) {
		return failure("Bad URI %s\n", file_node.buf);
	} else if (hostname) {
		return failure("hostname `%s' shouldn't exist\n", hostname);
	} else if (strcmp((const char*)path_str, (const char*)out_path)) {
		return failure("path=>URI=>path failure %s => %s => %s\n",
		               path_str, file_node.buf, out_path);
	}
	free(out_path);
	serd_node_free(&file_node);

	path_str  = USTR("/foo/bar");
	file_node = serd_node_new_file_uri(path_str, USTR("localhost"), 0, true);
	out_path  = serd_file_uri_parse(file_node.buf, &hostname);
	if (strcmp((const char*)file_node.buf, "file://localhost/foo/bar")) {
		return failure("Bad URI %s\n", file_node.buf);
	} else if (strcmp((const char*)hostname, "localhost")) {
		return failure("incorrect hostname `%s'\n", hostname);
	} else if (strcmp((const char*)path_str, (const char*)out_path)) {
		return failure("path=>URI=>path failure %s => %s => %s\n",
		               path_str, file_node.buf, out_path);
	}
	free(hostname);
	free(out_path);
	serd_node_free(&file_node);

	path_str  = USTR("a/relative path");
	file_node = serd_node_new_file_uri(path_str, 0, 0, false);
	out_path  = serd_file_uri_parse(file_node.buf, &hostname);
	if (strcmp((const char*)file_node.buf, "a/relative path")) {
		return failure("Bad URI %s\n", file_node.buf);
	} else if (hostname) {
		return failure("hostname `%s' shouldn't exist\n", hostname);
	} else if (strcmp((const char*)path_str, (const char*)out_path)) {
		return failure("path=>URI=>path failure %s => %s => %s\n",
		               path_str, file_node.buf, out_path);
	}
	free(hostname);
	free(out_path);
	serd_node_free(&file_node);

	if (serd_file_uri_parse(USTR("file://invalid"), NULL)) {
		return failure("successfully parsed bogus URI <file://invalid>\n");
	}

	out_path = serd_file_uri_parse(USTR("file://host/foo/%XYbar"), NULL);
	if (strcmp((const char*)out_path, "/foo/bar")) {
		return failure("bad tolerance of junk escape: `%s'\n", out_path);
	}
	free(out_path);
	out_path = serd_file_uri_parse(USTR("file://host/foo/%0Abar"), NULL);
	if (strcmp((const char*)out_path, "/foo/bar")) {
		return failure("bad tolerance of junk escape: `%s'\n", out_path);
	}
	free(out_path);

	// Test serd_node_equals

	const uint8_t replacement_char_str[] = { 0xEF, 0xBF, 0xBD, 0 };
	SerdNode lhs = serd_node_from_string(SERD_LITERAL, replacement_char_str);
	SerdNode rhs = serd_node_from_string(SERD_LITERAL, USTR("123"));
	if (serd_node_equals(&lhs, &rhs)) {
		return failure("%s == %s\n", lhs.buf, rhs.buf);
	}

	SerdNode qnode = serd_node_from_string(SERD_CURIE, USTR("foo:bar"));
	if (serd_node_equals(&lhs, &qnode)) {
		return failure("%s == %s\n", lhs.buf, qnode.buf);
	}

	if (!serd_node_equals(&lhs, &lhs)) {
		return failure("%s != %s\n", lhs.buf, lhs.buf);
	}

	// Test serd_node_from_string

	SerdNode node = serd_node_from_string(SERD_LITERAL, (const uint8_t*)"hello\"");
	if (node.n_bytes != 6 || node.n_chars != 6 || node.flags != SERD_HAS_QUOTE
	    || strcmp((const char*)node.buf, "hello\"")) {
		return failure("Bad node %s %zu %zu %d %d\n",
		        node.buf, node.n_bytes, node.n_chars, node.flags, node.type);
	}

	node = serd_node_from_string(SERD_URI, NULL);
	if (!serd_node_equals(&node, &SERD_NODE_NULL)) {
		return failure("Creating node from NULL string failed\n");
	}

	// Test serd_node_new_uri_from_string

	SerdURI base_uri;
	SerdNode base = serd_node_new_uri_from_string(USTR("http://example.org/"),
	                                              NULL, &base_uri);
	SerdNode nil = serd_node_new_uri_from_string(NULL, &base_uri, NULL);
	if (nil.type != SERD_URI || strcmp((const char*)nil.buf, (const char*)base.buf)) {
		return failure("URI %s != base %s\n", nil.buf, base.buf);
	}
	serd_node_free(&base);
	serd_node_free(&nil);

	// Test SerdEnv

	SerdNode u   = serd_node_from_string(SERD_URI, USTR("http://example.org/foo"));
	SerdNode b   = serd_node_from_string(SERD_CURIE, USTR("invalid"));
	SerdNode c   = serd_node_from_string(SERD_CURIE, USTR("eg.2:b"));
	SerdEnv* env = serd_env_new(NULL);
	serd_env_set_prefix_from_strings(env, USTR("eg.2"), USTR("http://example.org/"));

	if (!serd_env_set_base_uri(env, &node)) {
		return failure("Set base URI to %s\n", node.buf);
	}

	SerdChunk prefix, suffix;
	if (!serd_env_expand(env, &b, &prefix, &suffix)) {
		return failure("Expanded invalid curie %s\n", b.buf);
	}

	SerdNode xnode = serd_env_expand_node(env, &node);
	if (!serd_node_equals(&xnode, &SERD_NODE_NULL)) {
		return failure("Expanded %s to %s\n", c.buf, xnode.buf);
	}

	SerdNode xu = serd_env_expand_node(env, &u);
	if (strcmp((const char*)xu.buf, "http://example.org/foo")) {
		return failure("Expanded %s to %s\n", c.buf, xu.buf);
	}
	serd_node_free(&xu);

	SerdNode badpre = serd_node_from_string(SERD_CURIE, USTR("hm:what"));
	SerdNode xbadpre = serd_env_expand_node(env, &badpre);
	if (!serd_node_equals(&xbadpre, &SERD_NODE_NULL)) {
		return failure("Expanded invalid curie %s\n", badpre.buf);
	}

	SerdNode xc = serd_env_expand_node(env, &c);
	if (strcmp((const char*)xc.buf, "http://example.org/b")) {
		return failure("Expanded %s to %s\n", c.buf, xc.buf);
	}
	serd_node_free(&xc);

	if (!serd_env_set_prefix(env, &SERD_NODE_NULL, &SERD_NODE_NULL)) {
		return failure("Set NULL prefix\n");
	}

	const SerdNode lit = serd_node_from_string(SERD_LITERAL, USTR("hello"));
	if (!serd_env_set_prefix(env, &b, &lit)) {
		return failure("Set prefix to literal\n");
	}

	int n_prefixes = 0;
	serd_env_set_prefix_from_strings(env, USTR("eg.2"), USTR("http://example.org/"));
	serd_env_foreach(env, count_prefixes, &n_prefixes);
	if (n_prefixes != 1) {
		return failure("Bad prefix count %d\n", n_prefixes);
	}

	SerdNode shorter_uri = serd_node_from_string(SERD_URI, USTR("urn:foo"));
	SerdNode prefix_name;
	if (serd_env_qualify(env, &shorter_uri, &prefix_name, &suffix)) {
		return failure("Qualified %s\n", shorter_uri.buf);
	}

	// Test SerdReader and SerdWriter

	const char* path = "serd_test.ttl";
	FILE* fd = fopen(path, "w");
	if (!fd) {
		return failure("Failed to open file %s\n", path);
	}

	SerdWriter* writer = serd_writer_new(
		SERD_TURTLE, (SerdStyle)0, env, NULL, serd_file_sink, fd);
	if (!writer) {
		return failure("Failed to create writer\n");
	}

	serd_writer_chop_blank_prefix(writer, USTR("tmp"));
	serd_writer_chop_blank_prefix(writer, NULL);

	if (!serd_writer_set_base_uri(writer, &lit)) {
		return failure("Set base URI to %s\n", lit.buf);
	}

	if (!serd_writer_set_prefix(writer, &lit, &lit)) {
		return failure("Set prefix %s to %s\n", lit.buf, lit.buf);
	}

	if (!serd_writer_end_anon(writer, NULL)) {
		return failure("Ended non-existent anonymous node\n");
	}

	if (serd_writer_get_env(writer) != env) {
		return failure("Writer has incorrect env\n");
	}

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
	                              { NULL, NULL, NULL, NULL, NULL } };
	for (unsigned i = 0; i < sizeof(junk) / (sizeof(SerdNode*) * 5); ++i) {
		if (!serd_writer_write_statement(
			    writer, 0, NULL,
			    junk[i][0], junk[i][1], junk[i][2], junk[i][3], junk[i][4])) {
			return failure("Successfully wrote junk statement %d\n", i);
		}
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
		if (serd_writer_write_statement(
			    writer, 0, NULL,
			    good[i][0], good[i][1], good[i][2], good[i][3], good[i][4])) {
			return failure("Failed to write good statement %d\n", i);
		}
	}

	// Write 1 statement with bad UTF-8 (should be replaced)
	if (serd_writer_write_statement(writer, 0, NULL,
	                                &s, &p, &o, NULL, NULL)) {
		return failure("Failed to write junk UTF-8\n");
	}

	// Write 1 valid statement
	o = serd_node_from_string(SERD_LITERAL, USTR("hello"));
	if (serd_writer_write_statement(writer, 0, NULL,
	                                &s, &p, &o, NULL, NULL)) {
		return failure("Failed to write valid statement\n");
	}

	serd_writer_free(writer);

	// Test chunk sink
	SerdChunk chunk = { NULL, 0 };
	writer = serd_writer_new(
		SERD_TURTLE, (SerdStyle)0, env, NULL, serd_chunk_sink, &chunk);

	o = serd_node_from_string(SERD_URI, USTR("http://example.org/base"));
	if (serd_writer_set_base_uri(writer, &o)) {
		return failure("Failed to write to chunk sink\n");
	}

	serd_writer_free(writer);
	uint8_t* out = serd_chunk_sink_finish(&chunk);

	if (strcmp((const char*)out, "@base <http://example.org/base> .\n")) {
		return failure("Incorrect chunk output:\n%s\n", chunk.buf);
	}

	free(out);

	// Rewind and test reader
	fseek(fd, 0, SEEK_SET);

	ReaderTest* rt   = (ReaderTest*)malloc(sizeof(ReaderTest));
	rt->n_statements = 0;
	rt->graph        = NULL;

	SerdReader* reader = serd_reader_new(
		SERD_TURTLE, rt, free,
		NULL, NULL, test_sink, NULL);
	if (!reader) {
		return failure("Failed to create reader\n");
	}
	if (serd_reader_get_handle(reader) != rt) {
		return failure("Corrupt reader handle\n");
	}

	SerdNode g = serd_node_from_string(SERD_URI, USTR("http://example.org/"));
	serd_reader_set_default_graph(reader, &g);
	serd_reader_add_blank_prefix(reader, USTR("tmp"));
	serd_reader_add_blank_prefix(reader, NULL);

	if (!serd_reader_read_file(reader, USTR("http://notafile"))) {
		return failure("Apparently read an http URI\n");
	}
	if (!serd_reader_read_file(reader, USTR("file:///better/not/exist"))) {
		return failure("Apprently read a non-existent file\n");
	}
	SerdStatus st = serd_reader_read_file(reader, USTR(path));
	if (st) {
		return failure("Error reading file (%s)\n", serd_strerror(st));
	}

	if (rt->n_statements != 12) {
		return failure("Bad statement count %d\n", rt->n_statements);
	} else if (!rt->graph || !rt->graph->buf ||
	           strcmp((const char*)rt->graph->buf, "http://example.org/")) {
		return failure("Bad graph %p\n", rt->graph);
	}

	if (!serd_reader_read_string(reader, USTR("This isn't Turtle at all."))) {
		return failure("Parsed invalid string successfully.\n");
	}

	serd_reader_free(reader);
	fclose(fd);

	serd_env_free(env);

	printf("Success\n");
	return 0;
}
