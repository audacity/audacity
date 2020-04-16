/*
  Copyright 2012-2015 David Robillard <http://drobilla.net>

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

#include "lv2/atom/atom.h"
#include "lv2/atom/forge.h"
#include "lv2/atom/util.h"
#include "lv2/urid/urid.h"

#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char**   uris   = NULL;
uint32_t n_uris = 0;

static char*
copy_string(const char* str)
{
	const size_t len = strlen(str);
	char*        dup = (char*)malloc(len + 1);
	memcpy(dup, str, len + 1);
	return dup;
}

static LV2_URID
urid_map(LV2_URID_Map_Handle handle, const char* uri)
{
	for (uint32_t i = 0; i < n_uris; ++i) {
		if (!strcmp(uris[i], uri)) {
			return i + 1;
		}
	}

	uris = (char**)realloc(uris, ++n_uris * sizeof(char*));
	uris[n_uris - 1] = copy_string(uri);
	return n_uris;
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

int
main(void)
{
	LV2_URID_Map   map = { NULL, urid_map };
	LV2_Atom_Forge forge;
	lv2_atom_forge_init(&forge, &map);

	LV2_URID eg_Object  = urid_map(NULL, "http://example.org/Object");
	LV2_URID eg_one     = urid_map(NULL, "http://example.org/one");
	LV2_URID eg_two     = urid_map(NULL, "http://example.org/two");
	LV2_URID eg_three   = urid_map(NULL, "http://example.org/three");
	LV2_URID eg_four    = urid_map(NULL, "http://example.org/four");
	LV2_URID eg_true    = urid_map(NULL, "http://example.org/true");
	LV2_URID eg_false   = urid_map(NULL, "http://example.org/false");
	LV2_URID eg_path    = urid_map(NULL, "http://example.org/path");
	LV2_URID eg_uri     = urid_map(NULL, "http://example.org/uri");
	LV2_URID eg_urid    = urid_map(NULL, "http://example.org/urid");
	LV2_URID eg_string  = urid_map(NULL, "http://example.org/string");
	LV2_URID eg_literal = urid_map(NULL, "http://example.org/literal");
	LV2_URID eg_tuple   = urid_map(NULL, "http://example.org/tuple");
	LV2_URID eg_vector  = urid_map(NULL, "http://example.org/vector");
	LV2_URID eg_vector2 = urid_map(NULL, "http://example.org/vector2");
	LV2_URID eg_seq     = urid_map(NULL, "http://example.org/seq");

#define BUF_SIZE  1024
#define NUM_PROPS 15

	uint8_t buf[BUF_SIZE];
	lv2_atom_forge_set_buffer(&forge, buf, BUF_SIZE);

	LV2_Atom_Forge_Frame obj_frame;
	LV2_Atom* obj = lv2_atom_forge_deref(
		&forge, lv2_atom_forge_object(&forge, &obj_frame, 0, eg_Object));

	// eg_one = (Int)1
	lv2_atom_forge_key(&forge, eg_one);
	LV2_Atom_Int* one = (LV2_Atom_Int*)lv2_atom_forge_deref(
		&forge, lv2_atom_forge_int(&forge, 1));
	if (one->body != 1) {
		return test_fail("%d != 1\n", one->body);
	}

	// eg_two = (Long)2
	lv2_atom_forge_key(&forge, eg_two);
	LV2_Atom_Long* two = (LV2_Atom_Long*)lv2_atom_forge_deref(
		&forge, lv2_atom_forge_long(&forge, 2));
	if (two->body != 2) {
		return test_fail("%ld != 2\n", two->body);
	}

	// eg_three = (Float)3.0
	lv2_atom_forge_key(&forge, eg_three);
	LV2_Atom_Float* three = (LV2_Atom_Float*)lv2_atom_forge_deref(
		&forge, lv2_atom_forge_float(&forge, 3.0f));
	if (three->body != 3) {
		return test_fail("%f != 3\n", three->body);
	}

	// eg_four = (Double)4.0
	lv2_atom_forge_key(&forge, eg_four);
	LV2_Atom_Double* four = (LV2_Atom_Double*)lv2_atom_forge_deref(
		&forge, lv2_atom_forge_double(&forge, 4.0));
	if (four->body != 4) {
		return test_fail("%ld != 4\n", four->body);
	}

	// eg_true = (Bool)1
	lv2_atom_forge_key(&forge, eg_true);
	LV2_Atom_Bool* t = (LV2_Atom_Bool*)lv2_atom_forge_deref(
		&forge, lv2_atom_forge_bool(&forge, true));
	if (t->body != 1) {
		return test_fail("%ld != 1 (true)\n", t->body);
	}

	// eg_false = (Bool)0
	lv2_atom_forge_key(&forge, eg_false);
	LV2_Atom_Bool* f = (LV2_Atom_Bool*)lv2_atom_forge_deref(
		&forge, lv2_atom_forge_bool(&forge, false));
	if (f->body != 0) {
		return test_fail("%ld != 0 (false)\n", f->body);
	}

	// eg_path = (Path)"/foo/bar"
	const char*    pstr     = "/foo/bar";
	const uint32_t pstr_len = (uint32_t)strlen(pstr);
	lv2_atom_forge_key(&forge, eg_path);
	LV2_Atom_String* path  = (LV2_Atom_String*)lv2_atom_forge_deref(
		&forge, lv2_atom_forge_uri(&forge, pstr, pstr_len));
	char* pbody = (char*)LV2_ATOM_BODY(path);
	if (strcmp(pbody, pstr)) {
		return test_fail("%s != \"%s\"\n", pbody, pstr);
	}

	// eg_uri = (URI)"http://example.org/value"
	const char*    ustr     = "http://example.org/value";
	const uint32_t ustr_len = (uint32_t)strlen(ustr);
	lv2_atom_forge_key(&forge, eg_uri);
	LV2_Atom_String* uri = (LV2_Atom_String*)lv2_atom_forge_deref(
		&forge, lv2_atom_forge_uri(&forge, ustr, ustr_len));
	char* ubody = (char*)LV2_ATOM_BODY(uri);
	if (strcmp(ubody, ustr)) {
		return test_fail("%s != \"%s\"\n", ubody, ustr);
	}

	// eg_urid = (URID)"http://example.org/value"
	LV2_URID eg_value = urid_map(NULL, "http://example.org/value");
	lv2_atom_forge_key(&forge, eg_urid);
	LV2_Atom_URID* urid = (LV2_Atom_URID*)lv2_atom_forge_deref(
		&forge, lv2_atom_forge_urid(&forge, eg_value));
	if (urid->body != eg_value) {
		return test_fail("%u != %u\n", urid->body, eg_value);
	}

	// eg_string = (String)"hello"
	lv2_atom_forge_key(&forge, eg_string);
	LV2_Atom_String* string = (LV2_Atom_String*)lv2_atom_forge_deref(
		&forge, lv2_atom_forge_string(
			&forge, "hello", strlen("hello")));
	char* sbody = (char*)LV2_ATOM_BODY(string);
	if (strcmp(sbody, "hello")) {
		return test_fail("%s != \"hello\"\n", sbody);
	}

	// eg_literal = (Literal)"hello"@fr
	lv2_atom_forge_key(&forge, eg_literal);
	LV2_Atom_Literal* literal = (LV2_Atom_Literal*)lv2_atom_forge_deref(
		&forge, lv2_atom_forge_literal(
			&forge, "bonjour", strlen("bonjour"),
			0, urid_map(NULL, "http://lexvo.org/id/term/fr")));
	char* lbody = (char*)LV2_ATOM_CONTENTS(LV2_Atom_Literal, literal);
	if (strcmp(lbody, "bonjour")) {
		return test_fail("%s != \"bonjour\"\n", lbody);
	}

	// eg_tuple = "foo",true
	lv2_atom_forge_key(&forge, eg_tuple);
	LV2_Atom_Forge_Frame tuple_frame;
	LV2_Atom_Tuple*      tuple = (LV2_Atom_Tuple*)lv2_atom_forge_deref(
		&forge, lv2_atom_forge_tuple(&forge, &tuple_frame));
	LV2_Atom_String* tup0 = (LV2_Atom_String*)lv2_atom_forge_deref(
		&forge, lv2_atom_forge_string(
			&forge, "foo", strlen("foo")));
	LV2_Atom_Bool* tup1 = (LV2_Atom_Bool*)lv2_atom_forge_deref(
		&forge, lv2_atom_forge_bool(&forge, true));
	lv2_atom_forge_pop(&forge, &tuple_frame);
	LV2_Atom* i = lv2_atom_tuple_begin(tuple);
	if (lv2_atom_tuple_is_end(LV2_ATOM_BODY(tuple), tuple->atom.size, i)) {
		return test_fail("Tuple iterator is empty\n");
	}
	LV2_Atom* tup0i = i;
	if (!lv2_atom_equals((LV2_Atom*)tup0, tup0i)) {
		return test_fail("Corrupt tuple element 0\n");
	}
	i = lv2_atom_tuple_next(i);
	if (lv2_atom_tuple_is_end(LV2_ATOM_BODY(tuple), tuple->atom.size, i)) {
		return test_fail("Premature end of tuple iterator\n");
	}
	LV2_Atom* tup1i = i;
	if (!lv2_atom_equals((LV2_Atom*)tup1, tup1i)) {
		return test_fail("Corrupt tuple element 1\n");
	}
	i = lv2_atom_tuple_next(i);
	if (!lv2_atom_tuple_is_end(LV2_ATOM_BODY(tuple), tuple->atom.size, i)) {
		return test_fail("Tuple iter is not at end\n");
	}

	// eg_vector = (Vector<Int>)1,2,3,4
	lv2_atom_forge_key(&forge, eg_vector);
	int32_t elems[] = { 1, 2, 3, 4 };
	LV2_Atom_Vector* vector = (LV2_Atom_Vector*)lv2_atom_forge_deref(
		&forge, lv2_atom_forge_vector(
			&forge, sizeof(int32_t), forge.Int, 4, elems));
	void* vec_body = LV2_ATOM_CONTENTS(LV2_Atom_Vector, vector);
	if (memcmp(elems, vec_body, sizeof(elems))) {
		return test_fail("Corrupt vector\n");
	}

	// eg_vector2 = (Vector<Int>)1,2,3,4
	lv2_atom_forge_key(&forge, eg_vector2);
	LV2_Atom_Forge_Frame vec_frame;
	LV2_Atom_Vector* vector2 = (LV2_Atom_Vector*)lv2_atom_forge_deref(
		&forge, lv2_atom_forge_vector_head(
			&forge, &vec_frame, sizeof(int32_t), forge.Int));
	for (unsigned e = 0; e < sizeof(elems) / sizeof(int32_t); ++e) {
		lv2_atom_forge_int(&forge, elems[e]);
	}
	lv2_atom_forge_pop(&forge, &vec_frame);
	if (!lv2_atom_equals(&vector->atom, &vector2->atom)) {
		return test_fail("Vector != Vector2\n");
	}

	// eg_seq = (Sequence)1, 2
	lv2_atom_forge_key(&forge, eg_seq);
	LV2_Atom_Forge_Frame seq_frame;
	LV2_Atom_Sequence* seq = (LV2_Atom_Sequence*)lv2_atom_forge_deref(
		&forge, lv2_atom_forge_sequence_head(&forge, &seq_frame, 0));
	lv2_atom_forge_frame_time(&forge, 0);
	lv2_atom_forge_int(&forge, 1);
	lv2_atom_forge_frame_time(&forge, 1);
	lv2_atom_forge_int(&forge, 2);
	lv2_atom_forge_pop(&forge, &seq_frame);

	lv2_atom_forge_pop(&forge, &obj_frame);

	// Test equality
	LV2_Atom_Int itwo = { { forge.Int, sizeof(int32_t) }, 2 };
	if (lv2_atom_equals((LV2_Atom*)one, (LV2_Atom*)two)) {
		return test_fail("1 == 2.0\n");
	} else if (lv2_atom_equals((LV2_Atom*)one, (LV2_Atom*)&itwo)) {
		return test_fail("1 == 2\n");
	} else if (!lv2_atom_equals((LV2_Atom*)one, (LV2_Atom*)one)) {
		return test_fail("1 != 1\n");
	}

	unsigned n_events = 0;
	LV2_ATOM_SEQUENCE_FOREACH(seq, ev) {
		if (ev->time.frames != n_events) {
			return test_fail("Corrupt event %u has bad time\n", n_events);
		} else if (ev->body.type != forge.Int) {
			return test_fail("Corrupt event %u has bad type\n", n_events);
		} else if (((LV2_Atom_Int*)&ev->body)->body != (int)n_events + 1) {
			return test_fail("Event %u != %d\n", n_events, n_events + 1);
		}
		++n_events;
	}

	int n_props = 0;
	LV2_ATOM_OBJECT_FOREACH((LV2_Atom_Object*)obj, prop) {
		if (!prop->key) {
			return test_fail("Corrupt property %u has no key\n", n_props);
		} else if (prop->context) {
			return test_fail("Corrupt property %u has context\n", n_props);
		}
		++n_props;
	}

	if (n_props != NUM_PROPS) {
		return test_fail("Corrupt object has %u properties != %u\n",
		                 n_props, NUM_PROPS);
	}

	struct {
		const LV2_Atom* one;
		const LV2_Atom* two;
		const LV2_Atom* three;
		const LV2_Atom* four;
		const LV2_Atom* affirmative;
		const LV2_Atom* negative;
		const LV2_Atom* path;
		const LV2_Atom* uri;
		const LV2_Atom* urid;
		const LV2_Atom* string;
		const LV2_Atom* literal;
		const LV2_Atom* tuple;
		const LV2_Atom* vector;
		const LV2_Atom* vector2;
		const LV2_Atom* seq;
	} matches;

	memset(&matches, 0, sizeof(matches));

	LV2_Atom_Object_Query q[] = {
		{ eg_one,     &matches.one },
		{ eg_two,     &matches.two },
		{ eg_three,   &matches.three },
		{ eg_four,    &matches.four },
		{ eg_true,    &matches.affirmative },
		{ eg_false,   &matches.negative },
		{ eg_path,    &matches.path },
		{ eg_uri,     &matches.uri },
		{ eg_urid,    &matches.urid },
		{ eg_string,  &matches.string },
		{ eg_literal, &matches.literal },
		{ eg_tuple,   &matches.tuple },
		{ eg_vector,  &matches.vector },
		{ eg_vector2, &matches.vector2 },
		{ eg_seq,     &matches.seq },
		LV2_ATOM_OBJECT_QUERY_END
	};

	int n_matches = lv2_atom_object_query((LV2_Atom_Object*)obj, q);
	for (int n = 0; n < 2; ++n) {
		if (n_matches != n_props) {
			return test_fail("Query failed, %u matches != %u\n",
			                 n_matches, n_props);
		} else if (!lv2_atom_equals((LV2_Atom*)one, matches.one)) {
			return test_fail("Bad match one\n");
		} else if (!lv2_atom_equals((LV2_Atom*)two, matches.two)) {
			return test_fail("Bad match two\n");
		} else if (!lv2_atom_equals((LV2_Atom*)three, matches.three)) {
			return test_fail("Bad match three\n");
		} else if (!lv2_atom_equals((LV2_Atom*)four, matches.four)) {
			return test_fail("Bad match four\n");
		} else if (!lv2_atom_equals((LV2_Atom*)t, matches.affirmative)) {
			return test_fail("Bad match true\n");
		} else if (!lv2_atom_equals((LV2_Atom*)f, matches.negative)) {
			return test_fail("Bad match false\n");
		} else if (!lv2_atom_equals((LV2_Atom*)path, matches.path)) {
			return test_fail("Bad match path\n");
		} else if (!lv2_atom_equals((LV2_Atom*)uri, matches.uri)) {
			return test_fail("Bad match URI\n");
		} else if (!lv2_atom_equals((LV2_Atom*)string, matches.string)) {
			return test_fail("Bad match string\n");
		} else if (!lv2_atom_equals((LV2_Atom*)literal, matches.literal)) {
			return test_fail("Bad match literal\n");
		} else if (!lv2_atom_equals((LV2_Atom*)tuple, matches.tuple)) {
			return test_fail("Bad match tuple\n");
		} else if (!lv2_atom_equals((LV2_Atom*)vector, matches.vector)) {
			return test_fail("Bad match vector\n");
		} else if (!lv2_atom_equals((LV2_Atom*)vector, matches.vector2)) {
			return test_fail("Bad match vector2\n");
		} else if (!lv2_atom_equals((LV2_Atom*)seq, matches.seq)) {
			return test_fail("Bad match sequence\n");
		}
		memset(&matches, 0, sizeof(matches));
		n_matches = lv2_atom_object_get((LV2_Atom_Object*)obj,
		                                eg_one,     &matches.one,
		                                eg_two,     &matches.two,
		                                eg_three,   &matches.three,
		                                eg_four,    &matches.four,
		                                eg_true,    &matches.affirmative,
		                                eg_false,   &matches.negative,
		                                eg_path,    &matches.path,
		                                eg_uri,     &matches.uri,
		                                eg_urid,    &matches.urid,
		                                eg_string,  &matches.string,
		                                eg_literal, &matches.literal,
		                                eg_tuple,   &matches.tuple,
		                                eg_vector,  &matches.vector,
		                                eg_vector2, &matches.vector2,
		                                eg_seq,     &matches.seq,
		                                0);
	}

	return 0;
}
