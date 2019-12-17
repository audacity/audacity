/*
  Copyright 2012-2016 David Robillard <http://drobilla.net>

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

#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include "lv2/atom/forge.h"
#include "lv2/atom/util.h"
#include "lv2/midi/midi.h"
#include "sratom/sratom.h"

#define NS_RDF "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

#define USTR(s) ((const uint8_t*)(s))

/// Simple O(n) URI map
typedef struct {
	char** uris;
	size_t n_uris;
} Uris;

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
	Uris* const uris = (Uris*)handle;

	for (size_t i = 0; i < uris->n_uris; ++i) {
		if (!strcmp(uris->uris[i], uri)) {
			return i + 1;
		}
	}

	uris->uris = (char**)realloc(uris->uris, ++uris->n_uris * sizeof(char*));
	uris->uris[uris->n_uris - 1] = copy_string(uri);
	return uris->n_uris;
}

static const char*
urid_unmap(LV2_URID_Unmap_Handle handle,
           LV2_URID              urid)
{
	Uris* const uris = (Uris*)handle;

	if (urid > 0 && urid <= uris->n_uris) {
		return uris->uris[urid - 1];
	}
	return NULL;
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
test(SerdEnv* env, bool top_level, bool pretty_numbers)
{
	Uris           uris  = { NULL, 0 };
	LV2_URID_Map   map   = { &uris, urid_map };
	LV2_URID_Unmap unmap = { &uris, urid_unmap };
	LV2_Atom_Forge forge;
	lv2_atom_forge_init(&forge, &map);

	Sratom* sratom = sratom_new(&map);
	sratom_set_env(sratom, env);
	sratom_set_pretty_numbers(sratom, pretty_numbers);
	sratom_set_object_mode(
		sratom,
		top_level ? SRATOM_OBJECT_MODE_BLANK_SUBJECT : SRATOM_OBJECT_MODE_BLANK);

	LV2_URID eg_Object  = urid_map(&uris, "http://example.org/Object");
	LV2_URID eg_one     = urid_map(&uris, "http://example.org/a-one");
	LV2_URID eg_two     = urid_map(&uris, "http://example.org/b-two");
	LV2_URID eg_three   = urid_map(&uris, "http://example.org/c-three");
	LV2_URID eg_four    = urid_map(&uris, "http://example.org/d-four");
	LV2_URID eg_true    = urid_map(&uris, "http://example.org/e-true");
	LV2_URID eg_false   = urid_map(&uris, "http://example.org/f-false");
	LV2_URID eg_path    = urid_map(&uris, "http://example.org/g-path");
	LV2_URID eg_winpath = urid_map(&uris, "http://example.org/h-winpath");
	LV2_URID eg_relpath = urid_map(&uris, "http://example.org/i-relpath");
	LV2_URID eg_urid    = urid_map(&uris, "http://example.org/j-urid");
	LV2_URID eg_string  = urid_map(&uris, "http://example.org/k-string");
	LV2_URID eg_langlit = urid_map(&uris, "http://example.org/l-langlit");
	LV2_URID eg_typelit = urid_map(&uris, "http://example.org/m-typelit");
	LV2_URID eg_null    = urid_map(&uris, "http://example.org/n-null");
	LV2_URID eg_chunk   = urid_map(&uris, "http://example.org/o-chunk");
	LV2_URID eg_blob    = urid_map(&uris, "http://example.org/p-blob");
	LV2_URID eg_blank   = urid_map(&uris, "http://example.org/q-blank");
	LV2_URID eg_tuple   = urid_map(&uris, "http://example.org/r-tuple");
	LV2_URID eg_rectup  = urid_map(&uris, "http://example.org/s-rectup");
	LV2_URID eg_ivector = urid_map(&uris, "http://example.org/t-ivector");
	LV2_URID eg_lvector = urid_map(&uris, "http://example.org/u-lvector");
	LV2_URID eg_fvector = urid_map(&uris, "http://example.org/v-fvector");
	LV2_URID eg_dvector = urid_map(&uris, "http://example.org/w-dvector");
	LV2_URID eg_bvector = urid_map(&uris, "http://example.org/x-bvector");
	LV2_URID eg_fseq    = urid_map(&uris, "http://example.org/y-fseq");
	LV2_URID eg_bseq    = urid_map(&uris, "http://example.org/z-bseq");

	uint8_t buf[1024];
	lv2_atom_forge_set_buffer(&forge, buf, sizeof(buf));

	const char*          obj_uri = "http://example.org/obj";
	LV2_URID             obj_id  = urid_map(&uris, obj_uri);
	LV2_Atom_Forge_Frame obj_frame;
	if (top_level) {
		lv2_atom_forge_object(&forge, &obj_frame, obj_id, eg_Object);
	} else {
		lv2_atom_forge_object(&forge, &obj_frame, 0, eg_Object);
	}
	LV2_Atom* obj = lv2_atom_forge_deref(&forge, obj_frame.ref);

	// eg_one = (Int32)1
	lv2_atom_forge_key(&forge, eg_one);
	lv2_atom_forge_int(&forge, 1);

	// eg_two = (Int64)2
	lv2_atom_forge_key(&forge, eg_two);
	lv2_atom_forge_long(&forge, 2);

	// eg_three = (Float)3.0
	lv2_atom_forge_key(&forge, eg_three);
	lv2_atom_forge_float(&forge, 3.0f);

	// eg_four = (Double)4.0
	lv2_atom_forge_key(&forge, eg_four);
	lv2_atom_forge_double(&forge, 4.0);

	// eg_true = (Bool)1
	lv2_atom_forge_key(&forge, eg_true);
	lv2_atom_forge_bool(&forge, true);

	// eg_false = (Bool)0
	lv2_atom_forge_key(&forge, eg_false);
	lv2_atom_forge_bool(&forge, false);

	// eg_path = (Path)"/absolute/path"
	const char*  pstr     = "/absolute/path";
	const size_t pstr_len = strlen(pstr);
	lv2_atom_forge_key(&forge, eg_path);
	lv2_atom_forge_path(&forge, pstr, pstr_len);

	// eg_winpath = (Path)"C:\Stupid\File System"
	const char*  wpstr     = "C:/Stupid/File System";
	const size_t wpstr_len = strlen(wpstr);
	lv2_atom_forge_key(&forge, eg_winpath);
	lv2_atom_forge_path(&forge, wpstr, wpstr_len);

	// eg_relpath = (Path)"foo/bar"
	const char*  rpstr     = "foo/bar";
	const size_t rpstr_len = strlen(rpstr);
	lv2_atom_forge_key(&forge, eg_relpath);
	lv2_atom_forge_path(&forge, rpstr, rpstr_len);

	// eg_urid = (URID)"http://example.org/value"
	LV2_URID eg_value = urid_map(&uris, "http://example.org/value");
	lv2_atom_forge_key(&forge, eg_urid);
	lv2_atom_forge_urid(&forge, eg_value);

	// eg_string = (String)"hello"
	lv2_atom_forge_key(&forge, eg_string);
	lv2_atom_forge_string(&forge, "hello", strlen("hello"));

	// eg_langlit = (Literal)"ni hao"@cmn (but in actual mandarin)
	const uint8_t ni_hao[] = { 0xE4, 0xBD, 0xA0, 0xE5, 0xA5, 0xBD };
	lv2_atom_forge_key(&forge, eg_langlit);
	lv2_atom_forge_literal(
		&forge, (const char*)ni_hao, 6,
		0, urid_map(&uris, "http://lexvo.org/id/iso639-3/cmn"));

	// eg_typelit = (Literal)"value"^^<http://example.org/Type>
	lv2_atom_forge_key(&forge, eg_typelit);
	lv2_atom_forge_literal(
		&forge, "value", strlen("value"),
		urid_map(&uris, "http://example.org/Type"), 0);

	// eg_null = null
	lv2_atom_forge_key(&forge, eg_null);
	lv2_atom_forge_atom(&forge, 0, 0);

	// eg_chunk = 0xBEEFDEAD
	uint8_t chunk_buf[] = { 0xBE, 0xEF, 0xDE, 0xAD };
	lv2_atom_forge_key(&forge, eg_chunk);
	lv2_atom_forge_atom(&forge, sizeof(chunk_buf), forge.Chunk);
	lv2_atom_forge_write(&forge, chunk_buf, sizeof(chunk_buf));

	// eg_blob = 0xDEADBEEF
	uint32_t blob_type  = map.map(map.handle, "http://example.org/Blob");
	uint8_t  blob_buf[] = { 0xDE, 0xAD, 0xBE, 0xEF };
	lv2_atom_forge_key(&forge, eg_blob);
	lv2_atom_forge_atom(&forge, sizeof(blob_buf), blob_type);
	lv2_atom_forge_write(&forge, blob_buf, sizeof(blob_buf));

	// eg_blank = [ a eg:Object ; blank [ a eg:Object] ]
	lv2_atom_forge_key(&forge, eg_blank);
	LV2_Atom_Forge_Frame blank_frame;
	lv2_atom_forge_object(&forge, &blank_frame, 0, eg_Object);
	lv2_atom_forge_key(&forge, eg_blank);
	LV2_Atom_Forge_Frame sub_blank_frame;
	lv2_atom_forge_object(&forge, &sub_blank_frame, 0, eg_Object);
	lv2_atom_forge_pop(&forge, &sub_blank_frame);
	lv2_atom_forge_pop(&forge, &blank_frame);

	// eg_tuple = "foo",true
	lv2_atom_forge_key(&forge, eg_tuple);
	LV2_Atom_Forge_Frame tuple_frame;
	lv2_atom_forge_tuple(&forge, &tuple_frame);
	lv2_atom_forge_string(&forge, "foo", strlen("foo"));
	lv2_atom_forge_bool(&forge, true);
	lv2_atom_forge_pop(&forge, &tuple_frame);

	// eg_rectup = "foo",true,("bar",false)
	lv2_atom_forge_key(&forge, eg_rectup);
	LV2_Atom_Forge_Frame rectup_frame;
	lv2_atom_forge_tuple(&forge, &rectup_frame);
	lv2_atom_forge_string(&forge, "foo", strlen("foo"));
	lv2_atom_forge_bool(&forge, true);
	LV2_Atom_Forge_Frame subrectup_frame;
	lv2_atom_forge_tuple(&forge, &subrectup_frame);
	lv2_atom_forge_string(&forge, "bar", strlen("bar"));
	lv2_atom_forge_bool(&forge, false);
	lv2_atom_forge_pop(&forge, &subrectup_frame);
	lv2_atom_forge_pop(&forge, &rectup_frame);

	// eg_ivector = (Vector<Int>)1,2,3,4,5
	lv2_atom_forge_key(&forge, eg_ivector);
	int32_t ielems[] = { 1, 2, 3, 4, 5 };
	lv2_atom_forge_vector(&forge, sizeof(int32_t), forge.Int, 5, ielems);

	// eg_lvector = (Vector<Long>)1,2,3,4
	lv2_atom_forge_key(&forge, eg_lvector);
	int64_t lelems[] = { 1, 2, 3, 4 };
	lv2_atom_forge_vector(&forge, sizeof(int64_t), forge.Long, 4, lelems);

	// eg_fvector = (Vector<Float>)1.0,2.0,3.0,4.0,5.0
	lv2_atom_forge_key(&forge, eg_fvector);
	float felems[] = { 1, 2, 3, 4, 5 };
	lv2_atom_forge_vector(&forge, sizeof(float), forge.Float, 5, felems);

	// eg_dvector = (Vector<Double>)1.0,2.0,3.0,4.0
	lv2_atom_forge_key(&forge, eg_dvector);
	double delems[] = { 1, 2, 3, 4 };
	lv2_atom_forge_vector(&forge, sizeof(double), forge.Double, 4, delems);

	// eg_bvector = (Vector<Bool>)1,0,1
	lv2_atom_forge_key(&forge, eg_bvector);
	int32_t belems[] = { true, false , true };
	lv2_atom_forge_vector(&forge, sizeof(int32_t), forge.Bool, 3, belems);

	// eg_fseq = (Sequence)1, 2
	LV2_URID midi_midiEvent = map.map(map.handle, LV2_MIDI__MidiEvent);
	lv2_atom_forge_key(&forge, eg_fseq);
	LV2_Atom_Forge_Frame fseq_frame;
	lv2_atom_forge_sequence_head(&forge, &fseq_frame, 0);

	const uint8_t ev1[3] = { 0x90, 0x1A, 0x1 };
	lv2_atom_forge_frame_time(&forge, 1);
	lv2_atom_forge_atom(&forge, sizeof(ev1), midi_midiEvent);
	lv2_atom_forge_raw(&forge, ev1, sizeof(ev1));
	lv2_atom_forge_pad(&forge, sizeof(ev1));

	const uint8_t ev2[3] = { 0x90, 0x2B, 0x2 };
	lv2_atom_forge_frame_time(&forge, 3);
	lv2_atom_forge_atom(&forge, sizeof(ev2), midi_midiEvent);
	lv2_atom_forge_raw(&forge, ev2, sizeof(ev2));
	lv2_atom_forge_pad(&forge, sizeof(ev2));

	lv2_atom_forge_pop(&forge, &fseq_frame);

	// eg_bseq = (Sequence)1.1, 2.2
	LV2_URID atom_beatTime = map.map(map.handle, LV2_ATOM__beatTime);
	lv2_atom_forge_key(&forge, eg_bseq);
	LV2_Atom_Forge_Frame bseq_frame;
	lv2_atom_forge_sequence_head(&forge, &bseq_frame, atom_beatTime);

	lv2_atom_forge_beat_time(&forge, 1.0);
	lv2_atom_forge_atom(&forge, sizeof(ev1), midi_midiEvent);
	lv2_atom_forge_raw(&forge, ev1, sizeof(ev1));
	lv2_atom_forge_pad(&forge, sizeof(ev1));

	lv2_atom_forge_beat_time(&forge, 2.0);
	lv2_atom_forge_atom(&forge, sizeof(ev2), midi_midiEvent);
	lv2_atom_forge_raw(&forge, ev2, sizeof(ev2));
	lv2_atom_forge_pad(&forge, sizeof(ev2));

	lv2_atom_forge_pop(&forge, &bseq_frame);

	lv2_atom_forge_pop(&forge, &obj_frame);

	const char* base_uri = "file:///tmp/base/";

	SerdNode s = serd_node_from_string(SERD_URI, USTR("http://example.org/obj"));
	SerdNode p = serd_node_from_string(SERD_URI, USTR(NS_RDF "value"));

	SerdNode* subj = top_level ? NULL : &s;
	SerdNode* pred = top_level ? NULL : &p;

	char* outstr = sratom_to_turtle(
		sratom, &unmap, base_uri, subj, pred,
		obj->type, obj->size, LV2_ATOM_BODY(obj));

	printf("# Atom => Turtle\n\n%s", outstr);

	LV2_Atom* parsed = NULL;
	if (top_level) {
		SerdNode o = serd_node_from_string(SERD_URI, (const uint8_t*)obj_uri);
		parsed = sratom_from_turtle(sratom, base_uri, &o, NULL, outstr);
	} else {
		parsed = sratom_from_turtle(sratom, base_uri, subj, pred, outstr);
	}

	if (!pretty_numbers) {
		if (!lv2_atom_equals(obj, parsed)) {
			return test_fail("Parsed atom does not match original\n");
		}

		char* instr = sratom_to_turtle(
			sratom, &unmap, base_uri, subj, pred,
			parsed->type, parsed->size, LV2_ATOM_BODY(parsed));
		printf("# Turtle => Atom\n\n%s", instr);

		if (strcmp(outstr, instr)) {
			return test_fail("Re-serialised string differs from original\n");
		}
		free(instr);
	}

	printf("All tests passed.\n");

	free(parsed);
	free(outstr);
	sratom_free(sratom);
	for (uint32_t i = 0; i < uris.n_uris; ++i) {
		free(uris.uris[i]);
	}

	free(uris.uris);

	return 0;
}

static int
test_env(SerdEnv* env)
{
	if (test(env, false, false)) {
		return 1;
	} else if (test(env, true, false)) {
		return 1;
	} else if (test(env, false, true)) {
		return 1;
	} else if (test(env, true, true)) {
		return 1;
	}

	return 0;
}

int
main(void)
{
	// Test with no environment
	if (test_env(NULL)) {
		return 1;
	}

	// Test with a prefix defined
	SerdEnv* env = serd_env_new(NULL);
	serd_env_set_prefix_from_strings(
		env, (const uint8_t*)"eg", (const uint8_t*)"http://example.org/");
	test_env(env);
	serd_env_free(env);

	return 0;
}
