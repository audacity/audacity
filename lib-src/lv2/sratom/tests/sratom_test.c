/*
  Copyright 2012 David Robillard <http://drobilla.net>

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

#include "lv2/lv2plug.in/ns/ext/atom/forge.h"
#include "lv2/lv2plug.in/ns/ext/atom/util.h"
#include "lv2/lv2plug.in/ns/ext/midi/midi.h"
#include "sratom/sratom.h"

#define NS_RDF "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

#define USTR(s) ((const uint8_t*)(s))

static char** uris   = NULL;
static size_t n_uris = 0;

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
	for (size_t i = 0; i < n_uris; ++i) {
		if (!strcmp(uris[i], uri)) {
			return i + 1;
		}
	}

	uris = (char**)realloc(uris, ++n_uris * sizeof(char*));
	uris[n_uris - 1] = copy_string(uri);
	return n_uris;
}

static const char*
urid_unmap(LV2_URID_Unmap_Handle handle,
           LV2_URID              urid)
{
	if (urid > 0 && urid <= n_uris) {
		return uris[urid - 1];
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
test(bool top_level)
{
	LV2_URID_Map   map   = { NULL, urid_map };
	LV2_URID_Unmap unmap = { NULL, urid_unmap };
	LV2_Atom_Forge forge;
	lv2_atom_forge_init(&forge, &map);

	Sratom* sratom = sratom_new(&map);
	sratom_set_object_mode(
		sratom,
		top_level ? SRATOM_OBJECT_MODE_BLANK_SUBJECT : SRATOM_OBJECT_MODE_BLANK);

	LV2_URID eg_Object  = urid_map(NULL, "http://example.org/Object");
	LV2_URID eg_one     = urid_map(NULL, "http://example.org/a-one");
	LV2_URID eg_two     = urid_map(NULL, "http://example.org/b-two");
	LV2_URID eg_three   = urid_map(NULL, "http://example.org/c-three");
	LV2_URID eg_four    = urid_map(NULL, "http://example.org/d-four");
	LV2_URID eg_true    = urid_map(NULL, "http://example.org/e-true");
	LV2_URID eg_false   = urid_map(NULL, "http://example.org/f-false");
	LV2_URID eg_path    = urid_map(NULL, "http://example.org/g-path");
	LV2_URID eg_urid    = urid_map(NULL, "http://example.org/h-urid");
	LV2_URID eg_string  = urid_map(NULL, "http://example.org/i-string");
	LV2_URID eg_langlit = urid_map(NULL, "http://example.org/j-langlit");
	LV2_URID eg_typelit = urid_map(NULL, "http://example.org/k-typelit");
	LV2_URID eg_null    = urid_map(NULL, "http://example.org/l-null");
	LV2_URID eg_chunk   = urid_map(NULL, "http://example.org/m-chunk");
	LV2_URID eg_blob    = urid_map(NULL, "http://example.org/n-blob");
	LV2_URID eg_blank   = urid_map(NULL, "http://example.org/o-blank");
	LV2_URID eg_tuple   = urid_map(NULL, "http://example.org/p-tuple");
	LV2_URID eg_rectup  = urid_map(NULL, "http://example.org/q-rectup");
	LV2_URID eg_vector  = urid_map(NULL, "http://example.org/r-vector");
	LV2_URID eg_seq     = urid_map(NULL, "http://example.org/s-seq");

	uint8_t buf[1024];
	lv2_atom_forge_set_buffer(&forge, buf, sizeof(buf));

	const char*          obj_uri = "http://example.org/obj";
	LV2_URID             obj_id  = urid_map(NULL, obj_uri);
	LV2_Atom_Forge_Frame obj_frame;
	if (top_level) {
		lv2_atom_forge_resource(&forge, &obj_frame, obj_id, eg_Object);
	} else {
		lv2_atom_forge_blank(&forge, &obj_frame, 1, eg_Object);
	}
	LV2_Atom* obj = lv2_atom_forge_deref(&forge, obj_frame.ref);

	// eg_one = (Int32)1
	lv2_atom_forge_property_head(&forge, eg_one, 0);
	lv2_atom_forge_int(&forge, 1);

	// eg_two = (Int64)2
	lv2_atom_forge_property_head(&forge, eg_two, 0);
	lv2_atom_forge_long(&forge, 2);

	// eg_three = (Float)3.0
	lv2_atom_forge_property_head(&forge, eg_three, 0);
	lv2_atom_forge_float(&forge, 3.0f);

	// eg_four = (Double)4.0
	lv2_atom_forge_property_head(&forge, eg_four, 0);
	lv2_atom_forge_double(&forge, 4.0);

	// eg_true = (Bool)1
	lv2_atom_forge_property_head(&forge, eg_true, 0);
	lv2_atom_forge_bool(&forge, true);

	// eg_false = (Bool)0
	lv2_atom_forge_property_head(&forge, eg_false, 0);
	lv2_atom_forge_bool(&forge, false);

	// eg_path = (Path)"/absolute/path"
	const char*  pstr     = "/absolute/path";
	const size_t pstr_len = strlen(pstr);
	lv2_atom_forge_property_head(&forge, eg_path, 0);
	lv2_atom_forge_path(&forge, pstr, pstr_len);

	// eg_urid = (URID)"http://example.org/value"
	LV2_URID eg_value = urid_map(NULL, "http://example.org/value");
	lv2_atom_forge_property_head(&forge, eg_urid, 0);
	lv2_atom_forge_urid(&forge, eg_value);

	// eg_string = (String)"hello"
	lv2_atom_forge_property_head(&forge, eg_string, 0);
	lv2_atom_forge_string(&forge, "hello", strlen("hello"));

	// eg_langlit = (Literal)"bonjour"@fr
	lv2_atom_forge_property_head(&forge, eg_langlit, 0);
	lv2_atom_forge_literal(
		&forge, "bonjour", strlen("bonjour"),
		0, urid_map(NULL, "http://lexvo.org/id/iso639-3/fra"));

	// eg_typelit = (Literal)"bonjour"@fr
	lv2_atom_forge_property_head(&forge, eg_typelit, 0);
	lv2_atom_forge_literal(
		&forge, "value", strlen("value"),
		urid_map(NULL, "http://example.org/Type"), 0);

	// eg_null = null
	lv2_atom_forge_property_head(&forge, eg_null, 0);
	lv2_atom_forge_atom(&forge, 0, 0);

	// eg_chunk = 0xBEEFDEAD
	uint8_t chunk_buf[] = { 0xBE, 0xEF, 0xDE, 0xAD };
	lv2_atom_forge_property_head(&forge, eg_chunk, 0);
	lv2_atom_forge_atom(&forge, sizeof(chunk_buf), forge.Chunk);
	lv2_atom_forge_write(&forge, chunk_buf, sizeof(chunk_buf));

	// eg_blob = 0xDEADBEEF
	uint32_t blob_type  = map.map(map.handle, "http://example.org/Blob");
	uint8_t  blob_buf[] = { 0xDE, 0xAD, 0xBE, 0xEF };
	lv2_atom_forge_property_head(&forge, eg_blob, 0);
	lv2_atom_forge_atom(&forge, sizeof(blob_buf), blob_type);
	lv2_atom_forge_write(&forge, blob_buf, sizeof(blob_buf));

	// eg_blank = [ a <http://example.org/Object> ]
	lv2_atom_forge_property_head(&forge, eg_blank, 0);
	LV2_Atom_Forge_Frame blank_frame;
	lv2_atom_forge_blank(&forge, &blank_frame, top_level ? 1 : 2, eg_Object);
	lv2_atom_forge_pop(&forge, &blank_frame);

	// eg_tuple = "foo",true
	lv2_atom_forge_property_head(&forge, eg_tuple, 0);
	LV2_Atom_Forge_Frame tuple_frame;
	lv2_atom_forge_tuple(&forge, &tuple_frame);
	lv2_atom_forge_string(&forge, "foo", strlen("foo"));
	lv2_atom_forge_bool(&forge, true);
	lv2_atom_forge_pop(&forge, &tuple_frame);

	// eg_rectup = "foo",true,("bar",false)
	lv2_atom_forge_property_head(&forge, eg_rectup, 0);
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

	// eg_vector = (Vector<Int32>)1,2,3,4
	lv2_atom_forge_property_head(&forge, eg_vector, 0);
	int32_t elems[] = { 1, 2, 3, 4 };
	lv2_atom_forge_vector(&forge, 4, forge.Int, sizeof(int32_t), elems);

	// eg_seq = (Sequence)1, 2
	LV2_URID midi_midiEvent = map.map(map.handle, LV2_MIDI__MidiEvent);
	lv2_atom_forge_property_head(&forge, eg_seq, 0);
	LV2_Atom_Forge_Frame seq_frame;
	lv2_atom_forge_sequence_head(&forge, &seq_frame, 0);

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

	lv2_atom_forge_pop(&forge, &seq_frame);
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
		SerdNode o= serd_node_from_string(SERD_URI, (const uint8_t*)obj_uri);
		parsed = sratom_from_turtle(sratom, base_uri, &o, NULL, outstr);
	} else {
		parsed = sratom_from_turtle(sratom, base_uri, subj, pred, outstr);
	}
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

	printf("All tests passed.\n");

	free(parsed);
	free(instr);
	free(outstr);
	sratom_free(sratom);
	for (uint32_t i = 0; i < n_uris; ++i) {
		free(uris[i]);
	}

	free(uris);
	uris   = NULL;
	n_uris = 0;

	return 0;
}

int
main(void)
{
	if (test(false)) {
		return 1;
	} else if (test(true)) {
		return 1;
	}
	return 0;
}
