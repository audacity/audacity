/*
  Copyright 2012-2014 David Robillard <http://drobilla.net>

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

#define __STDC_LIMIT_MACROS 1

#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lv2/lv2plug.in/ns/ext/atom/forge.h"
#include "lv2/lv2plug.in/ns/ext/atom/util.h"
#include "lv2/lv2plug.in/ns/ext/midi/midi.h"

#include "sratom/sratom.h"

#define NS_RDF (const uint8_t*)"http://www.w3.org/1999/02/22-rdf-syntax-ns#"
#define NS_XSD (const uint8_t*)"http://www.w3.org/2001/XMLSchema#"

#define USTR(str) ((const uint8_t*)(str))

typedef enum {
	MODE_SUBJECT,
	MODE_BODY,
	MODE_SEQUENCE,
} ReadMode;

struct SratomImpl {
	LV2_URID_Map*     map;
	LV2_Atom_Forge    forge;
	LV2_URID          atom_Event;
	LV2_URID          midi_MidiEvent;
	unsigned          next_id;
	SerdNode          base_uri;
	SerdStatementSink write_statement;
	SerdEndSink       end_anon;
	void*             handle;
	SratomObjectMode  object_mode;
	bool              pretty_numbers;
	struct {
		SordNode* atom_childType;
		SordNode* atom_frameTime;
		SordNode* rdf_first;
		SordNode* rdf_rest;
		SordNode* rdf_type;
		SordNode* rdf_value;
		SordNode* xsd_base64Binary;
	} nodes;
};

static void
read_node(Sratom*         sratom,
          LV2_Atom_Forge* forge,
          SordWorld*      world,
          SordModel*      model,
          const SordNode* node,
          ReadMode        mode);

SRATOM_API
Sratom*
sratom_new(LV2_URID_Map* map)
{
	Sratom* sratom = (Sratom*)malloc(sizeof(Sratom));
	sratom->map            = map;
	sratom->atom_Event     = map->map(map->handle, LV2_ATOM__Event);
	sratom->midi_MidiEvent = map->map(map->handle, LV2_MIDI__MidiEvent);
	sratom->next_id        = 0;
	sratom->base_uri       = SERD_NODE_NULL;
	sratom->object_mode    = SRATOM_OBJECT_MODE_BLANK;
	sratom->pretty_numbers = false;
	memset(&sratom->nodes, 0, sizeof(sratom->nodes));
	lv2_atom_forge_init(&sratom->forge, map);
	return sratom;
}

SRATOM_API
void
sratom_free(Sratom* sratom)
{
	serd_node_free(&sratom->base_uri);
	free(sratom);
}

SRATOM_API
void
sratom_set_sink(Sratom*           sratom,
                const char*       base_uri,
                SerdStatementSink write_statement,
                SerdEndSink       end_anon,
                void*             handle)
{
	if (base_uri) {
		serd_node_free(&sratom->base_uri);
		sratom->base_uri = serd_node_new_uri_from_string(
			USTR(base_uri), NULL, NULL);
	}
	sratom->write_statement = write_statement;
	sratom->end_anon        = end_anon;
	sratom->handle          = handle;
}

SRATOM_API
void
sratom_set_pretty_numbers(Sratom* sratom,
                          bool    pretty_numbers)
{
	sratom->pretty_numbers = pretty_numbers;
}

SRATOM_API
void
sratom_set_object_mode(Sratom*          sratom,
                       SratomObjectMode object_mode)
{
	sratom->object_mode = object_mode;
}

static void
gensym(SerdNode* out, char c, unsigned num)
{
	out->n_bytes = out->n_chars = snprintf(
		(char*)out->buf, 10, "%c%u", c, num);
}

static void
list_append(Sratom*           sratom,
            LV2_URID_Unmap*   unmap,
            unsigned*         flags,
            SerdNode*         s,
            SerdNode*         p,
            SerdNode*         node,
            uint32_t          size,
            uint32_t          type,
            const void*       body)
{
	// Generate a list node
	gensym(node, 'l', sratom->next_id);
	sratom->write_statement(sratom->handle, *flags, NULL,
	                        s, p, node, NULL, NULL);

	// _:node rdf:first value
	*flags = SERD_LIST_CONT;
	*p = serd_node_from_string(SERD_URI, NS_RDF "first");
	sratom_write(sratom, unmap, *flags, node, p, type, size, body);

	// Set subject to node and predicate to rdf:rest for next time
	gensym(node, 'l', ++sratom->next_id);
	*s = *node;
	*p = serd_node_from_string(SERD_URI, NS_RDF "rest");
}

static void
list_end(SerdStatementSink sink,
         void*             handle,
         unsigned*         flags,
         SerdNode*         s,
         SerdNode*         p)
{
	// _:node rdf:rest rdf:nil
	const SerdNode nil = serd_node_from_string(SERD_URI, NS_RDF "nil");
	sink(handle, *flags, NULL, s, p, &nil, NULL, NULL);
}

static void
start_object(Sratom*         sratom,
             uint32_t*       flags,
             const SerdNode* subject,
             const SerdNode* predicate,
             const SerdNode* node,
             const char*     type)
{
	if (subject && predicate) {
		sratom->write_statement(sratom->handle, *flags|SERD_ANON_O_BEGIN, NULL,
		                        subject, predicate, node, NULL, NULL);
		// Start abbreviating object properties
		*flags |= SERD_ANON_CONT;

		// Object is in a list, stop list abbreviating if necessary
		*flags &= ~SERD_LIST_CONT;
	}
	if (type) {
		SerdNode p = serd_node_from_string(SERD_URI, NS_RDF "type");
		SerdNode o = serd_node_from_string(SERD_URI, USTR(type));
		sratom->write_statement(sratom->handle, *flags, NULL,
		                        node, &p, &o, NULL, NULL);
	}
}

static bool
path_is_absolute(const char* path)
{
	return (path[0] == '/'
	        || (isalpha(path[0]) && path[1] == ':'
	            && (path[2] == '/' || path[2] == '\\')));
}

SRATOM_API
int
sratom_write(Sratom*         sratom,
             LV2_URID_Unmap* unmap,
             uint32_t        flags,
             const SerdNode* subject,
             const SerdNode* predicate,
             uint32_t        type_urid,
             uint32_t        size,
             const void*     body)
{
	const char* const type        = unmap->unmap(unmap->handle, type_urid);
	uint8_t           idbuf[12]   = "b0000000000";
	SerdNode          id          = serd_node_from_string(SERD_BLANK, idbuf);
	uint8_t           nodebuf[12] = "b0000000000";
	SerdNode          node        = serd_node_from_string(SERD_BLANK, nodebuf);
	SerdNode          object      = SERD_NODE_NULL;
	SerdNode          datatype    = SERD_NODE_NULL;
	SerdNode          language    = SERD_NODE_NULL;
	bool              new_node    = false;
	if (type_urid == 0 && size == 0) {
		object = serd_node_from_string(SERD_URI, USTR(NS_RDF "nil"));
	} else if (type_urid == sratom->forge.String) {
		object = serd_node_from_string(SERD_LITERAL, (const uint8_t*)body);
	} else if (type_urid == sratom->forge.Chunk) {
		datatype = serd_node_from_string(SERD_URI, NS_XSD "base64Binary");
		object   = serd_node_new_blob(body, size, true);
		new_node = true;
	} else if (type_urid == sratom->forge.Literal) {
		const LV2_Atom_Literal_Body* lit = (const LV2_Atom_Literal_Body*)body;
		const uint8_t*         str = USTR(lit + 1);
		object = serd_node_from_string(SERD_LITERAL, str);
		if (lit->datatype) {
			datatype = serd_node_from_string(
				SERD_URI, USTR(unmap->unmap(unmap->handle, lit->datatype)));
		} else if (lit->lang) {
			const char*  lang       = unmap->unmap(unmap->handle, lit->lang);
			const char*  prefix     = "http://lexvo.org/id/iso639-3/";
			const size_t prefix_len = strlen(prefix);
			if (lang && !strncmp(lang, prefix, prefix_len)) {
				language = serd_node_from_string(
					SERD_LITERAL, USTR(lang + prefix_len));
			} else {
				fprintf(stderr, "Unknown language URID %d\n", lit->lang);
			}
		}
	} else if (type_urid == sratom->forge.URID) {
		const uint32_t urid = *(const uint32_t*)body;
		const uint8_t* str  = USTR(unmap->unmap(unmap->handle, urid));
		object = serd_node_from_string(SERD_URI, str);
	} else if (type_urid == sratom->forge.Path) {
		const uint8_t* str = USTR(body);
		if (path_is_absolute((const char*)str)) {
			new_node = true;
			object   = serd_node_new_file_uri(str, NULL, NULL, true);
		} else {
			SerdURI base_uri = SERD_URI_NULL;
			if (!sratom->base_uri.buf ||
			    strncmp((const char*)sratom->base_uri.buf, "file://", 7)) {
				fprintf(stderr, "warning: Relative path but base is not a file URI.\n");
				fprintf(stderr, "warning: Writing ambiguous atom:Path literal.\n");
				object   = serd_node_from_string(SERD_LITERAL, str);
				datatype = serd_node_from_string(SERD_URI, USTR(LV2_ATOM__Path));
			} else {
				if (sratom->base_uri.buf) {
					serd_uri_parse(sratom->base_uri.buf, &base_uri);
				}
				new_node = true;
				SerdNode rel = serd_node_new_file_uri(str, NULL, NULL, true);
				object = serd_node_new_uri_from_node(&rel, &base_uri, NULL);
				serd_node_free(&rel);
			}
		}
	} else if (type_urid == sratom->forge.URI) {
		const uint8_t* str = USTR(body);
		object = serd_node_from_string(SERD_URI, str);
	} else if (type_urid == sratom->forge.Int) {
		new_node = true;
		object   = serd_node_new_integer(*(const int32_t*)body);
		datatype = serd_node_from_string(SERD_URI, (sratom->pretty_numbers)
		                                 ? NS_XSD "integer" : NS_XSD "int");
	} else if (type_urid == sratom->forge.Long) {
		new_node = true;
		object   = serd_node_new_integer(*(const int64_t*)body);
		datatype = serd_node_from_string(SERD_URI, (sratom->pretty_numbers)
		                                 ? NS_XSD "integer" : NS_XSD "long");
	} else if (type_urid == sratom->forge.Float) {
		new_node = true;
		object   = serd_node_new_decimal(*(const float*)body, 8);
		datatype = serd_node_from_string(SERD_URI, (sratom->pretty_numbers)
		                                 ? NS_XSD "decimal" : NS_XSD "float");
	} else if (type_urid == sratom->forge.Double) {
		new_node = true;
		object   = serd_node_new_decimal(*(const double*)body, 16);
		datatype = serd_node_from_string(SERD_URI, (sratom->pretty_numbers)
		                                 ? NS_XSD "decimal" : NS_XSD "double");
	} else if (type_urid == sratom->forge.Bool) {
		const int32_t val = *(const int32_t*)body;
		datatype = serd_node_from_string(SERD_URI, NS_XSD "boolean");
		object   = serd_node_from_string(SERD_LITERAL,
		                                 USTR(val ? "true" : "false"));
	} else if (type_urid == sratom->midi_MidiEvent) {
		new_node = true;
		datatype = serd_node_from_string(SERD_URI, USTR(LV2_MIDI__MidiEvent));
		uint8_t* str = (uint8_t*)calloc(size * 2 + 1, 1);
		for (uint32_t i = 0; i < size; ++i) {
			snprintf((char*)str + (2 * i), size * 2 + 1, "%02X",
			         (unsigned)(uint8_t)*((const uint8_t*)body + i));
		}
		object = serd_node_from_string(SERD_LITERAL, USTR(str));
	} else if (type_urid == sratom->atom_Event) {
		const LV2_Atom_Event* ev = (const LV2_Atom_Event*)body;
		gensym(&id, 'e', sratom->next_id++);
		start_object(sratom, &flags, subject, predicate, &id, NULL);
		// TODO: beat time
		SerdNode time = serd_node_new_integer(ev->time.frames);
		SerdNode p    = serd_node_from_string(SERD_URI,
		                                      USTR(LV2_ATOM__frameTime));
		datatype = serd_node_from_string(SERD_URI, NS_XSD "decimal");
		sratom->write_statement(sratom->handle, SERD_ANON_CONT, NULL,
		                        &id, &p, &time, &datatype, &language);
		serd_node_free(&time);

		p = serd_node_from_string(SERD_URI, NS_RDF "value");
		sratom_write(sratom, unmap, SERD_ANON_CONT, &id, &p,
		             ev->body.type, ev->body.size, LV2_ATOM_BODY(&ev->body));
		if (sratom->end_anon) {
			sratom->end_anon(sratom->handle, &id);
		}
	} else if (type_urid == sratom->forge.Tuple) {
		gensym(&id, 't', sratom->next_id++);
		start_object(sratom, &flags, subject, predicate, &id, type);
		SerdNode p = serd_node_from_string(SERD_URI, NS_RDF "value");
		flags |= SERD_LIST_O_BEGIN;
		LV2_ATOM_TUPLE_BODY_FOREACH(body, size, i) {
			list_append(sratom, unmap, &flags, &id, &p, &node,
			            i->size, i->type, LV2_ATOM_BODY(i));
		}
		list_end(sratom->write_statement, sratom->handle, &flags, &id, &p);
		if (sratom->end_anon) {
			sratom->end_anon(sratom->handle, &id);
		}
	} else if (type_urid == sratom->forge.Vector) {
		const LV2_Atom_Vector_Body* vec  = (const LV2_Atom_Vector_Body*)body;
		gensym(&id, 'v', sratom->next_id++);
		start_object(sratom, &flags, subject, predicate, &id, type);
		SerdNode p = serd_node_from_string(SERD_URI, (const uint8_t*)LV2_ATOM__childType);
		SerdNode child_type = serd_node_from_string(
			SERD_URI, (const uint8_t*)unmap->unmap(unmap->handle, vec->child_type));
		sratom->write_statement(sratom->handle, flags, NULL, &id, &p, &child_type, NULL, NULL);
		p = serd_node_from_string(SERD_URI, NS_RDF "value");
		flags |= SERD_LIST_O_BEGIN;
		for (const char* i = (const char*)(vec + 1);
		     i < (const char*)vec + size;
		     i += vec->child_size) {
			list_append(sratom, unmap, &flags, &id, &p, &node,
			            vec->child_size, vec->child_type, i);
		}
		list_end(sratom->write_statement, sratom->handle, &flags, &id, &p);
		if (sratom->end_anon) {
			sratom->end_anon(sratom->handle, &id);
		}
	} else if (lv2_atom_forge_is_object_type(&sratom->forge, type_urid)) {
		const LV2_Atom_Object_Body* obj   = (const LV2_Atom_Object_Body*)body;
		const char*                 otype = unmap->unmap(unmap->handle,
		                                                 obj->otype);

		if (lv2_atom_forge_is_blank(&sratom->forge, type_urid, obj)) {
			gensym(&id, 'b', sratom->next_id++);
			start_object(sratom, &flags, subject, predicate, &id, otype);
		} else {
			id = serd_node_from_string(
				SERD_URI, (const uint8_t*)unmap->unmap(unmap->handle, obj->id));
			flags = 0;
			start_object(sratom, &flags, NULL, NULL, &id, otype);
		}
		LV2_ATOM_OBJECT_BODY_FOREACH(obj, size, prop) {
			const char* const key  = unmap->unmap(unmap->handle, prop->key);
			SerdNode          pred = serd_node_from_string(SERD_URI, USTR(key));
			sratom_write(sratom, unmap, flags, &id, &pred,
			             prop->value.type, prop->value.size,
			             LV2_ATOM_BODY(&prop->value));
		}
		if (sratom->end_anon && (flags & SERD_ANON_CONT)) {
			sratom->end_anon(sratom->handle, &id);
		}
	} else if (type_urid == sratom->forge.Sequence) {
		const LV2_Atom_Sequence_Body* seq = (const LV2_Atom_Sequence_Body*)body;
		gensym(&id, 'v', sratom->next_id++);
		start_object(sratom, &flags, subject, predicate, &id, type);
		SerdNode p = serd_node_from_string(SERD_URI, NS_RDF "value");
		flags |= SERD_LIST_O_BEGIN;
		LV2_ATOM_SEQUENCE_BODY_FOREACH(seq, size, ev) {
			list_append(sratom, unmap, &flags, &id, &p, &node,
			            sizeof(LV2_Atom_Event) + ev->body.size,
			            sratom->atom_Event,
			            ev);
		}
		list_end(sratom->write_statement, sratom->handle, &flags, &id, &p);
		if (sratom->end_anon && subject && predicate) {
			sratom->end_anon(sratom->handle, &id);
		}
	} else {
		gensym(&id, 'b', sratom->next_id++);
		start_object(sratom, &flags, subject, predicate, &id, type);
		SerdNode p = serd_node_from_string(SERD_URI, NS_RDF "value");
		SerdNode o = serd_node_new_blob(body, size, true);
		datatype = serd_node_from_string(SERD_URI, NS_XSD "base64Binary");
		sratom->write_statement(sratom->handle, flags, NULL, &id, &p, &o, &datatype, NULL);
		if (sratom->end_anon && subject && predicate) {
			sratom->end_anon(sratom->handle, &id);
		}
		serd_node_free(&o);
	}

	if (object.buf) {
		SerdNode def_s = serd_node_from_string(SERD_BLANK, USTR("atom"));
		SerdNode def_p = serd_node_from_string(SERD_URI, USTR(NS_RDF "value"));
		if (!subject) {
			subject = &def_s;
		}
		if (!predicate) {
			predicate = &def_p;
		}
		sratom->write_statement(sratom->handle, flags, NULL,
		                        subject, predicate, &object, &datatype, &language);
	}

	if (new_node) {
		serd_node_free(&object);
	}

	return 0;
}

SRATOM_API
char*
sratom_to_turtle(Sratom*         sratom,
                 LV2_URID_Unmap* unmap,
                 const char*     base_uri,
                 const SerdNode* subject,
                 const SerdNode* predicate,
                 uint32_t        type,
                 uint32_t        size,
                 const void*     body)
{
	SerdURI   buri = SERD_URI_NULL;
	SerdNode  base = serd_node_new_uri_from_string(USTR(base_uri), NULL, &buri);
	SerdEnv*  env  = serd_env_new(&base);
	SerdChunk str  = { NULL, 0 };

	serd_env_set_prefix_from_strings(env, USTR("midi"),
	                                 USTR(LV2_MIDI_PREFIX));
	serd_env_set_prefix_from_strings(env, USTR("atom"),
	                                 USTR(LV2_ATOM_URI "#"));
	serd_env_set_prefix_from_strings(env, USTR("rdf"), NS_RDF);
	serd_env_set_prefix_from_strings(env, USTR("xsd"), NS_XSD);

	SerdWriter* writer = serd_writer_new(
		SERD_TURTLE,
		(SerdStyle)(SERD_STYLE_ABBREVIATED |
		            SERD_STYLE_RESOLVED |
		            SERD_STYLE_CURIED),
		env, &buri, serd_chunk_sink, &str);

	// Write @prefix directives
	serd_env_foreach(env,
	                 (SerdPrefixSink)serd_writer_set_prefix,
	                 writer);

	sratom_set_sink(sratom, base_uri,
	                (SerdStatementSink)serd_writer_write_statement,
	                (SerdEndSink)serd_writer_end_anon,
	                writer);
	sratom_write(sratom, unmap, SERD_EMPTY_S,
	             subject, predicate, type, size, body);
	serd_writer_finish(writer);

	serd_writer_free(writer);
	serd_env_free(env);
	serd_node_free(&base);
	return (char*)serd_chunk_sink_finish(&str);
}

static void
read_list_value(Sratom*         sratom,
                LV2_Atom_Forge* forge,
                SordWorld*      world,
                SordModel*      model,
                const SordNode* node,
                ReadMode        mode)
{
	SordNode* fst = sord_get(model, node, sratom->nodes.rdf_first, NULL, NULL);
	SordNode* rst = sord_get(model, node, sratom->nodes.rdf_rest, NULL, NULL);
	if (fst && rst) {
		read_node(sratom, forge, world, model, fst, mode);
		read_list_value(sratom, forge, world, model, rst, mode);
	}
	sord_node_free(world, rst);
	sord_node_free(world, fst);
}

static void
read_resource(Sratom*         sratom,
              LV2_Atom_Forge* forge,
              SordWorld*      world,
              SordModel*      model,
              const SordNode* node,
              LV2_URID        otype)
{
	LV2_URID_Map* map = sratom->map;
	SordQuad      q   = { node, NULL, NULL, NULL };
	SordIter*     i   = sord_find(model, q);
	SordQuad      match;
	for (; !sord_iter_end(i); sord_iter_next(i)) {
		sord_iter_get(i, match);
		const SordNode* p      = match[SORD_PREDICATE];
		const SordNode* o      = match[SORD_OBJECT];
		const char*     p_uri  = (const char*)sord_node_get_string(p);
		uint32_t        p_urid = map->map(map->handle, p_uri);
		if (!(sord_node_equals(p, sratom->nodes.rdf_type) &&
		      sord_node_get_type(o) == SORD_URI &&
		      map->map(map->handle, (const char*)sord_node_get_string(o)) == otype)) {
			lv2_atom_forge_key(forge, p_urid);
			read_node(sratom, forge, world, model, o, MODE_BODY);
		}
	}
	sord_iter_free(i);
}

static uint32_t
atom_size(Sratom* sratom, uint32_t type_urid)
{
	if (type_urid == sratom->forge.Int) {
		return sizeof(int32_t);
	} else if (type_urid == sratom->forge.Long) {
		return sizeof(int64_t);
	} else if (type_urid == sratom->forge.Float) {
		return sizeof(float);
	} else if (type_urid == sratom->forge.Double) {
		return sizeof(double);
	} else if (type_urid == sratom->forge.Bool) {
		return sizeof(int32_t);
	} else if (type_urid == sratom->forge.URID) {
		return sizeof(uint32_t);
	} else {
		return 0;
	}
}

static void
read_node(Sratom*         sratom,
          LV2_Atom_Forge* forge,
          SordWorld*      world,
          SordModel*      model,
          const SordNode* node,
          ReadMode        mode)
{
	LV2_URID_Map* map = sratom->map;
	size_t        len = 0;
	const char*   str = (const char*)sord_node_get_string_counted(node, &len);
	if (sord_node_get_type(node) == SORD_LITERAL) {
		char*       endptr;
		SordNode*   datatype = sord_node_get_datatype(node);
		const char* language = sord_node_get_language(node);
		if (datatype) {
			const char* type_uri = (const char*)sord_node_get_string(datatype);
			if (!strcmp(type_uri, (const char*)NS_XSD "int") ||
			    !strcmp(type_uri, (const char*)NS_XSD "integer")) {
				lv2_atom_forge_int(forge, strtol(str, &endptr, 10));
			} else if (!strcmp(type_uri, (const char*)NS_XSD "long")) {
				lv2_atom_forge_long(forge, strtol(str, &endptr, 10));
			} else if (!strcmp(type_uri, (const char*)NS_XSD "float") ||
			           !strcmp(type_uri, (const char*)NS_XSD "decimal")) {
				lv2_atom_forge_float(forge, serd_strtod(str, &endptr));
			} else if (!strcmp(type_uri, (const char*)NS_XSD "double")) {
				lv2_atom_forge_double(forge, serd_strtod(str, &endptr));
			} else if (!strcmp(type_uri, (const char*)NS_XSD "boolean")) {
				lv2_atom_forge_bool(forge, !strcmp(str, "true"));
			} else if (!strcmp(type_uri, (const char*)NS_XSD "base64Binary")) {
				size_t size = 0;
				void*  body = serd_base64_decode(USTR(str), len, &size);
				lv2_atom_forge_atom(forge, size, forge->Chunk);
				lv2_atom_forge_write(forge, body, size);
				free(body);
			} else if (!strcmp(type_uri, LV2_ATOM__Path)) {
				lv2_atom_forge_path(forge, str, len);
			} else if (!strcmp(type_uri, LV2_MIDI__MidiEvent)) {
				lv2_atom_forge_atom(forge, len / 2, sratom->midi_MidiEvent);
				for (const char* s = str; s < str + len; s += 2) {
					unsigned num;
					sscanf(s, "%2X", &num);
					assert(num < UINT8_MAX);
					const uint8_t c = num;
					lv2_atom_forge_raw(forge, &c, 1);
				}
				lv2_atom_forge_pad(forge, len / 2);
			} else {
				lv2_atom_forge_literal(
					forge, str, len,
					sratom->map->map(sratom->map->handle, type_uri),
					0);
			}
		} else if (language) {
			const char*  prefix   = "http://lexvo.org/id/iso639-3/";
			const size_t lang_len = strlen(prefix) + strlen(language);
			char*        lang_uri = (char*)calloc(lang_len + 1, 1);
			snprintf(lang_uri, lang_len + 1, "%s%s", prefix, language);
			lv2_atom_forge_literal(
				forge, str, len, 0,
				sratom->map->map(sratom->map->handle, lang_uri));
			free(lang_uri);
		} else {
			lv2_atom_forge_string(forge, str, len);
		}
	} else if (sord_node_get_type(node) == SORD_URI &&
	           !(sratom->object_mode == SRATOM_OBJECT_MODE_BLANK_SUBJECT
	             && mode == MODE_SUBJECT)) {
		if (!strcmp(str, (const char*)NS_RDF "nil")) {
			lv2_atom_forge_atom(forge, 0, 0);
		} else if (!strncmp(str, "file://", 7)) {
			uint8_t* path = serd_file_uri_parse((const uint8_t*)str, NULL);
			lv2_atom_forge_path(forge, (const char*)path, strlen((const char*)path));
			free(path);
		} else {
			lv2_atom_forge_urid(forge, map->map(map->handle, str));
		}
	} else {
		SordNode* type = sord_get(
			model, node, sratom->nodes.rdf_type, NULL, NULL);
		SordNode* value = sord_get(
			model, node, sratom->nodes.rdf_value, NULL, NULL);

		const uint8_t* type_uri  = NULL;
		uint32_t       type_urid = 0;
		if (type) {
			type_uri  = sord_node_get_string(type);
			type_urid = map->map(map->handle, (const char*)type_uri);
		}

		LV2_Atom_Forge_Frame frame = { 0, 0 };
		if (mode == MODE_SEQUENCE) {
			SordNode* frame_time = sord_get(
				model, node, sratom->nodes.atom_frameTime, NULL, NULL);
			const char* frame_time_str = frame_time
				? (const char*)sord_node_get_string(frame_time)
				: "";
			lv2_atom_forge_frame_time(forge, serd_strtod(frame_time_str, NULL));
			read_node(sratom, forge, world, model, value, MODE_BODY);
			sord_node_free(world, frame_time);
		} else if (type_urid == sratom->forge.Tuple) {
			lv2_atom_forge_tuple(forge, &frame);
			read_list_value(sratom, forge, world, model, value, MODE_BODY);
		} else if (type_urid == sratom->forge.Sequence) {
			lv2_atom_forge_sequence_head(forge, &frame, 0);
			read_list_value(sratom, forge, world, model, value, MODE_SEQUENCE);
		} else if (type_urid == sratom->forge.Vector) {
			SordNode* child_type_node = sord_get(
				model, node, sratom->nodes.atom_childType, NULL, NULL);
			uint32_t child_type = map->map(
				map->handle, (const char*)sord_node_get_string(child_type_node));
			uint32_t child_size = atom_size(sratom, child_type);
			if (child_size > 0) {
				lv2_atom_forge_vector_head(forge, &frame, child_size, child_type);
				read_list_value(sratom, forge, world, model, value, MODE_BODY);
			}
			sord_node_free(world, child_type_node);
		} else if (value && sord_node_equals(sord_node_get_datatype(value),
		                                     sratom->nodes.xsd_base64Binary)) {
			size_t         vlen = 0;
			const uint8_t* vstr = sord_node_get_string_counted(value, &vlen);
			size_t         size = 0;
			void*          body = serd_base64_decode(vstr, vlen, &size);
			lv2_atom_forge_atom(forge, size, type_urid);
			lv2_atom_forge_write(forge, body, size);
			free(body);
		} else if (sord_node_get_type(node) == SORD_URI) {
			lv2_atom_forge_object(
				forge, &frame, map->map(map->handle, str), type_urid);
			read_resource(sratom, forge, world, model, node, type_urid);
		} else {
			lv2_atom_forge_object(forge, &frame, 0, type_urid);
			read_resource(sratom, forge, world, model, node, type_urid);
		}
		if (frame.ref) {
			lv2_atom_forge_pop(forge, &frame);
		}
		sord_node_free(world, value);
		sord_node_free(world, type);
	}
}

SRATOM_API
void
sratom_read(Sratom*         sratom,
            LV2_Atom_Forge* forge,
            SordWorld*      world,
            SordModel*      model,
            const SordNode* node)
{
	sratom->nodes.atom_childType   = sord_new_uri(world, USTR(LV2_ATOM__childType));
	sratom->nodes.atom_frameTime   = sord_new_uri(world, USTR(LV2_ATOM__frameTime));
	sratom->nodes.rdf_first        = sord_new_uri(world, NS_RDF "first");
	sratom->nodes.rdf_rest         = sord_new_uri(world, NS_RDF "rest");
	sratom->nodes.rdf_type         = sord_new_uri(world, NS_RDF "type");
	sratom->nodes.rdf_value        = sord_new_uri(world, NS_RDF "value");
	sratom->nodes.xsd_base64Binary = sord_new_uri(world, NS_XSD "base64Binary");

	sratom->next_id = 1;
	read_node(sratom, forge, world, model, node, MODE_SUBJECT);

	sord_node_free(world, sratom->nodes.xsd_base64Binary);
	sord_node_free(world, sratom->nodes.rdf_value);
	sord_node_free(world, sratom->nodes.rdf_type);
	sord_node_free(world, sratom->nodes.rdf_rest);
	sord_node_free(world, sratom->nodes.rdf_first);
	sord_node_free(world, sratom->nodes.atom_frameTime);
	sord_node_free(world, sratom->nodes.atom_childType);
	memset(&sratom->nodes, 0, sizeof(sratom->nodes));
}

SRATOM_API
LV2_Atom_Forge_Ref
sratom_forge_sink(LV2_Atom_Forge_Sink_Handle handle,
                  const void*                buf,
                  uint32_t                   size)
{
	SerdChunk*               chunk = (SerdChunk*)handle;
	const LV2_Atom_Forge_Ref ref   = chunk->len + 1;
	serd_chunk_sink(buf, size, chunk);
	return ref;
}

SRATOM_API
LV2_Atom*
sratom_forge_deref(LV2_Atom_Forge_Sink_Handle handle, LV2_Atom_Forge_Ref ref)
{
	SerdChunk* chunk = (SerdChunk*)handle;
	return (LV2_Atom*)(chunk->buf + ref - 1);
}

SRATOM_API
LV2_Atom*
sratom_from_turtle(Sratom*         sratom,
                   const char*     base_uri,
                   const SerdNode* subject,
                   const SerdNode* predicate,
                   const char*     str)
{
	SerdChunk   out    = { NULL, 0 };
	SerdNode    base   = serd_node_new_uri_from_string(USTR(base_uri), NULL, NULL);
	SordWorld*  world  = sord_world_new();
	SordModel*  model  = sord_new(world, SORD_SPO, false);
	SerdEnv*    env    = serd_env_new(&base);
	SerdReader* reader = sord_new_reader(model, env, SERD_TURTLE, NULL);

	if (!serd_reader_read_string(reader, (const uint8_t*)str)) {
		SordNode* s = sord_node_from_serd_node(world, env, subject, 0, 0);
		lv2_atom_forge_set_sink(
			&sratom->forge, sratom_forge_sink, sratom_forge_deref, &out);
		if (subject && predicate) {
			SordNode* p = sord_node_from_serd_node(world, env, predicate, 0, 0);
			SordNode* o = sord_get(model, s, p, NULL, NULL);
			if (o) {
				sratom_read(sratom, &sratom->forge, world, model, o);
				sord_node_free(world, o);
			} else {
				fprintf(stderr, "Failed to find node\n");
			}
		} else {
			sratom_read(sratom, &sratom->forge, world, model, s);
		}
	} else {
		fprintf(stderr, "Failed to read Turtle\n");
	}

	serd_reader_free(reader);
	serd_env_free(env);
	sord_free(model);
	sord_world_free(world);
	serd_node_free(&base);

	return (LV2_Atom*)out.buf;
}
