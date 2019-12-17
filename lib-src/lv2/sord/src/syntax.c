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

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "serd/serd.h"

#include "sord_config.h"
#include "sord_internal.h"

struct SordInserterImpl {
	SordModel* model;
	SerdEnv*   env;
};

SordInserter*
sord_inserter_new(SordModel* model,
                  SerdEnv*   env)
{
	SordInserter* inserter = (SordInserter*)malloc(sizeof(SordInserter));
	inserter->model = model;
	inserter->env   = env;
	return inserter;
}

void
sord_inserter_free(SordInserter* inserter)
{
	free(inserter);
}

SerdStatus
sord_inserter_set_base_uri(SordInserter*   inserter,
                           const SerdNode* uri)
{
	return serd_env_set_base_uri(inserter->env, uri);
}

SerdStatus
sord_inserter_set_prefix(SordInserter*   inserter,
                         const SerdNode* name,
                         const SerdNode* uri)
{
	return serd_env_set_prefix(inserter->env, name, uri);
}

SerdStatus
sord_inserter_write_statement(SordInserter*      inserter,
                              SerdStatementFlags flags,
                              const SerdNode*    graph,
                              const SerdNode*    subject,
                              const SerdNode*    predicate,
                              const SerdNode*    object,
                              const SerdNode*    object_datatype,
                              const SerdNode*    object_lang)
{
	SordWorld* world = sord_get_world(inserter->model);
	SerdEnv*   env   = inserter->env;

	SordNode* g = sord_node_from_serd_node(world, env, graph, NULL, NULL);
	SordNode* s = sord_node_from_serd_node(world, env, subject, NULL, NULL);
	SordNode* p = sord_node_from_serd_node(world, env, predicate, NULL, NULL);
	SordNode* o = sord_node_from_serd_node(world, env, object,
	                                       object_datatype, object_lang);

	if (!s || !p || !o) {
		return SERD_ERR_BAD_ARG;
	}

	const SordQuad tup = { s, p, o, g };
	sord_add(inserter->model, tup);

	sord_node_free(world, o);
	sord_node_free(world, p);
	sord_node_free(world, s);
	sord_node_free(world, g);

	return SERD_SUCCESS;
}

SORD_API
SerdReader*
sord_new_reader(SordModel* model,
                SerdEnv*   env,
                SerdSyntax syntax,
                SordNode*  graph)
{
	SordInserter* inserter = sord_inserter_new(model, env);

	SerdReader* reader = serd_reader_new(
		syntax, inserter, (void (*)(void* ptr))sord_inserter_free,
		(SerdBaseSink)sord_inserter_set_base_uri,
		(SerdPrefixSink)sord_inserter_set_prefix,
		(SerdStatementSink)sord_inserter_write_statement,
		NULL);

	if (graph) {
		serd_reader_set_default_graph(reader, sord_node_to_serd_node(graph));
	}

	return reader;
}

static SerdStatus
write_statement(SordModel*         sord,
                SerdWriter*        writer,
                SordQuad           tup,
                SerdStatementFlags flags)
{
	const SordNode* s  = tup[SORD_SUBJECT];
	const SordNode* p  = tup[SORD_PREDICATE];
	const SordNode* o  = tup[SORD_OBJECT];
	const SordNode* d  = sord_node_get_datatype(o);
	const SerdNode* ss = sord_node_to_serd_node(s);
	const SerdNode* sp = sord_node_to_serd_node(p);
	const SerdNode* so = sord_node_to_serd_node(o);
	const SerdNode* sd = sord_node_to_serd_node(d);

	const char* lang_str = sord_node_get_language(o);
	size_t      lang_len = lang_str ? strlen(lang_str) : 0;
	SerdNode    language = SERD_NODE_NULL;
	if (lang_str) {
		language.type    = SERD_LITERAL;
		language.n_bytes = lang_len;
		language.n_chars = lang_len;
		language.buf     = (const uint8_t*)lang_str;
	};

	// TODO: Subject abbreviation

	if (sord_node_is_inline_object(s) && !(flags & SERD_ANON_CONT)) {
		return SERD_SUCCESS;
	}

	SerdStatus st = SERD_SUCCESS;
	if (sord_node_is_inline_object(o)) {
		SordQuad  sub_pat  = { o, 0, 0, 0 };
		SordIter* sub_iter = sord_find(sord, sub_pat);

		SerdStatementFlags start_flags = flags
			| ((sub_iter) ? SERD_ANON_O_BEGIN : SERD_EMPTY_O);

		st = serd_writer_write_statement(
			writer, start_flags, NULL, ss, sp, so, sd, &language);

		if (!st && sub_iter) {
			flags |= SERD_ANON_CONT;
			for (; !st && !sord_iter_end(sub_iter); sord_iter_next(sub_iter)) {
				SordQuad sub_tup;
				sord_iter_get(sub_iter, sub_tup);
				st = write_statement(sord, writer, sub_tup, flags);
			}
			sord_iter_free(sub_iter);
			serd_writer_end_anon(writer, so);
		}
	} else {
		st = serd_writer_write_statement(
			writer, flags, NULL, ss, sp, so, sd, &language);
	}

	return st;
}

bool
sord_write(SordModel*  model,
           SerdWriter* writer,
           SordNode*   graph)
{
	SordQuad  pat  = { 0, 0, 0, graph };
	SordIter* iter = sord_find(model, pat);
	return sord_write_iter(iter, writer);
}

bool
sord_write_iter(SordIter*   iter,
                SerdWriter* writer)
{
	if (!iter) {
		return false;
	}

	SordModel* model = (SordModel*)sord_iter_get_model(iter);
	SerdStatus st    = SERD_SUCCESS;
	for (; !st && !sord_iter_end(iter); sord_iter_next(iter)) {
		SordQuad tup;
		sord_iter_get(iter, tup);
		st = write_statement(model, writer, tup, 0);
	}
	sord_iter_free(iter);

	return !st;
}
