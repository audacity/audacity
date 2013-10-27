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

#include "serd_internal.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NS_RDF "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
#define NS_XSD "http://www.w3.org/2001/XMLSchema#"

typedef struct {
	SerdNode graph;
	SerdNode subject;
	SerdNode predicate;
} WriteContext;

static const WriteContext WRITE_CONTEXT_NULL = {
	{ 0, 0, 0, 0, SERD_NOTHING },
	{ 0, 0, 0, 0, SERD_NOTHING },
	{ 0, 0, 0, 0, SERD_NOTHING }
};

typedef enum {
	SEP_NONE,
	SEP_END_S,       ///< End of a subject ('.')
	SEP_END_P,       ///< End of a predicate (';')
	SEP_END_O,       ///< End of an object (',')
	SEP_S_P,         ///< Between a subject and predicate (whitespace)
	SEP_P_O,         ///< Between a predicate and object (whitespace)
	SEP_ANON_BEGIN,  ///< Start of anonymous node ('[')
	SEP_ANON_END,    ///< End of anonymous node (']')
	SEP_LIST_BEGIN,  ///< Start of list ('(')
	SEP_LIST_SEP,    ///< List separator (whitespace)
	SEP_LIST_END     ///< End of list (')')
} Sep;

typedef struct {
	const char* str;               ///< Sep string
	uint8_t     len;               ///< Length of sep string
	uint8_t     space_before;      ///< Newline before sep
	uint8_t     space_after_node;  ///< Newline after sep if after node
	uint8_t     space_after_sep;   ///< Newline after sep if after sep
} SepRule;

static const SepRule rules[] = {
	{ NULL,     0, 0, 0, 0 },
	{ " .\n\n", 4, 0, 0, 0 },
	{ " ;",     2, 0, 1, 1 },
	{ " ,",     2, 0, 1, 0 },
	{ NULL,     0, 0, 1, 0 },
	{ " ",      1, 0, 0, 0 },
	{ "[",      1, 0, 1, 1 },
	{ "]",      1, 1, 0, 0 },
	{ "(",      1, 0, 0, 0 },
	{ NULL,     1, 0, 1, 0 },
	{ ")",      1, 1, 0, 0 },
	{ "\n",     1, 0, 1, 0 }
};

struct SerdWriterImpl {
	SerdSyntax    syntax;
	SerdStyle     style;
	SerdEnv*      env;
	SerdNode      root_node;
	SerdURI       root_uri;
	SerdURI       base_uri;
	SerdStack     anon_stack;
	SerdBulkSink  bulk_sink;
	SerdSink      sink;
	void*         stream;
	SerdErrorSink error_sink;
	void*         error_handle;
	WriteContext  context;
	SerdNode      list_subj;
	unsigned      list_depth;
	uint8_t*      bprefix;
	size_t        bprefix_len;
	unsigned      indent;
	Sep           last_sep;
	bool          empty;
};

typedef enum {
	WRITE_URI,
	WRITE_STRING,
	WRITE_LONG_STRING
} TextContext;

static void
w_err(SerdWriter* writer, SerdStatus st, const char* fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	const SerdError e = { st, NULL, 0, 0, fmt, &args };
	serd_error(writer->error_sink, writer->error_handle, &e);
	va_end(args);
}

static inline WriteContext*
anon_stack_top(SerdWriter* writer)
{
	assert(!serd_stack_is_empty(&writer->anon_stack));
	return (WriteContext*)(writer->anon_stack.buf
	                       + writer->anon_stack.size - sizeof(WriteContext));
}

static void
copy_node(SerdNode* dst, const SerdNode* src)
{
	if (src) {
		dst->buf = (uint8_t*)realloc((char*)dst->buf, src->n_bytes + 1);
		dst->n_bytes = src->n_bytes;
		dst->n_chars = src->n_chars;
		dst->flags   = src->flags;
		dst->type    = src->type;
		memcpy((char*)dst->buf, src->buf, src->n_bytes + 1);
	} else {
		dst->type = SERD_NOTHING;
	}
}

static inline size_t
sink(const void* buf, size_t len, SerdWriter* writer)
{
	if (writer->style & SERD_STYLE_BULK) {
		return serd_bulk_sink_write(buf, len, &writer->bulk_sink);
	} else {
		return writer->sink(buf, len, writer->stream);
	}
}

static size_t
write_text(SerdWriter* writer, TextContext ctx,
           const uint8_t* utf8, size_t n_bytes)
{
	size_t len        = 0;
	char   escape[11] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	for (size_t i = 0; i < n_bytes;) {
		// Fast bulk write for long strings of printable ASCII
		size_t j = i;
		for (; j < n_bytes; ++j) {
			if (utf8[j] == '>' || utf8[j] == '\\' || utf8[j] == '"'
			    || (!in_range(utf8[j], 0x20, 0x7E))) {
				break;
			}
		}

		if (j > i) {
			len += sink(&utf8[i], j - i, writer);
			i = j;
			continue;
		}

		uint8_t in = utf8[i++];
		if (ctx == WRITE_LONG_STRING) {
			if (in == '\\') {
				len += sink("\\\\", 2, writer); continue;
			} else if (in == '\"' && i == n_bytes) {
				len += sink("\\\"", 2, writer); continue;  // '"' at string end
			}
		} else {
			switch (in) {
			case '\\': len += sink("\\\\", 2, writer); continue;
			case '\n': len += sink("\\n", 2, writer);  continue;
			case '\r': len += sink("\\r", 2, writer);  continue;
			case '\t': len += sink("\\t", 2, writer);  continue;
			case '"':
				if (ctx == WRITE_STRING) {
					len += sink("\\\"", 2, writer);
					continue;
				}  // else fall-through
			default: break;
			}

			if ((ctx == WRITE_STRING && in == '"') ||
			    (ctx == WRITE_URI    && in == '>')) {
				snprintf(escape, sizeof(escape), "\\u%04X",
				         ctx == WRITE_STRING ? '"' : '>');
				len += sink(escape, 6, writer);
				continue;
			}
		}

		uint32_t c    = 0;
		size_t   size = 0;
		if ((in & 0x80) == 0) {  // Starts with `0'
			c = in & 0x7F;
			if (in_range(c, 0x20, 0x7E)
			    || (is_space(c) && ctx == WRITE_LONG_STRING)) {
				len += sink(&in, 1, writer);  // Print ASCII character
			} else {
				snprintf(escape, sizeof(escape), "\\u%04X", c);
				len += sink(escape, 6, writer);  // ASCII control character
			}
			continue;
		} else if ((in & 0xE0) == 0xC0) {  // Starts with `110'
			size = 2;
			c    = in & 0x1F;
		} else if ((in & 0xF0) == 0xE0) {  // Starts with `1110'
			size = 3;
			c    = in & 0x0F;
		} else if ((in & 0xF8) == 0xF0) {  // Starts with `11110'
			size = 4;
			c    = in & 0x07;
		} else {
			w_err(writer, SERD_ERR_BAD_ARG, "invalid UTF-8: %X\n", in);
			const uint8_t replacement_char[] = { 0xEF, 0xBF, 0xBD };
			len += sink(replacement_char, sizeof(replacement_char), writer);
			return len;
		}

		if (ctx != WRITE_URI && !(writer->style & SERD_STYLE_ASCII)) {
			// Write UTF-8 character directly to UTF-8 output
			// TODO: Always parse and validate character?
			len += sink(utf8 + i - 1, size, writer);
			i += size - 1;
			continue;
		}

#define READ_BYTE() \
		in = utf8[i++] & 0x3f; \
		c  = (c << 6) | in;

		switch (size) {
		case 4: READ_BYTE();
		case 3: READ_BYTE();
		case 2: READ_BYTE();
		}

		if (c < 0xFFFF) {
			snprintf(escape, sizeof(escape), "\\u%04X", c);
			len += sink(escape, 6, writer);
		} else {
			snprintf(escape, sizeof(escape), "\\U%08X", c);
			len += sink(escape, 10, writer);
		}
	}
	return len;
}

static size_t
uri_sink(const void* buf, size_t len, void* stream)
{
	return write_text((SerdWriter*)stream, WRITE_URI,
	                  (const uint8_t*)buf, len);
}

static void
write_newline(SerdWriter* writer)
{
	sink("\n", 1, writer);
	for (unsigned i = 0; i < writer->indent; ++i) {
		sink("\t", 1, writer);
	}
}

static void
write_sep(SerdWriter* writer, const Sep sep)
{
	const SepRule* rule = &rules[sep];
	if (rule->space_before) {
		write_newline(writer);
	}
	if (rule->str) {
		sink(rule->str, rule->len, writer);
	}
	if (    (writer->last_sep && rule->space_after_sep)
	    || (!writer->last_sep && rule->space_after_node)) {
		write_newline(writer);
	} else if (writer->last_sep && rule->space_after_node) {
		sink(" ", 1, writer);
	}
	writer->last_sep = sep;
}

static SerdStatus
reset_context(SerdWriter* writer, bool del)
{
	if (del) {
		serd_node_free(&writer->context.graph);
		serd_node_free(&writer->context.subject);
		serd_node_free(&writer->context.predicate);
		writer->context = WRITE_CONTEXT_NULL;
	} else {
		writer->context.graph.type     = SERD_NOTHING;
		writer->context.subject.type   = SERD_NOTHING;
		writer->context.predicate.type = SERD_NOTHING;
	}
	writer->empty = false;
	return SERD_SUCCESS;
}

typedef enum {
	FIELD_NONE,
	FIELD_SUBJECT,
	FIELD_PREDICATE,
	FIELD_OBJECT
} Field;

static bool
write_node(SerdWriter*        writer,
           const SerdNode*    node,
           const SerdNode*    datatype,
           const SerdNode*    lang,
           Field              field,
           SerdStatementFlags flags)
{
	SerdChunk uri_prefix;
	SerdChunk uri_suffix;
	bool      has_scheme;
	switch (node->type) {
	case SERD_BLANK:
		if (writer->syntax != SERD_NTRIPLES
		    && ((field == FIELD_SUBJECT && (flags & SERD_ANON_S_BEGIN))
		        || (field == FIELD_OBJECT && (flags & SERD_ANON_O_BEGIN)))) {
			++writer->indent;
			write_sep(writer, SEP_ANON_BEGIN);
		} else if (writer->syntax != SERD_NTRIPLES
		           && (field == FIELD_SUBJECT && (flags & SERD_LIST_S_BEGIN))) {
			assert(writer->list_depth == 0);
			copy_node(&writer->list_subj, node);
			++writer->list_depth;
			++writer->indent;
			write_sep(writer, SEP_LIST_BEGIN);
		} else if (writer->syntax != SERD_NTRIPLES
		           && (field == FIELD_OBJECT && (flags & SERD_LIST_O_BEGIN))) {
			++writer->indent;
			++writer->list_depth;
			write_sep(writer, SEP_LIST_BEGIN);
		} else if (writer->syntax != SERD_NTRIPLES
		           && ((field == FIELD_SUBJECT && (flags & SERD_EMPTY_S))
		               || (field == FIELD_OBJECT && (flags & SERD_EMPTY_O)))) {
			sink("[]", 2, writer);
		} else {
			sink("_:", 2, writer);
			if (writer->bprefix && !strncmp((const char*)node->buf,
			                                (const char*)writer->bprefix,
			                                writer->bprefix_len)) {
				sink(node->buf + writer->bprefix_len,
				     node->n_bytes - writer->bprefix_len,
				     writer);
			} else {
				sink(node->buf, node->n_bytes, writer);
			}
		}
		break;
	case SERD_CURIE:
		switch (writer->syntax) {
		case SERD_NTRIPLES:
			if (serd_env_expand(writer->env, node, &uri_prefix, &uri_suffix)) {
				w_err(writer, SERD_ERR_BAD_CURIE,
				      "undefined namespace prefix `%s'\n", node->buf);
				return false;
			}
			sink("<", 1, writer);
			write_text(writer, WRITE_URI, uri_prefix.buf, uri_prefix.len);
			write_text(writer, WRITE_URI, uri_suffix.buf, uri_suffix.len);
			sink(">", 1, writer);
			break;
		case SERD_TURTLE:
			sink(node->buf, node->n_bytes, writer);
		}
		break;
	case SERD_LITERAL:
		if (writer->syntax == SERD_TURTLE && datatype && datatype->buf) {
			const char* type_uri = (const char*)datatype->buf;
			if (!strncmp(type_uri, NS_XSD, sizeof(NS_XSD) - 1) && (
				    !strcmp(type_uri + sizeof(NS_XSD) - 1, "boolean") ||
				    !strcmp(type_uri + sizeof(NS_XSD) - 1, "decimal") ||
				    !strcmp(type_uri + sizeof(NS_XSD) - 1, "integer"))) {
				sink(node->buf, node->n_bytes, writer);
				break;
			}
		}
		if (writer->syntax != SERD_NTRIPLES
		    && (node->flags & (SERD_HAS_NEWLINE|SERD_HAS_QUOTE))) {
			sink("\"\"\"", 3, writer);
			write_text(writer, WRITE_LONG_STRING, node->buf, node->n_bytes);
			sink("\"\"\"", 3, writer);
		} else {
			sink("\"", 1, writer);
			write_text(writer, WRITE_STRING, node->buf, node->n_bytes);
			sink("\"", 1, writer);
		}
		if (lang && lang->buf) {
			sink("@", 1, writer);
			sink(lang->buf, lang->n_bytes, writer);
		} else if (datatype && datatype->buf) {
			sink("^^", 2, writer);
			write_node(writer, datatype, NULL, NULL, FIELD_NONE, flags);
		}
		break;
	case SERD_URI:
		has_scheme = serd_uri_string_has_scheme(node->buf);
		if (field == FIELD_PREDICATE && (writer->syntax == SERD_TURTLE)
		    && !strcmp((const char*)node->buf, NS_RDF "type")) {
			sink("a", 1, writer);
			break;
		} else if ((writer->syntax == SERD_TURTLE)
		           && !strcmp((const char*)node->buf, NS_RDF "nil")) {
			sink("()", 2, writer);
			break;
		} else if (has_scheme && (writer->style & SERD_STYLE_CURIED)) {
			SerdNode  prefix;
			SerdChunk suffix;
			if (serd_env_qualify(writer->env, node, &prefix, &suffix)) {
				write_text(writer, WRITE_URI, prefix.buf, prefix.n_bytes);
				sink(":", 1, writer);
				write_text(writer, WRITE_URI, suffix.buf, suffix.len);
				break;
			}
		}
		sink("<", 1, writer);
		if (writer->style & SERD_STYLE_RESOLVED) {
			SerdURI in_base_uri, uri, abs_uri;
			serd_env_get_base_uri(writer->env, &in_base_uri);
			serd_uri_parse(node->buf, &uri);
			serd_uri_resolve(&uri, &in_base_uri, &abs_uri);
			bool rooted = uri_is_under(&writer->base_uri, &writer->root_uri);
			SerdURI* root = rooted ? &writer->root_uri : & writer->base_uri;
			if (!uri_is_under(&abs_uri, root) ||
			    writer->syntax == SERD_NTRIPLES) {
				serd_uri_serialise(&abs_uri, uri_sink, writer);
			} else {
				serd_uri_serialise_relative(
					&uri, &writer->base_uri, root, uri_sink, writer);
			}
		} else {
			write_text(writer, WRITE_URI, node->buf, node->n_bytes);
		}
		sink(">", 1, writer);
	default:
		break;
	}
	writer->last_sep = SEP_NONE;
	return true;
}

static inline bool
is_resource(const SerdNode* node)
{
	return node->type > SERD_LITERAL;
}

static void
write_pred(SerdWriter* writer, SerdStatementFlags flags, const SerdNode* pred)
{
	write_node(writer, pred, NULL, NULL, FIELD_PREDICATE, flags);
	write_sep(writer, SEP_P_O);
	copy_node(&writer->context.predicate, pred);
}

static bool
write_list_obj(SerdWriter*        writer,
               SerdStatementFlags flags,
               const SerdNode*    predicate,
               const SerdNode*    object,
               const SerdNode*    datatype,
               const SerdNode*    lang)
{
	if (!strcmp((const char*)object->buf, NS_RDF "nil")) {
		--writer->indent;
		write_sep(writer, SEP_LIST_END);
		return true;
	} else if (!strcmp((const char*)predicate->buf, NS_RDF "first")) {
		write_sep(writer, SEP_LIST_SEP);
		write_node(writer, object, datatype, lang, FIELD_OBJECT, flags);
	}
	return false;
}

SERD_API
SerdStatus
serd_writer_write_statement(SerdWriter*        writer,
                            SerdStatementFlags flags,
                            const SerdNode*    graph,
                            const SerdNode*    subject,
                            const SerdNode*    predicate,
                            const SerdNode*    object,
                            const SerdNode*    datatype,
                            const SerdNode*    lang)
{
	if (!subject || !predicate || !object
	    || !subject->buf || !predicate->buf || !object->buf
	    || !is_resource(subject) || !is_resource(predicate)) {
		return SERD_ERR_BAD_ARG;
	}

	switch (writer->syntax) {
	case SERD_NTRIPLES:
		write_node(writer, subject, NULL, NULL, FIELD_SUBJECT, flags);
		sink(" ", 1, writer);
		write_node(writer, predicate, NULL, NULL, FIELD_PREDICATE, flags);
		sink(" ", 1, writer);
		if (!write_node(writer, object, datatype, lang, FIELD_OBJECT, flags)) {
			return SERD_ERR_UNKNOWN;
		}
		sink(" .\n", 3, writer);
		return SERD_SUCCESS;
	default:
		break;
	}

	if ((flags & SERD_LIST_CONT)) {
		if (write_list_obj(writer, flags, predicate, object, datatype, lang)) {
			// Reached end of list
			if (--writer->list_depth == 0 && writer->list_subj.type) {
				reset_context(writer, true);
				writer->context.subject = writer->list_subj;
				writer->list_subj       = SERD_NODE_NULL;
			}
			return SERD_SUCCESS;
		}
	} else if (serd_node_equals(subject, &writer->context.subject)) {
		if (serd_node_equals(predicate, &writer->context.predicate)) {
			// Abbreviate S P
			if (!(flags & SERD_ANON_O_BEGIN)) {
				++writer->indent;
			}
			write_sep(writer, SEP_END_O);
			write_node(writer, object, datatype, lang, FIELD_OBJECT, flags);
			if (!(flags & SERD_ANON_O_BEGIN)) {
				--writer->indent;
			}
		} else {
			// Abbreviate S
			Sep sep = writer->context.predicate.type ? SEP_END_P : SEP_S_P;
			write_sep(writer, sep);
			write_pred(writer, flags, predicate);
			write_node(writer, object, datatype, lang, FIELD_OBJECT, flags);
		}
	} else {
		// No abbreviation
		if (writer->context.subject.type) {
			assert(writer->indent > 0);
			--writer->indent;
			if (serd_stack_is_empty(&writer->anon_stack)) {
				write_sep(writer, SEP_END_S);
			}
		} else if (!writer->empty) {
			write_sep(writer, SEP_S_P);
		}

		if (!(flags & SERD_ANON_CONT)) {
			write_node(writer, subject, NULL, NULL, FIELD_SUBJECT, flags);
			++writer->indent;
			write_sep(writer, SEP_S_P);
		} else {
			++writer->indent;
		}

		reset_context(writer, true);
		copy_node(&writer->context.subject, subject);

		if (!(flags & SERD_LIST_S_BEGIN)) {
			write_pred(writer, flags, predicate);
		}

		write_node(writer, object, datatype, lang, FIELD_OBJECT, flags);
	}

	if (flags & (SERD_ANON_S_BEGIN|SERD_ANON_O_BEGIN)) {
		WriteContext* ctx = (WriteContext*)serd_stack_push(
			&writer->anon_stack, sizeof(WriteContext));
		*ctx = writer->context;
		WriteContext new_context = {
			serd_node_copy(graph), serd_node_copy(subject), SERD_NODE_NULL };
		if ((flags & SERD_ANON_S_BEGIN)) {
			new_context.predicate = serd_node_copy(predicate);
		}
		writer->context = new_context;
	} else {
		copy_node(&writer->context.graph, graph);
		copy_node(&writer->context.subject, subject);
		copy_node(&writer->context.predicate, predicate);
	}

	return SERD_SUCCESS;
}

SERD_API
SerdStatus
serd_writer_end_anon(SerdWriter*     writer,
                     const SerdNode* node)
{
	if (writer->syntax == SERD_NTRIPLES) {
		return SERD_SUCCESS;
	}
	if (serd_stack_is_empty(&writer->anon_stack)) {
		w_err(writer, SERD_ERR_UNKNOWN,
		      "unexpected end of anonymous node\n");
		return SERD_ERR_UNKNOWN;
	}
	assert(writer->indent > 0);
	--writer->indent;
	write_sep(writer, SEP_ANON_END);
	reset_context(writer, true);
	writer->context = *anon_stack_top(writer);
	serd_stack_pop(&writer->anon_stack, sizeof(WriteContext));
	const bool is_subject = serd_node_equals(node, &writer->context.subject);
	if (is_subject) {
		copy_node(&writer->context.subject, node);
		writer->context.predicate.type = SERD_NOTHING;
	}
	return SERD_SUCCESS;
}

SERD_API
SerdStatus
serd_writer_finish(SerdWriter* writer)
{
	if (writer->context.subject.type) {
		sink(" .\n", 3, writer);
	}
	if (writer->style & SERD_STYLE_BULK) {
		serd_bulk_sink_flush(&writer->bulk_sink);
	}
	writer->indent = 0;
	return reset_context(writer, true);
}

SERD_API
SerdWriter*
serd_writer_new(SerdSyntax     syntax,
                SerdStyle      style,
                SerdEnv*       env,
                const SerdURI* base_uri,
                SerdSink       ssink,
                void*          stream)
{
	const WriteContext context = WRITE_CONTEXT_NULL;
	SerdWriter*        writer  = (SerdWriter*)malloc(sizeof(SerdWriter));
	writer->syntax       = syntax;
	writer->style        = style;
	writer->env          = env;
	writer->root_node    = SERD_NODE_NULL;
	writer->root_uri     = SERD_URI_NULL;
	writer->base_uri     = base_uri ? *base_uri : SERD_URI_NULL;
	writer->anon_stack   = serd_stack_new(sizeof(WriteContext));
	writer->sink         = ssink;
	writer->stream       = stream;
	writer->error_sink   = NULL;
	writer->error_handle = NULL;
	writer->context      = context;
	writer->list_subj    = SERD_NODE_NULL;
	writer->list_depth   = 0;
	writer->bprefix      = NULL;
	writer->bprefix_len  = 0;
	writer->indent       = 0;
	writer->last_sep     = SEP_NONE;
	writer->empty        = true;
	if (style & SERD_STYLE_BULK) {
		writer->bulk_sink = serd_bulk_sink_new(ssink, stream, SERD_PAGE_SIZE);
	}
	return writer;
}

SERD_API
void
serd_writer_set_error_sink(SerdWriter*   writer,
                           SerdErrorSink error_sink,
                           void*         error_handle)
{
	writer->error_sink   = error_sink;
	writer->error_handle = error_handle;
}

SERD_API
void
serd_writer_chop_blank_prefix(SerdWriter*    writer,
                              const uint8_t* prefix)
{
	free(writer->bprefix);
	writer->bprefix_len = 0;
	writer->bprefix     = NULL;
	if (prefix) {
		writer->bprefix_len = strlen((const char*)prefix);
		writer->bprefix     = (uint8_t*)malloc(writer->bprefix_len + 1);
		memcpy(writer->bprefix, prefix, writer->bprefix_len + 1);
	}
}

SERD_API
SerdStatus
serd_writer_set_base_uri(SerdWriter*     writer,
                         const SerdNode* uri)
{
	if (!serd_env_set_base_uri(writer->env, uri)) {
		serd_env_get_base_uri(writer->env, &writer->base_uri);

		if (writer->syntax != SERD_NTRIPLES) {
			if (writer->context.graph.type || writer->context.subject.type) {
				sink(" .\n\n", 4, writer);
				reset_context(writer, false);
			}
			sink("@base <", 7, writer);
			sink(uri->buf, uri->n_bytes, writer);
			sink("> .\n", 4, writer);
		}
		writer->indent = 0;
		return reset_context(writer, false);
	}
	return SERD_ERR_UNKNOWN;
}

SERD_API
SerdStatus
serd_writer_set_root_uri(SerdWriter*     writer,
                         const SerdNode* uri)
{
	serd_node_free(&writer->root_node);
	if (uri && uri->buf) {
		writer->root_node = serd_node_copy(uri);
		serd_uri_parse(uri->buf, &writer->root_uri);
	} else {
		writer->root_node = SERD_NODE_NULL;
		writer->root_uri  = SERD_URI_NULL;
	}
	return SERD_SUCCESS;
}

SERD_API
SerdStatus
serd_writer_set_prefix(SerdWriter*     writer,
                       const SerdNode* name,
                       const SerdNode* uri)
{
	if (!serd_env_set_prefix(writer->env, name, uri)) {
		if (writer->syntax != SERD_NTRIPLES) {
			if (writer->context.graph.type || writer->context.subject.type) {
				sink(" .\n\n", 4, writer);
				reset_context(writer, false);
			}
			sink("@prefix ", 8, writer);
			sink(name->buf, name->n_bytes, writer);
			sink(": <", 3, writer);
			write_text(writer, WRITE_URI, uri->buf, uri->n_bytes);
			sink("> .\n", 4, writer);
		}
		writer->indent = 0;
		return reset_context(writer, false);
	}
	return SERD_ERR_UNKNOWN;
}

SERD_API
void
serd_writer_free(SerdWriter* writer)
{
	serd_writer_finish(writer);
	serd_stack_free(&writer->anon_stack);
	free(writer->bprefix);
	if (writer->style & SERD_STYLE_BULK) {
		serd_bulk_sink_free(&writer->bulk_sink);
	}
	serd_node_free(&writer->root_node);
	free(writer);
}

SERD_API
SerdEnv*
serd_writer_get_env(SerdWriter* writer)
{
	return writer->env;
}

SERD_API
size_t
serd_file_sink(const void* buf, size_t len, void* stream)
{
	return fwrite(buf, 1, len, (FILE*)stream);
}

SERD_API
size_t
serd_chunk_sink(const void* buf, size_t len, void* stream)
{
	SerdChunk* chunk = (SerdChunk*)stream;
	chunk->buf = (uint8_t*)realloc((uint8_t*)chunk->buf, chunk->len + len);
	memcpy((uint8_t*)chunk->buf + chunk->len, buf, len);
	chunk->len += len;
	return len;
}

SERD_API
uint8_t*
serd_chunk_sink_finish(SerdChunk* stream)
{
	serd_chunk_sink("", 1, stream);
	return (uint8_t*)stream->buf;
}
