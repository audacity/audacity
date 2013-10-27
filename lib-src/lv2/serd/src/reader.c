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
#include <errno.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NS_XSD "http://www.w3.org/2001/XMLSchema#"
#define NS_RDF "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

#define TRY_THROW(exp) if (!(exp)) goto except;
#define TRY_RET(exp)   if (!(exp)) return 0;

#ifdef SERD_STACK_CHECK
#    define SERD_STACK_ASSERT_TOP(reader, ref) \
            assert(ref == reader->allocs[reader->n_allocs - 1]);
#else
#    define SERD_STACK_ASSERT_TOP(reader, ref)
#endif

typedef struct {
	const uint8_t* filename;
	unsigned       line;
	unsigned       col;
} Cursor;

typedef uint32_t uchar;

/* Reference to a node in the stack (we can not use pointers since the
   stack may be reallocated, invalidating any pointers to elements).
*/
typedef size_t Ref;

typedef struct {
	Ref                 graph;
	Ref                 subject;
	Ref                 predicate;
	SerdStatementFlags* flags;
} ReadContext;

struct SerdReaderImpl {
	void*             handle;
	void              (*free_handle)(void* ptr);
	SerdBaseSink      base_sink;
	SerdPrefixSink    prefix_sink;
	SerdStatementSink statement_sink;
	SerdEndSink       end_sink;
	SerdErrorSink     error_sink;
	void*             error_handle;
	Ref               rdf_first;
	Ref               rdf_rest;
	Ref               rdf_nil;
	SerdNode          default_graph;
	FILE*             fd;
	SerdStack         stack;
	SerdSyntax        syntax;
	Cursor            cur;
	uint8_t*          buf;
	uint8_t*          bprefix;
	size_t            bprefix_len;
	unsigned          next_id;
	uint8_t*          read_buf;
	int32_t           read_head;  ///< Offset into read_buf
	uint8_t           read_byte;  ///< 1-byte 'buffer' used when not paging
	bool              from_file;  ///< True iff reading from @ref fd
	bool              paging;  ///< True iff reading a page at a time
	bool              eof;
	bool              seen_genid;
#ifdef SERD_STACK_CHECK
	Ref*              allocs;     ///< Stack of push offsets
	size_t            n_allocs;   ///< Number of stack pushes
#endif
};

static int
r_err(SerdReader* reader, SerdStatus st, const char* fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	const SerdError e = {
		st, reader->cur.filename, reader->cur.line, reader->cur.col, fmt, &args
	};
	serd_error(reader->error_sink, reader->error_handle, &e);
	va_end(args);
	return 0;
}

static inline SerdStatus
page(SerdReader* reader)
{
	reader->read_head = 0;
	size_t n_read = fread(reader->read_buf, 1, SERD_PAGE_SIZE, reader->fd);
	if (n_read == 0) {
		reader->read_buf[0] = '\0';
		reader->eof = true;
		return ferror(reader->fd) ? SERD_ERR_UNKNOWN : SERD_FAILURE;
	} else if (n_read < SERD_PAGE_SIZE) {
		reader->read_buf[n_read] = '\0';
	}
	return SERD_SUCCESS;
}

static inline uint8_t
peek_byte(SerdReader* reader)
{
	return reader->read_buf[reader->read_head];
}

static inline uint8_t
eat_byte_safe(SerdReader* reader, const uint8_t byte)
{
	assert(peek_byte(reader) == byte);
	switch (byte) {
	case '\0': reader->eof = true; break;
	case '\n': ++reader->cur.line; reader->cur.col = 0; break;
	default:   ++reader->cur.col;
	}

	if (reader->from_file && !reader->paging) {
		const int c = fgetc(reader->fd);
		reader->read_byte = (c == EOF) ? 0 : (uint8_t)c;
		if (c == EOF) {
			reader->eof = true;
		}
	} else if (++reader->read_head == SERD_PAGE_SIZE && reader->paging) {
		page(reader);
	}
	return byte;
}

static inline uint8_t
eat_byte_check(SerdReader* reader, const uint8_t byte)
{
	const uint8_t c = peek_byte(reader);
	if (c != byte) {
		return r_err(reader, SERD_ERR_BAD_SYNTAX,
		             "expected `%c', not `%c'\n", byte, c);
	}
	return eat_byte_safe(reader, byte);
}

static inline void
eat_string(SerdReader* reader, const char* str, unsigned n)
{
	for (unsigned i = 0; i < n; ++i) {
		eat_byte_check(reader, ((const uint8_t*)str)[i]);
	}
}

static Ref
push_node_padded(SerdReader* reader, size_t maxlen,
                 SerdType type, const char* str, size_t n_bytes)
{
	uint8_t* mem = serd_stack_push(&reader->stack,
	                               sizeof(SerdNode) + maxlen + 1);

	SerdNode* const node = (SerdNode*)mem;
	node->n_bytes = node->n_chars = n_bytes;
	node->flags   = 0;
	node->type    = type;
	node->buf     = NULL;

	uint8_t* buf = mem + sizeof(SerdNode);
	memcpy(buf, str, n_bytes + 1);

#ifdef SERD_STACK_CHECK
	reader->allocs = realloc(
		reader->allocs, sizeof(uint8_t*) * (++reader->n_allocs));
	reader->allocs[reader->n_allocs - 1] = (mem - reader->stack.buf);
#endif
	return (uint8_t*)node - reader->stack.buf;
}

static Ref
push_node(SerdReader* reader, SerdType type, const char* str, size_t n_bytes)
{
	return push_node_padded(reader, n_bytes, type, str, n_bytes);
}

static inline SerdNode*
deref(SerdReader* reader, const Ref ref)
{
	if (ref) {
		SerdNode* node = (SerdNode*)(reader->stack.buf + ref);
		node->buf = (uint8_t*)node + sizeof(SerdNode);
		return node;
	}
	return NULL;
}

static inline void
push_byte(SerdReader* reader, Ref ref, const uint8_t c)
{
	SERD_STACK_ASSERT_TOP(reader, ref);
	uint8_t* const  s    = serd_stack_push(&reader->stack, 1);
	SerdNode* const node = (SerdNode*)(reader->stack.buf + ref);
	++node->n_bytes;
	if (!(c & 0x80)) {  // Starts with 0 bit, start of new character
		++node->n_chars;
	}
	*(s - 1) = c;
	*s       = '\0';
}

static inline void
push_replacement(SerdReader* reader, Ref dest)
{
	push_byte(reader, dest, 0xEF);
	push_byte(reader, dest, 0xBF);
	push_byte(reader, dest, 0xBD);
}

static Ref
pop_node(SerdReader* reader, Ref ref)
{
	if (ref && ref != reader->rdf_first && ref != reader->rdf_rest
	    && ref != reader->rdf_nil) {
#ifdef SERD_STACK_CHECK
		SERD_STACK_ASSERT_TOP(reader, ref);
		--reader->n_allocs;
#endif
		SerdNode* const node = deref(reader, ref);
		uint8_t* const  top  = reader->stack.buf + reader->stack.size;
		serd_stack_pop(&reader->stack, top - (uint8_t*)node);
	}
	return 0;
}

static inline bool
emit_statement(SerdReader* reader, ReadContext ctx, Ref o, Ref d, Ref l)
{
	SerdNode* graph = deref(reader, ctx.graph);
	if (!graph && reader->default_graph.buf) {
		graph = &reader->default_graph;
	}
	bool ret = !reader->statement_sink ||
		!reader->statement_sink(
			reader->handle, *ctx.flags, graph,
			deref(reader, ctx.subject), deref(reader, ctx.predicate),
			deref(reader, o), deref(reader, d), deref(reader, l));
	*ctx.flags &= SERD_ANON_CONT|SERD_LIST_CONT;  // Preserve only cont flags
	return ret;
}

static bool
read_collection(SerdReader* reader, ReadContext ctx, Ref* dest);

static bool
read_predicateObjectList(SerdReader* reader, ReadContext ctx);

// [40]	hex	::=	[#x30-#x39] | [#x41-#x46]
static inline uint8_t
read_hex(SerdReader* reader)
{
	const uint8_t c = peek_byte(reader);
	if (in_range(c, 0x30, 0x39) || in_range(c, 0x41, 0x46)) {
		return eat_byte_safe(reader, c);
	} else {
		return r_err(reader, SERD_ERR_BAD_SYNTAX,
		             "invalid hexadecimal digit `%c'\n", c);
	}
}

static inline bool
read_hex_escape(SerdReader* reader, unsigned length, Ref dest)
{
	uint8_t buf[9] = { 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	for (unsigned i = 0; i < length; ++i) {
		if (!(buf[i] = read_hex(reader))) {
			return false;
		}
	}

	uint32_t c;
	sscanf((const char*)buf, "%X", &c);

	unsigned size = 0;
	if (c < 0x00000080) {
		size = 1;
	} else if (c < 0x00000800) {
		size = 2;
	} else if (c < 0x00010000) {
		size = 3;
	} else if (c < 0x00110000) {
		size = 4;
	} else {
		r_err(reader, SERD_ERR_BAD_SYNTAX,
		      "unicode character 0x%X out of range\n", c);
		push_replacement(reader, dest);
		return true;
	}

	// Build output in buf
	// (Note # of bytes = # of leading 1 bits in first byte)
	switch (size) {
	case 4:
		buf[3] = 0x80 | (uint8_t)(c & 0x3F);
		c >>= 6;
		c |= (16 << 12);  // set bit 4
	case 3:
		buf[2] = 0x80 | (uint8_t)(c & 0x3F);
		c >>= 6;
		c |= (32 << 6);  // set bit 5
	case 2:
		buf[1] = 0x80 | (uint8_t)(c & 0x3F);
		c >>= 6;
		c |= 0xC0;  // set bits 6 and 7
	case 1:
		buf[0] = (uint8_t)c;
	}

	for (unsigned i = 0; i < size; ++i) {
		push_byte(reader, dest, buf[i]);
	}
	return true;
}

static inline bool
read_character_escape(SerdReader* reader, Ref dest)
{
	switch (peek_byte(reader)) {
	case '\\':
		push_byte(reader, dest, eat_byte_safe(reader, '\\'));
		return true;
	case 'u':
		eat_byte_safe(reader, 'u');
		return read_hex_escape(reader, 4, dest);
	case 'U':
		eat_byte_safe(reader, 'U');
		return read_hex_escape(reader, 8, dest);
	default:
		return false;
	}
}

static inline bool
read_echaracter_escape(SerdReader* reader, Ref dest, SerdNodeFlags* flags)
{
	switch (peek_byte(reader)) {
	case 't':
		eat_byte_safe(reader, 't');
		push_byte(reader, dest, '\t');
		return true;
	case 'n':
		*flags |= SERD_HAS_NEWLINE;
		eat_byte_safe(reader, 'n');
		push_byte(reader, dest, '\n');
		return true;
	case 'r':
		*flags |= SERD_HAS_NEWLINE;
		eat_byte_safe(reader, 'r');
		push_byte(reader, dest, '\r');
		return true;
	default:
		return read_character_escape(reader, dest);
	}
}

static inline bool
read_scharacter_escape(SerdReader* reader, Ref dest, SerdNodeFlags* flags)
{
	switch (peek_byte(reader)) {
	case '"':
		*flags |= SERD_HAS_QUOTE;
		push_byte(reader, dest, eat_byte_safe(reader, '"'));
		return true;
	default:
		return read_echaracter_escape(reader, dest, flags);
	}
}

static inline bool
read_ucharacter_escape(SerdReader* reader, Ref dest)
{
	SerdNodeFlags flags = 0;
	switch (peek_byte(reader)) {
	case '>':
		push_byte(reader, dest, eat_byte_safe(reader, '>'));
		return true;
	default:
		return read_echaracter_escape(reader, dest, &flags);
	}
}

static inline SerdStatus
bad_char(SerdReader* reader, Ref dest, const char* fmt, uint8_t c)
{
	r_err(reader, SERD_ERR_BAD_SYNTAX, fmt, c);
	push_replacement(reader, dest);

	// Skip bytes until the next start byte
	for (uint8_t b = peek_byte(reader); (b & 0x80);) {
		eat_byte_safe(reader, b);
		b = peek_byte(reader);
	}

	return SERD_SUCCESS;
}

static SerdStatus
read_utf8_character(SerdReader* reader, Ref dest, uint8_t c)
{
	unsigned size = 1;
	if ((c & 0xE0) == 0xC0) {  // Starts with `110'
		size = 2;
	} else if ((c & 0xF0) == 0xE0) {  // Starts with `1110'
		size = 3;
	} else if ((c & 0xF8) == 0xF0) {  // Starts with `11110'
		size = 4;
	} else {
		return bad_char(reader, dest, "invalid UTF-8 start 0x%X\n",
		                eat_byte_safe(reader, c));
	}

	char bytes[4];
	bytes[0] = eat_byte_safe(reader, c);

	// Check character validity
	for (unsigned i = 1; i < size; ++i) {
		if (((bytes[i] = peek_byte(reader)) & 0x80) == 0) {
			return bad_char(reader, dest, "invalid UTF-8 continuation 0x%X\n",
			                bytes[i]);
		}
		eat_byte_safe(reader, bytes[i]);
	}

	// Emit character
	for (unsigned i = 0; i < size; ++i) {
		push_byte(reader, dest, bytes[i]);
	}
	return SERD_SUCCESS;
}

// [38] character ::= '\u' hex hex hex hex
//    | '\U' hex hex hex hex hex hex hex hex
//    | '\\'
//    | [#x20-#x5B] | [#x5D-#x10FFFF]
static inline SerdStatus
read_character(SerdReader* reader, Ref dest)
{
	const uint8_t c = peek_byte(reader);
	assert(c != '\\');  // Only called from methods that handle escapes first
	if (c == '\0') {
		r_err(reader, SERD_ERR_BAD_SYNTAX, "unexpected end of input\n", c);
		return SERD_ERR_BAD_SYNTAX;
	} else if (c < 0x20) {
		return bad_char(reader, dest,
		                "unexpected control character 0x%X\n",
		                eat_byte_safe(reader, c));
	} else if (!(c & 0x80)) {
		push_byte(reader, dest, eat_byte_safe(reader, c));
		return SERD_SUCCESS;
	} else {
		return read_utf8_character(reader, dest, c);
	}
}

// [43] lcharacter ::= echaracter | '\"' | #x9 | #xA | #xD
static inline SerdStatus
read_lcharacter(SerdReader* reader, Ref dest, SerdNodeFlags* flags)
{
	const uint8_t c = peek_byte(reader);
	uint8_t       buf[2];
	switch (c) {
	case '"':
		eat_byte_safe(reader, '\"');
		buf[0] = eat_byte_safe(reader, peek_byte(reader));
		buf[1] = eat_byte_safe(reader, peek_byte(reader));
		if (buf[0] == '\"' && buf[1] == '\"') {
			return SERD_FAILURE;
		} else {
			*flags |= SERD_HAS_QUOTE;
			push_byte(reader, dest, c);
			push_byte(reader, dest, buf[0]);
			push_byte(reader, dest, buf[1]);
			return SERD_SUCCESS;
		}
	case '\\':
		eat_byte_safe(reader, '\\');
		if (read_scharacter_escape(reader, dest, flags)) {
			return SERD_SUCCESS;
		} else {
			r_err(reader, SERD_ERR_BAD_SYNTAX,
			      "invalid escape `\\%c'\n", peek_byte(reader));
			return SERD_ERR_BAD_SYNTAX;
		}
	case 0xA: case 0xD:
		*flags |= SERD_HAS_NEWLINE;
	case 0x9:
		push_byte(reader, dest, eat_byte_safe(reader, c));
		return SERD_SUCCESS;
	default:
		return read_character(reader, dest);
	}
}

// [42] scharacter ::= ( echaracter - #x22 ) | '\"'
static inline SerdStatus
read_scharacter(SerdReader* reader, Ref dest, SerdNodeFlags* flags)
{
	uint8_t c = peek_byte(reader);
	switch (c) {
	case '\\':
		eat_byte_safe(reader, '\\');
		if (read_scharacter_escape(reader, dest, flags)) {
			return SERD_SUCCESS;
		} else {
			r_err(reader, SERD_ERR_BAD_SYNTAX,
			      "invalid escape `\\%c'\n", peek_byte(reader));
			return SERD_ERR_BAD_SYNTAX;
		}
	case '\"':
		return SERD_FAILURE;
	default:
		return read_character(reader, dest);
	}
}

// Spec: [41] ucharacter ::= ( character - #x3E ) | '\>'
// Impl: [41] ucharacter ::= ( echaracter - #x3E ) | '\>'
static inline SerdStatus
read_ucharacter(SerdReader* reader, Ref dest)
{
	const uint8_t c = peek_byte(reader);
	switch (c) {
	case '\\':
		eat_byte_safe(reader, '\\');
		if (read_ucharacter_escape(reader, dest)) {
			return SERD_SUCCESS;
		} else {
			r_err(reader, SERD_ERR_BAD_SYNTAX,
			      "invalid escape `\\%c'\n", peek_byte(reader));
			return SERD_FAILURE;
		}
	case '>':
		return SERD_FAILURE;
	default:
		return read_character(reader, dest);
	}
}

// [10] comment ::= '#' ( [^#xA #xD] )*
static void
read_comment(SerdReader* reader)
{
	eat_byte_safe(reader, '#');
	uint8_t c;
	while (((c = peek_byte(reader)) != 0xA) && (c != 0xD) && c) {
		eat_byte_safe(reader, c);
	}
}

// [24] ws ::= #x9 | #xA | #xD | #x20 | comment
static inline bool
read_ws(SerdReader* reader)
{
	const uint8_t c = peek_byte(reader);
	switch (c) {
	case 0x9: case 0xA: case 0xD: case 0x20:
		eat_byte_safe(reader, c);
		return true;
	case '#':
		read_comment(reader);
		return true;
	default:
		return false;
	}
}

static inline bool
read_ws_star(SerdReader* reader)
{
	while (read_ws(reader)) {}
	return true;
}

static inline bool
read_ws_plus(SerdReader* reader)
{
	TRY_RET(read_ws(reader));
	return read_ws_star(reader);
}

static inline bool
peek_delim(SerdReader* reader, const char delim)
{
	read_ws_star(reader);
	return peek_byte(reader) == delim;
}

static inline bool
eat_delim(SerdReader* reader, const char delim)
{
	if (peek_delim(reader, delim)) {
		eat_byte_safe(reader, delim);
		return read_ws_star(reader);
	}
	return false;
}

// [37] longString ::= #x22 #x22 #x22 lcharacter* #x22 #x22 #x22
static Ref
read_longString(SerdReader* reader, SerdNodeFlags* flags)
{
	Ref        ref = push_node(reader, SERD_LITERAL, "", 0);
	SerdStatus st;
	while (!(st = read_lcharacter(reader, ref, flags))) {}
	if (st < SERD_ERR_UNKNOWN) {
		return ref;
	}
	return pop_node(reader, ref);
}

// [36] string ::= #x22 scharacter* #x22
static Ref
read_string(SerdReader* reader, SerdNodeFlags* flags)
{
	Ref        ref = push_node(reader, SERD_LITERAL, "", 0);
	SerdStatus st;
	while (!(st = read_scharacter(reader, ref, flags))) {}
	if (st < SERD_ERR_UNKNOWN) {
		eat_byte_check(reader, '\"');
		return ref;
	}
	return pop_node(reader, ref);
}

// [35] quotedString ::= string | longString
static Ref
read_quotedString(SerdReader* reader, SerdNodeFlags* flags)
{
	eat_byte_safe(reader, '\"');  // q1
	const uint8_t q2 = peek_byte(reader);
	if (q2 != '\"') {  // Non-empty single-quoted string
		return read_string(reader, flags);
	}

	eat_byte_safe(reader, q2);
	const uint8_t q3 = peek_byte(reader);
	if (q3 != '\"') {  // Empty single-quoted string
		return push_node(reader, SERD_LITERAL, "", 0);
	}

	eat_byte_safe(reader, '\"');
	return read_longString(reader, flags);
}

// [34] relativeURI ::= ucharacter*
static inline Ref
read_relativeURI(SerdReader* reader)
{
	Ref ref = push_node(reader, SERD_URI, "", 0);
	SerdStatus st;
	while (!(st = read_ucharacter(reader, ref))) {}
	if (st < SERD_ERR_UNKNOWN) {
		return ref;
	}
	return pop_node(reader, ref);
}

// [30] nameStartChar ::= [A-Z] | "_" | [a-z]
//    | [#x00C0-#x00D6] | [#x00D8-#x00F6] | [#x00F8-#x02FF] | [#x0370-#x037D]
//    | [#x037F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF]
//    | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
static inline uchar
read_nameStartChar(SerdReader* reader)
{
	const uint8_t c = peek_byte(reader);
	if (c == '_' || is_alpha(c) || is_digit(c)) {  // TODO: Not correct
		return eat_byte_safe(reader, c);
	}
	return 0;
}

// [31] nameChar ::= nameStartChar | '-' | [0-9]
//    | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040]
static inline uchar
read_nameChar(SerdReader* reader)
{
	uchar c = read_nameStartChar(reader);
	if (c)
		return c;

	switch ((c = peek_byte(reader))) {
	case '-': case 0xB7: case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
		return eat_byte_safe(reader, c);
	default:  // TODO: 0x300-0x036F | 0x203F-0x2040
		return 0;
	}
	return 0;
}

// [33] prefixName ::= ( nameStartChar - '_' ) nameChar*
static Ref
read_prefixName(SerdReader* reader, Ref dest)
{
	uint8_t c = peek_byte(reader);
	if (c == '_') {
		r_err(reader, SERD_ERR_BAD_SYNTAX, "unexpected `_'\n");
		return pop_node(reader, dest);
	}
	TRY_RET(c = read_nameStartChar(reader));
	if (!dest) {
		dest = push_node(reader, SERD_CURIE, "", 0);
	}
	push_byte(reader, dest, c);
	while ((c = read_nameChar(reader))) {
		push_byte(reader, dest, c);
	}
	return dest;
}

// [32] name ::= nameStartChar nameChar*
static Ref
read_name(SerdReader* reader, Ref dest)
{
	uchar c = read_nameStartChar(reader);
	if (!c) {
		return 0;
	}
	do {
		push_byte(reader, dest, c);
	} while ((c = read_nameChar(reader)) != 0);
	return dest;
}

// [29] language ::= [a-z]+ ('-' [a-z0-9]+ )*
static Ref
read_language(SerdReader* reader)
{
	uint8_t c = peek_byte(reader);
	if (!in_range(c, 'a', 'z')) {
		return r_err(reader, SERD_ERR_BAD_SYNTAX, "unexpected `%c'\n", c);
	}
	Ref ref = push_node(reader, SERD_LITERAL, "", 0);
	push_byte(reader, ref, eat_byte_safe(reader, c));
	while ((c = peek_byte(reader)) && in_range(c, 'a', 'z')) {
		push_byte(reader, ref, eat_byte_safe(reader, c));
	}
	while (peek_byte(reader) == '-') {
		push_byte(reader, ref, eat_byte_safe(reader, '-'));
		while ((c = peek_byte(reader)) && (
			       in_range(c, 'a', 'z') || in_range(c, '0', '9'))) {
			push_byte(reader, ref, eat_byte_safe(reader, c));
		}
	}
	return ref;
}

// [28] uriref ::= '<' relativeURI '>'
static Ref
read_uriref(SerdReader* reader)
{
	TRY_RET(eat_byte_check(reader, '<'));
	Ref const str = read_relativeURI(reader);
	if (str && eat_byte_check(reader, '>')) {
		return str;
	}
	return pop_node(reader, str);
}

// [27] qname ::= prefixName? ':' name?
static Ref
read_qname(SerdReader* reader, Ref dest, bool read_prefix)
{
	Ref str = 0;
	if (!dest) {
		dest = push_node(reader, SERD_CURIE, "", 0);
	}
	if (read_prefix) {
		read_prefixName(reader, dest);
	}
	TRY_THROW(eat_byte_check(reader, ':'));
	push_byte(reader, dest, ':');
	str = read_name(reader, dest);
	return str ? str : dest;
except:
	return pop_node(reader, dest);
}

static bool
read_0_9(SerdReader* reader, Ref str, bool at_least_one)
{
	uint8_t c;
	if (at_least_one) {
		if (!is_digit((c = peek_byte(reader)))) {
			return r_err(reader, SERD_ERR_BAD_SYNTAX, "expected digit\n");
		}
		push_byte(reader, str, eat_byte_safe(reader, c));
	}
	while (is_digit((c = peek_byte(reader)))) {
		push_byte(reader, str, eat_byte_safe(reader, c));
	}
	return true;
}

// [19] exponent ::= [eE] ('-' | '+')? [0-9]+
// [18] decimal ::= ( '-' | '+' )? ( [0-9]+ '.' [0-9]*
//                                  | '.' ([0-9])+
//                                  | ([0-9])+ )
// [17] double  ::= ( '-' | '+' )? ( [0-9]+ '.' [0-9]* exponent
//                                  | '.' ([0-9])+ exponent
//                                  | ([0-9])+ exponent )
// [16] integer ::= ( '-' | '+' ) ? [0-9]+
static bool
read_number(SerdReader* reader, Ref* dest, Ref* datatype)
{
	#define XSD_DECIMAL NS_XSD "decimal"
	#define XSD_DOUBLE  NS_XSD "double"
	#define XSD_INTEGER NS_XSD "integer"
	Ref     ref         = push_node(reader, SERD_LITERAL, "", 0);
	uint8_t c           = peek_byte(reader);
	bool    has_decimal = false;
	if (c == '-' || c == '+') {
		push_byte(reader, ref, eat_byte_safe(reader, c));
	}
	if ((c = peek_byte(reader)) == '.') {
		has_decimal = true;
		// decimal case 2 (e.g. '.0' or `-.0' or `+.0')
		push_byte(reader, ref, eat_byte_safe(reader, c));
		TRY_THROW(read_0_9(reader, ref, true));
	} else {
		// all other cases ::= ( '-' | '+' ) [0-9]+ ( . )? ( [0-9]+ )? ...
		assert(is_digit(c));
		read_0_9(reader, ref, true);
		if ((c = peek_byte(reader)) == '.') {
			has_decimal = true;
			push_byte(reader, ref, eat_byte_safe(reader, c));
			read_0_9(reader, ref, false);
		}
	}
	c = peek_byte(reader);
	if (c == 'e' || c == 'E') {
		// double
		push_byte(reader, ref, eat_byte_safe(reader, c));
		switch ((c = peek_byte(reader))) {
		case '+': case '-':
			push_byte(reader, ref, eat_byte_safe(reader, c));
		default: break;
		}
		read_0_9(reader, ref, true);
		*datatype = push_node(reader, SERD_URI,
		                      XSD_DOUBLE, sizeof(XSD_DOUBLE) - 1);
	} else if (has_decimal) {
		*datatype = push_node(reader, SERD_URI,
		                      XSD_DECIMAL, sizeof(XSD_DECIMAL) - 1);
	} else {
		*datatype = push_node(reader, SERD_URI,
		                      XSD_INTEGER, sizeof(XSD_INTEGER) - 1);
	}
	*dest = ref;
	return true;
except:
	pop_node(reader, *datatype);
	pop_node(reader, ref);
	return false;
}

// [25] resource ::= uriref | qname
static bool
read_resource(SerdReader* reader, Ref* dest)
{
	switch (peek_byte(reader)) {
	case '<':
		*dest = read_uriref(reader);
		break;
	default:
		*dest = read_qname(reader, 0, true);
	}
	return *dest != 0;
}

static bool
read_literal(SerdReader* reader, Ref* dest,
             Ref* datatype, Ref* lang, SerdNodeFlags* flags)
{
	Ref str = read_quotedString(reader, flags);
	if (!str) {
		return false;
	}

	switch (peek_byte(reader)) {
	case '^':
		eat_byte_safe(reader, '^');
		eat_byte_check(reader, '^');
		TRY_THROW(read_resource(reader, datatype));
		break;
	case '@':
		eat_byte_safe(reader, '@');
		TRY_THROW(*lang = read_language(reader));
	}
	*dest = str;
	return true;
except:
	pop_node(reader, str);
	return false;
}

inline static bool
is_token_end(uint8_t c)
{
	switch (c) {
	case 0x9: case 0xA: case 0xD: case 0x20: case '\0':
	case '#': case '.': case ';': case '<':
		return true;
	default:
		return false;
	}
}

// [9] verb ::= predicate | 'a'
static bool
read_verb(SerdReader* reader, Ref* dest)
{
	SerdNode* node;
	bool ret;
	switch (peek_byte(reader)) {
	case '<':
		ret = (*dest = read_uriref(reader));
		break;
	default:
		/* Either a qname, or "a".  Read the prefix first, and if it is in fact
		   "a", produce that instead.
		*/
		*dest = read_prefixName(reader, 0);
		node  = deref(reader, *dest);
		if (node && node->n_bytes == 1 && node->buf[0] == 'a'
		    && is_token_end(peek_byte(reader))) {
			pop_node(reader, *dest);
			ret = (*dest = push_node(reader, SERD_URI, NS_RDF "type", 47));
		} else {
			ret = (*dest = read_qname(reader, *dest, false));
		}
	}
	read_ws_star(reader);
	return ret;
}

// [26] nodeID ::= '_:' name
static Ref
read_nodeID(SerdReader* reader)
{
	eat_byte_safe(reader, '_');
	eat_byte_check(reader, ':');
	Ref ref = push_node(reader, SERD_BLANK,
	                    reader->bprefix ? (char*)reader->bprefix : "",
	                    reader->bprefix_len);
	if (!read_name(reader, ref)) {
		return r_err(reader, SERD_ERR_BAD_SYNTAX,
		             "invalid character at start of name\n");
	}
	if (reader->syntax == SERD_TURTLE) {
		const char* const buf = (const char*)deref(reader, ref)->buf;
		if (!strncmp(buf, "genid", 5)) {
			memcpy((char*)buf, "docid", 5);  // Prevent clash
			reader->seen_genid = true;
		} else if (reader->seen_genid && !strncmp(buf, "docid", 5)) {
			r_err(reader, SERD_ERR_ID_CLASH,
			      "found both `genid' and `docid' IDs, prefix required\n");
			return pop_node(reader, ref);
		}
	}
	return ref;
}

static void
set_blank_id(SerdReader* reader, Ref ref, size_t buf_size)
{
	SerdNode*   node   = deref(reader, ref);
	const char* prefix = reader->bprefix ? (const char*)reader->bprefix : "";
	node->n_bytes = node->n_chars = snprintf(
		(char*)node->buf, buf_size, "%sgenid%u", prefix, reader->next_id++);
}

static size_t
genid_size(SerdReader* reader)
{
	return reader->bprefix_len + 5 + 10 + 1;  // + "genid" + UINT32_MAX + \0
}

static Ref
blank_id(SerdReader* reader)
{
	Ref ref = push_node_padded(reader, genid_size(reader), SERD_BLANK, "", 0);
	set_blank_id(reader, ref, genid_size(reader));
	return ref;
}

// Spec: [21] blank ::= nodeID | '[]'
//          | '[' predicateObjectList ']' | collection
// Impl: [21] blank ::= nodeID | '[' ws* ']'
//          | '[' ws* predicateObjectList ws* ']' | collection
static bool
read_blank(SerdReader* reader, ReadContext ctx, bool subject, Ref* dest)
{
	const SerdStatementFlags old_flags = *ctx.flags;
	bool empty;
	switch (peek_byte(reader)) {
	case '_':
		return (*dest = read_nodeID(reader));
	case '[':
		eat_byte_safe(reader, '[');
		if ((empty = peek_delim(reader, ']'))) {
			*ctx.flags |= (subject) ? SERD_EMPTY_S : SERD_EMPTY_O;
		} else {
			*ctx.flags |= (subject) ? SERD_ANON_S_BEGIN : SERD_ANON_O_BEGIN;
		}

		*dest = blank_id(reader);
		if (ctx.subject) {
			TRY_RET(emit_statement(reader, ctx, *dest, 0, 0));
		}

		ctx.subject = *dest;
		if (!empty) {
			*ctx.flags &= ~(SERD_LIST_CONT);
			if (!subject) {
				*ctx.flags |= SERD_ANON_CONT;
			}
			read_predicateObjectList(reader, ctx);
			read_ws_star(reader);
			if (reader->end_sink) {
				reader->end_sink(reader->handle, deref(reader, *dest));
			}
			*ctx.flags = old_flags;
		}
		eat_byte_check(reader, ']');
		return true;
	case '(':
		return read_collection(reader, ctx, dest);
	default:
		return r_err(reader, SERD_ERR_BAD_SYNTAX, "invalid blank node\n");
	}
}

// [13] object ::= resource | blank | literal
// Recurses, calling statement_sink for every statement encountered.
// Leaves stack in original calling state (i.e. pops everything it pushes).
static bool
read_object(SerdReader* reader, ReadContext ctx)
{
	static const char* const XSD_BOOLEAN     = NS_XSD "boolean";
	static const size_t      XSD_BOOLEAN_LEN = 40;

#ifndef NDEBUG
	const size_t orig_stack_size = reader->stack.size;
#endif

	bool          ret      = false;
	bool          emit     = (ctx.subject != 0);
	SerdNode*     node     = NULL;
	Ref           o        = 0;
	Ref           datatype = 0;
	Ref           lang     = 0;
	uint32_t      flags    = 0;
	const uint8_t c        = peek_byte(reader);
	switch (c) {
	case '\0':
	case ')':
		return false;
	case '[': case '(':
		emit = false;
		// fall through
	case '_':
		TRY_THROW(ret = read_blank(reader, ctx, false, &o));
		break;
	case '<': case ':':
		TRY_THROW(ret = read_resource(reader, &o));
		break;
	case '+': case '-': case '.': case '0': case '1': case '2': case '3':
	case '4': case '5': case '6': case '7': case '8': case '9':
		TRY_THROW(ret = read_number(reader, &o, &datatype));
		break;
	case '\"':
		TRY_THROW(ret = read_literal(reader, &o, &datatype, &lang, &flags));
		break;
	default:
		/* Either a boolean literal, or a qname.  Read the prefix first, and if
		   it is in fact a "true" or "false" literal, produce that instead.
		*/
		o    = read_prefixName(reader, 0);
		node = deref(reader, o);
		if (node && is_token_end(peek_byte(reader)) &&
		    ((node->n_bytes == 4 && !memcmp(node->buf, "true", 4))
		     || (node->n_bytes == 5 && !memcmp(node->buf, "false", 5)))) {
			node->type = SERD_LITERAL;
			datatype = push_node(reader, SERD_URI,
			                     XSD_BOOLEAN, XSD_BOOLEAN_LEN);
		} else {
			o = o ? o : push_node(reader, SERD_CURIE, "", 0);
			o = read_qname(reader, o, false);
		}
		ret = o;
	}

	if (ret && emit) {
		deref(reader, o)->flags = flags;
		ret = emit_statement(reader, ctx, o, datatype, lang);
	}

except:
	pop_node(reader, lang);
	pop_node(reader, datatype);
	pop_node(reader, o);
#ifndef NDEBUG
	assert(reader->stack.size == orig_stack_size);
#endif
	return ret;
}

// Spec: [8] objectList ::= object ( ',' object )*
// Impl: [8] objectList ::= object ( ws* ',' ws* object )*
static bool
read_objectList(SerdReader* reader, ReadContext ctx)
{
	TRY_RET(read_object(reader, ctx));
	while (eat_delim(reader, ',')) {
		TRY_RET(read_object(reader, ctx));
	}
	return true;
}

// Spec: [7] predicateObjectList ::= verb objectList
//                                   (';' verb objectList)* (';')?
// Impl: [7] predicateObjectList ::= verb ws* objectList
//                                   (ws* ';' ws* verb ws+ objectList)* (';')?
static bool
read_predicateObjectList(SerdReader* reader, ReadContext ctx)
{
	TRY_RET(read_verb(reader, &ctx.predicate));
	TRY_THROW(read_objectList(reader, ctx));
	ctx.predicate = pop_node(reader, ctx.predicate);
	while (eat_delim(reader, ';')) {
		switch (peek_byte(reader)) {
		case '.': case ']':
			return true;
		default:
			TRY_THROW(read_verb(reader, &ctx.predicate));
			TRY_THROW(read_objectList(reader, ctx));
			ctx.predicate = pop_node(reader, ctx.predicate);
		}
	}
	pop_node(reader, ctx.predicate);
	return true;
except:
	pop_node(reader, ctx.predicate);
	return false;
}

static bool
end_collection(SerdReader* reader, ReadContext ctx, Ref n1, Ref n2, bool ret)
{
	pop_node(reader, n2);
	pop_node(reader, n1);
	*ctx.flags &= ~SERD_LIST_CONT;
	return ret && (eat_byte_safe(reader, ')') == ')');
}

// [22] itemList   ::= object+
// [23] collection ::= '(' itemList? ')'
static bool
read_collection(SerdReader* reader, ReadContext ctx, Ref* dest)
{
	eat_byte_safe(reader, '(');
	bool end = peek_delim(reader, ')');
	*dest = end ? reader->rdf_nil : blank_id(reader);
	if (ctx.subject) {
		// subject predicate _:head
		*ctx.flags |= (end ? 0 : SERD_LIST_O_BEGIN);
		TRY_RET(emit_statement(reader, ctx, *dest, 0, 0));
		*ctx.flags |= SERD_LIST_CONT;
	} else {
		*ctx.flags |= (end ? 0 : SERD_LIST_S_BEGIN);
	}

	if (end) {
		return end_collection(reader, ctx, 0, 0, true);
	}

	/* The order of node allocation here is necessarily not in stack order,
	   so we create two nodes and recycle them throughout. */
	Ref n1   = push_node_padded(reader, genid_size(reader), SERD_BLANK, "", 0);
	Ref n2   = 0;
	Ref node = n1;
	Ref rest = 0;

	ctx.subject = *dest;
	while (!(end = peek_delim(reader, ')'))) {
		// _:node rdf:first object
		ctx.predicate = reader->rdf_first;
		if (!read_object(reader, ctx)) {
			return end_collection(reader, ctx, n1, n2, false);
		}

		if (!(end = peek_delim(reader, ')'))) {
			/* Give rest a new ID.  Done as late as possible to ensure it is
			   used and > IDs generated by read_object above. */
			if (!rest) {
				rest = n2 = blank_id(reader);  // First pass, push a new node
			} else {
				set_blank_id(reader, rest, genid_size(reader));
			}
		}

		// _:node rdf:rest _:rest
		*ctx.flags |= SERD_LIST_CONT;
		ctx.predicate = reader->rdf_rest;
		TRY_RET(emit_statement(reader, ctx,
		                       (end ? reader->rdf_nil : rest), 0, 0));

		ctx.subject = rest;         // _:node = _:rest
		rest        = node;         // _:rest = (old)_:node
		node        = ctx.subject;  // invariant
	}

	return end_collection(reader, ctx, n1, n2, true);
}

// [11] subject ::= resource | blank
static Ref
read_subject(SerdReader* reader, ReadContext ctx)
{
	Ref subject = 0;
	switch (peek_byte(reader)) {
	case '[': case '(': case '_':
		read_blank(reader, ctx, true, &subject);
		break;
	default:
		read_resource(reader, &subject);
	}
	return subject;
}

// Spec: [6] triples ::= subject predicateObjectList
// Impl: [6] triples ::= subject ws+ predicateObjectList
static bool
read_triples(SerdReader* reader, ReadContext ctx)
{
	const Ref subject = read_subject(reader, ctx);
	bool      ret     = false;
	if (subject) {
		ctx.subject = subject;
		TRY_RET(read_ws_plus(reader));
		ret = read_predicateObjectList(reader, ctx);
		pop_node(reader, subject);
	}
	ctx.subject = ctx.predicate = 0;
	return ret;
}

// [5] base ::= '@base' ws+ uriref
static bool
read_base(SerdReader* reader)
{
	// `@' is already eaten in read_directive
	eat_string(reader, "base", 4);
	TRY_RET(read_ws_plus(reader));
	Ref uri;
	TRY_RET(uri = read_uriref(reader));
	if (reader->base_sink) {
		reader->base_sink(reader->handle, deref(reader, uri));
	}
	pop_node(reader, uri);
	return true;
}

// Spec: [4] prefixID ::= '@prefix' ws+ prefixName? ':' uriref
// Impl: [4] prefixID ::= '@prefix' ws+ prefixName? ':' ws* uriref
static bool
read_prefixID(SerdReader* reader)
{
	bool ret  = true;
	Ref  name = 0;
	Ref  uri  = 0;
	// `@' is already eaten in read_directive
	eat_string(reader, "prefix", 6);
	TRY_RET(read_ws_plus(reader));
	name = read_prefixName(reader, 0);
	if (!name) {
		name = push_node(reader, SERD_LITERAL, "", 0);
	}
	TRY_THROW(eat_byte_check(reader, ':') == ':');
	read_ws_star(reader);
	TRY_THROW(uri = read_uriref(reader));
	if (reader->prefix_sink) {
		ret = !reader->prefix_sink(reader->handle,
		                           deref(reader, name),
		                           deref(reader, uri));
	}
	pop_node(reader, uri);
except:
	pop_node(reader, name);
	return ret;
}

// [3] directive ::= prefixID | base
static bool
read_directive(SerdReader* reader)
{
	eat_byte_safe(reader, '@');
	switch (peek_byte(reader)) {
	case 'b': return read_base(reader);
	case 'p': return read_prefixID(reader);
	default:  return r_err(reader, SERD_ERR_BAD_SYNTAX, "invalid directive\n");
	}
}

// Spec: [1] statement ::= directive '.' | triples '.' | ws+
// Impl: [1] statement ::= directive ws* '.' | triples ws* '.' | ws+
static bool
read_statement(SerdReader* reader)
{
	SerdStatementFlags flags = 0;
	ReadContext ctx = { 0, 0, 0, &flags };
	read_ws_star(reader);
	switch (peek_byte(reader)) {
	case '\0':
		reader->eof = true;
		return true;
	case '@':
		TRY_RET(read_directive(reader));
		break;
	default:
		TRY_RET(read_triples(reader, ctx));
		break;
	}
	read_ws_star(reader);
	return eat_byte_check(reader, '.');
}

// [1] turtleDoc ::= (statement)*
static bool
read_turtleDoc(SerdReader* reader)
{
	while (!reader->eof) {
		TRY_RET(read_statement(reader));
	}
	return true;
}

SERD_API
SerdReader*
serd_reader_new(SerdSyntax        syntax,
                void*             handle,
                void              (*free_handle)(void*),
                SerdBaseSink      base_sink,
                SerdPrefixSink    prefix_sink,
                SerdStatementSink statement_sink,
                SerdEndSink       end_sink)
{
	const Cursor cur = { NULL, 0, 0 };
	SerdReader*  me  = (SerdReader*)malloc(sizeof(struct SerdReaderImpl));
	me->handle           = handle;
	me->free_handle      = free_handle;
	me->base_sink        = base_sink;
	me->prefix_sink      = prefix_sink;
	me->statement_sink   = statement_sink;
	me->end_sink         = end_sink;
	me->error_sink       = NULL;
	me->error_handle     = NULL;
	me->default_graph    = SERD_NODE_NULL;
	me->fd               = 0;
	me->stack            = serd_stack_new(SERD_PAGE_SIZE);
	me->syntax           = syntax;
	me->cur              = cur;
	me->bprefix          = NULL;
	me->bprefix_len      = 0;
	me->next_id          = 1;
	me->read_buf         = 0;
	me->read_head        = 0;
	me->eof              = false;
	me->seen_genid       = false;
#ifdef SERD_STACK_CHECK
	me->allocs           = 0;
	me->n_allocs         = 0;
#endif

	me->rdf_first = push_node(me, SERD_URI, NS_RDF "first", 48);
	me->rdf_rest  = push_node(me, SERD_URI, NS_RDF "rest", 47);
	me->rdf_nil   = push_node(me, SERD_URI, NS_RDF "nil", 46);

	return me;
}

SERD_API
void
serd_reader_set_error_sink(SerdReader*   reader,
                           SerdErrorSink error_sink,
                           void*         error_handle)
{
	reader->error_sink   = error_sink;
	reader->error_handle = error_handle;
}

SERD_API
void
serd_reader_free(SerdReader* reader)
{
	pop_node(reader, reader->rdf_nil);
	pop_node(reader, reader->rdf_rest);
	pop_node(reader, reader->rdf_first);
	serd_node_free(&reader->default_graph);

#ifdef SERD_STACK_CHECK
	free(reader->allocs);
#endif
	free(reader->stack.buf);
	free(reader->bprefix);
	if (reader->free_handle) {
		reader->free_handle(reader->handle);
	}
	free(reader);
}

SERD_API
void*
serd_reader_get_handle(const SerdReader* reader)
{
	return reader->handle;
}

SERD_API
void
serd_reader_add_blank_prefix(SerdReader*    reader,
                             const uint8_t* prefix)
{
	free(reader->bprefix);
	reader->bprefix_len = 0;
	reader->bprefix     = NULL;
	if (prefix) {
		reader->bprefix_len = strlen((const char*)prefix);
		reader->bprefix     = (uint8_t*)malloc(reader->bprefix_len + 1);
		memcpy(reader->bprefix, prefix, reader->bprefix_len + 1);
	}
}

SERD_API
void
serd_reader_set_default_graph(SerdReader*     reader,
                              const SerdNode* graph)
{
	serd_node_free(&reader->default_graph);
	reader->default_graph = serd_node_copy(graph);
}

SERD_API
SerdStatus
serd_reader_read_file(SerdReader*    reader,
                      const uint8_t* uri)
{
	const uint8_t* path = serd_uri_to_path(uri);
	if (!path) {
		return SERD_ERR_BAD_ARG;
	}

	FILE* fd = serd_fopen((const char*)path, "r");
	if (!fd) {
		return SERD_ERR_UNKNOWN;
	}

	SerdStatus ret = serd_reader_read_file_handle(reader, fd, path);
	fclose(fd);
	return ret;
}

static void
skip_bom(SerdReader* me)
{
	const uint8_t* const b = me->read_buf;
	if (me->paging && b[0] == 0xEF && b[1] == 0xBB && b[2] == 0xBF) {
		me->read_head += 3;
	}
}

SERD_API
SerdStatus
serd_reader_start_stream(SerdReader*    me,
                         FILE*          file,
                         const uint8_t* name,
                         bool           bulk)
{
	const Cursor cur = { name, 1, 1 };
	me->fd        = file;
	me->read_head = 0;
	me->cur       = cur;
	me->from_file = true;
	me->eof       = false;
	me->paging    = bulk;

	if (bulk) {
		me->read_buf = (uint8_t*)serd_bufalloc(SERD_PAGE_SIZE);
		memset(me->read_buf, '\0', SERD_PAGE_SIZE);
		SerdStatus st = page(me);
		if (st) {
			serd_reader_end_stream(me);
			return st;
		}
		skip_bom(me);
	} else {
		me->read_buf  = &me->read_byte;
		me->read_byte = 0;  // Don't read to avoid potentially blocking
	}

	return SERD_SUCCESS;
}

SERD_API
SerdStatus
serd_reader_read_chunk(SerdReader* me)
{
	if (!me->read_byte) {
		// Read initial byte
		const int c = fgetc(me->fd);
		me->read_byte = (c == EOF) ? 0 : (uint8_t)c;
		if (c == EOF) {
			me->eof = true;
			return SERD_FAILURE;
		}
	}
	return read_statement(me) ? SERD_SUCCESS : SERD_FAILURE;
}

SERD_API
SerdStatus
serd_reader_end_stream(SerdReader* me)
{
	if (me->paging) {
		free(me->read_buf);
	}
	me->fd       = 0;
	me->read_buf = NULL;
	return SERD_SUCCESS;
}

SERD_API
SerdStatus
serd_reader_read_file_handle(SerdReader* me, FILE* file, const uint8_t* name)
{
	SerdStatus st = serd_reader_start_stream(me, file, name, true);
	if (!st) {
		st = read_turtleDoc(me) ? SERD_SUCCESS : SERD_ERR_UNKNOWN;
		serd_reader_end_stream(me);
	}
	return st;
}

SERD_API
SerdStatus
serd_reader_read_string(SerdReader* me, const uint8_t* utf8)
{
	const Cursor cur = { (const uint8_t*)"(string)", 1, 1 };

	me->read_buf  = (uint8_t*)utf8;
	me->read_head = 0;
	me->cur       = cur;
	me->from_file = false;
	me->paging    = false;
	me->eof       = false;

	skip_bom(me);
	const bool ret = read_turtleDoc(me);

	me->read_buf = NULL;
	return ret ? SERD_SUCCESS : SERD_ERR_UNKNOWN;
}
