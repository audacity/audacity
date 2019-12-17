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

#include "serd_internal.h"

static inline uint8_t
peek_byte(SerdReader* reader)
{
	return serd_byte_source_peek(&reader->source);
}

static inline uint8_t
eat_byte(SerdReader* reader)
{
	const uint8_t    c  = peek_byte(reader);
	const SerdStatus st = serd_byte_source_advance(&reader->source);
	if (st) {
		reader->status = st;
	}
	return c;
}

static inline uint8_t
eat_byte_safe(SerdReader* reader, const uint8_t byte)
{
	(void)byte;

	const uint8_t c = eat_byte(reader);
	assert(c == byte);
	return c;
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

static inline bool
eat_string(SerdReader* reader, const char* str, unsigned n)
{
	bool bad = false;
	for (unsigned i = 0; i < n; ++i) {
		bad |= (bool)eat_byte_check(reader, ((const uint8_t*)str)[i]);
	}
	return bad;
}

static inline SerdStatus
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
	return SERD_SUCCESS;
}

static inline void
push_bytes(SerdReader* reader, Ref ref, const uint8_t* bytes, unsigned len)
{
	for (unsigned i = 0; i < len; ++i) {
		push_byte(reader, ref, bytes[i]);
	}
}
