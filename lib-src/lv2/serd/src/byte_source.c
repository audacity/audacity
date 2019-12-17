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

SerdStatus
serd_byte_source_page(SerdByteSource* source)
{
	source->read_head = 0;
	const size_t n_read = source->read_func(
		source->file_buf, 1, source->page_size, source->stream);
	if (n_read == 0) {
		source->file_buf[0] = '\0';
		source->eof         = true;
		return (source->error_func(source->stream)
		        ? SERD_ERR_UNKNOWN : SERD_FAILURE);
	} else if (n_read < source->page_size) {
		source->file_buf[n_read] = '\0';
	}
	return SERD_SUCCESS;
}

SerdStatus
serd_byte_source_open_source(SerdByteSource*     source,
                             SerdSource          read_func,
                             SerdStreamErrorFunc error_func,
                             void*               stream,
                             const uint8_t*      name,
                             size_t              page_size)
{
	const Cursor cur = { name, 1, 1 };

	memset(source, '\0', sizeof(*source));
	source->stream      = stream;
	source->from_stream = true;
	source->page_size   = page_size;
	source->cur         = cur;
	source->error_func  = error_func;
	source->read_func   = read_func;

	if (page_size > 1) {
		source->file_buf = (uint8_t*)serd_bufalloc(page_size);
		source->read_buf = source->file_buf;
		memset(source->file_buf, '\0', page_size);
	} else {
		source->read_buf = &source->read_byte;
	}

	return SERD_SUCCESS;
}

SerdStatus
serd_byte_source_prepare(SerdByteSource* source)
{
	source->prepared = true;
	if (source->from_stream) {
		if (source->page_size > 1) {
			return serd_byte_source_page(source);
		} else if (source->from_stream) {
			return serd_byte_source_advance(source);
		}
	}
	return SERD_SUCCESS;
}

SerdStatus
serd_byte_source_open_string(SerdByteSource* source, const uint8_t* utf8)
{
	const Cursor cur = { (const uint8_t*)"(string)", 1, 1 };

	memset(source, '\0', sizeof(*source));
	source->cur      = cur;
	source->read_buf = utf8;
	return SERD_SUCCESS;
}

SerdStatus
serd_byte_source_close(SerdByteSource* source)
{
	if (source->page_size > 1) {
		free(source->file_buf);
	}
	memset(source, '\0', sizeof(*source));
	return SERD_SUCCESS;
}
