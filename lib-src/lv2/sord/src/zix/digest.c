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

#include "zix/digest.h"

#ifdef __SSE4_2__
#    include <smmintrin.h>
#endif

ZIX_API uint32_t
zix_digest_start(void)
{
#ifdef __SSE4_2__
	return 1;  // CRC32 initial value
#else
	return 5381;  // DJB hash initial value
#endif
}

ZIX_API uint32_t
zix_digest_add(uint32_t hash, const void* const buf, const size_t len)
{
	const uint8_t* str = (const uint8_t*)buf;
#ifdef __SSE4_2__
	// SSE 4.2 CRC32
	for (size_t i = 0; i < (len / sizeof(uint32_t)); ++i) {
		hash = _mm_crc32_u32(hash, *(const uint32_t*)str);
		str += sizeof(uint32_t);
	}
	if (len & sizeof(uint16_t)) {
		hash = _mm_crc32_u16(hash, *(const uint16_t*)str);
		str += sizeof(uint16_t);
	}
	if (len & sizeof(uint8_t)) {
		hash = _mm_crc32_u8(hash, *(const uint8_t*)str);
	}
#else
	// Classic DJB hash
	for (size_t i = 0; i < len; ++i) {
		hash = (hash << 5) + hash + str[i];
	}
#endif
	return hash;
}
