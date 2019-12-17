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

#ifndef ZIX_DIGEST_H
#define ZIX_DIGEST_H

#include <stddef.h>
#include <stdint.h>

#include "zix/common.h"

#ifdef __cplusplus
extern "C" {
#endif

ZIX_API uint32_t
zix_digest_start(void);

ZIX_API uint32_t
zix_digest_add(uint32_t hash, const void* buf, size_t len);

#ifdef __cplusplus
}  /* extern "C" */
#endif

#endif  /* ZIX_DIGEST_H */
