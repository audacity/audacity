/*
  Copyright 2007-2016 David Robillard <http://drobilla.net>

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

#ifndef LV2_BUF_SIZE_H
#define LV2_BUF_SIZE_H

/**
   @defgroup buf-size Buffer Size

   Access to, and restrictions on, buffer sizes; see
   <http://lv2plug.in/ns/ext/buf-size> for details.

   @{
*/

#define LV2_BUF_SIZE_URI    "http://lv2plug.in/ns/ext/buf-size"  ///< http://lv2plug.in/ns/ext/buf-size
#define LV2_BUF_SIZE_PREFIX LV2_BUF_SIZE_URI "#" ///< http://lv2plug.in/ns/ext/buf-size#

#define LV2_BUF_SIZE__boundedBlockLength  LV2_BUF_SIZE_PREFIX "boundedBlockLength"   ///< http://lv2plug.in/ns/ext/buf-size#boundedBlockLength
#define LV2_BUF_SIZE__fixedBlockLength    LV2_BUF_SIZE_PREFIX "fixedBlockLength"     ///< http://lv2plug.in/ns/ext/buf-size#fixedBlockLength
#define LV2_BUF_SIZE__maxBlockLength      LV2_BUF_SIZE_PREFIX "maxBlockLength"       ///< http://lv2plug.in/ns/ext/buf-size#maxBlockLength
#define LV2_BUF_SIZE__minBlockLength      LV2_BUF_SIZE_PREFIX "minBlockLength"       ///< http://lv2plug.in/ns/ext/buf-size#minBlockLength
#define LV2_BUF_SIZE__nominalBlockLength  LV2_BUF_SIZE_PREFIX "nominalBlockLength"   ///< http://lv2plug.in/ns/ext/buf-size#nominalBlockLength
#define LV2_BUF_SIZE__powerOf2BlockLength LV2_BUF_SIZE_PREFIX "powerOf2BlockLength"  ///< http://lv2plug.in/ns/ext/buf-size#powerOf2BlockLength
#define LV2_BUF_SIZE__sequenceSize        LV2_BUF_SIZE_PREFIX "sequenceSize"         ///< http://lv2plug.in/ns/ext/buf-size#sequenceSize

/**
   @}
*/

#endif  /* LV2_BUF_SIZE_H */
