/*
  Copyright 2012-2016 David Robillard <http://drobilla.net>

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

/**
   @defgroup morph Morph

   Ports that can dynamically change type, see <http://lv2plug.in/ns/ext/morph>
   for details.

   @{
*/

#ifndef LV2_MORPH_H
#define LV2_MORPH_H

#define LV2_MORPH_URI    "http://lv2plug.in/ns/ext/morph"  ///< http://lv2plug.in/ns/ext/morph
#define LV2_MORPH_PREFIX LV2_MORPH_URI "#"                 ///< http://lv2plug.in/ns/ext/morph#

#define LV2_MORPH__AutoMorphPort LV2_MORPH_PREFIX "AutoMorphPort"  ///< http://lv2plug.in/ns/ext/morph#AutoMorphPort
#define LV2_MORPH__MorphPort     LV2_MORPH_PREFIX "MorphPort"      ///< http://lv2plug.in/ns/ext/morph#MorphPort
#define LV2_MORPH__interface     LV2_MORPH_PREFIX "interface"      ///< http://lv2plug.in/ns/ext/morph#interface
#define LV2_MORPH__supportsType  LV2_MORPH_PREFIX "supportsType"   ///< http://lv2plug.in/ns/ext/morph#supportsType
#define LV2_MORPH__currentType   LV2_MORPH_PREFIX "currentType"    ///< http://lv2plug.in/ns/ext/morph#currentType

#endif  /* LV2_MORPH_H */

/**
   @}
*/
