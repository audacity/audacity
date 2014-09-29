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

/**
   @file presets.h

   C definitions for the LV2 Presets extension
   <http://lv2plug.in/ns/ext/presets>.
*/

#ifndef LV2_PRESETS_H
#define LV2_PRESETS_H

#define LV2_PRESETS_URI    "http://lv2plug.in/ns/ext/presets"
#define LV2_PRESETS_PREFIX LV2_PRESETS_URI "#"

#define LV2_PRESETS__Preset LV2_PRESETS_PREFIX "Preset"
#define LV2_PRESETS__preset LV2_PRESETS_PREFIX "preset"
#define LV2_PRESETS__value  LV2_PRESETS_PREFIX "value"

#endif  /* LV2_PRESETS_H */
