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
   @file units.h
   C definitions for the LV2 Units extension
   <http://lv2plug.in/ns/extensions/units>.
*/

#ifndef LV2_UNITS_H
#define LV2_UNITS_H

#define LV2_UNITS_URI    "http://lv2plug.in/ns/extensions/units"
#define LV2_UNITS_PREFIX LV2_UNITS_URI "#"

#define LV2_UNITS__Conversion       LV2_UNITS_PREFIX "Conversion"
#define LV2_UNITS__Unit             LV2_UNITS_PREFIX "Unit"
#define LV2_UNITS__bar              LV2_UNITS_PREFIX "bar"
#define LV2_UNITS__beat             LV2_UNITS_PREFIX "beat"
#define LV2_UNITS__bpm              LV2_UNITS_PREFIX "bpm"
#define LV2_UNITS__cent             LV2_UNITS_PREFIX "cent"
#define LV2_UNITS__cm               LV2_UNITS_PREFIX "cm"
#define LV2_UNITS__coef             LV2_UNITS_PREFIX "coef"
#define LV2_UNITS__conversion       LV2_UNITS_PREFIX "conversion"
#define LV2_UNITS__db               LV2_UNITS_PREFIX "db"
#define LV2_UNITS__degree           LV2_UNITS_PREFIX "degree"
#define LV2_UNITS__frame            LV2_UNITS_PREFIX "frame"
#define LV2_UNITS__hz               LV2_UNITS_PREFIX "hz"
#define LV2_UNITS__inch             LV2_UNITS_PREFIX "inch"
#define LV2_UNITS__khz              LV2_UNITS_PREFIX "khz"
#define LV2_UNITS__km               LV2_UNITS_PREFIX "km"
#define LV2_UNITS__m                LV2_UNITS_PREFIX "m"
#define LV2_UNITS__mhz              LV2_UNITS_PREFIX "mhz"
#define LV2_UNITS__midiNote         LV2_UNITS_PREFIX "midiNote"
#define LV2_UNITS__mile             LV2_UNITS_PREFIX "mile"
#define LV2_UNITS__min              LV2_UNITS_PREFIX "min"
#define LV2_UNITS__mm               LV2_UNITS_PREFIX "mm"
#define LV2_UNITS__ms               LV2_UNITS_PREFIX "ms"
#define LV2_UNITS__name             LV2_UNITS_PREFIX "name"
#define LV2_UNITS__oct              LV2_UNITS_PREFIX "oct"
#define LV2_UNITS__pc               LV2_UNITS_PREFIX "pc"
#define LV2_UNITS__prefixConversion LV2_UNITS_PREFIX "prefixConversion"
#define LV2_UNITS__render           LV2_UNITS_PREFIX "render"
#define LV2_UNITS__s                LV2_UNITS_PREFIX "s"
#define LV2_UNITS__semitone12TET    LV2_UNITS_PREFIX "semitone12TET"
#define LV2_UNITS__symbol           LV2_UNITS_PREFIX "symbol"
#define LV2_UNITS__unit             LV2_UNITS_PREFIX "unit"

#endif  /* LV2_UNITS_H */
