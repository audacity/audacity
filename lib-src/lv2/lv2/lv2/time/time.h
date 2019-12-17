/*
  Copyright 2011-2016 David Robillard <http://drobilla.net>

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
   @defgroup time Time

   Properties for describing time, see <http://lv2plug.in/ns/ext/time> for
   details.

   Note the time extension is purely data, this header merely defines URIs for
   convenience.

   @{
*/

#ifndef LV2_TIME_H
#define LV2_TIME_H

#define LV2_TIME_URI    "http://lv2plug.in/ns/ext/time"  ///< http://lv2plug.in/ns/ext/time
#define LV2_TIME_PREFIX LV2_TIME_URI "#"                 ///< http://lv2plug.in/ns/ext/time#

#define LV2_TIME__Time            LV2_TIME_PREFIX "Time"             ///< http://lv2plug.in/ns/ext/time#Time
#define LV2_TIME__Position        LV2_TIME_PREFIX "Position"         ///< http://lv2plug.in/ns/ext/time#Position
#define LV2_TIME__Rate            LV2_TIME_PREFIX "Rate"             ///< http://lv2plug.in/ns/ext/time#Rate
#define LV2_TIME__position        LV2_TIME_PREFIX "position"         ///< http://lv2plug.in/ns/ext/time#position
#define LV2_TIME__barBeat         LV2_TIME_PREFIX "barBeat"          ///< http://lv2plug.in/ns/ext/time#barBeat
#define LV2_TIME__bar             LV2_TIME_PREFIX "bar"              ///< http://lv2plug.in/ns/ext/time#bar
#define LV2_TIME__beat            LV2_TIME_PREFIX "beat"             ///< http://lv2plug.in/ns/ext/time#beat
#define LV2_TIME__beatUnit        LV2_TIME_PREFIX "beatUnit"         ///< http://lv2plug.in/ns/ext/time#beatUnit
#define LV2_TIME__beatsPerBar     LV2_TIME_PREFIX "beatsPerBar"      ///< http://lv2plug.in/ns/ext/time#beatsPerBar
#define LV2_TIME__beatsPerMinute  LV2_TIME_PREFIX "beatsPerMinute"   ///< http://lv2plug.in/ns/ext/time#beatsPerMinute
#define LV2_TIME__frame           LV2_TIME_PREFIX "frame"            ///< http://lv2plug.in/ns/ext/time#frame
#define LV2_TIME__framesPerSecond LV2_TIME_PREFIX "framesPerSecond"  ///< http://lv2plug.in/ns/ext/time#framesPerSecond
#define LV2_TIME__speed           LV2_TIME_PREFIX "speed"            ///< http://lv2plug.in/ns/ext/time#speed

/**
   @}
*/

#endif  /* LV2_TIME_H */
