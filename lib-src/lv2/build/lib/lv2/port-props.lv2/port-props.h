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
   @defgroup port-props Port Properties

   Various port properties.

   @{
*/

#ifndef LV2_PORT_PROPS_H
#define LV2_PORT_PROPS_H

#define LV2_PORT_PROPS_URI    "http://lv2plug.in/ns/ext/port-props"  ///< http://lv2plug.in/ns/ext/port-props
#define LV2_PORT_PROPS_PREFIX LV2_PORT_PROPS_URI "#"                 ///< http://lv2plug.in/ns/ext/port-props#

#define LV2_PORT_PROPS__causesArtifacts      LV2_PORT_PROPS_PREFIX "causesArtifacts"       ///< http://lv2plug.in/ns/ext/port-props#causesArtifacts
#define LV2_PORT_PROPS__continuousCV         LV2_PORT_PROPS_PREFIX "continuousCV"          ///< http://lv2plug.in/ns/ext/port-props#continuousCV
#define LV2_PORT_PROPS__discreteCV           LV2_PORT_PROPS_PREFIX "discreteCV"            ///< http://lv2plug.in/ns/ext/port-props#discreteCV
#define LV2_PORT_PROPS__displayPriority      LV2_PORT_PROPS_PREFIX "displayPriority"       ///< http://lv2plug.in/ns/ext/port-props#displayPriority
#define LV2_PORT_PROPS__expensive            LV2_PORT_PROPS_PREFIX "expensive"             ///< http://lv2plug.in/ns/ext/port-props#expensive
#define LV2_PORT_PROPS__hasStrictBounds      LV2_PORT_PROPS_PREFIX "hasStrictBounds"       ///< http://lv2plug.in/ns/ext/port-props#hasStrictBounds
#define LV2_PORT_PROPS__logarithmic          LV2_PORT_PROPS_PREFIX "logarithmic"           ///< http://lv2plug.in/ns/ext/port-props#logarithmic
#define LV2_PORT_PROPS__notAutomatic         LV2_PORT_PROPS_PREFIX "notAutomatic"          ///< http://lv2plug.in/ns/ext/port-props#notAutomatic
#define LV2_PORT_PROPS__notOnGUI             LV2_PORT_PROPS_PREFIX "notOnGUI"              ///< http://lv2plug.in/ns/ext/port-props#notOnGUI
#define LV2_PORT_PROPS__rangeSteps           LV2_PORT_PROPS_PREFIX "rangeSteps"            ///< http://lv2plug.in/ns/ext/port-props#rangeSteps
#define LV2_PORT_PROPS__supportsStrictBounds LV2_PORT_PROPS_PREFIX "supportsStrictBounds"  ///< http://lv2plug.in/ns/ext/port-props#supportsStrictBounds
#define LV2_PORT_PROPS__trigger              LV2_PORT_PROPS_PREFIX "trigger"               ///< http://lv2plug.in/ns/ext/port-props#trigger

#endif  /* LV2_PORT_PROPS_H */

/**
   @}
*/
