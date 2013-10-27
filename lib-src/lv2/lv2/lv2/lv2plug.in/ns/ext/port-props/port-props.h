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
   @file port-props.h
   C definitions for the LV2 Port Props extension
   <http://lv2plug.in/ns/ext/port-props>.
*/

#ifndef LV2_PORT_PROPS_H
#define LV2_PORT_PROPS_H

#define LV2_PORT_PROPS_URI    "http://lv2plug.in/ns/ext/port-props"
#define LV2_PORT_PROPS_PREFIX LV2_PORT_PROPS_URI "#"

#define LV2_PORT_PROPS__causesArtifacts      LV2_PORT_PROPS_PREFIX "causesArtifacts"
#define LV2_PORT_PROPS__continuousCV         LV2_PORT_PROPS_PREFIX "continuousCV"
#define LV2_PORT_PROPS__discreteCV           LV2_PORT_PROPS_PREFIX "discreteCV"
#define LV2_PORT_PROPS__displayPriority      LV2_PORT_PROPS_PREFIX "displayPriority"
#define LV2_PORT_PROPS__expensive            LV2_PORT_PROPS_PREFIX "expensive"
#define LV2_PORT_PROPS__hasStrictBounds      LV2_PORT_PROPS_PREFIX "hasStrictBounds"
#define LV2_PORT_PROPS__logarithmic          LV2_PORT_PROPS_PREFIX "logarithmic"
#define LV2_PORT_PROPS__notAutomatic         LV2_PORT_PROPS_PREFIX "notAutomatic"
#define LV2_PORT_PROPS__notOnGUI             LV2_PORT_PROPS_PREFIX "notOnGUI"
#define LV2_PORT_PROPS__rangeSteps           LV2_PORT_PROPS_PREFIX "rangeSteps"
#define LV2_PORT_PROPS__supportsStrictBounds LV2_PORT_PROPS_PREFIX "supportsStrictBounds"
#define LV2_PORT_PROPS__trigger              LV2_PORT_PROPS_PREFIX "trigger"

#endif  /* LV2_PORT_PROPS_H */
