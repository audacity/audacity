/*
  Copyright 2008-2016 David Robillard <http://drobilla.net>

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
   @defgroup uri-map URI Map

   C API for the LV2 URI Map extension <http://lv2plug.in/ns/ext/uri-map>.

   This extension defines a simple mechanism for plugins to map URIs to
   integers, usually for performance reasons (e.g. processing events typed by
   URIs in real time). The expected use case is for plugins to map URIs to
   integers for things they 'understand' at instantiation time, and store those
   values for use in the audio thread without doing any string comparison.
   This allows the extensibility of RDF with the performance of integers (or
   centrally defined enumerations).

   @{
*/

#ifndef LV2_URI_MAP_H
#define LV2_URI_MAP_H

#define LV2_URI_MAP_URI    "http://lv2plug.in/ns/ext/uri-map"  ///< http://lv2plug.in/ns/ext/uri-map
#define LV2_URI_MAP_PREFIX LV2_URI_MAP_URI "#"                 ///< http://lv2plug.in/ns/ext/uri-map#

#include "lv2/core/attributes.h"

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

LV2_DISABLE_DEPRECATION_WARNINGS

/**
   Opaque pointer to host data.
*/
LV2_DEPRECATED
typedef void* LV2_URI_Map_Callback_Data;

/**
   URI Map Feature.

   To support this feature the host must pass an LV2_Feature struct to the
   plugin's instantiate method with URI "http://lv2plug.in/ns/ext/uri-map"
   and data pointed to an instance of this struct.
*/
LV2_DEPRECATED
typedef struct {
	/**
	   Opaque pointer to host data.

	   The plugin MUST pass this to any call to functions in this struct.
	   Otherwise, it must not be interpreted in any way.
	*/
	LV2_URI_Map_Callback_Data callback_data;

	/**
	   Get the numeric ID of a URI from the host.

	   @param callback_data Must be the callback_data member of this struct.
	   @param map The 'context' of this URI. Certain extensions may define a
	   URI that must be passed here with certain restrictions on the return
	   value (e.g. limited range). This value may be NULL if the plugin needs
	   an ID for a URI in general. Extensions SHOULD NOT define a context
	   unless there is a specific need to do so, e.g. to restrict the range of
	   the returned value.
	   @param uri The URI to be mapped to an integer ID.

	   This function is referentially transparent; any number of calls with the
	   same arguments is guaranteed to return the same value over the life of a
	   plugin instance (though the same URI may return different values with a
	   different map parameter). However, this function is not necessarily very
	   fast: plugins SHOULD cache any IDs they might need in performance
	   critical situations.

	   The return value 0 is reserved and indicates that an ID for that URI
	   could not be created for whatever reason. Extensions MAY define more
	   precisely what this means in a certain context, but in general plugins
	   SHOULD handle this situation as gracefully as possible. However, hosts
	   SHOULD NOT return 0 from this function in non-exceptional circumstances
	   (e.g. the URI map SHOULD be dynamic). Hosts that statically support only
	   a fixed set of URIs should not expect plugins to function correctly.
	*/
	uint32_t (*uri_to_id)(LV2_URI_Map_Callback_Data callback_data,
	                      const char*               map,
	                      const char*               uri);
} LV2_URI_Map_Feature;

LV2_RESTORE_WARNINGS

#ifdef __cplusplus
}  /* extern "C" */
#endif

#endif /* LV2_URI_MAP_H */

/**
   @}
*/
