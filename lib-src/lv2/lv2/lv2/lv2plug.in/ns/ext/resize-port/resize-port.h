/*
  Copyright 2007-2012 David Robillard <http://drobilla.net>

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

#ifndef LV2_RESIZE_PORT_H
#define LV2_RESIZE_PORT_H

#include <stddef.h>
#include <stdint.h>

#define LV2_RESIZE_PORT_URI    "http://lv2plug.in/ns/ext/resize-port"
#define LV2_RESIZE_PORT_PREFIX LV2_RESIZE_PORT_URI "#"

#define LV2_RESIZE_PORT__asLargeAs   LV2_RESIZE_PORT_PREFIX "asLargeAs"
#define LV2_RESIZE_PORT__minimumSize LV2_RESIZE_PORT_PREFIX "minimumSize"
#define LV2_RESIZE_PORT__resize      LV2_RESIZE_PORT_PREFIX "resize"

#ifdef __cplusplus
extern "C" {
#else
#    include <stdbool.h>
#endif

/** A status code for state functions. */
typedef enum {
	LV2_RESIZE_PORT_SUCCESS      = 0,  /**< Completed successfully. */
	LV2_RESIZE_PORT_ERR_UNKNOWN  = 1,  /**< Unknown error. */
	LV2_RESIZE_PORT_ERR_NO_SPACE = 2   /**< Insufficient space. */
} LV2_Resize_Port_Status;

typedef void* LV2_Resize_Port_Feature_Data;

typedef struct {
	LV2_Resize_Port_Feature_Data data;

	/**
	   Resize a port buffer to at least @a size bytes.
	 
	   This function MAY return an error, in which case the port buffer was not
	   resized and the port is still connected to the same location.  Plugins
	   MUST gracefully handle this situation.
	 
	   This function is in the audio threading class.
	 
	   The host MUST preserve the contents of the port buffer when resizing.
	 
	   Plugins MAY resize a port many times in a single run callback.  Hosts
	   SHOULD make this as inexpensive as possible.
	*/
	LV2_Resize_Port_Status (*resize)(LV2_Resize_Port_Feature_Data data,
	                                 uint32_t                     index,
	                                 size_t                       size);
} LV2_Resize_Port_Resize;

#ifdef __cplusplus
}  /* extern "C" */
#endif

#endif  /* LV2_RESIZE_PORT_H */

