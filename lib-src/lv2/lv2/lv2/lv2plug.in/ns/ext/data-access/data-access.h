/*
  LV2 Data Access Extension
  Copyright 2008-2011 David Robillard <http://drobilla.net>

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
   @file data-access.h
   C header for the LV2 Extension Data extension
   <http://lv2plug.in/ns/ext/data-access>.

   This extension defines a method for (e.g.) plugin UIs to have (possibly
   marshalled) access to the extension_data function on a plugin instance.
*/

#ifndef LV2_DATA_ACCESS_H
#define LV2_DATA_ACCESS_H

#define LV2_DATA_ACCESS_URI "http://lv2plug.in/ns/ext/data-access"

#ifdef __cplusplus
extern "C" {
#endif

/**
   The data field of the LV2_Feature for this extension.

   To support this feature the host must pass an LV2_Feature struct to the
   instantiate method with URI "http://lv2plug.in/ns/ext/data-access"
   and data pointed to an instance of this struct.
*/
typedef struct {
	/**
	   A pointer to a method the UI can call to get data (of a type specified
	   by some other extension) from the plugin.

	   This call never is never guaranteed to return anything, UIs should
	   degrade gracefully if direct access to the plugin data is not possible
	   (in which case this function will return NULL).

	   This is for access to large data that can only possibly work if the UI
	   and plugin are running in the same process.  For all other things, use
	   the normal LV2 UI communication system.
	*/
	const void* (*data_access)(const char* uri);
} LV2_Extension_Data_Feature;

#ifdef __cplusplus
}  /* extern "C" */
#endif

#endif  /* LV2_DATA_ACCESS_H */
