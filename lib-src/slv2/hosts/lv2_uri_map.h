/* lv2_uri_map.h - C header file for the LV2 URI Map extension.
 * 
 * Copyright (C) 2008 Dave Robillard <dave@drobilla.net>
 * 
 * This header is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This header is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this header; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 01222-1307 USA
 */

#ifndef LV2_URI_MAP_H
#define LV2_URI_MAP_H

#define LV2_URI_MAP_URI "http://lv2plug.in/ns/ext/uri-map"

#include <stdint.h>

/** @file
 * This header defines the LV2 URI Map extension with the URI
 * <http://lv2plug.in/ns/ext/uri-map> (preferred prefix 'lv2urimap').
 *
 * This extension defines a simple mechanism for plugins to map URIs to
 * integers, usually for performance reasons (e.g. processing events
 * typed by URIs in real time).  The expected use case is for plugins to
 * map URIs to integers for things they 'understand' at instantiation time,
 * and store those values for use in the audio thread without doing any string
 * comparison.  This allows the extensibility of RDF with the performance of
 * integers (or centrally defined enumerations).
 */
	

/** Opaque pointer to host data. */
typedef void* LV2_URI_Map_Callback_Data;


/** The data field of the LV2_Feature for this extension.
 *
 * To support this feature the host must pass an LV2_Feature struct to the
 * plugin's instantiate method with URI "http://lv2plug.in/ns/ext/uri-map"
 * and data pointed to an instance of this struct.
 */
typedef struct {
	
	/** Opaque pointer to host data.
	 *
	 * The plugin MUST pass this to any call to functions in this struct.
	 * Otherwise, it must not be interpreted in any way.
	 */
	LV2_URI_Map_Callback_Data callback_data;
	
	/** Get the numeric ID of a URI from the host.
	 *
	 * @param callback_data Must be the callback_data member of this struct.
	 * @param map The 'context' of this URI.  Certain extensions may define a
	 *        URI that must be passed here with certain restrictions on the
	 *        return value (e.g. limited range).  This value may be NULL if
	 *        the plugin needs an ID for a URI in general.
	 * @param uri The URI to be mapped to an integer ID.
	 *
	 * This function is referentially transparent - any number of calls with
	 * the same arguments is guaranteed to return the same value over the life
	 * of a plugin instance (though the same URI may return different values
	 * with a different map parameter).  However, this function is not
	 * necessarily very fast: plugins should cache any IDs they might need in
	 * performance critical situations.
	 * The return value 0 is reserved and means an ID for that URI could not
	 * be created for whatever reason.  Extensions may define more precisely
	 * what this means, but in general plugins should gracefully handle 0
	 * and consider whatever they wanted the URI for "unsupported".
	 */
	uint32_t (*uri_to_id)(LV2_URI_Map_Callback_Data callback_data,
	                      const char*               map,
	                      const char*               uri);

} LV2_URI_Map_Feature;


#endif // LV2_URI_MAP_H

