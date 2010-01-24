/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_stream.h - RDF Stream interface and definitions
 *
 * Copyright (C) 2000-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2000-2004, University of Bristol, UK http://www.bristol.ac.uk/
 * 
 * This package is Free Software and part of Redland http://librdf.org/
 * 
 * It is licensed under the following three licenses as alternatives:
 *   1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
 *   2. GNU General Public License (GPL) V2 or any newer version
 *   3. Apache License, V2.0 or any newer version
 * 
 * You may not use this file except in compliance with at least one of
 * the above three licenses.
 * 
 * See LICENSE.html or LICENSE.txt at the top of this package for the
 * complete terms and further detail along with the license texts for
 * the licenses in COPYING.LIB, COPYING and LICENSE-2.0.txt respectively.
 * 
 * 
 */


#ifndef LIBRDF_STREAM_H
#define LIBRDF_STREAM_H

#ifdef __cplusplus
extern "C" {
#endif

/**
 * librdf_stream_map_handler: 
 * @stream: Stream that this map is operating over.
 * @map_context: Map data context pointer.
 * @item: Pointer to the current item in the iteration.
 *
 * Map function for a #librdf_stream map operation.
 *
 * See librdf_stream_add_map().
 *
 * Returns: item in keep the iteration or NULL to remove it
 */
typedef librdf_statement* (*librdf_stream_map_handler)(librdf_stream *stream, void *map_context, librdf_statement *item);

/**
 * librdf_stream_map_free_context_handler:
 * @map_context: Map data context pointer.
 *
 * Free handler function for a #librdf_stream map operation.
 *
 * See librdf_stream_add_map().
 */
typedef void (*librdf_stream_map_free_context_handler)(void *map_context);

#ifdef LIBRDF_INTERNAL
#include <rdf_stream_internal.h>
#endif


/* constructor */

REDLAND_API
librdf_stream* librdf_new_stream(librdf_world *world, void* context, int (*is_end_method)(void*), int (*next_method)(void*), void* (*get_method)(void*, int), void (*finished_method)(void*));
REDLAND_API
librdf_stream* librdf_new_stream_from_node_iterator(librdf_iterator* iterator, librdf_statement* statement, librdf_statement_part field);

/* destructor */

REDLAND_API
void librdf_free_stream(librdf_stream* stream);

/* methods */
REDLAND_API
int librdf_stream_end(librdf_stream* stream);

REDLAND_API
int librdf_stream_next(librdf_stream* stream);
REDLAND_API
librdf_statement* librdf_stream_get_object(librdf_stream* stream);
REDLAND_API
void* librdf_stream_get_context(librdf_stream* stream);

REDLAND_API
int librdf_stream_add_map(librdf_stream* stream, librdf_stream_map_handler map_function, librdf_stream_map_free_context_handler free_context, void *map_context);

REDLAND_API
void librdf_stream_print(librdf_stream *stream, FILE *fh);

REDLAND_API
librdf_stream* librdf_new_empty_stream(librdf_world *world);

#ifdef __cplusplus
}
#endif

#endif
