/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_uri.h - RDF URI Definition
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



#ifndef LIBRDF_URI_H
#define LIBRDF_URI_H

#ifdef LIBRDF_INTERNAL
#include <rdf_uri_internal.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

/**
 * librdf_uri_filter_func:
 * @user_data: user data
 * @uri: #librdf_uri URI to check
 *
 * Callback function for librdf_parser_set_uri_filter()
 *
 * Return value: non-0 to filter the URI
*/
typedef int (*librdf_uri_filter_func)(void *user_data, librdf_uri* uri);


/* constructors */
REDLAND_API
librdf_uri* librdf_new_uri (librdf_world *world, const unsigned char *uri_string);
/* Create a new URI from an existing URI - CLONE */
REDLAND_API
librdf_uri* librdf_new_uri_from_uri (librdf_uri* old_uri);
/* Create a new URI from an existing URI and local name */
REDLAND_API
librdf_uri* librdf_new_uri_from_uri_local_name (librdf_uri* old_uri, const unsigned char *local_name);

/* destructor */
REDLAND_API
void librdf_free_uri(librdf_uri *uri);

/* methods */
REDLAND_API
unsigned char* librdf_uri_as_string (librdf_uri *uri);
REDLAND_API
unsigned char* librdf_uri_as_counted_string (librdf_uri *uri, size_t *len_p);
REDLAND_API
void librdf_uri_print (librdf_uri* uri, FILE *fh);
REDLAND_API
unsigned char* librdf_uri_to_string (librdf_uri* uri);
REDLAND_API
unsigned char* librdf_uri_to_counted_string (librdf_uri* uri, size_t* len_p);
REDLAND_API
int librdf_uri_equals(librdf_uri* first_uri, librdf_uri* second_uri);
REDLAND_API
int librdf_uri_compare(librdf_uri* uri1, librdf_uri* uri2);
REDLAND_API
int librdf_uri_is_file_uri(librdf_uri* uri);
REDLAND_API
const char* librdf_uri_to_filename(librdf_uri* uri);
REDLAND_API
librdf_uri* librdf_new_uri_normalised_to_base(const unsigned char *uri_string, librdf_uri* source_uri, librdf_uri* base_uri);
REDLAND_API
librdf_uri* librdf_new_uri_relative_to_base(librdf_uri* base_uri, const unsigned char *uri_string);
REDLAND_API
librdf_uri* librdf_new_uri_from_filename(librdf_world* world, const char *filename);

#ifdef __cplusplus
}
#endif

#endif
