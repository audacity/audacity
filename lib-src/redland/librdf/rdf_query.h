/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_query.h - RDF Query Adaptor Factory and Query interfaces and definitions
 *
 * Copyright (C) 2002-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2002-2005, University of Bristol, UK http://www.bristol.ac.uk/
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


#ifndef LIBRDF_QUERY_H
#define LIBRDF_QUERY_H

#ifdef LIBRDF_INTERNAL
#include <rdf_query_internal.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* class methods */
REDLAND_API
void librdf_query_register_factory(librdf_world *world, const char *name, const unsigned char *uri_string, void (*factory) (librdf_query_factory*));

/* constructor */
REDLAND_API
librdf_query* librdf_new_query(librdf_world* world, const char *name, librdf_uri* uri, const unsigned char *query_string, librdf_uri* base_uri);
REDLAND_API
librdf_query* librdf_new_query_from_query (librdf_query* old_query);
REDLAND_API
librdf_query* librdf_new_query_from_factory(librdf_world* world, librdf_query_factory* factory, const char *name, librdf_uri* uri, const unsigned char* query_string, librdf_uri* base_uri);

/* destructor */
REDLAND_API
void librdf_free_query(librdf_query *query);


/* methods */
REDLAND_API
librdf_query_results* librdf_query_execute(librdf_query* query, librdf_model *model);
REDLAND_API
int librdf_query_get_limit(librdf_query *query);
REDLAND_API
int librdf_query_set_limit(librdf_query *query, int limit);
REDLAND_API
int librdf_query_get_offset(librdf_query *query);
REDLAND_API
int librdf_query_set_offset(librdf_query *query, int offset);

REDLAND_API
librdf_stream* librdf_query_results_as_stream(librdf_query_results* query_results);

REDLAND_API
int librdf_query_results_get_count(librdf_query_results* query_results);
REDLAND_API
int librdf_query_results_next(librdf_query_results* query_results);
REDLAND_API
int librdf_query_results_finished(librdf_query_results* query_results);

REDLAND_API
int librdf_query_results_get_bindings(librdf_query_results* query_results, const char ***names, librdf_node **values);
REDLAND_API
librdf_node* librdf_query_results_get_binding_value(librdf_query_results* query_results, int offset);
REDLAND_API
const char* librdf_query_results_get_binding_name(librdf_query_results* query_results, int offset);
REDLAND_API
librdf_node* librdf_query_results_get_binding_value_by_name(librdf_query_results* query_results, const char *name);
REDLAND_API
int librdf_query_results_get_bindings_count(librdf_query_results* query_results);
REDLAND_API
unsigned char* librdf_query_results_to_counted_string(librdf_query_results *query_results, librdf_uri *format_uri, librdf_uri *base_uri, size_t *length_p);
REDLAND_API
unsigned char* librdf_query_results_to_string(librdf_query_results *query_results, librdf_uri *format_uri, librdf_uri *base_uri);
REDLAND_API
int librdf_query_results_to_file_handle(librdf_query_results *query_results, FILE *handle, librdf_uri *format_uri, librdf_uri *base_uri);
REDLAND_API
int librdf_query_results_to_file(librdf_query_results *query_results, const char *name, librdf_uri *format_uri, librdf_uri *base_uri);

REDLAND_API
void librdf_free_query_results(librdf_query_results* query_results);

REDLAND_API
int librdf_query_results_is_bindings(librdf_query_results *query_results);
REDLAND_API
int librdf_query_results_is_boolean(librdf_query_results *query_results);
REDLAND_API
int librdf_query_results_is_graph(librdf_query_results *query_results);
REDLAND_API
int librdf_query_results_is_syntax(librdf_query_results* query_results);

REDLAND_API
int librdf_query_results_get_boolean(librdf_query_results *query_results);

/* query results formatter class */
REDLAND_API
librdf_query_results_formatter* librdf_new_query_results_formatter(librdf_query_results* query_results, const char *name, librdf_uri* uri);
REDLAND_API
librdf_query_results_formatter* librdf_new_query_results_formatter_by_mime_type(librdf_query_results* query_results, const char *mime_type);
REDLAND_API
void librdf_free_query_results_formatter(librdf_query_results_formatter* formatter);
REDLAND_API
int librdf_query_results_formatter_write(raptor_iostream *iostr, librdf_query_results_formatter* formatter, librdf_query_results* results, librdf_uri *base_uri);

REDLAND_API
int librdf_query_results_formats_check(librdf_world* world, const char *name, librdf_uri* uri, const char *mime_type);
REDLAND_API
int librdf_query_results_formats_enumerate(librdf_world* world, const unsigned int counter, const char **name, const char **label, const unsigned char **uri_string, const char **mime_type);

#ifdef __cplusplus
}
#endif

#endif
