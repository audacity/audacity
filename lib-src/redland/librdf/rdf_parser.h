/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_parser.h - RDF Parser Factory / Parser interfaces and definition
 *
 * Copyright (C) 2000-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2000-2005, University of Bristol, UK http://www.bristol.ac.uk/
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



#ifndef LIBRDF_PARSER_H
#define LIBRDF_PARSER_H

#ifdef LIBRDF_INTERNAL
#include <rdf_parser_internal.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* class methods */
REDLAND_API
void librdf_parser_register_factory(librdf_world *world, const char *name, const char *label, const char *mime_type, const unsigned char *uri_string, void (*factory) (librdf_parser_factory*));

REDLAND_API
int librdf_parser_enumerate(librdf_world* world, const unsigned int counter, const char **name, const char **label);

/* constructor */
REDLAND_API
librdf_parser* librdf_new_parser(librdf_world* world, const char *name, const char *mime_type, librdf_uri *type_uri);
REDLAND_API
librdf_parser* librdf_new_parser_from_factory(librdf_world* world, librdf_parser_factory *factory);

/* destructor */
REDLAND_API
void librdf_free_parser(librdf_parser *parser);


/* methods */
REDLAND_API
librdf_stream* librdf_parser_parse_as_stream(librdf_parser* parser, librdf_uri* uri, librdf_uri* base_uri);
REDLAND_API
int librdf_parser_parse_into_model(librdf_parser* parser, librdf_uri* uri, librdf_uri* base_uri, librdf_model* model);
REDLAND_API
librdf_stream* librdf_parser_parse_string_as_stream(librdf_parser* parser, const unsigned char* string, librdf_uri* base_uri);
REDLAND_API
int librdf_parser_parse_string_into_model(librdf_parser* parser, const unsigned char *string, librdf_uri* base_uri, librdf_model* model);
REDLAND_API
librdf_stream* librdf_parser_parse_file_handle_as_stream(librdf_parser* parser, FILE* fh, int close_fh, librdf_uri* base_uri);
REDLAND_API
int librdf_parser_parse_file_handle_into_model(librdf_parser* parser, FILE *fh, int close_fh, librdf_uri* base_uri, librdf_model* model);
REDLAND_API REDLAND_DEPRECATED
void librdf_parser_set_error(librdf_parser* parser, void *user_data, void (*error_fn)(void *user_data, const char *msg, ...));
REDLAND_API REDLAND_DEPRECATED
void librdf_parser_set_warning(librdf_parser* parser, void *user_data, void (*warning_fn)(void *user_data, const char *msg, ...));
REDLAND_API
librdf_stream* librdf_parser_parse_counted_string_as_stream(librdf_parser* parser, const unsigned char *string, size_t length, librdf_uri* base_uri);
REDLAND_API
int librdf_parser_parse_counted_string_into_model(librdf_parser* parser, const unsigned char *string, size_t length, librdf_uri* base_uri, librdf_model* model);
REDLAND_API
void librdf_parser_set_uri_filter(librdf_parser* parser, librdf_uri_filter_func 
filter, void* user_data);
REDLAND_API
librdf_uri_filter_func librdf_parser_get_uri_filter(librdf_parser* parser, void** user_data_p);

/**
 * LIBRDF_PARSER_FEATURE_ERROR_COUNT:
 *
 * Parser feature URI string for getting the error count of the last parse.
 */
#define LIBRDF_PARSER_FEATURE_ERROR_COUNT "http://feature.librdf.org/parser-error-count"

/**
 * LIBRDF_PARSER_FEATURE_WARNING_COUNT:
 *
 * Parser feature URI string for getting the warning count of the last parse.
 */
#define LIBRDF_PARSER_FEATURE_WARNING_COUNT "http://feature.librdf.org/parser-warning-count"

REDLAND_API
librdf_node* librdf_parser_get_feature(librdf_parser* parser, librdf_uri *feature);
REDLAND_API
int librdf_parser_set_feature(librdf_parser* parser, librdf_uri* feature, librdf_node* value);
REDLAND_API
char* librdf_parser_get_accept_header(librdf_parser* parser);
REDLAND_API
const char* librdf_parser_guess_name(const char *mime_type, const unsigned char *buffer, const unsigned char *identifier);
REDLAND_API
const char* librdf_parser_get_namespaces_seen_prefix(librdf_parser* parser, int offset);
REDLAND_API
librdf_uri* librdf_parser_get_namespaces_seen_uri(librdf_parser* parser, int offset);
REDLAND_API
int librdf_parser_get_namespaces_seen_count(librdf_parser* parser);

#ifdef __cplusplus
}
#endif

#endif
