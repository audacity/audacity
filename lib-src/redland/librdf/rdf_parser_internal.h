/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_parser_internal.h - Internal RDF Parser Factory / Parser definitions
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



#ifndef LIBRDF_PARSER_INTERNAL_H
#define LIBRDF_PARSER_INTERNAL_H


#ifdef __cplusplus
extern "C" {
#endif

struct librdf_parser_factory_s 
{
  struct librdf_parser_factory_s* next;
  /* syntax name - required */
  char *name;
  /* syntax label */
  char *label;
  /* handle this MIME type/ Internet Media Type - optional */
  char *mime_type;
  /* handles the syntax defined by this URI - optional */
  librdf_uri *type_uri;

  /* the rest of this structure is populated by the
     parser-specific register function */
  size_t  context_length;

  /* create a new parser */
  int (*init)(librdf_parser* parser, void *_context);

  /* destroy a parser */
  void (*terminate)(void *_context);

  /* get/set features of parser (think of Java properties) */
  librdf_node* (*get_feature)(void *_context, librdf_uri *feature);
  int (*set_feature)(void *_context, librdf_uri *feature, librdf_node *value);

  /* parsing methods - all are optional but the only
   * current implementation, raptor, implements all of them
   */
  librdf_stream* (*parse_uri_as_stream)(void *_context, librdf_uri *uri, librdf_uri* base_uri);
  int (*parse_uri_into_model)(void *_context, librdf_uri *uri, librdf_uri* base_uri, librdf_model *model);
  librdf_stream* (*parse_file_as_stream)(void *_context, librdf_uri *uri, librdf_uri *base_uri);
  int (*parse_file_into_model)(void *_context, librdf_uri *uri, librdf_uri *base_uri, librdf_model *model);
  int (*parse_string_into_model)(void *_context, const unsigned char *string, librdf_uri* base_uri, librdf_model *model);
  librdf_stream* (*parse_string_as_stream)(void *_context, const unsigned char *string, librdf_uri *base_uri);
  int (*parse_counted_string_into_model)(void *_context, const unsigned char *string, size_t length, librdf_uri* base_uri, librdf_model *model);
  librdf_stream* (*parse_counted_string_as_stream)(void *_context, const unsigned char *string, size_t length, librdf_uri *base_uri);
  char* (*get_accept_header)(void* context);
  const char* (*get_namespaces_seen_prefix)(void* context, int offset);
  librdf_uri* (*get_namespaces_seen_uri)(void* context, int offset);
  int (*get_namespaces_seen_count)(void* context);
  int (*parse_file_handle_into_model)(void *_context, FILE *fh, int close_fh, librdf_uri* base_uri, librdf_model *model);
  librdf_stream* (*parse_file_handle_as_stream)(void *_context, FILE *fh, int close_fh, librdf_uri *base_uri);
};


struct librdf_parser_s {
  librdf_world *world;
  
  void *context;

  librdf_parser_factory* factory;

  void* uri_filter_user_data;
  librdf_uri_filter_func uri_filter;
};

/* class methods */
librdf_parser_factory* librdf_get_parser_factory(librdf_world *world, const char *name, const char *mime_type, librdf_uri *type_uri);


/* module init */
void librdf_init_parser(librdf_world *world);
/* module finish */
void librdf_finish_parser(librdf_world *world);
                    
void librdf_parser_raptor_constructor(librdf_world* world);
void librdf_parser_raptor_destructor(void);


#ifdef __cplusplus
}
#endif

#endif
