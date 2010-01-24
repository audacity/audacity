/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_serializer_internal.h - Internal RDF Serializer definitions
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



#ifndef LIBRDF_SERIALIZER_INTERNAL_H
#define LIBRDF_SERIALIZER_INTERNAL_H


#ifdef __cplusplus
extern "C" {
#endif

#include <raptor.h>

struct librdf_serializer_factory_s 
{
  struct librdf_serializer_factory_s* next;

  /* factory name - required */
  char *name;

  /* factory label */
  char *label;

  /* serialize to this MIME type/ Internet Media Type - optional */
  char *mime_type;

  /* writes the syntax defined by this URI - optional */
  librdf_uri *type_uri;

  /* the rest of this structure is populated by the
     serializer-specific register function */
  size_t  context_length;

  /* create a new serializer */
  int (*init)(librdf_serializer* serializer, void *_context);

  /* destroy a serializer */
  void (*terminate)(void *_context);

  /* get/set features of serializer */
  librdf_node* (*get_feature)(void *_context, librdf_uri* feature);
  int (*set_feature)(void *_context, librdf_uri *feature, librdf_node* value);

  int (*set_namespace)(void *_context, librdf_uri *uri, const char *prefix);

  int (*serialize_stream_to_file_handle)(void *_context, FILE *handle, librdf_uri* base_uri, librdf_stream *stream);
  
  int (*serialize_model_to_file_handle)(void *_context, FILE *handle, librdf_uri* base_uri, librdf_model *model);

  unsigned char* (*serialize_stream_to_counted_string)(void *_context, librdf_uri* base_uri, librdf_stream *stream, size_t *length_p);

  unsigned char* (*serialize_model_to_counted_string)(void *_context, librdf_uri* base_uri, librdf_model *model, size_t *length_p);

  int (*serialize_stream_to_iostream)(void *context, librdf_uri* base_uri, librdf_stream *stream, raptor_iostream* iostr);

  int (*serialize_model_to_iostream)(void *context, librdf_uri* base_uri, librdf_model *model, raptor_iostream* iostr);
};


struct librdf_serializer_s {
  librdf_world *world;
  
  void *context;

  void *error_user_data;
  void *warning_user_data;
  void (*error_fn)(void *user_data, const char *msg, ...);
  void (*warning_fn)(void *user_data, const char *msg, ...);

  librdf_serializer_factory* factory;
};

/* class methods */
librdf_serializer_factory* librdf_get_serializer_factory(librdf_world *world, const char *name, const char *mime_type, librdf_uri *type_uri);


/* module init */
void librdf_init_serializer(librdf_world *world);
/* module finish */
void librdf_finish_serializer(librdf_world *world);
                    
void librdf_serializer_raptor_constructor(librdf_world* world);
void librdf_serializer_rdfxml_constructor(librdf_world* world);


#ifdef __cplusplus
}
#endif

#endif
