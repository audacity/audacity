/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_serializer_raptor.c - RDF Serializers via Raptor
 *
 * Copyright (C) 2002-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2002-2004, University of Bristol, UK http://www.bristol.ac.uk/
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


#ifdef HAVE_CONFIG_H
#include <rdf_config.h>
#endif

#ifdef WIN32
#include <win32_rdf_config.h>
#endif

#include <stdio.h>
#include <string.h>

#include <redland.h>


typedef struct {
  librdf_serializer *serializer;        /* librdf serializer object */
  raptor_serializer *rdf_serializer;    /* raptor serializer object */
  char *serializer_name;                /* raptor serializer name to use */

  int errors;
  int warnings;
} librdf_serializer_raptor_context;


/**
 * librdf_serializer_raptor_init:
 * @serializer: the serializer
 * @context: context
 *
 * Initialise the N-Triples RDF serializer.
 * 
 * Return value: non 0 on failure
 **/
static int
librdf_serializer_raptor_init(librdf_serializer *serializer, void *context) 
{
  librdf_serializer_raptor_context* scontext=(librdf_serializer_raptor_context*)context;

  scontext->serializer = serializer;
  scontext->serializer_name=scontext->serializer->factory->name;

  scontext->rdf_serializer=raptor_new_serializer(scontext->serializer_name);
  if(!scontext->rdf_serializer)
    return 1;

  return 0;
}


/**
 * librdf_serializer_raptor_terminate:
 * @context: context
 *
 * Terminate the raptor RDF serializer.
 *
 **/
static void
librdf_serializer_raptor_terminate(void *context) 
{
  librdf_serializer_raptor_context* scontext=(librdf_serializer_raptor_context*)context;
  
  if(scontext->rdf_serializer)
    raptor_free_serializer(scontext->rdf_serializer);
}


/**
 * librdf_serializer_raptor_get_feature:
 * @context: context
 * @feature: #librdf_uri of feature
 *
 * Get a raptor parser feature.
 *
 * Return value: new #librdf_node value or NULL on failure
 **/
static librdf_node*
librdf_serializer_raptor_get_feature(void *context, librdf_uri* feature) {
  librdf_serializer_raptor_context* scontext=(librdf_serializer_raptor_context*)context;
  unsigned char intbuffer[20]; /* FIXME */
  unsigned char *uri_string;
  raptor_feature feature_i;
  
  if(!feature)
    return NULL;

  uri_string=librdf_uri_as_string(feature);
  if(!uri_string)
    return NULL;
  
  feature_i=raptor_feature_from_uri((raptor_uri*)feature);
  if((int)feature_i >= 0) {
    int value=raptor_serializer_get_feature(scontext->rdf_serializer, feature_i);
    sprintf((char*)intbuffer, "%d", value);
    return librdf_new_node_from_typed_literal(scontext->serializer->world,
                                              intbuffer, NULL, NULL);
  }
  
  return NULL;
}


static int
librdf_serializer_raptor_set_feature(void *context, 
                                     librdf_uri *feature, librdf_node* value) 
{
  librdf_serializer_raptor_context* scontext=(librdf_serializer_raptor_context*)context;
  raptor_feature feature_i;
  const unsigned char* value_s;
  
  if(!feature)
    return 1;

  /* try a raptor feature */
  feature_i=raptor_feature_from_uri((raptor_uri*)feature);
  if((int)feature_i < 0)
    return 1;
  
  if(!librdf_node_is_literal(value))
    return 1;
  
  value_s=(const unsigned char*)librdf_node_get_literal_value(value);

  return raptor_serializer_set_feature_string(scontext->rdf_serializer,
                                              feature_i, value_s);
}


static int
librdf_serializer_raptor_set_namespace(void* context,
                                       librdf_uri *uri, const char *prefix) 
{
  librdf_serializer_raptor_context* scontext=(librdf_serializer_raptor_context*)context;
  return raptor_serialize_set_namespace(scontext->rdf_serializer, 
                                        (raptor_uri*)uri, (const unsigned char*)prefix);
}
  

static int
librdf_serializer_raptor_serialize_statement(raptor_serializer *rserializer,
                                             librdf_statement* statement)
{
  raptor_statement rstatement;
  librdf_node *subject=librdf_statement_get_subject(statement);
  librdf_node *predicate=librdf_statement_get_predicate(statement);
  librdf_node *object=librdf_statement_get_object(statement);

  if(librdf_node_is_blank(subject)) {
    rstatement.subject=librdf_node_get_blank_identifier(subject);
    rstatement.subject_type=RAPTOR_IDENTIFIER_TYPE_ANONYMOUS;
  } else if(librdf_node_is_resource(subject)) {
    rstatement.subject=(raptor_uri*)librdf_node_get_uri(subject);
    rstatement.subject_type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
  } else {
    librdf_log(statement->world,
               0, LIBRDF_LOG_ERROR, LIBRDF_FROM_SERIALIZER, NULL,
               "Do not know how to serialize triple subject type %d",
               librdf_node_get_type(subject));
    return 1;
  }

  if(!librdf_node_is_resource(predicate)) {
    librdf_log(statement->world,
               0, LIBRDF_LOG_ERROR, LIBRDF_FROM_SERIALIZER, NULL,
               "Do not know how to serialize triple predicate type %d",
               librdf_node_get_type(predicate));
    return 1;
  }

  rstatement.predicate=(raptor_uri*)librdf_node_get_uri(predicate);
  rstatement.predicate_type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;

  rstatement.object_literal_language=NULL;
  rstatement.object_literal_datatype=NULL;
  switch(librdf_node_get_type(object)) {
    case LIBRDF_NODE_TYPE_LITERAL:
      rstatement.object=librdf_node_get_literal_value(object);
      rstatement.object_type=RAPTOR_IDENTIFIER_TYPE_LITERAL;
      
      rstatement.object_literal_language=(const unsigned char*)librdf_node_get_literal_value_language(object);
      rstatement.object_literal_datatype=(raptor_uri*)librdf_node_get_literal_value_datatype_uri(object);
      break;

    case LIBRDF_NODE_TYPE_BLANK:
      rstatement.object=librdf_node_get_blank_identifier(object);
      rstatement.object_type=RAPTOR_IDENTIFIER_TYPE_ANONYMOUS;
      break;

    case LIBRDF_NODE_TYPE_RESOURCE:
      rstatement.object=librdf_node_get_uri(object);
      rstatement.object_type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
      break;

    case LIBRDF_NODE_TYPE_UNKNOWN:
    default:
      librdf_log(statement->world,
                 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_SERIALIZER, NULL,
                 "Do not know how to serialize triple object type %d",
                 librdf_node_get_type(object));
      return 1;
  }

  return raptor_serialize_statement(rserializer, &rstatement);
}


static void
librdf_serializer_raptor_error_handler(void *data, raptor_locator *locator,
                                       const char *message) 
{
  librdf_serializer_raptor_context* scontext=(librdf_serializer_raptor_context*)data;
  scontext->errors++;

  librdf_log_simple(scontext->serializer->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_SERIALIZER, locator, message);
}


static void
librdf_serializer_raptor_warning_handler(void *data, raptor_locator *locator,
                                         const char *message) 
{
  librdf_serializer_raptor_context* scontext=(librdf_serializer_raptor_context*)data;
  scontext->warnings++;

  librdf_log_simple(scontext->serializer->world, 0, LIBRDF_LOG_WARN, LIBRDF_FROM_SERIALIZER, locator, message);
}


static int
librdf_serializer_raptor_serialize_stream_to_file_handle(void *context,
                                                         FILE *handle, 
                                                         librdf_uri* base_uri,
                                                         librdf_stream *stream) 
{
  librdf_serializer_raptor_context* scontext=(librdf_serializer_raptor_context*)context;
  int rc=0;

  if(!stream)
    return 1;

  /* start the serialize */
  rc=raptor_serialize_start_to_file_handle(scontext->rdf_serializer, 
                                           (raptor_uri*)base_uri, handle);
  if(rc) {
    /* free up resources on error */
    raptor_serialize_end(scontext->rdf_serializer);
    return 1;
  }

  scontext->errors=0;
  scontext->warnings=0;
  raptor_serializer_set_error_handler(scontext->rdf_serializer, scontext, 
                                      librdf_serializer_raptor_error_handler);
  raptor_serializer_set_warning_handler(scontext->rdf_serializer, scontext, 
                                        librdf_serializer_raptor_warning_handler);

  rc=0;
  while(!librdf_stream_end(stream)) {
    librdf_statement *statement=librdf_stream_get_object(stream);
    rc=librdf_serializer_raptor_serialize_statement(scontext->rdf_serializer, 
                                                    statement);
    if(rc)
      break;
    librdf_stream_next(stream);
  }
  raptor_serialize_end(scontext->rdf_serializer);

  return rc;
}



static int
librdf_serializer_raptor_serialize_model_to_file_handle(void *context,
                                                        FILE *handle, 
                                                        librdf_uri* base_uri,
                                                        librdf_model *model) 
{
  int rc;
  librdf_stream *stream;

  stream=librdf_model_as_stream(model);
  if(!stream)
    return 1;
  rc=librdf_serializer_raptor_serialize_stream_to_file_handle(context, handle,
                                                              base_uri, stream);
  librdf_free_stream(stream);

  return rc;
}



static unsigned char*
librdf_serializer_raptor_serialize_stream_to_counted_string(void *context,
                                                           librdf_uri* base_uri,
                                                           librdf_stream *stream,
                                                           size_t* length_p)
{
  librdf_serializer_raptor_context* scontext=(librdf_serializer_raptor_context*)context;
  raptor_iostream *iostr;
  void *string=NULL;
  size_t string_length=0;
  int rc=0;

  if(!stream)
    return NULL;

  /* start the serialize */
  iostr=raptor_new_iostream_to_string(&string, &string_length,
                                      malloc);
  if(!iostr) {
    free(string);
    return NULL;
  }
    
  rc=raptor_serialize_start(scontext->rdf_serializer, 
                            (raptor_uri*)base_uri, iostr);

  if(rc) {
    raptor_free_iostream(iostr);
    free(string);
    return NULL;
  }
    
  scontext->errors=0;
  scontext->warnings=0;
  raptor_serializer_set_error_handler(scontext->rdf_serializer, scontext, 
                                      librdf_serializer_raptor_error_handler);
  raptor_serializer_set_warning_handler(scontext->rdf_serializer, scontext, 
                                        librdf_serializer_raptor_warning_handler);

  rc=0;    
  while(!librdf_stream_end(stream)) {
    librdf_statement *statement=librdf_stream_get_object(stream);
    rc=librdf_serializer_raptor_serialize_statement(scontext->rdf_serializer, 
                                                    statement);
    if(rc)
      break;
    librdf_stream_next(stream);
  }
  raptor_serialize_end(scontext->rdf_serializer);

  if(rc) {
    free(string);
    return NULL;
  }

  if(length_p)
    *length_p=string_length;
  
  return (unsigned char *)string;
}


static unsigned char*
librdf_serializer_raptor_serialize_model_to_counted_string(void *context,
                                                           librdf_uri* base_uri,
                                                           librdf_model *model,
                                                           size_t* length_p)
{
  unsigned char *string=NULL;
  librdf_stream *stream;

  stream=librdf_model_as_stream(model);
  if(!stream)
    return NULL;

  string=librdf_serializer_raptor_serialize_stream_to_counted_string(context,
                                                                     base_uri,
                                                                     stream,
                                                                     length_p);
  librdf_free_stream(stream);
  
  return string;
}


static int
librdf_serializer_raptor_serialize_stream_to_iostream(void *context,
                                                     librdf_uri* base_uri,
                                                     librdf_stream *stream,
                                                     raptor_iostream* iostr)
{
  librdf_serializer_raptor_context* scontext=(librdf_serializer_raptor_context*)context;
  int rc=0;
  
  if(!iostr)
    return 1;
  
  if(!stream)
    return 1;

  /* start the serialize */
  rc=raptor_serialize_start(scontext->rdf_serializer, 
                            (raptor_uri*)base_uri, iostr);

  if(rc) {
    raptor_free_iostream(iostr);
    return 1;
  }
    
  scontext->errors=0;
  scontext->warnings=0;
  raptor_serializer_set_error_handler(scontext->rdf_serializer, scontext, 
                                      librdf_serializer_raptor_error_handler);
  raptor_serializer_set_warning_handler(scontext->rdf_serializer, scontext, 
                                        librdf_serializer_raptor_warning_handler);

  rc=0;
  while(!librdf_stream_end(stream)) {
    librdf_statement *statement=librdf_stream_get_object(stream);
    rc=librdf_serializer_raptor_serialize_statement(scontext->rdf_serializer, 
                                                    statement);
    if(rc)
      break;
    librdf_stream_next(stream);
  }
  raptor_serialize_end(scontext->rdf_serializer);

  return rc;
}


static int
librdf_serializer_raptor_serialize_model_to_iostream(void *context,
                                                     librdf_uri* base_uri,
                                                     librdf_model *model,
                                                     raptor_iostream* iostr)
{
  int rc=0;
  librdf_stream *stream;
  
  if(!iostr)
    return 1;
  
  stream=librdf_model_as_stream(model);
  if(!stream)
    return 1;
  rc=librdf_serializer_raptor_serialize_stream_to_iostream(context,
                                                           base_uri,
                                                           stream, iostr);
  librdf_free_stream(stream);

  return rc;
}


/**
 * librdf_serializer_raptor_register_factory:
 * @factory: factory
 *
 * Register the N-riples serializer with the RDF serializer factory.
 * 
 **/
static void
librdf_serializer_raptor_register_factory(librdf_serializer_factory *factory) 
{
  factory->context_length = sizeof(librdf_serializer_raptor_context);
  
  factory->init  = librdf_serializer_raptor_init;
  factory->terminate = librdf_serializer_raptor_terminate;

  factory->get_feature = librdf_serializer_raptor_get_feature;
  factory->set_feature = librdf_serializer_raptor_set_feature;
  factory->set_namespace = librdf_serializer_raptor_set_namespace;

  factory->serialize_stream_to_file_handle= librdf_serializer_raptor_serialize_stream_to_file_handle;
  factory->serialize_model_to_file_handle = librdf_serializer_raptor_serialize_model_to_file_handle;
  factory->serialize_stream_to_counted_string = librdf_serializer_raptor_serialize_stream_to_counted_string;
  factory->serialize_model_to_counted_string = librdf_serializer_raptor_serialize_model_to_counted_string;
  factory->serialize_stream_to_iostream = librdf_serializer_raptor_serialize_stream_to_iostream;
  factory->serialize_model_to_iostream = librdf_serializer_raptor_serialize_model_to_iostream;
}


/**
 * librdf_serializer_raptor_constructor:
 * @world: redland world object
 *
 * INTERNAL - Initialise the serializer_raptor module.
 *
 **/
void
librdf_serializer_raptor_constructor(librdf_world *world)
{
  int i;
  
  /* enumerate from serializer 1, so the default serializer 0 is done last */
  for(i=1; 1; i++) {
    const char *syntax_name=NULL;
    const char *syntax_label=NULL;
    const char *mime_type=NULL;
    const unsigned char *uri_string=NULL;

    if(raptor_serializers_enumerate(i, &syntax_name, &syntax_label, 
                                    &mime_type, &uri_string)) {
      /* reached the end of the serializers, now register the default one */
      i=0;
      raptor_serializers_enumerate(i, &syntax_name, &syntax_label,
                                   &mime_type, &uri_string);
    }
    
    librdf_serializer_register_factory(world, syntax_name, syntax_label,
                                       mime_type, uri_string,
                                       &librdf_serializer_raptor_register_factory);

    if(!i) /* registered default serializer, end */
      break;
  }
}
