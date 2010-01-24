/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_serialize_json.c - JSON serializers
 *
 * Copyright (C) 2008, David Beckett http://www.dajobe.org/
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
 */

#ifdef HAVE_CONFIG_H
#include <raptor_config.h>
#endif

#ifdef WIN32
#include <win32_raptor_config.h>
#endif


#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

/* Raptor includes */
#include "raptor.h"
#include "raptor_internal.h"


/*
 * Raptor JSON serializer object
 */
typedef struct {
  /* non-0 if json-r otherwise json-t */
  int is_resource;

  int need_subject_comma;

  /* JSON writer object */
  raptor_json_writer* json_writer;

  /* Ordered sequence of triples if is_resource */
  raptor_avltree* avltree;

  /* Last statement generated if is_resource (shared pointer) */
  raptor_statement* last_statement;

  int need_object_comma;

} raptor_json_context;


static int raptor_json_serialize_init(raptor_serializer* serializer,
                                      const char *name);
static void raptor_json_serialize_terminate(raptor_serializer* serializer);
static int raptor_json_serialize_start(raptor_serializer* serializer);
static int raptor_json_serialize_statement(raptor_serializer* serializer, 
                                           const raptor_statement *statement);
static int raptor_json_serialize_end(raptor_serializer* serializer);
static void raptor_json_serialize_finish_factory(raptor_serializer_factory* factory);


/*
 * raptor serializer JSON implementation
 */


/* create a new serializer */
static int
raptor_json_serialize_init(raptor_serializer* serializer, const char *name)
{
  raptor_json_context* context=(raptor_json_context*)serializer->context;

  context->is_resource=!strcmp(name,"json");

  /* Default for JSON serializer is absolute URIs */
  serializer->feature_relative_uris=0;
  
  return 0;
}


/* destroy a serializer */
static void
raptor_json_serialize_terminate(raptor_serializer* serializer)
{
  raptor_json_context* context=(raptor_json_context*)serializer->context;

  if(context->json_writer) {
    raptor_free_json_writer(context->json_writer);
    context->json_writer=NULL;
  }

  if(context->avltree) {
    raptor_free_avltree(context->avltree);
    context->avltree=NULL;
  }
}


static int
raptor_json_serialize_start(raptor_serializer* serializer)
{
  raptor_json_context* context=(raptor_json_context*)serializer->context;
  const raptor_uri_handler *uri_handler;
  void *uri_context;
  raptor_uri* base_uri;
  
  raptor_uri_get_handler(&uri_handler, &uri_context);

  base_uri=(serializer->feature_relative_uris) ? serializer->base_uri : NULL;
  
  context->json_writer=raptor_new_json_writer(base_uri,
                                              uri_handler, uri_context,
                                              serializer->iostream,
                                              (raptor_simple_message_handler)raptor_serializer_simple_error,
                                              serializer);
  if(!context->json_writer)
    return 1;

  if(context->is_resource) {
    context->avltree=raptor_new_avltree((raptor_data_compare_function)raptor_statement_compare,
                                        (raptor_data_free_function)raptor_free_statement,
                                        0);
    if(!context->avltree) {
      raptor_free_json_writer(context->json_writer);
      context->json_writer=NULL;
      return 1;
    }
  }

  /* start callback */
  if(serializer->feature_json_callback) {
    raptor_iostream_write_string(serializer->iostream,
                                 serializer->feature_json_callback);
    raptor_iostream_write_byte(serializer->iostream, '(');
  }

  if(!context->is_resource) {
    /* start outer object */
    raptor_json_writer_start_block(context->json_writer, '{');
    raptor_json_writer_newline(context->json_writer);

    /* start triples array */
    raptor_iostream_write_counted_string(serializer->iostream,
                                         (const unsigned char*)"\"triples\" : ", 12);
    raptor_json_writer_start_block(context->json_writer, '[');
    raptor_json_writer_newline(context->json_writer);
  }
  
  return 0;
}


static int
raptor_json_serialize_statement(raptor_serializer* serializer, 
                                const raptor_statement *statement)
{
  raptor_json_context* context=(raptor_json_context*)serializer->context;

  if(context->is_resource) {
    raptor_statement* s=raptor_statement_copy(statement);
    if(!s)
      return 1;
    return raptor_avltree_add(context->avltree, s);
  }

  if(context->need_subject_comma) {
    raptor_iostream_write_byte(serializer->iostream, ',');
    raptor_json_writer_newline(context->json_writer);
  }

  /* start triple */
  raptor_json_writer_start_block(context->json_writer, '{');
  raptor_json_writer_newline(context->json_writer);

  /* subject */
  raptor_iostream_write_string(serializer->iostream,
                               (const unsigned char*)"\"subject\" : ");
  switch(statement->subject_type) {
    case RAPTOR_IDENTIFIER_TYPE_RESOURCE:
    case RAPTOR_IDENTIFIER_TYPE_PREDICATE:
      raptor_json_writer_uri_object(context->json_writer,
                                    (raptor_uri*)statement->subject);
      break;
          
    case RAPTOR_IDENTIFIER_TYPE_ANONYMOUS:
      raptor_json_writer_blank_object(context->json_writer,
                                      (const char*)statement->subject);
      break;

    case RAPTOR_IDENTIFIER_TYPE_LITERAL:
    case RAPTOR_IDENTIFIER_TYPE_XML_LITERAL:
    case RAPTOR_IDENTIFIER_TYPE_ORDINAL:
    case RAPTOR_IDENTIFIER_TYPE_UNKNOWN:
      default:
        RAPTOR_FATAL1("Unsupported identifier type\n");
        break;
  }
  raptor_iostream_write_byte(serializer->iostream, ',');
  raptor_json_writer_newline(context->json_writer);
  
  /* predicate */
  raptor_iostream_write_string(serializer->iostream,
                               (const unsigned char*)"\"predicate\" : ");
  raptor_json_writer_uri_object(context->json_writer,
                                (raptor_uri*)statement->predicate);
  raptor_iostream_write_byte(serializer->iostream, ',');
  raptor_json_writer_newline(context->json_writer);

  /* object */
  raptor_iostream_write_string(serializer->iostream,
                               (const unsigned char*)"\"object\" : ");
  switch(statement->object_type) {
    case RAPTOR_IDENTIFIER_TYPE_RESOURCE:
    case RAPTOR_IDENTIFIER_TYPE_PREDICATE:
      raptor_json_writer_uri_object(context->json_writer,
                                    (raptor_uri*)statement->object);
      break;
          
    case RAPTOR_IDENTIFIER_TYPE_LITERAL:
    case RAPTOR_IDENTIFIER_TYPE_XML_LITERAL:
      raptor_json_writer_literal_object(context->json_writer,
                                        (unsigned char*)statement->object,
                                        (unsigned char*)statement->object_literal_language, 
                                        statement->object_literal_datatype,
                                        "value", "type");
      break;

    case RAPTOR_IDENTIFIER_TYPE_ANONYMOUS:
      raptor_json_writer_blank_object(context->json_writer,
                                      (const char*)statement->object);
      break;

    case RAPTOR_IDENTIFIER_TYPE_ORDINAL:
    case RAPTOR_IDENTIFIER_TYPE_UNKNOWN:
      default:
        RAPTOR_FATAL1("Unsupported identifier type\n");
        break;
  }
  raptor_json_writer_newline(context->json_writer);

  /* end triple */
  raptor_json_writer_end_block(context->json_writer, '}');

  context->need_subject_comma=1;
  return 0;
}


/* return 0 to abort visit */
static int
raptor_json_serialize_avltree_visit(int depth, void* data, void *user_data)
{
  raptor_serializer* serializer=(raptor_serializer*)user_data;
  raptor_json_context* context=(raptor_json_context*)serializer->context;

  raptor_statement* statement=(raptor_statement*)data;
  int new_subject=0;
  int new_predicate=0;
  
  if(context->last_statement) {
    raptor_statement* s1=statement;
    raptor_statement* s2=context->last_statement;
    
    if(s1->subject_type != s2->subject_type) {
      new_subject=1;
    } else {
      /* subjects are URIs or blank nodes */
      if(s1->subject_type == RAPTOR_IDENTIFIER_TYPE_ANONYMOUS)
        new_subject=strcmp((char*)s1->subject, (char*)s2->subject);
      else
        new_subject=!raptor_uri_equals((raptor_uri*)s1->subject,
                                       (raptor_uri*)s2->subject);
    }

    if(new_subject) {
      /* end last predicate */
      raptor_json_writer_newline(context->json_writer);

      raptor_json_writer_end_block(context->json_writer, ']');
      raptor_json_writer_newline(context->json_writer);

      /* end last statement */
      raptor_json_writer_end_block(context->json_writer, '}');
      raptor_json_writer_newline(context->json_writer);

      context->need_subject_comma=1;
      context->need_object_comma=0;
    }
  } else
    new_subject=1;
  
  if(new_subject)  {
    if(context->need_subject_comma) {
      raptor_iostream_write_byte(serializer->iostream, ',');
      raptor_json_writer_newline(context->json_writer);
    }

    /* start triple */

    /* subject */
    switch(statement->subject_type) {
      case RAPTOR_IDENTIFIER_TYPE_RESOURCE:
      case RAPTOR_IDENTIFIER_TYPE_PREDICATE:
        raptor_json_writer_key_uri_value(context->json_writer, 
                                         NULL, 0,
                                         (raptor_uri*)statement->subject);
        break;
        
      case RAPTOR_IDENTIFIER_TYPE_ANONYMOUS:
        raptor_iostream_write_counted_string(serializer->iostream, "\"_:", 3);
        raptor_iostream_write_string_python(serializer->iostream,
                                            (const unsigned char*)statement->subject, 0, 
                                            '"', 2);
        raptor_iostream_write_byte(serializer->iostream, '"');
        break;
        
      case RAPTOR_IDENTIFIER_TYPE_LITERAL:
      case RAPTOR_IDENTIFIER_TYPE_XML_LITERAL:
      case RAPTOR_IDENTIFIER_TYPE_ORDINAL:
      case RAPTOR_IDENTIFIER_TYPE_UNKNOWN:
      default:
        RAPTOR_FATAL1("Unsupported identifier type\n");
        break;
    }

    raptor_iostream_write_counted_string(serializer->iostream, " : ", 3);
    raptor_json_writer_start_block(context->json_writer, '{');
  
    raptor_json_writer_newline(context->json_writer);
  }
  

  /* predicate */
  if(context->last_statement) {
    if(new_subject)
      new_predicate=1;
    else {
      new_predicate=!raptor_uri_equals((raptor_uri*)statement->predicate,
                                       (raptor_uri*)context->last_statement->predicate);
      if(new_predicate) {
        raptor_json_writer_newline(context->json_writer);
        raptor_json_writer_end_block(context->json_writer, ']');
        raptor_iostream_write_byte(serializer->iostream, ',');
        raptor_json_writer_newline(context->json_writer);
      }
    }
  } else
    new_predicate=1;

  if(new_predicate) {
    /* start predicate */

    raptor_json_writer_key_uri_value(context->json_writer, 
                                   NULL, 0,
                                   (raptor_uri*)statement->predicate);
    raptor_iostream_write_counted_string(serializer->iostream, " : ", 3);
    raptor_json_writer_start_block(context->json_writer, '[');
    raptor_iostream_write_byte(serializer->iostream, ' ');

    context->need_object_comma=0;
  }

  if(context->need_object_comma) {
    raptor_iostream_write_byte(serializer->iostream, ',');
    raptor_json_writer_newline(context->json_writer);
  }
  
  /* object */
  switch(statement->object_type) {
    case RAPTOR_IDENTIFIER_TYPE_RESOURCE:
    case RAPTOR_IDENTIFIER_TYPE_PREDICATE:
      raptor_json_writer_uri_object(context->json_writer,
                                    (raptor_uri*)statement->object);
      raptor_json_writer_newline(context->json_writer);
      break;
          
    case RAPTOR_IDENTIFIER_TYPE_LITERAL:
    case RAPTOR_IDENTIFIER_TYPE_XML_LITERAL:
      raptor_json_writer_literal_object(context->json_writer,
                                        (unsigned char*)statement->object,
                                        (unsigned char*)statement->object_literal_language, 
                                        statement->object_literal_datatype,
                                        "value", "type");
      break;

    case RAPTOR_IDENTIFIER_TYPE_ANONYMOUS:
      raptor_json_writer_blank_object(context->json_writer, 
                                      (const char*)statement->object);
      raptor_json_writer_newline(context->json_writer);
      break;

    case RAPTOR_IDENTIFIER_TYPE_ORDINAL:
    case RAPTOR_IDENTIFIER_TYPE_UNKNOWN:
      default:
        RAPTOR_FATAL1("Unsupported identifier type\n");
        break;
  }

  /* end triple */

  context->need_object_comma=1;
  context->last_statement=statement;

  return 1;
}


static int
raptor_json_serialize_end(raptor_serializer* serializer)
{
  raptor_json_context* context=(raptor_json_context*)serializer->context;

  raptor_json_writer_newline(context->json_writer);

  if(context->is_resource) {
    /* start outer object */
    raptor_json_writer_start_block(context->json_writer, '{');
    raptor_json_writer_newline(context->json_writer);
    
    raptor_avltree_visit(context->avltree,
                         raptor_json_serialize_avltree_visit,
                         serializer);

    /* end last triples block */
    if(context->last_statement) {
      raptor_json_writer_newline(context->json_writer);
      raptor_json_writer_end_block(context->json_writer, ']');
      raptor_json_writer_newline(context->json_writer);
      
      raptor_json_writer_end_block(context->json_writer, '}');
      raptor_json_writer_newline(context->json_writer);
    }
  } else {
    /* end triples array */
    raptor_json_writer_end_block(context->json_writer, ']');
    raptor_json_writer_newline(context->json_writer);
  }


  if(serializer->feature_json_extra_data) {
    raptor_iostream_write_byte(serializer->iostream, ',');
    raptor_json_writer_newline(context->json_writer);
    raptor_iostream_write_string(serializer->iostream,
                                 serializer->feature_json_extra_data);
    raptor_json_writer_newline(context->json_writer);
  }


  /* end outer object */
  raptor_json_writer_end_block(context->json_writer, '}');
  raptor_json_writer_newline(context->json_writer);

  /* end callback */
  if(serializer->feature_json_callback)
    raptor_iostream_write_counted_string(serializer->iostream,
                                         (const unsigned char*)");", 2);

  return 0;
}


static void
raptor_json_serialize_finish_factory(raptor_serializer_factory* factory)
{
  /* NOP */
}



static int
raptor_json_triples_serializer_register_factory(raptor_serializer_factory *factory)
{
  factory->context_length     = sizeof(raptor_json_context);
  
  factory->init                = raptor_json_serialize_init;
  factory->terminate           = raptor_json_serialize_terminate;
  factory->declare_namespace   = NULL;
  factory->declare_namespace_from_namespace   = NULL;
  factory->serialize_start     = raptor_json_serialize_start;
  factory->serialize_statement = raptor_json_serialize_statement;
  factory->serialize_end       = raptor_json_serialize_end;
  factory->finish_factory      = raptor_json_serialize_finish_factory;

  return 0;
}


static int
raptor_json_resource_serializer_register_factory(raptor_serializer_factory *factory)
{
  factory->context_length     = sizeof(raptor_json_context);
  
  factory->init                = raptor_json_serialize_init;
  factory->terminate           = raptor_json_serialize_terminate;
  factory->declare_namespace   = NULL;
  factory->declare_namespace_from_namespace   = NULL;
  factory->serialize_start     = raptor_json_serialize_start;
  factory->serialize_statement = raptor_json_serialize_statement;
  factory->serialize_end       = raptor_json_serialize_end;
  factory->finish_factory      = raptor_json_serialize_finish_factory;

  return 0;
}


int
raptor_init_serializer_json(void)
{
  int rc;
  
  rc=raptor_serializer_register_factory("json-triples",
                                        "RDF/JSON Triples", 
                                        "application/json",
                                        NULL,
                                        NULL,
                                        &raptor_json_triples_serializer_register_factory);
  if(rc)
    return rc;
  
  return raptor_serializer_register_factory("json",
                                            "RDF/JSON Resource-Centric", 
                                            "application/json",
                                            NULL,
                                            (const unsigned char *)"http://n2.talis.com/wiki/RDF_JSON_Specification",
                                            &raptor_json_resource_serializer_register_factory);
}
