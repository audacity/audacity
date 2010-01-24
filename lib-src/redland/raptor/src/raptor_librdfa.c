/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_librdfa.c - Raptor RDFA Parser via librdfa implementation
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
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

/* Raptor includes */
#include "raptor.h"
#include "raptor_internal.h"

#include "rdfa.h"
#include "rdfa_utils.h"



/*
 * RDFA parser object
 */
struct raptor_librdfa_parser_context_s {
  /* librdfa object */
  rdfacontext* context;
  
  /* static statement for use in passing to user code */
  raptor_statement statement;
};


typedef struct raptor_librdfa_parser_context_s raptor_librdfa_parser_context;


static int
raptor_librdfa_parse_init(raptor_parser* rdf_parser, const char *name)
{
  /*raptor_librdfa_parser_context *librdfa_parser=(raptor_librdfa_parser_context*)rdf_parser->context; */
  return 0;
}


static void
raptor_librdfa_parse_terminate(raptor_parser* rdf_parser)
{
  raptor_librdfa_parser_context *librdfa_parser=(raptor_librdfa_parser_context*)rdf_parser->context;

  if(librdfa_parser->context) {
    rdfa_parse_end(librdfa_parser->context);
    rdfa_free_context(librdfa_parser->context);
    librdfa_parser->context=NULL;
  }
}


static void
raptor_librdfa_generate_statement(rdftriple* triple, void* callback_data)
{
  raptor_parser* parser=(raptor_parser*)callback_data;
  raptor_statement *s=&parser->statement;
  raptor_uri *subject_uri=NULL;
  raptor_uri *predicate_uri=NULL;
  raptor_uri *object_uri=NULL;
  raptor_uri *datatype_uri=NULL;

  if(!triple->subject || !triple->predicate || !triple->object) {
    RAPTOR_FATAL1("Triple has NULL parts\n");
    rdfa_free_triple(triple);
    return;
  }
  
  if(triple->object_type == RDF_TYPE_NAMESPACE_PREFIX) {
    RAPTOR_FATAL1("Triple has namespace object type\n");
    rdfa_free_triple(triple);
    return;
  }
  
  if((triple->subject[0] == '_') && (triple->subject[1] == ':')) {
    s->subject_type = RAPTOR_IDENTIFIER_TYPE_ANONYMOUS;
    s->subject= (triple->subject + 2);
  } else {
    s->subject_type = RAPTOR_IDENTIFIER_TYPE_RESOURCE;
    subject_uri=raptor_new_uri((const unsigned char*)triple->subject);
    if(!subject_uri)
      goto cleanup;
    s->subject=subject_uri;
  }
  

  predicate_uri=raptor_new_uri((const unsigned char*)triple->predicate);
  if(!predicate_uri)
    goto cleanup;
  s->predicate=predicate_uri;
  s->predicate_type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
 
  s->object = triple->object;
  s->object_literal_datatype=NULL;
  s->object_literal_language=NULL;
  if(triple->object_type == RDF_TYPE_IRI) {
    if((triple->object[0] == '_') && (triple->object[1] == ':')) {
      s->object_type = RAPTOR_IDENTIFIER_TYPE_ANONYMOUS;
      s->object = (triple->object + 2);
    } else {
      s->object_type = RAPTOR_IDENTIFIER_TYPE_RESOURCE;
      object_uri=raptor_new_uri((const unsigned char*)triple->object);
      if(!object_uri)
        goto cleanup;
      s->object=object_uri;
    }
  } else if(triple->object_type == RDF_TYPE_PLAIN_LITERAL) {
    s->object_type=RAPTOR_IDENTIFIER_TYPE_LITERAL;
    if(triple->language)
      s->object_literal_language=(const unsigned char*)triple->language;
  } else if(triple->object_type == RDF_TYPE_XML_LITERAL) {
    s->object_type = RAPTOR_IDENTIFIER_TYPE_XML_LITERAL;
  } else if(triple->object_type == RDF_TYPE_TYPED_LITERAL) {
    s->object_type=RAPTOR_IDENTIFIER_TYPE_LITERAL;
    if(triple->language)
      s->object_literal_language=(const unsigned char*)triple->language;
    if(triple->datatype) {
      datatype_uri=raptor_new_uri((const unsigned char*)triple->datatype);
      if(!datatype_uri)
        goto cleanup;
      s->object_literal_datatype=datatype_uri;
      /* If datatype, no language allowed */
      s->object_literal_language=NULL;
    }
  } else {
    RAPTOR_FATAL2("Triple has unknown object type %d\n", s->object_type);
    goto cleanup;
  }
  
  if(!parser->statement_handler)
    goto cleanup;

  /* Generate triple */
  (*parser->statement_handler)(parser->user_data, s);

  cleanup:
  rdfa_free_triple(triple);
  
  if(subject_uri)
    raptor_free_uri(subject_uri);
  if(predicate_uri)
    raptor_free_uri(predicate_uri);
  if(object_uri)
    raptor_free_uri(object_uri);
  if(datatype_uri)
    raptor_free_uri(datatype_uri);
}


static void
raptor_librdfa_sax2_new_namespace_handler(void *user_data,
                                          raptor_namespace* nspace)
{
  raptor_parser* rdf_parser;
  rdf_parser=(raptor_parser*)user_data;
  raptor_parser_start_namespace(rdf_parser, nspace);
}



static int
raptor_librdfa_parse_start(raptor_parser* rdf_parser) 
{
  raptor_locator *locator=&rdf_parser->locator;
  raptor_librdfa_parser_context *librdfa_parser=(raptor_librdfa_parser_context*)rdf_parser->context;
  int rc;
  char* base_uri_string=NULL;
  
  locator->line=1;
  locator->column=0;
  locator->byte=0;

  if(rdf_parser->base_uri)
    base_uri_string=(char*)raptor_uri_as_string(rdf_parser->base_uri);

  if(librdfa_parser->context)
    rdfa_free_context(librdfa_parser->context);
  librdfa_parser->context=rdfa_create_context(base_uri_string);
  if(!librdfa_parser->context)
    return 1;

  librdfa_parser->context->namespace_handler=raptor_librdfa_sax2_new_namespace_handler;
  librdfa_parser->context->namespace_handler_user_data=rdf_parser;
  librdfa_parser->context->error_handlers=&rdf_parser->error_handlers;

  librdfa_parser->context->callback_data=rdf_parser;
  rdfa_set_triple_handler(librdfa_parser->context, 
                          raptor_librdfa_generate_statement);

  rc = rdfa_parse_start(librdfa_parser->context);
  if(rc != RDFA_PARSE_SUCCESS)
    return 1;
  
  return 0;
}


static int
raptor_librdfa_parse_chunk(raptor_parser* rdf_parser, 
                           const unsigned char *s, size_t len,
                           int is_end)
{
  raptor_librdfa_parser_context *librdfa_parser=(raptor_librdfa_parser_context*)rdf_parser->context;
  int rval=rdfa_parse_chunk(librdfa_parser->context, (char*)s, len, is_end);
  return rval != RDFA_PARSE_SUCCESS;
}

static int
raptor_librdfa_parse_recognise_syntax(raptor_parser_factory* factory, 
                                      const unsigned char *buffer, size_t len,
                                      const unsigned char *identifier, 
                                      const unsigned char *suffix, 
                                      const char *mime_type)
{
  int score=0;
  
  if(identifier) {
    if(strstr((const char*)identifier, "RDFa"))
      score=10;
  }
  
  if(buffer && len) {
#define  HAS_RDFA_1 (strstr((const char*)buffer, "-//W3C//DTD XHTML+RDFa 1.0//EN") != NULL)
#define  HAS_RDFA_2 (strstr((const char*)buffer, "http://www.w3.org/MarkUp/DTD/xhtml-rdfa-1.dtd") != NULL)

    if(HAS_RDFA_1 || HAS_RDFA_2)
      score=10;
  }
  
  return score;
}


static int
raptor_librdfa_parser_register_factory(raptor_parser_factory *factory) 
{
  int rc=0;

  factory->context_length     = sizeof(raptor_librdfa_parser_context);

  factory->need_base_uri = 0;
  
  factory->init      = raptor_librdfa_parse_init;
  factory->terminate = raptor_librdfa_parse_terminate;
  factory->start     = raptor_librdfa_parse_start;
  factory->chunk     = raptor_librdfa_parse_chunk;
  factory->recognise_syntax = raptor_librdfa_parse_recognise_syntax;

  rc=raptor_parser_factory_add_uri(factory, 
                                (const unsigned char*)"http://www.w3.org/TR/rdfa/");

  return rc;
}


int
raptor_init_parser_rdfa(void)
{
  return !raptor_parser_register_factory("rdfa",  "RDF/A via librdfa",
                                         &raptor_librdfa_parser_register_factory);
}
