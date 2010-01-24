/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_serialize_rdfxml.c - Simple serializer
 *
 * Copyright (C) 2004-2006, David Beckett http://purl.org/net/dajobe/
 * Copyright (C) 2004-2005, University of Bristol, UK http://www.bristol.ac.uk/
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
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

/* Raptor includes */
#include "raptor.h"
#include "raptor_internal.h"


/*
 * Raptor 'Simple' serializer object
 */
typedef struct {
  int dummy;
} raptor_simple_serializer_context;



/* create a new serializer */
static int
raptor_simple_serialize_init(raptor_serializer* serializer, const char *name)
{
  return 0;
}
  

/* destroy a serializer */
static void
raptor_simple_serialize_terminate(raptor_serializer* serializer)
{

}
  

/* serialize a statement */
static int
raptor_simple_serialize_statement(raptor_serializer* serializer, 
                                  const raptor_statement *statement)
{
  raptor_iostream *iostr=serializer->iostream;
  
  /* was: fprintf(stdout, "%s: Statement: ", program); */
  raptor_iostream_write_string(iostr, "Statement: ");

  /* from raptor_print_statement */
  raptor_iostream_write_byte(iostr, '[');

  if(statement->subject_type == RAPTOR_IDENTIFIER_TYPE_ANONYMOUS) {
    raptor_iostream_write_string(iostr, statement->subject);
  } else {
#ifdef RAPTOR_DEBUG
    if(!statement->subject)
      RAPTOR_FATAL1("Statement has NULL subject URI\n");
#endif
    raptor_iostream_write_uri(iostr, (raptor_uri*)statement->subject);
  }

  raptor_iostream_write_counted_string(iostr, ", ", 2);

  if(statement->predicate_type == RAPTOR_IDENTIFIER_TYPE_ORDINAL) {
    raptor_iostream_write_counted_string(iostr, "[rdf:_", 6);
    raptor_iostream_write_decimal(iostr, *((int*)statement->predicate));
    raptor_iostream_write_byte(iostr, ']');
  } else {
#ifdef RAPTOR_DEBUG
    if(!statement->predicate)
      RAPTOR_FATAL1("Statement has NULL predicate URI\n");
#endif
    raptor_iostream_write_uri(iostr, (raptor_uri*)statement->predicate);
  }

  raptor_iostream_write_counted_string(iostr, ", ", 2);

  if(statement->object_type == RAPTOR_IDENTIFIER_TYPE_LITERAL || 
     statement->object_type == RAPTOR_IDENTIFIER_TYPE_XML_LITERAL) {
    if(statement->object_type == RAPTOR_IDENTIFIER_TYPE_XML_LITERAL) {
      raptor_iostream_write_byte(iostr, '<');
      raptor_iostream_write_string(iostr, raptor_xml_literal_datatype_uri_string);
      raptor_iostream_write_byte(iostr, '>');
    } else if(statement->object_literal_datatype) {
      raptor_iostream_write_byte(iostr, '<');
      raptor_iostream_write_uri(iostr, (raptor_uri*)statement->object_literal_datatype);
      raptor_iostream_write_byte(iostr, '>');
    }
    raptor_iostream_write_byte(iostr, '"');
    raptor_iostream_write_string(iostr, statement->object);
    raptor_iostream_write_byte(iostr, '"');
  } else if(statement->object_type == RAPTOR_IDENTIFIER_TYPE_ANONYMOUS)
    raptor_iostream_write_string(iostr, statement->object);
  else if(statement->object_type == RAPTOR_IDENTIFIER_TYPE_ORDINAL) {
    raptor_iostream_write_counted_string(iostr, "[rdf:_", 6);
    raptor_iostream_write_decimal(iostr, *((int*)statement->object));
    raptor_iostream_write_byte(iostr, ']');
  } else {
#ifdef RAPTOR_DEBUG
    if(!statement->object)
      RAPTOR_FATAL1("Statement has NULL object URI\n");
#endif
    raptor_iostream_write_uri(iostr, (raptor_uri*)statement->object);
  }

  raptor_iostream_write_counted_string(iostr, "]\n", 2);

  return 0;
}


  
/* finish the serializer factory */
static void
raptor_simple_serialize_finish_factory(raptor_serializer_factory* factory)
{

}


static int
raptor_simple_serializer_register_factory(raptor_serializer_factory *factory)
{
  factory->context_length     = sizeof(raptor_simple_serializer_context);
  
  factory->init                = raptor_simple_serialize_init;
  factory->terminate           = raptor_simple_serialize_terminate;
  factory->declare_namespace   = NULL;
  factory->serialize_start     = NULL;
  factory->serialize_statement = raptor_simple_serialize_statement;
  factory->serialize_end       = NULL;
  factory->finish_factory      = raptor_simple_serialize_finish_factory;

  return 0;
}


int
raptor_init_serializer_simple (void) {
  return raptor_serializer_register_factory("simple",  "A simple format", 
                                            NULL,
                                            NULL,
                                            NULL,
                                            &raptor_simple_serializer_register_factory);
}
