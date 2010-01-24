/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_serialize_ntriples.c - N-Triples serializer
 *
 * Copyright (C) 2004-2008, David Beckett http://www.dajobe.org/
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
 * Raptor N-Triples serializer object
 */
typedef struct {
  int dummy;
} raptor_ntriples_serializer_context;



/* create a new serializer */
static int
raptor_ntriples_serialize_init(raptor_serializer* serializer, const char *name)
{
  return 0;
}
  

/* destroy a serializer */
static void
raptor_ntriples_serialize_terminate(raptor_serializer* serializer)
{

}
  

/* add a namespace */
static int
raptor_ntriples_serialize_declare_namespace(raptor_serializer* serializer, 
                                            raptor_uri *uri,
                                            const unsigned char *prefix)
{
  /* NOP */
  return 0;
}


#if 0
/* start a serialize */
static int
raptor_ntriples_serialize_start(raptor_serializer* serializer)
{
  return 0;
}
#endif



/**
 * raptor_iostream_write_string_ntriples:
 * @iostr: #raptor_iostream to write to
 * @string: UTF-8 string to write
 * @len: length of UTF-8 string
 * @delim: Terminating delimiter character for string (such as " or >)
 * or \0 for no escaping.
 *
 * Write an UTF-8 string using N-Triples escapes to an iostream.
 * 
 * Return value: non-0 on failure such as bad UTF-8 encoding.
 **/
int
raptor_iostream_write_string_ntriples(raptor_iostream *iostr,
                                      const unsigned char *string,
                                      size_t len,
                                      const char delim)
{
  return raptor_iostream_write_string_python(iostr, string, len, delim, 0);
}


static void
raptor_iostream_write_statement_part_ntriples(raptor_iostream* iostr,
                                               const void *term, 
                                               raptor_identifier_type type,
                                               raptor_uri* literal_datatype,
                                               const unsigned char *literal_language) 
{
  size_t len;
  
  switch(type) {
    case RAPTOR_IDENTIFIER_TYPE_LITERAL:
    case RAPTOR_IDENTIFIER_TYPE_XML_LITERAL:
      raptor_iostream_write_byte(iostr, '"');
      raptor_iostream_write_string_ntriples(iostr, (const unsigned char*)term, strlen((const char*)term), '"');
      raptor_iostream_write_byte(iostr, '"');
      if(literal_language && type == RAPTOR_IDENTIFIER_TYPE_LITERAL) {
        raptor_iostream_write_byte(iostr, '@');
        raptor_iostream_write_string(iostr, literal_language);
      }
      if(type == RAPTOR_IDENTIFIER_TYPE_XML_LITERAL) {
        raptor_iostream_write_counted_string(iostr, "^^<", 3);
        raptor_iostream_write_string(iostr, raptor_xml_literal_datatype_uri_string);
        raptor_iostream_write_byte(iostr, '>');
      } else if(literal_datatype) {
        raptor_iostream_write_counted_string(iostr, "^^<", 3);
        raptor_iostream_write_string(iostr, raptor_uri_as_string((raptor_uri*)literal_datatype));
        raptor_iostream_write_byte(iostr, '>');
      }

      break;
      
    case RAPTOR_IDENTIFIER_TYPE_ANONYMOUS:
      raptor_iostream_write_counted_string(iostr, "_:", 2);
      raptor_iostream_write_string(iostr, term);
      break;
      
    case RAPTOR_IDENTIFIER_TYPE_ORDINAL:
      raptor_iostream_write_counted_string(iostr, "<_", 1);
      raptor_iostream_write_counted_string(iostr, raptor_rdf_namespace_uri,
                                           raptor_rdf_namespace_uri_len);
      raptor_iostream_write_counted_string(iostr, "_", 1);
      raptor_iostream_write_decimal(iostr, *((int*)term));
      raptor_iostream_write_byte(iostr, '>');
      break;
  
    case RAPTOR_IDENTIFIER_TYPE_RESOURCE:
    case RAPTOR_IDENTIFIER_TYPE_PREDICATE:
      raptor_iostream_write_byte(iostr, '<');
      term=raptor_uri_as_counted_string((raptor_uri*)term, &len);
      raptor_iostream_write_string_ntriples(iostr, (const unsigned char*)term, len, '>');
      raptor_iostream_write_byte(iostr, '>');
      break;
      
    case RAPTOR_IDENTIFIER_TYPE_UNKNOWN:
    default:
      RAPTOR_FATAL2("Unknown type %d", type);
  }
}


/**
 * raptor_iostream_write_statement_ntriples:
 * @iostr: raptor iosteram
 * @statement: statement to write
 * 
 * Write a #raptor_statement formatted in N-Triples format to a #raptor_iostream
 * 
 **/
void
raptor_iostream_write_statement_ntriples(raptor_iostream* iostr,
                                         const raptor_statement *statement)
{
  raptor_iostream_write_statement_part_ntriples(iostr,
                                                statement->subject,
                                                statement->subject_type,
                                                NULL, NULL);
  raptor_iostream_write_byte(iostr, ' ');
  raptor_iostream_write_statement_part_ntriples(iostr,
                                                statement->predicate,
                                                statement->predicate_type,
                                                NULL, NULL);
  raptor_iostream_write_byte(iostr, ' ');
  raptor_iostream_write_statement_part_ntriples(iostr,
                                                statement->object,
                                                statement->object_type,
                                                statement->object_literal_datatype,
                                                statement->object_literal_language);
  raptor_iostream_write_counted_string(iostr, " .\n", 3);
}


/* serialize a statement */
static int
raptor_ntriples_serialize_statement(raptor_serializer* serializer, 
                                    const raptor_statement *statement)
{
  raptor_iostream_write_statement_ntriples(serializer->iostream, statement);
  return 0;
}


#if 0
/* end a serialize */
static int
raptor_ntriples_serialize_end(raptor_serializer* serializer)
{
  return 0;
}
#endif
  
/* finish the serializer factory */
static void
raptor_ntriples_serialize_finish_factory(raptor_serializer_factory* factory)
{

}


static int
raptor_ntriples_serializer_register_factory(raptor_serializer_factory *factory)
{
  factory->context_length     = sizeof(raptor_ntriples_serializer_context);
  
  factory->init                = raptor_ntriples_serialize_init;
  factory->terminate           = raptor_ntriples_serialize_terminate;
  factory->declare_namespace   = raptor_ntriples_serialize_declare_namespace;
  factory->serialize_start     = NULL;
  factory->serialize_statement = raptor_ntriples_serialize_statement;
  factory->serialize_end       = NULL;
  factory->finish_factory      = raptor_ntriples_serialize_finish_factory;

  return 0;
}


int
raptor_init_serializer_ntriples (void) {
  return raptor_serializer_register_factory("ntriples",  "N-Triples", 
                                            "text/plain",
                                            NULL,
                                            (const unsigned char*)"http://www.w3.org/TR/rdf-testcases/#ntriples",
                                            &raptor_ntriples_serializer_register_factory);
}


