/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_json_writer.c - Raptor JSON Writer
 *
 * Copyright (C) 2008, David Beckett http://purl.org/net/dajobe/
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
#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif


/* Raptor includes */
#include "raptor.h"
#include "raptor_internal.h"

#ifndef STANDALONE

#ifndef RAPTOR_JSON_WRITER_DATATYPES
#define RAPTOR_JSON_WRITER_DATATYPES 0
#endif

struct raptor_json_writer_s {
  raptor_uri* base_uri;
  
  const raptor_uri_handler *uri_handler;
  void *uri_context;

  raptor_simple_message_handler error_handler;
  void *error_data;

  /* outputting to this iostream */
  raptor_iostream *iostr;

#if RAPTOR_JSON_WRITER_DATATYPES == 1
  raptor_uri* xsd_boolean_uri;
  raptor_uri* xsd_decimal_uri;
  raptor_uri* xsd_double_uri;
  raptor_uri* xsd_integer_uri;
#endif

  /* current indent */
  int indent;

  /* indent step */
  int indent_step;
};



/**
 * raptor_new_json_writer:
 * @base_uri: Base URI for the writer
 * @uri_handler: URI handler function
 * @uri_context: URI handler context data
 * @iostr: I/O stream to write to
 * @error_handler: error handler function
 * @error_data: error handler data
 * 
 * Constructor - Create a new JSON writer writing to a raptor_iostream
 * 
 * Return value: a new #raptor_json_writer object or NULL on failure
 **/
raptor_json_writer*
raptor_new_json_writer(raptor_uri* base_uri,
                       const raptor_uri_handler *uri_handler,
                       void *uri_context,
                       raptor_iostream* iostr,
                       raptor_simple_message_handler error_handler,
                       void *error_data)
{
  raptor_json_writer* json_writer;

  json_writer=(raptor_json_writer*)RAPTOR_CALLOC(raptor_json_writer, 1, sizeof(raptor_json_writer)+1);

  if(!json_writer)
    return NULL;

  json_writer->uri_handler=uri_handler;
  json_writer->uri_context=uri_context;
  json_writer->error_handler=error_handler;
  json_writer->error_data=error_data;
  json_writer->iostr=iostr;
  json_writer->base_uri=base_uri;

#if RAPTOR_JSON_WRITER_DATATYPES == 1
  json_writer->xsd_boolean_uri=raptor_new_uri((const unsigned char*)"http://www.w3.org/2001/XMLSchema#boolean");
  json_writer->xsd_decimal_uri=raptor_new_uri((const unsigned char*)"http://www.w3.org/2001/XMLSchema#decimal");
  json_writer->xsd_double_uri=raptor_new_uri((const unsigned char*)"http://www.w3.org/2001/XMLSchema#double");
  json_writer->xsd_integer_uri=raptor_new_uri((const unsigned char*)"http://www.w3.org/2001/XMLSchema#integer");
#endif

  json_writer->indent_step=2;
  
  return json_writer;
}


/**
 * raptor_free_json_writer:
 * @json_writer: JSON writer object
 *
 * Destructor - Free JSON Writer
 * 
 **/
void
raptor_free_json_writer(raptor_json_writer* json_writer)
{

#if RAPTOR_JSON_WRITER_DATATYPES == 1
  if(json_writer->xsd_boolean_uri)
    raptor_free_uri(json_writer->xsd_boolean_uri);
  if(json_writer->xsd_decimal_uri)
    raptor_free_uri(json_writer->xsd_decimal_uri);
  if(json_writer->xsd_double_uri)
    raptor_free_uri(json_writer->xsd_double_uri);
  if(json_writer->xsd_integer_uri)
    raptor_free_uri(json_writer->xsd_integer_uri);
#endif

  RAPTOR_FREE(raptor_json_writer, json_writer);
}


static int
raptor_json_writer_quoted(raptor_json_writer* json_writer,
                          const char *value, size_t value_len)
{
  if(!value) {
    raptor_iostream_write_counted_string(json_writer->iostr, "\"\"", 2);
    return 0;
  }
  if(!value_len)
    value_len=strlen((const char*)value);

  raptor_iostream_write_byte(json_writer->iostr, '\"');
  raptor_iostream_write_string_python(json_writer->iostr,
                                      (const unsigned char*)value, value_len,
                                      '"', 3);
  raptor_iostream_write_byte(json_writer->iostr, '\"');

  return 0;
}


static int
raptor_json_writer_spaces(raptor_json_writer* json_writer, int depth) 
{
  int i;
  for(i=0; i<depth; i++)
    raptor_iostream_write_byte(json_writer->iostr, ' ');
  return 0;
}


int
raptor_json_writer_newline(raptor_json_writer* json_writer)
{
  raptor_iostream_write_byte(json_writer->iostr, '\n');
  if(json_writer->indent)
    raptor_json_writer_spaces(json_writer, json_writer->indent);
  return 0;
}


int
raptor_json_writer_key_value(raptor_json_writer* json_writer,
                             const char* key, size_t key_len,
                             const char* value, size_t value_len)
{
  if(!key_len && key)
    key_len=strlen(key);
  if(!value_len && value)
    value_len=strlen(value);
  
  raptor_json_writer_quoted(json_writer, key, key_len);
  raptor_iostream_write_counted_string(json_writer->iostr, " : ", 3);
  raptor_json_writer_quoted(json_writer, value, value_len);

  return 0;
}


int
raptor_json_writer_key_uri_value(raptor_json_writer* json_writer, 
                                 const char* key, size_t key_len,
                                 raptor_uri* uri)
{
  const char* value;
  size_t value_len;
  int rc=0;
  
  value=(const char*)raptor_uri_to_relative_counted_uri_string(json_writer->base_uri, uri, &value_len);
  if(!value)
    return 1;

  if(key)
    rc=raptor_json_writer_key_value(json_writer, key, key_len, 
                                    value, value_len);
  else
    rc=raptor_json_writer_quoted(json_writer, value, value_len);
  
  RAPTOR_FREE(cstring, value);

  return 0;
}


int
raptor_json_writer_start_block(raptor_json_writer* json_writer, char c)
{
  json_writer->indent += json_writer->indent_step;
  raptor_iostream_write_byte(json_writer->iostr, c);
  return 0;
}


int
raptor_json_writer_end_block(raptor_json_writer* json_writer, char c)
{
  raptor_iostream_write_byte(json_writer->iostr, c);
  json_writer->indent -= json_writer->indent_step;
  return 0;
}


int
raptor_json_writer_literal_object(raptor_json_writer* json_writer,
                                  unsigned char* s, unsigned char* lang,
                                  raptor_uri* datatype,
                                  const char* key, const char* type_key)
{

  if(key) {
    raptor_json_writer_start_block(json_writer, '{');
    raptor_json_writer_newline(json_writer);
    
    raptor_json_writer_quoted(json_writer, key, 0);
  
    raptor_iostream_write_counted_string(json_writer->iostr, " : ", 3);
  }

  raptor_json_writer_quoted(json_writer, (const char*)s, 0);
  
  if(datatype || lang) {
    raptor_iostream_write_byte(json_writer->iostr, ',');
    raptor_json_writer_newline(json_writer);

    if(datatype)
      raptor_json_writer_key_uri_value(json_writer, "datatype", 8, datatype);
    
    if(lang) {
      if(datatype) {
        raptor_iostream_write_byte(json_writer->iostr, ',');
        raptor_json_writer_newline(json_writer);
      }

      raptor_json_writer_key_value(json_writer, "lang", 4,
                                   (const char*)lang, 0);
    }
  }

  if(type_key) {
    raptor_iostream_write_byte(json_writer->iostr, ',');
    raptor_json_writer_newline(json_writer);

    raptor_json_writer_key_value(json_writer, type_key, 0, "literal", 0);
  }

  raptor_json_writer_newline(json_writer);

  if(key) {
    raptor_json_writer_end_block(json_writer, '}');
    raptor_json_writer_newline(json_writer);
  }

  return 0;
}


/* not used here */

#if RAPTOR_JSON_WRITER_DATATYPES == 1
int raptor_json_writer_literal_datatype(raptor_json_writer* json_writer, raptor_namespace_stack *nstack, unsigned char* s, unsigned char* lang, raptor_uri* datatype);


int
raptor_json_writer_literal_datatype(raptor_json_writer* json_writer,
                                    raptor_namespace_stack *nstack,
                                    unsigned char* s, unsigned char* lang,
                                    raptor_uri* datatype)
{
  /* DBL_MAX = 309 decimal digits */
  #define INT_MAX_LEN 309 

  /* DBL_EPSILON = 52 digits */
  #define FRAC_MAX_LEN 52
  
  const size_t buflen = INT_MAX_LEN + FRAC_MAX_LEN + 3; /* sign, decimal, \0 */
  char buf[buflen];

  size_t len = 0;
  char* endptr = (char *)s;
  int written = 0;

  /* typed literal special cases */
  if(datatype) {
    /* integer */
    if(raptor_uri_equals(datatype, json_writer->xsd_integer_uri)) {
      long inum = strtol((const char*)s, NULL, 10);
      if(inum != LONG_MIN && inum != LONG_MAX) {
        raptor_iostream_write_decimal(json_writer->iostr, inum);
        written = 1;
      }
    
    /* double */
    } else if(raptor_uri_equals(datatype, json_writer->xsd_double_uri)) {
      double dnum = strtod((const char*)s, &endptr);
      if(endptr != (char*)s) {
        const char* decimal = strchr((const char*)s, '.');
        const size_t max_digits = (decimal ? (endptr - decimal - 2) : 1);
        char* num_str;

        num_str=raptor_format_float(buf, &len, buflen, dnum, 1, max_digits, 0);
        raptor_iostream_write_counted_string(json_writer->iostr, num_str, len);
        written = 1;
      }

    /* decimal */
    } else if(raptor_uri_equals(datatype, json_writer->xsd_decimal_uri)) {
      double dnum = strtod((const char*)s, &endptr);
      if(endptr != (char*)s) {
        snprintf(buf, 20, "%.1lf", dnum);
        raptor_iostream_write_string(json_writer->iostr, buf);
        written = 1;
      }
    
    /* boolean */
    } else if(raptor_uri_equals(datatype, json_writer->xsd_boolean_uri)) {
      if(!strcmp((const char*)s, "0") || !strcmp((const char*)s, "false")) {
        raptor_iostream_write_string(json_writer->iostr, "false");
        written = 1;
      } else if(!strcmp((const char*)s, "1") || !strcmp((const char*)s, "true")) {
        raptor_iostream_write_string(json_writer->iostr, "true");
        written = 1;
      } else {
        json_writer->error_handler(json_writer->error_data, "Illegal value for xsd:boolean literal.");
        return 1;
      }
    }
  }

  if(written)
    return 0;

  return raptor_json_writer_literal_object(json_writer,
                                           s, lang, datatype, "value", NULL);
}
#endif


int
raptor_json_writer_blank_object(raptor_json_writer* json_writer,
                                const char* blank)
{
  raptor_json_writer_start_block(json_writer, '{');
  raptor_json_writer_newline(json_writer);

  raptor_iostream_write_counted_string(json_writer->iostr, 
                                       "\"value\" : \"_:", 13);
  raptor_iostream_write_string(json_writer->iostr, blank);
  raptor_iostream_write_counted_string(json_writer->iostr, "\",", 2);
  raptor_json_writer_newline(json_writer);

  raptor_iostream_write_counted_string(json_writer->iostr,
                                       "\"type\" : \"bnode\"", 16);
  raptor_json_writer_newline(json_writer);

  raptor_json_writer_end_block(json_writer, '}');
  return 0;
}


int
raptor_json_writer_uri_object(raptor_json_writer* json_writer,
                              raptor_uri* uri)
{
  raptor_json_writer_start_block(json_writer, '{');
  raptor_json_writer_newline(json_writer);

  raptor_json_writer_key_uri_value(json_writer, "value", 5, uri);
  raptor_iostream_write_byte(json_writer->iostr, ',');
  raptor_json_writer_newline(json_writer);

  raptor_iostream_write_counted_string(json_writer->iostr,
                                       "\"type\" : \"uri\"", 14);
  raptor_json_writer_newline(json_writer);

  raptor_json_writer_end_block(json_writer, '}');

  return 0;
}

#endif
