/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_turtle_writer.c - Raptor Turtle Writer
 *
 * Copyright (C) 2006, Dave Robillard
 * Copyright (C) 2003-2008, David Beckett http://purl.org/net/dajobe/
 * Copyright (C) 2003-2005, University of Bristol, UK http://www.bristol.ac.uk/
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
#include <math.h>

/* Raptor includes */
#include "raptor.h"
#include "raptor_internal.h"

#ifndef STANDALONE


typedef enum {
  TURTLE_WRITER_AUTO_INDENT = 1,
} raptor_turtle_writer_flags;


#define TURTLE_WRITER_AUTO_INDENT(turtle_writer) ((turtle_writer->flags & TURTLE_WRITER_AUTO_INDENT) != 0)

struct raptor_turtle_writer_s {
  int canonicalize;

  int depth;
 
  raptor_uri* base_uri;

  int my_nstack;
  raptor_namespace_stack *nstack;
  int nstack_depth;

  const raptor_uri_handler *uri_handler;
  void *uri_context;

  raptor_simple_message_handler error_handler;
  void *error_data;

  /* outputting to this iostream */
  raptor_iostream *iostr;

  /* Turtle Writer flags - bits defined in enum raptor_turtle_writer_flags */
  int flags;

  /* indentation per level if formatting */
  int indent;

  raptor_uri* xsd_boolean_uri;
  raptor_uri* xsd_decimal_uri;
  raptor_uri* xsd_double_uri;
  raptor_uri* xsd_integer_uri;
};


/* 16 spaces */
#define SPACES_BUFFER_SIZE sizeof(spaces_buffer)
static const unsigned char spaces_buffer[] = {
  ' ', ' ', ' ', ' ',
  ' ', ' ', ' ', ' ',
  ' ', ' ', ' ', ' ',
  ' ', ' ', ' ', ' '
};


void
raptor_turtle_writer_increase_indent(raptor_turtle_writer *turtle_writer)
{
  turtle_writer->depth += turtle_writer->indent;
}


void
raptor_turtle_writer_decrease_indent(raptor_turtle_writer *turtle_writer)
{
  turtle_writer->depth -= turtle_writer->indent;
}

  
void
raptor_turtle_writer_newline(raptor_turtle_writer *turtle_writer)
{
  int num_spaces;
  
  raptor_iostream_write_byte(turtle_writer->iostr, '\n');
 
  if(!TURTLE_WRITER_AUTO_INDENT(turtle_writer))
    return;

  num_spaces = turtle_writer->depth * turtle_writer->indent;

  while(num_spaces > 0) {
    int count;
    count = (num_spaces > (int)SPACES_BUFFER_SIZE) ? (int)SPACES_BUFFER_SIZE : num_spaces;

    raptor_iostream_write_counted_string(turtle_writer->iostr, spaces_buffer, count);

    num_spaces -= count;
  }

  return;
}


/**
 * raptor_new_turtle_writer:
 * @base_uri: Base URI for the writer
 * @write_base_uri: non-0 to write 'base' in output
 * @nstack: Namespace stack for the writer to start with (or NULL)
 * @uri_handler: URI handler function
 * @uri_context: URI handler context data
 * @iostr: I/O stream to write to
 * @error_handler: error handler function
 * @error_data: error handler data
 * 
 * Constructor - Create a new Turtle Writer writing Turtle to a raptor_iostream
 * 
 * Return value: a new #raptor_turtle_writer object or NULL on failure
 **/
raptor_turtle_writer*
raptor_new_turtle_writer(raptor_uri* base_uri, int write_base_uri,
                         raptor_namespace_stack *nstack,
                         const raptor_uri_handler *uri_handler,
                         void *uri_context,
                         raptor_iostream* iostr,
                         raptor_simple_message_handler error_handler,
                         void *error_data)
{
  raptor_turtle_writer* turtle_writer=(raptor_turtle_writer*)RAPTOR_CALLOC(raptor_turtle_writer, 1, sizeof(raptor_turtle_writer)+1);

  if(!turtle_writer)
    return NULL;

  turtle_writer->nstack_depth=0;

  turtle_writer->uri_handler=uri_handler;
  turtle_writer->uri_context=uri_context;

  turtle_writer->error_handler=error_handler;
  turtle_writer->error_data=error_data;

  turtle_writer->nstack=nstack;
  if(!turtle_writer->nstack) {
    turtle_writer->nstack=nstack=raptor_new_namespaces(uri_handler,
                                                       uri_context,
                                                       error_handler,
                                                       error_data,
                                                       1);
    turtle_writer->my_nstack=1;
  }

  turtle_writer->iostr=iostr;

  turtle_writer->flags = 0;
  turtle_writer->indent = 2;

  if(base_uri && write_base_uri)
    raptor_turtle_writer_base(turtle_writer, base_uri);

  turtle_writer->xsd_boolean_uri=raptor_new_uri((const unsigned char*)"http://www.w3.org/2001/XMLSchema#boolean");
  turtle_writer->xsd_decimal_uri=raptor_new_uri((const unsigned char*)"http://www.w3.org/2001/XMLSchema#decimal");
  turtle_writer->xsd_double_uri=raptor_new_uri((const unsigned char*)"http://www.w3.org/2001/XMLSchema#double");
  turtle_writer->xsd_integer_uri=raptor_new_uri((const unsigned char*)"http://www.w3.org/2001/XMLSchema#integer");
  
  return turtle_writer;
}


/**
 * raptor_free_turtle_writer:
 * @turtle_writer: Turtle writer object
 *
 * Destructor - Free Turtle Writer
 * 
 **/
void
raptor_free_turtle_writer(raptor_turtle_writer* turtle_writer)
{
  if(turtle_writer->nstack && turtle_writer->my_nstack)
    raptor_free_namespaces(turtle_writer->nstack);

  if(turtle_writer->xsd_boolean_uri)
    raptor_free_uri(turtle_writer->xsd_boolean_uri);
  if(turtle_writer->xsd_decimal_uri)
    raptor_free_uri(turtle_writer->xsd_decimal_uri);
  if(turtle_writer->xsd_double_uri)
    raptor_free_uri(turtle_writer->xsd_double_uri);
  if(turtle_writer->xsd_integer_uri)
    raptor_free_uri(turtle_writer->xsd_integer_uri);

  RAPTOR_FREE(raptor_turtle_writer, turtle_writer);
}


static int
raptor_turtle_writer_contains_newline(const unsigned char *s)
{
  size_t i=0;

  for( ; i < strlen((char*)s); i++)
    if(s[i] == '\n')
      return 1;

  return 0;
}


/**
 * raptor_turtle_writer_raw:
 * @turtle_writer: Turtle writer object
 * @s: raw string to write
 *
 * Write a raw string to the Turtle writer verbatim.
 *
 **/
void
raptor_turtle_writer_raw(raptor_turtle_writer* turtle_writer,
                         const unsigned char *s)
{
  raptor_iostream_write_string(turtle_writer->iostr, s);
}


/**
 * raptor_turtle_writer_raw_counted:
 * @turtle_writer: Turtle writer object
 * @s: raw string to write
 * @len: length of string
 *
 * Write a counted string to the Turtle writer verbatim.
 *
 **/
void
raptor_turtle_writer_raw_counted(raptor_turtle_writer* turtle_writer,
                                 const unsigned char *s, unsigned int len)
{
  raptor_iostream_write_counted_string(turtle_writer->iostr, s, len);
}


/**
 * raptor_turtle_writer_namespace_prefix:
 * @turtle_writer: Turtle writer object
 * @ns: Namespace to write prefix declaration for
 *
 * Write a namespace prefix declaration (@prefix)
 *
 * Must only be used at the beginning of a document.
 */
void
raptor_turtle_writer_namespace_prefix(raptor_turtle_writer* turtle_writer,
                                      raptor_namespace* ns)
{
  raptor_iostream_write_string(turtle_writer->iostr, "@prefix ");
  if(ns->prefix)
    raptor_iostream_write_string(turtle_writer->iostr, 
                                 raptor_namespace_get_prefix(ns));
  raptor_iostream_write_counted_string(turtle_writer->iostr, ": ", 2);
  raptor_turtle_writer_reference(turtle_writer, raptor_namespace_get_uri(ns));
  raptor_iostream_write_counted_string(turtle_writer->iostr, " .\n", 3);
}


/**
 * raptor_turtle_writer_base:
 * @turtle_writer: Turtle writer object
 * @base_uri: New base URI or NULL
 *
 * Write a base URI directive (@base) to set the in-scope base URI
 */
void
raptor_turtle_writer_base(raptor_turtle_writer* turtle_writer,
                          raptor_uri* base_uri)
{
  if(base_uri) {
    raptor_iostream_write_counted_string(turtle_writer->iostr, "@base ", 6);
    raptor_turtle_writer_reference(turtle_writer, base_uri);
    raptor_iostream_write_counted_string(turtle_writer->iostr, " .\n", 3);
  }
  
  if(turtle_writer->base_uri)
    raptor_free_uri(turtle_writer->base_uri);
  turtle_writer->base_uri=base_uri;
}


/**
 * raptor_turtle_writer_reference:
 * @turtle_writer: Turtle writer object
 * @uri: URI to write
 *
 * Write a URI to the Turtle writer.
 *
 **/
void
raptor_turtle_writer_reference(raptor_turtle_writer* turtle_writer, 
                               raptor_uri* uri)
{
  unsigned char* uri_str;
  size_t length;
  
  uri_str = raptor_uri_to_relative_counted_uri_string(turtle_writer->base_uri, uri, &length);

  raptor_iostream_write_byte(turtle_writer->iostr, '<');
  if(uri_str)
    raptor_iostream_write_string_ntriples(turtle_writer->iostr,
                                          uri_str, length, '>');
  raptor_iostream_write_byte(turtle_writer->iostr, '>');

  RAPTOR_FREE(cstring, uri_str);
}


/**
 * raptor_turtle_writer_qname:
 * @turtle_writer: Turtle writer object
 * @qname: qname to write
 *
 * Write a QName to the Turtle writer.
 *
 **/
void
raptor_turtle_writer_qname(raptor_turtle_writer* turtle_writer,
                           raptor_qname* qname)
{
  raptor_iostream* iostr=turtle_writer->iostr;
  
  if(qname->nspace && qname->nspace->prefix_length > 0)
    raptor_iostream_write_counted_string(iostr, qname->nspace->prefix,
                                         qname->nspace->prefix_length);
  raptor_iostream_write_byte(iostr, ':');
  
  raptor_iostream_write_counted_string(iostr, qname->local_name,
                                       qname->local_name_length);
  return;
}


/**
 * raptor_iostream_write_string_python:
 * @iostr: #raptor_iostream to write to
 * @string: UTF-8 string to write
 * @len: length of UTF-8 string
 * @delim: Terminating delimiter character for string (such as " or >)
 * or \0 for no escaping.
 * @flags: flags 0=N-Triples mode, 1=Turtle (allow raw UTF-8), 2=Turtle long string (allow raw UTF-8), 3=JSON
 *
 * Write a UTF-8 string using Python-style escapes (N-Triples, Turtle, JSON) to an iostream.
 * 
 * Return value: non-0 on failure such as bad UTF-8 encoding.
 **/
int
raptor_iostream_write_string_python(raptor_iostream *iostr,
                                    const unsigned char *string,
                                    size_t len,
                                    const char delim,
                                    int flags)
{
  unsigned char c;
  int unichar_len;
  raptor_unichar unichar;

  if(flags < 0 || flags > 3)
    return 1;
  
  for(; (c=*string); string++, len--) {
    if((delim && c == delim && (delim=='\'' || delim == '"')) ||
       c == '\\') {
      raptor_iostream_write_byte(iostr, '\\');
      raptor_iostream_write_byte(iostr, c);
      continue;
    }
    if(delim && c == delim) {
      raptor_iostream_write_counted_string(iostr, "\\u", 2);
      raptor_iostream_format_hexadecimal(iostr, c, 4);
      continue;
    }
    
    if(flags != 2) {
      /* N-Triples, Turtle or JSON */

      /* Note: NTriples is ASCII */
      if(c == 0x09) {
        raptor_iostream_write_counted_string(iostr, "\\t", 2);
        continue;
      } else if((flags == 3) && c == 0x08) {
        /* JSON has \b for backspace */
        raptor_iostream_write_counted_string(iostr, "\\b", 2);
        continue;
      } else if(c == 0x0a) {
        raptor_iostream_write_counted_string(iostr, "\\n", 2);
        continue;
      } else if((flags == 3) && c == 0x0b) {
        /* JSON has \f for formfeed */
        raptor_iostream_write_counted_string(iostr, "\\f", 2);
        continue;
      } else if(c == 0x0d) {
        raptor_iostream_write_counted_string(iostr, "\\r", 2);
        continue;
      } else if(c < 0x20|| c == 0x7f) {
        raptor_iostream_write_counted_string(iostr, "\\u", 2);
        raptor_iostream_format_hexadecimal(iostr, c, 4);
        continue;
      } else if(c < 0x80) {
        raptor_iostream_write_byte(iostr, c);
        continue;
      }
    } else if(c < 0x80) {
      /* Turtle long string has no escapes except delim */
      raptor_iostream_write_byte(iostr, c);
      continue;
    } 
    
    /* It is unicode */
    
    unichar_len=raptor_utf8_to_unicode_char(NULL, string, len);
    if(unichar_len < 0 || unichar_len > (int)len)
      /* UTF-8 encoding had an error or ended in the middle of a string */
      return 1;

    if(flags >= 1 && flags <= 3) {
      /* Turtle and JSON are UTF-8 - no need to escape */
      raptor_iostream_write_counted_string(iostr, string, unichar_len);
    } else {
      unichar_len=raptor_utf8_to_unicode_char(&unichar, string, len);

      if(unichar < 0x10000) {
        raptor_iostream_write_counted_string(iostr, "\\u", 2);
        raptor_iostream_format_hexadecimal(iostr, unichar, 4);
      } else {
        raptor_iostream_write_counted_string(iostr, "\\U", 2);
        raptor_iostream_format_hexadecimal(iostr, unichar, 8);
      }
    }
    
    unichar_len--; /* since loop does len-- */
    string += unichar_len; len -= unichar_len;

  }

  return 0;
}


/**
 * raptor_iostream_write_string_turtle:
 * @iostr: #raptor_iostream to write to
 * @string: UTF-8 string to write
 * @len: length of UTF-8 string
 *
 * Write an UTF-8 string using Turtle "longString" triple quoting to
 * an iostream.
 *
 **/
void
raptor_iostream_write_string_turtle(raptor_iostream *iostr,
                                    const unsigned char *string, size_t len)
{
  raptor_iostream_write_string_python(iostr, string, len, '"', 1);
}



/**
 * raptor_turtle_writer_quoted_counted_string:
 * @turtle_writer: Turtle writer object
 * @s: string to write
 * @len: string length
 *
 * Write a Turtle escaped-string inside double quotes to the writer.
 *
 * Return value: non-0 on failure
 **/
int
raptor_turtle_writer_quoted_counted_string(raptor_turtle_writer* turtle_writer,
                                           const unsigned char *s, size_t len)
{
  const unsigned char *quotes=(const unsigned char *)"\"\"\"\"";
  const unsigned char *q;
  size_t q_len;
  int flags;
  
  if(!s)
    return 1;
  
  /* Turtle """longstring""" (2) or "string" (1) */
  flags=raptor_turtle_writer_contains_newline(s) ? 2 : 1;
  q=(flags == 2) ? quotes : quotes+2;
  q_len=(q == quotes) ? 3 : 1;
  raptor_iostream_write_counted_string(turtle_writer->iostr, q, q_len);
  raptor_iostream_write_string_python(turtle_writer->iostr,
                                      s, strlen((const char*)s), '"', flags);
  raptor_iostream_write_counted_string(turtle_writer->iostr, q, q_len);

  return 0;
}


/**
 * raptor_turtle_writer_literal:
 * @turtle_writer: Turtle writer object
 * @nstack: Namespace stack for making a QName for datatype URI
 * @s: literal string to write (SHARED)
 * @lang: language tag (may be NULL)
 * @datatype: datatype URI (may be NULL)
 *
 * Write a literal (possibly with lang and datatype) to the Turtle writer.
 *
 * Return value: non-0 on failure
 **/
int
raptor_turtle_writer_literal(raptor_turtle_writer* turtle_writer,
                             raptor_namespace_stack *nstack,
                             const unsigned char* s, const unsigned char* lang,
                             raptor_uri* datatype)
{
  /* DBL_MAX = 309 decimal digits */
  #define INT_MAX_LEN 309 

  /* DBL_EPSILON = 52 digits */
  #define FRAC_MAX_LEN 52

  int written = 0;

  /* typed literal special cases */
  if(datatype) {
    /* integer, double, decimal */
    if(raptor_uri_equals(datatype, turtle_writer->xsd_integer_uri) ||
       raptor_uri_equals(datatype, turtle_writer->xsd_double_uri) ||
       raptor_uri_equals(datatype, turtle_writer->xsd_decimal_uri)) {
      raptor_iostream_write_string(turtle_writer->iostr, s);
      written = 1;
    /* boolean */
    } else if(raptor_uri_equals(datatype, turtle_writer->xsd_boolean_uri)) {
      if(!strcmp((const char*)s, "0") || !strcmp((const char*)s, "false")) {
        raptor_iostream_write_string(turtle_writer->iostr, "false");
        written = 1;
      } else if(!strcmp((const char*)s, "1") || !strcmp((const char*)s, "true")) {
        raptor_iostream_write_string(turtle_writer->iostr, "true");
        written = 1;
      } else {
        turtle_writer->error_handler(turtle_writer->error_data, "Illegal value for xsd:boolean literal.");
      }
    }
  }

  if(written)
    return 0;
    
  if(raptor_turtle_writer_quoted_counted_string(turtle_writer, s,
                                                strlen((const char*)s)))
    return 1;

  /* typed literal, not a special case */
  if(datatype) {
    raptor_qname* qname;

    raptor_iostream_write_string(turtle_writer->iostr, "^^");
    qname = raptor_namespaces_qname_from_uri(nstack, datatype, 10);
    if(qname) {
      raptor_turtle_writer_qname(turtle_writer, qname);
      raptor_free_qname(qname);
    } else
      raptor_turtle_writer_reference(turtle_writer, datatype);
  } else if(lang) {
    /* literal with language tag */
    raptor_iostream_write_byte(turtle_writer->iostr, '@');
    raptor_iostream_write_string(turtle_writer->iostr, lang);
  }

  return 0;
}


/**
 * raptor_turtle_writer_comment:
 * @turtle_writer: Turtle writer object
 * @s: comment string to write
 *
 * Write a Turtle comment to the Turtle writer.
 *
 **/
void
raptor_turtle_writer_comment(raptor_turtle_writer* turtle_writer,
                             const unsigned char *string)
{
  unsigned char c;
  size_t len = strlen((const char*)string);

  raptor_iostream_write_counted_string(turtle_writer->iostr,
                                       (const unsigned char*)"# ", 2);

  for(; (c=*string); string++, len--) {
    if(c == '\n') {
      raptor_turtle_writer_newline(turtle_writer);
      raptor_iostream_write_counted_string(turtle_writer->iostr,
                                           (const unsigned char*)"# ", 2);
    } else if(c != '\r') { 
      /* skip carriage returns (windows... *sigh*) */
      raptor_iostream_write_byte(turtle_writer->iostr, c);
    }
  }
  
  raptor_turtle_writer_newline(turtle_writer);
}


/**
 * raptor_turtle_writer_features_enumerate:
 * @feature: feature enumeration (0+)
 * @name: pointer to store feature short name (or NULL)
 * @uri: pointer to store feature URI (or NULL)
 * @label: pointer to feature label (or NULL)
 *
 * Get list of turtle_writer features.
 * 
 * If uri is not NULL, a pointer to a new raptor_uri is returned
 * that must be freed by the caller with raptor_free_uri().
 *
 * Return value: 0 on success, <0 on failure, >0 if feature is unknown
 **/
int
raptor_turtle_writer_features_enumerate(const raptor_feature feature,
                                        const char **name, 
                                        raptor_uri **uri, const char **label)
{
  return raptor_features_enumerate_common(feature, name, uri, label, 8);
}


/**
 * raptor_turtle_writer_set_feature:
 * @turtle_writer: #raptor_turtle_writer turtle_writer object
 * @feature: feature to set from enumerated #raptor_feature values
 * @value: integer feature value (0 or larger)
 *
 * Set turtle_writer features with integer values.
 * 
 * The allowed features are available via raptor_features_enumerate().
 *
 * Return value: non 0 on failure or if the feature is unknown
 **/
int
raptor_turtle_writer_set_feature(raptor_turtle_writer *turtle_writer, 
                                 raptor_feature feature, int value)
{
  if(value < 0)
    return -1;
  
  switch(feature) {
    case RAPTOR_FEATURE_WRITER_AUTO_INDENT:
      if(value)
        turtle_writer->flags |= TURTLE_WRITER_AUTO_INDENT;
      else
        turtle_writer->flags &= ~TURTLE_WRITER_AUTO_INDENT;        
      break;

    case RAPTOR_FEATURE_WRITER_INDENT_WIDTH:
      turtle_writer->indent = value;
      break;
    
    case RAPTOR_FEATURE_WRITER_AUTO_EMPTY:
    case RAPTOR_FEATURE_WRITER_XML_VERSION:
    case RAPTOR_FEATURE_WRITER_XML_DECLARATION:
      break;
        
    /* parser features */
    case RAPTOR_FEATURE_SCANNING:
    case RAPTOR_FEATURE_ASSUME_IS_RDF:
    case RAPTOR_FEATURE_ALLOW_NON_NS_ATTRIBUTES:
    case RAPTOR_FEATURE_ALLOW_OTHER_PARSETYPES:
    case RAPTOR_FEATURE_ALLOW_BAGID:
    case RAPTOR_FEATURE_ALLOW_RDF_TYPE_RDF_LIST:
    case RAPTOR_FEATURE_NORMALIZE_LANGUAGE:
    case RAPTOR_FEATURE_NON_NFC_FATAL:
    case RAPTOR_FEATURE_WARN_OTHER_PARSETYPES:
    case RAPTOR_FEATURE_CHECK_RDF_ID:
    case RAPTOR_FEATURE_HTML_TAG_SOUP:
    case RAPTOR_FEATURE_MICROFORMATS:
    case RAPTOR_FEATURE_HTML_LINK:
    case RAPTOR_FEATURE_WWW_TIMEOUT:

    /* Shared */
    case RAPTOR_FEATURE_NO_NET:

    /* XML writer features */
    case RAPTOR_FEATURE_RELATIVE_URIS:
    case RAPTOR_FEATURE_START_URI:

    /* DOT serializer features */
    case RAPTOR_FEATURE_RESOURCE_BORDER:
    case RAPTOR_FEATURE_LITERAL_BORDER:
    case RAPTOR_FEATURE_BNODE_BORDER:
    case RAPTOR_FEATURE_RESOURCE_FILL:
    case RAPTOR_FEATURE_LITERAL_FILL:
    case RAPTOR_FEATURE_BNODE_FILL:

    /* JSON serializer features */
    case RAPTOR_FEATURE_JSON_CALLBACK:
    case RAPTOR_FEATURE_JSON_EXTRA_DATA:
    
    /* Turtle serializer feature */
    case RAPTOR_FEATURE_WRITE_BASE_URI:

    /* WWW feature */
    case RAPTOR_FEATURE_WWW_HTTP_CACHE_CONTROL:
    case RAPTOR_FEATURE_WWW_HTTP_USER_AGENT:
      
    default:
      return -1;
      break;
  }

  return 0;
}


/**
 * raptor_turtle_writer_set_feature_string:
 * @turtle_writer: #raptor_turtle_writer turtle_writer object
 * @feature: feature to set from enumerated #raptor_feature values
 * @value: feature value
 *
 * Set turtle_writer features with string values.
 * 
 * The allowed features are available via raptor_turtle_writer_features_enumerate().
 * If the feature type is integer, the value is interpreted as an integer.
 *
 * Return value: non 0 on failure or if the feature is unknown
 **/
int
raptor_turtle_writer_set_feature_string(raptor_turtle_writer *turtle_writer, 
                                        raptor_feature feature, 
                                        const unsigned char *value)
{
  int value_is_string=(raptor_feature_value_type(feature) == 1);
  if(!value_is_string)
    return raptor_turtle_writer_set_feature(turtle_writer, feature, 
                                            atoi((const char*)value));
  else
    return -1;
}


/**
 * raptor_turtle_writer_get_feature:
 * @turtle_writer: #raptor_turtle_writer serializer object
 * @feature: feature to get value
 *
 * Get various turtle_writer features.
 * 
 * The allowed features are available via raptor_features_enumerate().
 *
 * Note: no feature value is negative
 *
 * Return value: feature value or < 0 for an illegal feature
 **/
int
raptor_turtle_writer_get_feature(raptor_turtle_writer *turtle_writer, 
                                 raptor_feature feature)
{
  int result= -1;

  switch(feature) {
    case RAPTOR_FEATURE_WRITER_AUTO_INDENT:
      result=TURTLE_WRITER_AUTO_INDENT(turtle_writer);
      break;

    case RAPTOR_FEATURE_WRITER_INDENT_WIDTH:
      result=turtle_writer->indent;
      break;
    
    /* writer features */
    case RAPTOR_FEATURE_WRITER_AUTO_EMPTY:
    case RAPTOR_FEATURE_WRITER_XML_VERSION:
    case RAPTOR_FEATURE_WRITER_XML_DECLARATION:
      
    /* parser features */
    case RAPTOR_FEATURE_SCANNING:
    case RAPTOR_FEATURE_ASSUME_IS_RDF:
    case RAPTOR_FEATURE_ALLOW_NON_NS_ATTRIBUTES:
    case RAPTOR_FEATURE_ALLOW_OTHER_PARSETYPES:
    case RAPTOR_FEATURE_ALLOW_BAGID:
    case RAPTOR_FEATURE_ALLOW_RDF_TYPE_RDF_LIST:
    case RAPTOR_FEATURE_NORMALIZE_LANGUAGE:
    case RAPTOR_FEATURE_NON_NFC_FATAL:
    case RAPTOR_FEATURE_WARN_OTHER_PARSETYPES:
    case RAPTOR_FEATURE_CHECK_RDF_ID:
    case RAPTOR_FEATURE_HTML_TAG_SOUP:
    case RAPTOR_FEATURE_MICROFORMATS:
    case RAPTOR_FEATURE_HTML_LINK:
    case RAPTOR_FEATURE_WWW_TIMEOUT:

    /* Shared */
    case RAPTOR_FEATURE_NO_NET:

    /* XML writer features */
    case RAPTOR_FEATURE_RELATIVE_URIS:
    case RAPTOR_FEATURE_START_URI:

    /* DOT serializer features */
    case RAPTOR_FEATURE_RESOURCE_BORDER:
    case RAPTOR_FEATURE_LITERAL_BORDER:
    case RAPTOR_FEATURE_BNODE_BORDER:
    case RAPTOR_FEATURE_RESOURCE_FILL:
    case RAPTOR_FEATURE_LITERAL_FILL:
    case RAPTOR_FEATURE_BNODE_FILL:

    /* JSON serializer features */
    case RAPTOR_FEATURE_JSON_CALLBACK:
    case RAPTOR_FEATURE_JSON_EXTRA_DATA:
    
    /* Turtle serializer feature */
    case RAPTOR_FEATURE_WRITE_BASE_URI:

    /* WWW feature */
    case RAPTOR_FEATURE_WWW_HTTP_CACHE_CONTROL:
    case RAPTOR_FEATURE_WWW_HTTP_USER_AGENT:
      
    default:
      break;
  }
  
  return result;
}


/**
 * raptor_turtle_writer_get_feature_string:
 * @turtle_writer: #raptor_turtle_writer serializer object
 * @feature: feature to get value
 *
 * Get turtle_writer features with string values.
 * 
 * The allowed features are available via raptor_features_enumerate().
 *
 * Return value: feature value or NULL for an illegal feature or no value
 **/
const unsigned char *
raptor_turtle_writer_get_feature_string(raptor_turtle_writer *turtle_writer, 
                                        raptor_feature feature)
{
  return NULL;
}

#endif



#ifdef STANDALONE

/* one more prototype */
int main(int argc, char *argv[]);


const unsigned char *base_uri_string=(const unsigned char*)"http://example.org/base#";

const unsigned char* longstr=(const unsigned char*)"it's quoted\nand has newlines, \"s <> and\n\ttabbing";

#define OUT_BYTES_COUNT 149

int
main(int argc, char *argv[]) 
{
  const char *program=raptor_basename(argv[0]);
  const raptor_uri_handler *uri_handler;
  void *uri_context;
  raptor_iostream *iostr;
  raptor_namespace_stack *nstack;
  raptor_namespace* ex_ns;
  raptor_turtle_writer* turtle_writer;
  raptor_uri* base_uri;
  raptor_qname* el_name;
  unsigned long count;
  raptor_uri* datatype;
  
  /* for raptor_new_iostream_to_string */
  void *string=NULL;
  size_t string_len=0;

  raptor_init();
  
  iostr=raptor_new_iostream_to_string(&string, &string_len, NULL);
  if(!iostr) {
    fprintf(stderr, "%s: Failed to create iostream to string\n", program);
    exit(1);
  }

  raptor_uri_get_handler(&uri_handler, &uri_context);

  nstack=raptor_new_namespaces(uri_handler, uri_context,
                               NULL, NULL, /* errors */
                               1);

  base_uri=raptor_new_uri(base_uri_string);

  turtle_writer=raptor_new_turtle_writer(base_uri, 1,
                                         nstack,
                                         uri_handler, uri_context,
                                         iostr,
                                         NULL, NULL /* errors */
                                         );
  if(!turtle_writer) {
    fprintf(stderr, "%s: Failed to create turtle_writer to iostream\n", program);
    exit(1);
  }

  raptor_turtle_writer_set_feature(turtle_writer, 
                                   RAPTOR_FEATURE_WRITER_AUTO_INDENT, 1);

  ex_ns=raptor_new_namespace(nstack,
                              (const unsigned char*)"ex",
                              (const unsigned char*)"http://example.org/ns#",
                              0);


  raptor_turtle_writer_namespace_prefix(turtle_writer, ex_ns);

  raptor_turtle_writer_reference(turtle_writer, base_uri);
  
  raptor_turtle_writer_increase_indent(turtle_writer);
  raptor_turtle_writer_newline(turtle_writer);
  
  raptor_turtle_writer_raw(turtle_writer, (const unsigned char*)"ex:foo ");

  raptor_turtle_writer_quoted_counted_string(turtle_writer, longstr,
                                             strlen((const char*)longstr));
  raptor_turtle_writer_raw_counted(turtle_writer,
                                   (const unsigned char*)" ;", 2);
  raptor_turtle_writer_newline(turtle_writer);

  el_name=raptor_new_qname_from_namespace_local_name(ex_ns,
                                                     (const unsigned char*)"bar", 
                                                     NULL);

  raptor_turtle_writer_qname(turtle_writer, el_name);
  raptor_free_qname(el_name);

  raptor_turtle_writer_raw_counted(turtle_writer, (const unsigned char*)" ", 1);

  datatype=raptor_new_uri((const unsigned char*)"http://www.w3.org/2001/XMLSchema#decimal");
  raptor_turtle_writer_literal(turtle_writer, nstack,
                               (const unsigned char*)"10.0", NULL,
                               datatype);
  raptor_free_uri(datatype);

  raptor_turtle_writer_newline(turtle_writer);

  raptor_turtle_writer_decrease_indent(turtle_writer);

  raptor_turtle_writer_raw_counted(turtle_writer, (const unsigned char*)".", 1);
  raptor_turtle_writer_newline(turtle_writer);

  
  raptor_free_turtle_writer(turtle_writer);

  raptor_free_namespace(ex_ns);

  raptor_free_namespaces(nstack);

  raptor_free_uri(base_uri);

  
  count=raptor_iostream_tell(iostr);

#if RAPTOR_DEBUG > 1
  fprintf(stderr, "%s: Freeing iostream\n", program);
#endif
  raptor_free_iostream(iostr);

  if(count != OUT_BYTES_COUNT) {
    fprintf(stderr, "%s: I/O stream wrote %d bytes, expected %d\n", program,
            (int)count, (int)OUT_BYTES_COUNT);
    fputs("[[", stderr);
    (void)fwrite(string, 1, string_len, stderr);
    fputs("]]\n", stderr);
    return 1;
  }
  
  if(!string) {
    fprintf(stderr, "%s: I/O stream failed to create a string\n", program);
    return 1;
  }
  string_len=strlen((const char*)string);
  if(string_len != count) {
    fprintf(stderr, "%s: I/O stream created a string length %d, expected %d\n", program, (int)string_len, (int)count);
    return 1;
  }

#if RAPTOR_DEBUG > 1
  fprintf(stderr, "%s: Made Turtle string of %d bytes\n", program, (int)string_len);
  fputs("[[", stderr);
  (void)fwrite(string, 1, string_len, stderr);
  fputs("]]\n", stderr);
#endif

  raptor_free_memory(string);
  

  raptor_finish();

  /* keep gcc -Wall happy */
  return(0);
}

#endif
