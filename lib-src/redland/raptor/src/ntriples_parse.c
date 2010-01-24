/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * ntriples_parse.c - Raptor N-Triples Parser implementation
 *
 * N-Triples
 * http://www.w3.org/TR/rdf-testcases/#ntriples
 *
 * Copyright (C) 2001-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2001-2005, University of Bristol, UK http://www.bristol.ac.uk/
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

/* Set RAPTOR_DEBUG to > 1 to get lots of buffer related debugging */
/*
#undef RAPTOR_DEBUG
#define RAPTOR_DEBUG 2
*/

/* Prototypes for local functions */
static void raptor_ntriples_generate_statement(raptor_parser* parser, const unsigned char *subject, const raptor_ntriples_term_type subject_type, const unsigned char *predicate, const raptor_ntriples_term_type predicate_type, const void *object, const raptor_ntriples_term_type object_type, const unsigned char *object_literal_language, const unsigned char *object_literal_datatype);

/*
 * NTriples parser object
 */
struct raptor_ntriples_parser_context_s {
  /* current line */
  unsigned char *line;
  /* current line length */
  int line_length;
  /* current char in line buffer */
  int offset;

  char last_char;
  
  /* static statement for use in passing to user code */
  raptor_statement statement;
};


typedef struct raptor_ntriples_parser_context_s raptor_ntriples_parser_context;



/**
 * raptor_ntriples_parse_init:
 *
 * Initialise the Raptor NTriples parser.
 *
 * Return value: non 0 on failure
 **/

static int
raptor_ntriples_parse_init(raptor_parser* rdf_parser, const char *name)
{
  /*raptor_ntriples_parser_context *ntriples_parser=(raptor_ntriples_parser_context*)rdf_parser->context; */
  return 0;
}


/* PUBLIC FUNCTIONS */


/*
 * raptor_ntriples_parse_terminate - Free the Raptor NTriples parser
 * @rdf_parser: parser object
 * 
 **/
static void
raptor_ntriples_parse_terminate(raptor_parser* rdf_parser)
{
  raptor_ntriples_parser_context *ntriples_parser=(raptor_ntriples_parser_context*)rdf_parser->context;
  if(ntriples_parser->line_length)
    RAPTOR_FREE(cdata, ntriples_parser->line);
}


static const char * const term_type_strings[]={
  "URIref",
  "bnodeID",
  "Literal"
};


#ifndef RAPTOR_DISABLE_DEPRECATED
/**
 * raptor_ntriples_term_as_string:
 * @term: N-Triples term.
 * 
 * Get a label for a #raptor_ntriples_term_type.
 * 
 * @deprecated: an internal debug function, do not use.
 *
 * Return value: a pointer to a constant string.
 **/
const char *
raptor_ntriples_term_as_string(raptor_ntriples_term_type term)
{
  return term_type_strings[(int)term];
}
#endif


static void
raptor_ntriples_generate_statement(raptor_parser* parser, 
                                   const unsigned char *subject,
                                   const raptor_ntriples_term_type subject_type,
                                   const unsigned char *predicate,
                                   const raptor_ntriples_term_type predicate_type,
                                   const void *object,
                                   const raptor_ntriples_term_type object_type,
                                   const unsigned char *object_literal_language,
                                   const unsigned char *object_literal_datatype)
{
  /* raptor_ntriples_parser_context *ntriples_parser=(raptor_ntriples_parser_context*)parser->context; */
  raptor_statement *statement=&parser->statement;
  raptor_uri *subject_uri=NULL;
  raptor_uri *predicate_uri=NULL;
  raptor_uri *object_uri=NULL;
  raptor_uri *datatype_uri=NULL;

  /* Two choices for subject from N-Triples */
  if(subject_type == RAPTOR_NTRIPLES_TERM_TYPE_BLANK_NODE) {
    statement->subject=subject;
    statement->subject_type=RAPTOR_IDENTIFIER_TYPE_ANONYMOUS;
  } else {
    subject_uri=raptor_new_uri(subject);
    if(!subject_uri)
      goto cleanup;
    statement->subject=subject_uri;
    statement->subject_type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
  }

  if(object_literal_datatype) {
    datatype_uri=raptor_new_uri(object_literal_datatype);
    if(!datatype_uri)
      goto cleanup;
    object_literal_language=NULL;
  }

  /* Predicates in N-Triples are URIs but check for bad ordinals */
  if(!strncmp((const char*)predicate, "http://www.w3.org/1999/02/22-rdf-syntax-ns#_", 44)) {
    int predicate_ordinal=raptor_check_ordinal(predicate+44);
    if(predicate_ordinal <= 0)
      raptor_parser_error(parser, "Illegal ordinal value %d in property '%s'.", predicate_ordinal, predicate);
  }
  
  predicate_uri=raptor_new_uri(predicate);
  if(!predicate_uri)
    goto cleanup;
  statement->predicate_type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
  statement->predicate=predicate_uri;
  
  /* Three choices for object from N-Triples */
  statement->object_literal_language=NULL;
  statement->object_literal_datatype=NULL;
  if(object_type == RAPTOR_NTRIPLES_TERM_TYPE_URI_REF) {
    object_uri=raptor_new_uri((const unsigned char*)object);
    if(!object_uri)
      goto cleanup;
    statement->object=object_uri;
    statement->object_type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
  } else if(object_type == RAPTOR_NTRIPLES_TERM_TYPE_BLANK_NODE) {
    statement->object=object;
    statement->object_type=RAPTOR_IDENTIFIER_TYPE_ANONYMOUS;
  } else { 
    statement->object_type=RAPTOR_IDENTIFIER_TYPE_LITERAL;
    statement->object=object;
    statement->object_literal_language=object_literal_language;
    statement->object_literal_datatype=datatype_uri;
  }

  if(!parser->statement_handler)
    goto cleanup;

  /* Generate the statement; or is it fact? */
  (*parser->statement_handler)(parser->user_data, statement);

  cleanup:
  if(subject_uri)
    raptor_free_uri(subject_uri);
  if(predicate_uri)
    raptor_free_uri(predicate_uri);
  if(object_uri)
    raptor_free_uri(object_uri);
  if(datatype_uri)
    raptor_free_uri(datatype_uri);
}


/* These are for 7-bit ASCII and not locale-specific */
#define IS_ASCII_ALPHA(c) (((c)>0x40 && (c)<0x5B) || ((c)>0x60 && (c)<0x7B))
#define IS_ASCII_UPPER(c) ((c)>0x40 && (c)<0x5B)
#define IS_ASCII_DIGIT(c) ((c)>0x2F && (c)<0x3A)
#define IS_ASCII_PRINT(c) ((c)>0x1F && (c)<0x7F)
#define TO_ASCII_LOWER(c) ((c)+0x20)

typedef enum {
  RAPTOR_TERM_CLASS_URI,      /* ends on > */
  RAPTOR_TERM_CLASS_BNODEID,  /* ends on first non [A-Za-z][A-Za-z0-9]* */
  RAPTOR_TERM_CLASS_STRING,   /* ends on non-escaped " */
  RAPTOR_TERM_CLASS_LANGUAGE, /* ends on first non [a-z0-9]+ ('-' [a-z0-9]+ )? */
  RAPTOR_TERM_CLASS_FULL      /* the entire string is used */
} raptor_ntriples_term_class;


static int 
raptor_ntriples_term_valid(unsigned char c, int position, 
                           raptor_ntriples_term_class term_class) 
{
  int result=0;

  switch(term_class) {
    case RAPTOR_TERM_CLASS_URI:
      /* ends on > */
      result=(c!= '>');
      break;
      
    case RAPTOR_TERM_CLASS_BNODEID:  
      /* ends on first non [A-Za-z][A-Za-z0-9]* */
      result=IS_ASCII_ALPHA(c);
      if(position)
        result = (result || IS_ASCII_DIGIT(c));
      break;
      
    case RAPTOR_TERM_CLASS_STRING:
      /* ends on " */
      result=(c!= '"');
      break;

    case RAPTOR_TERM_CLASS_LANGUAGE:
      /* ends on first non [a-z0-9]+ ('-' [a-z0-9]+ )? */
      result=(IS_ASCII_ALPHA(c) || IS_ASCII_DIGIT(c));
      if(position)
        result = (result || c=='-');
      break;
      
    case RAPTOR_TERM_CLASS_FULL:
      result=1;
      break;
      
    default:
      RAPTOR_FATAL2("Unknown ntriples term %d", term_class);
  }

  return result;
}


/*
 * raptor_ntriples_term - Parse an N-Triples term with escapes
 * @parser: NTriples parser
 * @start: pointer to starting character of string (in)
 * @dest: destination of string (in)
 * @lenp: pointer to length of string (in/out)
 * @dest_lenp: pointer to length of destination string (out)
 * @end_char: string ending character
 * @class: string class
 * @allow_utf8: Non-0 if UTF-8 chars are allowed in the term
 * 
 * N-Triples strings/URIs are written in ASCII at present; characters
 * outside the printable ASCII range are discarded with a warning.
 * See the grammar for full details of the allowed ranges.
 *
 * If the class is RAPTOR_TERM_CLASS_FULL, the end_char is ignored.
 *
 * UTF-8 is only allowed if allow_utf8 is non-0, otherwise the
 * string is US-ASCII and only the \u and \U esapes are allowed.
 * If enabled, both are allowed.
 *
 * Return value: Non 0 on failure
 **/
static int
raptor_ntriples_term(raptor_parser* rdf_parser, 
                     const unsigned char **start, unsigned char *dest, 
                     size_t *lenp, size_t *dest_lenp,
                     char end_char,
                     raptor_ntriples_term_class term_class,
                     int allow_utf8)
{
  const unsigned char *p=*start;
  unsigned char c='\0';
  size_t ulen=0;
  unsigned long unichar=0;
  unsigned int position=0;
  int end_char_seen=0;

  if(term_class == RAPTOR_TERM_CLASS_FULL)
    end_char='\0';
  
  /* find end of string, fixing backslashed characters on the way */
  while(*lenp > 0) {
    c = *p;

    p++;
    (*lenp)--;
    rdf_parser->locator.column++;
    rdf_parser->locator.byte++;

    if(allow_utf8) {
      if(c > 0x7f) {
        /* just copy the UTF-8 bytes through */
        size_t unichar_len=raptor_utf8_to_unicode_char(NULL, (const unsigned char*)p-1, 1+*lenp);
        if(unichar_len > *lenp) {
          raptor_parser_error(rdf_parser, "UTF-8 encoding error at character %d (0x%02X) found.", c, c);
          /* UTF-8 encoding had an error or ended in the middle of a string */
          return 1;
        }
        memcpy(dest, p-1, unichar_len);
        dest+= unichar_len;

        unichar_len--; /* p, *lenp were moved on by 1 earlier */
        
        p += unichar_len;
        (*lenp) -= unichar_len;
        rdf_parser->locator.column+= unichar_len;
        rdf_parser->locator.byte+= unichar_len;
        continue;
      }
    } else if(!IS_ASCII_PRINT(c)) {
      /* This is an ASCII check, not a printable character check 
       * so isprint() is not appropriate, since that is a locale check.
       */
      raptor_parser_error(rdf_parser, "Non-printable ASCII character %d (0x%02X) found.", c, c);
      continue;
    }
    
    if(c != '\\') {
      /* finish at non-backslashed end_char */
      if(end_char && c == end_char) {
        end_char_seen=1;
        break;
      }

      if(!raptor_ntriples_term_valid(c, position, term_class)) {
        if(end_char) {
          /* end char was expected, so finding an invalid thing is an error */
          raptor_parser_error(rdf_parser, "Missing terminating '%c' (found '%c')", end_char, c);
          return 0;
        } else {
          /* it's the end - so rewind 1 to save next char */
          p--;
          (*lenp)++;
          rdf_parser->locator.column--;
          rdf_parser->locator.byte--;
          break;
        }
      }
      
      /* otherwise store and move on */
      *dest++=c;
      position++;
      continue;
    }

    if(!*lenp) {
      if(term_class != RAPTOR_TERM_CLASS_FULL)
        raptor_parser_error(rdf_parser, "\\ at end of line");
      return 0;
    }

    c = *p;

    p++;
    (*lenp)--;
    rdf_parser->locator.column++;
    rdf_parser->locator.byte++;

    switch(c) {
      case '"':
      case '\\':
        *dest++=c;
        break;
      case 'n':
        *dest++='\n';
        break;
      case 'r':
        *dest++='\r';
        break;
      case 't':
        *dest++='\t';
        break;
      case 'u':
      case 'U':
        ulen=(c == 'u') ? 4 : 8;
        
        if(*lenp < ulen) {
          raptor_parser_error(rdf_parser, "%c over end of line", c);
          return 0;
        }

        if(1) {
          int n;

          n=sscanf((const char*)p, ((ulen == 4) ? "%04lx" : "%08lx"), &unichar);
          if(n != 1) {
            raptor_parser_error(rdf_parser, "Illegal Uncode escape '%c%s...'", c, p);
            break;
          }
        }

        p+=ulen;
        (*lenp)-=ulen;
        rdf_parser->locator.column+=ulen;
        rdf_parser->locator.byte+=ulen;
        
        if(unichar > 0x10ffff) {
          raptor_parser_error(rdf_parser, "Illegal Unicode character with code point #x%lX.", unichar);
          break;
        }
          
        dest+=raptor_unicode_char_to_utf8(unichar, dest);
        break;

      default:
        raptor_parser_error(rdf_parser,
                            "Illegal string escape \\%c in \"%s\"", c, 
                            (char*)start);
        return 0;
    }

    position++;
  } /* end while */

  
  if(end_char && !end_char_seen) {
    raptor_parser_error(rdf_parser, "Missing terminating '%c' before end of line.", end_char);
    return 1;
  }

  /* terminate dest, can be shorter than source */
  *dest='\0';

  if(dest_lenp)
    *dest_lenp=p-*start;

  *start=p;

  return 0;
}


#ifndef RAPTOR_DISABLE_DEPRECATED
/**
 * raptor_ntriples_string_as_utf8_string:
 * @rdf_parser: parser object
 * @src: data to read from
 * @len: size of data
 * @dest_lenp: pointer to length of destination (out) or NULL
 * 
 * Turn an N-Triples string with escapes into a UTF-8 string.
 *
 * @deprecated: This requires use of parser internals and was never in the public API header.
 * 
 * Return value: a new UTF-8 string
 **/
unsigned char*
raptor_ntriples_string_as_utf8_string(raptor_parser* rdf_parser, 
                                      const unsigned char *src, int len,  
                                      size_t *dest_lenp)
{
  const unsigned char *start=src;
  size_t length=len;
  unsigned char *dest;
  int rc;
  
  dest=(unsigned char*)RAPTOR_MALLOC(cstring, len+1);
  if(!dest)
    return NULL;

  rc=raptor_ntriples_term(rdf_parser, &start, dest, &length, dest_lenp,
                              '\0', RAPTOR_TERM_CLASS_FULL, 1);
  if(rc) {
    RAPTOR_FREE(cstring, dest);
    dest=NULL;
  }
  return dest;
}
#endif


static int
raptor_ntriples_parse_line(raptor_parser* rdf_parser,
                           unsigned char *buffer, size_t len)
{
  int i;
  unsigned char *p;
  unsigned char *dest;
  unsigned char *terms[3];
  int terms_allocated[3];
  size_t term_lengths[3];
  raptor_ntriples_term_type term_types[3];
  size_t term_length= 0;
  unsigned char *object_literal_language=NULL;
  unsigned char *object_literal_datatype=NULL;
  int rc=0;
  
  for(i=0; i<3; i++)
    terms_allocated[i]=0;

  /* ASSERTION:
   * p always points to first char we are considering
   * p[len-1] always points to last char
   */
  
  /* Handle empty  lines */
  if(!len)
    return 0;

#if defined(RAPTOR_DEBUG) && RAPTOR_DEBUG > 1
  RAPTOR_DEBUG3("handling line '%s' (%d bytes)\n", buffer, (unsigned int)len);
#endif
  
  p=buffer;

  while(len>0 && isspace((int)*p)) {
    p++;
    rdf_parser->locator.column++;
    rdf_parser->locator.byte++;
    len--;
  }

  /* Handle empty - all whitespace lines */
  if(!len)
    return 0;
  
  /* Handle comment lines */
  if(*p == '#')
    return 0;
  
  /* Remove trailing spaces */
  while(len>0 && isspace((int)p[len-1])) {
    p[len-1]='\0';
    len--;
  }

  /* can't be empty now - that would have been caught above */
  
  /* Check for terminating '.' */
  if(p[len-1] != '.') {
    /* Move current location to point to problem */
    rdf_parser->locator.column += len-2;
    rdf_parser->locator.byte += len-2;
    raptor_parser_error(rdf_parser, "Missing . at end of line");
    return 0;
  }

  p[len-1]='\0';
  len--;


  /* Must be triple */

  for(i=0; i<3; i++) {
    if(!len) {
      raptor_parser_error(rdf_parser, "Unexpected end of line");
      goto cleanup;
    }
    
    /* Expect either <URI> or _:name */
    if(i == 2) {
      if(*p != '<' && *p != '_' && *p != '"' && *p != 'x') {
        raptor_parser_error(rdf_parser, "Saw '%c', expected <URIref>, _:bnodeID or \"literal\"", *p);
        goto cleanup;
      }
      if(*p == 'x') {
        if(len < 4 || strncmp((const char*)p, "xml\"", 4)) {
          raptor_parser_error(rdf_parser, "Saw '%c', expected xml\"...\")", *p);
          goto cleanup;
        }
      }
    } else if(i == 1) {
      if(*p != '<') {
        raptor_parser_error(rdf_parser, "Saw '%c', expected <URIref>", *p);
        goto cleanup;
      }
    } else /* i==0 */ {
      if(*p != '<' && *p != '_') {
        raptor_parser_error(rdf_parser, "Saw '%c', expected <URIref> or _:bnodeID", *p);
        goto cleanup;
      }
    }

    switch(*p) {
      case '<':
        term_types[i]= RAPTOR_NTRIPLES_TERM_TYPE_URI_REF;
        
        dest=p;

        p++;
        len--;
        rdf_parser->locator.column++;
        rdf_parser->locator.byte++;

        if(raptor_ntriples_term(rdf_parser,
                                (const unsigned char**)&p, 
                                dest, &len, &term_length, 
                                '>', RAPTOR_TERM_CLASS_URI, 0)) {
          rc=1;
          goto cleanup;
        }
        break;

      case '"':
        term_types[i]= RAPTOR_NTRIPLES_TERM_TYPE_LITERAL;
        
        dest=p;

        p++;
        len--;
        rdf_parser->locator.column++;
        rdf_parser->locator.byte++;

        if(raptor_ntriples_term(rdf_parser,
                                (const unsigned char**)&p,
                                dest, &len, &term_length,
                                '"', RAPTOR_TERM_CLASS_STRING, 0)) {
          rc=1;
          goto cleanup;
        }
        
        if(len && (*p == '-' || *p == '@')) {
          if(*p == '-')
            raptor_parser_error(rdf_parser, "Old N-Triples language syntax using \"string\"-lang rather than \"string\"@lang.");

          object_literal_language=p;

          /* Skip - */
          p++;
          len--;
          rdf_parser->locator.column++;
          rdf_parser->locator.byte++;

          if(!len) {
            raptor_parser_error(rdf_parser, "Missing language after \"string\"-");
            goto cleanup;
          }
          

          if(raptor_ntriples_term(rdf_parser,
                                  (const unsigned char**)&p,
                                  object_literal_language, &len, NULL,
                                  '\0', RAPTOR_TERM_CLASS_LANGUAGE, 0)) {
            rc=1;
            goto cleanup;
          }
        }

        if(len >1 && *p == '^' && p[1] == '^') {

          object_literal_datatype=p;

          /* Skip ^^ */
          p+= 2;
          len-= 2;
          rdf_parser->locator.column+= 2;
          rdf_parser->locator.byte+= 2;

          if(!len || (len && *p != '<')) {
            raptor_parser_error(rdf_parser, "Missing datatype URI-ref in\"string\"^^<URI-ref> after ^^");
            goto cleanup;
          }

          p++;
          len--;
          rdf_parser->locator.column++;
          rdf_parser->locator.byte++;

          if(raptor_ntriples_term(rdf_parser,
                                  (const unsigned char**)&p,
                                  object_literal_datatype, &len, NULL,
                                  '>', RAPTOR_TERM_CLASS_URI, 0)) {
            rc=1;
            goto cleanup;
          }
          
        }

        if(object_literal_datatype && object_literal_language) {
          raptor_parser_warning(rdf_parser, "Typed literal used with a language - ignoring the language");
          object_literal_language=NULL;
        }
          

        break;


      case '_':
        term_types[i]= RAPTOR_NTRIPLES_TERM_TYPE_BLANK_NODE;

        /* store where _ was */
        dest=p;

        p++;
        len--;
        rdf_parser->locator.column++;
        rdf_parser->locator.byte++;

        if(!len || (len > 0 && *p != ':')) {
          raptor_parser_error(rdf_parser, "Illegal bNodeID - _ not followed by :");
          goto cleanup;
        }

        /* Found ':' - move on */

        p++;
        len--;
        rdf_parser->locator.column++;
        rdf_parser->locator.byte++;

        if(raptor_ntriples_term(rdf_parser,
                                (const unsigned char**)&p,
                                dest, &len, &term_length,
                                '\0', RAPTOR_TERM_CLASS_BNODEID, 0)) {
          rc=1;
          goto cleanup;
        }

        if(!term_length) {
          raptor_parser_error(rdf_parser, "Bad or missing bNodeID after _:");
          goto cleanup;
        } else {
          unsigned char *blank=(unsigned char*)RAPTOR_MALLOC(cstring, term_length+1);
          if(!blank) {
            raptor_parser_fatal_error(rdf_parser, "Out of memory");
            rc=1;
            goto cleanup;
          }
          strcpy((char*)blank, (const char*)dest);
          dest=raptor_parser_internal_generate_id(rdf_parser, 
                                                  RAPTOR_GENID_TYPE_BNODEID,
                                                  blank);
          terms_allocated[i]=1;
        }

        break;

      case 'x':

        raptor_parser_error(rdf_parser, "Old N-Triples XML using xml\"string\"-lang rather than \"string\"@lang^^<%s>.", raptor_xml_literal_datatype_uri_string);

        /* already know we have 'xml"' coming up */
        term_types[i]= RAPTOR_NTRIPLES_TERM_TYPE_LITERAL;
        
        /* 3=strlen("xml") */
        p+=3;
        len-=3;

        dest=p;

        p++;
        len--;
        rdf_parser->locator.column++;
        rdf_parser->locator.byte++;

        if(raptor_ntriples_term(rdf_parser,
                                (const unsigned char**)&p,
                                dest, &len, &term_length, 
                                '"', RAPTOR_TERM_CLASS_STRING, 0)) {
          rc=1;
          goto cleanup;
        }

        /* got XML literal string */
        object_literal_datatype=(unsigned char*)raptor_xml_literal_datatype_uri_string;

        if(len && (*p == '-' || *p == '@')) {
          if(*p == '-')
            raptor_parser_error(rdf_parser, "Old N-Triples language syntax using xml\"string\"-lang rather than xml\"string\"@lang.");

          object_literal_language=p;

          /* Skip - */
          p++;
          len--;
          rdf_parser->locator.column++;
          rdf_parser->locator.byte++;

          if(!len) {
            raptor_parser_error(rdf_parser, "Missing language in xml\"string\"-language after -");
            goto cleanup;
          }

          if(raptor_ntriples_term(rdf_parser,
                                  (const unsigned char**)&p,
                                  object_literal_language, &len, NULL,
                                  '"', RAPTOR_TERM_CLASS_STRING, 0)) {
            rc=1;
            goto cleanup;
          }
          
        }

        if(len >1 && *p == '^' && p[1] == '^') {

          object_literal_datatype=p;

          /* Skip ^^ */
          p+= 2;
          len-= 2;
          rdf_parser->locator.column+= 2;
          rdf_parser->locator.byte+= 2;

          if(!len || (len && *p != '<')) {
            raptor_parser_error(rdf_parser, "Missing datatype URI-ref in xml\"string\"^^<URI-ref> after ^^");
            goto cleanup;
          }

          p++;
          len--;
          rdf_parser->locator.column++;
          rdf_parser->locator.byte++;

          if(raptor_ntriples_term(rdf_parser,
                                  (const unsigned char**)&p,
                                  object_literal_datatype, &len, NULL,
                                  '>', RAPTOR_TERM_CLASS_URI, 0)) {
            rc=1;
            goto cleanup;
          }
          
        }

        if(len) {
          if(*p != ' ') {
            raptor_parser_error(rdf_parser, "Missing terminating ' '");
            return 0;
          }

          p++;
          len--;
          rdf_parser->locator.column++;
          rdf_parser->locator.byte++;
        }
        
        break;


      default:
        raptor_parser_fatal_error(rdf_parser, "Unknown term type");
        rc=1;
        goto cleanup;
    }


    /* Store term */
    terms[i]=dest; term_lengths[i]=term_length;

    /* Whitespace must separate the terms */
    if(i<2 && !isspace((int)*p)) {
      raptor_parser_error(rdf_parser, "Missing whitespace after term '%s'", terms[i]);
      rc=1;
      goto cleanup;
    }

    /* Skip whitespace after terms */
    while(len>0 && isspace((int)*p)) {
      p++;
      len--;
      rdf_parser->locator.column++;
      rdf_parser->locator.byte++;
    }

#if defined(RAPTOR_DEBUG) && RAPTOR_DEBUG > 1
    fprintf(stderr, "item %d: term '%s' len %d type %s\n",
            i, terms[i], (unsigned int)term_lengths[i],
            raptor_ntriples_term_as_string(term_types[i]));
#endif    
  }

  if(len) {
    raptor_parser_error(rdf_parser, "Junk before terminating \".\"");
    return 0;
  }
  

  if(object_literal_language) {
    unsigned char *q;
    /* Normalize language to lowercase
     * http://www.w3.org/TR/rdf-concepts/#dfn-language-identifier
     */
    for(q=object_literal_language; *q; q++) {
      if(IS_ASCII_UPPER(*q))
        *q=TO_ASCII_LOWER(*q);
    }
  }

  raptor_ntriples_generate_statement(rdf_parser, 
                                     terms[0], term_types[0],
                                     terms[1], term_types[1],
                                     terms[2], term_types[2],
                                     object_literal_language,
                                     object_literal_datatype);

  rdf_parser->locator.byte += len;

 cleanup:
  for(i=0; i<3; i++)
    if(terms_allocated[i])
      RAPTOR_FREE(cstring, terms[i]);

  return rc;
}


static int
raptor_ntriples_parse_chunk(raptor_parser* rdf_parser, 
                            const unsigned char *s, size_t len,
                            int is_end)
{
  unsigned char *buffer;
  unsigned char *ptr;
  unsigned char *start;
  raptor_ntriples_parser_context *ntriples_parser=(raptor_ntriples_parser_context*)rdf_parser->context;
  
#if defined(RAPTOR_DEBUG) && RAPTOR_DEBUG > 1
  RAPTOR_DEBUG2("adding %d bytes to buffer\n", (unsigned int)len);
#endif

  /* No data?  It's the end */
  if(!len)
    return 0;

  buffer=(unsigned char*)RAPTOR_MALLOC(cstring, ntriples_parser->line_length + len + 1);
  if(!buffer) {
    raptor_parser_fatal_error(rdf_parser, "Out of memory");
    return 1;
  }

  if(ntriples_parser->line_length) {
    strncpy((char*)buffer, (const char*)ntriples_parser->line, ntriples_parser->line_length);
    RAPTOR_FREE(cstring, ntriples_parser->line);
  }

  ntriples_parser->line=buffer;

  /* move pointer to end of cdata buffer */
  ptr=buffer+ntriples_parser->line_length;

  /* adjust stored length */
  ntriples_parser->line_length += len;

  /* now write new stuff at end of cdata buffer */
  strncpy((char*)ptr, (const char*)s, len);
  ptr += len;
  *ptr = '\0';

#if defined(RAPTOR_DEBUG) && RAPTOR_DEBUG > 1
  RAPTOR_DEBUG2("buffer now %d bytes\n", ntriples_parser->line_length);
#endif

  ptr=buffer+ntriples_parser->offset;
  while(*(start=ptr)) {
    unsigned char *line_start=ptr;
    
#if defined(RAPTOR_DEBUG) && RAPTOR_DEBUG > 1
  RAPTOR_DEBUG3("line buffer now '%s' (offset %d)\n", ptr, ptr-(buffer+ntriples_parser->offset));
#endif

    /* skip \n when just seen \r - i.e. \r\n or CR LF */
    if(ntriples_parser->last_char == '\r' && *ptr == '\n') {
#if defined(RAPTOR_DEBUG) && RAPTOR_DEBUG > 1
      RAPTOR_DEBUG1("skipping a \\n\n");
#endif
      ptr++;
      rdf_parser->locator.byte++;
      rdf_parser->locator.column=0;
      start=line_start=ptr;
    }

    while(*ptr && *ptr != '\n' && *ptr != '\r')
      ptr++;

    if(!*ptr)
      break;

#if defined(RAPTOR_DEBUG) && RAPTOR_DEBUG > 1
    RAPTOR_DEBUG3("found newline \\x%02x at offset %d\n", *ptr,
                  ptr-line_start);
#endif
    ntriples_parser->last_char=*ptr;

    len=ptr-line_start;
    rdf_parser->locator.column=0;

    *ptr='\0';
    if(raptor_ntriples_parse_line(rdf_parser,line_start,len))
      return 1;
    
    rdf_parser->locator.line++;

    /* go past newline */
    ptr++;
    rdf_parser->locator.byte++;

#if defined(RAPTOR_DEBUG) && RAPTOR_DEBUG > 1
    /* Do not peek if too far */
    if(ptr-buffer < ntriples_parser->line_length)
      RAPTOR_DEBUG2("next char is \\x%02x\n", *ptr);
    else
      RAPTOR_DEBUG1("next char unknown - end of buffer\n");
#endif
  }

  ntriples_parser->offset=start-buffer;

  len=ntriples_parser->line_length - ntriples_parser->offset;
    
  if(len) {
    /* collapse buffer */

#if defined(RAPTOR_DEBUG) && RAPTOR_DEBUG > 1
    RAPTOR_DEBUG3("collapsing buffer from %d to %d bytes\n", ntriples_parser->line_length, (unsigned int)len);
#endif
    buffer=(unsigned char*)RAPTOR_MALLOC(cstring, len + 1);
    if(!buffer) {
      raptor_parser_fatal_error(rdf_parser, "Out of memory");
      return 1;
    }

    strncpy((char*)buffer, 
            (const char*)ntriples_parser->line+ntriples_parser->line_length-len,
            len);
    buffer[len]='\0';

    RAPTOR_FREE(cstring, ntriples_parser->line);

    ntriples_parser->line=buffer;
    ntriples_parser->line_length -= ntriples_parser->offset;
    ntriples_parser->offset=0;

#if defined(RAPTOR_DEBUG) && RAPTOR_DEBUG > 1
    RAPTOR_DEBUG3("buffer now '%s' (%d bytes)\n", ntriples_parser->line, ntriples_parser->line_length);
#endif    
  }
  
  /* exit now, no more input */
  if(is_end) {
    if(ntriples_parser->offset != ntriples_parser->line_length) {
       raptor_parser_error(rdf_parser, "Junk at end of input.\"");
       return 1;
    }
    
    return 0;
  }
    
  return 0;
}


static int
raptor_ntriples_parse_start(raptor_parser* rdf_parser) 
{
  raptor_locator *locator=&rdf_parser->locator;
  raptor_ntriples_parser_context *ntriples_parser=(raptor_ntriples_parser_context*)rdf_parser->context;

  locator->line=1;
  locator->column=0;
  locator->byte=0;

  ntriples_parser->last_char='\0';

  return 0;
}


static int
raptor_ntriples_parse_recognise_syntax(raptor_parser_factory* factory, 
                                       const unsigned char *buffer, size_t len,
                                       const unsigned char *identifier, 
                                       const unsigned char *suffix, 
                                       const char *mime_type)
{
  int score= 0;
  
  if(suffix) {
    if(!strcmp((const char*)suffix, "nt"))
      score=8;
    if(!strcmp((const char*)suffix, "ttl"))
      score=3;
    if(!strcmp((const char*)suffix, "n3"))
      score=1;
  }
  
  if(mime_type) {
    if(strstr((const char*)mime_type, "ntriples"))
      score+=6;
  }
  
  return score;
}


static int
raptor_ntriples_parser_register_factory(raptor_parser_factory *factory) 
{
  int rc=0;

  factory->context_length     = sizeof(raptor_ntriples_parser_context);

  factory->need_base_uri = 0;
  
  factory->init      = raptor_ntriples_parse_init;
  factory->terminate = raptor_ntriples_parse_terminate;
  factory->start     = raptor_ntriples_parse_start;
  factory->chunk     = raptor_ntriples_parse_chunk;
  factory->recognise_syntax = raptor_ntriples_parse_recognise_syntax;

  rc=raptor_parser_factory_add_uri(factory, 
                                (const unsigned char*)"http://www.w3.org/TR/rdf-testcases/#ntriples");

  return rc;
}


int
raptor_init_parser_ntriples(void)
{
  return !raptor_parser_register_factory("ntriples",  "N-Triples",
                                         &raptor_ntriples_parser_register_factory);
}
