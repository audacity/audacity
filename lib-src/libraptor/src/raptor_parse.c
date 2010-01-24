/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_parse.c - Raptor Parser API
 *
 * Copyright (C) 2000-2008, David Beckett http://purl.org/net/dajobe/
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
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

/* Raptor includes */
#include "raptor.h"
#include "raptor_internal.h"


#ifndef STANDALONE

/* prototypes for helper functions */


/* statics */

/* list of parser factories */
static raptor_sequence* parsers=NULL;


/* helper methods */

static void
raptor_free_parser_factory(raptor_parser_factory* factory)
{
  if(factory->finish_factory)
    factory->finish_factory(factory);
  
  if(factory->name)
    RAPTOR_FREE(raptor_parser_factory, (void*)factory->name);
  if(factory->label)
    RAPTOR_FREE(raptor_parser_factory, (void*)factory->label);
  if(factory->alias)
    RAPTOR_FREE(raptor_parser_factory, (void*)factory->alias);
  if(factory->mime_types)
    raptor_free_sequence(factory->mime_types);
  if(factory->uri_string)
    RAPTOR_FREE(raptor_parser_factory, (void*)factory->uri_string);
  
  RAPTOR_FREE(raptor_parser_factory, factory);
}


/* class methods */

int
raptor_parsers_init(void)
{
  int rc=0;

  parsers=raptor_new_sequence((raptor_sequence_free_handler *)raptor_free_parser_factory, NULL);
  if(!parsers)
    return 1;
  
#ifdef RAPTOR_PARSER_RDFXML
  rc+= raptor_init_parser_rdfxml() != 0;
#endif

#ifdef RAPTOR_PARSER_NTRIPLES
  rc+= raptor_init_parser_ntriples() != 0;
#endif

#ifdef RAPTOR_PARSER_N3
  rc+= raptor_init_parser_n3() != 0;
#endif

#ifdef RAPTOR_PARSER_TURTLE
  rc+= raptor_init_parser_turtle() != 0;
#endif

#ifdef RAPTOR_PARSER_TRIG
  rc+= raptor_init_parser_trig() != 0;
#endif

#ifdef RAPTOR_PARSER_RSS
  rc+= raptor_init_parser_rss() != 0;
#endif

#if defined(RAPTOR_PARSER_GRDDL)
  rc+= raptor_init_parser_grddl_common() != 0;

#ifdef RAPTOR_PARSER_GRDDL
  rc+= raptor_init_parser_grddl() != 0;
#endif

#endif

#ifdef RAPTOR_PARSER_GUESS
  rc+= raptor_init_parser_guess() != 0;
#endif

  return rc;
}


/*
 * raptor_finish_parsers - delete all the registered parsers
 */
void
raptor_parsers_finish(void)
{
  if(parsers) {
    raptor_free_sequence(parsers);
    parsers=NULL;
  }
#if defined(RAPTOR_PARSER_GRDDL)
  raptor_terminate_parser_grddl_common();
#endif
}


/*
 * raptor_parser_register_factory:
 * @name: the short syntax name
 * @label: readable label for syntax
 * @mime_type: MIME type of the syntax handled by the parser (or NULL)
 * @uri_string: URI string of the syntax (or NULL)
 * @factory: pointer to function to call to register the factory
 * 
 * Register a syntax handled by a parser factory.
 *
 * INTERNAL
 *
 **/
raptor_parser_factory*
raptor_parser_register_factory(const char *name, const char *label,
                               int (*factory) (raptor_parser_factory*)) 
{
  raptor_parser_factory *parser=NULL;
  raptor_parser_factory *h;
  char *name_copy, *label_copy;
  int i;
  
#if defined(RAPTOR_DEBUG) && RAPTOR_DEBUG > 1
  RAPTOR_DEBUG3("Received registration for syntax %s '%s'\n", name, label);
#endif
  
  for(i=0;
      (h=(raptor_parser_factory*)raptor_sequence_get_at(parsers, i));
      i++) {
    if(!strcmp(h->name, name)) {
      raptor_finish();
      RAPTOR_FATAL2("parser %s already registered\n", h->name);
      return NULL;
    }
  }
  
  parser=(raptor_parser_factory*)RAPTOR_CALLOC(raptor_parser_factory, 1,
                                               sizeof(raptor_parser_factory));
  if(!parser)
    return NULL;

  name_copy=(char*)RAPTOR_CALLOC(cstring, strlen(name)+1, 1);
  if(!name_copy)
    goto tidy;
  strcpy(name_copy, name);
  parser->name=name_copy;
        
  label_copy=(char*)RAPTOR_CALLOC(cstring, strlen(label)+1, 1);
  if(!label_copy)
    goto tidy;
  strcpy(label_copy, label);
  parser->label=label_copy;

  parser->mime_types=raptor_new_sequence((raptor_sequence_free_handler*)raptor_free_type_q, NULL);
  if(!parser->mime_types)
    goto tidy;

  if(raptor_sequence_push(parsers, parser))
    return NULL; /* on error, parser is already freed by the sequence */
  
  /* Call the parser registration function on the new object */
  if (factory(parser))
    return NULL; /* parser is owned and freed by the parsers sequence */
  
#if defined(RAPTOR_DEBUG) && RAPTOR_DEBUG > 1
  RAPTOR_DEBUG3("%s has context size %d\n", name, parser->context_length);
#endif

  return parser;

  /* Clean up on failure */
  tidy:
  raptor_free_parser_factory(parser);
  return NULL;
}


int
raptor_parser_factory_add_alias(raptor_parser_factory* factory,
                                const char *alias)
{
  raptor_parser_factory *p;
  char *alias_copy;
  int i;
  
  for(i=0;
      (p=(raptor_parser_factory*)raptor_sequence_get_at(parsers, i));
      i++) {
    if(!strcmp(p->name, alias)) {
      raptor_finish();
      RAPTOR_FATAL2("parser %s already registered\n", p->name);
      return 1;
    }
  }
  
  alias_copy=(char*)RAPTOR_CALLOC(cstring, strlen(alias)+1, 1);
  if(!alias_copy)
    return 1;
  strcpy(alias_copy, alias);
  factory->alias=alias_copy;

  return 0;
}


void
raptor_free_type_q(raptor_type_q* type_q)
{
  RAPTOR_FREE(cstring, (void*)type_q->mime_type);
  RAPTOR_FREE(raptor_type_q, (void*)type_q);
}


/**
 * raptor_parser_factory_add_mime_type:
 * @factory: Raptor parser factory
 * @mime_type: MIME Type string
 * @q: Accept 'Q' value 0 to 10 inclusive representing 0.0 to 1.0
 * 
 * Register a MIME type as handled by a factory.
 *
 * The FIRST added MIME type is the default or main one reported.
 *
 * Return value: non-0 on failure
 *
 **/
int
raptor_parser_factory_add_mime_type(raptor_parser_factory* factory,
                                    const char* mime_type, int q)
{
  raptor_type_q* type_q;
  char* mime_type_copy;
  size_t len;
  
  type_q=(raptor_type_q*)RAPTOR_CALLOC(raptor_type_q, sizeof(raptor_type_q), 1);
  if(!type_q)
    return 1;
  len=strlen(mime_type);
  mime_type_copy=(char*)RAPTOR_CALLOC(cstring, len+1, 1);
  if(!mime_type_copy) {
    raptor_free_type_q(type_q);
    return 1;
  }
  strcpy(mime_type_copy, mime_type);

  type_q->mime_type=mime_type_copy;
  type_q->mime_type_len=len;

  if(q<0)
    q=0;
  if(q>10)
    q=10;
  type_q->q=q;

  return raptor_sequence_push(factory->mime_types, type_q);
}


/**
 * raptor_parser_factory_add_uri:
 * @factory: Raptor parser factory
 * @uri_string: URI string
 * 
 * Register an identifying URI as handled by a factory.
 *
 * Return value: non-0 on failure
 **/
int
raptor_parser_factory_add_uri(raptor_parser_factory* factory,
                              const unsigned char *uri_string)
{
  unsigned char *uri_string_copy;

  if(!uri_string)
    return 1;
  
  uri_string_copy=(unsigned char*)RAPTOR_CALLOC(cstring, strlen((const char*)uri_string)+1, 1);
  if(!uri_string_copy)
    return 1;

  strcpy((char*)uri_string_copy, (const char*)uri_string);
  factory->uri_string=uri_string_copy;

  return 0;
}


/**
 * raptor_get_parser_factory:
 * @name: the factory name or NULL for the default factory
 *
 * Get a parser factory by name.
 * 
 * Return value: the factory object or NULL if there is no such factory
 **/
raptor_parser_factory*
raptor_get_parser_factory(const char *name) 
{
  raptor_parser_factory *factory;

  /* return 1st parser if no particular one wanted - why? */
  if(!name) {
    factory=(raptor_parser_factory *)raptor_sequence_get_at(parsers, 0);
    if(!factory) {
      RAPTOR_DEBUG1("No (default) parsers registered\n");
      return NULL;
    }
  } else {
    int i;
    
    for(i=0;
        (factory=(raptor_parser_factory*)raptor_sequence_get_at(parsers, i));
        i++) {
      if(!strcmp(factory->name, name) ||
         (factory->alias && !strcmp(factory->alias, name)))
        break;
    }
    /* else FACTORY name not found */
    if(!factory) {
      RAPTOR_DEBUG2("No parser with name %s found\n", name);
      return NULL;
    }
  }
        
  return factory;
}


/**
 * raptor_syntaxes_enumerate:
 * @counter: index into the list of syntaxes
 * @name: pointer to store the name of the syntax (or NULL)
 * @label: pointer to store syntax readable label (or NULL)
 * @mime_type: pointer to store syntax MIME Type (or NULL)
 * @uri_string: pointer to store syntax URI string (or NULL)
 *
 * Get information on syntaxes.
 * 
 * Return value: non 0 on failure of if counter is out of range
 **/
int
raptor_syntaxes_enumerate(const unsigned int counter,
                          const char **name, const char **label,
                          const char **mime_type,
                          const unsigned char **uri_string)
{
  raptor_parser_factory *factory;

  factory=(raptor_parser_factory*)raptor_sequence_get_at(parsers,
                                                         counter);

  if(!factory)
    return 1;

  if(name)
    *name=factory->name;
  if(label)
    *label=factory->label;
  if(mime_type) {
    const char *mime_type_t=NULL;
    if(factory->mime_types) {
      raptor_type_q* tq;
      tq=(raptor_type_q*)raptor_sequence_get_at(factory->mime_types, 0);
      if(tq)
        mime_type_t=tq->mime_type;
    }
    *mime_type=mime_type_t;
  }
  if(uri_string)
    *uri_string=factory->uri_string;
  return 0;
}


/**
 * raptor_parsers_enumerate:
 * @counter: index to list of parsers
 * @name: pointer to store syntax name (or NULL)
 * @label: pointer to store syntax label (or NULL)
 *
 * Get list of syntax parsers.
 * 
 * Return value: non 0 on failure of if counter is out of range
 **/
int
raptor_parsers_enumerate(const unsigned int counter,
                         const char **name, const char **label)
{
  return raptor_syntaxes_enumerate(counter, name, label, NULL, NULL);
}


/**
 * raptor_syntax_name_check:
 * @name: the syntax name
 *
 * Check name of a parser.
 *
 * Return value: non 0 if name is a known syntax name
 */
int
raptor_syntax_name_check(const char *name) {
  return (raptor_get_parser_factory(name) != NULL);
}


/**
 * raptor_new_parser:
 * @name: the parser name
 *
 * Constructor - create a new raptor_parser object.
 *
 * Return value: a new #raptor_parser object or NULL on failure
 */
raptor_parser*
raptor_new_parser(const char *name) {
  raptor_parser_factory* factory;
  raptor_parser* rdf_parser;

  factory=raptor_get_parser_factory(name);
  if(!factory)
    return NULL;

  rdf_parser=(raptor_parser*)RAPTOR_CALLOC(raptor_parser, 1,
                                           sizeof(raptor_parser));
  if(!rdf_parser)
    return NULL;
  
  rdf_parser->context=(char*)RAPTOR_CALLOC(raptor_parser_context, 1,
                                           factory->context_length);
  if(!rdf_parser->context) {
    raptor_free_parser(rdf_parser);
    return NULL;
  }
  
#ifdef RAPTOR_XML_LIBXML
  rdf_parser->magic=RAPTOR_LIBXML_MAGIC;
#endif  
  rdf_parser->factory=factory;

  rdf_parser->failed=0;

  rdf_parser->error_handlers.locator=&rdf_parser->locator;
  rdf_parser->error_handlers.last_log_level=RAPTOR_LOG_LEVEL_LAST;
  raptor_error_handlers_init(&rdf_parser->error_handlers);
  
  /* Initialise default (lax) feature values */
  raptor_set_parser_strict(rdf_parser, 0);

  if(factory->init(rdf_parser, name)) {
    raptor_free_parser(rdf_parser);
    return NULL;
  }
  
  return rdf_parser;
}


/**
 * raptor_new_parser_for_content:
 * @uri: URI identifying the syntax (or NULL)
 * @mime_type: mime type identifying the content (or NULL)
 * @buffer: buffer of content to guess (or NULL)
 * @len: length of buffer
 * @identifier: identifier of content (or NULL)
 * 
 * Constructor - create a new raptor_parser.
 *
 * Uses raptor_guess_parser_name() to find a parser by scoring
 * recognition of the syntax by a block of characters, the content
 * identifier or a mime type.  The content identifier is typically a
 * filename or URI or some other identifier.
 * 
 * Return value: a new #raptor_parser object or NULL on failure
 **/
raptor_parser*
raptor_new_parser_for_content(raptor_uri *uri, const char *mime_type,
                              const unsigned char *buffer, size_t len,
                              const unsigned char *identifier)
{
  return raptor_new_parser(raptor_guess_parser_name(uri, mime_type, buffer, len, identifier));
}



/**
 * raptor_start_parse:
 * @rdf_parser: RDF parser
 * @uri: base URI or may be NULL if no base URI is required
 *
 * Start a parse of content with base URI.
 * 
 * Parsers that need a base URI can be tested with raptor_get_need_base_uri().
 * 
 * Return value: non-0 on failure, <0 if a required base URI was missing
 **/
int
raptor_start_parse(raptor_parser *rdf_parser, raptor_uri *uri) 
{
  if(rdf_parser->factory->need_base_uri && !uri) {
    raptor_parser_error(rdf_parser, "Missing base URI for %s parser.",
                        rdf_parser->factory->name);
    return -1;
  }

  if(uri)
    uri=raptor_uri_copy(uri);
  
  if(rdf_parser->base_uri)
    raptor_free_uri(rdf_parser->base_uri);
  rdf_parser->base_uri=uri;

  rdf_parser->locator.uri    = uri;
  rdf_parser->locator.line   = -1;
  rdf_parser->locator.column = -1;
  rdf_parser->locator.byte   = -1;

  if(rdf_parser->factory->start)
    return rdf_parser->factory->start(rdf_parser);
  else
    return 0;
}




/**
 * raptor_parse_chunk:
 * @rdf_parser: RDF parser
 * @buffer: content to parse
 * @len: length of buffer
 * @is_end: non-0 if this is the end of the content (such as EOF)
 *
 * Parse a block of content into triples.
 * 
 * This method can only be called after raptor_start_parse has
 * initialised the parser.
 * 
 * Return value: non-0 on failure.
 **/
int
raptor_parse_chunk(raptor_parser* rdf_parser,
                   const unsigned char *buffer, size_t len, int is_end) 
{
  if(rdf_parser->sb)
    raptor_stringbuffer_append_counted_string(rdf_parser->sb, buffer, len, 1);
    
  return rdf_parser->factory->chunk(rdf_parser, buffer, len, is_end);
}


/**
 * raptor_free_parser:
 * @parser: #raptor_parser object
 *
 * Destructor - destroy a raptor_parser object.
 * 
 **/
void
raptor_free_parser(raptor_parser* rdf_parser) 
{
  if(rdf_parser->factory)
    rdf_parser->factory->terminate(rdf_parser);

  if(rdf_parser->www)
    raptor_www_free(rdf_parser->www);

  if(rdf_parser->context)
    RAPTOR_FREE(raptor_parser_context, rdf_parser->context);

  if(rdf_parser->base_uri)
    raptor_free_uri(rdf_parser->base_uri);

  if(rdf_parser->default_generate_id_handler_prefix)
    RAPTOR_FREE(cstring, rdf_parser->default_generate_id_handler_prefix);

  if(rdf_parser->sb)
    raptor_free_stringbuffer(rdf_parser->sb);

  if(rdf_parser->cache_control)
    RAPTOR_FREE(cstring, rdf_parser->cache_control);

  if(rdf_parser->user_agent)
    RAPTOR_FREE(cstring, rdf_parser->user_agent);

  RAPTOR_FREE(raptor_parser, rdf_parser);
}


/* Size of XML buffer to use when reading from a file */
#define RAPTOR_READ_BUFFER_SIZE 4096


/**
 * raptor_parse_file_stream:
 * @rdf_parser: parser
 * @stream: FILE* of RDF content
 * @filename: filename of content or NULL if it has no name
 * @base_uri: the base URI to use
 *
 * Parse RDF content from a FILE*.
 *
 * After draining the stream, fclose is not called on it internally.
 *
 * Return value: non 0 on failure
 **/
int
raptor_parse_file_stream(raptor_parser* rdf_parser,
                         FILE *stream, const char* filename,
                         raptor_uri *base_uri)
{
  /* Read buffer */
  unsigned char buffer[RAPTOR_READ_BUFFER_SIZE+1];
  int rc=0;
  raptor_locator *locator=&rdf_parser->locator;

  if(!stream || !base_uri)
    return 1;

  locator->line= locator->column = -1;
  locator->file= filename;

  if(raptor_start_parse(rdf_parser, base_uri))
    return 1;
  
  while(!feof(stream)) {
    int len=fread(buffer, 1, RAPTOR_READ_BUFFER_SIZE, stream);
    int is_end=(len < RAPTOR_READ_BUFFER_SIZE);
    buffer[len] = '\0';
    rc=raptor_parse_chunk(rdf_parser, buffer, len, is_end);
    if(rc || is_end)
      break;
  }

  return (rc != 0);
}


/**
 * raptor_parse_file:
 * @rdf_parser: parser
 * @uri: URI of RDF content or NULL to read from standard input
 * @base_uri: the base URI to use (or NULL if the same)
 *
 * Parse RDF content at a file URI.
 *
 * If uri is NULL (source is stdin), then the base_uri is required.
 * 
 * Return value: non 0 on failure
 **/
int
raptor_parse_file(raptor_parser* rdf_parser, raptor_uri *uri,
                  raptor_uri *base_uri) 
{
  int rc=0;
  int free_base_uri=0;
  const char *filename=NULL;
  FILE *fh=NULL;
#if defined(HAVE_UNISTD_H) && defined(HAVE_SYS_STAT_H)
  struct stat buf;
#endif

  if(uri) {
    filename=raptor_uri_uri_string_to_filename(raptor_uri_as_string(uri));
    if(!filename)
      return 1;

#if defined(HAVE_UNISTD_H) && defined(HAVE_SYS_STAT_H)
    if(!stat(filename, &buf) && S_ISDIR(buf.st_mode)) {
      raptor_parser_error(rdf_parser, "Cannot read from a directory '%s'",
                          filename);
      goto cleanup;
    }
#endif

    fh=fopen(filename, "r");
    if(!fh) {
      raptor_parser_error(rdf_parser, "file '%s' open failed - %s",
                          filename, strerror(errno));
      goto cleanup;
    }
    if(!base_uri) {
      base_uri=raptor_uri_copy(uri);
      free_base_uri=1;
    }
  } else {
    if(!base_uri)
      return 1;
    fh=stdin;
  }

  rc=raptor_parse_file_stream(rdf_parser, fh, filename, base_uri);

  cleanup:
  if(uri) {
    if(fh)
      fclose(fh);
    RAPTOR_FREE(cstring, (void*)filename);
  }
  if(free_base_uri)
    raptor_free_uri(base_uri);

  return rc;
}


void
raptor_parse_uri_write_bytes(raptor_www* www,
                             void *userdata, const void *ptr, 
                             size_t size, size_t nmemb)
{
  raptor_parse_bytes_context* rpbc=(raptor_parse_bytes_context*)userdata;
  int len=size*nmemb;

  if(!rpbc->started) {
    raptor_uri* base_uri=rpbc->base_uri;
    
    if(!base_uri) {
      rpbc->final_uri=raptor_www_get_final_uri(www);
      /* base URI after URI resolution is finally chosen */
      base_uri = rpbc->final_uri ? rpbc->final_uri : www->uri;
    }

    if(raptor_start_parse(rpbc->rdf_parser, base_uri))
      raptor_www_abort(www, "Parsing failed");
    rpbc->started=1;
  }

  if(raptor_parse_chunk(rpbc->rdf_parser, (unsigned char*)ptr, len, 0))
    raptor_www_abort(www, "Parsing failed");
}


static void
raptor_parse_uri_content_type_handler(raptor_www* www, void* userdata, 
                                      const char* content_type)
{
  raptor_parser* rdf_parser=(raptor_parser*)userdata;
  if(rdf_parser->factory->content_type_handler)
    rdf_parser->factory->content_type_handler(rdf_parser, content_type);
}


int
raptor_parse_uri_no_net_filter(void *user_data, raptor_uri* uri)
{
  unsigned char* uri_string=raptor_uri_as_string(uri);
  
  if(raptor_uri_uri_string_is_file_uri(uri_string))
    return 0;

  raptor_parser_error((raptor_parser*)user_data, 
                      "Network fetch of URI '%s' denied", uri_string);
  return 1;
}


/**
 * raptor_parse_uri:
 * @rdf_parser: parser
 * @uri: URI of RDF content
 * @base_uri: the base URI to use (or NULL if the same)
 *
 * Parse the RDF content at URI.
 * 
 * Sends an HTTP Accept: header whent the URI is of the HTTP protocol,
 * see raptor_parse_uri_with_connection() for details including
 * how the @base_uri is used.
 *
 * Return value: non 0 on failure
 **/
int
raptor_parse_uri(raptor_parser* rdf_parser, raptor_uri *uri,
                 raptor_uri *base_uri)
{
  return raptor_parse_uri_with_connection(rdf_parser, uri, base_uri, NULL);
}


/**
 * raptor_parse_uri_with_connection:
 * @rdf_parser: parser
 * @uri: URI of RDF content
 * @base_uri: the base URI to use (or NULL if the same)
 * @connection: connection object pointer or NULL to create a new one
 *
 * Parse RDF content at URI using existing WWW connection.
 * 
 * If @base_uri is not given and during resolution of the URI, a
 * protocol redirection occurs, the final resolved URI will be
 * used as the base URI.  If redirection does not occur, the
 * base URI will be @uri.
 *
 * If @base_uri is given, it overrides the process above.
 *
 * When @connection is NULL and a MIME Type exists for the parser
 * type - such as returned by raptor_get_mime_type(parser) - this
 * type is sent in an HTTP Accept: header in the form
 * Accept: MIME-TYPE along with a wildcard of 0.1 quality, so MIME-TYPE is
 * prefered rather than the sole answer.  The latter part may not be
 * necessary but should ensure an HTTP 200 response.
 *
 * Return value: non 0 on failure
 **/
int
raptor_parse_uri_with_connection(raptor_parser* rdf_parser, raptor_uri *uri,
                                 raptor_uri *base_uri, void *connection)
{
  int ret=0;
  raptor_parse_bytes_context rpbc;
  
  if(connection) {
    if(rdf_parser->www)
      raptor_www_free(rdf_parser->www);
    rdf_parser->www=raptor_www_new_with_connection(connection);
    if(!rdf_parser->www)
      return 1;
  } else {
    const char *accept_h;
    
    if(rdf_parser->www)
      raptor_www_free(rdf_parser->www);
    rdf_parser->www=raptor_www_new();
    if(!rdf_parser->www)
      return 1;

    accept_h=raptor_parser_get_accept_header(rdf_parser);
    if(accept_h) {
      raptor_www_set_http_accept(rdf_parser->www, accept_h);
      RAPTOR_FREE(cstring, accept_h);
    }
  }

  rpbc.rdf_parser=rdf_parser;
  rpbc.base_uri=base_uri;
  rpbc.final_uri=NULL;
  rpbc.started=0;
  
  if(rdf_parser->uri_filter)
    raptor_www_set_uri_filter(rdf_parser->www, rdf_parser->uri_filter,
                              rdf_parser->uri_filter_user_data);
  else if(rdf_parser->features[RAPTOR_FEATURE_NO_NET])
    raptor_www_set_uri_filter(rdf_parser->www, raptor_parse_uri_no_net_filter, rdf_parser);
  
  raptor_www_set_error_handler(rdf_parser->www,
                               rdf_parser->error_handlers.handlers[RAPTOR_LOG_LEVEL_ERROR].handler,
                               rdf_parser->error_handlers.handlers[RAPTOR_LOG_LEVEL_ERROR].user_data);
  raptor_www_set_write_bytes_handler(rdf_parser->www, raptor_parse_uri_write_bytes, 
                                     &rpbc);

  raptor_www_set_content_type_handler(rdf_parser->www,
                                      raptor_parse_uri_content_type_handler,
                                      rdf_parser);

  raptor_www_set_http_cache_control(rdf_parser->www, rdf_parser->cache_control);

  if(rdf_parser->user_agent)
    raptor_www_set_user_agent(rdf_parser->www, rdf_parser->user_agent);
  
  ret=raptor_www_fetch(rdf_parser->www, uri);
  
  if(!rpbc.started && !ret)
    ret=raptor_start_parse(rdf_parser, base_uri);

  if(rpbc.final_uri)
    raptor_free_uri(rpbc.final_uri);

  if(ret) {
    raptor_www_free(rdf_parser->www);
    rdf_parser->www=NULL;
    return 1;
  }

  if(raptor_parse_chunk(rdf_parser, NULL, 0, 1))
    rdf_parser->failed=1;

  raptor_www_free(rdf_parser->www);
  rdf_parser->www=NULL;

  return rdf_parser->failed;
}


/*
 * raptor_parser_fatal_error - Fatal Error from a parser - Internal
 */
void
raptor_parser_fatal_error(raptor_parser* parser, const char *message, ...)
{
  va_list arguments;

  va_start(arguments, message);
  if(parser)
    raptor_log_error_varargs(RAPTOR_LOG_LEVEL_FATAL,
                             parser->error_handlers.handlers[RAPTOR_LOG_LEVEL_FATAL].handler, 
                             parser->error_handlers.handlers[RAPTOR_LOG_LEVEL_FATAL].user_data, 
                             &parser->locator,
                             message, arguments); 
  else
    raptor_log_error_varargs(RAPTOR_LOG_LEVEL_FATAL, NULL, NULL, NULL, 
                             message, arguments); 
  va_end(arguments);
}


/*
 * raptor_parser_fatal_error_message_handler - Error from a parser - Internal
 */
void
raptor_parser_fatal_error_message_handler(void *user_data,
                                          raptor_locator* locator,
                                          const char *message)
{
  raptor_parser* parser=(raptor_parser*)user_data;

  parser->failed=1;

  if(parser)
    raptor_log_error_to_handlers(&parser->error_handlers,
                                 RAPTOR_LOG_LEVEL_FATAL, &parser->locator,
                                 message); 
  else
    raptor_log_error(RAPTOR_LOG_LEVEL_FATAL, NULL, NULL, NULL, message); 
}


/*
 * raptor_parser_error - Error from a parser - Internal
 */
void
raptor_parser_error(raptor_parser* parser, const char *message, ...)
{
  va_list arguments;

  va_start(arguments, message);

  raptor_parser_error_varargs(parser, message, arguments);
  
  va_end(arguments);
}


/*
 * raptor_parser_simple_error - Error from a parser - Internal
 *
 * Matches the raptor_simple_message_handler API but same as
 * raptor_parser_error 
 */
void
raptor_parser_simple_error(void* user_data, const char *message, ...)
{
  raptor_parser* parser=(raptor_parser*)user_data;
  va_list arguments;

  va_start(arguments, message);

  if(parser)
    raptor_log_error_varargs(RAPTOR_LOG_LEVEL_ERROR,
                             parser->error_handlers.handlers[RAPTOR_LOG_LEVEL_ERROR].handler,
                             parser->error_handlers.handlers[RAPTOR_LOG_LEVEL_ERROR].user_data,
                             &parser->locator,
                             message, arguments);
  else
    raptor_log_error_varargs(RAPTOR_LOG_LEVEL_ERROR,
                             NULL, NULL, NULL,
                             message, arguments);
  
  va_end(arguments);
}


/**
 * raptor_parser_error_varargs:
 * @parser: parser
 * @message: error format message
 * @arguments: varargs for message
 *
 * Error from a parser - Internal.
 */  
void  
raptor_parser_error_varargs(raptor_parser* parser, const char *message,  
                            va_list arguments)
{
  if(parser)
    raptor_log_error_varargs(RAPTOR_LOG_LEVEL_ERROR,
                             parser->error_handlers.handlers[RAPTOR_LOG_LEVEL_ERROR].handler,
                             parser->error_handlers.handlers[RAPTOR_LOG_LEVEL_ERROR].user_data,
                             &parser->locator,
                             message, arguments);
  else
    raptor_log_error_varargs(RAPTOR_LOG_LEVEL_ERROR,
                             NULL, NULL, NULL,
                             message, arguments);
}


/*
 * raptor_parser_error_message_handler - Error from a parser - Internal
 */
void
raptor_parser_error_message_handler(void *user_data,
                                    raptor_locator* locator,
                                    const char *message)
{
  raptor_parser* parser=(raptor_parser*)user_data;

  if(parser)
    raptor_log_error_to_handlers(&parser->error_handlers,
                                 RAPTOR_LOG_LEVEL_ERROR,
                                 &parser->locator, message);
  else
    raptor_log_error(RAPTOR_LOG_LEVEL_ERROR, NULL, NULL, NULL, message);
}


/*
 * raptor_parser_warning - Warning from a parser - Internal
 */
void
raptor_parser_warning(raptor_parser* parser, const char *message, ...)
{
  va_list arguments;

  va_start(arguments, message);

  if(parser)
    raptor_log_error_varargs(RAPTOR_LOG_LEVEL_WARNING,
                             parser->error_handlers.handlers[RAPTOR_LOG_LEVEL_WARNING].handler,
                             parser->error_handlers.handlers[RAPTOR_LOG_LEVEL_WARNING].user_data,
                             &parser->locator,
                             message, arguments);
  else
    raptor_log_error_varargs(RAPTOR_LOG_LEVEL_WARNING,
                             NULL, NULL, NULL,
                             message, arguments);
  
  va_end(arguments);
}


/*
 * raptor_parser_warning_message_handler - Warning from a parser - Internal
 */
void
raptor_parser_warning_message_handler(void *user_data,
                                      raptor_locator* locator,
                                      const char *message) 
{
  raptor_parser* parser=(raptor_parser*)user_data;

  if(parser)
    raptor_log_error_to_handlers(&parser->error_handlers,
                                 RAPTOR_LOG_LEVEL_WARNING,
                                 &parser->locator, message);
  else
    raptor_log_error(RAPTOR_LOG_LEVEL_WARNING, NULL, NULL, NULL, message);
}



/* PUBLIC FUNCTIONS */

/**
 * raptor_set_fatal_error_handler:
 * @parser: the parser
 * @user_data: user data to pass to function
 * @handler: pointer to the function
 *
 * Set the parser error handling function.
 * 
 * The function will receive callbacks when the parser fails.
 * 
 **/
void
raptor_set_fatal_error_handler(raptor_parser* parser, void *user_data,
                               raptor_message_handler handler)
{
  parser->error_handlers.handlers[RAPTOR_LOG_LEVEL_FATAL].user_data=user_data;
  parser->error_handlers.handlers[RAPTOR_LOG_LEVEL_FATAL].handler=handler;
}


/**
 * raptor_set_error_handler:
 * @parser: the parser
 * @user_data: user data to pass to function
 * @handler: pointer to the function
 *
 * Set the parser error handling function.
 * 
 * The function will receive callbacks when the parser fails.
 * 
 **/
void
raptor_set_error_handler(raptor_parser* parser, void *user_data,
                         raptor_message_handler handler)
{
  parser->error_handlers.handlers[RAPTOR_LOG_LEVEL_ERROR].user_data=user_data;
  parser->error_handlers.handlers[RAPTOR_LOG_LEVEL_ERROR].handler=handler;
}


/**
 * raptor_set_warning_handler:
 * @parser: the parser
 * @user_data: user data to pass to function
 * @handler: pointer to the function
 *
 * Set the parser warning handling function.
 * 
 * The function will receive callbacks when the parser gives a warning.
 * 
 **/
void
raptor_set_warning_handler(raptor_parser* parser, void *user_data,
                           raptor_message_handler handler)
{
  parser->error_handlers.handlers[RAPTOR_LOG_LEVEL_WARNING].user_data=user_data;
  parser->error_handlers.handlers[RAPTOR_LOG_LEVEL_WARNING].handler=handler;
}


/**
 * raptor_set_statement_handler:
 * @parser: #raptor_parser parser object
 * @user_data: user data pointer for callback
 * @handler: new statement callback function
 *
 * Set the statement handler function for the parser.
 * 
 **/
void
raptor_set_statement_handler(raptor_parser* parser,
                             void *user_data,
                             raptor_statement_handler handler)
{
  parser->user_data=user_data;
  parser->statement_handler=handler;
}


/**
 * raptor_set_graph_handler:
 * @parser: #raptor_parser parser object
 * @user_data: user data pointer for callback
 * @handler: new graph callback function
 *
 * Set the graph handler function for the parser.
 * 
 **/
void
raptor_set_graph_handler(raptor_parser* parser,
			 void *user_data,
			 raptor_graph_handler handler)
{
  parser->user_data=user_data;
  parser->graph_handler=handler;
}


/**
 * raptor_set_generate_id_handler:
 * @parser: #raptor_parser parser object
 * @user_data: user data pointer for callback
 * @handler: generate ID callback function
 *
 * Set the generate ID handler function for the parser.
 *
 * Sets the function to generate IDs for the parser.  The handler is
 * called with the @user_data parameter and an ID type of either
 * RAPTOR_GENID_TYPE_BNODEID or RAPTOR_GENID_TYPE_BAGID (latter is deprecated).
 *
 * The final argument of the callback method is user_bnodeid, the value of
 * the rdf:nodeID attribute that the user provided if any (or NULL).
 * It can either be returned directly as the generated value when present or
 * modified.  The passed in value must be free()d if it is not used.
 *
 * If handler is NULL, the default method is used
 * 
 **/
void
raptor_set_generate_id_handler(raptor_parser* parser,
                               void *user_data,
                               raptor_generate_id_handler handler)
{
  parser->generate_id_handler_user_data=user_data;
  parser->generate_id_handler=handler;
}


/**
 * raptor_set_namespace_handler:
 * @parser: #raptor_parser parser object
 * @user_data: user data pointer for callback
 * @handler: new namespace callback function
 *
 * Set the namespace handler function for the parser.
 *
 * When a prefix/namespace is seen in a parser, call the given
 * @handler with the prefix string and the #raptor_uri namespace URI.
 * Either can be NULL for the default prefix or default namespace.
 *
 * The handler function does not deal with duplicates so any
 * namespace may be declared multiple times.
 * 
 **/
void
raptor_set_namespace_handler(raptor_parser* parser,
                             void *user_data,
                             raptor_namespace_handler handler)
{
  parser->namespace_handler=handler;
  parser->namespace_handler_user_data=user_data;
}


/**
 * raptor_parser_set_uri_filter:
 * @parser: parser object
 * @filter: URI filter function
 * @user_data: User data to pass to filter function
 * 
 * Set URI filter function for WWW retrieval.
 **/
void
raptor_parser_set_uri_filter(raptor_parser* parser, 
                             raptor_uri_filter_func filter,
                             void *user_data)
{
  parser->uri_filter=filter;
  parser->uri_filter_user_data=user_data;
}


/**
 * raptor_features_enumerate:
 * @feature: feature enumeration (0+)
 * @name: pointer to store feature short name (or NULL)
 * @uri: pointer to store feature URI (or NULL)
 * @label: pointer to feature label (or NULL)
 *
 * Get list of syntax features.
 * 
 * If uri is not NULL, a pointer toa new raptor_uri is returned
 * that must be freed by the caller with raptor_free_uri().
 *
 * Return value: 0 on success, <0 on failure, >0 if feature is unknown
 **/
int
raptor_features_enumerate(const raptor_feature feature,
                          const char **name, 
                          raptor_uri **uri, const char **label)
{
  return raptor_features_enumerate_common(feature, name, uri, label, 1);
}


/**
 * raptor_set_feature:
 * @parser: #raptor_parser parser object
 * @feature: feature to set from enumerated #raptor_feature values
 * @value: integer feature value (0 or larger)
 *
 * Set various parser features.
 * 
 * The allowed features are available via raptor_features_enumerate().
 *
 * Return value: non 0 on failure or if the feature is unknown
 **/
int
raptor_set_feature(raptor_parser *parser, raptor_feature feature, int value)
{
  if(value < 0)
    return -1;
  
  switch(feature) {
    case RAPTOR_FEATURE_SCANNING:
    case RAPTOR_FEATURE_ALLOW_NON_NS_ATTRIBUTES:
    case RAPTOR_FEATURE_ALLOW_OTHER_PARSETYPES:
    case RAPTOR_FEATURE_ALLOW_BAGID:
    case RAPTOR_FEATURE_ALLOW_RDF_TYPE_RDF_LIST:
    case RAPTOR_FEATURE_NORMALIZE_LANGUAGE:
    case RAPTOR_FEATURE_NON_NFC_FATAL:
    case RAPTOR_FEATURE_WARN_OTHER_PARSETYPES:
    case RAPTOR_FEATURE_CHECK_RDF_ID:
    case RAPTOR_FEATURE_NO_NET:
    case RAPTOR_FEATURE_HTML_TAG_SOUP:
    case RAPTOR_FEATURE_MICROFORMATS:
    case RAPTOR_FEATURE_HTML_LINK:
    case RAPTOR_FEATURE_WWW_TIMEOUT:
      parser->features[(int)feature]=value;
      break;

    case RAPTOR_FEATURE_ASSUME_IS_RDF:
      break;


    case RAPTOR_FEATURE_WRITE_BASE_URI:
    case RAPTOR_FEATURE_RELATIVE_URIS:
    case RAPTOR_FEATURE_START_URI:
    case RAPTOR_FEATURE_WRITER_AUTO_INDENT:
    case RAPTOR_FEATURE_WRITER_AUTO_EMPTY:
    case RAPTOR_FEATURE_WRITER_INDENT_WIDTH:
    case RAPTOR_FEATURE_WRITER_XML_VERSION:
    case RAPTOR_FEATURE_WRITER_XML_DECLARATION:

    case RAPTOR_FEATURE_RESOURCE_BORDER:
    case RAPTOR_FEATURE_LITERAL_BORDER:
    case RAPTOR_FEATURE_BNODE_BORDER:
    case RAPTOR_FEATURE_RESOURCE_FILL:
    case RAPTOR_FEATURE_LITERAL_FILL:
    case RAPTOR_FEATURE_BNODE_FILL:

    case RAPTOR_FEATURE_JSON_CALLBACK:
    case RAPTOR_FEATURE_JSON_EXTRA_DATA:

    case RAPTOR_FEATURE_WWW_HTTP_CACHE_CONTROL:
    case RAPTOR_FEATURE_WWW_HTTP_USER_AGENT:
    default:
      return -1;
      break;
  }

  return 0;
}


/**
 * raptor_parser_set_feature_string:
 * @parser: #raptor_parser parser object
 * @feature: feature to set from enumerated #raptor_feature values
 * @value: feature value
 *
 * Set parser features with string values.
 * 
 * The allowed features are available via raptor_features_enumerate().
 * If the feature type is integer, the value is interpreted as an integer.
 *
 * Return value: non 0 on failure or if the feature is unknown
 **/
int
raptor_parser_set_feature_string(raptor_parser *parser, 
                                 raptor_feature feature, 
                                 const unsigned char *value)
{
  int value_is_string=(raptor_feature_value_type(feature) == 1);
  if(!value_is_string)
    return raptor_set_feature(parser, feature, atoi((const char*)value));

  if((feature == RAPTOR_FEATURE_WWW_HTTP_CACHE_CONTROL) ||
     (feature == RAPTOR_FEATURE_WWW_HTTP_USER_AGENT)) {
    char *value_copy;
    size_t len=0;
    if(value)
      len=strlen((const char*)value);
    value_copy=(char*)RAPTOR_MALLOC(cstring, len+1);
    if(!value_copy)
      return 1;
    
    if(len)
      strncpy(value_copy, (const char*)value, len);
    value_copy[len]='\0';
    
    if(feature == RAPTOR_FEATURE_WWW_HTTP_CACHE_CONTROL)
      parser->cache_control=value_copy;
    else
      parser->user_agent=value_copy;

    return 0;
  }

  return -1;
}


/**
 * raptor_get_feature:
 * @parser: #raptor_parser parser object
 * @feature: feature to get value
 *
 * Get various parser features.
 * 
 * The allowed features are available via raptor_features_enumerate().
 *
 * Note: no feature value is negative
 *
 * Return value: feature value or < 0 for an illegal feature
 **/
int
raptor_get_feature(raptor_parser *parser, raptor_feature feature)
{
  int result= -1;
  
  switch(feature) {
    case RAPTOR_FEATURE_SCANNING:
    case RAPTOR_FEATURE_ALLOW_NON_NS_ATTRIBUTES:
    case RAPTOR_FEATURE_ALLOW_OTHER_PARSETYPES:
    case RAPTOR_FEATURE_ALLOW_BAGID:
    case RAPTOR_FEATURE_ALLOW_RDF_TYPE_RDF_LIST:
    case RAPTOR_FEATURE_NORMALIZE_LANGUAGE:
    case RAPTOR_FEATURE_NON_NFC_FATAL:
    case RAPTOR_FEATURE_WARN_OTHER_PARSETYPES:
    case RAPTOR_FEATURE_CHECK_RDF_ID:
    case RAPTOR_FEATURE_NO_NET:
    case RAPTOR_FEATURE_HTML_TAG_SOUP:
    case RAPTOR_FEATURE_MICROFORMATS:
    case RAPTOR_FEATURE_HTML_LINK:
    case RAPTOR_FEATURE_WWW_TIMEOUT:
      result=(parser->features[(int)feature] != 0);
      break;

    case RAPTOR_FEATURE_ASSUME_IS_RDF:
      result=0;
      break;

    /* serializing features */
    case RAPTOR_FEATURE_WRITE_BASE_URI:
    case RAPTOR_FEATURE_RELATIVE_URIS:
    case RAPTOR_FEATURE_START_URI:
    case RAPTOR_FEATURE_RESOURCE_BORDER:
    case RAPTOR_FEATURE_LITERAL_BORDER:
    case RAPTOR_FEATURE_BNODE_BORDER:
    case RAPTOR_FEATURE_RESOURCE_FILL:
    case RAPTOR_FEATURE_LITERAL_FILL:
    case RAPTOR_FEATURE_BNODE_FILL:
    case RAPTOR_FEATURE_JSON_CALLBACK:
    case RAPTOR_FEATURE_JSON_EXTRA_DATA:

    /* XML writer features */
    case RAPTOR_FEATURE_WRITER_AUTO_INDENT:
    case RAPTOR_FEATURE_WRITER_AUTO_EMPTY:
    case RAPTOR_FEATURE_WRITER_INDENT_WIDTH:
    case RAPTOR_FEATURE_WRITER_XML_VERSION:
    case RAPTOR_FEATURE_WRITER_XML_DECLARATION:

    /* WWW features */
    case RAPTOR_FEATURE_WWW_HTTP_CACHE_CONTROL:
    case RAPTOR_FEATURE_WWW_HTTP_USER_AGENT:
      
    default:
      break;
  }
  
  return result;
}


/**
 * raptor_parser_get_feature_string:
 * @parser: #raptor_parser parser object
 * @feature: feature to get value
 *
 * Get parser features with string values.
 * 
 * The allowed features are available via raptor_features_enumerate().
 * If a string is returned, it must be freed by the caller.
 *
 * Return value: feature value or NULL for an illegal feature or no value
 **/
const unsigned char *
raptor_parser_get_feature_string(raptor_parser *parser, 
                                 raptor_feature feature)
{
  int value_is_string=(raptor_feature_value_type(feature) == 1);
  if(!value_is_string)
    return NULL;
  
  return NULL;
}


/**
 * raptor_set_parser_strict:
 * @rdf_parser: #raptor_parser object
 * @is_strict: Non 0 for strict parsing
 *
 * Set parser to strict / lax mode.
 * 
 **/
void
raptor_set_parser_strict(raptor_parser* rdf_parser, int is_strict)
{
  is_strict=(is_strict) ? 1 : 0;

  /* Initialise default parser mode */
  rdf_parser->features[RAPTOR_FEATURE_SCANNING]=0;

  rdf_parser->features[RAPTOR_FEATURE_ALLOW_NON_NS_ATTRIBUTES]=!is_strict;
  rdf_parser->features[RAPTOR_FEATURE_ALLOW_OTHER_PARSETYPES]=!is_strict;
  rdf_parser->features[RAPTOR_FEATURE_ALLOW_BAGID]=!is_strict;
  rdf_parser->features[RAPTOR_FEATURE_ALLOW_RDF_TYPE_RDF_LIST]=0;
  rdf_parser->features[RAPTOR_FEATURE_NORMALIZE_LANGUAGE]=1;
  rdf_parser->features[RAPTOR_FEATURE_NON_NFC_FATAL]=is_strict;
  rdf_parser->features[RAPTOR_FEATURE_WARN_OTHER_PARSETYPES]=!is_strict;
  rdf_parser->features[RAPTOR_FEATURE_CHECK_RDF_ID]=1;
  rdf_parser->features[RAPTOR_FEATURE_HTML_TAG_SOUP]=!is_strict;
  rdf_parser->features[RAPTOR_FEATURE_MICROFORMATS]=!is_strict;
  rdf_parser->features[RAPTOR_FEATURE_HTML_LINK]=!is_strict;
}


/**
 * raptor_set_default_generate_id_parameters:
 * @rdf_parser: #raptor_parser object
 * @prefix: prefix string
 * @base: integer base identifier
 *
 * Set default ID generation parameters.
 *
 * Sets the parameters for the default algorithm used to generate IDs.
 * The default algorithm uses both @prefix and @base to generate a new
 * identifier.   The exact identifier generated is not guaranteed to
 * be a strict concatenation of @prefix and @base but will use both
 * parts. The @prefix parameter is copied to generate an ID.
 *
 * For finer control of the generated identifiers, use
 * raptor_set_default_generate_id_handler().
 *
 * If @prefix is NULL, the default prefix is used (currently "genid")
 * If @base is less than 1, it is initialised to 1.
 * 
 **/
void
raptor_set_default_generate_id_parameters(raptor_parser* rdf_parser, 
                                          char *prefix, int base)
{
  char *prefix_copy=NULL;
  size_t length=0;

  if(--base<0)
    base=0;

  if(prefix) {
    length=strlen(prefix);
    
    prefix_copy=(char*)RAPTOR_MALLOC(cstring, length+1);
    if(!prefix_copy)
      return;
    strcpy(prefix_copy, prefix);
  }
  
  if(rdf_parser->default_generate_id_handler_prefix)
    RAPTOR_FREE(cstring, rdf_parser->default_generate_id_handler_prefix);

  rdf_parser->default_generate_id_handler_prefix=prefix_copy;
  rdf_parser->default_generate_id_handler_prefix_length=length;
  rdf_parser->default_generate_id_handler_base=base;
}


/**
 * raptor_get_name:
 * @rdf_parser: #raptor_parser parser object
 *
 * Get the name of a parser.
 *
 * Return value: the short name for the parser.
 **/
const char*
raptor_get_name(raptor_parser *rdf_parser) 
{
  return rdf_parser->factory->name;
}


/**
 * raptor_get_label:
 * @rdf_parser: #raptor_parser parser object
 *
 * Get a descriptive label of a parser.
 *
 * Return value: a readable label for the parser.
 **/
const char*
raptor_get_label(raptor_parser *rdf_parser) 
{
  return rdf_parser->factory->label;
}


/**
 * raptor_get_mime_type:
 * @rdf_parser: #raptor_parser parser object
 *
 * Return MIME type for the parser.
 *
 * Return value: MIME type or NULL if none available
 **/
const char*
raptor_get_mime_type(raptor_parser *rdf_parser) 
{
  const char *mime_type=NULL;
  if(rdf_parser->factory->mime_types) {
    raptor_type_q* tq;
    tq=(raptor_type_q*)raptor_sequence_get_at(rdf_parser->factory->mime_types, 0);
    if(tq)
      mime_type=tq->mime_type;
  }
  
  return mime_type;
}


/**
 * raptor_get_need_base_uri:
 * @rdf_parser: #raptor_parser parser object
 *
 * Get a boolean whether this parser needs a base URI to start parsing.
 *
 * Return value: non-0 if this parser needs a base URI
 **/
int
raptor_get_need_base_uri(raptor_parser *rdf_parser)
{
  return rdf_parser->factory->need_base_uri;
}


/**
 * raptor_parse_abort:
 * @rdf_parser: #raptor_parser parser object
 *
 * Abort an ongoing parse.
 * 
 * Causes any ongoing generation of statements by a parser to be
 * terminated and the parser to return controlto the application
 * as soon as draining any existing buffers.
 *
 * Most useful inside raptor_parse_file or raptor_parse_uri when
 * the Raptor library is directing the parsing and when one of the
 * callback handlers such as as set by raptor_set_statement_handler
 * requires to return to the main application code.
 **/
void
raptor_parse_abort(raptor_parser *rdf_parser)
{
  rdf_parser->failed=1;
}


static unsigned char*
raptor_default_generate_id_handler(void *user_data, raptor_genid_type type,
                                   unsigned char *user_bnodeid) 
{
  raptor_parser *rdf_parser=(raptor_parser *)user_data;
  int id;
  unsigned char *buffer;
  int length;
  int tmpid;

  if(user_bnodeid)
    return user_bnodeid;

  id=++rdf_parser->default_generate_id_handler_base;

  tmpid=id;
  length=2; /* min length 1 + \0 */
  while(tmpid/=10)
    length++;

  if(rdf_parser->default_generate_id_handler_prefix)
    length += rdf_parser->default_generate_id_handler_prefix_length;
  else
    length += 5; /* genid */
  
  buffer=(unsigned char*)RAPTOR_MALLOC(cstring, length);
  if(!buffer)
    return NULL;
  if(rdf_parser->default_generate_id_handler_prefix) {
    strncpy((char*)buffer, rdf_parser->default_generate_id_handler_prefix,
            rdf_parser->default_generate_id_handler_prefix_length);
    sprintf((char*)buffer+rdf_parser->default_generate_id_handler_prefix_length,
            "%d", id);
  } else 
    sprintf((char*)buffer, "genid%d", id);

  return buffer;
}


/**
 * raptor_parser_generate_id:
 * @rdf_parser: #raptor_parser parser object
 * @type: Type of ID to generate
 * 
 * Generate an ID for a parser
 *
 * Type can be either RAPTOR_GENID_TYPE_BNODEID or
 * RAPTOR_GENID_TYPE_BAGID
 * 
 * Return value: newly allocated generated ID or NULL on failure
 **/
unsigned char*
raptor_parser_generate_id(raptor_parser *rdf_parser, raptor_genid_type type)
{
  if(type != RAPTOR_GENID_TYPE_BNODEID || 
     type != RAPTOR_GENID_TYPE_BAGID)
    return NULL;
  
  return raptor_parser_internal_generate_id(rdf_parser, type, NULL);
}


unsigned char*
raptor_parser_internal_generate_id(raptor_parser *rdf_parser, 
                                   raptor_genid_type type,
                                   unsigned char *user_bnodeid)
{
  if(rdf_parser->generate_id_handler)
    return rdf_parser->generate_id_handler(rdf_parser->generate_id_handler_user_data,
                                           type, user_bnodeid);
  else
    return raptor_default_generate_id_handler(rdf_parser, type, user_bnodeid);
}


/**
 * raptor_get_locator:
 * @rdf_parser: raptor parser
 *
 * Get the current raptor locator object.
 * 
 * Return value: raptor locator
 **/
raptor_locator*
raptor_get_locator(raptor_parser *rdf_parser) 
{
  return &rdf_parser->locator;
}


#ifdef RAPTOR_DEBUG
void
raptor_stats_print(raptor_parser *rdf_parser, FILE *stream)
{
#ifdef RAPTOR_PARSER_RDFXML
#if RAPTOR_DEBUG > 1
  if(!strcmp(rdf_parser->factory->name, "rdfxml")) {
    raptor_rdfxml_parser *rdf_xml_parser=(raptor_rdfxml_parser*)rdf_parser->context;
    fputs("raptor parser stats\n  ", stream);
    raptor_rdfxml_parser_stats_print(rdf_xml_parser, stream);
  }
#endif
#endif
}
#endif


struct syntax_score
{
  int score;
  raptor_parser_factory* factory;
};


static int
compare_syntax_score(const void *a, const void *b) {
  return ((struct syntax_score*)b)->score - ((struct syntax_score*)a)->score;
}
  

/**
 * raptor_guess_parser_name:
 * @uri: URI identifying the syntax (or NULL)
 * @mime_type: mime type identifying the content (or NULL)
 * @buffer: buffer of content to guess (or NULL)
 * @len: length of buffer
 * @identifier: identifier of content (or NULL)
 *
 * Guess a parser name for content.
 * 
 * Find a parser by scoring recognition of the syntax by a block of
 * characters, the content identifier or a mime type.  The content
 * identifier is typically a filename or URI or some other identifier.
 * 
 * Return value: a parser name or NULL if no guess could be made
 **/
const char*
raptor_guess_parser_name(raptor_uri *uri, const char *mime_type,
                         const unsigned char *buffer, size_t len,
                         const unsigned char *identifier)
{
  unsigned int i;
  raptor_parser_factory *factory;
  unsigned char *suffix=NULL;
/* FIXME - up to 10 parsers :) */
#define MAX_PARSERS 10
  struct syntax_score scores[MAX_PARSERS];

  if(identifier) {
    unsigned char *p=(unsigned char*)strrchr((const char*)identifier, '.');
    if(p) {
      unsigned char *from, *to;
      p++;
      suffix=(unsigned char*)RAPTOR_MALLOC(cstring, strlen((const char*)p)+1);
      if(!suffix)
        return NULL;
      for(from=p, to=suffix; *from; ) {
        unsigned char c=*from++;
        /* discard the suffix if it wasn't '\.[a-zA-Z0-9]+$' */
        if(!isalpha(c) && !isdigit(c)) {
          RAPTOR_FREE(cstring, suffix);
          suffix=NULL;
          to=NULL;
          break;
        }
        *to++=isupper((char)c) ? (unsigned char)tolower((char)c): c;
      }
      if(to)
        *to='\0';
    }
  }

  for(i=0;
      (factory=(raptor_parser_factory*)raptor_sequence_get_at(parsers, i));
      i++) {
    int score= -1;
    raptor_type_q* type_q=NULL;
    
    if(mime_type && factory->mime_types) {
      int j;
      type_q=NULL;
      for(j=0; 
          (type_q=(raptor_type_q*)raptor_sequence_get_at(factory->mime_types, j)); 
          j++) {
        if(!strcmp(mime_type, type_q->mime_type))
          break;
      }
      /* got an exact match mime type - score it via the Q */
      if(type_q)
        score=type_q->q;
    }
    /* mime type match has high Q - return result */
    if(score >= 10)
      break;
    
    if(uri && factory->uri_string &&
       !strcmp((const char*)raptor_uri_as_string(uri), 
               (const char*)factory->uri_string))
      /* got an exact match syntax for URI - return result */
      break;

    if(factory->recognise_syntax) {
      int c= -1;
    
      /* Only use first N bytes to avoid HTML documents that contain
       * RDF/XML examples
       */
#define FIRSTN 1024
      if(buffer && len && len > FIRSTN) {
        c=buffer[FIRSTN];
        ((char*)buffer)[FIRSTN]='\0';
      }

      score += factory->recognise_syntax(factory, buffer, len, 
                                         identifier, suffix, 
                                         mime_type);

      if(c >= 0)
        ((char*)buffer)[FIRSTN]=c;
    }

    if(i > MAX_PARSERS) {
      raptor_finish();
      RAPTOR_FATAL2("Number of parsers greater than static buffer size %d\n",
                    MAX_PARSERS);
    }

    scores[i].score=score < 10 ? score : 10; scores[i].factory=factory;
#if RAPTOR_DEBUG > 2
    RAPTOR_DEBUG3("Score %15s : %d\n", factory->name, score);
#endif
  }
  
  if(!factory) {
    /* sort the scores and pick a factory */
    qsort(scores, i, sizeof(struct syntax_score), compare_syntax_score);
    if(scores[0].score >= 0)
      factory=scores[0].factory;
  }

  if(suffix)
    RAPTOR_FREE(cstring, suffix);

  return factory ? factory->name : NULL;
}


/*
 * raptor_parser_copy_user_state:
 * @to_parser: destination parser
 * @from_parser: source parser
 * 
 * Copy user state between parsers - INTERNAL.
 *
 **/
void
raptor_parser_copy_user_state(raptor_parser *to_parser, 
                              raptor_parser *from_parser)
{
  int i;
  
  to_parser->user_data= from_parser->user_data;
  memcpy(&to_parser->error_handlers, &from_parser->error_handlers,
         sizeof(raptor_error_handlers));
  to_parser->statement_handler= from_parser->statement_handler;
  to_parser->generate_id_handler_user_data= from_parser->generate_id_handler_user_data;
  to_parser->generate_id_handler= from_parser->generate_id_handler;
  to_parser->default_generate_id_handler_base= from_parser->default_generate_id_handler_base;
  /* copy over non-shared user state - generate ID prefix string */
  if(from_parser->default_generate_id_handler_prefix) {
    to_parser->default_generate_id_handler_prefix=(char*)RAPTOR_MALLOC(cstring,
                    from_parser->default_generate_id_handler_prefix_length+1);
    strncpy((char*)to_parser->default_generate_id_handler_prefix, 
            (const char*)from_parser->default_generate_id_handler_prefix,
            from_parser->default_generate_id_handler_prefix_length+1);
  }
  to_parser->default_generate_id_handler_prefix_length= from_parser->default_generate_id_handler_prefix_length;
  to_parser->namespace_handler= from_parser->namespace_handler;
  to_parser->namespace_handler_user_data= from_parser->namespace_handler_user_data;
  to_parser->uri_filter= from_parser->uri_filter;
  to_parser->uri_filter_user_data= from_parser->uri_filter_user_data;

  /* copy features */
  for(i=0; i<= RAPTOR_FEATURE_LAST; i++)
    to_parser->features[i]= from_parser->features[i];

}


/*
 * raptor_parser_start_namespace:
 * @rdf_parser: parser
 * @nspace: namespace starting
 * 
 * Internal - Invoke start namespace handler
 **/
void
raptor_parser_start_namespace(raptor_parser* rdf_parser, 
                              raptor_namespace* nspace)
{
  if(!rdf_parser->namespace_handler)
    return;

  (*rdf_parser->namespace_handler)(rdf_parser->namespace_handler_user_data, 
                                   nspace);
}


/**
 * raptor_parser_get_accept_header:
 * @rdf_parser: parser
 * 
 * Get an HTTP Accept value for the parser.
 *
 * The returned string must be freed by the caller such as with
 * raptor_free_memory().
 *
 * Return value: a new Accept: header string or NULL on failure
 **/
const char*
raptor_parser_get_accept_header(raptor_parser* rdf_parser)
{
  raptor_parser_factory *factory=rdf_parser->factory;
  char *accept_header=NULL;
  size_t len;
  char *p;
  int i;
  raptor_type_q* type_q;
  
  if(factory->accept_header)
    return factory->accept_header(rdf_parser);

  if(!factory->mime_types)
    return NULL;

  len=0;
  for(i=0; 
      (type_q=(raptor_type_q*)raptor_sequence_get_at(factory->mime_types, i));
      i++) {
    if(type_q->mime_type) {
      len+= type_q->mime_type_len + 2; /* ", " */
      if(type_q->q < 10)
        len+= 6; /* ";q=X.Y" */
    }
  }
  
  /* 9 = "\*\/\*;q=0.1" */
  accept_header=(char*)RAPTOR_MALLOC(cstring, len + 9 + 1);
  if(!accept_header)
    return NULL;

  p=accept_header;
  for(i=0; 
      (type_q=(raptor_type_q*)raptor_sequence_get_at(factory->mime_types, i));
      i++) {
    if(type_q->mime_type) {
      strncpy(p, type_q->mime_type, type_q->mime_type_len);
      p+= type_q->mime_type_len;
      if(type_q->q < 10) {
        *p++ = ';';
        *p++ = 'q';
        *p++ = '=';
        *p++ = '0';
        *p++ = '.';
        *p++ = '0' + (type_q->q);
      }
    }
    
    *p++ = ',';
    *p++ = ' ';
  }

  strncpy(p, "*/*;q=0.1", 10);

  return accept_header;
}


const char*
raptor_parser_get_accept_header_all(void)
{
  raptor_parser_factory *factory;
  char *accept_header=NULL;
  size_t len;
  char *p;
  int i;
  
  len=0;
  for(i=0;
      (factory=(raptor_parser_factory*)raptor_sequence_get_at(parsers, i));
      i++) {
    raptor_type_q* type_q;
    int j;
    
    for(j=0;
        (type_q=(raptor_type_q*)raptor_sequence_get_at(factory->mime_types, j));
        j++) {
      if(type_q->mime_type) {
        len+= type_q->mime_type_len + 2; /* ", " */
        if(type_q->q < 10)
          len+= 6; /* ";q=X.Y" */
      }
    }
  }
  
  /* 9 = "\*\/\*;q=0.1" */
  accept_header=(char*)RAPTOR_MALLOC(cstring, len + 9 + 1);
  if(!accept_header)
    return NULL;

  p=accept_header;
  for(i=0;
      (factory=(raptor_parser_factory*)raptor_sequence_get_at(parsers, i));
      i++) {
    raptor_type_q* type_q;
    int j;
    
    for(j=0; 
        (type_q=(raptor_type_q*)raptor_sequence_get_at(factory->mime_types, j));
        j++) {
      if(type_q->mime_type) {
        strncpy(p, type_q->mime_type, type_q->mime_type_len);
        p+= type_q->mime_type_len;
        if(type_q->q < 10) {
          *p++ = ';';
          *p++ = 'q';
          *p++ = '=';
          *p++ = '0';
          *p++ = '.';
          *p++ = '0' + (type_q->q);
        }
      }
      
      *p++ = ',';
      *p++ = ' ';
    }
    
  }
  
  strncpy(p, "*/*;q=0.1", 10);
  
  return accept_header;
}


void
raptor_parser_save_content(raptor_parser* rdf_parser, int save)
{
  if(rdf_parser->sb)
    raptor_free_stringbuffer(rdf_parser->sb);

  rdf_parser->sb= save ? raptor_new_stringbuffer() : NULL;
}


const unsigned char*
raptor_parser_get_content(raptor_parser* rdf_parser, size_t* length_p)
{
  unsigned char* buffer;
  size_t len;
  
  if(!rdf_parser->sb)
    return NULL;
  
  len=raptor_stringbuffer_length(rdf_parser->sb);
  buffer=(unsigned char*)RAPTOR_MALLOC(cstring, len+1);
  if(!buffer)
    return NULL;

  raptor_stringbuffer_copy_to_string(rdf_parser->sb, buffer, len);

  if(length_p)
    *length_p=len;

  return buffer;
}


void 
raptor_parser_set_graph_name(raptor_parser* parser, raptor_uri* uri)
{
  if(parser->graph_handler)
    (*parser->graph_handler)(parser->user_data, uri);
}


int
raptor_parser_get_current_base_id(raptor_parser* parser)
{
  if(parser->factory->get_current_base_id)
    return parser->factory->get_current_base_id(parser);
  else
    return parser->default_generate_id_handler_base;
}



/* end not STANDALONE */
#endif


#ifdef STANDALONE
#include <stdio.h>

int main(int argc, char *argv[]);


int
main(int argc, char *argv[])
{
#ifdef RAPTOR_DEBUG
  const char *program=raptor_basename(argv[0]);
#endif
  int i;
  const char *s;

  raptor_init();
  
#ifdef RAPTOR_DEBUG
  fprintf(stderr, "%s: Known features:\n", program);
#endif

  for(i=0; i <= RAPTOR_FEATURE_LAST; i++) {
    const char *feature_name;
    const char *feature_label;
    raptor_uri *feature_uri;
    int fn;
    
    if(raptor_features_enumerate((raptor_feature)i,
                                 &feature_name, &feature_uri, &feature_label))
      continue;

#ifdef RAPTOR_DEBUG
    fprintf(stderr, " %2d %-20s %s\n", i, feature_name, feature_label);
#endif
    fn=raptor_feature_from_uri(feature_uri);
    if(fn != i) {
      fprintf(stderr, "raptor_feature_from_uri returned %d expected %d\n", fn, i);
      return 1;
    }
    raptor_free_uri(feature_uri);
  }

  s=raptor_parser_get_accept_header_all();
  fprintf(stderr, "Default HTTP accept header: '%s'\n", s);
  RAPTOR_FREE(cstring, s);

  raptor_finish();
  
  return 0;
}

#endif
