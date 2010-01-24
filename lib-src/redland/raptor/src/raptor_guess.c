/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_guess.c - Raptor guessing real parser implementation
 *
 * Copyright (C) 2005-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2005, University of Bristol, UK http://www.bristol.ac.uk/
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
 * guess parser object
 */
struct raptor_guess_parser_context_s {
  /* content type got from URI request */
  char* content_type;

  /* URI from start_parse */
  raptor_uri* uri;

  /* Non-0 when we need to guess */
  int do_guess;
  
  /* Actual parser to use */
  raptor_parser* parser;
};


typedef struct raptor_guess_parser_context_s raptor_guess_parser_context;


static int
raptor_guess_parse_init(raptor_parser* rdf_parser, const char *name)
{
  raptor_guess_parser_context *guess_parser=(raptor_guess_parser_context*)rdf_parser->context;
  guess_parser->content_type=NULL;

  guess_parser->do_guess=1;

  return 0;
}


static void
raptor_guess_parse_terminate(raptor_parser *rdf_parser)
{
  raptor_guess_parser_context *guess_parser=(raptor_guess_parser_context*)rdf_parser->context;

  if(guess_parser->content_type)
    RAPTOR_FREE(cstring, guess_parser->content_type);

  if(guess_parser->parser)
    raptor_free_parser(guess_parser->parser);
}


static void
raptor_guess_parse_content_type_handler(raptor_parser* rdf_parser, 
                                        const char* content_type)
{
  raptor_guess_parser_context* guess_parser=(raptor_guess_parser_context*)rdf_parser->context;

  if(content_type) {
    const char *p;
    size_t len;

    if((p=strchr(content_type,';')))
      len=p-content_type;
    else
      len=strlen(content_type);
    
    guess_parser->content_type=(char*)RAPTOR_MALLOC(cstring, len+1);
    strncpy(guess_parser->content_type, content_type, len);
    guess_parser->content_type[len]='\0';

    RAPTOR_DEBUG2("Got content type '%s'\n", guess_parser->content_type);
  }
}


static int
raptor_guess_parse_chunk(raptor_parser* rdf_parser, 
                        const unsigned char *buffer, size_t len,
                        int is_end)
{
  raptor_guess_parser_context* guess_parser=(raptor_guess_parser_context*)rdf_parser->context;

  if(guess_parser->do_guess) {
    const unsigned char *identifier=NULL;
    const char *name;
    
    guess_parser->do_guess=0;

    if(rdf_parser->base_uri)
      identifier=raptor_uri_as_string(rdf_parser->base_uri);
    
    name=raptor_guess_parser_name(NULL, guess_parser->content_type, buffer, len,
                                  identifier);
    if(!name) {
      raptor_parser_error(rdf_parser,
                          "Failed to guess parser from content type '%s'",
                          guess_parser->content_type ? 
                          guess_parser->content_type : "(none)");
      raptor_parse_abort(rdf_parser);
      if(guess_parser->parser) {
        raptor_free_parser(guess_parser->parser);
        guess_parser->parser=NULL;
      }
      return 1;
    } else {
    
#if RAPTOR_DEBUG > 1
      RAPTOR_DEBUG2("Guessed parser name '%s'\n", name);
#endif
      
      if(guess_parser->parser) {
        raptor_parser_factory* factory=raptor_get_parser_factory(name);

        if(guess_parser->parser->factory != factory) {
          raptor_free_parser(guess_parser->parser);
          guess_parser->parser=NULL;
        }
      }
      
      if(!guess_parser->parser) {
        guess_parser->parser=raptor_new_parser(name);
        if(!guess_parser->parser)
          return 1;
      }

      /* copy any user data to the grddl parser */
      if(raptor_parser_copy_user_state(guess_parser->parser, rdf_parser))
        return 1;
      
      if(raptor_start_parse(guess_parser->parser, rdf_parser->base_uri))
        return 1;
    }
  }
  

  /* now we can pass on calls to internal guess_parser */
  return raptor_parse_chunk(guess_parser->parser, buffer, len, is_end);
}


static const char*
raptor_guess_accept_header(raptor_parser* rdf_parser)
{
  return raptor_parser_get_accept_header_all();
}


static int
raptor_guess_get_current_base_id(raptor_parser* rdf_parser)
{
  raptor_guess_parser_context *guess_parser=(raptor_guess_parser_context*)rdf_parser->context;

  return raptor_parser_get_current_base_id(guess_parser->parser);
}


static int
raptor_guess_parser_register_factory(raptor_parser_factory *factory) 
{
  factory->context_length     = sizeof(raptor_guess_parser_context);
  
  factory->need_base_uri = 1;
  
  factory->init      = raptor_guess_parse_init;
  factory->terminate = raptor_guess_parse_terminate;
  factory->chunk     = raptor_guess_parse_chunk;
  factory->content_type_handler = raptor_guess_parse_content_type_handler;
  factory->accept_header = raptor_guess_accept_header;
  factory->get_current_base_id = raptor_guess_get_current_base_id;

  return 0;
}


int
raptor_init_parser_guess(void)
{
  return !raptor_parser_register_factory("guess",  
                                         "Pick the parser to use using content type and URI",
                                         &raptor_guess_parser_register_factory);
}
