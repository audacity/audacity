/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_query_triples.c - RDF Query simple triple query language
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
#include <sys/types.h>

#include <redland.h>


typedef struct
{
  librdf_statement statement; /* statement to match */
  librdf_model *model;
} librdf_query_triples_context;


/* prototypes for local functions */
static int librdf_query_triples_init(librdf_query* query, const char *name, librdf_uri* uri, const unsigned char *query_string, librdf_uri* base_uri);
static librdf_query_results* librdf_query_triples_query_execute(librdf_query* query, librdf_model* model);
static librdf_stream* librdf_query_triples_results_as_stream(librdf_query_results* query_results);


static void librdf_query_triples_register_factory(librdf_query_factory *factory);


/*
 * librdf_query_triples_find_next_term - Locate end of triples query term
 * @string: the string to search
 * 
 * Find the character after a term like '[...]' or '"..."' or NULL
 * if not found, end of string 
 * 
 * Return value: pointer to the character in the string or NULL
 **/
static unsigned char *
librdf_query_triples_find_next_term(unsigned char *string) 
{
  unsigned char c;
  unsigned char delim='\0';

  if(!string)
    return NULL;
  
  switch(*string++) {
    case '"':
      delim='"';
      break;
    case '[':
      delim=']';
      break;
    case '-':
      return string;
    default:
      /* Bad value - includes '\0' */
      return NULL;
  }

  while((c=*string++)) {
    if(c == delim)
      break;
  }
  
  if(!c)
    string=NULL;
  
  return string;
}



/* functions implementing query api */


/**
 * librdf_query_triples_init:
 * @query: the #librdf_query
 * @name: the query language name
 * @uri: the query language URI or NULL
 * @query_string: the query string
 *
 * Initialise a triples query from the string.
 * 
 * Parses the query string in the triples form to create an internal
 * representation, suitable for use in querying.
 *
 * The syntax of the query format is as follows:
 *   query     := subject ' ' predicate ' ' object
 *   subject   := null | uri
 *   predicate := null | uri
 *   object    := null | uri | literal
 *   null      : '-'
 *   uri       : '[' URI-string ']'
 *   literal   : ''' string '''
 * 
 * Return value: 
 **/
static int
librdf_query_triples_init(librdf_query* query, 
                          const char *name, librdf_uri* uri,
                          const unsigned char* query_string, 
                          librdf_uri* base_uri)
{
  librdf_query_triples_context *context=(librdf_query_triples_context*)query->context;
  int len;
  unsigned char *query_string_copy;
  unsigned char *cur, *p;
  librdf_node *subject, *predicate, *object;

  librdf_statement_init(query->world, &context->statement);
  
  len=strlen((const char*)query_string);
  query_string_copy=(unsigned char*)LIBRDF_MALLOC(cstring, len+1);
  if(!query_string_copy)
    return 0;
  strcpy((char*)query_string_copy, (const char*)query_string);

  cur=query_string_copy;
  
  /* subject - NULL or URI */
  p=librdf_query_triples_find_next_term(cur);
  if(!p) {
    librdf_log(query->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_QUERY, NULL,
               "Bad triples query language syntax - bad subject in '%s'", cur);
    LIBRDF_FREE(cstring, query_string_copy);
    return 0;
  }
  *p='\0';
  p++;
  if(strcmp((const char*)cur, "-")) {
    /* Expecting query_string='[URI]' */
    cur++; /* Move past '[' */
    p[-2]='\0';     /* Zap ']' */
    subject=librdf_new_node_from_uri_string(query->world, cur);
    if(!subject) {
      LIBRDF_FREE(cstring, query_string_copy);
      return 0;
    }
    librdf_statement_set_subject(&context->statement, subject);
  } else
   subject=NULL;
  cur=p;
  
  /* predicate - NULL or URI */
  p=(unsigned char*)librdf_query_triples_find_next_term(cur);
  if(!p) {
    librdf_log(query->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_QUERY, NULL,
               "Bad triples query language syntax - bad predicate in '%s'", cur);
    LIBRDF_FREE(cstring, query_string_copy);
    librdf_free_node(subject);
    return 0;
  }
  *p='\0';
  p++;
  if(strcmp((const char*)cur, "-")) {
    /* Expecting cur='[URI]' */
    cur++; /* Move past '[' */
    p[-2]='\0';     /* Zap ']' */
    predicate=librdf_new_node_from_uri_string(query->world, cur);
    if(!predicate) {
      LIBRDF_FREE(cstring, query_string_copy);
      librdf_free_node(subject);
      return 0;
    }
    librdf_statement_set_predicate(&context->statement, predicate);
  } else
   predicate=NULL;
  cur=p;
  
  /* object - NULL, literal or URI */
  p=librdf_query_triples_find_next_term(cur);
  if(!p) {
    librdf_log(query->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_QUERY, NULL,
               "Bad triples query language syntax - bad object in '%s'", cur);
    LIBRDF_FREE(cstring, query_string_copy);
    if(subject)
      librdf_free_node(subject);
    if(predicate)
      librdf_free_node(predicate);
    return 0;
  }
  *p='\0';
  p++;
  if(strcmp((const char*)cur, "-")) {
    /* Expecting cur='[URI]' or '"string"' */
    cur++; /* Move past '[' or '"' */
    p[-2]='\0';     /* Zap ']' or '"' */
    if(cur[-1] == '"')
      object=librdf_new_node_from_literal(query->world, cur, NULL, 0);
    else
      object=librdf_new_node_from_uri_string(query->world, cur);
    if(!object) {
      LIBRDF_FREE(cstring, query_string_copy);
      if(subject)
        librdf_free_node(subject);
      if(predicate)
        librdf_free_node(predicate);
      return 0;
    }
    librdf_statement_set_object(&context->statement, object);
  }

  LIBRDF_FREE(cstring, query_string_copy);
  
  return 0;
}


static void
librdf_query_triples_terminate(librdf_query* query)
{
  librdf_query_triples_context *context=(librdf_query_triples_context*)query->context;
  librdf_node *node;

  node=librdf_statement_get_subject(&context->statement);
  if(node)
    librdf_free_node(node);

  node=librdf_statement_get_predicate(&context->statement);
  if(node)
    librdf_free_node(node);

  node=librdf_statement_get_object(&context->statement);
  if(node)
    librdf_free_node(node);

}


static librdf_query_results*
librdf_query_triples_query_execute(librdf_query* query, librdf_model* model)
{
  librdf_query_triples_context* context=(librdf_query_triples_context*)query->context;
  librdf_query_results* results;

  results=(librdf_query_results*)LIBRDF_MALLOC(librdf_query_results, sizeof(librdf_query_results));
  results->query=query;
  
  context->model=model;
  
  return results;
}


static librdf_stream*
librdf_query_triples_results_as_stream(librdf_query_results* query_results)
{
  librdf_query_triples_context* context=(librdf_query_triples_context*)query_results->query->context;

  return librdf_model_find_statements(context->model, &context->statement);
}


/* local function to register list query functions */

static void
librdf_query_triples_register_factory(librdf_query_factory *factory) 
{
  factory->context_length     = sizeof(librdf_query_triples_context);
  
  factory->init               = librdf_query_triples_init;
  factory->terminate          = librdf_query_triples_terminate;
  factory->execute            = librdf_query_triples_query_execute;
  factory->results_as_stream  = librdf_query_triples_results_as_stream;
}


void
librdf_query_triples_constructor(librdf_world *world)
{
  librdf_query_register_factory(world, "triples", NULL,
                                &librdf_query_triples_register_factory);
}
