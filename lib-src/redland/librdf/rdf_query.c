/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_query.c - RDF Query Language/Syntax and Execution Interface
 *
 * Copyright (C) 2002-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2002-2005, University of Bristol, UK http://www.bristol.ac.uk/
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
#include <ctype.h>
#include <sys/types.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h> /* for abort() as used in errors */
#endif

#include <redland.h>
#include <rdf_query.h>


#ifndef STANDALONE

/* prototypes for helper functions */
static void librdf_delete_query_factories(librdf_world *world);


/**
 * librdf_init_query:
 * @world: redland world object
 *
 * INTERNAL - Initialise the query module.
 * 
 * Initialises and registers all
 * compiled query modules.  Must be called before using any of the query
 * factory functions such as librdf_get_query_factory()
 **/
void
librdf_init_query(librdf_world *world) 
{
  /* Always have query triple, rasqal implementations available */
  librdf_query_triples_constructor(world);
  librdf_query_rasqal_constructor(world);
}


/**
 * librdf_finish_query:
 * @world: redland world object
 *
 * INTERNAL - Terminate the query module.
 *
 **/
void
librdf_finish_query(librdf_world *world) 
{
  librdf_query_rasqal_destructor(world);
  librdf_delete_query_factories(world);
}



/* helper functions */

/*
 * librdf_free_query_factory - delete a query factory
 */
static void
librdf_free_query_factory(librdf_query_factory *factory)
{
  if(factory) {
    if(factory->name)
      LIBRDF_FREE(librdf_query_factory, factory->name);
    if(factory->uri)
      librdf_free_uri(factory->uri);
    LIBRDF_FREE(librdf_query_factory, factory);
  }
}


/*
 * librdf_delete_query_factories - helper function to delete all the registered query factories
 */
static void
librdf_delete_query_factories(librdf_world *world)
{
  librdf_query_factory *factory, *next;
  
  for(factory=world->query_factories; factory; factory=next) {
    next=factory->next;
    librdf_free_query_factory(factory);
  }
  world->query_factories=NULL;
}


/* class methods */

/**
 * librdf_query_register_factory:
 * @world: redland world object
 * @name: the query language name
 * @uri_string: the query language URI string (or NULL if none)
 * @factory: pointer to function to call to register the factory
 *
 * Register a query factory.
 * 
 **/
void
librdf_query_register_factory(librdf_world *world, const char *name,
                              const unsigned char *uri_string,
                              void (*factory) (librdf_query_factory*)) 
{
  librdf_query_factory *query;

  librdf_world_open(world);

#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
  LIBRDF_DEBUG2("Received registration for query name %s\n", name);
#endif

  for(query = world->query_factories; query; query = query->next ) {
    if(!strcmp(query->name, name)) {
      librdf_log(world,
                 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_QUERY, NULL,
                 "query language %s already registered", query->name);
      return;
    }
  }

  query=(librdf_query_factory*)LIBRDF_CALLOC(librdf_query_factory, 1,
                                             sizeof(librdf_query_factory));
  if(!query)
    goto oom;

  query->name=(char*)LIBRDF_MALLOC(cstring, strlen(name)+1);
  if(!query->name)
    goto oom_tidy;
  strcpy(query->name, name);

  if(uri_string) {
    query->uri=librdf_new_uri(world, uri_string);
    if(!query->uri)
      goto oom_tidy;
  }

  query->next = world->query_factories;
  world->query_factories = query;

  /* Call the query registration function on the new object */
  (*factory)(query);

#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
  LIBRDF_DEBUG3("%s has context size %d\n", name, query->context_length);
#endif

  return;

  oom_tidy:
  librdf_free_query_factory(query);
  oom:
  LIBRDF_FATAL1(world, LIBRDF_FROM_QUERY, "Out of memory");
}


/**
 * librdf_get_query_factory:
 * @world: redland world object
 * @name: the factory name or NULL for the default factory
 * @uri: the factory URI or NULL for the default factory
 *
 * Get a query factory by name.
 * 
 * Return value: the factory object or NULL if there is no such factory
 **/
librdf_query_factory*
librdf_get_query_factory(librdf_world *world, 
                         const char *name, librdf_uri *uri) 
{
  librdf_query_factory *factory;

  librdf_world_open(world);

  /* return 1st query if no particular one wanted - why? */
  if(!name && !uri) {
    factory=world->query_factories;
    if(!factory) {
      LIBRDF_DEBUG1("No (default) query factories registered\n");
      return NULL;
    }
  } else {
    for(factory=world->query_factories; factory; factory=factory->next) {
      if(name && !strcmp(factory->name, name)) {
        break;
      }
      if(uri && factory->uri && librdf_uri_equals(factory->uri, uri)) {
        break;
      }
    }
    /* else FACTORY name not found */
    if(!factory) {
      LIBRDF_DEBUG3("No query language with name '%s' uri %s found\n", 
                    name, (uri ? (char*)librdf_uri_as_string(uri) : "NULL"));
      return NULL;
    }
  }
        
  return factory;
}



/**
 * librdf_new_query:
 * @world: redland world object
 * @name: the name identifying the query language
 * @uri: the URI identifying the query language (or NULL)
 * @query_string: the query string
 * @base_uri: the base URI of the query string (or NULL)
 *
 * Constructor - create a new #librdf_query object.
 *
 * Return value: a new #librdf_query object or NULL on failure
 */
librdf_query*
librdf_new_query(librdf_world *world,
                 const char *name, librdf_uri *uri,
                 const unsigned char *query_string,
                 librdf_uri* base_uri)
{
  librdf_query_factory* factory;

  librdf_world_open(world);

  factory=librdf_get_query_factory(world, name, uri);
  if(!factory)
    return NULL;

  return librdf_new_query_from_factory(world, factory, name, uri, 
                                       query_string, base_uri);
}


/**
 * librdf_new_query_from_query - Copy constructor - create a new librdf_query object from an existing one
 * @old_query: the existing query #librdf_query to use
 *
 * Should create a new query in the same context as the existing one
 * as appropriate.
 *
 * Return value: a new #librdf_query object or NULL on failure
 */
librdf_query*
librdf_new_query_from_query(librdf_query* old_query) 
{
  librdf_query* new_query;

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(old_query, librdf_query, NULL);

  /* FIXME: fail if clone is not supported by this query (factory) */
  if(!old_query->factory->clone) {
    LIBRDF_FATAL1(old_query->world, LIBRDF_FROM_QUERY, "clone not implemented for query factory");
    return NULL;
  }

  new_query=(librdf_query*)LIBRDF_CALLOC(librdf_query, 1, 
                                         sizeof(librdf_query));
  if(!new_query)
    return NULL;
  
  new_query->usage=1;

  new_query->context=(char*)LIBRDF_CALLOC(librdf_query_context, 1,
                                          old_query->factory->context_length);
  if(!new_query->context) {
    librdf_free_query(new_query);
    return NULL;
  }

  new_query->world=old_query->world;

  /* do this now so librdf_free_query won't call new factory on
   * partially copied query
   */
  new_query->factory=old_query->factory;

  /* clone is assumed to do leave the new query in the same state
   * after an init() method on an existing query - i.e ready to use
   */
  if(old_query->factory->clone(new_query, old_query)) {
    librdf_free_query(new_query);
    return NULL;
  }

  return new_query;
}


/**
 * librdf_new_query_from_factory:
 * @world: redland world object
 * @factory: the factory to use to construct the query
 * @name: query language name
 * @uri: query language URI (or NULL)
 * @query_string: the query string
 * @base_uri: base URI of the query string (or NULL)
 *
 * Constructor - create a new #librdf_query object.
 *
 * Return value: a new #librdf_query object or NULL on failure
 */
librdf_query*
librdf_new_query_from_factory(librdf_world *world,
                              librdf_query_factory* factory,
                              const char *name, librdf_uri *uri,
                              const unsigned char *query_string,
                              librdf_uri *base_uri)
{
  librdf_query* query;

  librdf_world_open(world);

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(factory, librdf_query_factory, NULL);

  if(!factory) {
    LIBRDF_DEBUG1("No query factory given\n");
    return NULL;
  }
  
  query=(librdf_query*)LIBRDF_CALLOC(librdf_query, 1, sizeof(librdf_query));
  if(!query)
    return NULL;

  query->world=world;

  query->usage=1;

  query->context=(char*)LIBRDF_CALLOC(librdf_query_context, 1,
                                      factory->context_length);
  if(!query->context) {
    librdf_free_query(query);
    return NULL;
  }
  
  query->factory=factory;
  
  if(factory->init(query, name, uri, query_string, base_uri)) {
    librdf_free_query(query);
    return NULL;
  }
  
  return query;
}


/**
 * librdf_free_query:
 * @query: #librdf_query object
 * 
 * Destructor - destroy a #librdf_query object.
 **/
void
librdf_free_query(librdf_query* query) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN(query, librdf_query);

  if(--query->usage)
    return;
  
  if(query->factory)
    query->factory->terminate(query);

  if(query->context)
    LIBRDF_FREE(librdf_query_context, query->context);

  LIBRDF_FREE(librdf_query, query);
}


/* methods */


void
librdf_query_add_query_result(librdf_query *query,
                              librdf_query_results* query_results)
{
  query_results->next=query->results;
  query->results=query_results;
  /* add reference to ensure query lives as long as this runs */
  query->usage++;
}


void
librdf_query_remove_query_result(librdf_query *query,
                                 librdf_query_results* query_results)
{
  librdf_query_results *cur, *prev=NULL;
  for(cur=query->results; cur && cur != query_results; cur=cur->next)
    prev=cur;
  
  if(cur == query_results) {
    if(prev)
      prev->next=cur->next;
  }
  if(cur == query->results && cur != NULL)
    query->results=cur->next;

  /* remove reference and free if we are the last */
  librdf_free_query(query);
}


/**
 * librdf_query_execute:
 * @query: #librdf_query object
 * @model: model to operate query on
 *
 * Run the query on a model.
 * 
 * Runs the query against the (previously registered) model
 * and returns a #librdf_query_results for the result objects.
 * 
 * Return value:  #librdf_query_results or NULL on failure
 **/
librdf_query_results*
librdf_query_execute(librdf_query* query, librdf_model* model)
{
  librdf_query_results* results=NULL;
  
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(query, librdf_query, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, NULL);

  if(query->factory->execute) {
    if((results=query->factory->execute(query, model)))
      librdf_query_add_query_result(query, results);
  }
  
  return results;
}


/**
 * librdf_query_get_limit:
 * @query: #librdf_query query object
 *
 * Get the query-specified limit on results.
 *
 * This is the limit given in the query on the number of results allowed.
 *
 * Return value: integer >=0 if a limit is given, otherwise <0
 **/
int
librdf_query_get_limit(librdf_query *query)
{
  if(query->factory->get_limit)
    return query->factory->get_limit(query);
  
  return -1;
}


/**
 * librdf_query_set_limit:
 * @query: #librdf_query query object
 * @limit: the limit on results, >=0 to set a limit, <0 to have no limit
 *
 * Set the query-specified limit on results.
 *
 * This is the limit given in the query on the number of results allowed.
 *
 * Return value: non-0 on failure
 **/
int
librdf_query_set_limit(librdf_query *query, int limit)
{
  if(query->factory->set_limit) {
    query->factory->set_limit(query, limit);
    return 0;
  }

  return -1;
}


/**
 * librdf_query_get_offset:
 * @query: #librdf_query query object
 *
 * Get the query-specified offset on results.
 *
 * This is the offset given in the query on the number of results allowed.
 *
 * Return value: integer >=0 if a offset is given, otherwise <0
 **/
int
librdf_query_get_offset(librdf_query *query)
{
  if(query->factory->get_offset)
    return query->factory->get_offset(query);
  
  return -1;
}


/**
 * librdf_query_set_offset:
 * @query: #librdf_query query object
 * @offset: offset for results, >=0 to set an offset, <0 to have no offset
 *
 * Set the query-specified offset on results.
 *
 * This is the offset given in the query on the number of results allowed.
 *
 * Return value: non-0 on failure
 **/
int
librdf_query_set_offset(librdf_query *query, int offset)
{
  if(query->factory->set_offset) {
    query->factory->set_offset(query, offset);
    return 0;
  }

  return -1;
}

#endif



/* TEST CODE */


#ifdef STANDALONE

/* one more prototype */
int main(int argc, char *argv[]);


#define DATA "@prefix ex: <http://example.org/> .\
ex:fido a ex:Dog ;\
        ex:label \"Fido\" .\
"
#define DATA_LANGUAGE "turtle"
#define DATA_BASE_URI "http://example.org/"
#define QUERY_STRING "select ?x where (?x rdf:type ?y)";
#define QUERY_LANGUAGE "rdql"
#define VARIABLES_COUNT 1

int
main(int argc, char *argv[]) 
{
  librdf_query* query;
  librdf_query_results* results;
  librdf_model* model;
  librdf_storage* storage;
  librdf_parser* parser;
  librdf_uri *uri;
  const char *program=librdf_basename((const char*)argv[0]);
  librdf_world *world;
  librdf_uri* format_uri;
  size_t string_length;
  unsigned char *string;
  const char *query_string=QUERY_STRING;

  world=librdf_new_world();
  librdf_world_open(world);

  /* create model and storage */
  if(1) {
    /* test in memory */
    storage=librdf_new_storage(world, NULL, NULL, NULL);
  } else {
    /* test on disk */
    storage=librdf_new_storage(world, "hashes", "test", "hash-type='bdb',dir='.',write='yes',new='yes',contexts='yes'");
  }
  if(!storage) {
    fprintf(stderr, "%s: Failed to create new storage\n", program);
    return(1);
  }
  fprintf(stderr, "%s: Creating model\n", program);
  model=librdf_new_model(world, storage, NULL);
  if(!model) {
    fprintf(stderr, "%s: Failed to create new model\n", program);
    return(1);
  }

  /* read the example data in */
  uri=librdf_new_uri(world, (const unsigned char*)DATA_BASE_URI);
  parser=librdf_new_parser(world, DATA_LANGUAGE, NULL, NULL);
  librdf_parser_parse_string_into_model(parser, (const unsigned char*)DATA,
                                        uri, model);
  librdf_free_parser(parser);
  librdf_free_uri(uri);


  fprintf(stdout, "%s: Creating query\n", program);
  query=librdf_new_query(world, QUERY_LANGUAGE,
                         NULL, (const unsigned char*)query_string, NULL);
  if(!query) {
    fprintf(stderr, "%s: Failed to create new query\n", program);
    return(1);
  }

  /* do the query */
  if(!(results=librdf_model_query_execute(model, query))) {
    fprintf(stderr, "%s: Query of model with '%s' failed\n", 
            program, query_string);
    return 1;
  }

  /* print the results */
  while(!librdf_query_results_finished(results)) {
    const char **names=NULL;
    librdf_node* values[VARIABLES_COUNT];
    
    if(librdf_query_results_get_bindings(results, &names, values))
      break;
    
    fputs("result: [", stdout);
    if(names) {
      int i;
      
      for(i=0; names[i]; i++) {
        fprintf(stdout, "%s=", names[i]);
        if(values[i]) {
          librdf_node_print(values[i], stdout);
          librdf_free_node(values[i]);
        } else
          fputs("NULL", stdout);
        if(names[i+1])
          fputs(", ", stdout);
      }
    }
    fputs("]\n", stdout);
    
    librdf_query_results_next(results);
  }
  
  fprintf(stdout, "%s: Query returned %d results\n", program, 
          librdf_query_results_get_count(results));

  librdf_free_query_results(results);


  fprintf(stdout, "%s: Executing a second time\n", program);
  if(!(results=librdf_model_query_execute(model, query))) {
    fprintf(stderr, "%s: Second query of model with '%s' failed\n", 
            program, query_string);
    return 1;
  }

  format_uri=librdf_new_uri(world, (unsigned const char*)"http://www.w3.org/TR/2004/WD-rdf-sparql-XMLres-20041221/");
  string_length=0;

  string=librdf_query_results_to_counted_string(results, 
                                                format_uri, NULL,
                                                &string_length);
  fprintf(stdout, "%s: Got query results string of length %d\n", program,
          (int)string_length);

  librdf_free_uri(format_uri);
  free(string);
  
  librdf_free_query_results(results);


  fprintf(stdout, "%s: Freeing query\n", program);
  librdf_free_query(query);

  librdf_free_model(model);
  librdf_free_storage(storage);

  librdf_free_world(world);
  
  /* keep gcc -Wall happy */
  return(0);
}

#endif
