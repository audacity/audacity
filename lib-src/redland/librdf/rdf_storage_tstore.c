/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_storage_tstore.c - RDF Storage using 3store
 *
 * Copyright (C) 2003-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2003-2004, University of Bristol, UK http://www.bristol.ac.uk/
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
#ifdef HAVE_STDLIB_H
#include <stdlib.h> /* for abort() as used in errors */
#endif
#include <sys/types.h>

#include <redland.h>

#include <rdfsql/rdfsql.h>

typedef struct
{
  /* Tstore args for connecting */
  const char *host;
  const char *db;
  const char *user;
  const char *password;
  const char *model;
  
  RDFSQL* rdfsql;
  
} librdf_storage_tstore_context;


/* prototypes for local functions */
static int librdf_storage_tstore_init(librdf_storage* storage, const char *name, librdf_hash* options);
static int librdf_storage_tstore_open(librdf_storage* storage, librdf_model* model);
static int librdf_storage_tstore_close(librdf_storage* storage);
static int librdf_storage_tstore_size(librdf_storage* storage);
static int librdf_storage_tstore_add_statement(librdf_storage* storage, librdf_statement* statement);
static int librdf_storage_tstore_remove_statement(librdf_storage* storage, librdf_statement* statement);
static int librdf_storage_tstore_contains_statement(librdf_storage* storage, librdf_statement* statement);
static librdf_stream* librdf_storage_tstore_serialise(librdf_storage* storage);
static librdf_stream* librdf_storage_tstore_find_statements(librdf_storage* storage, librdf_statement* statement);

/* serialising implementing functions */
static int librdf_storage_tstore_serialise_end_of_stream(void* context);
static int librdf_storage_tstore_serialise_next_statement(void* context);
static void* librdf_storage_tstore_serialise_get_statement(void* context, int flags);
static void librdf_storage_tstore_serialise_finished(void* context);

/* find implementing functions */
static int librdf_storage_tstore_find_end_of_stream(void* context);
static int librdf_storage_tstore_find_next_statement(void* context);
static void* librdf_storage_tstore_find_get_statement(void* context, int flags);
static void librdf_storage_tstore_find_finished(void* context);


/* context functions */
static int librdf_storage_tstore_context_add_statement(librdf_storage* storage, librdf_node* context_node, librdf_statement* statement);
static int librdf_storage_tstore_context_remove_statement(librdf_storage* storage, librdf_node* context_node, librdf_statement* statement);
static librdf_stream* librdf_storage_tstore_context_serialise(librdf_storage* storage, librdf_node* context_node);

/* context list statement stream methods */
#if 0
static int librdf_storage_tstore_context_serialise_end_of_stream(void* context);
static int librdf_storage_tstore_context_serialise_next_statement(void* context);
static void* librdf_storage_tstore_context_serialise_get_statement(void* context, int flags);
static void librdf_storage_tstore_context_serialise_finished(void* context);
#endif

static librdf_statement* librdf_storage_tstore_statement_from_rs_triple(librdf_world* world, rs_triple *triple);
static rs_triple* librdf_storage_tstore_statement_as_rs_triple(librdf_statement *statement);

static void librdf_storage_tstore_register_factory(librdf_storage_factory *factory);



/* functions implementing storage api */
static int
librdf_storage_tstore_init(librdf_storage* storage, const char *name,
                           librdf_hash* options)
{
  librdf_storage_tstore_context *context=(librdf_storage_tstore_context*)storage->context;
  
  context->host=librdf_hash_get_del(options, "host");
  context->db=librdf_hash_get_del(options, "database");
  context->user=librdf_hash_get_del(options, "user");
  context->password=librdf_hash_get_del(options, "password");
  context->model=librdf_hash_get_del(options, "model");

  /* no more options, might as well free them now */
  if(options)
    librdf_free_hash(options);

  return 0;
}


static void
librdf_storage_tstore_terminate(librdf_storage* storage)
{
  /* nop */  
}



static int
librdf_storage_tstore_open(librdf_storage* storage, librdf_model* model)
{
  librdf_storage_tstore_context *context=(librdf_storage_tstore_context*)storage->context;

  if(context->host)
    context->rdfsql=rs_connect_remote(context->host,
                                      context->db, context->user, 
                                      context->password, context->model);
  else
    context->rdfsql=rs_connect(context->db, context->user, 
                               context->password, context->model);
  if(!context->rdfsql)
    return 1;

  return 0;
}


/**
 * librdf_storage_tstore_close:
 * @storage: the storage
 *
 * .
 * 
 * Close the storage list storage, and free all content since there is no 
 * persistance.
 * 
 * Return value: non 0 on failure
 **/
static int
librdf_storage_tstore_close(librdf_storage* storage)
{
  /* librdf_storage_tstore_context* context=(librdf_storage_tstore_context*)storage->context; */

  return 0;
}


static int
librdf_storage_tstore_size(librdf_storage* storage)
{
  return -1;
}


static int
librdf_storage_tstore_add_statement(librdf_storage* storage, librdf_statement* statement)
{
  /* FIXME - cannot check for adding duplicate statements */

  return librdf_storage_tstore_context_add_statement(storage, NULL, statement);
}



static int
librdf_storage_tstore_remove_statement(librdf_storage* storage, librdf_statement* statement)
{
  return librdf_storage_tstore_context_remove_statement(storage, NULL, statement);
}


static int
librdf_storage_tstore_contains_statement(librdf_storage* storage, librdf_statement* statement)
{
  /*librdf_storage_tstore_context* context=(librdf_storage_tstore_context*)storage->context; */
  /* FIXME */
  return 0;
}


static librdf_statement*
librdf_storage_tstore_statement_from_rs_triple(librdf_world* world,
                                               rs_triple *triple)
{
  librdf_node *subject_node;
  librdf_node *predicate_node;
  librdf_node *object_node;

  if(triple->subject) {
    if(!strncmp(triple->subject, "_:",2))
      subject_node=librdf_new_node_from_blank_identifier(world, 
                                                         (const unsigned char *)triple->subject+2);
    else
      subject_node=librdf_new_node_from_uri_string(world, 
                                                   (const unsigned char *)triple->subject);

    if(!subject_node)
      return NULL;

  } else
    subject_node=NULL;
  
  if(triple->predicate) {
    predicate_node=librdf_new_node_from_uri_string(world,
                                                   (const unsigned char *)triple->predicate);

    if(!predicate_node) {
      librdf_free_node(subject_node);
      return NULL;
    }
  } else
    predicate_node=NULL;
  
  if(triple->object) {
    if(triple->literal)
      object_node=librdf_new_node_from_typed_literal(world, 
                                                     (const unsigned char *)triple->object,
                                                     NULL, NULL);
    else if(!strncmp(triple->object, ":", 2))
      object_node=librdf_new_node_from_blank_identifier(world, 
                                                        (const unsigned char *)triple->object+2);
    else
      object_node=librdf_new_node_from_uri_string(world, 
                                                  (const unsigned char *)triple->object);

    if(!object_node) {
      librdf_free_node(subject_node);
      librdf_free_node(predicate_node);
      return NULL;
    }
  } else
    object_node=NULL;  
  
  return librdf_new_statement_from_nodes(world, subject_node, predicate_node, object_node);
}


/* FIXME returns an alloced triple pointing to shared strings */
static rs_triple*
librdf_storage_tstore_statement_as_rs_triple(librdf_statement *statement)
{
  librdf_node *subject_node=statement->subject;
  librdf_node *predicate_node=statement->predicate;
  librdf_node *object_node=statement->object;
  rs_triple* triple=LIBRDF_MALLOC(rs_triple, sizeof(rs_triple));

  if(subject_node) {
    if(librdf_node_is_blank(subject_node))
      triple->subject=(char*)librdf_node_get_blank_identifier(subject_node);
    else
      triple->subject=(char*)librdf_uri_as_string(librdf_node_get_uri(subject_node));
  } else
    triple->subject=NULL;

  if(predicate_node)
    triple->predicate=(char*)librdf_uri_as_string(librdf_node_get_uri(predicate_node));
  else
    triple->predicate=NULL;
  
  /* Assumptions - FIXME */
  triple->literal = 0;
  if(object_node) {
    if(librdf_node_is_literal(object_node)) {
      triple->object=(char*)librdf_node_get_literal_value(object_node);
      triple->literal = 1;
    } else if(librdf_node_is_blank(object_node)) {
      triple->object=(char*)librdf_node_get_blank_identifier(object_node);
    } else {
      triple->object=(char*)librdf_uri_as_string(librdf_node_get_uri(object_node));
    }
  } else
    triple->object=NULL;
  
  return triple;
}


typedef struct {
  librdf_storage* storage;
  rs_result *result;
  rs_triple *triple;
} librdf_storage_tstore_serialise_stream_context;


static librdf_stream*
librdf_storage_tstore_serialise(librdf_storage* storage)
{
  librdf_storage_tstore_context* context=(librdf_storage_tstore_context*)storage->context;
  librdf_storage_tstore_serialise_stream_context* scontext;
  librdf_stream* stream;
  
  scontext=(librdf_storage_tstore_serialise_stream_context*)LIBRDF_CALLOC(librdf_storage_tstore_serialise_stream_context, 1, sizeof(librdf_storage_tstore_serialise_stream_context));
  if(!scontext)
    return NULL;
  
  scontext->storage=storage;
  librdf_storage_add_reference(scontext->storage);

  scontext->result=rs_find_all_resources(context->rdfsql, 0, context->model);
  if(!scontext->result)
    /* empty */
    scontext->triple=NULL;
  else
    scontext->triple=rs_next_triple(scontext->result);

  stream=librdf_new_stream(storage->world,
                           (void*)scontext,
                           &librdf_storage_tstore_serialise_end_of_stream,
                           &librdf_storage_tstore_serialise_next_statement,
                           &librdf_storage_tstore_serialise_get_statement,
                           &librdf_storage_tstore_serialise_finished);
  if(!stream) {
    librdf_storage_tstore_serialise_finished((void*)scontext);
    return NULL;
  }
  
  return stream;  
}


static int
librdf_storage_tstore_serialise_end_of_stream(void* context)
{
  librdf_storage_tstore_serialise_stream_context* scontext=(librdf_storage_tstore_serialise_stream_context*)context;

  return scontext->triple == NULL;

}

static int
librdf_storage_tstore_serialise_next_statement(void* context)
{
  librdf_storage_tstore_serialise_stream_context* scontext=(librdf_storage_tstore_serialise_stream_context*)context;

  if(!scontext->triple)
    return 1;

  scontext->triple=rs_next_triple(scontext->result);

  return scontext->triple == NULL;
}


static void*
librdf_storage_tstore_serialise_get_statement(void* context, int flags)
{
  librdf_storage_tstore_serialise_stream_context* scontext=(librdf_storage_tstore_serialise_stream_context*)context;

  switch(flags) {
    case LIBRDF_ITERATOR_GET_METHOD_GET_OBJECT:
      {
        librdf_statement* statement=librdf_storage_tstore_statement_from_rs_triple(scontext->storage->world, scontext->triple);
        return statement;
      }
    case LIBRDF_ITERATOR_GET_METHOD_GET_CONTEXT:
      return NULL;
    default:
      librdf_log(scontext->storage->world,
                 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
                 "Unknown iterator method flag %d", flags);
      return NULL;
  }
}


static void
librdf_storage_tstore_serialise_finished(void* context)
{
  librdf_storage_tstore_serialise_stream_context* scontext=(librdf_storage_tstore_serialise_stream_context*)context;

  if(scontext->triple) {
    /* The docs say about rs_find_triples:[[
     *   NB Once rs_find_triples has been called, all the triples
     *   /must/ be fetched with rs_next_triple(), even if they are
     *   not required.
     * ]]
     * but let's assume it applies to rs_find_all_resources
     */
    while(rs_next_triple(scontext->result))
      ;
  }

  if(scontext->result)
    rs_free_result(scontext->result);

  if(scontext->storage)
    librdf_storage_remove_reference(scontext->storage);

  LIBRDF_FREE(librdf_storage_tstore_serialise_stream_context, scontext);
}


typedef struct {
  librdf_storage* storage;
  rs_result *result;
  rs_triple *triple;
  rs_triple *search_triple;
} librdf_storage_tstore_find_stream_context;


/**
 * librdf_storage_tstore_find_statements:
 * @storage: the storage
 * @statement: the statement to match
 *
 * .
 * 
 * Return a stream of statements matching the given statement (or
 * all statements if NULL).  Parts (subject, predicate, object) of the
 * statement can be empty in which case any statement part will match that.
 * Uses #librdf_statement_match to do the matching.
 * 
 * Return value: a #librdf_stream or NULL on failure
 **/
static librdf_stream*
librdf_storage_tstore_find_statements(librdf_storage* storage, librdf_statement* statement)
{
  librdf_storage_tstore_context* context=(librdf_storage_tstore_context*)storage->context;
  librdf_storage_tstore_find_stream_context* scontext;
  librdf_stream* stream;
  rs_triple* triple;
  rs_obj_type type;

  statement=librdf_new_statement_from_statement(statement);
  if(!statement)
    return NULL;

  scontext=(librdf_storage_tstore_find_stream_context*)LIBRDF_CALLOC(librdf_storage_tstore_find_stream_context, 1, sizeof(librdf_storage_tstore_find_stream_context));
  if(!scontext)
    return NULL;
  
  scontext->storage=storage;
  librdf_storage_add_reference(scontext->storage);

  triple=librdf_storage_tstore_statement_as_rs_triple(statement);
  scontext->search_triple=triple;

  if(triple->object)
    type=(triple->literal ? ObjLiteral: ObjURI);
  else
    type=ObjAny;

  scontext->result=rs_find_triples(context->rdfsql,
                                   triple->subject, 
                                   triple->predicate,
                                   triple->object,
                                   type,
                                   0, 
                                   context->model);
  if(!scontext->result)
    /* empty */
    scontext->triple=NULL;
  else
    scontext->triple=rs_next_triple(scontext->result);

  stream=librdf_new_stream(storage->world,
                           (void*)scontext,
                           &librdf_storage_tstore_find_end_of_stream,
                           &librdf_storage_tstore_find_next_statement,
                           &librdf_storage_tstore_find_get_statement,
                           &librdf_storage_tstore_find_finished);
  if(!stream) {
    librdf_storage_tstore_find_finished((void*)scontext);
    return NULL;
  }
  
  return stream;  

}


static int
librdf_storage_tstore_find_end_of_stream(void* context)
{
  librdf_storage_tstore_find_stream_context* scontext=(librdf_storage_tstore_find_stream_context*)context;

  return scontext->triple == NULL;

}

static int
librdf_storage_tstore_find_next_statement(void* context)
{
  librdf_storage_tstore_find_stream_context* scontext=(librdf_storage_tstore_find_stream_context*)context;

  if(!scontext->triple)
    return 1;

  scontext->triple=rs_next_triple(scontext->result);

  return scontext->triple == NULL;
}


static void*
librdf_storage_tstore_find_get_statement(void* context, int flags)
{
  librdf_storage_tstore_find_stream_context* scontext=(librdf_storage_tstore_find_stream_context*)context;

  switch(flags) {
    case LIBRDF_ITERATOR_GET_METHOD_GET_OBJECT:
      {
        librdf_statement* statement=librdf_storage_tstore_statement_from_rs_triple(scontext->storage->world, scontext->triple);
        return statement;
      }
    case LIBRDF_ITERATOR_GET_METHOD_GET_CONTEXT:
      return NULL;
    default:
      librdf_log(scontext->storage->world,
                 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
                 "Unknown iterator method flag %d", flags);
      return NULL;
  }
}


static void
librdf_storage_tstore_find_finished(void* context)
{
  librdf_storage_tstore_find_stream_context* scontext=(librdf_storage_tstore_find_stream_context*)context;

  if(scontext->triple) {
    /* The docs say about rs_find_triples:[[
     *   NB Once rs_find_triples has been called, all the triples
     *   /must/ be fetched with rs_next_triple(), even if they are
     *   not required.
     * ]]
     */
    while(rs_next_triple(scontext->result))
      ;
  }

  if(scontext->result)
    rs_free_result(scontext->result);


  /* FIXME: as alloced in librdf_storage_tstore_statement_as_rs_triple */
  if(scontext->search_triple)
    LIBRDF_FREE(rs_triple, scontext->search_triple);

  if(scontext->storage)
    librdf_storage_remove_reference(scontext->storage);

  LIBRDF_FREE(librdf_storage_tstore_find_stream_context, scontext);
}

/**
 * librdf_storage_tstore_context_add_statement:
 * @storage: #librdf_storage object
 * @context_node: #librdf_node object
 * @statement: #librdf_statement statement to add
 *
 * Add a statement to a storage context.
 * 
 * Return value: non 0 on failure
 **/
static int
librdf_storage_tstore_context_add_statement(librdf_storage* storage,
                                            librdf_node* context_node,
                                            librdf_statement* statement) 
{
  librdf_storage_tstore_context* context=(librdf_storage_tstore_context*)storage->context;
  librdf_node *subject_node=statement->subject;
  librdf_node *predicate_node=statement->predicate;
  librdf_node *object_node=statement->object;
  char *subject;
  char *predicate;
  char *object;
  rs_obj_type type;

  
  if(librdf_node_is_blank(subject_node)) {
    subject=(char*)librdf_node_get_blank_identifier(subject_node);
  } else
    subject=(char*)librdf_uri_as_string(librdf_node_get_uri(subject_node));
  

  predicate=(char*)librdf_uri_as_string(librdf_node_get_uri(predicate_node));
  
  /* Assumptions - FIXME */
  if(librdf_node_is_literal(object_node)) {
    object=(char*)librdf_node_get_literal_value(object_node);
    type = ObjLiteral;
  } else if(librdf_node_is_blank(object_node)) {
    object=(char*)librdf_node_get_blank_identifier(object_node);
    type = ObjURI;
  } else {
    object=(char*)librdf_uri_as_string(librdf_node_get_uri(object_node));
    type = ObjURI;
  }
  

  if(rs_assert_triple(context->rdfsql, subject, predicate, object, type))
    return 1;

  return 0;
}


/**
 * librdf_storage_tstore_context_remove_statement:
 * @storage: #librdf_storage object
 * @context_node: #librdf_node object
 * @statement: #librdf_statement statement to remove
 *
 * Remove a statement from a storage context.
 * 
 * Return value: non 0 on failure
 **/
static int
librdf_storage_tstore_context_remove_statement(librdf_storage* storage, 
                                             librdf_node* context_node,
                                             librdf_statement* statement) 
{
  /* librdf_storage_tstore_context* context=(librdf_storage_tstore_context*)storage->context; */

  /* FIXME */

  return 0;
}


typedef struct {
  librdf_iterator* iterator;
  librdf_hash_datum *key;
  librdf_hash_datum *value;
  librdf_statement current; /* static, shared statement */
} librdf_storage_tstore_context_serialise_stream_context;


/**
 * librdf_storage_tstore_context_serialise:
 * @storage: #librdf_storage object
 * @context_node: #librdf_node object
 *
 * List all statements in a storage context.
 * 
 * Return value: #librdf_stream of statements or NULL on failure or context is empty
 **/
static librdf_stream*
librdf_storage_tstore_context_serialise(librdf_storage* storage,
                                      librdf_node* context_node) 
{
  return NULL;
}


#if 0
static int
librdf_storage_tstore_context_serialise_end_of_stream(void* context)
{
  /* librdf_storage_tstore_context_serialise_stream_context* scontext=(librdf_storage_tstore_context_serialise_stream_context*)context; */

  return 1;
}


static int
librdf_storage_tstore_context_serialise_next_statement(void* context)
{
  /* librdf_storage_tstore_context_serialise_stream_context* scontext=(librdf_storage_tstore_context_serialise_stream_context*)context; */

  return 1;
}


static void*
librdf_storage_tstore_context_serialise_get_statement(void* context, int flags)
{
  /* librdf_storage_tstore_context_serialise_stream_context* scontext=(librdf_storage_tstore_context_serialise_stream_context*)context; */
  
  return NULL;
}


static void
librdf_storage_tstore_context_serialise_finished(void* context)
{
  librdf_storage_tstore_context_serialise_stream_context* scontext=(librdf_storage_tstore_context_serialise_stream_context*)context;
  
  LIBRDF_FREE(librdf_storage_tstore_context_serialise_stream_context, scontext);
}
#endif


/* local function to register list storage functions */

static void
librdf_storage_tstore_register_factory(librdf_storage_factory *factory) 
{
  factory->context_length     = sizeof(librdf_storage_tstore_context);
  
  factory->init               = librdf_storage_tstore_init;
  factory->terminate          = librdf_storage_tstore_terminate;
  factory->open               = librdf_storage_tstore_open;
  factory->close              = librdf_storage_tstore_close;
  factory->size               = librdf_storage_tstore_size;
  factory->add_statement      = librdf_storage_tstore_add_statement;
  factory->remove_statement   = librdf_storage_tstore_remove_statement;
  factory->contains_statement = librdf_storage_tstore_contains_statement;
  factory->serialise          = librdf_storage_tstore_serialise;
  factory->find_statements    = librdf_storage_tstore_find_statements;
  factory->context_add_statement    = librdf_storage_tstore_context_add_statement;
  factory->context_remove_statement = librdf_storage_tstore_context_remove_statement;
  factory->context_serialise        = librdf_storage_tstore_context_serialise;
}


/**
 * librdf_init_storage_tstore:
 * @world: world object
 * 
 * INTERNAL - initialise the storage_tstore module.
 **/
void
librdf_init_storage_tstore(librdf_world *world)
{
  librdf_storage_register_factory(world, "tstore", "AKT triplestore",
                                  &librdf_storage_tstore_register_factory);
}
