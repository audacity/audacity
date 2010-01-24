/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_storage_list.c - RDF Storage in memory as a list implementation
 *
 * Copyright (C) 2000-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2000-2004, University of Bristol, UK http://www.bristol.ac.uk/
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


typedef struct
{
  librdf_list* list;

  /* If this is non-0, contexts are being used */
  int index_contexts;
  librdf_hash* contexts;
  
} librdf_storage_list_context;


/* These are stored in the list */
typedef struct
{
  librdf_statement *statement;
  librdf_node *context;
} librdf_storage_list_node;


/* prototypes for local functions */
static int librdf_storage_list_init(librdf_storage* storage, const char *name, librdf_hash* options);
static int librdf_storage_list_open(librdf_storage* storage, librdf_model* model);
static int librdf_storage_list_close(librdf_storage* storage);
static int librdf_storage_list_size(librdf_storage* storage);
static int librdf_storage_list_add_statement(librdf_storage* storage, librdf_statement* statement);
static int librdf_storage_list_add_statements(librdf_storage* storage, librdf_stream* statement_stream);
static int librdf_storage_list_remove_statement(librdf_storage* storage, librdf_statement* statement);
static int librdf_storage_list_contains_statement(librdf_storage* storage, librdf_statement* statement);
static librdf_stream* librdf_storage_list_serialise(librdf_storage* storage);
static librdf_stream* librdf_storage_list_find_statements(librdf_storage* storage, librdf_statement* statement);

/* serialising implementing functions */
static int librdf_storage_list_serialise_end_of_stream(void* context);
static int librdf_storage_list_serialise_next_statement(void* context);
static void* librdf_storage_list_serialise_get_statement(void* context, int flags);
static void librdf_storage_list_serialise_finished(void* context);

/* context functions */
static int librdf_storage_list_context_add_statement(librdf_storage* storage, librdf_node* context_node, librdf_statement* statement);
static int librdf_storage_list_context_remove_statement(librdf_storage* storage, librdf_node* context_node, librdf_statement* statement);
static librdf_stream* librdf_storage_list_context_serialise(librdf_storage* storage, librdf_node* context_node);

/* context list statement stream methods */
static int librdf_storage_list_context_serialise_end_of_stream(void* context);
static int librdf_storage_list_context_serialise_next_statement(void* context);
static void* librdf_storage_list_context_serialise_get_statement(void* context, int flags);
static void librdf_storage_list_context_serialise_finished(void* context);

/* helper functions for contexts */
static int librdf_storage_list_node_equals(librdf_storage_list_node *first, librdf_storage_list_node *second);

static librdf_iterator* librdf_storage_list_get_contexts(librdf_storage* storage);

/* get_context iterator functions */
static int librdf_storage_list_get_contexts_is_end(void* iterator);
static int librdf_storage_list_get_contexts_next_method(void* iterator);
static void* librdf_storage_list_get_contexts_get_method(void* iterator, int);
static void librdf_storage_list_get_contexts_finished(void* iterator);


static void librdf_storage_list_register_factory(librdf_storage_factory *factory);



/* functions implementing storage api */
static int
librdf_storage_list_init(librdf_storage* storage, const char *name,
                         librdf_hash* options)
{
  librdf_storage_list_context *context=(librdf_storage_list_context*)storage->context;
  int index_contexts=0;
  
  if((index_contexts=librdf_hash_get_as_boolean(options, "contexts"))<0)
    index_contexts=0; /* default is no contexts */

  context->index_contexts=index_contexts;
  
  /* no more options, might as well free them now */
  if(options)
    librdf_free_hash(options);

  return 0;
}


static void
librdf_storage_list_terminate(librdf_storage* storage)
{
  /* nop */  
}


/* Helper for comparing list nodes when using contexts */
static int
librdf_storage_list_node_equals(librdf_storage_list_node *first, 
                                librdf_storage_list_node *second)
{
  if(!librdf_statement_equals(first->statement, second->statement))
    return 0;

  if(!first->context && !second->context)
    return 1;
  
  if(!first->context || !second->context)
    return 0;
  
  if(!librdf_node_equals(first->context, second->context))
    return 0;

  return 1;
}


static int
librdf_storage_list_open(librdf_storage* storage, librdf_model* model)
{
  librdf_storage_list_context *context=(librdf_storage_list_context*)storage->context;

  context->list=librdf_new_list(storage->world);
  if(!context->list)
    return 1;

  if(context->index_contexts) {
    /* create a new memory hash */
    context->contexts=librdf_new_hash(storage->world, NULL);
    if(librdf_hash_open(context->contexts, NULL, 0, 1, 1, NULL)) {
      librdf_free_list(context->list);
      context->list=NULL;
      return 1;
    }
  }

  librdf_list_set_equals(context->list, 
                         (int (*)(void*, void*))&librdf_storage_list_node_equals);

  return 0;
}


/**
 * librdf_storage_list_close:
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
librdf_storage_list_close(librdf_storage* storage)
{
  librdf_storage_list_context* context=(librdf_storage_list_context*)storage->context;
  
  if(context->list) {
    librdf_storage_list_node* sln;
    while((sln=(librdf_storage_list_node*)librdf_list_pop(context->list))) {
      librdf_free_statement(sln->statement);
      if(sln->context)
        librdf_free_node(sln->context);
      LIBRDF_FREE(librdf_storage_list_node, sln);
    }
    librdf_free_list(context->list);
    context->list=NULL;
  }

  if(context->index_contexts) {
    if(context->contexts) {
      librdf_free_hash(context->contexts);
      context->contexts=NULL;
    }
  }
  
  return 0;
}


static int
librdf_storage_list_size(librdf_storage* storage)
{
  librdf_storage_list_context* context=(librdf_storage_list_context*)storage->context;

  return librdf_list_size(context->list);
}


static int
librdf_storage_list_add_statement(librdf_storage* storage, librdf_statement* statement)
{
  /* Do not add duplicate statements */
  if(librdf_storage_list_contains_statement(storage, statement))
    return 0;

  return librdf_storage_list_context_add_statement(storage, NULL, statement);
}


static int
librdf_storage_list_add_statements(librdf_storage* storage,
                                   librdf_stream* statement_stream)
{
  librdf_storage_list_context* context=(librdf_storage_list_context*)storage->context;
  int status=0;

  for(; !librdf_stream_end(statement_stream);
      librdf_stream_next(statement_stream)) {
    librdf_statement* statement=librdf_stream_get_object(statement_stream);
    librdf_storage_list_node* sln;

    if(!statement) {
      status=1;
      break;
    }

    /* Do not add duplicate statements */
    if(librdf_storage_list_contains_statement(storage, statement))
      continue;

    sln=(librdf_storage_list_node*)LIBRDF_MALLOC(librdf_storage_list_node, sizeof(librdf_storage_list_node));
    if(!sln) {
      status=1;
      break;
    }
    
    /* copy shared statement */
    sln->statement=librdf_new_statement_from_statement(statement);
    if(!sln->statement) {
      LIBRDF_FREE(librdf_storage_list_node, sln);
      status=1;
      break;
    }
    sln->context=NULL;
    librdf_list_add(context->list, sln);
  }
  
  return status;
}


static int
librdf_storage_list_remove_statement(librdf_storage* storage, librdf_statement* statement)
{
  return librdf_storage_list_context_remove_statement(storage, NULL, statement);
}


static int
librdf_storage_list_contains_statement(librdf_storage* storage, librdf_statement* statement)
{
  librdf_storage_list_context* context=(librdf_storage_list_context*)storage->context;
  librdf_storage_list_node sln; /* STATIC */
  sln.statement=statement;
  sln.context=NULL;

  if(context->index_contexts) {
    /* When we have contexts, we have to use find_statements for contains
     * since we do not know what context node may be stored for a statement
     */
    librdf_stream *stream=librdf_storage_list_find_statements(storage, statement);
    int status;

    if(!stream)
      return 0;
    /* librdf_stream_end returns 0 if have more, non-0 at end */
    status=!librdf_stream_end(stream);
    /* convert to 0 if at end (not found) and non-zero otherwise (found) */
    librdf_free_stream(stream);
    return status;
  }
  

  return librdf_list_contains(context->list, &sln);
}


typedef struct {
  librdf_storage *storage;
  int index_contexts;
  librdf_iterator* iterator;
} librdf_storage_list_serialise_stream_context;


static librdf_stream*
librdf_storage_list_serialise(librdf_storage* storage)
{
  librdf_storage_list_context* context=(librdf_storage_list_context*)storage->context;
  librdf_storage_list_serialise_stream_context* scontext;
  librdf_stream* stream;
  
  scontext=(librdf_storage_list_serialise_stream_context*)LIBRDF_CALLOC(librdf_storage_list_serialise_stream_context, 1, sizeof(librdf_storage_list_serialise_stream_context));
  if(!scontext)
    return NULL;

  scontext->index_contexts=context->index_contexts;
  scontext->iterator=librdf_list_get_iterator(context->list);
  if(!scontext->iterator) {
    LIBRDF_FREE(librdf_storage_list_serialise_stream_context, scontext);
    return librdf_new_empty_stream(storage->world);
  }
    
  
  scontext->storage=storage;
  librdf_storage_add_reference(scontext->storage);

  stream=librdf_new_stream(storage->world,
                           (void*)scontext,
                           &librdf_storage_list_serialise_end_of_stream,
                           &librdf_storage_list_serialise_next_statement,
                           &librdf_storage_list_serialise_get_statement,
                           &librdf_storage_list_serialise_finished);
  if(!stream) {
    librdf_storage_list_serialise_finished((void*)scontext);
    return NULL;
  }
  
  return stream;  
}


static int
librdf_storage_list_serialise_end_of_stream(void* context)
{
  librdf_storage_list_serialise_stream_context* scontext=(librdf_storage_list_serialise_stream_context*)context;

  return librdf_iterator_end(scontext->iterator);

}

static int
librdf_storage_list_serialise_next_statement(void* context)
{
  librdf_storage_list_serialise_stream_context* scontext=(librdf_storage_list_serialise_stream_context*)context;

  return librdf_iterator_next(scontext->iterator);
}


static void*
librdf_storage_list_serialise_get_statement(void* context, int flags)
{
  librdf_storage_list_serialise_stream_context* scontext=(librdf_storage_list_serialise_stream_context*)context;
  librdf_storage_list_node* sln=(librdf_storage_list_node*)librdf_iterator_get_object(scontext->iterator);

  switch(flags) {
    case LIBRDF_ITERATOR_GET_METHOD_GET_OBJECT:
      return sln->statement;
    case LIBRDF_ITERATOR_GET_METHOD_GET_CONTEXT:
      if(scontext->index_contexts)
        return sln->context;
      else
        return NULL;
    default:
      librdf_log(scontext->iterator->world,
                 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
                 "Unknown iterator method flag %d", flags);
      return NULL;
  }
}


static void
librdf_storage_list_serialise_finished(void* context)
{
  librdf_storage_list_serialise_stream_context* scontext=(librdf_storage_list_serialise_stream_context*)context;

  if(scontext->iterator)
    librdf_free_iterator(scontext->iterator);

  if(scontext->storage)
    librdf_storage_remove_reference(scontext->storage);

  LIBRDF_FREE(librdf_storage_list_serialise_stream_context, scontext);
}


/**
 * librdf_storage_list_find_statements:
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
librdf_storage_list_find_statements(librdf_storage* storage, librdf_statement* statement)
{
  librdf_stream* stream;

  statement=librdf_new_statement_from_statement(statement);
  if(!statement)
    return NULL;
  
  stream=librdf_storage_list_serialise(storage);
  if(stream) {
    if(librdf_stream_add_map(stream, &librdf_stream_statement_find_map,
                             (librdf_stream_map_free_context_handler)&librdf_free_statement,
                             (void*)statement)) {
      /* error - stream_add_map failed */
      librdf_free_stream(stream);
      stream=NULL;
    }
  }
  else
    librdf_free_statement(statement);

  return stream;
}


/**
 * librdf_storage_list_context_add_statement:
 * @storage: #librdf_storage object
 * @context_node: #librdf_node object
 * @statement: #librdf_statement statement to add
 *
 * Add a statement to a storage context.
 * 
 * Return value: non 0 on failure
 **/
static int
librdf_storage_list_context_add_statement(librdf_storage* storage,
                                          librdf_node* context_node,
                                          librdf_statement* statement) 
{
  librdf_storage_list_context* context=(librdf_storage_list_context*)storage->context;
  librdf_hash_datum key, value; /* on stack - not allocated */
  size_t size;
  librdf_storage_list_node* sln;
  int status;

  if(context_node && !context->index_contexts) {
    librdf_log(storage->world, 0, LIBRDF_LOG_WARN, LIBRDF_FROM_STORAGE, NULL,
               "Storage was created without context support");
    return 1;
  }
  
  /* Store statement + node in the storage_list */
  sln=(librdf_storage_list_node*)LIBRDF_MALLOC(librdf_storage_list_node, sizeof(librdf_storage_list_node));
  if(!sln)
    return 1;

  sln->statement=librdf_new_statement_from_statement(statement);
  if(!sln->statement) {
    LIBRDF_FREE(librdf_storage_list_node, sln);
    return 1;
  }
  if(context->index_contexts && context_node) {
    sln->context=librdf_new_node_from_node(context_node);
    if(!sln->context) {
      librdf_free_statement(sln->statement);
      LIBRDF_FREE(librdf_storage_list_node, sln);
      return 1;
    }
  } else
    sln->context=NULL;
  
  status=librdf_list_add(context->list, sln);
  if(status) {
    if(context_node)
      librdf_free_node(sln->context);
    librdf_free_statement(sln->statement);
    LIBRDF_FREE(librdf_storage_list_node, sln);
    return 1;
  }

  if(!context->index_contexts || !context_node)
    return 0;
  
  /* Store (context => statement) in the context hash */
  size=librdf_node_encode(context_node, NULL, 0);
  key.data=(char*)LIBRDF_MALLOC(cstring, size);
  key.size=librdf_node_encode(context_node, (unsigned char*)key.data, size);

  size=librdf_statement_encode(statement, NULL, 0);
  value.data=(char*)LIBRDF_MALLOC(cstring, size);
  value.size=librdf_statement_encode(statement, (unsigned char*)value.data, size);

  status=librdf_hash_put(context->contexts, &key, &value);
  LIBRDF_FREE(data, key.data);
  LIBRDF_FREE(data, value.data);

  return status;
}


/**
 * librdf_storage_list_context_remove_statement:
 * @storage: #librdf_storage object
 * @context_node: #librdf_node object
 * @statement: #librdf_statement statement to remove
 *
 * Remove a statement from a storage context.
 * 
 * Return value: non 0 on failure
 **/
static int
librdf_storage_list_context_remove_statement(librdf_storage* storage, 
                                             librdf_node* context_node,
                                             librdf_statement* statement) 
{
  librdf_storage_list_context* context=(librdf_storage_list_context*)storage->context;
  librdf_hash_datum key, value; /* on stack - not allocated */
  librdf_storage_list_node* sln;
  librdf_storage_list_node search_sln; /* on stack - not allocated */
  size_t size;
  int status;

  if(context_node && !context->index_contexts) {
    librdf_log(storage->world, 0, LIBRDF_LOG_WARN, LIBRDF_FROM_STORAGE, NULL,
               "Storage was created without context support");
    return 1;
  }
  
  search_sln.statement=statement;
  search_sln.context=context_node;

  /* Remove stored statement+context */
  sln=(librdf_storage_list_node*)librdf_list_remove(context->list, &search_sln);
  if(!sln)
    return 1;

  librdf_free_statement(sln->statement);
  if(sln->context)
    librdf_free_node(sln->context);
  LIBRDF_FREE(librdf_storage_list_node, sln);

  if(!context->index_contexts || !context_node)
    return 0;
  
  /* Remove (context => statement) in the context hash */
  size=librdf_node_encode(context_node, NULL, 0);
  key.data=(char*)LIBRDF_MALLOC(cstring, size);
  key.size=librdf_node_encode(context_node, (unsigned char*)key.data, size);

  size=librdf_statement_encode(statement, NULL, 0);
  value.data=(char*)LIBRDF_MALLOC(cstring, size);
  value.size=librdf_statement_encode(statement, (unsigned char*)value.data, size);

  status=librdf_hash_delete(context->contexts, &key, &value);
  LIBRDF_FREE(data, key.data);
  LIBRDF_FREE(data, value.data);
  
  return status;
}


typedef struct {
  librdf_storage *storage;
  librdf_iterator* iterator;
  librdf_hash_datum *key;
  librdf_hash_datum *value;
  librdf_statement current; /* static, shared statement */
  librdf_node *context_node;
  char *context_node_data;
} librdf_storage_list_context_serialise_stream_context;


/**
 * librdf_storage_list_context_serialise:
 * @storage: #librdf_storage object
 * @context_node: #librdf_node object
 *
 * List all statements in a storage context.
 * 
 * Return value: #librdf_stream of statements or NULL on failure or context is empty
 **/
static librdf_stream*
librdf_storage_list_context_serialise(librdf_storage* storage,
                                      librdf_node* context_node) 
{
  librdf_storage_list_context* context=(librdf_storage_list_context*)storage->context;
  librdf_storage_list_context_serialise_stream_context* scontext;
  librdf_stream* stream;
  size_t size;

  if(!context->index_contexts) {
    librdf_log(storage->world, 0, LIBRDF_LOG_WARN, LIBRDF_FROM_STORAGE, NULL,
               "Storage was created without context support");
    return NULL;
  }
  
  scontext=(librdf_storage_list_context_serialise_stream_context*)LIBRDF_CALLOC(librdf_storage_list_context_serialise_stream_context, 1, sizeof(librdf_storage_list_context_serialise_stream_context));
  if(!scontext)
    return NULL;

  librdf_statement_init(storage->world, &scontext->current);

  scontext->key=librdf_new_hash_datum(storage->world, NULL, 0);
  if(!scontext->key)
    return NULL;
  
  scontext->value=librdf_new_hash_datum(storage->world, NULL, 0);
  if(!scontext->value) {
    librdf_free_hash_datum(scontext->key);
    return NULL;
  }

  scontext->context_node=librdf_new_node_from_node(context_node);

  size=librdf_node_encode(scontext->context_node, NULL, 0);
  scontext->key->data=scontext->context_node_data=(char*)LIBRDF_MALLOC(cstring, size);
  scontext->key->size=librdf_node_encode(scontext->context_node,
                                         (unsigned char*)scontext->key->data,
                                         size);

  scontext->iterator=librdf_hash_get_all(context->contexts, 
                                         scontext->key, scontext->value);
  if(!scontext->iterator)
    return librdf_new_empty_stream(storage->world);


  scontext->storage=storage;
  librdf_storage_add_reference(scontext->storage);

  stream=librdf_new_stream(storage->world,
                           (void*)scontext,
                           &librdf_storage_list_context_serialise_end_of_stream,
                           &librdf_storage_list_context_serialise_next_statement,
                           &librdf_storage_list_context_serialise_get_statement,
                           &librdf_storage_list_context_serialise_finished);
  if(!stream) {
    librdf_storage_list_context_serialise_finished((void*)scontext);
    return NULL;
  }
  
  return stream;  
}


static int
librdf_storage_list_context_serialise_end_of_stream(void* context)
{
  librdf_storage_list_context_serialise_stream_context* scontext=(librdf_storage_list_context_serialise_stream_context*)context;

  return librdf_iterator_end(scontext->iterator);
}


static int
librdf_storage_list_context_serialise_next_statement(void* context)
{
  librdf_storage_list_context_serialise_stream_context* scontext=(librdf_storage_list_context_serialise_stream_context*)context;

  return librdf_iterator_next(scontext->iterator);
}


static void*
librdf_storage_list_context_serialise_get_statement(void* context, int flags)
{
  librdf_storage_list_context_serialise_stream_context* scontext=(librdf_storage_list_context_serialise_stream_context*)context;
  librdf_hash_datum* v;
  
  switch(flags) {
    case LIBRDF_ITERATOR_GET_METHOD_GET_OBJECT:
      if(!(v=(librdf_hash_datum*)librdf_iterator_get_value(scontext->iterator)))
        return NULL;

      librdf_statement_clear(&scontext->current);

      /* decode value content */
      if(!librdf_statement_decode(&scontext->current,
                                  (unsigned char*)v->data, v->size)) {
        return NULL;
      }

      return &scontext->current;

    case LIBRDF_ITERATOR_GET_METHOD_GET_CONTEXT:
      return scontext->context_node;
    default:
      librdf_log(scontext->iterator->world,
                 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
                 "Unknown iterator method flag %d", flags);
      return NULL;
  }
  
}


static void
librdf_storage_list_context_serialise_finished(void* context)
{
  librdf_storage_list_context_serialise_stream_context* scontext=(librdf_storage_list_context_serialise_stream_context*)context;
  
  if(scontext->context_node)
    librdf_free_node(scontext->context_node);
  
  if(scontext->iterator)
    librdf_free_iterator(scontext->iterator);

  if(scontext->key) {
    scontext->key->data=NULL;
    librdf_free_hash_datum(scontext->key);
  }
  
  if(scontext->value) {
    scontext->value->data=NULL;
    librdf_free_hash_datum(scontext->value);
  }

  if(scontext->context_node_data)
    LIBRDF_FREE(cstring, scontext->context_node_data);

  librdf_statement_clear(&scontext->current);

  if(scontext->storage)
    librdf_storage_remove_reference(scontext->storage);

  LIBRDF_FREE(librdf_storage_list_context_serialise_stream_context, scontext);
}



typedef struct {
  librdf_storage *storage;
  librdf_iterator *iterator;
  librdf_hash_datum *key;
  librdf_node *current;
} librdf_storage_list_get_contexts_iterator_context;



static int
librdf_storage_list_get_contexts_is_end(void* iterator)
{
  librdf_storage_list_get_contexts_iterator_context* icontext=(librdf_storage_list_get_contexts_iterator_context*)iterator;

  return librdf_iterator_end(icontext->iterator);
}


static int
librdf_storage_list_get_contexts_next_method(void* iterator) 
{
  librdf_storage_list_get_contexts_iterator_context* icontext=(librdf_storage_list_get_contexts_iterator_context*)iterator;

  return librdf_iterator_next(icontext->iterator);
}


static void*
librdf_storage_list_get_contexts_get_method(void* iterator, int flags) 
{
  librdf_storage_list_get_contexts_iterator_context* icontext=(librdf_storage_list_get_contexts_iterator_context*)iterator;
  void *result=NULL;
  librdf_hash_datum* k;
  
  switch(flags) {
    case LIBRDF_ITERATOR_GET_METHOD_GET_OBJECT:
      if(!(k=(librdf_hash_datum*)librdf_iterator_get_key(icontext->iterator)))
        return NULL;

      if(icontext->current)
        librdf_free_node(icontext->current);

      /* decode value content */
      icontext->current=librdf_node_decode(icontext->storage->world, NULL,
                                           (unsigned char*)k->data, k->size);
      result=icontext->current;
      break;

    case LIBRDF_ITERATOR_GET_METHOD_GET_KEY:
    case LIBRDF_ITERATOR_GET_METHOD_GET_VALUE:
      result=NULL;
      break;
      
    default:
      librdf_log(icontext->iterator->world,
                 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
                 "Unknown iterator method flag %d", flags);
      result=NULL;
      break;
  }

  return result;
}


static void
librdf_storage_list_get_contexts_finished(void* iterator) 
{
  librdf_storage_list_get_contexts_iterator_context* icontext=(librdf_storage_list_get_contexts_iterator_context*)iterator;

  if(icontext->iterator)
    librdf_free_iterator(icontext->iterator);

  librdf_free_hash_datum(icontext->key);
  
  if(icontext->current)
    librdf_free_node(icontext->current);

  if(icontext->storage)
    librdf_storage_remove_reference(icontext->storage);
  
  LIBRDF_FREE(librdf_storage_list_get_contexts_iterator_context, icontext);
}


/**
 * librdf_storage_list_context_get_contexts:
 * @storage: #librdf_storage object
 *
 * List all context nodes in a storage.
 * 
 * Return value: #librdf_iterator of context_nodes or NULL on failure or no contexts
 **/
static librdf_iterator*
librdf_storage_list_get_contexts(librdf_storage* storage) 
{
  librdf_storage_list_context* context=(librdf_storage_list_context*)storage->context;
  librdf_storage_list_get_contexts_iterator_context* icontext;
  librdf_iterator* iterator;

  if(!context->index_contexts) {
    librdf_log(storage->world, 0, LIBRDF_LOG_WARN, LIBRDF_FROM_STORAGE, NULL,
               "Storage was created without context support");
    return NULL;
  }
  
  icontext=(librdf_storage_list_get_contexts_iterator_context*)LIBRDF_CALLOC(librdf_storage_list_get_contexts_iterator_context, 1, sizeof(librdf_storage_list_get_contexts_iterator_context));
  if(!icontext)
    return NULL;

  icontext->key=librdf_new_hash_datum(storage->world, NULL, 0);
  if(!icontext->key)
    return NULL;
  
  icontext->storage=storage;
  librdf_storage_add_reference(icontext->storage);
  
  icontext->iterator=librdf_hash_keys(context->contexts, icontext->key);
  if(!icontext->iterator) {
    librdf_storage_list_get_contexts_finished(icontext);
    return librdf_new_empty_iterator(storage->world);
  }


  iterator=librdf_new_iterator(storage->world,
                               (void*)icontext,
                               &librdf_storage_list_get_contexts_is_end,
                               &librdf_storage_list_get_contexts_next_method,
                               &librdf_storage_list_get_contexts_get_method,
                               &librdf_storage_list_get_contexts_finished);
  if(!iterator)
    librdf_storage_list_get_contexts_finished(icontext);
  return iterator;
}



/**
 * librdf_storage_list_get_feature:
 * @storage: #librdf_storage object
 * @feature: #librdf_uri feature property
 *
 * Get the value of a storage feature.
 * 
 * Return value: #librdf_node feature value or NULL if no such feature
 * exists or the value is empty.
 **/
static librdf_node*
librdf_storage_list_get_feature(librdf_storage* storage, librdf_uri* feature)
{
  librdf_storage_list_context* scontext=(librdf_storage_list_context*)storage->context;
  unsigned char *uri_string;

  if(!feature)
    return NULL;

  uri_string=librdf_uri_as_string(feature);
  if(!uri_string)
    return NULL;
  
  if(!strcmp((const char*)uri_string, LIBRDF_MODEL_FEATURE_CONTEXTS)) {
    unsigned char value[2];

    sprintf((char*)value, "%d", (scontext->index_contexts != 0));
    return librdf_new_node_from_typed_literal(storage->world,
                                              value, NULL, NULL);
  }

  return NULL;
}


/* local function to register list storage functions */

static void
librdf_storage_list_register_factory(librdf_storage_factory *factory) 
{
  factory->context_length     = sizeof(librdf_storage_list_context);
  
  factory->init               = librdf_storage_list_init;
  factory->terminate          = librdf_storage_list_terminate;
  factory->open               = librdf_storage_list_open;
  factory->close              = librdf_storage_list_close;
  factory->size               = librdf_storage_list_size;
  factory->add_statement      = librdf_storage_list_add_statement;
  factory->add_statements     = librdf_storage_list_add_statements;
  factory->remove_statement   = librdf_storage_list_remove_statement;
  factory->contains_statement = librdf_storage_list_contains_statement;
  factory->serialise          = librdf_storage_list_serialise;
  factory->find_statements    = librdf_storage_list_find_statements;
  factory->context_add_statement    = librdf_storage_list_context_add_statement;
  factory->context_remove_statement = librdf_storage_list_context_remove_statement;
  factory->context_serialise        = librdf_storage_list_context_serialise;
  factory->get_contexts             = librdf_storage_list_get_contexts;
  factory->get_feature              = librdf_storage_list_get_feature;
}


/**
 * librdf_init_storage_list:
 * @world: world object
 * 
 * INTERNAL - initialise the storage_list module.
 **/
void
librdf_init_storage_list(librdf_world *world)
{
  librdf_storage_register_factory(world, "memory", "In memory",
                                  &librdf_storage_list_register_factory);
}
