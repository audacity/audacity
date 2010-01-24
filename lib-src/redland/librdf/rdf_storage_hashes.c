/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_storage_hashes.c - RDF Storage as Hashes Implementation
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

#include <sys/types.h>

#include <stdio.h>
#include <string.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h> /* for abort() as used in errors */
#endif


#include <redland.h>
#include <rdf_storage.h>


typedef struct 
{
  const char *name;
  int key_fields; /* OR of LIBRDF_STATEMENT_* fields defined in rdf_statement.h */
  int value_fields; /* ditto */
} librdf_hash_descriptor;


static const librdf_hash_descriptor librdf_storage_hashes_descriptions[]= {
  {"sp2o",
   LIBRDF_STATEMENT_SUBJECT|LIBRDF_STATEMENT_PREDICATE,
   LIBRDF_STATEMENT_OBJECT},  /* For 'get targets' */
  {"po2s",
   LIBRDF_STATEMENT_PREDICATE|LIBRDF_STATEMENT_OBJECT,
   LIBRDF_STATEMENT_SUBJECT},  /* For 'get sources' */
  {"so2p", 
   LIBRDF_STATEMENT_SUBJECT|LIBRDF_STATEMENT_OBJECT,
   LIBRDF_STATEMENT_PREDICATE},  /* For 'get arcs' */
  {"p2so", 
   LIBRDF_STATEMENT_PREDICATE,
   LIBRDF_STATEMENT_SUBJECT|LIBRDF_STATEMENT_OBJECT},  /* For '(?, p, ?)' */
  {"contexts",
   0L, /* for contexts - do not touch when storing statements! */
   0L},
  {NULL,0L,0L}
};


static const librdf_hash_descriptor*
librdf_storage_get_hash_description_by_name(const char *name) 
{
  int i;
  const librdf_hash_descriptor *d;
  
  for(i=0; (d=&librdf_storage_hashes_descriptions[i]); i++) {
    if(!d->name)
      return NULL;
    
    if(!strcmp(d->name, name))
      return d;
  }
  return NULL;
}


typedef struct
{
  /* from init() argument */
  char         *name;
  /* from options decoded from options hash during init() */
  char         *hash_type;
  char         *db_dir;
  char         *indexes;
  int           mode;
  int           is_writable;
  int           is_new;
  librdf_hash  *options;     /* remaining options for hash open method */

  /* internals */
  int                      hash_count; /* how many hashes are present? */
  /* The following are allocated arrays of size hash_count */
  librdf_hash**            hashes;
  librdf_hash_descriptor** hash_descriptions;
  char**                   names;       /* hash names for hash open method */

  int sources_index;
  int arcs_index;
  int targets_index;

  int p2so_index;

  /* If this is non-0, contexts are being used */
  int index_contexts;
  int contexts_index;

  int all_statements_hash_index;

  /* growing buffers used to en/decode keys/values */
  unsigned char *key_buffer;
  size_t key_buffer_len;
  unsigned char *value_buffer;
  size_t value_buffer_len;
} librdf_storage_hashes_context;



/* helper function for implementing init and clone methods */
static int librdf_storage_hashes_register(librdf_storage *storage, const char *name, const librdf_hash_descriptor *source_desc);
static int librdf_storage_hashes_init_common(librdf_storage* storage, const char *name, char *hash_type, char *db_dir, char *indexes, int mode, int is_writable, int is_new, librdf_hash* options);


/* prototypes for local functions */
static int librdf_storage_hashes_init(librdf_storage* storage, const char *name, librdf_hash* options);
static void librdf_storage_hashes_terminate(librdf_storage* storage);
static int librdf_storage_hashes_clone(librdf_storage* new_storage, librdf_storage* old_storage);
static int librdf_storage_hashes_open(librdf_storage* storage, librdf_model* model);
static int librdf_storage_hashes_close(librdf_storage* storage);
static int librdf_storage_hashes_size(librdf_storage* storage);
static int librdf_storage_hashes_add_statement(librdf_storage* storage, librdf_statement* statement);
static int librdf_storage_hashes_add_statements(librdf_storage* storage, librdf_stream* statement_stream);
static int librdf_storage_hashes_remove_statement(librdf_storage* storage, librdf_statement* statement);
static int librdf_storage_hashes_contains_statement(librdf_storage* storage, librdf_statement* statement);
static librdf_stream* librdf_storage_hashes_serialise(librdf_storage* storage);
static librdf_stream* librdf_storage_hashes_find_statements(librdf_storage* storage, librdf_statement* statement);
static librdf_iterator* librdf_storage_hashes_find_sources(librdf_storage* storage, librdf_node* arc, librdf_node *target);
static librdf_iterator* librdf_storage_hashes_find_arcs(librdf_storage* storage, librdf_node* source, librdf_node *target);
static librdf_iterator* librdf_storage_hashes_find_targets(librdf_storage* storage, librdf_node* source, librdf_node *arc);

/* serialising implementing functions */
static int librdf_storage_hashes_serialise_end_of_stream(void* context);
static int librdf_storage_hashes_serialise_next_statement(void* context);
static void* librdf_storage_hashes_serialise_get_statement(void* context, int flags);
static void librdf_storage_hashes_serialise_finished(void* context);

/* context functions */
static int librdf_storage_hashes_context_add_statement(librdf_storage* storage, librdf_node* context_node, librdf_statement* statement);
static int librdf_storage_hashes_context_remove_statement(librdf_storage* storage, librdf_node* context_node, librdf_statement* statement);
static librdf_stream* librdf_storage_hashes_context_serialise(librdf_storage* storage, librdf_node* context_node);

/* context list statement stream methods */
static int librdf_storage_hashes_context_serialise_end_of_stream(void* context);
static int librdf_storage_hashes_context_serialise_next_statement(void* context);
static void* librdf_storage_hashes_context_serialise_get_statement(void* context, int flags);
static void librdf_storage_hashes_context_serialise_finished(void* context);

static void librdf_storage_hashes_register_factory(librdf_storage_factory *factory);


/* node iterator implementing functions for get sources, targets, arcs methods */
static int librdf_storage_hashes_node_iterator_is_end(void* iterator);
static int librdf_storage_hashes_node_iterator_next_method(void* iterator);
static void* librdf_storage_hashes_node_iterator_get_method(void* iterator, int flags);
static void librdf_storage_hashes_node_iterator_finished(void* iterator);
/* common initialisation code for creating get sources, targets, arcs iterators */
static librdf_iterator* librdf_storage_hashes_node_iterator_create(librdf_storage* storage, librdf_node* node1, librdf_node *node2, int hash_index, int want);



static int
librdf_storage_hashes_register(librdf_storage *storage,
                               const char *name,
                               const librdf_hash_descriptor *source_desc) 
{
  librdf_storage_hashes_context *context=(librdf_storage_hashes_context*)storage->context;
  int len;
  char *full_name=NULL;
  int hash_index;
  librdf_hash_descriptor *desc=(librdf_hash_descriptor*)LIBRDF_MALLOC(librdf_hash_descriptor, sizeof(librdf_hash_descriptor));
  
  if(!desc)
    return 1;
    
  memcpy(desc, source_desc, sizeof(librdf_hash_descriptor));

  hash_index=context->hash_count++;
  
  context->hash_descriptions[hash_index]=desc;
    
  if(name) {
    len=strlen(desc->name) + 1 + strlen(name) + 1; /* "%s-%s\0" */
    if(context->db_dir)
      len+=strlen(context->db_dir) +1;
    
    full_name=(char*)LIBRDF_MALLOC(cstring, len);
    if(!full_name)
      return 1;

    /* FIXME: Implies Unix filenames */
    if(context->db_dir)
      sprintf(full_name, "%s/%s-%s", context->db_dir, name, desc->name);
    else
      sprintf(full_name, "%s-%s", name, desc->name);
  }
  
  context->hashes[hash_index]=librdf_new_hash(storage->world, 
                                              context->hash_type);
  context->names[hash_index]=full_name;

  return (context->hashes[hash_index] == NULL);
}

/* helper function for implementing init and clone methods */

static int
librdf_storage_hashes_init_common(librdf_storage* storage, const char *name,
                                  char *hash_type, char *db_dir,
                                  char *indexes,
                                  int mode, int is_writable, int is_new,
                                  librdf_hash* options)
{
  librdf_storage_hashes_context *context=(librdf_storage_hashes_context*)storage->context;
  int i;
  int status=0;
  int index_predicates=0;
  int index_contexts=0;
  int hash_count=0;
  
  context->name=(char*)name;
  
  context->hash_type=hash_type;
  context->db_dir=db_dir;
  context->indexes=indexes;

  context->mode=mode;
  context->is_writable=is_writable;
  context->is_new=is_new;
  context->options=options;

  /* Work out the number of hashes for allocating stuff below */
  hash_count=3;

  if((index_contexts=librdf_hash_get_as_boolean(options, "contexts"))<0)
    index_contexts=0; /* default is no contexts */
  context->index_contexts=index_contexts;

  if(index_contexts)
    hash_count++;

  if((index_predicates=librdf_hash_get_as_boolean(options, "index-predicates"))<0)
    index_predicates=0; /* default is NO index on properties */
  
  if(index_predicates)
    hash_count++;


  /* Start allocating the arrays */
  context->hashes=(librdf_hash**)LIBRDF_CALLOC(librdf_hash, hash_count, sizeof(librdf_hash*));
  if(!context->hashes) {
    if(context->name)
      LIBRDF_FREE(cstring, context->name);
    return 1;
  }

  context->hash_descriptions=(librdf_hash_descriptor**)LIBRDF_CALLOC(librdf_hash_descriptor, hash_count, sizeof(librdf_hash_descriptor*));
  if(!context->hash_descriptions) {
    LIBRDF_FREE(librdf_hash, context->hashes);
    if(context->name)
      LIBRDF_FREE(cstring, context->name);
    return 1;
  }
  
  context->names=(char**)LIBRDF_CALLOC(cstring, hash_count, sizeof(char*));
  if(!context->names) {
    LIBRDF_FREE(librdf_hash, context->hashes);
    LIBRDF_FREE(librdf_hash_descriptor, context->hash_descriptions);
    if(context->name)
      LIBRDF_FREE(cstring, context->name);
    return 1;
  }
  
  for(i=0; i<3; i++) {
    status=librdf_storage_hashes_register(storage, name,
                                          &librdf_storage_hashes_descriptions[i]);
    if(status)
      break;
  }

  if(index_predicates && !status)
    status=librdf_storage_hashes_register(storage, name,
                                          librdf_storage_get_hash_description_by_name("p2so"));

  if(index_contexts && !status)
    librdf_storage_hashes_register(storage, name,
                                   librdf_storage_get_hash_description_by_name("contexts"));


  /* find indexes for get targets, sources and arcs */
  context->sources_index= -1;
  context->arcs_index= -1;
  context->targets_index= -1;
  context->p2so_index= -1;
  /* and index for contexts (no key or value fields) */
  context->contexts_index= -1;

  context->all_statements_hash_index= -1;

  for(i=0; i<context->hash_count; i++) {
    int key_fields=context->hash_descriptions[i]->key_fields;
    int value_fields=context->hash_descriptions[i]->value_fields;

    if(context->all_statements_hash_index <0 &&
       ((key_fields|value_fields)==(LIBRDF_STATEMENT_SUBJECT|LIBRDF_STATEMENT_PREDICATE|LIBRDF_STATEMENT_OBJECT))) {
      context->all_statements_hash_index=i;
    }
    
    if(key_fields == (LIBRDF_STATEMENT_SUBJECT|LIBRDF_STATEMENT_PREDICATE) &&
       value_fields == LIBRDF_STATEMENT_OBJECT) {
      context->targets_index=i;
    } else if(key_fields == (LIBRDF_STATEMENT_PREDICATE|LIBRDF_STATEMENT_OBJECT) &&
              value_fields == LIBRDF_STATEMENT_SUBJECT) {
      context->sources_index=i;
    } else if(key_fields == (LIBRDF_STATEMENT_SUBJECT|LIBRDF_STATEMENT_OBJECT) &&
              value_fields == LIBRDF_STATEMENT_PREDICATE) {
      context->arcs_index=i;
    } else if(key_fields == LIBRDF_STATEMENT_PREDICATE &&
              value_fields == (LIBRDF_STATEMENT_SUBJECT|LIBRDF_STATEMENT_OBJECT)) {
      context->p2so_index=i;
    } else if(!key_fields || !value_fields) {
       context->contexts_index=i;
    }
  }

  return status;
}


/* functions implementing storage api */
static int
librdf_storage_hashes_init(librdf_storage* storage, const char *name,
                           librdf_hash* options)
{
  char *hash_type, *db_dir, *indexes;
  int mode, is_writable, is_new;
  char *name_copy=NULL;
  
  if(!options)
    return 1;

  hash_type=librdf_hash_get_del(options, "hash-type");
  if(!hash_type)
    return 1;

  db_dir=librdf_hash_get_del(options, "dir");
  
  indexes=librdf_hash_get_del(options, "indexes");
  
  if((mode=librdf_hash_get_as_long(options, "mode"))<0)
    mode=0644; /* default mode */
  
  if((is_writable=librdf_hash_get_as_boolean(options, "write"))<0)
    is_writable=1; /* default is WRITABLE */
  
  if((is_new=librdf_hash_get_as_boolean(options, "new"))<0)
    is_new=0; /* default is NOT NEW */

  if(name) {
    name_copy = (char *)LIBRDF_MALLOC(cstring, strlen(name)+1);
    if(!name_copy)
      return 1;
    strcpy(name_copy, name);
  }

  return librdf_storage_hashes_init_common(storage, name_copy, 
                                           hash_type, db_dir, indexes,
                                           mode, is_writable, is_new, 
                                           options);
}

  

static void
librdf_storage_hashes_terminate(librdf_storage* storage)
{
  librdf_storage_hashes_context *context=(librdf_storage_hashes_context*)storage->context;
  int i;
  
  for(i=0; i<context->hash_count; i++) {
    if(context->hash_descriptions && context->hash_descriptions[i])
      LIBRDF_FREE(librdf_hash_descriptor, context->hash_descriptions[i]);
    if(context->hashes && context->hashes[i])
      librdf_free_hash(context->hashes[i]);
    if(context->names && context->names[i])
      LIBRDF_FREE(cstring,context->names[i]);
  }

  if(context->hash_descriptions)
    LIBRDF_FREE(librdf_hash_descriptor, context->hash_descriptions);

  if(context->hashes)
    LIBRDF_FREE(librdf_hash_descriptor, context->hashes);

  if(context->names)
    LIBRDF_FREE(cstring, context->names);

  if(context->options)
    librdf_free_hash(context->options);

  if(context->hash_type)
    LIBRDF_FREE(cstring, context->hash_type);

  if(context->db_dir)
    LIBRDF_FREE(cstring, context->db_dir);

  if(context->indexes)
    LIBRDF_FREE(cstring, context->indexes);

  if(context->key_buffer)
    LIBRDF_FREE(data, context->key_buffer);
  if(context->value_buffer)
    LIBRDF_FREE(data, context->value_buffer);

  if(context->name)
    LIBRDF_FREE(cstring, context->name);
}


static int
librdf_storage_hashes_clone(librdf_storage* new_storage, librdf_storage* old_storage)
{
  librdf_storage_hashes_context *old_context=(librdf_storage_hashes_context*)old_storage->context;
  librdf_storage_hashes_context *new_context=(librdf_storage_hashes_context*)new_storage->context;

  /* Bump up old context name if any */
  if(old_context->name) {  
    new_context->name=librdf_heuristic_gen_name(old_context->name);
    if(!new_context->name)
      return 1;
  }
  
  /* This is always a copy of an in-memory hash */
  new_context->options=librdf_new_hash_from_hash(old_context->options);
  if(!new_context->options) {
    if(new_context->name)
      LIBRDF_FREE(cstring, new_context->name);
    return 1;
  }

  if(librdf_storage_hashes_init_common(new_storage, new_context->name,
                                       old_context->hash_type,
                                       old_context->db_dir,
                                       old_context->indexes,
                                       old_context->mode,
                                       old_context->is_writable,
                                       old_context->is_new,
                                       new_context->options)) {
    librdf_free_hash(new_context->options);
    if(new_context->name)
      LIBRDF_FREE(cstring, new_context->name);
    return 1;
  }

  return 0;
}
 

static int
librdf_storage_hashes_open(librdf_storage* storage, librdf_model* model)
{
  librdf_storage_hashes_context *context=(librdf_storage_hashes_context*)storage->context;
  int i;
  int result=0;
  
  for(i=0; i<context->hash_count; i++) {
    librdf_hash *hash=context->hashes[i];

    if(!hash ||
       librdf_hash_open(hash, context->names[i], 
                        context->mode, context->is_writable, context->is_new,
                        context->options)) {
      /* I still have my "Structured Fortran" book */
      int j;
      for (j=0; j<i; j++) {
        librdf_hash_close(context->hashes[j]);
        context->hashes[j]=NULL;
      }
      
      result=1;
    }

    if(result)
      break;
  }

  return result;
}


/**
 * librdf_storage_hashes_close:
 * @storage: 
 *
 * .
 * 
 * Close the storage hashes storage, and free all content since there is no 
 * persistance.
 * 
 * Return value: non 0 on failure
 **/
static int
librdf_storage_hashes_close(librdf_storage* storage)
{
  librdf_storage_hashes_context* context=(librdf_storage_hashes_context*)storage->context;
  int i;
  
  for(i=0; i<context->hash_count; i++) {
    if(context->hashes[i])
      librdf_hash_close(context->hashes[i]);
  }
  
  return 0;
}


static int
librdf_storage_hashes_size(librdf_storage* storage)
{
  librdf_storage_hashes_context* context=(librdf_storage_hashes_context*)storage->context;
  librdf_hash* any_hash=context->hashes[context->all_statements_hash_index];

  if(!any_hash)
    return -1;

  return librdf_hash_values_count(any_hash);
}


static int
librdf_storage_hashes_grow_buffer(unsigned char **buffer, size_t *len,
                                  size_t required_len) 
{
  if(required_len <= *len)
    return 0;

  if(*buffer)
    LIBRDF_FREE(data, *buffer);
  *len=required_len+8;
  *buffer=(unsigned char*)LIBRDF_MALLOC(data, *len);
  if(!*buffer)
    *len=0;
  return (*len < required_len);
}


static int
librdf_storage_hashes_add_remove_statement(librdf_storage* storage, 
                                           librdf_statement* statement,
                                           librdf_node* context_node,
                                           int is_addition)
{
  librdf_storage_hashes_context* context=(librdf_storage_hashes_context*)storage->context;
  int i;
  int status=0;

#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
  if(is_addition)
    LIBRDF_DEBUG1("Adding statement: ");
  else
    LIBRDF_DEBUG1("Removing statement: ");
  librdf_statement_print(statement, stderr);
  fputc('\n', stderr);
#endif  

  for(i=0; i<context->hash_count; i++) {
    librdf_hash_datum hd_key, hd_value; /* on stack */
    int key_len, value_len;

    /* ENCODE KEY */

    librdf_statement_part fields=(librdf_statement_part)context->hash_descriptions[i]->key_fields;
    if(!fields)
      continue;
    
    key_len=librdf_statement_encode_parts(statement, NULL, NULL, 0, fields);
    if(!key_len)
      return 1;
    if(librdf_storage_hashes_grow_buffer(&context->key_buffer, 
                                         &context->key_buffer_len, key_len)) {
      status=1;
      break;
    }
       
    if(!librdf_statement_encode_parts(statement, NULL,
                                      context->key_buffer,
                                      context->key_buffer_len, fields)) {
      status=1;
      break;
    }

    
    /* ENCODE VALUE */
    
    fields=(librdf_statement_part)context->hash_descriptions[i]->value_fields;
    if(!fields)
      continue;
    
    value_len=librdf_statement_encode_parts(statement, context_node,
                                            NULL, 0, fields);
    if(!value_len) {
      status=1;
      break;
    }
    
    if(librdf_storage_hashes_grow_buffer(&context->value_buffer, 
                                         &context->value_buffer_len, value_len)) {
      status=1;
      break;
    }
       
    if(!librdf_statement_encode_parts(statement, context_node,
                                      context->value_buffer,
                                      context->value_buffer_len, fields)) {
      status=1;
      break;
    }


#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
    LIBRDF_DEBUG4("Using %s hash key %d bytes -> value %d bytes\n", context->hash_descriptions[i]->name, key_len, value_len);
#endif

    /* Finally, store / remove the sucker */
    hd_key.data=context->key_buffer; hd_key.size=key_len;
    hd_value.data=context->value_buffer; hd_value.size=value_len;
    
    if(is_addition)
      status=librdf_hash_put(context->hashes[i], &hd_key, &hd_value);
    else
      status=librdf_hash_delete(context->hashes[i], &hd_key, &hd_value);
    
    if(status)
      break;
  }

  return status;
}


static int
librdf_storage_hashes_add_statement(librdf_storage* storage, librdf_statement* statement)
{
  /* Do not add duplicate statements */
  if(librdf_storage_hashes_contains_statement(storage, statement))
    return 0;

  return librdf_storage_hashes_add_remove_statement(storage, statement, NULL, 1);
}


static int
librdf_storage_hashes_add_statements(librdf_storage* storage,
                                     librdf_stream* statement_stream)
{
  int status=0;

  while(!librdf_stream_end(statement_stream)) {
    librdf_statement* statement=librdf_stream_get_object(statement_stream);

    if(statement) {
      status=librdf_storage_hashes_add_statement(storage, statement);
    } else
      status=1;

    if(status)
      break;

    librdf_stream_next(statement_stream);
  }

  return status;
}


static int
librdf_storage_hashes_remove_statement(librdf_storage* storage, librdf_statement* statement)
{
  return librdf_storage_hashes_add_remove_statement(storage, statement, NULL, 0);
}


static int
librdf_storage_hashes_contains_statement(librdf_storage* storage, librdf_statement* statement)
{
  librdf_storage_hashes_context* context=(librdf_storage_hashes_context*)storage->context;
  librdf_hash_datum hd_key, hd_value; /* on stack */
  unsigned char *key_buffer, *value_buffer;
  int key_len, value_len;
  int hash_index=context->all_statements_hash_index;
  librdf_statement_part fields;
  int status;
  
  if(storage->index_contexts) {
    /* When we have contexts, we have to use find_statements for contains
     * since a statement is encoded in KEY/VALUE and the VALUE may
     * contain some context node.
     */
    librdf_stream *stream=librdf_storage_hashes_find_statements(storage, statement);
    
    if(!stream)
      return 0;
    /* librdf_stream_end returns 0 if have more, non-0 at end */
    status=!librdf_stream_end(stream);
    /* convert to 0 if at end (not found) and non-zero otherwise (found) */
    librdf_free_stream(stream);
    return status;
  }

  /* ENCODE KEY */
  fields=(librdf_statement_part)context->hash_descriptions[hash_index]->key_fields;
  key_len=librdf_statement_encode_parts(statement, NULL,
                                        NULL, 0, fields);
  if(!key_len)
    return 1;
  if(!(key_buffer=(unsigned char*)LIBRDF_MALLOC(data, key_len)))
    return 1;
       
  if(!librdf_statement_encode_parts(statement, NULL,
                                    key_buffer, key_len, fields)) {
    LIBRDF_FREE(data, key_buffer);
    return 1;
  }

  /* ENCODE VALUE */
  fields=(librdf_statement_part)context->hash_descriptions[hash_index]->value_fields;
  value_len=librdf_statement_encode_parts(statement, NULL,
                                          NULL, 0, fields);
  if(!value_len) {
    LIBRDF_FREE(data, key_buffer);
    return 1;
  }
    
  if(!(value_buffer=(unsigned char*)LIBRDF_MALLOC(data, value_len))) {
    LIBRDF_FREE(data, key_buffer);
    return 1;
  }

       
  if(!librdf_statement_encode_parts(statement, NULL,
                                    value_buffer, value_len, fields)) {
    LIBRDF_FREE(data, key_buffer);
    LIBRDF_FREE(data, value_buffer);
    return 1;
  }


#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
  LIBRDF_DEBUG4("Using %s hash key %d bytes -> value %d bytes\n", context->hash_descriptions[hash_index]->name, key_len, value_len);
#endif

  hd_key.data=key_buffer; hd_key.size=key_len;
  hd_value.data=value_buffer; hd_value.size=value_len;
  status=librdf_hash_exists(context->hashes[hash_index], &hd_key, &hd_value);
  
  LIBRDF_FREE(data, key_buffer);
  LIBRDF_FREE(data, value_buffer);

  /* DO NOT free statement, ownership was not passed in */
  return status;
}



typedef struct {
  librdf_storage *storage;
  librdf_storage_hashes_context *hash_context;
  int index;
  librdf_iterator* iterator;
  librdf_hash_datum *key;
  librdf_hash_datum *value;
  librdf_node *search_node;
  librdf_statement current; /* static, shared when search_node not used */
  int index_contexts; /* true if this storage indexes contexts */
  librdf_node *context_node;
  int current_is_ok; /* true when current statement and context_node fresh */
} librdf_storage_hashes_serialise_stream_context;


static librdf_stream*
librdf_storage_hashes_serialise_common(librdf_storage* storage, int hash_index,
                                       librdf_node* search_node, int want)
{
  librdf_storage_hashes_context *context=(librdf_storage_hashes_context*)storage->context;
  librdf_storage_hashes_serialise_stream_context *scontext;
  librdf_hash *hash;
  librdf_stream *stream;
  
  scontext=(librdf_storage_hashes_serialise_stream_context*)LIBRDF_CALLOC(librdf_storage_hashes_serialise_stream_context, 1, sizeof(librdf_storage_hashes_serialise_stream_context));
  if(!scontext)
    return NULL;

  scontext->hash_context=context;

  librdf_statement_init(storage->world, &scontext->current);

  hash=context->hashes[scontext->index];

  scontext->key=librdf_new_hash_datum(storage->world, NULL, 0);
  if(!scontext->key)
    return NULL;
  
  scontext->value=librdf_new_hash_datum(storage->world, NULL, 0);
  if(!scontext->value) {
    librdf_free_hash_datum(scontext->key);
    return NULL;
  }

  /* scurrent->current_is_ok=0; */
  scontext->index_contexts=context->index_contexts;
  
  if(search_node) {
    scontext->search_node=search_node;
    scontext->iterator=librdf_storage_hashes_node_iterator_create(storage,
                                                                  search_node,
                                                                  NULL,
                                                                  hash_index,
                                                                  want);
  } else {
    scontext->iterator=librdf_hash_get_all(hash,
                                           scontext->key, scontext->value);
  }
  if(!scontext->iterator) {
    librdf_storage_hashes_serialise_finished((void*)scontext);
    return librdf_new_empty_stream(storage->world);
  }

  scontext->storage=storage;
  librdf_storage_add_reference(scontext->storage);

  stream=librdf_new_stream(storage->world,
                           (void*)scontext,
                           &librdf_storage_hashes_serialise_end_of_stream,
                           &librdf_storage_hashes_serialise_next_statement,
                           &librdf_storage_hashes_serialise_get_statement,
                           &librdf_storage_hashes_serialise_finished);
  if(!stream) {
    librdf_storage_hashes_serialise_finished((void*)scontext);
    return NULL;
  }
  
  return stream;  

}


static librdf_stream*
librdf_storage_hashes_serialise(librdf_storage* storage)
{
  librdf_storage_hashes_context *context=(librdf_storage_hashes_context*)storage->context;
  return librdf_storage_hashes_serialise_common(storage, 
                                                context->all_statements_hash_index,
                                                NULL, 0);
}


static int
librdf_storage_hashes_serialise_end_of_stream(void* context)
{
  librdf_storage_hashes_serialise_stream_context* scontext=(librdf_storage_hashes_serialise_stream_context*)context;

  return librdf_iterator_end(scontext->iterator);
}


static int
librdf_storage_hashes_serialise_next_statement(void* context)
{
  librdf_storage_hashes_serialise_stream_context* scontext=(librdf_storage_hashes_serialise_stream_context*)context;

  scontext->current_is_ok=0;
  return librdf_iterator_next(scontext->iterator);
}


static void*
librdf_storage_hashes_serialise_get_statement(void* context, int flags)
{
  librdf_storage_hashes_serialise_stream_context* scontext=(librdf_storage_hashes_serialise_stream_context*)context;
  librdf_hash_datum* hd;
  librdf_node** cnp=NULL;

  if(scontext->search_node) {
    switch(flags) {
      case LIBRDF_ITERATOR_GET_METHOD_GET_OBJECT:
        return librdf_iterator_get_object(scontext->iterator);
      case LIBRDF_ITERATOR_GET_METHOD_GET_CONTEXT:
        return librdf_iterator_get_context(scontext->iterator);
      default:
        librdf_log(scontext->iterator->world,
                   0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
                   "Unimplemented flags %d seen", flags);
        return NULL;
    }
  }
    
  switch(flags) {
    case LIBRDF_ITERATOR_GET_METHOD_GET_OBJECT:
    case LIBRDF_ITERATOR_GET_METHOD_GET_CONTEXT:

      if(scontext->current_is_ok) {
        if(flags==LIBRDF_ITERATOR_GET_METHOD_GET_OBJECT)
          return &scontext->current;
        else
          return scontext->context_node;
      }
      
      /* current stuff is out of date - get new cached answers */
      if(scontext->index_contexts) {
        if(scontext->context_node)
          librdf_free_node(scontext->context_node);
        scontext->context_node=NULL;
        cnp=&scontext->context_node;
      }
      
      librdf_statement_clear(&scontext->current);
      
      hd=(librdf_hash_datum*)librdf_iterator_get_key(scontext->iterator);
      
      /* decode key content */
      if(!librdf_statement_decode_parts(&scontext->current, NULL,
                                        (unsigned char*)hd->data, hd->size)) {
        return NULL;
      }
      
      hd=(librdf_hash_datum*)librdf_iterator_get_value(scontext->iterator);
      
      /* decode value content and optional context */
      if(!librdf_statement_decode_parts(&scontext->current, cnp,
                                        (unsigned char*)hd->data, hd->size)) {
        return NULL;
      }

      scontext->current_is_ok=1;

      if(flags==LIBRDF_ITERATOR_GET_METHOD_GET_OBJECT)
        return &scontext->current;
      else
        return scontext->context_node;
        
    default:
      librdf_log(scontext->iterator->world,
                 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
                 "Unimplemented flags %d seen", flags);
      return NULL;
  }
}


static void
librdf_storage_hashes_serialise_finished(void* context)
{
  librdf_storage_hashes_serialise_stream_context* scontext=(librdf_storage_hashes_serialise_stream_context*)context;

  if(scontext->iterator)
    librdf_free_iterator(scontext->iterator);

  if(scontext->context_node)
    librdf_free_node(scontext->context_node);
      
  if(scontext->key) {
    scontext->key->data=NULL;
    librdf_free_hash_datum(scontext->key);
  }
  if(scontext->value) {
    scontext->value->data=NULL;
    librdf_free_hash_datum(scontext->value);
  }

  librdf_statement_clear(&scontext->current);

  if(scontext->storage)
    librdf_storage_remove_reference(scontext->storage);

  LIBRDF_FREE(librdf_storage_hashes_serialise_stream_context, scontext);
}


/**
 * librdf_storage_hashes_find_statements:
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
librdf_storage_hashes_find_statements(librdf_storage* storage, librdf_statement* statement)
{
  librdf_storage_hashes_context* context=(librdf_storage_hashes_context*)storage->context;
  librdf_stream* stream;

  if(!librdf_statement_get_subject(statement) &&
     librdf_statement_get_predicate(statement) &&
     !librdf_statement_get_object(statement) &&
     context->p2so_index >= 0) {
    /* (? p ?) -> (s p o) wanted */
    stream=librdf_storage_hashes_serialise_common(storage,
                                                  context->p2so_index,
                                                  librdf_statement_get_predicate(statement),
                                                  LIBRDF_STATEMENT_SUBJECT|LIBRDF_STATEMENT_OBJECT);
  } else {
    statement=librdf_new_statement_from_statement(statement);
    if(!statement)
      return NULL;

    stream=librdf_storage_hashes_serialise(storage);
    if(stream)
      librdf_stream_add_map(stream, 
                            &librdf_stream_statement_find_map,
                            (librdf_stream_map_free_context_handler)&librdf_free_statement, (void*)statement);
  }
  
  return stream;
}


typedef struct {
  librdf_storage* storage;   /* (shared) pointer to storage */
  int hash_index;            /* index of hash in storage list of hashes */
  librdf_iterator* iterator; /* owned iterator over above hash */
  int want;                  /* part of decoded statement to return */
  librdf_statement statement; /* NOTE: stored here, never allocated */
  librdf_statement statement2; /* NOTE: stored here, never allocated */
  librdf_hash_datum key;
  librdf_hash_datum value;
  librdf_node *search_node;
  int index_contexts;
  librdf_node *context_node;
} librdf_storage_hashes_node_iterator_context;


static int
librdf_storage_hashes_node_iterator_is_end(void* iterator)
{
  librdf_storage_hashes_node_iterator_context* context=(librdf_storage_hashes_node_iterator_context*)iterator;

  return librdf_iterator_end(context->iterator);
}


static int
librdf_storage_hashes_node_iterator_next_method(void* iterator) 
{
  librdf_storage_hashes_node_iterator_context* context=(librdf_storage_hashes_node_iterator_context*)iterator;

  if(librdf_iterator_end(context->iterator))
    return 1;

  return librdf_iterator_next(context->iterator);
}


static void*
librdf_storage_hashes_node_iterator_get_method(void* iterator, int flags) 
{
  librdf_storage_hashes_node_iterator_context* context=(librdf_storage_hashes_node_iterator_context*)iterator;
  librdf_node* node;
  librdf_hash_datum* value;
  
  if(librdf_iterator_end(context->iterator))
    return NULL;

  if(flags == LIBRDF_ITERATOR_GET_METHOD_GET_CONTEXT) {
    /* current stuff is out of date - get new cached answers */
    if(!context->index_contexts)
      return NULL;

    value=(librdf_hash_datum*)librdf_iterator_get_value(context->iterator);
      
    if(context->context_node)
      librdf_free_node(context->context_node);
    context->context_node=NULL;
      
    /* decode value content and optional context */
    if(!librdf_statement_decode_parts(&context->statement,
                                      &context->context_node,
                                      (unsigned char*)value->data, value->size))
      return NULL;
    librdf_statement_clear(&context->statement);
    
    return context->context_node;
  }
  
  
  if(flags != LIBRDF_ITERATOR_GET_METHOD_GET_OBJECT) {
    librdf_log(context->iterator->world,
               0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
               "Unimplemented iterator method %d", flags);
    return NULL;
  }


  /* get object */
  switch(context->want) {
    case LIBRDF_STATEMENT_SUBJECT: /* SOURCES (subjects) */
      if((node=librdf_statement_get_subject(&context->statement)))
         librdf_free_node(node);
      break;
      
    case LIBRDF_STATEMENT_PREDICATE: /* ARCS (predicates) */
      if((node=librdf_statement_get_predicate(&context->statement)))
         librdf_free_node(node);
      break;
      
    case LIBRDF_STATEMENT_OBJECT: /* TARGETS (objects) */
      if((node=librdf_statement_get_object(&context->statement)))
         librdf_free_node(node);
      break;
      
    case (LIBRDF_STATEMENT_SUBJECT|LIBRDF_STATEMENT_OBJECT): /* p2so */
      if((node=librdf_statement_get_subject(&context->statement)))
         librdf_free_node(node);
      if((node=librdf_statement_get_object(&context->statement)))
         librdf_free_node(node);
      break;
      
    default: /* error */
      librdf_log(context->iterator->world,
                 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
                 "Illegal statement part %d seen", context->want);
      return NULL;
  }


  value=(librdf_hash_datum*)librdf_iterator_get_value(context->iterator);
  if(!value)
    return NULL;

  if(!librdf_statement_decode(&context->statement, 
                              (unsigned char*)value->data,
                              value->size))
    return NULL;

  switch(context->want) {
    case LIBRDF_STATEMENT_SUBJECT: /* SOURCES (subjects) */
      node=librdf_statement_get_subject(&context->statement);
      break;
      
    case LIBRDF_STATEMENT_PREDICATE: /* ARCS (predicates) */
      node=librdf_statement_get_predicate(&context->statement);
      break;
      
    case LIBRDF_STATEMENT_OBJECT: /* TARGETS (objects) */
      node=librdf_statement_get_object(&context->statement);
      break;
      
    case (LIBRDF_STATEMENT_SUBJECT|LIBRDF_STATEMENT_OBJECT): /* p2so */
      librdf_statement_set_subject(&context->statement2, librdf_statement_get_subject(&context->statement));
      /* fill in the only blank from the node stored in our context */
      node=librdf_new_node_from_node(context->search_node);
      if(!node)
        return NULL;
      librdf_statement_set_predicate(&context->statement2, node);
      librdf_statement_set_object(&context->statement2, librdf_statement_get_object(&context->statement));
      return (void*)&context->statement2;
      break;
      
    default: /* error */
      librdf_log(context->iterator->world,
                 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
                 "Illegal statement part %d seen", context->want);
      return NULL;
  }
  
  return (void*)node;
}


static void
librdf_storage_hashes_node_iterator_finished(void* iterator) 
{
  librdf_storage_hashes_node_iterator_context* icontext=(librdf_storage_hashes_node_iterator_context*)iterator;
  librdf_node* node;
  
  if(icontext->search_node)
    librdf_free_node(icontext->search_node);

  if(icontext->context_node)
    librdf_free_node(icontext->context_node);

  if(icontext->iterator)
    librdf_free_iterator(icontext->iterator);

  librdf_statement_clear(&icontext->statement);
  if((node=librdf_statement_get_predicate(&icontext->statement2)))
     librdf_free_node(node);

  if(icontext->storage)
    librdf_storage_remove_reference(icontext->storage);
  
  LIBRDF_FREE(librdf_storage_hashes_node_iterator_context, icontext);
}


/*
 * librdf_storage_hashes_node_iterator_create - Create a node iterator for get sources, targets or arcs methods
 * @storage: the storage hashes object to iterate
 * @node1: the first node to encode in the key
 * @node2: the second node to encode in the key
 * @hash_index: the index of the hash to iterate over
 * @want: the field required from the hash value
 * 
 * Return value: a new #librdf_iterator or NULL on failure
 **/
static librdf_iterator*
librdf_storage_hashes_node_iterator_create(librdf_storage* storage, 
                                           librdf_node* node1,
                                           librdf_node *node2,
                                           int hash_index,
                                           int want) 
{
  librdf_storage_hashes_context *scontext=(librdf_storage_hashes_context*)storage->context;
  librdf_storage_hashes_node_iterator_context* icontext;
  librdf_hash *hash;
  librdf_statement_part fields;
  unsigned char *key_buffer;
  librdf_iterator* iterator;
  
  icontext=(librdf_storage_hashes_node_iterator_context*)LIBRDF_CALLOC(librdf_storage_hashes_node_iterator_context, 1, sizeof(librdf_storage_hashes_node_iterator_context));
  if(!icontext)
    return NULL;

  icontext->storage=storage;

  icontext->hash_index=hash_index;
  icontext->want=want;

  icontext->index_contexts=scontext->index_contexts;

  node1=librdf_new_node_from_node(node1);
  if(!node1) {
    LIBRDF_FREE(librdf_storage_hashes_node_iterator_context, icontext);
    return NULL;
  }
  if(node2) {
    node2=librdf_new_node_from_node(node2);
    if(!node2) {
      librdf_free_node(node2);
      LIBRDF_FREE(librdf_storage_hashes_node_iterator_context, icontext);
      return NULL;
    }
  }

  librdf_statement_init(storage->world, &icontext->statement);
  librdf_statement_init(storage->world, &icontext->statement2);

  hash=scontext->hashes[icontext->hash_index];

  /* set the fields in the static statement contained in the context */
  switch(icontext->want) {
    case LIBRDF_STATEMENT_SUBJECT: /* SOURCES (subjects) */
      librdf_statement_set_predicate(&icontext->statement, node1);
      librdf_statement_set_object(&icontext->statement, node2);
      break;
      
    case LIBRDF_STATEMENT_PREDICATE: /* PREDICATES (arcs) */
      librdf_statement_set_subject(&icontext->statement, node1);
      librdf_statement_set_object(&icontext->statement, node2);
      break;
      
    case LIBRDF_STATEMENT_OBJECT: /* OBJECTS (targets) */
      librdf_statement_set_subject(&icontext->statement, node1);
      librdf_statement_set_predicate(&icontext->statement, node2);
      break;
      
    case (LIBRDF_STATEMENT_SUBJECT|LIBRDF_STATEMENT_OBJECT): /* p2so */
      icontext->search_node=librdf_new_node_from_node(node1);
      librdf_statement_set_predicate(&icontext->statement, node1);
      break;
      
    default: /* error */
      LIBRDF_FREE(librdf_storage_hashes_node_iterator_context, icontext);
      librdf_log(storage->world,
                 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
                 "Illegal statement part %d seen", want);
      return NULL;
  }


  /* ENCODE KEY */
  fields=(librdf_statement_part)scontext->hash_descriptions[hash_index]->key_fields;
  icontext->key.size=librdf_statement_encode_parts(&icontext->statement, NULL,
                                                   NULL, 0, fields);
  if(!icontext->key.size) {
    LIBRDF_FREE(librdf_storage_hashes_node_iterator_context, icontext);
    return NULL;
  }
  if(!(key_buffer=(unsigned char*)LIBRDF_MALLOC(data, icontext->key.size))) {
    LIBRDF_FREE(librdf_storage_hashes_node_iterator_context, icontext);
    return NULL;
  }
       
  /* after this point the finished method is called on errors
   * so must bump the reference count
   */
  librdf_storage_add_reference(icontext->storage);

  if(!librdf_statement_encode_parts(&icontext->statement, NULL,
                                    key_buffer, icontext->key.size, fields)) {
    LIBRDF_FREE(data, key_buffer);
    librdf_storage_hashes_node_iterator_finished(icontext);
    return NULL;
  }

    
  icontext->key.data=key_buffer;

  icontext->iterator=librdf_hash_get_all(hash, &icontext->key, &icontext->value);
  if(!icontext->iterator) {
    LIBRDF_FREE(data, key_buffer);
    librdf_storage_hashes_node_iterator_finished(icontext);
    return librdf_new_empty_iterator(storage->world);
  }

  LIBRDF_FREE(data, key_buffer);

  iterator=librdf_new_iterator(storage->world,
                               (void*)icontext,
                               librdf_storage_hashes_node_iterator_is_end,
                               librdf_storage_hashes_node_iterator_next_method,
                               librdf_storage_hashes_node_iterator_get_method,
                               librdf_storage_hashes_node_iterator_finished);
  if(!iterator)
    librdf_storage_hashes_node_iterator_finished(icontext);
  return iterator;
}


static librdf_iterator*
librdf_storage_hashes_find_sources(librdf_storage* storage, 
                                   librdf_node* arc, librdf_node *target) 
{
  librdf_storage_hashes_context *scontext=(librdf_storage_hashes_context*)storage->context;
  return librdf_storage_hashes_node_iterator_create(storage, arc, target,
                                                    scontext->sources_index,
                                                    LIBRDF_STATEMENT_SUBJECT);
}


static librdf_iterator*
librdf_storage_hashes_find_arcs(librdf_storage* storage,
                                librdf_node* source, librdf_node *target) 
{
  librdf_storage_hashes_context *scontext=(librdf_storage_hashes_context*)storage->context;
  return librdf_storage_hashes_node_iterator_create(storage, source, target,
                                                    scontext->arcs_index,
                                                    LIBRDF_STATEMENT_PREDICATE);
}

static librdf_iterator*
librdf_storage_hashes_find_targets(librdf_storage* storage,
                                   librdf_node* source, librdf_node *arc) 
{
  librdf_storage_hashes_context *scontext=(librdf_storage_hashes_context*)storage->context;
  return librdf_storage_hashes_node_iterator_create(storage, source, arc,
                                                    scontext->targets_index,
                                                    LIBRDF_STATEMENT_OBJECT);
}

/**
 * librdf_storage_hashes_context_add_statement:
 * @storage: #librdf_storage object
 * @context_node: #librdf_node object
 * @statement: #librdf_statement statement to add
 *
 * Add a statement to a storage context.
 * 
 * Return value: non 0 on failure
 **/
static int
librdf_storage_hashes_context_add_statement(librdf_storage* storage,
                                            librdf_node* context_node,
                                            librdf_statement* statement) 
{
  librdf_storage_hashes_context* context=(librdf_storage_hashes_context*)storage->context;
  librdf_hash_datum key, value; /* on stack - not allocated */
  int size;
  int status;
  
  if(context->contexts_index <0) {
    librdf_log(storage->world, 0, LIBRDF_LOG_WARN, LIBRDF_FROM_STORAGE, NULL,
               "Storage was created without context support");
    return 1;
  }
  
  if(librdf_storage_hashes_add_remove_statement(storage, 
                                                statement, context_node, 1))
    return 1;

  size=librdf_node_encode(context_node, NULL, 0);
  key.data=(char*)LIBRDF_MALLOC(cstring, size);
  key.size=librdf_node_encode(context_node, 
                               (unsigned char*)key.data, size);

  size=librdf_statement_encode(statement, NULL, 0);

  value.data=(char*)LIBRDF_MALLOC(cstring, size);
  value.size=librdf_statement_encode(statement, (unsigned char*)value.data, size);

  status=librdf_hash_put(context->hashes[context->contexts_index], &key, &value);
  LIBRDF_FREE(data, key.data);
  LIBRDF_FREE(data, value.data);

  return status;
}


/**
 * librdf_storage_hashes_context_remove_statement:
 * @storage: #librdf_storage object
 * @context_node: #librdf_node object
 * @statement: #librdf_statement statement to remove
 *
 * Remove a statement from a storage context.
 * 
 * Return value: non 0 on failure
 **/
static int
librdf_storage_hashes_context_remove_statement(librdf_storage* storage, 
                                               librdf_node* context_node,
                                               librdf_statement* statement) 
{
  librdf_storage_hashes_context* context=(librdf_storage_hashes_context*)storage->context;
  librdf_hash_datum key, value; /* on stack - not allocated */
  int size;
  int status;
  
  if(context_node && context->contexts_index <0) {
    librdf_log(storage->world, 0, LIBRDF_LOG_WARN, LIBRDF_FROM_STORAGE, NULL,
               "Storage was created without context support");
  }
  
  if(librdf_storage_hashes_add_remove_statement(storage, 
                                                statement, context_node, 0))
    return 1;
  
  size=librdf_node_encode(context_node, NULL, 0);
  key.data=(char*)LIBRDF_MALLOC(cstring, size);
  key.size=librdf_node_encode(context_node,
                               (unsigned char*)key.data, size);

  size=librdf_statement_encode(statement, NULL, 0);

  value.data=(char*)LIBRDF_MALLOC(cstring, size);
  value.size=librdf_statement_encode(statement, (unsigned char*)value.data, size);

  status=librdf_hash_delete(context->hashes[context->contexts_index], &key, &value);
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
  int index_contexts; /* true if this storage indexes contexts */
  librdf_node *context_node;
  char *context_node_data;
  int current_is_ok; /* true when current statement and context_node fresh */
} librdf_storage_hashes_context_serialise_stream_context;


/**
 * librdf_storage_hashes_context_serialise:
 * @storage: #librdf_storage object
 * @context_node: #librdf_node object
 *
 * List all statements in a storage context.
 * 
 * Return value: #librdf_stream of statements or NULL on failure or context is empty
 **/
static librdf_stream*
librdf_storage_hashes_context_serialise(librdf_storage* storage,
                                        librdf_node* context_node) 
{
  librdf_storage_hashes_context* context=(librdf_storage_hashes_context*)storage->context;
  librdf_storage_hashes_context_serialise_stream_context* scontext;
  librdf_stream* stream;
  size_t size;

  if(context->contexts_index <0) {
    librdf_log(storage->world, 0, LIBRDF_LOG_WARN, LIBRDF_FROM_STORAGE, NULL,
               "Storage was created without context support");
    return NULL;
  }
  
  scontext=(librdf_storage_hashes_context_serialise_stream_context*)LIBRDF_CALLOC(librdf_storage_hashes_context_serialise_stream_context, 1, sizeof(librdf_storage_hashes_context_serialise_stream_context));
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

  /* scurrent->current_is_ok=0; */
  scontext->index_contexts=context->index_contexts;
  scontext->context_node=librdf_new_node_from_node(context_node);

  size=librdf_node_encode(context_node, NULL, 0);
  scontext->key->data=scontext->context_node_data=(char*)LIBRDF_MALLOC(cstring, size);
  scontext->key->size=librdf_node_encode(context_node,
                                         (unsigned char*)scontext->key->data,
                                         size);

  scontext->iterator=librdf_hash_get_all(context->hashes[context->contexts_index], 
                                         scontext->key, scontext->value);
  if(!scontext->iterator)
    return librdf_new_empty_stream(storage->world);


  scontext->storage=storage;
  librdf_storage_add_reference(scontext->storage);

  stream=librdf_new_stream(storage->world,
                           (void*)scontext,
                           &librdf_storage_hashes_context_serialise_end_of_stream,
                           &librdf_storage_hashes_context_serialise_next_statement,
                           &librdf_storage_hashes_context_serialise_get_statement,
                           &librdf_storage_hashes_context_serialise_finished);
  if(!stream) {
    librdf_storage_hashes_context_serialise_finished((void*)scontext);
    return NULL;
  }
  
  return stream;  
}


static int
librdf_storage_hashes_context_serialise_end_of_stream(void* context)
{
  librdf_storage_hashes_context_serialise_stream_context* scontext=(librdf_storage_hashes_context_serialise_stream_context*)context;

  return librdf_iterator_end(scontext->iterator);
}


static int
librdf_storage_hashes_context_serialise_next_statement(void* context)
{
  librdf_storage_hashes_context_serialise_stream_context* scontext=(librdf_storage_hashes_context_serialise_stream_context*)context;

  scontext->current_is_ok=0;
  return librdf_iterator_next(scontext->iterator);
}


static void*
librdf_storage_hashes_context_serialise_get_statement(void* context, int flags)
{
  librdf_storage_hashes_context_serialise_stream_context* scontext=(librdf_storage_hashes_context_serialise_stream_context*)context;
  librdf_hash_datum* v;
  librdf_node** cnp=NULL;

  switch(flags) {
    case LIBRDF_ITERATOR_GET_METHOD_GET_OBJECT:
    case LIBRDF_ITERATOR_GET_METHOD_GET_CONTEXT:

      if(scontext->current_is_ok) {
        if(flags==LIBRDF_ITERATOR_GET_METHOD_GET_OBJECT)
          return &scontext->current;
        else
          return scontext->context_node;
      }

      /* current stuff is out of date - get new cached answers */
      if(scontext->index_contexts) {
        if(scontext->context_node)
          librdf_free_node(scontext->context_node);
        scontext->context_node=NULL;
        cnp=&scontext->context_node;
      }
      
      librdf_statement_clear(&scontext->current);

      v=(librdf_hash_datum*)librdf_iterator_get_value(scontext->iterator);
      
      /* decode value content and optional context */
      if(!librdf_statement_decode_parts(&scontext->current, cnp,
                                        (unsigned char*)v->data, v->size)) {
        return NULL;
      }
      
      scontext->current_is_ok=1;

      if(flags==LIBRDF_ITERATOR_GET_METHOD_GET_OBJECT)
        return &scontext->current;
      else
        return scontext->context_node;

    default:
      librdf_log(scontext->iterator->world,
                 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
                 "Unimplemented flags %d seen", flags);
      return NULL;
  }

}


static void
librdf_storage_hashes_context_serialise_finished(void* context)
{
  librdf_storage_hashes_context_serialise_stream_context* scontext=(librdf_storage_hashes_context_serialise_stream_context*)context;
  
  if(scontext->context_node)
    librdf_free_node(scontext->context_node);
  
  if(scontext->iterator)
    librdf_free_iterator(scontext->iterator);

  if(scontext->key) {
    librdf_free_hash_datum(scontext->key);
  }
  if(scontext->value) {
    scontext->value->data=NULL;
    librdf_free_hash_datum(scontext->value);
  }

  librdf_statement_clear(&scontext->current);

  if(scontext->context_node_data)
    LIBRDF_FREE(cstring, scontext->context_node_data);

  if(scontext->storage)
    librdf_storage_remove_reference(scontext->storage);
  
  LIBRDF_FREE(librdf_storage_hashes_context_serialise_stream_context, scontext);
}


static int
librdf_storage_hashes_sync(librdf_storage *storage)
{
  librdf_storage_hashes_context* context=(librdf_storage_hashes_context*)storage->context;
  int i;
  
  for(i=0; i<context->hash_count; i++)
    librdf_hash_sync(context->hashes[i]);
  return 0;
}


typedef struct {
  librdf_storage *storage;
  librdf_iterator *iterator;
  librdf_hash_datum *key;
  librdf_node *current;
} librdf_storage_hashes_get_contexts_iterator_context;



static int
librdf_storage_hashes_get_contexts_is_end(void* iterator)
{
  librdf_storage_hashes_get_contexts_iterator_context* icontext=(librdf_storage_hashes_get_contexts_iterator_context*)iterator;

  return librdf_iterator_end(icontext->iterator);
}


static int
librdf_storage_hashes_get_contexts_next_method(void* iterator) 
{
  librdf_storage_hashes_get_contexts_iterator_context* icontext=(librdf_storage_hashes_get_contexts_iterator_context*)iterator;

  return librdf_iterator_next(icontext->iterator);
}


static void*
librdf_storage_hashes_get_contexts_get_method(void* iterator, int flags) 
{
  librdf_storage_hashes_get_contexts_iterator_context* icontext=(librdf_storage_hashes_get_contexts_iterator_context*)iterator;
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
librdf_storage_hashes_get_contexts_finished(void* iterator) 
{
  librdf_storage_hashes_get_contexts_iterator_context* icontext=(librdf_storage_hashes_get_contexts_iterator_context*)iterator;

  if(icontext->iterator)
    librdf_free_iterator(icontext->iterator);

  librdf_free_hash_datum(icontext->key);
  
  if(icontext->current)
    librdf_free_node(icontext->current);

  if(icontext->storage)
    librdf_storage_remove_reference(icontext->storage);
  
  LIBRDF_FREE(librdf_storage_hashes_get_contexts_iterator_context, icontext);
}


/**
 * librdf_storage_hashes_context_get_contexts:
 * @storage: #librdf_storage object
 *
 * List all context nodes in a storage.
 * 
 * Return value: #librdf_iterator of context_nodes or NULL on failure or no contexts
 **/
static librdf_iterator*
librdf_storage_hashes_get_contexts(librdf_storage* storage) 
{
  librdf_storage_hashes_context* context=(librdf_storage_hashes_context*)storage->context;
  librdf_storage_hashes_get_contexts_iterator_context* icontext;
  librdf_iterator* iterator;

  if(context->index_contexts <0) {
    librdf_log(storage->world, 0, LIBRDF_LOG_WARN, LIBRDF_FROM_STORAGE, NULL,
               "Storage was created without context support");
    return NULL;
  }
  
  icontext=(librdf_storage_hashes_get_contexts_iterator_context*)LIBRDF_CALLOC(librdf_storage_hashes_get_contexts_iterator_context, 1, sizeof(librdf_storage_hashes_get_contexts_iterator_context));
  if(!icontext)
    return NULL;

  icontext->key=librdf_new_hash_datum(storage->world, NULL, 0);
  if(!icontext->key)
    return NULL;
  
  icontext->iterator=librdf_hash_keys(context->hashes[context->contexts_index],
                                      icontext->key);
  if(!icontext->iterator) {
    librdf_storage_hashes_get_contexts_finished(icontext);
    return NULL;
  }


  icontext->storage=storage;
  librdf_storage_add_reference(icontext->storage);

  iterator=librdf_new_iterator(storage->world,
                               (void*)icontext,
                               &librdf_storage_hashes_get_contexts_is_end,
                               &librdf_storage_hashes_get_contexts_next_method,
                               &librdf_storage_hashes_get_contexts_get_method,
                               &librdf_storage_hashes_get_contexts_finished);
  if(!iterator)
    librdf_storage_hashes_get_contexts_finished(icontext);
  return iterator;
}



/**
 * librdf_storage_hashes_get_feature:
 * @storage: #librdf_storage object
 * @feature: #librdf_uri feature property
 *
 * Get the value of a storage feature.
 * 
 * Return value: new #librdf_node feature value or NULL if no such feature
 * exists or the value is empty.
 **/
static librdf_node*
librdf_storage_hashes_get_feature(librdf_storage* storage, librdf_uri* feature)
{
  librdf_storage_hashes_context* scontext=(librdf_storage_hashes_context*)storage->context;
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


/* local function to register hashes storage functions */

static void
librdf_storage_hashes_register_factory(librdf_storage_factory *factory) 
{
  factory->context_length     = sizeof(librdf_storage_hashes_context);
  
  factory->init               = librdf_storage_hashes_init;
  factory->clone              = librdf_storage_hashes_clone;
  factory->terminate          = librdf_storage_hashes_terminate;
  factory->open               = librdf_storage_hashes_open;
  factory->close              = librdf_storage_hashes_close;
  factory->size               = librdf_storage_hashes_size;
  factory->add_statement      = librdf_storage_hashes_add_statement;
  factory->add_statements     = librdf_storage_hashes_add_statements;
  factory->remove_statement   = librdf_storage_hashes_remove_statement;
  factory->contains_statement = librdf_storage_hashes_contains_statement;
  factory->serialise          = librdf_storage_hashes_serialise;

  factory->find_statements    = librdf_storage_hashes_find_statements;
  factory->find_sources       = librdf_storage_hashes_find_sources;
  factory->find_arcs          = librdf_storage_hashes_find_arcs;
  factory->find_targets       = librdf_storage_hashes_find_targets;

  factory->context_add_statement    = librdf_storage_hashes_context_add_statement;
  factory->context_remove_statement = librdf_storage_hashes_context_remove_statement;
  factory->context_serialise        = librdf_storage_hashes_context_serialise;
  factory->sync                     = librdf_storage_hashes_sync;
  factory->get_contexts             = librdf_storage_hashes_get_contexts;
  factory->get_feature              = librdf_storage_hashes_get_feature;
}


/**
 * librdf_init_storage_hashes:
 * @world: world object
 * 
 * INTERNAL - initialise the storage_hashes module.
 **/
void
librdf_init_storage_hashes(librdf_world *world)
{
  librdf_storage_register_factory(world, "hashes", "Indexed hashes",
                                  &librdf_storage_hashes_register_factory);
}
