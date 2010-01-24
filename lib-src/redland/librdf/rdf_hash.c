/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_hash.c - RDF Hash interface - set of (key: value) pairs with dups
 *
 * Copyright (C) 2000-2008, David Beckett http://www.dajobe.org/
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
#include <rdf_config.h>
#endif

#ifdef WIN32
#include <win32_rdf_config.h>
#endif

#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>
#include <sys/types.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h> /* for strtol */
#endif

#include <redland.h>

#include <rdf_hash.h>
#include <rdf_heuristics.h>

#ifndef STANDALONE

/* prototypes for helper functions */
static void librdf_delete_hash_factories(librdf_world *world);

static void librdf_init_hash_datums(librdf_world *world);
static void librdf_free_hash_datums(librdf_world *world);


/* prototypes for iterator for getting all keys and values */
static int librdf_hash_get_all_iterator_is_end(void* iterator);
static int librdf_hash_get_all_iterator_next_method(void* iterator);
static void* librdf_hash_get_all_iterator_get_method(void* iterator, int);
static void librdf_hash_get_all_iterator_finished(void* iterator);

/* prototypes for iterator for getting all keys */
static int librdf_hash_keys_iterator_is_end(void* iterator);
static int librdf_hash_keys_iterator_next_method(void* iterator);
static void* librdf_hash_keys_iterator_get_method(void* iterator, int);
static void librdf_hash_keys_iterator_finished(void* iterator);




/**
 * librdf_init_hash:
 *
 * INTERNAL - Initialise the hash module.
 *
 * Initialises and registers all
 * compiled hash modules.  Must be called before using any of the hash
 * factory functions such as librdf_get_hash_factory()
 * @world: redland world object
 **/
void
librdf_init_hash(librdf_world *world) 
{
  /* Init hash datum cache */
  librdf_init_hash_datums(world);
#ifdef HAVE_BDB_HASH
  librdf_init_hash_bdb(world);
#endif
  /* Always have hash in memory implementation available */
  librdf_init_hash_memory(world);
}


/**
 * librdf_finish_hash:
 * @world: redland world object
 *
 * INTERNAL - Terminate the hash module.
 *
 **/
void
librdf_finish_hash(librdf_world *world) 
{
  librdf_delete_hash_factories(world);
  librdf_free_hash_datums(world);
}



/* helper functions */
static void
librdf_delete_hash_factories(librdf_world *world)
{
  librdf_hash_factory *factory, *next;
  
  for(factory=world->hashes; factory; factory=next) {
    next=factory->next;
    LIBRDF_FREE(librdf_hash_factory, factory->name);
    LIBRDF_FREE(librdf_hash_factory, factory);
  }
  world->hashes=NULL;
  
}



/* hash datums structures */

static void
librdf_init_hash_datums(librdf_world *world)
{
  world->hash_datums_list=NULL;
}


static void
librdf_free_hash_datums(librdf_world *world)
{
  librdf_hash_datum *datum, *next;
  
  for(datum=world->hash_datums_list; datum; datum=next) {
    next=datum->next;
    LIBRDF_FREE(librdf_hash_datum, datum);
  }
  world->hash_datums_list=NULL;
}


/**
 * librdf_new_hash_datum:
 * @world: redland world object
 * @data: data to store
 * @size: size of data
 *
 * Constructor - Create a new #librdf_hash_datum object.
 * 
 * Return value: New #librdf_hash_datum object or NULL on failure
 **/
librdf_hash_datum*
librdf_new_hash_datum(librdf_world *world, void *data, size_t size)
{
  librdf_hash_datum *datum;

  librdf_world_open(world);

  /* get one from free list, or allocate new one */ 
  if((datum=world->hash_datums_list)) {
    world->hash_datums_list=datum->next;
  } else {
    datum=(librdf_hash_datum*)LIBRDF_CALLOC(librdf_hash_datum, 1, sizeof(librdf_hash_datum));
    if(datum)
      datum->world=world;
  }
  
  if(datum) {
    datum->data=data;
    datum->size=size;
  }

  return datum;
}


/**
 * librdf_free_hash_datum:
 * @datum: hash datum object
 *
 * Destructor - destroy a #librdf_hash_datum object.
 *
 **/
void
librdf_free_hash_datum(librdf_hash_datum *datum) 
{
  if(datum->data)
    LIBRDF_FREE(cstring, datum->data);
  datum->next=datum->world->hash_datums_list;
  datum->world->hash_datums_list=datum;
}


/* class methods */

/**
 * librdf_hash_register_factory:
 * @world: redland world object
 * @name: the hash factory name
 * @factory: pointer to function to call to register the factory
 *
 * Register a hash factory.
 * 
 **/
void
librdf_hash_register_factory(librdf_world *world, const char *name,
                             void (*factory) (librdf_hash_factory*)) 
{
  librdf_hash_factory *hash;

  librdf_world_open(world);

#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
  LIBRDF_DEBUG2("Received registration for hash %s\n", name);
#endif
  
  for(hash = world->hashes; hash; hash = hash->next ) {
    if(!strcmp(hash->name, name)) {
      librdf_log(world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_HASH, NULL,
                 "hash %s already registered", hash->name);
      return;
    }
  }

  hash=(librdf_hash_factory*)LIBRDF_CALLOC(librdf_hash_factory, 1,
                                           sizeof(librdf_hash_factory));
  if(!hash)
    goto oom;
  
  hash->name=(char*)LIBRDF_MALLOC(cstring, strlen(name)+1);
  if(!hash->name)
    goto oom_tidy;
  strcpy(hash->name, name);
  
  hash->next = world->hashes;
  world->hashes = hash;
  
  /* Call the hash registration function on the new object */
  (*factory)(hash);
  
#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
  LIBRDF_DEBUG3("%s has context size %d\n", name, hash->context_length);
#endif

  return;

  oom_tidy:
  LIBRDF_FREE(librdf_hash, hash);
  oom:
  LIBRDF_FATAL1(world, LIBRDF_FROM_HASH, "Out of memory");
}


/**
 * librdf_get_hash_factory:
 * @world: redland world object
 * @name: the factory name or NULL for the default factory
 *
 * Get a hash factory by name.
 * 
 * FIXME: several bits of code assume the default hash factory is
 * in memory.
 *
 * Return value: the factory object or NULL if there is no such factory
 **/
librdf_hash_factory*
librdf_get_hash_factory(librdf_world *world, const char *name) 
{
  librdf_hash_factory *factory;

  librdf_world_open(world);

  /* return 1st hash if no particular one wanted - why? */
  if(!name) {
    factory=world->hashes;
    if(!factory) {
      LIBRDF_DEBUG1("No (default) hashes registered\n");
      return NULL;
    }
  } else {
    for(factory=world->hashes; factory; factory=factory->next) {
      if(!strcmp(factory->name, name)) {
	break;
      }
    }
    /* else FACTORY name not found */
    if(!factory)
      return NULL;
  }
  
  return factory;
}



/**
 * librdf_new_hash:
 * @world: redland world object
 * @name: factory name
 *
 * Constructor - create a new #librdf_hash object.
 *
 * Return value: a new #librdf_hash object or NULL on failure
 */
librdf_hash*
librdf_new_hash(librdf_world *world, const char* name)
{
  librdf_hash_factory *factory;

  librdf_world_open(world);

  factory=librdf_get_hash_factory(world, name);
  if(!factory)
    return NULL;

  return librdf_new_hash_from_factory(world, factory);
}


/**
 * librdf_new_hash_from_factory:
 * @world: redland world object
 * @factory: the factory to use to construct the hash
 *
 * Constructor - create a new #librdf_hash object from a factory.
 *
 * Return value: a new #librdf_hash object or NULL on failure
 */
librdf_hash*
librdf_new_hash_from_factory(librdf_world *world,
                             librdf_hash_factory* factory)
{
  librdf_hash* h;

  librdf_world_open(world);

  h=(librdf_hash*)LIBRDF_CALLOC(librdf_hash, sizeof(librdf_hash), 1);
  if(!h)
    return NULL;
  
  h->context=(char*)LIBRDF_CALLOC(librdf_hash_context, 1,
                                  factory->context_length);
  if(!h->context) {
    librdf_free_hash(h);
    return NULL;
  }

  h->world=world;
  
  h->factory=factory;

  /* call factory constructor */
  if(h->factory->create(h, h->context)) {
    librdf_free_hash(h);
    return NULL;
  }
  
  return h;
}


/**
 * librdf_new_hash_from_string:
 * @world: redland world object
 * @name: hash name
 * @string: hash encoded as a string
 * 
 * Constructor - create a new #librdf_hash object from a string.
 *
 * See #librdf_hash_from_string for the string format.
 * 
 * Return value: a new #librdf_hash object or NULL on failure
 */
librdf_hash*
librdf_new_hash_from_string(librdf_world *world, const char *name,
                            const char *string)
{
  librdf_hash* hash;

  librdf_world_open(world);

  hash=librdf_new_hash(world, name);
  if(!hash)
    return NULL;
  
  if(librdf_hash_from_string(hash, string)) {
    librdf_free_hash(hash);
    return NULL;
  }

  return hash;
}


/**
 * librdf_new_hash_from_array_of_strings:
 * @world: redland world object
 * @name: hash name
 * @array: address of the start of the array of char* pointers
 *
 * Constructor - create a new #librdf_hash object from an array of strings.
 *
 * Return value: a new #librdf_hash object or NULL on failure
 */
librdf_hash*
librdf_new_hash_from_array_of_strings(librdf_world *world, const char *name,
                                      const char **array)
{
  librdf_hash* hash;

  librdf_world_open(world);

  hash=librdf_new_hash(world, name);
  if(!hash)
    return NULL;
  
  if(librdf_hash_from_array_of_strings(hash, array)) {
    librdf_free_hash(hash);
    return NULL;
  }

  return hash;
}


/**
 * librdf_new_hash_from_hash:
 * @old_hash: the hash to use to construct the hash
 *
 * Copy Constructor - create a new #librdf_hash object from an existing one.
 *
 * Return value: a new #librdf_hash object or NULL on failure
 */
librdf_hash*
librdf_new_hash_from_hash(librdf_hash* old_hash)
{
  librdf_hash* hash;
  
  hash=(librdf_hash*)LIBRDF_CALLOC(librdf_hash, sizeof(librdf_hash), 1);
  if(!hash)
    return NULL;

  hash->world=old_hash->world;
  hash->factory=old_hash->factory;

  hash->context=(char*)LIBRDF_CALLOC(librdf_hash_context, 1,
                                     hash->factory->context_length);
  if(!hash->context) {
    librdf_free_hash(hash);
    return NULL;
  }

  if(old_hash->identifier) {
    hash->identifier=librdf_heuristic_gen_name(old_hash->identifier);
    if(!hash->identifier) {
      librdf_free_hash(hash);
      return NULL;
    }
  }

  if(hash->factory->clone(hash, hash->context, hash->identifier,
                          old_hash->context)) {
    if(hash->identifier)
      LIBRDF_FREE(cstring, hash->identifier);
    librdf_free_hash(hash);
    return NULL;
  }

  return hash;
}


/**
 * librdf_free_hash:
 * @hash: hash object
 *
 * Destructor - destroy a #librdf_hash object.
 */
void
librdf_free_hash(librdf_hash* hash) 
{
  if(hash->context) {
    if(hash->is_open)
      librdf_hash_close(hash);
    hash->factory->destroy(hash->context);
    LIBRDF_FREE(librdf_hash_context, hash->context);
  }
  LIBRDF_FREE(librdf_hash, hash);
}


/* methods */

/**
 * librdf_hash_open:
 * @hash: hash object
 * @identifier: indentifier for the hash factory - usually a URI or file name
 * @mode: hash access mode
 * @is_writable: is hash writable?
 * @is_new: is hash new?
 * @options: a hash of options for the hash factory or NULL if there are none.
 *
 * Start a hash association .
 * 
 * This method opens and/or creates a new hash with any resources it
 * needs.
 * 
 * Return value: non 0 on failure
 **/
int
librdf_hash_open(librdf_hash* hash, const char *identifier,
                 int mode, int is_writable, int is_new,
                 librdf_hash* options) 
{
  int status;

  if(identifier) {
    hash->identifier=(char*)LIBRDF_MALLOC(cstring, strlen(identifier)+1);
    if(!hash->identifier)
      return 1;
    strcpy(hash->identifier, identifier);
  }
  status=hash->factory->open(hash->context, identifier, 
                             mode, is_writable, is_new, 
                             options);
  if(!status)
    hash->is_open=1;
  return status;
}


/**
 * librdf_hash_close:
 * @hash: hash object
 *
 * End a hash association.
 * 
 * Return value: non 0 on failure
 **/
int
librdf_hash_close(librdf_hash* hash)
{
  hash->is_open=0;
  if(hash->identifier) {
    LIBRDF_FREE(cstring,hash->identifier);
    hash->identifier=NULL;
  }
  return hash->factory->close(hash->context);
}


/**
 * librdf_hash_values_count:
 * @hash: 
 *
 * Get the number of values in the hash.
 * 
 * Return value: integer number of values in the hash or <0 if cannot be determined
 **/
int
librdf_hash_values_count(librdf_hash* hash) 
{
  return hash->factory->values_count(hash->context);
}


/**
 * librdf_hash_get:
 * @hash: hash object
 * @key: pointer to key
 *
 * Retrieve one value from hash for a given key as string.
 * 
 * The value returned is from newly allocated memory which the
 * caller must free.
 * 
 * Return value: the value or NULL on failure
 **/
char*
librdf_hash_get(librdf_hash* hash, const char *key)
{
  librdf_hash_datum *hd_key, *hd_value;
  char *value=NULL;

  hd_key=librdf_new_hash_datum(hash->world, (void*)key, strlen(key));
  if(!hd_key)
    return NULL;

  hd_value=librdf_hash_get_one(hash, hd_key);

  if(hd_value) {
    if(hd_value->data) {
      value=(char*)LIBRDF_MALLOC(cstring, hd_value->size+1);
      if(value) {
        /* Copy into new null terminated string for userland */
        memcpy(value, hd_value->data, hd_value->size);
        value[hd_value->size]='\0';
      }
    }
    librdf_free_hash_datum(hd_value);
  }

  /* don't free user key */
  hd_key->data=NULL;
  librdf_free_hash_datum(hd_key);

  return value;
}


/**
 * librdf_hash_get_one:
 * @hash: hash object
 * @key: pointer to key
 *
 * Retrieve one value from hash for a given key.
 * 
 * The value returned is from newly allocated memory which the
 * caller must free.
 * 
 * Return value: the value or NULL on failure
 **/
librdf_hash_datum*
librdf_hash_get_one(librdf_hash* hash, librdf_hash_datum *key)
{
  librdf_hash_datum *value;
  librdf_hash_cursor *cursor;
  int status;
  char *new_value;
  
  value=librdf_new_hash_datum(hash->world, NULL, 0);
  if(!value)
    return NULL;

  cursor=librdf_new_hash_cursor(hash);
  if(!cursor) {
    librdf_free_hash_datum(value);
    return NULL;
  }

  status=librdf_hash_cursor_get_next(cursor, key, value);
  if(!status) {
    /* value->data will point to SHARED area, so copy it */
    new_value=(char*)LIBRDF_MALLOC(cstring, value->size);
    if(new_value) {
      memcpy(new_value, value->data, value->size);
      value->data=new_value;
    } else {
      status=1;
      value->data=NULL;
    }
  }

  /* this deletes the data behind the datum */
  librdf_free_hash_cursor(cursor);

  if(status) {
    librdf_free_hash_datum(value);
    return NULL;
  }
  
  return value;
}


typedef struct {
  librdf_hash* hash;
  librdf_hash_cursor* cursor;
  librdf_hash_datum *key;
  librdf_hash_datum *value;

  librdf_hash_datum next_key; /* not used if one_key set */
  librdf_hash_datum next_value;
  int is_end;
  int one_key;
} librdf_hash_get_all_iterator_context;



/**
 * librdf_hash_get_all:
 * @hash: hash object
 * @key: pointer to key
 * @value: pointer to value
 *
 * Retrieve all values from hash for a given key.
 * 
 * The iterator returns #librdf_hash_datum objects containing the values.
 * These are newly allocated memory which the caller must free.
 * 
 * Return value: a #librdf_iterator serialization of all values or NULL on failure
 **/
librdf_iterator*
librdf_hash_get_all(librdf_hash* hash, 
                    librdf_hash_datum *key, librdf_hash_datum *value)
{
  librdf_hash_get_all_iterator_context* context;
  int status;
  librdf_iterator* iterator;
  
  context=(librdf_hash_get_all_iterator_context*)LIBRDF_CALLOC(librdf_hash_get_all_iterator_context, 1, sizeof(librdf_hash_get_all_iterator_context));
  if(!context)
    return NULL;

  if(!(context->cursor=librdf_new_hash_cursor(hash))) {
    librdf_hash_get_all_iterator_finished(context);
    return NULL;
  }

  if(key->data)
    context->one_key=1;

  context->hash=hash;
  context->key=key;
  context->value=value;

  if(context->one_key)
    status=librdf_hash_cursor_set(context->cursor, context->key, 
                                  &context->next_value);
  else
    status=librdf_hash_cursor_get_first(context->cursor, &context->next_key, 
                                        &context->next_value);

  context->is_end=(status != 0);
  
  iterator=librdf_new_iterator(hash->world,
                               (void*)context,
                               librdf_hash_get_all_iterator_is_end,
                               librdf_hash_get_all_iterator_next_method,
                               librdf_hash_get_all_iterator_get_method,
                               librdf_hash_get_all_iterator_finished);
  if(!iterator)
    librdf_hash_get_all_iterator_finished(context);
  return iterator;
}


static int
librdf_hash_get_all_iterator_is_end(void* iterator)
{
  librdf_hash_get_all_iterator_context* context=(librdf_hash_get_all_iterator_context*)iterator;

  return context->is_end;
}


static int
librdf_hash_get_all_iterator_next_method(void* iterator) 
{
  librdf_hash_get_all_iterator_context* context=(librdf_hash_get_all_iterator_context*)iterator;
  int status;
    
  if(context->is_end)
    return 1;
  
  /* move on */
  
  if(context->one_key)
    status=librdf_hash_cursor_get_next_value(context->cursor, 
                                             &context->next_key,
                                             &context->next_value);
  else {
    /* want the next key/value pair, so mark last data as used */
    context->next_key.data=NULL;
    status=librdf_hash_cursor_get_next(context->cursor, 
                                       &context->next_key, 
                                       &context->next_value);
  }
  
  if(status)
    context->is_end=1;

  return context->is_end;
}


static void*
librdf_hash_get_all_iterator_get_method(void* iterator, int flags) 
{
  librdf_hash_get_all_iterator_context* context=(librdf_hash_get_all_iterator_context*)iterator;
  void *result=NULL;
  
  if(context->is_end)
    return NULL;

  switch(flags) {
    case LIBRDF_ITERATOR_GET_METHOD_GET_OBJECT:
      /* This is so that librdf_iterator_update_current_element works OK,
       * since the get_object method isn't used for hashes,
       * might as well return something useful to signify not-end-of-list.
       */

      result=&context;
      break;

    case LIBRDF_ITERATOR_GET_METHOD_GET_KEY:
      result=&context->next_key;
      break;
      
    case LIBRDF_ITERATOR_GET_METHOD_GET_VALUE:
      result=&context->next_value;
      break;

    default:
      librdf_log(context->hash->world, 
                 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_HASH, NULL,
                 "Unknown iterator method flag %d", flags);
      result=NULL;
      break;
  }

  return result;
}


static void
librdf_hash_get_all_iterator_finished(void* iterator) 
{
  librdf_hash_get_all_iterator_context* context=(librdf_hash_get_all_iterator_context*)iterator;

  if(context->cursor)
    librdf_free_hash_cursor(context->cursor);

  if(context->key)
    context->key->data=NULL;

  if(context->value)
    context->value->data=NULL;

  LIBRDF_FREE(librdf_hash_get_all_iterator_context, context);
}


/**
 * librdf_hash_get_del:
 * @hash: hash object
 * @key: pointer to key
 *
 * Retrieve one value from hash for a given key as string and remove all values with that key.
 * 
 * The value returned is from newly allocated memory which the
 * caller must free.
 * 
 * Return value: the value or NULL on failure
 **/
char*
librdf_hash_get_del(librdf_hash* hash, const char *key)
{
  librdf_hash_datum hd_key;
  char *s;
  
  s=librdf_hash_get(hash, key);
  if(!s)
    return NULL;

  hd_key.data=(char*)key;
  hd_key.size=strlen(key);
  
  librdf_hash_delete_all(hash, &hd_key);

  return s;
}



/**
 * librdf_hash_put:
 * @hash: hash object
 * @key: key 
 * @value: value
 *
 * Insert key/value pairs into the hash according to flags.
 * 
 * The key and values are copied into the hash; the original pointers
 * can be deleted.
 * 
 * Return value: non 0 on failure
 **/
int
librdf_hash_put(librdf_hash* hash, librdf_hash_datum *key, 
                librdf_hash_datum *value)
{
  return hash->factory->put(hash->context, key, value);
}


/**
 * librdf_hash_exists:
 * @hash: hash object
 * @key: key
 * @value: value
 *
 * Check if a given key/value is in the hash.
 * 
 * Return value: >0 if the key/value exists in the hash, 0 if not, <0 on failure
 **/
int
librdf_hash_exists(librdf_hash* hash, librdf_hash_datum *key,
                   librdf_hash_datum *value)
{
  return hash->factory->exists(hash->context, key, value);
}


/**
 * librdf_hash_delete:
 * @hash: hash object
 * @key: key
 * @value: value
 *
 * Delete a key/value pair from the hash.
 * 
 * Return value: non 0 on failure (including pair not present)
 **/
int
librdf_hash_delete(librdf_hash* hash, librdf_hash_datum *key,
                   librdf_hash_datum *value)
{
  return hash->factory->delete_key_value(hash->context, key, value);
}


/**
 * librdf_hash_delete_all:
 * @hash: hash object
 * @key: key
 *
 * Delete a key and all values from the hash.
 * 
 * Return value: non 0 on failure (including pair not present)
 **/
int
librdf_hash_delete_all(librdf_hash* hash, librdf_hash_datum *key)
{
  return hash->factory->delete_key(hash->context, key);
}


typedef struct {
  librdf_hash* hash;
  librdf_hash_cursor* cursor;
  librdf_hash_datum *key;

  librdf_hash_datum next_key;
  int is_end;
} librdf_hash_keys_iterator_context;



/**
 * librdf_hash_keys:
 * @hash: hash object
 * @key: pointer to key
 *
 * Get the hash keys.
 * 
 * The iterator returns #librdf_hash_datum objects containingvalue returned is from newly allocated memory which the
 * caller must free.
 * 
 * Return value: #librdf_iterator serialisation of keys or NULL on failure
 **/
librdf_iterator*
librdf_hash_keys(librdf_hash* hash, librdf_hash_datum *key)
{
  librdf_hash_keys_iterator_context* context;
  int status;
  librdf_iterator* iterator;
  
  context=(librdf_hash_keys_iterator_context*)LIBRDF_CALLOC(librdf_hash_keys_iterator_context, 1, sizeof(librdf_hash_keys_iterator_context));
  if(!context)
    return NULL;


  if(!(context->cursor=librdf_new_hash_cursor(hash))) {
    librdf_hash_keys_iterator_finished(context);
    return NULL;
  }

  context->hash=hash;
  context->key=key;
 
  status=librdf_hash_cursor_get_first(context->cursor, &context->next_key, 
                                      NULL);
  context->is_end=(status != 0);
  
  iterator=librdf_new_iterator(hash->world, 
                               (void*)context,
                               librdf_hash_keys_iterator_is_end,
                               librdf_hash_keys_iterator_next_method,
                               librdf_hash_keys_iterator_get_method,
                               librdf_hash_keys_iterator_finished);
  if(!iterator)
    librdf_hash_keys_iterator_finished(context);
  return iterator;
}


static int
librdf_hash_keys_iterator_is_end(void* iterator)
{
  librdf_hash_keys_iterator_context* context=(librdf_hash_keys_iterator_context*)iterator;
  
  if(context->is_end)
    return 1;

  /* have key */
  if(context->next_key.data)
    return 0;
  
  /* no stored data, so check for it */
  if(librdf_hash_cursor_get_next(context->cursor, &context->next_key, NULL))
    context->is_end=1;
  
  return context->is_end;
}


static int
librdf_hash_keys_iterator_next_method(void* iterator) 
{
  librdf_hash_keys_iterator_context* context=(librdf_hash_keys_iterator_context*)iterator;

  if(context->is_end)
    return 1;
  
  /* move on */

  /* want the next key, so mark last key data as used */
  context->next_key.data=NULL;
  if(librdf_hash_cursor_get_next(context->cursor, &context->next_key, NULL))
    context->is_end=1;

  return context->is_end;
}


static void*
librdf_hash_keys_iterator_get_method(void* iterator, int flags) 
{
  librdf_hash_keys_iterator_context* context=(librdf_hash_keys_iterator_context*)iterator;
  void *result=NULL;
  
  if(context->is_end)
    return NULL;

  switch(flags) {
    case LIBRDF_ITERATOR_GET_METHOD_GET_OBJECT:
      /* This is so that librdf_iterator_update_current_element works OK,
       * since the get_object method isn't used for hashes,
       * might as well return something useful to signify not-end-of-list.
       */

      result=&context;
      break;
      
    case LIBRDF_ITERATOR_GET_METHOD_GET_KEY:
      result=&context->next_key;
      break;
      
    default:
      result=NULL;
  }

  return result;
}


static void
librdf_hash_keys_iterator_finished(void* iterator) 
{
  librdf_hash_keys_iterator_context* context=(librdf_hash_keys_iterator_context*)iterator;

  if(context->cursor)
    librdf_free_hash_cursor(context->cursor);

  context->key->data=NULL;

  LIBRDF_FREE(librdf_hash_keys_iterator_context, context);
}


/**
 * librdf_hash_sync:
 * @hash: hash object
 *
 * Flush any cached information to disk if appropriate.
 * 
 * Return value: non 0 on failure
 **/
int
librdf_hash_sync(librdf_hash* hash)
{
  return hash->factory->sync(hash->context);
}


/**
 * librdf_hash_get_fd:
 * @hash: hash object
 *
 * Get the file descriptor for the hash.
 * 
 * This returns the file descriptor if it is file based for
 * use with file locking.
 * 
 * Return value: the file descriptor
 **/
int
librdf_hash_get_fd(librdf_hash* hash)
{
  return hash->factory->get_fd(hash->context);
}


/**
 * librdf_hash_print:
 * @hash: the hash
 * @fh: file handle
 *
 * Pretty print the hash to a file descriptor.
 *
 **/
void
librdf_hash_print(librdf_hash* hash, FILE *fh) 
{
  librdf_iterator* iterator;
  librdf_hash_datum *key, *value;
  
  fputs(hash->factory->name, fh);
  fputs(" hash: {\n", fh);

  key=librdf_new_hash_datum(hash->world, NULL, 0);
  value=librdf_new_hash_datum(hash->world, NULL, 0);

  iterator=librdf_hash_get_all(hash, key, value);
  while(!librdf_iterator_end(iterator)) {
    librdf_hash_datum *k, *v;
    size_t l;
    
    k=(librdf_hash_datum *)librdf_iterator_get_key(iterator);
    v=(librdf_hash_datum *)librdf_iterator_get_value(iterator);
    
    fputs("  '", fh);
    l=fwrite(k->data, k->size, 1, fh);
    if(l != k->size)
      break;
    
    fputs("'=>'", fh);
    l=fwrite(v->data, v->size, 1, fh);
    if(l != v->size)
      break;

    fputs("'\n", fh);

    librdf_iterator_next(iterator);
  }
  if(iterator)
    librdf_free_iterator(iterator);

  librdf_free_hash_datum(value);
  librdf_free_hash_datum(key);

  fputc('}', fh);
}


/**
 * librdf_hash_print_keys:
 * @hash: the hash
 * @fh: file handle
 *
 * Pretty print the keys to a file descriptor.
 *
 **/
void
librdf_hash_print_keys(librdf_hash* hash, FILE *fh) 
{
  librdf_iterator* iterator;
  librdf_hash_datum *key;
  
  fputs("{\n", fh);

  key=librdf_new_hash_datum(hash->world, NULL, 0);

  iterator=librdf_hash_keys(hash, key);
  while(!librdf_iterator_end(iterator)) {
    librdf_hash_datum *k=(librdf_hash_datum *)librdf_iterator_get_key(iterator);
    size_t l;
    
    fputs("  '", fh);
    l=fwrite(k->data, k->size, 1, fh);
    if(l != k->size)
      break;
    fputs("'\n", fh);

    librdf_iterator_next(iterator);
  }
  if(iterator)
    librdf_free_iterator(iterator);

  librdf_free_hash_datum(key);

  fputc('}', fh);
}


/**
 * librdf_hash_print_values:
 * @hash: the hash
 * @key_string: the key as a string
 * @fh: file handle
 *
 * Pretty print the values of one key to a file descriptor.
 *
 **/
void
librdf_hash_print_values(librdf_hash* hash, const char *key_string, FILE *fh)
{
  librdf_hash_datum *key, *value;
  librdf_iterator* iterator;
  int first=1;
  
  key=librdf_new_hash_datum(hash->world, (char*)key_string, strlen(key_string));
  if(!key)
    return;
  
  value=librdf_new_hash_datum(hash->world, NULL, 0);
  if(!value) {
    key->data=NULL;
    librdf_free_hash_datum(key);
    return;
  }
  
  iterator=librdf_hash_get_all(hash, key, value);
  fputc('(', fh);
  while(!librdf_iterator_end(iterator)) {
    librdf_hash_datum *v=(librdf_hash_datum *)librdf_iterator_get_value(iterator);
    size_t l;
    
    if(!first)
      fputs(", ", fh);
      
    fputc('\'', fh);
    l=fwrite(v->data, v->size, 1, fh);
    if(l != v->size)
      break;

    fputc('\'', fh);
    first=0;
    librdf_iterator_next(iterator);
  }
  fputc(')', fh);
  librdf_free_iterator(iterator);

  key->data=NULL;
  librdf_free_hash_datum(key);

  librdf_free_hash_datum(value);
}



/* private enum */
typedef enum {
  HFS_PARSE_STATE_INIT = 0,
  HFS_PARSE_STATE_KEY = 1,
  HFS_PARSE_STATE_SEP = 2,
  HFS_PARSE_STATE_EQ = 3,
  HFS_PARSE_STATE_VALUE = 4
} librdf_hfs_parse_state;


/**
 * librdf_hash_from_string:
 * @hash: hash object
 * @string: hash encoded as a string
 *
 * Initialise a hash from a string.
 * 
 * The string format is something like:
 * key1='value1',key2='value2', key3='\'quoted value\''
 *
 * The 's are required and whitespace can appear around the = and ,s
 * 
 * Return value: non 0 on failure
 **/
int
librdf_hash_from_string(librdf_hash* hash, const char *string) 
{
  const char * p;
  librdf_hash_datum hd_key, hd_value; /* on stack */
  const char *key;
  size_t key_len;
  const char *value;
  size_t value_len;
  int backslashes;
  int saw_backslash;
  librdf_hfs_parse_state state;
  int real_value_len;
  char *new_value;
  int i;
  char *to;

  if(!string)
    return 0;
  
#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
  LIBRDF_DEBUG2("Parsing >>%s<<\n", string);
#endif

  p=string;
  key=NULL; key_len=0;
  value=NULL; value_len=0;
  backslashes=0;
  state=HFS_PARSE_STATE_INIT;
  while(*p) {

#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
    LIBRDF_DEBUG3("state %d at %s\n", state, p);
#endif

    switch(state){
      /* start of config - before key */
      case HFS_PARSE_STATE_INIT:
        while(*p && (isspace((int)*p) || *p == ','))
          p++;
        if(!*p)
          break;

        /* fall through to next state */
        state=HFS_PARSE_STATE_KEY;
        
#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
    LIBRDF_DEBUG3("state %d at %s\n", state, p);
#endif

        /* start of key */
      case HFS_PARSE_STATE_KEY:
        key=p;
        while(*p && (isalnum((int)*p) || *p == '_' || *p == '-'))
          p++;
        if(!*p)
          break;
        key_len=p-key;
        
        /* if 1st char is not space or alpha, move on */
        if(!key_len) {
          p++;
          state=HFS_PARSE_STATE_INIT;
          break;
        }
        
        state=HFS_PARSE_STATE_SEP;
        /* fall through to next state */
      
#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
    LIBRDF_DEBUG3("state %d at %s\n", state, p);
#endif

        /* got key, now skipping spaces */
      case HFS_PARSE_STATE_SEP:
        while(*p && isspace((int)*p))
          p++;
        if(!*p)
          break;
        /* expecting = now */
        if(*p != '=') {
          p++;
          state=HFS_PARSE_STATE_INIT;
          break;
        }
        p++;
        state=HFS_PARSE_STATE_EQ;
        /* fall through to next state */
        
#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
    LIBRDF_DEBUG3("state %d at %s\n", state, p);
#endif

        /* got key\s+= now skipping spaces " */
      case HFS_PARSE_STATE_EQ:
        while(*p && isspace((int)*p))
          p++;
        if(!*p)
          break;
        /* expecting ' now */
        if(*p != '\'') {
          p++;
          state=HFS_PARSE_STATE_INIT;
          break;
        }
        p++;
        state=HFS_PARSE_STATE_VALUE;
        /* fall through to next state */
        
#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
    LIBRDF_DEBUG3("state %d at %s\n", state, p);
#endif

        /* got key\s+=\s+" now reading value */
      case HFS_PARSE_STATE_VALUE:
        value=p;
        backslashes=0;
        saw_backslash=0;
        while(*p) {
          if(!saw_backslash && *p == '\\') {
            /* backslashes are removed during value copy later */
            backslashes++; /* reduces real length */
            saw_backslash=1;
          } else {
            if (!saw_backslash && *p == '\'')
              break;
            saw_backslash=0;
          }
          
          p++;
        }
        if(!*p)
          return 1;
        
        /* ' at end of value found */
        value_len=p-value;
        real_value_len=value_len-backslashes;
        new_value=(char*)LIBRDF_MALLOC(cstring, real_value_len+1);
        if(!new_value)
          return 1;
        for(i=0, to=new_value; i<(int)value_len; i++){
          if(value[i]=='\\')
            i++;
          *to++=value[i];
        }
        *to='\0';

#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
        LIBRDF_DEBUG3("decoded key >>%s<< (true) value >>%s<<\n", key, new_value);
#endif
        
        hd_key.data=(void*)key; hd_key.size=key_len;
        hd_value.data=(void*)new_value; hd_value.size=real_value_len;
        
        librdf_hash_put(hash, &hd_key, &hd_value);
        
        LIBRDF_FREE(cstring, new_value);
        
#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
        LIBRDF_DEBUG1("after decoding ");
        librdf_hash_print (hash, stderr) ;
        fputc('\n', stderr);
#endif
        state=HFS_PARSE_STATE_INIT;
        p++;

        break;
        
      default:
        librdf_log(hash->world, 
                   0, LIBRDF_LOG_ERROR, LIBRDF_FROM_HASH, NULL,
                   "No such state %d", state);
        return 1;
    }
  }
  return 0;
}


/**
 * librdf_hash_from_array_of_strings:
 * @hash: hash object
 * @array: address of the start of the array of char* pointers
 *
 * Initialise a hash from an array of strings.
 * 
 * Return value:  non 0 on failure
 **/
int
librdf_hash_from_array_of_strings(librdf_hash* hash, const char **array)
{
  librdf_hash_datum key, value; /* on stack */
  int i;
  
  for(i=0; (key.data=(char*)array[i]); i+=2) {
    value.data=(char*)array[i+1];
    if(!value.data) {
      librdf_log(hash->world, 
                 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_HASH, NULL,
                 "Array contains an odd number of strings - %d", i);
      return 1;
    }
    key.size=strlen((char*)key.data);
    value.size=strlen((char*)value.data);
    librdf_hash_put(hash, &key, &value);
  }
  return 0;
}


/**
 * librdf_hash_get_as_boolean:
 * @hash: #librdf_hash object
 * @key: key string to look up
 *
 * Lookup a hash key and decode value as a boolean.
 * 
 * Return value: >0 (for true), 0 (for false) or <0 (for key not found or not known boolean value)
 **/
int
librdf_hash_get_as_boolean (librdf_hash* hash, const char *key) 
{
  int bvalue= (-1);
  char *value;

  value=librdf_hash_get(hash, key);
  if(!value)
    /* does not exist - fail */
    return -1;

  switch(strlen(value)) {
  case 2: /* try 'no' */
    if(*value=='n' && value[1]=='o')
      bvalue=0;
    break;
  case 3: /* try 'yes' */
    if(*value=='y' && value[1]=='e' && value[2]=='s')
      bvalue=1;
    break;
  case 4: /* try 'true' */
    if(*value=='t' && value[1]=='r' && value[2]=='u' && value[3]=='e')
      bvalue=1;
    break;
  case 5: /* try 'false' */
    if(!strncmp(value, "false", 5))
      bvalue=1;
    break;
  /* no need for default, bvalue is set above */
  }

  LIBRDF_FREE(cstring, value);

  return bvalue;
}


/**
 * librdf_hash_get_as_long:
 * @hash: #librdf_hash object
 * @key: key string to look up
 *
 * Lookup a hash key and decode value as a long.
 * 
 * Return value: >0 (for success), <0 (for key not found or not known boolean value)
 **/
long
librdf_hash_get_as_long (librdf_hash* hash, const char *key) 
{
  int lvalue;
  char *value;
  char *end_ptr;
  
  value=librdf_hash_get(hash, key);
  if(!value)
    /* does not exist - fail */
    return -1;

  /* Using special base 0 which allows decimal, hex and octal */
  lvalue=strtol(value, &end_ptr, 0);

  /* nothing found, return error */
  if(end_ptr == value)
    lvalue= (-1);

  LIBRDF_FREE(cstring, value);
  return lvalue;
}

/**
 * librdf_hash_put_strings:
 * @hash: hash object
 * @key: key 
 * @value: value
 *
 * Insert key/value pairs into the hash as strings.
 * 
 * The key and values are copied into the hash, no sharing i s done.
 * 
 * Return value: non 0 on failure
 **/
int
librdf_hash_put_strings(librdf_hash* hash, const char *key, const char *value)
{
  librdf_hash_datum key_hd; /* static */
  librdf_hash_datum value_hd;

  /* Note: We do not have to init the world field of
   * these librdf_hash_datum since they are never put on the
   * hash datums free list
   */

  key_hd.data=(void*)key;
  key_hd.size=strlen(key);
  value_hd.data=(void*)value;
  value_hd.size=strlen(value);
  return librdf_hash_put(hash, &key_hd, &value_hd);
}


/**
 * librdf_hash_interpret_template:
 * @template_string: template string to interprate
 * @dictionary: dictionary of key/values to substitute
 * @prefix: prefix to mark a key in the template
 * @suffix: suffix to mark a key in the template
 * 
 * Interpret keys in a template string to their value in a dictionary.
 * 
 * Can be used to do variable substitution for a string where
 * the syntax that marks the variable is defined by the @prefix
 * and @suffix strings, and the variables are stored in the @dictionary
 * hash table.
 *
 * Return value: Newly allocated string, or NULL on failure
 **/
unsigned char*
librdf_hash_interpret_template(const unsigned char* template_string,
                               librdf_hash* dictionary,
                               const unsigned char* prefix, 
                               const unsigned char* suffix) 
{
  raptor_stringbuffer* sb;
  unsigned char* result=NULL;
  size_t len;
  size_t prefix_len=strlen((const char*)prefix);
  size_t suffix_len=strlen((const char*)suffix);
  
  sb=raptor_new_stringbuffer();
  if(!sb)
    return NULL;
  
  len=strlen((const char*)template_string);
  
  while(*template_string) {
    unsigned char* p;
    unsigned char* s;
    librdf_hash_datum key; /* static */
    librdf_hash_datum *hd_value;
    size_t len2;
    
    p=(unsigned char*)strstr((const char*)template_string, (const char*)prefix);
    if(!p) {
      /* No more prefixes found so append rest of template */
      raptor_stringbuffer_append_counted_string(sb, template_string, len, 1);
      break;
    }
    len2=p-template_string;
    if(len2)
      raptor_stringbuffer_append_counted_string(sb, template_string, len2, 1);

    template_string += len2 + prefix_len;  len -= len2 + prefix_len;
    
    /* key starts here */
    key.data=(void*)template_string;
    
    s=(unsigned char*)strstr((const char*)template_string, (const char*)suffix);
    if(!s)
      /* template ended without a closing key suffix so just give up */
      break;

    /* now have key */
    len2= s - (unsigned char*)key.data;
    key.size= len2;

    /* move past key and suffix */
    template_string += len2 + suffix_len;  len -= len2 + suffix_len;

    hd_value=librdf_hash_get_one(dictionary, &key);
    /* append value if there is one */
    if(hd_value) {
      raptor_stringbuffer_append_counted_string(sb,
                                                (const unsigned char*)hd_value->data, 
                                                hd_value->size, 1);
      librdf_free_hash_datum(hd_value);
    }
    
  }

  /* Generate a string result */
  len=raptor_stringbuffer_length(sb);
  if(len) {
    result=(unsigned char*)LIBRDF_MALLOC(cstring, len+1);
    raptor_stringbuffer_copy_to_string(sb, result, len);
  }
  
  raptor_free_stringbuffer(sb);
  return result;
}




#endif


/* TEST CODE */


#ifdef STANDALONE

/* one more prototype */
int main(int argc, char *argv[]);


int
main(int argc, char *argv[]) 
{
  librdf_hash *h, *h2, *ch;
  const char *test_hash_types[]={"bdb", "memory", NULL};
  const char *test_hash_values[]={"colour","yellow", /* Made in UK, can you guess? */
			    "age", "new",
			    "size", "large",
                            "colour", "green",
			    "fruit", "banana",
                            "colour", "yellow",
			    NULL, NULL};
  const char *test_duplicate_key="colour";
  const char *test_hash_array[]={"shape", "cube",
                                 "sides", "6", /* for testing get as long */
                                 "3d", "yes", /* testing bool */
                                 "colours", "red",
                                 "colours", "yellow",
                                 "creator", "rubik",
                                 NULL};
  const char * const test_hash_string="field1='value1', field2='\\'value2', field3='\\\\', field4='\\\\\\'', field5 = 'a' ";
  const char *test_hash_delete_key="size";
  const unsigned char* template_string=(const unsigned char*)"the shape is %{shape} and the sides are %{sides} created by %{rubik}";
  const unsigned char* template_expected=(const unsigned char*)"the shape is cube and the sides are 6 created by ";
  int i,j;
  const char *type;
  librdf_hash_datum hd_key, hd_value; /* on stack */
  const char *program=librdf_basename((const char*)argv[0]);
  int b;
  long l;
  unsigned char* template_result;
  librdf_world *world;
  
  world=librdf_new_world();
  librdf_world_open(world);
  
  if(argc ==2) {
    type=argv[1];
    h=librdf_new_hash(world, NULL);
    if(!h) {
      fprintf(stderr, "%s: Failed to create new hash type '%s'\n",
	      program, type);
      return(0);
    }
    
    librdf_hash_open(h, "test", 0644, 1, 1, NULL);
    librdf_hash_from_string(h, argv[1]);
    fprintf(stdout, "%s: resulting ", program);
    librdf_hash_print(h, stdout);
    fputc('\n', stdout);
    fprintf(stdout, "%s: values count %d\n", program, librdf_hash_values_count(h));
    librdf_hash_close(h);
    librdf_free_hash(h);
    return(0);
  }
  
  
  for(i=0; (type=test_hash_types[i]); i++) {
    fprintf(stdout, "%s: Trying to create new %s hash\n", program, type);
    h=librdf_new_hash(world, type);
    if(!h) {
      fprintf(stderr, "%s: Failed to create new hash type '%s'\n", program, type);
      continue;
    }

    if(librdf_hash_open(h, "test", 0644, 1, 1, NULL)) {
      fprintf(stderr, "%s: Failed to open new hash type '%s'\n", program, type);
      continue;
    }
    
    
    for(j=0; test_hash_values[j]; j+=2) {
      hd_key.data=(char*)test_hash_values[j];
      hd_value.data=(char*)test_hash_values[j+1];
      fprintf(stdout, "%s: Adding key/value pair: %s=%s\n", program,
	      (char*)hd_key.data, (char*)hd_value.data);
      
      hd_key.size=strlen((char*)hd_key.data);
      hd_value.size=strlen((char*)hd_value.data); 
      librdf_hash_put(h, &hd_key, &hd_value);
      
      fprintf(stdout, "%s: resulting ", program);
      librdf_hash_print(h, stdout);
      fputc('\n', stdout);
      fprintf(stdout, "%s: values count %d\n", program, librdf_hash_values_count(h));
    }
    
    fprintf(stdout, "%s: Deleting key '%s'\n", program, test_hash_delete_key);
    hd_key.data=(char*)test_hash_delete_key;
    hd_key.size=strlen((char*)hd_key.data);
    librdf_hash_delete_all(h, &hd_key);
    
    fprintf(stdout, "%s: resulting ", program);
    librdf_hash_print(h, stdout);
    fputc('\n', stdout);
    fprintf(stdout, "%s: values count %d\n", program, librdf_hash_values_count(h));
    
    fprintf(stdout, "%s: resulting %s hash keys: ", program, type);
    librdf_hash_print_keys(h, stdout);
    fputc('\n', stdout);

    fprintf(stdout, "%s: all values of key '%s'=", program, test_duplicate_key);
    librdf_hash_print_values(h, test_duplicate_key, stdout);
    fputc('\n', stdout);

    fprintf(stdout, "%s: cloning %s hash\n", program, type);
    ch=librdf_new_hash_from_hash(h);
    if(ch) {
      fprintf(stdout, "%s: resulting cloned ", program);
      librdf_hash_print(ch, stdout);
      fputc('\n', stdout);
      fprintf(stdout, "%s: values count %d\n", program, librdf_hash_values_count(ch));
      
      librdf_hash_close(ch);
      librdf_free_hash(ch);
    } else {
      fprintf(stderr, "%s: Failed to clone %s hash\n", program, type);
    }

    librdf_hash_close(h);
      
    fprintf(stdout, "%s: Freeing hash\n", program);
    librdf_free_hash(h);
  }
  fprintf(stdout, "%s: Getting default hash factory\n", program);
  h2=librdf_new_hash(world, NULL);
  if(!h2) {
    fprintf(stderr, "%s: Failed to create new hash from default factory\n", program);
    return(1);
  }

  fprintf(stdout, "%s: Initialising hash from array of strings\n", program);
  if(librdf_hash_from_array_of_strings(h2, test_hash_array)) {
    fprintf(stderr, "%s: Failed to init hash from array of strings\n", program);
    return(1);
  }
  
  fprintf(stdout, "%s: resulting hash ", program);
  librdf_hash_print(h2, stdout);
  fputc('\n', stdout);
  fprintf(stdout, "%s: values count %d\n", program, librdf_hash_values_count(h2));

  fprintf(stdout, "%s: resulting hash keys: ", program);
  librdf_hash_print_keys(h2, stdout);
  fputc('\n', stdout);

  
  /* test get as boolean and long functions */
  {
    librdf_iterator* iterator;
    librdf_hash_datum *key_hd;
    
    key_hd=librdf_new_hash_datum(world, NULL, 0);
    
    iterator=librdf_hash_keys(h2, key_hd);
    while(!librdf_iterator_end(iterator)) {
      librdf_hash_datum *k=(librdf_hash_datum*)librdf_iterator_get_key(iterator);
      char *key_string;
      
      key_string=(char*)LIBRDF_MALLOC(cstring, k->size+1);
      if(!key_string)
        break;
      strncpy(key_string, (char*)k->data, k->size);
      key_string[k->size]='\0';
      
      fprintf(stdout, "%s: boolean value of key '%s' is ", program,
              key_string);
      b=librdf_hash_get_as_boolean(h2, key_string);
      fprintf(stdout, "%d (0 F, -1 Bad, else T)\n", b);
      
      fprintf(stdout, "%s: long value of key '%s' is ", program,
              key_string);
      l=librdf_hash_get_as_long(h2, key_string);
      fprintf(stdout, "%ld (decimal, -1 Bad)\n", l);
      
      LIBRDF_FREE(cstring, key_string);
      librdf_iterator_next(iterator);
    }
    if(iterator)
      librdf_free_iterator(iterator);
    
    librdf_free_hash_datum(key_hd);
  }


  fprintf(stdout, "%s: Freeing hash\n", program);
  /* close() done automatically by free so not required */
  /* librdf_hash_close(h2); */
  librdf_free_hash(h2);

  h2=librdf_new_hash(world, NULL);
  fprintf(stdout, "%s: Initialising hash from string >>%s<<\n", program, 
          test_hash_string);
  librdf_hash_from_string (h2, test_hash_string);
  fprintf(stdout, "%s: resulting ", program);
  librdf_hash_print(h2, stdout);
  fputc('\n', stdout);
  fprintf(stdout, "%s: values count %d\n", program, librdf_hash_values_count(h2));

  librdf_free_hash(h2);

   
  fprintf(stdout, "%s: Subtituting into template >>%s<<\n", program, 
          template_string);
  h2=librdf_new_hash(world, NULL);
  librdf_hash_from_array_of_strings(h2, test_hash_array);

  template_result=librdf_hash_interpret_template(template_string, h2, 
                                                 (const unsigned char*)"%{", 
                                                 (const unsigned char*)"}");
  if(strcmp((const char*)template_result, (const char*)template_expected)) {
    fprintf(stdout, "%s: Templating failed. Result was >>%s<< but expected >>%s<<\n", program, 
            template_result, template_expected);
    exit(1);
  } else
    fprintf(stdout, "%s: resulting in >>%s<<\n", program, template_result);

  LIBRDF_FREE(cstring, template_result);

  librdf_free_hash(h2);

   
  librdf_free_world(world);
  
  /* keep gcc -Wall happy */
  return(0);
}

#endif
