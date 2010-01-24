/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_hash_internal.h - Internal RDF Hash Factory and Hash definitions
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


#ifndef LIBRDF_HASH_INTERNAL_H
#define LIBRDF_HASH_INTERNAL_H

#ifdef __cplusplus
extern "C" {
#endif

/** data type used to describe hash key and data */
struct librdf_hash_datum_s
{
  librdf_world *world;
  void *data;
  size_t size;
  /* used internally to build lists of these  */
  struct librdf_hash_datum_s *next;
};
typedef struct librdf_hash_datum_s librdf_hash_datum;

/* constructor / destructor for above */
librdf_hash_datum* librdf_new_hash_datum(librdf_world *world, void *data, size_t size);
void librdf_free_hash_datum(librdf_hash_datum *ptr);
  


/** A hash object */
struct librdf_hash_s
{
  librdf_world* world;
  char* identifier; /* as passed in during open(), used by clone() */
  void* context;
  int   is_open;
  struct librdf_hash_factory_s* factory;
};


/** A Hash Factory */
struct librdf_hash_factory_s {
  struct librdf_hash_factory_s* next;
  char* name;

  /* the rest of this structure is populated by the
     hash-specific register function */
  size_t context_length;

  /* size of the cursor context */
  size_t cursor_context_length;

  /* clone an existing storage */
  int (*clone)(librdf_hash* new_hash, void* new_context, char* new_name, void* old_context);

  /* create / destroy a hash implementation */
  int (*create)(librdf_hash* hash, void* context);
  int (*destroy)(void* context);

  /* open/create hash with identifier and options  */
  int (*open)(void* context, const char *identifier, int mode, int is_writable, int is_new, librdf_hash* options);
  /* end hash association */
  int (*close)(void* context);

  /* hoe many values? */
  int (*values_count)(void* context);

  /* insert key/value pairs according to flags */
  int (*put)(void* context, librdf_hash_datum *key, librdf_hash_datum *data);

  /* returns true if key exists in hash, without returning value */
  int (*exists)(void* context, librdf_hash_datum *key, librdf_hash_datum *value);

  int (*delete_key)(void* context, librdf_hash_datum *key);
  int (*delete_key_value)(void* context, librdf_hash_datum *key, librdf_hash_datum *value);

  /* flush any cached information to disk */
  int (*sync)(void* context);

  /* get the file descriptor for the hash, if it is file based (for locking) */
  int (*get_fd)(void* context);

  /* create a cursor and operate on it */
  int (*cursor_init)(void *cursor_context, void* hash_context);
  int (*cursor_get)(void *cursor, librdf_hash_datum *key, librdf_hash_datum *value, unsigned int flags);
  void (*cursor_finish)(void *context);
};
typedef struct librdf_hash_factory_s librdf_hash_factory;

/* factory class methods */
void librdf_hash_register_factory(librdf_world *world, const char *name, void (*factory) (librdf_hash_factory*));
librdf_hash_factory* librdf_get_hash_factory(librdf_world *world, const char *name);


/* module init */
void librdf_init_hash(librdf_world *world);

/* module terminate */
void librdf_finish_hash(librdf_world *world);

/* hash cursor_get method flags */
#define LIBRDF_HASH_CURSOR_SET 0
#define LIBRDF_HASH_CURSOR_NEXT_VALUE 1
#define LIBRDF_HASH_CURSOR_FIRST 2
#define LIBRDF_HASH_CURSOR_NEXT 3


/* constructors */
librdf_hash* librdf_new_hash(librdf_world *world, const char *name);
librdf_hash* librdf_new_hash_from_factory(librdf_world *world, librdf_hash_factory* factory);

/* methods */

/* open/create hash with identifier and options  */
int librdf_hash_open(librdf_hash* hash, const char *identifier, int mode, int is_writable, int is_new, librdf_hash* options);
/* end hash association */
int librdf_hash_close(librdf_hash* hash);

/* how many values */
int librdf_hash_values_count(librdf_hash* hash);

/* retrieve one value for a given hash key as a hash datum */
librdf_hash_datum* librdf_hash_get_one(librdf_hash* hash, librdf_hash_datum *key);

/* retrieve all values for a given hash key according to flags */
librdf_iterator* librdf_hash_get_all(librdf_hash* hash, librdf_hash_datum *key, librdf_hash_datum *value);

/* insert a key/value pair */
int librdf_hash_put(librdf_hash* hash, librdf_hash_datum *key, librdf_hash_datum *value);

  /* returns true if key exists in hash, without returning value */
int librdf_hash_exists(librdf_hash* hash, librdf_hash_datum *key, librdf_hash_datum *value);

int librdf_hash_delete(librdf_hash* hash, librdf_hash_datum *key, librdf_hash_datum *value);
int librdf_hash_delete_all(librdf_hash* hash, librdf_hash_datum *key);
librdf_iterator* librdf_hash_keys(librdf_hash* hash, librdf_hash_datum *key);

/* flush any cached information to disk */
int librdf_hash_sync(librdf_hash* hash);
/* get the file descriptor for the hash, if it is file based (for locking) */
int librdf_hash_get_fd(librdf_hash* hash);

/* init a hash from a string representation */
int librdf_hash_from_string(librdf_hash* hash, const char *string);

/* init a hash from an array of strings */
int librdf_hash_from_array_of_strings(librdf_hash* hash, const char *array[]);


/* cursor methods from rdf_hash_cursor.c */

librdf_hash_cursor* librdf_new_hash_cursor (librdf_hash* hash);
void librdf_free_hash_cursor (librdf_hash_cursor* cursor);
int librdf_hash_cursor_set(librdf_hash_cursor *cursor, librdf_hash_datum *key,librdf_hash_datum *value);
int librdf_hash_cursor_get_next_value(librdf_hash_cursor *cursor, librdf_hash_datum *key,librdf_hash_datum *value);
int librdf_hash_cursor_get_first(librdf_hash_cursor *cursor, librdf_hash_datum *key, librdf_hash_datum *value);
int librdf_hash_cursor_get_next(librdf_hash_cursor *cursor, librdf_hash_datum *key, librdf_hash_datum *value);

#ifdef HAVE_BDB_HASH
void librdf_init_hash_bdb(librdf_world *world);
#endif
void librdf_init_hash_memory(librdf_world *world);


#ifdef __cplusplus
}
#endif

#endif
