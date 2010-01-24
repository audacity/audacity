/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_hash_bdb.c - RDF hash Berkeley DB Interface Implementation
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
#include <stdarg.h>

#include <sys/types.h>

/* for the memory allocation functions */
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif


#ifdef HAVE_DB_H
#include <db.h>
#endif

#include <redland.h>
#include <rdf_hash.h>


typedef struct 
{
  librdf_hash *hash;
  int mode;
  int is_writable;
  int is_new;
  /* for BerkeleyDB only */
  DB* db;
  char* file_name;
} librdf_hash_bdb_context;


/* Implementing the hash cursor */
static int librdf_hash_bdb_cursor_init(void *cursor_context, void *hash_context);
static int librdf_hash_bdb_cursor_get(void *context, librdf_hash_datum* key, librdf_hash_datum* value, unsigned int flags);
static void librdf_hash_bdb_cursor_finish(void* context);


/* prototypes for local functions */
static int librdf_hash_bdb_create(librdf_hash* hash, void* context);
static int librdf_hash_bdb_destroy(void* context);
static int librdf_hash_bdb_open(void* context, const char *identifier, int mode, int is_writable, int is_new, librdf_hash* options);
static int librdf_hash_bdb_close(void* context);
static int librdf_hash_bdb_clone(librdf_hash* new_hash, void *new_context, char *new_identifier, void* old_context);
static int librdf_hash_bdb_values_count(void *context);
static int librdf_hash_bdb_put(void* context, librdf_hash_datum *key, librdf_hash_datum *data);
static int librdf_hash_bdb_exists(void* context, librdf_hash_datum *key, librdf_hash_datum *value);
static int librdf_hash_bdb_delete_key(void* context, librdf_hash_datum *key);
static int librdf_hash_bdb_delete_key_value(void* context, librdf_hash_datum *key, librdf_hash_datum *value);
static int librdf_hash_bdb_sync(void* context);
static int librdf_hash_bdb_get_fd(void* context);

static void librdf_hash_bdb_register_factory(librdf_hash_factory *factory);


/* functions implementing hash api */

/**
 * librdf_hash_bdb_create:
 * @hash: #librdf_hash hash that this implements
 * @context: BerkeleyDB hash context
 *
 * Create a BerkeleyDB hash.
 * 
 * Return value: non 0 on failure.
 **/
static int
librdf_hash_bdb_create(librdf_hash* hash, void* context) 
{
  librdf_hash_bdb_context* hcontext=(librdf_hash_bdb_context*)context;

  hcontext->hash=hash;
  return 0;
}


/**
 * librdf_hash_bdb_destroy:
 * @context: BerkeleyDB hash context
 *
 * Destroy a BerkeleyDB hash.
 * 
 * Return value: non 0 on failure
 **/
static int
librdf_hash_bdb_destroy(void* context) 
{
  /* NOP */
  return 0;
}


/**
 * librdf_hash_bdb_open:
 * @context: BerkeleyDB hash context
 * @identifier: filename to use for BerkeleyDB file
 * @mode: file creation mode
 * @is_writable: is hash writable?
 * @is_new: is hash new?
 * @options: hash options (currently unused)
 *
 * Open and maybe create a BerkeleyDB hash.
 * 
 * Return value: non 0 on failure.
 **/
static int
librdf_hash_bdb_open(void* context, const char *identifier, 
                     int mode, int is_writable, int is_new,
                     librdf_hash* options) 
{
  librdf_hash_bdb_context* bdb_context=(librdf_hash_bdb_context*)context;
  DB* bdb;
  char *file;
  int ret;
  int flags;

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(identifier, cstring, 1);
  
#ifdef HAVE_DB_OPEN
  DB_INFO bdb_info;
#endif
  
  /* NOTE: If the options parameter is ever used here, the data must be
   * copied into a private part of the context so that the clone
   * method can access them
   */
  bdb_context->mode=mode;
  bdb_context->is_writable=is_writable;
  bdb_context->is_new=is_new;
  
  file=(char*)LIBRDF_MALLOC(cstring, strlen(identifier)+4);
  if(!file)
    return 1;
  sprintf(file, "%s.db", identifier);

#ifdef HAVE_DB_CREATE
  /* V3 prototype:
   * int db_create(DB **dbp, DB_ENV *dbenv, u_int32_t flags);
   */
  if((ret=db_create(&bdb, NULL, 0))) {
    LIBRDF_DEBUG2("Failed to create BDB context - %d\n", ret);
    return 1;
  }
  
#ifdef HAVE_BDB_SET_FLAGS
  if((ret=bdb->set_flags(bdb, DB_DUP))) {
    LIBRDF_DEBUG2("Failed to set BDB duplicate flag - %d\n", ret);
    return 1;
  }
#endif
  
  /* V3 prototype:
   * int DB->open(DB *db, const char *file, const char *database,
   *              DBTYPE type, u_int32_t flags, int mode);
   */
  flags=is_writable ? DB_CREATE : DB_RDONLY;
  if(is_new)
    flags |= DB_TRUNCATE;
#endif

#if defined(HAVE_BDB_OPEN_6_ARGS) || defined(HAVE_BDB_OPEN_7_ARGS)

#ifdef HAVE_BDB_OPEN_6_ARGS  
/* 
 * int DB->open(DB *db, const char *file,
 *              const char *database, DBTYPE type, u_int32_t flags, int mode);
 */
  if((ret=bdb->open(bdb, file, NULL, DB_BTREE, flags, mode))) {
    librdf_log(bdb_context->hash->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
               "BDB V4.0+ open of '%s' failed - %s", file, db_strerror(ret));
    LIBRDF_FREE(cstring, file);
    return 1;
  }
#else
/* Must be HAVE_BDB_OPEN_7_ARGS */

/* 
 * int DB->open(DB *db, DB_TXN *txnid, const char *file,
 *              const char *database, DBTYPE type, u_int32_t flags, int mode);
 */
  if((ret=bdb->open(bdb, NULL, file, NULL, DB_BTREE, flags, mode))) {
    librdf_log(bdb_context->hash->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
               "BDB V4.1+ open of '%s' failed - %s", file, db_strerror(ret));
    LIBRDF_FREE(cstring, file);
    return 1;
  }
#endif

#else
#ifdef HAVE_DB_OPEN
  /* V2 prototype:
   * int db_open(const char *file, DBTYPE type, u_int32_t flags,
   *             int mode, DB_ENV *dbenv, DB_INFO *dbinfo, DB **dbpp);
   */

  memset(&bdb_info, 0, sizeof(DB_INFO));
  bdb_info.flags=DB_DUP;
 
  flags=is_writable ? DB_CREATE : DB_RDONLY;
  if(is_new)
    flags |= DB_TRUNCATE;

  if((ret=db_open(file, DB_BTREE, flags, mode, NULL, &bdb_info, &bdb))) {
    librdf_log(bdb_context->hash->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
               "BDB V2 open of '%s' failed - %d", file, ret);
    LIBRDF_FREE(cstring, file);
    return 1;
  }
#else
#ifdef HAVE_DBOPEN
  /* V1 prototype:
    const char *file, int flags, int mode, DBTYPE, const void *openinfo
  */
  flags=is_writable ? O_RDWR|O_CREAT : O_RDONLY

  flags|=R_DUP;

  /* There does not seem to be a V1 flag for truncate */ 
  if(is_new)
    remove(file);

  if((bdb=dbopen(file, flags, mode, DB_BTREE, NULL)) == 0) {
    librdf_log(bdb_context->hash->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
               "BDB V1 open of '%s' failed - %d", file, ret);
    LIBRDF_FREE(cstring, file);
    return 1;
  }
  ret=0;
#else
#ifdef HAVE_DB_CREATE
/* earlier */
#else
ERROR - no idea how to use Berkeley DB
#endif
#endif
#endif
#endif

  bdb_context->db=bdb;
  bdb_context->file_name=file;
  return 0;
}


/**
 * librdf_hash_bdb_close:
 * @context: BerkeleyDB hash context
 *
 * Close the hash.
 * 
 * Finish the association between the rdf hash and the BDB file (does
 * not delete the file)
 * 
 * Return value: non 0 on failure
 **/
static int
librdf_hash_bdb_close(void* context) 
{
  librdf_hash_bdb_context* bdb_context=(librdf_hash_bdb_context*)context;
  DB* db=bdb_context->db;
  int ret;
  
#ifdef HAVE_BDB_CLOSE_2_ARGS
  /* V2/V3 */
  ret=db->close(db, 0);
#else
  /* V1 */
  ret=db->close(db);
#endif
  LIBRDF_FREE(cstring, bdb_context->file_name);
  return ret;
}


/**
 * librdf_hash_bdb_clone:
 * @hash: new #librdf_hash that this implements
 * @context: new BerkeleyDB hash context
 * @new_identifier: new identifier for this hash
 * @old_context: old BerkeleyDB hash context
 *
 * Clone the BerkeleyDB hash.
 * 
 * Clones the existing Berkeley DB hash into the new one with the
 * new identifier.
 * 
 * Return value: non 0 on failure
 **/
static int
librdf_hash_bdb_clone(librdf_hash *hash, void* context, char *new_identifier,
                      void *old_context) 
{
  librdf_hash_bdb_context* hcontext=(librdf_hash_bdb_context*)context;
  librdf_hash_bdb_context* old_hcontext=(librdf_hash_bdb_context*)old_context;
  librdf_hash_datum *key, *value;
  librdf_iterator *iterator;
  int status=0;
  
  /* copy data fields that might change */
  hcontext->hash=hash;

  /* Note: The options are not used at present, so no need to make a copy 
   */
  if(librdf_hash_bdb_open(context, new_identifier,
                          old_hcontext->mode, old_hcontext->is_writable,
                          old_hcontext->is_new, NULL))
    return 1;


  /* Use higher level functions to iterator this data
   * on the other hand, maybe this is a good idea since that
   * code is tested and works
   */

  key=librdf_new_hash_datum(hash->world, NULL, 0);
  value=librdf_new_hash_datum(hash->world, NULL, 0);

  iterator=librdf_hash_get_all(old_hcontext->hash, key, value);
  while(!librdf_iterator_end(iterator)) {
    librdf_hash_datum* k= (librdf_hash_datum*)librdf_iterator_get_key(iterator);
    librdf_hash_datum* v= (librdf_hash_datum*)librdf_iterator_get_value(iterator);

    if(librdf_hash_bdb_put(hcontext, k, v)) {
      status=1;
      break;
    }
    librdf_iterator_next(iterator);
  }
  if(iterator)
    librdf_free_iterator(iterator);

  librdf_free_hash_datum(value);
  librdf_free_hash_datum(key);

  return status;
}


/**
 * librdf_hash_bdb_values_count:
 * @context: BerkeleyDB hash context
 *
 * Get the number of values in the hash.
 * 
 * Return value: number of values in the hash or <0 if not available
 **/
static int
librdf_hash_bdb_values_count(void *context) 
{
  return -1;
}



typedef struct {
  librdf_hash_bdb_context* hash;
  void *last_key;
  void *last_value;
#ifdef HAVE_BDB_CURSOR
  DBC* cursor;
#endif
} librdf_hash_bdb_cursor_context;


/**
 * librdf_hash_bdb_cursor_init:
 * @cursor_context: hash cursor context
 * @hash_context: hash to operate over
 *
 * Initialise a new bdb cursor.
 * 
 * Return value: non 0 on failure
 **/
static int
librdf_hash_bdb_cursor_init(void *cursor_context, void *hash_context)
{
  librdf_hash_bdb_cursor_context *cursor=(librdf_hash_bdb_cursor_context*)cursor_context;
#ifdef HAVE_BDB_CURSOR
  DB* db;
#endif

  cursor->hash=(librdf_hash_bdb_context*)hash_context;

#ifdef HAVE_BDB_CURSOR
  db=cursor->hash->db;
#ifdef HAVE_BDB_CURSOR_4_ARGS
  /* V3 prototype:
   * int DB->cursor(DB *db, DB_TXN *txnid, DBC **cursorp, u_int32_t flags);
   */
  if(db->cursor(db, NULL, &cursor->cursor, 0))
    return 1;
#else
  /* V2 prototype:
   * int DB->cursor(DB *db, DB_TXN *txnid, DBC **cursorp);
   */
  if(db->cursor(db, NULL, &cursor->cursor))
    return 1;
#endif
#endif

  return 0;
}


/**
 * librdf_hash_bdb_cursor_get:
 * @context: BerkeleyDB hash cursor context
 * @key: pointer to key to use
 * @value: pointer to value to use
 * @flags: flags
 *
 * Retrieve a hash value for the given key.
 * 
 * Return value: non 0 on failure
 **/
static int
librdf_hash_bdb_cursor_get(void* context, 
                           librdf_hash_datum *key, librdf_hash_datum *value,
                           unsigned int flags)
{
  librdf_hash_bdb_cursor_context *cursor=(librdf_hash_bdb_cursor_context*)context;
#ifdef HAVE_BDB_CURSOR
  DBC *bdb_cursor=cursor->cursor;
#else
  /* For BDB V1 */
  DB* db;
#endif
  DBT bdb_key;
  DBT bdb_value;
  int ret;

  /* docs say you must zero DBT's before use */
  memset(&bdb_key, 0, sizeof(DBT));
  memset(&bdb_value, 0, sizeof(DBT));

  /* Always initialise BDB version of key */
  bdb_key.data = (char*)key->data;
  bdb_key.size = key->size;
  
#ifdef DB_DBT_MALLOC
  /* BDB V2 or later? */
  bdb_key.flags=DB_DBT_MALLOC;   /* Return in malloc() allocated memory */
  bdb_value.flags=DB_DBT_MALLOC;
#endif

#ifndef HAVE_BDB_CURSOR
  /* For BDB V1 */
  db=cursor->hash->db;
#endif
  
  switch(flags) {
    case LIBRDF_HASH_CURSOR_SET:

#ifdef HAVE_BDB_CURSOR
      /* V2/V3 prototype:
       * int DBcursor->c_get(DBC *cursor, DBT *key, DBT *data, u_int32_t flags);
       */
      ret=bdb_cursor->c_get(bdb_cursor, &bdb_key, &bdb_value, DB_SET);
#else
      /* V1 */
      ret=db->seq(db, &bdb_key, &bdb_value, 0);
#endif
      break;
      
    case LIBRDF_HASH_CURSOR_FIRST:
#ifdef HAVE_BDB_CURSOR
      /* V2/V3 prototype:
       * int DBcursor->c_get(DBC *cursor, DBT *key, DBT *data, u_int32_t flags);
       */
      ret=bdb_cursor->c_get(bdb_cursor, &bdb_key, &bdb_value, DB_FIRST);
#else
      /* V1 */
      ret=db->seq(db, &bdb_key, &bdb_value, R_FIRST);
#endif
      break;
      
    case LIBRDF_HASH_CURSOR_NEXT_VALUE:
#ifdef HAVE_BDB_CURSOR
      /* V2/V3 */
      ret=bdb_cursor->c_get(bdb_cursor, &bdb_key, &bdb_value, DB_NEXT);
#else
      /* V1 */
      ret=db->seq(db, &bdb_key, &bdb_value, R_NEXT);
#endif
      
      /* If succeeded and key has changed, end */
      if(!ret && cursor->last_key &&
         memcmp(cursor->last_key, bdb_key.data, bdb_key.size)) {
        
        /* always allocated by BDB using system malloc */
        SYSTEM_FREE(bdb_key.data);
        SYSTEM_FREE(bdb_value.data);

#ifdef DB_NOTFOUND
        /* V2 and V3 */
        ret=DB_NOTFOUND;
#else
        ret=1;
#endif
      }
      
      break;
      
    case LIBRDF_HASH_CURSOR_NEXT:
#ifdef HAVE_BDB_CURSOR
#ifdef DB_NEXT_NODUP
      /* V3 */

      /* Get next key, or next key/value (when value defined) */
      ret=bdb_cursor->c_get(bdb_cursor, &bdb_key, &bdb_value,
                            (value) ? DB_NEXT : DB_NEXT_NODUP);
#else
      /* V2 */

      /* Must mess about finding next key - note this relies on
       * the bdb btree having the keys in sorted order
       */
      while(1) {
        ret=bdb_cursor->c_get(bdb_cursor, &bdb_key, &bdb_value, DB_NEXT);
        /* finish on error, want all values or no previous key */
        if(ret || value || !cursor->last_key)
          break;
        /* else have previous key and want unique keys, so keep
         * going until the key changes
         */
        if(memcmp(cursor->last_key, bdb_key.data, bdb_key.size))
          break;
        
        /* always allocated by BDB using system malloc */
        SYSTEM_FREE(bdb_key.data);
        SYSTEM_FREE(bdb_value.data);
      }
#endif
#else
      /* V1 */
      ret=db->seq(db, &bdb_key, &bdb_value, R_NEXT);
#endif
      break;
      
    default:
      librdf_log(cursor->hash->hash->world,
                 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_HASH, NULL,
                 "Unknown hash method flag %d", flags);
      return 1;
  }


  /* Free previous key and values */
  if(cursor->last_key) {
    LIBRDF_FREE(cstring, cursor->last_key);
    cursor->last_key=NULL;
  }
    
  if(cursor->last_value) {
    LIBRDF_FREE(cstring, cursor->last_value);
    cursor->last_value=NULL;
  }

  

  if(ret) {
#ifdef DB_NOTFOUND
    /* V2 and V3 */
    if(ret != DB_NOTFOUND)
      LIBRDF_DEBUG2("BDB cursor error - %d\n", ret);
#endif
    key->data=NULL;
    return ret;
  }
  
  cursor->last_key = key->data = LIBRDF_MALLOC(cstring, bdb_key.size);
  if(!key->data) {
    /* always allocated by BDB using system malloc */
    if(flags != LIBRDF_HASH_CURSOR_SET)
      SYSTEM_FREE(bdb_key.data);
    SYSTEM_FREE(bdb_value.data);
    return 1;
  }
  
  memcpy(key->data, bdb_key.data, bdb_key.size);
  key->size = bdb_key.size;

  if(value) {
    cursor->last_value = value->data = LIBRDF_MALLOC(cstring, bdb_value.size);
    if(!value->data) {
      /* always allocated by BDB using system malloc */
      if(flags != LIBRDF_HASH_CURSOR_SET)
        SYSTEM_FREE(bdb_key.data);
      SYSTEM_FREE(bdb_value.data);
      return 1;
    }
    
    memcpy(value->data, bdb_value.data, bdb_value.size);
    value->size = bdb_value.size;
  }

  /* always allocated by BDB using system malloc */
  if(flags != LIBRDF_HASH_CURSOR_SET)
    SYSTEM_FREE(bdb_key.data);
  SYSTEM_FREE(bdb_value.data);

  return 0;
}


/**
 * librdf_hash_bdb_cursor_finished:
 * @context: BerkeleyDB hash cursor context
 *
 * Finish the serialisation of the hash bdb get.
 *
 **/
static void
librdf_hash_bdb_cursor_finish(void* context)
{
  librdf_hash_bdb_cursor_context* cursor=(librdf_hash_bdb_cursor_context*)context;

#ifdef HAVE_BDB_CURSOR
  /* BDB V2/V3 */
  if(cursor->cursor)
    cursor->cursor->c_close(cursor->cursor);
#endif
  if(cursor->last_key)
    LIBRDF_FREE(cstring, cursor->last_key);
    
  if(cursor->last_value)
    LIBRDF_FREE(cstring, cursor->last_value);
}


/**
 * librdf_hash_bdb_put:
 * @context: BerkeleyDB hash context
 * @key: pointer to key to store
 * @value: pointer to value to store
 *
 * Store a key/value pair in the hash.
 * 
 * Return value: non 0 on failure
 **/
static int
librdf_hash_bdb_put(void* context, librdf_hash_datum *key, 
                    librdf_hash_datum *value) 
{
  librdf_hash_bdb_context* bdb_context=(librdf_hash_bdb_context*)context;
  DB* db=bdb_context->db;
  DBT bdb_value;
  DBT bdb_key;
  int ret;

  /* docs say you must zero DBT's before use */
  memset(&bdb_value, 0, sizeof(DBT));
  memset(&bdb_key, 0, sizeof(DBT));
  
  /* Initialise BDB version of key */
  bdb_key.data = (char*)key->data;
  bdb_key.size = key->size;
  
  /* Initialise BDB version of data */
  bdb_value.data = (char*)value->data;
  bdb_value.size = value->size;
  
#ifdef HAVE_BDB_DB_TXN
  /* V2/V3 prototype:
   * int DB->put(DB *db, DB_TXN *txnid, DBT *key, DBT *data, u_int32_t flags); 
   */
  ret=db->put(db, NULL, &bdb_key, &bdb_value, 0);
#else
  /* V1 */
  ret=db->put(db, &bdb_key, &bdb_value, 0);
#endif
  if(ret)
    LIBRDF_DEBUG2("BDB put failed - %d\n", ret);

  return (ret != 0);
}


/**
 * librdf_hash_bdb_exists:
 * @context: BerkeleyDB hash context
 * @key: pointer to key
 * @value: pointer to value (optional)
 *
 * Test the existence of a key/value in the hash.
 * 
 * The value can be NULL in which case the check will just be
 * for the key.
 *
 * Return value: >0 if the key/value exists in the hash, 0 if not, <0 on failure
 **/
static int
librdf_hash_bdb_exists(void* context, librdf_hash_datum *key,
                       librdf_hash_datum *value) 
{
  librdf_hash_bdb_context* bdb_context=(librdf_hash_bdb_context*)context;
  DB* db=bdb_context->db;
  DBT bdb_key;
  DBT bdb_value;
  int ret;
  
  /* docs say you must zero DBT's before use */
  memset(&bdb_key, 0, sizeof(DBT));
  memset(&bdb_value, 0, sizeof(DBT));
  
  /* Initialise BDB version of key */
  bdb_key.data = (char*)key->data;
  bdb_key.size = key->size;

  if(value) {
    bdb_value.data = (char*)value->data;
    bdb_value.size = value->size;
  }
	
#ifdef HAVE_BDB_DB_TXN
#ifdef DB_GET_BOTH
  /* later V2 (sigh)/V3 */
  ret=db->get(db, NULL, &bdb_key, &bdb_value, (value ? DB_GET_BOTH : 0));
  if(ret == DB_NOTFOUND)
    ret= 0;
  else if(ret) /* failed */
    ret= -1;
  else
    ret= 1;
#else
  /* earlier V2 */
  if(!value) {
    /* don't care about value, can use standard get */
    ret=db->get(db, NULL, &bdb_key, &bdb_value, 0);
    if(ret == DB_NOTFOUND)
      ret= 0;
    else if(ret) /* failed */
      ret= -1;
    else
      ret= 1;
  } else {
    /* Want exact key/value - have to use a cursor, darn */
    DBC* dbc=NULL;
    
    ret=1;
    
    if(db->cursor(db, NULL, &dbc))
      ret= -1;

    if(ret >= 0) {
      ret=dbc->c_get(dbc, &bdb_key, &bdb_value, DB_SET);
      if(ret == DB_NOTFOUND)
        ret= 0;
      else if(ret) /* failed */
        ret= -1;
      else
        ret= 1;
    }

    while(ret > 0) {
      /* key different - match failed */
      if(memcmp(key->data, bdb_key.data, key->size)) {
        ret=0;
        break;
      }
      /* value equal - match found */
      if(!memcmp(value->data, bdb_value.data, value->size)) {
        ret=1;
        break;
      }
      ret=dbc->c_get(dbc, &bdb_key, &bdb_value, DB_NEXT);
      if(ret == DB_NOTFOUND)
        ret= 0;
      else if(ret) /* failed */
        ret= -1;
      else
        ret= 1;
    }
    if(dbc)
      dbc->c_close(dbc);
  }
#endif
#else
  /* V1 */
  if(!value) {
    /* don't care about value, can use standard get */
    ret=db->get(db, &bdb_key, &bdb_value, 0);
    if(ret >0) /* not found */
      ret= 0;
    else if(ret <0) /* failed */
      ret= -1;
    else /* 0 = found */
      ret= 1;
  } else {
    /* Want exact key/value - have to use sequence */

    ret=db->seq(bdb, &bdb_key, &bdb_value, 0);
    if(ret) /* failed */
      ret= -1;
    else
      ret= 1;

    while(ret > 0) {
      /* key different - match failed */
      if(memcmp(key->data, bdb_key.data, key->size)) {
        ret=0;
        break;
      }
      /* value equal - match found */
      if(!memcmp(value->data, bdb_value.data, value->size)) {
        ret=1;
        break;
      }
      ret=db->seq(dbc, &bdb_key, &bdb_value, R_NEXT);
      if(ret) /* not found */
        ret= 0;
      else
        ret= 1;
    }
  }
  
#endif

  return ret;
}


/**
 * librdf_hash_bdb_delete_key:
 * @context: BerkeleyDB hash context
 * @key: key
 *
 * Delete all values for given key from the hash.
 * 
 * Return value: non 0 on failure
 **/
static int
librdf_hash_bdb_delete_key(void* context, librdf_hash_datum *key) 
{
  librdf_hash_bdb_context* bdb_context=(librdf_hash_bdb_context*)context;
  DB* bdb=bdb_context->db;
  DBT bdb_key;
  int ret;

  memset(&bdb_key, 0, sizeof(DBT));

  /* Initialise BDB version of key */
  bdb_key.data = (char*)key->data;
  bdb_key.size = key->size;
  
#ifdef HAVE_BDB_DB_TXN
  /* V2/V3 */
  ret=bdb->del(bdb, NULL, &bdb_key, 0);
#else
  /* V1 */
  ret=bdb->del(bdb, &bdb_key, 0);
#endif
  if(ret)
    LIBRDF_DEBUG2("BDB del failed - %d\n", ret);

  return (ret != 0);
}


/**
 * librdf_hash_bdb_delete_key_value:
 * @context: BerkeleyDB hash context
 * @key: key
 * @value: value
 *
 * Delete given key/value from the hash.
 * 
 * Return value: non 0 on failure
 **/
static int
librdf_hash_bdb_delete_key_value(void* context, 
                                 librdf_hash_datum *key, librdf_hash_datum *value)
{
  librdf_hash_bdb_context* bdb_context=(librdf_hash_bdb_context*)context;
  DB* bdb=bdb_context->db;
  DBT bdb_key, bdb_value;
  int ret;
#ifdef HAVE_BDB_CURSOR
  DBC* dbc;
#endif

  memset(&bdb_key, 0, sizeof(DBT));
  memset(&bdb_value, 0, sizeof(DBT));

  /* Initialise BDB version of key */
  bdb_key.data = (char*)key->data;
  bdb_key.size = key->size;
  
  bdb_value.data = (char*)value->data;
  bdb_value.size = value->size;
  
#ifdef HAVE_BDB_CURSOR
#ifdef HAVE_BDB_CURSOR_4_ARGS
  /* V3 prototype:
   * int DB->cursor(DB *db, DB_TXN *txnid, DBC **cursorp, u_int32_t flags);
   */
  if(bdb->cursor(bdb, NULL, &dbc, 0))
    return 1;
#else
  /* V2 prototype:
   * int DB->cursor(DB *db, DB_TXN *txnid, DBC **cursorp);
   */
  if(bdb->cursor(bdb, NULL, &dbc))
    return 1;
#endif
  
  /* V2/V3 prototype:
   * int DBcursor->c_get(DBC *cursor, DBT *key, DBT *data, u_int32_t flags);
   */
#ifdef DB_GET_BOTH
  /* later V2 (sigh) / V3 */
  ret=dbc->c_get(dbc, &bdb_key, &bdb_value, DB_GET_BOTH);
#else
  /* earlier V2 probably gives a memory leak */
  ret=dbc->c_get(dbc, &bdb_key, &bdb_value, 0);
#endif
  if(ret) {
    dbc->c_close(dbc);
    return 1;
  }
  
  /* finally - delete the sucker */
  ret=dbc->c_del(dbc, 0);

  dbc->c_close(dbc);
#else
  /* V1 prototype:
   * int db->seq(DB* db, DBT *key, DBT *data, u_int flags);
   */
  ret=bdb->seq(bdb, &bdb_key, &bdb_value, 0);
  if(ret)
    return 1;
  
  /* V1 prototype:
   * int db->del(DB* db, DBT *key, u_int flags);
   */
  ret=bdb->del(bdb, &bdb_key, R_CURSOR);
#endif

  if(ret)
    LIBRDF_DEBUG2("BDB del failed - %d\n", ret);

  return (ret != 0);
}


/**
 * librdf_hash_bdb_sync:
 * @context: BerkeleyDB hash context
 *
 * Flush the hash to disk.
 * 
 * Return value: non 0 on failure
 **/
static int
librdf_hash_bdb_sync(void* context) 
{
  librdf_hash_bdb_context* bdb_context=(librdf_hash_bdb_context*)context;
  DB* db=bdb_context->db;
  int ret;

  return (ret=db->sync(db, 0));
}


/**
 * librdf_hash_bdb_get_fd:
 * @context: BerkeleyDB hash context
 *
 * Get the file description representing the hash.
 * 
 * Return value: the file descriptor
 **/
static int
librdf_hash_bdb_get_fd(void* context) 
{
  librdf_hash_bdb_context* bdb_context=(librdf_hash_bdb_context*)context;
  DB* db=bdb_context->db;
  int fd;
  int ret;
  
#ifdef HAVE_BDB_FD_2_ARGS
  ret=db->fd(db, &fd);
#else
  ret=0;
  fd=db->fd(db);
#endif
  return fd;
}


/* local function to register BDB hash functions */

/**
 * librdf_hash_bdb_register_factory:
 * @factory: hash factory prototype
 *
 * Register the BerkeleyDB hash module with the hash factory.
 * 
 **/
static void
librdf_hash_bdb_register_factory(librdf_hash_factory *factory) 
{
  factory->context_length = sizeof(librdf_hash_bdb_context);
  factory->cursor_context_length = sizeof(librdf_hash_bdb_cursor_context);
  
  factory->create  = librdf_hash_bdb_create;
  factory->destroy = librdf_hash_bdb_destroy;

  factory->open    = librdf_hash_bdb_open;
  factory->close   = librdf_hash_bdb_close;
  factory->clone   = librdf_hash_bdb_clone;

  factory->values_count = librdf_hash_bdb_values_count;

  factory->put     = librdf_hash_bdb_put;
  factory->exists  = librdf_hash_bdb_exists;
  factory->delete_key  = librdf_hash_bdb_delete_key;
  factory->delete_key_value  = librdf_hash_bdb_delete_key_value;
  factory->sync    = librdf_hash_bdb_sync;
  factory->get_fd  = librdf_hash_bdb_get_fd;

  factory->cursor_init   = librdf_hash_bdb_cursor_init;
  factory->cursor_get    = librdf_hash_bdb_cursor_get;
  factory->cursor_finish = librdf_hash_bdb_cursor_finish;
}


/**
 * librdf_init_hash_bdb:
 * @world: redland world object
 *
 * Initialise the BerkeleyDB hash module.
 *
 **/
void
librdf_init_hash_bdb(librdf_world *world)
{
  librdf_hash_register_factory(world,
                               "bdb", &librdf_hash_bdb_register_factory);
}
