/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_hash_memory.c - RDF Hash In Memory Implementation
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

#ifdef HAVE_STDLIB_H
#include <stdlib.h> /* for abort() as used in errors */
#endif

#include <redland.h>
#include <rdf_types.h>


/* private structures */
struct librdf_hash_memory_node_value_s
{
  struct librdf_hash_memory_node_value_s* next;
  void *value;
  size_t value_len;
};
typedef struct librdf_hash_memory_node_value_s librdf_hash_memory_node_value;


struct librdf_hash_memory_node_s
{
  struct librdf_hash_memory_node_s* next;
  void *key;
  size_t key_len;
  u32 hash_key;
  librdf_hash_memory_node_value *values;
  int values_count;
};
typedef struct librdf_hash_memory_node_s librdf_hash_memory_node;


typedef struct
{
  /* the hash object */
  librdf_hash* hash;
  /* An array pointing to a list of nodes (buckets) */
  librdf_hash_memory_node** nodes;
  /* this many buckets used */
  int size;
  /* this many keys */
  int keys;
  /* this many values */
  int values;
  /* total array size */
  int capacity;

  /* array load factor expressed out of 1000.
   * Always true: (size/capacity * 1000) < load_factor,
   * or in the code: size * 1000 < load_factor * capacity
   */
  int load_factor;
} librdf_hash_memory_context;



/* default load_factor out of 1000 */
static const int librdf_hash_default_load_factor=750;

/* starting capacity - MUST BE POWER OF 2 */
static const int librdf_hash_initial_capacity=8;


/* prototypes for local functions */
static librdf_hash_memory_node* librdf_hash_memory_find_node(librdf_hash_memory_context* hash, void *key, size_t key_len, int *bucket, librdf_hash_memory_node** prev);
static void librdf_free_hash_memory_node(librdf_hash_memory_node* node);
static int librdf_hash_memory_expand_size(librdf_hash_memory_context* hash);

/* Implementing the hash cursor */
static int librdf_hash_memory_cursor_init(void *cursor_context, void *hash_context);
static int librdf_hash_memory_cursor_get(void* context, librdf_hash_datum* key, librdf_hash_datum* value, unsigned int flags);
static void librdf_hash_memory_cursor_finish(void* context);




/* functions implementing the API */

static int librdf_hash_memory_create(librdf_hash* new_hash, void* context);
static int librdf_hash_memory_destroy(void* context);
static int librdf_hash_memory_open(void* context, const char *identifier, int mode, int is_writable, int is_new, librdf_hash* options);
static int librdf_hash_memory_close(void* context);
static int librdf_hash_memory_clone(librdf_hash* new_hash, void *new_context, char *new_identifier, void* old_context);
static int librdf_hash_memory_values_count(void *context);
static int librdf_hash_memory_put(void* context, librdf_hash_datum *key, librdf_hash_datum *data);
static int librdf_hash_memory_exists(void* context, librdf_hash_datum *key, librdf_hash_datum *value);
static int librdf_hash_memory_delete_key(void* context, librdf_hash_datum *key);
static int librdf_hash_memory_delete_key_value(void* context, librdf_hash_datum *key, librdf_hash_datum *value);
static int librdf_hash_memory_sync(void* context);
static int librdf_hash_memory_get_fd(void* context);

static void librdf_hash_memory_register_factory(librdf_hash_factory *factory);




/*
 * perldelta 5.8.0 says under *Performance Enhancements*
 *
 *   Hashes now use Bob Jenkins "One-at-a-Time" hashing key algorithm
 *   http://burtleburtle.net/bob/hash/doobs.html  This algorithm is
 *   reasonably fast while producing a much better spread of values
 *   than the old hashing algorithm ...
 *
 * Changed here to hash the string backwards to help do URIs better
 *
 */

#define ONE_AT_A_TIME_HASH(hash,str,len) \
     do { \
        register const unsigned char *c_oneat = (unsigned char*)str+len-1; \
        register int i_oneat = len; \
        register u32 hash_oneat = 0; \
        while (i_oneat--) { \
            hash_oneat += *c_oneat--; \
            hash_oneat += (hash_oneat << 10); \
            hash_oneat ^= (hash_oneat >> 6); \
        } \
        hash_oneat += (hash_oneat << 3); \
        hash_oneat ^= (hash_oneat >> 11); \
        (hash) = (hash_oneat + (hash_oneat << 15)); \
    } while(0)



/* helper functions */


/**
 * librdf_hash_memory_find_node:
 * @hash: the memory hash context
 * @key: key string
 * @key_len: key string length
 * @user_bucket: pointer to store bucket
 * @prev: pointer to store previous node
 *
 * Find the node for the given key or value.
 * 
 * If value is not NULL and value_len is non 0, the value will also be
 * compared in the search.
 *
 * If user_bucket is not NULL, the bucket used will be returned.  if
 * prev is no NULL, the previous node in the list will be returned.
 * 
 * Return value: #librdf_hash_memory_node of content or NULL on failure
 **/
static librdf_hash_memory_node*
librdf_hash_memory_find_node(librdf_hash_memory_context* hash, 
			     void *key, size_t key_len,
			     int *user_bucket,
			     librdf_hash_memory_node** prev) 
{
  librdf_hash_memory_node* node;
  int bucket;
  u32 hash_key;

  /* empty hash */
  if(!hash->capacity)
    return NULL;
  
  ONE_AT_A_TIME_HASH(hash_key, key, key_len);

  if(prev)
    *prev=NULL;

  /* find slot in table */
  bucket=hash_key & (hash->capacity - 1);
  if(user_bucket)
    *user_bucket=bucket;

  /* check if there is a list present */ 
  node=hash->nodes[bucket];
  if(!node)
    /* no list there */
    return NULL;
    
  /* walk the list */
  while(node) {
    if(key_len == node->key_len && !memcmp(key, node->key, key_len))
      break;
    if(prev)
      *prev=node;
    node=node->next;
  }

  return node;
}


static void
librdf_free_hash_memory_node(librdf_hash_memory_node* node) 
{
  if(node->key)
    LIBRDF_FREE(cstring, node->key);
  if(node->values) {
    librdf_hash_memory_node_value *vnode, *next;

    /* Empty the list of values */
    for(vnode=node->values; vnode; vnode=next) {
      next=vnode->next;
      if(vnode->value)
        LIBRDF_FREE(cstring, vnode->value);
      LIBRDF_FREE(librdf_hash_memory_node_value, vnode);
    }
  }
  LIBRDF_FREE(librdf_hash_memory_node, node);
}


static int
librdf_hash_memory_expand_size(librdf_hash_memory_context* hash) {
  int required_capacity=0;
  librdf_hash_memory_node **new_nodes;
  int i;

  if (hash->capacity) {
    /* big enough */
    if((1000 * hash->keys) < (hash->load_factor * hash->capacity))
      return 0;
    /* grow hash (keeping it a power of two) */
    required_capacity=hash->capacity << 1;
  } else {
    required_capacity=librdf_hash_initial_capacity;
  }

  /* allocate new table */
  new_nodes=(librdf_hash_memory_node**)LIBRDF_CALLOC(librdf_hash_memory_nodes, 
						     required_capacity,
						     sizeof(librdf_hash_memory_node*));
  if(!new_nodes)
    return 1;


  /* it is a new hash empty hash - we are done */
  if(!hash->size) {
    hash->capacity=required_capacity;
    hash->nodes=new_nodes;
    return 0;
  }
  

  for(i=0; i<hash->capacity; i++) {
    librdf_hash_memory_node *node=hash->nodes[i];
      
    /* walk all attached nodes */
    while(node) {
      librdf_hash_memory_node *next;
      int bucket;

      next=node->next;
      /* find slot in new table */
      bucket=node->hash_key & (required_capacity - 1);
      node->next=new_nodes[bucket];
      new_nodes[bucket]=node;

      node=next;
    }
  }

  /* now free old table */
  LIBRDF_FREE(librdf_hash_memory_nodes, hash->nodes);

  /* attach new one */
  hash->capacity=required_capacity;
  hash->nodes=new_nodes;

  return 0;
}



/* functions implementing hash api */

/**
 * librdf_hash_memory_create:
 * @hash: #librdf_hash hash
 * @context: memory hash contxt
 *
 * Create a new memory hash.
 * 
 * Return value: non 0 on failure
 **/
static int
librdf_hash_memory_create(librdf_hash* hash, void* context) 
{
  librdf_hash_memory_context* hcontext=(librdf_hash_memory_context*)context;

  hcontext->hash=hash;
  hcontext->load_factor=librdf_hash_default_load_factor;
  return librdf_hash_memory_expand_size(hcontext);
}


/**
 * librdf_hash_memory_destroy:
 * @context: memory hash context
 *
 * Destroy a memory hash.
 * 
 * Return value: non 0 on failure
 **/
static int
librdf_hash_memory_destroy(void* context) 
{
  librdf_hash_memory_context* hcontext=(librdf_hash_memory_context*)context;

  if(hcontext->nodes) {
    int i;
  
    for(i=0; i<hcontext->capacity; i++) {
      librdf_hash_memory_node *node=hcontext->nodes[i];
      
      /* this entry is used */
      if(node) {
        librdf_hash_memory_node *next;
        /* free all attached nodes */
        while(node) {
          next=node->next;
          librdf_free_hash_memory_node(node);
          node=next;
        }
      }
    }
    LIBRDF_FREE(librdf_hash_memory_nodes, hcontext->nodes);
  }

  return 0;
}


/**
 * librdf_hash_memory_open:
 * @context: memory hash context
 * @identifier: identifier - not used
 * @mode: access mode - not used
 * @is_writable: is hash writable? - not used
 * @is_new: is hash new? - not used
 * @options: #librdf_hash of options - not used
 *
 * Open memory hash with given parameters.
 * 
 * Return value: non 0 on failure
 **/
static int
librdf_hash_memory_open(void* context, const char *identifier,
                        int mode, int is_writable, int is_new,
                        librdf_hash* options) 
{
  /* NOP */
  return 0;
}


/**
 * librdf_hash_memory_close:
 * @context: memory hash context
 *
 * Close the hash.
 * 
 * Return value: non 0 on failure
 **/
static int
librdf_hash_memory_close(void* context) 
{
  /* NOP */
  return 0;
}


static int
librdf_hash_memory_clone(librdf_hash *hash, void* context, char *new_identifer,
                         void *old_context) 
{
  librdf_hash_memory_context* hcontext=(librdf_hash_memory_context*)context;
  librdf_hash_memory_context* old_hcontext=(librdf_hash_memory_context*)old_context;
  librdf_hash_datum *key, *value;
  librdf_iterator *iterator;
  int status=0;
  
  /* copy data fields that might change */
  hcontext->hash=hash;
  hcontext->load_factor=old_hcontext->load_factor;

  /* Don't need to deal with new_identifier - not used for memory hashes */

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

    if(librdf_hash_memory_put(hcontext, k, v)) {
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
 * librdf_hash_memory_values_count:
 * @context: memory hash cursor context
 *
 * Get the number of values in the hash.
 * 
 * Return value: number of values in the hash or <0 on failure
 **/
static int
librdf_hash_memory_values_count(void *context) 
{
  librdf_hash_memory_context* hash=(librdf_hash_memory_context*)context;

  return hash->values;
}



typedef struct {
  librdf_hash_memory_context* hash;
  int current_bucket;
  librdf_hash_memory_node* current_node;
  librdf_hash_memory_node_value *current_value;
} librdf_hash_memory_cursor_context;



/**
 * librdf_hash_memory_cursor_init:
 * @cursor_context: hash cursor context
 * @hash_context: hash to operate over
 *
 * Initialise a new hash cursor.
 * 
 * Return value: non 0 on failure
 **/
static int
librdf_hash_memory_cursor_init(void *cursor_context, void *hash_context) 
{
  librdf_hash_memory_cursor_context *cursor=(librdf_hash_memory_cursor_context*)cursor_context;

  cursor->hash = (librdf_hash_memory_context*)hash_context;
  return 0;
}


/**
 * librdf_hash_memory_cursor_get:
 * @context: memory hash cursor context
 * @key: pointer to key to use
 * @value: pointer to value to use
 * @flags: flags
 *
 * Retrieve a hash value for the given key.
 * 
 * Return value: non 0 on failure
 **/
static int
librdf_hash_memory_cursor_get(void* context, 
                              librdf_hash_datum *key,
                              librdf_hash_datum *value,
                              unsigned int flags)
{
  librdf_hash_memory_cursor_context *cursor=(librdf_hash_memory_cursor_context*)context;
  librdf_hash_memory_node_value *vnode=NULL;
  librdf_hash_memory_node *node;
  

  /* First step, make sure cursor->current_node points to a valid node,
     if possible */

  /* Move to start of hash if necessary  */
  if(flags == LIBRDF_HASH_CURSOR_FIRST) {
    int i;
    
    cursor->current_node=NULL;
    /* find first used bucket (with keys) */
    cursor->current_bucket=0;

    for(i=0; i< cursor->hash->capacity; i++)
      if((cursor->current_node=cursor->hash->nodes[i])) {
        cursor->current_bucket=i;
        break;
      }

    if(cursor->current_node)
      cursor->current_value=cursor->current_node->values;
  }

  /* If still have no current node, try to find it from the key */
  if(!cursor->current_node && key && key->data) {
    cursor->current_node=librdf_hash_memory_find_node(cursor->hash,
                                                      (char*)key->data,
                                                      key->size,
                                                      NULL, NULL);
    if(cursor->current_node)
      cursor->current_value=cursor->current_node->values;
  }


  /* If still have no node, failed */
  if(!cursor->current_node)
    return 1;

  /* Check for end of values */

  switch(flags) {
    case LIBRDF_HASH_CURSOR_SET:
      /* If key does not exist, failed above, so test if there are values */

      /* FALLTHROUGH */
    case LIBRDF_HASH_CURSOR_NEXT_VALUE:
      /* If want values and have reached end of values list, end */
      if(!cursor->current_value)
        return 1;
      break;
      
    case LIBRDF_HASH_CURSOR_FIRST:
    case LIBRDF_HASH_CURSOR_NEXT:
      /* If have reached last bucket, end */
      if(cursor->current_bucket >= cursor->hash->capacity)
        return 1;
      
      break;
    default:
      librdf_log(cursor->hash->hash->world,
                 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_HASH, NULL,
                 "Unknown hash method flag %d", flags);
      return 1;
  }
  

  /* Ok, there is data, retrieve it */

  switch(flags) {
    case LIBRDF_HASH_CURSOR_SET:

      /* FALLTHROUGH */
    case LIBRDF_HASH_CURSOR_NEXT_VALUE:
      vnode=cursor->current_value;
      
      /* copy value */
      value->data=vnode->value;
      value->size=vnode->value_len;
      
      /* move on */
      cursor->current_value=vnode->next;
      break;
      
    case LIBRDF_HASH_CURSOR_FIRST:
    case LIBRDF_HASH_CURSOR_NEXT:
      node=cursor->current_node;

      /* get key */
      key->data= node->key;
      key->size= node->key_len;

      /* if want values, walk through them */
      if(value) {
        vnode=cursor->current_value;
        
        /* get value */
        value->data=vnode->value;
        value->size=vnode->value_len;

        /* move on */
        cursor->current_value=vnode->next;
        
        /* stop here if there are more values, otherwise need next
         * key & values so drop through and move to the next node
         */
        if(cursor->current_value)
          break;
      }
      
      /* move on to next node in current bucket */
      if(!(node=cursor->current_node->next)) {
        int i;
        
        /* end of list - move to next used bucket */
        for(i=cursor->current_bucket+1; i< cursor->hash->capacity; i++)
          if((node=cursor->hash->nodes[i])) {
            cursor->current_bucket=i;
            break;
          }
        
      }
      
      if((cursor->current_node=node))
        cursor->current_value=node->values;
      
      break;
    default:
      librdf_log(cursor->hash->hash->world,
                 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_HASH, NULL,
                 "Unknown hash method flag %d", flags);
      return 1;
  }
  

  return 0;
}


/**
 * librdf_hash_memory_cursor_finished:
 * @context: hash memory get iterator context
 *
 * Finish the serialisation of the hash memory get.
 *
 **/
static void
librdf_hash_memory_cursor_finish(void* context)
{
/* librdf_hash_memory_cursor_context *cursor=(librdf_hash_memory_cursor_context*)context; */

}


/**
 * librdf_hash_memory_put:
 * @context: memory hash context
 * @key: pointer to key to store
 * @value: pointer to value to store
 *
 * - Store a key/value pair in the hash.
 * 
 * Return value: non 0 on failure
 **/
static int
librdf_hash_memory_put(void* context, librdf_hash_datum *key, 
		       librdf_hash_datum *value) 
{
  librdf_hash_memory_context* hash=(librdf_hash_memory_context*)context;
  librdf_hash_memory_node *node;
  librdf_hash_memory_node_value *vnode;
  u32 hash_key;
  void *new_key=NULL;
  void *new_value;
  int bucket= (-1);
  int is_new_node;

  /* ensure there is enough space in the hash */
  if (librdf_hash_memory_expand_size(hash))
    return 1;
  
  /* find node for key */
  node=librdf_hash_memory_find_node(hash,
				    key->data, key->size,
				    NULL, NULL);

  is_new_node=(node == NULL);
  
  /* not found - new key */
  if(is_new_node) {
    ONE_AT_A_TIME_HASH(hash_key, key->data, key->size);

    bucket=hash_key & (hash->capacity - 1);

    /* allocate new node */
    node=(librdf_hash_memory_node*)LIBRDF_CALLOC(librdf_hash_memory_node, 1,
                                                 sizeof(librdf_hash_memory_node));
    if(!node)
      return 1;

    node->hash_key=hash_key;
    
    /* allocate key for new node */
    new_key=LIBRDF_MALLOC(cstring, key->size);
    if(!new_key) {
      LIBRDF_FREE(librdf_hash_memory_node, node);
      return 1;
    }

    /* copy new key */
    memcpy(new_key, key->data, key->size);
    node->key=new_key;
    node->key_len=key->size;
  }
  
  
  /* always allocate new value */
  new_value=LIBRDF_MALLOC(cstring, value->size);
  if(!new_value) {
    if(is_new_node) {
      LIBRDF_FREE(cstring, new_key);
      LIBRDF_FREE(librdf_hash_memory_node, node);
    }
    return 1;
  }

  /* always allocate new librdf_hash_memory_node_value */
  vnode=(librdf_hash_memory_node_value*)LIBRDF_CALLOC(librdf_hash_memory_node_value, 1, sizeof(librdf_hash_memory_node_value));
  if(!vnode) {
    LIBRDF_FREE(cstring, new_value);
    if(is_new_node) {
      LIBRDF_FREE(cstring, new_key);
      LIBRDF_FREE(librdf_hash_memory_node, node);
    }
    return 1;
  }

  /* if we get here, all allocations succeeded */


  /* put new value node in list */
  vnode->next=node->values;
  node->values=vnode;

  /* note that in counter */
  node->values_count++;
 
  /* copy new value */
  memcpy(new_value, value->data, value->size);
  vnode->value=new_value;
  vnode->value_len=value->size;


  /* now update buckets and hash counts */
  if(is_new_node) {
    node->next=hash->nodes[bucket];
    hash->nodes[bucket]=node;
  
    hash->keys++;
  }
  

  hash->values++;

  /* Only increase bucket count use when previous value was NULL */
  if(!node->next)
    hash->size++;

  return 0;
}


/**
 * librdf_hash_memory_exists:
 * @context: memory hash context
 * @key: key
 * @value: value
 *
 * Test the existence of a key in the hash.
 * 
 * Return value: >0 if the key/value exists in the hash, 0 if not, <0 on failure
 **/
static int
librdf_hash_memory_exists(void* context, 
                          librdf_hash_datum *key, librdf_hash_datum *value)
{
  librdf_hash_memory_context* hash=(librdf_hash_memory_context*)context;
  librdf_hash_memory_node* node;
  librdf_hash_memory_node_value *vnode;
  
  node=librdf_hash_memory_find_node(hash,
				    (char*)key->data, key->size,
				    NULL, NULL);
  /* key not found */
  if(!node)
    return 0;
  
  /* no value wanted */
  if(!value)
    return 1;

  /* search for value in list of values */
  for(vnode=node->values; vnode; vnode=vnode->next) {
    if(value->size == vnode->value_len && 
       !memcmp(value->data, vnode->value, value->size))
      break;
  }

  return (vnode != NULL);
}



/**
 * librdf_hash_memory_delete_key_value:
 * @context: memory hash context
 * @key: pointer to key to delete
 * @value: pointer to value to delete
 *
 * - Delete a key/value pair from the hash.
 * 
 * Return value: non 0 on failure
 **/
static int
librdf_hash_memory_delete_key_value(void* context, librdf_hash_datum *key,
                                    librdf_hash_datum *value)
{
  librdf_hash_memory_context* hash=(librdf_hash_memory_context*)context;
  librdf_hash_memory_node *node, *prev, *next;
  librdf_hash_memory_node_value *vnode, *vprev;
  int bucket;
  
  node=librdf_hash_memory_find_node(hash, 
				    (char*)key->data, key->size,
				    &bucket, &prev);
  /* key not found anywhere */
  if(!node)
    return 1;

  /* search for value in list of values */
  vnode=node->values;
  vprev=NULL;
  while(vnode) {
    if(value->size == vnode->value_len && 
       !memcmp(value->data, vnode->value, value->size))
      break;
    vprev=vnode;
    vnode=vnode->next;
  }

  /* key/value combination not found */
  if(!vnode)
    return 1;

  /* found - delete it from list */
  if(!vprev) {
    /* at start of list so delete from there */
    node->values=vnode->next;
  } else
    vprev->next=vnode->next;

  /* free value and value node */
  if(vnode->value)
    LIBRDF_FREE(librdf_hash_memory_node_value, vnode->value);
  LIBRDF_FREE(librdf_hash_memory_node_value, vnode);

  /* update hash counts */
  hash->values--;

  /* check if last value was removed */
  if(node->values)
    /* no, so return success */
    return 0;
  

  /* yes - all values gone so need to delete entire key node */

  if(!prev) {
    /* is at start of list, so delete from there */
    if(!(hash->nodes[bucket]=node->next))
      /* hash bucket occupancy is one less if bucket is now empty */
      hash->size--;
    next=NULL;
  } else
    next=prev->next=node->next;
  
  /* free node */
  librdf_free_hash_memory_node(node);
  
  /* see if there are remaining values for this key */
  if(!next) {
    /* no - so was last value for that key, reduce key count */
    hash->keys--;
  } else {
    int found=0;
    
    node=next;
    while(node) {
      if(key->size == node->key_len && !memcmp(key, node->key, key->size)){
        found=1;
        break;
      }
      node=node->next;
    }
    
    /* no further key values found - so was last value for that key */
    if(!found)
      hash->keys--;
  }

  return 0;
}


/**
 * librdf_hash_memory_delete_key:
 * @context: memory hash context
 * @key: pointer to key to delete
 *
 * - Delete a key and all its values from the hash.
 * 
 * Return value: non 0 on failure
 **/
static int
librdf_hash_memory_delete_key(void* context, librdf_hash_datum *key) 
{
  librdf_hash_memory_context* hash=(librdf_hash_memory_context*)context;
  librdf_hash_memory_node *node, *prev;
  int bucket;
  
  node=librdf_hash_memory_find_node(hash, 
				    (char*)key->data, key->size,
				    &bucket, &prev);
  /* not found anywhere */
  if(!node)
    return 1;

  /* search list from here */
  if(!prev) {
    /* is at start of list, so delete from there */
    if(!(hash->nodes[bucket]=node->next))
      /* hash bucket occupancy is one less if bucket is now empty */
      hash->size--;
  } else
    prev->next=node->next;

  /* update hash counts */
  hash->keys--;
  hash->values-= node->values_count;
  
  /* free node */
  librdf_free_hash_memory_node(node);
  return 0;
}


/**
 * librdf_hash_memory_sync:
 * @context: memory hash context
 *
 * Flush the hash to disk.
 * 
 * Not used
 * 
 * Return value: 0
 **/
static int
librdf_hash_memory_sync(void* context) 
{
  /* Not applicable */
  return 0;
}


/**
 * librdf_hash_memory_get_fd:
 * @context: memory hash context
 *
 * Get the file descriptor representing the hash.
 * 
 * Not used
 * 
 * Return value: -1
 **/
static int
librdf_hash_memory_get_fd(void* context) 
{
  /* Not applicable */
  return -1;
}


/* local function to register memory hash functions */

/**
 * librdf_hash_memory_register_factory:
 * @factory: hash factory prototype
 *
 * Register the memory hash module with the hash factory.
 * 
 **/
static void
librdf_hash_memory_register_factory(librdf_hash_factory *factory) 
{
  factory->context_length = sizeof(librdf_hash_memory_context);
  factory->cursor_context_length = sizeof(librdf_hash_memory_cursor_context);
  
  factory->create  = librdf_hash_memory_create;
  factory->destroy = librdf_hash_memory_destroy;

  factory->open    = librdf_hash_memory_open;
  factory->close   = librdf_hash_memory_close;
  factory->clone   = librdf_hash_memory_clone;

  factory->values_count = librdf_hash_memory_values_count;

  factory->put     = librdf_hash_memory_put;
  factory->exists  = librdf_hash_memory_exists;
  factory->delete_key  = librdf_hash_memory_delete_key;
  factory->delete_key_value  = librdf_hash_memory_delete_key_value;
  factory->sync    = librdf_hash_memory_sync;
  factory->get_fd  = librdf_hash_memory_get_fd;

  factory->cursor_init   = librdf_hash_memory_cursor_init;
  factory->cursor_get    = librdf_hash_memory_cursor_get;
  factory->cursor_finish = librdf_hash_memory_cursor_finish;
}

/**
 * librdf_init_hash_memory:
 * @world: redland world object
 *
 * Initialise the memory hash module.
 * 
 * Initialises the memory hash module and sets the default hash load factor.
 *
 * The recommended and current default value is 0.75, i.e. 750/1000.  
 * To use the default value (whatever it is) use a value less than 0.
 **/
void
librdf_init_hash_memory(librdf_world *world) 
{
  /* use default load factor */
  if(world->hash_load_factor <= 0 || world->hash_load_factor > 999)
    world->hash_load_factor=librdf_hash_default_load_factor;

  librdf_hash_register_factory(world,
                               "memory", &librdf_hash_memory_register_factory);
}
