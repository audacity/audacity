/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_set.c - Sets for checking IDs
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
#include <stdarg.h>
#include <sys/types.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h> /* for abort() as used in errors */
#endif

/* Raptor includes */
#include "raptor.h"
#include "raptor_internal.h"


#ifndef STANDALONE

/*
 * The only methods needed here are:
 *  Create Set
 *  Destroy Set
 *  Check a (base, ID) pair present add it if not, return if added/not
 *
 */

struct raptor_base_id_set_s
{
  /* The base URI of this set of IDs */
  raptor_uri *uri;
  
  /* neighbour ID sets */
  struct raptor_base_id_set_s* prev;
  struct raptor_base_id_set_s* next;

  /* binary tree */
  raptor_avltree* tree;
};
typedef struct raptor_base_id_set_s raptor_base_id_set;


struct raptor_id_set_s
{
  /* start of trees, 1 per base URI */
  struct raptor_base_id_set_s* first;

#if RAPTOR_DEBUG > 1
  int hits;
  int misses;
#endif
};


/* functions implementing the ID set api */

/**
 * raptor_new_id_set:
 *
 * INTERNAL - Constructor - create a new ID set.
 * 
 * Return value: non 0 on failure
 **/
raptor_id_set*
raptor_new_id_set(void) 
{
  raptor_id_set* set=(raptor_id_set*)RAPTOR_CALLOC(raptor_id_set, 1, 
                                                   sizeof(raptor_id_set));
  if(!set)
    return NULL;
  
  return set;
}


/**
 * raptor_free_base_id_set:
 * @set: #raptor_base_id_set
 *
 * INTERNAL - Destructor - Free a Base ID Set.
 *
 **/
static void
raptor_free_base_id_set(raptor_base_id_set *base) 
{
  if(base->tree)
    raptor_free_avltree(base->tree);
  if(base->uri)
    raptor_free_uri(base->uri);
  RAPTOR_FREE(raptor_base_id_set, base);
}


/**
 * raptor_free_id_set:
 * @set: #raptor_id_set
 *
 * INTERNAL - Destructor - Free ID Set.
 *
 **/
void
raptor_free_id_set(raptor_id_set *set) 
{
  raptor_base_id_set *base;

  RAPTOR_ASSERT_OBJECT_POINTER_RETURN(set, raptor_id_set);

  base=set->first;
  while(base) {
    raptor_base_id_set *next=base->next;
    raptor_free_base_id_set(base);
    base=next;
  }
  RAPTOR_FREE(raptor_id_set, set);
}



/**
 * raptor_id_set_add:
 * @set: #raptor_id_set
 * @base_uri: base #raptor_uri of identifier
 * @id: identifier name
 * @id_len: length of identifier
 *
 * INTERNAL - Add an item to the set.
 * 
 * Return value: <0 on failure, 0 on success, 1 if already present
 **/
int
raptor_id_set_add(raptor_id_set* set, raptor_uri *base_uri,
                  const unsigned char *id, size_t id_len)
{
  raptor_base_id_set *base;
  char* item;
  
  if(!base_uri || !id || !id_len)
    return -1;

  base=set->first;
  while(base) {
    if(raptor_uri_equals(base->uri, base_uri))
      break;
    base=base->next;
  }

  if(!base) {
    /* a set for this base_uri not found */
    base=(raptor_base_id_set*)RAPTOR_CALLOC(raptor_base_id_set, 1, 
                                            sizeof(raptor_base_id_set));
    if(!base)
      return -1;

    base->uri=raptor_uri_copy(base_uri);

    base->tree=raptor_new_avltree((raptor_data_compare_function)strcmp,
                                  free, 0);
  
    /* Add to the start of the list */
    if(set->first)
      set->first->prev=base;
    /* base->prev=NULL; */
    base->next=set->first;

    set->first=base;
  } else {
    /* If not at the start of the list, move there */
    if(base != set->first) {
      /* remove from the list */
      base->prev->next=base->next;
      if(base->next)
        base->next->prev=base->prev;
      /* add at the start of the list */
      set->first->prev=base;
      base->prev=NULL;
      base->next=set->first;
    }
  }
  
  item=(char*)raptor_avltree_search(base->tree, id);

  /* if already there, error */
  if(item) {
#if RAPTOR_DEBUG > 1
    set->misses++;
#endif
    return 1;
  }
  
#if RAPTOR_DEBUG > 1
  set->hits++;
#endif
  
  item=(char*)RAPTOR_MALLOC(cstring, id_len+1);
  if(!item)
    return 1;

  strncpy(item, (const char*)id, id_len+1);

  return raptor_avltree_add(base->tree, item);
}


#if RAPTOR_DEBUG > 1
void
raptor_id_set_stats_print(raptor_id_set* set, FILE *stream) {
  fprintf(stream, "set hits: %d misses: %d\n", set->hits, set->misses);
}
#endif

#endif


#ifdef STANDALONE

/* one more prototype */
int main(int argc, char *argv[]);


int
main(int argc, char *argv[]) 
{
  const char *program=raptor_basename(argv[0]);
  const char *items[8] = { "ron", "amy", "jen", "bij", "jib", "daj", "jim", NULL };
  raptor_id_set *set;
  raptor_uri *base_uri;
  int i=0;
  
  raptor_init();
  
  base_uri=raptor_new_uri((const unsigned char*)"http://example.org/base#");

#if RAPTOR_DEBUG > 1
  fprintf(stderr, "%s: Creating set\n", program);
#endif

  set=raptor_new_id_set();
  if(!set) {
    fprintf(stderr, "%s: Failed to create set\n", program);
    exit(1);
  }

  for(i=0; items[i]; i++) {
    size_t len=strlen(items[i]);
    int rc;

#if RAPTOR_DEBUG > 1
    fprintf(stderr, "%s: Adding set item '%s'\n", program, items[i]);
#endif
  
    rc=raptor_id_set_add(set, base_uri, (const unsigned char*)items[i], len);
if(rc) {
      fprintf(stderr, "%s: Adding set item %d '%s' failed, returning error %d\n",
              program, i, items[i], rc);
      exit(1);
    }
  }

  for(i=0; items[i]; i++) {
    size_t len=strlen(items[i]);
    int rc;

#if RAPTOR_DEBUG > 1
    fprintf(stderr, "%s: Adding duplicate set item '%s'\n", program, items[i]);
#endif

    rc=raptor_id_set_add(set, base_uri, (const unsigned char*)items[i], len);
    if(rc <= 0) {
      fprintf(stderr, "%s: Adding duplicate set item %d '%s' succeeded, should have failed, returning error %d\n",
              program, i, items[i], rc);
      exit(1);
    }
  }

#if RAPTOR_DEBUG > 1
  raptor_id_set_stats_print(set, stderr);
#endif

#if RAPTOR_DEBUG > 1
  fprintf(stderr, "%s: Freeing set\n", program);
#endif
  raptor_free_id_set(set);

  raptor_free_uri(base_uri);
  
  raptor_finish();
  
  /* keep gcc -Wall happy */
  return(0);
}

#endif
