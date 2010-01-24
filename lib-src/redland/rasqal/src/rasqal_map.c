/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rasqal_map.c - Rasqal simple Key:Value Map with duplicates allowed
 *
 * Copyright (C) 2005-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2005-2005, University of Bristol, UK http://www.bristol.ac.uk/
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
#include <rasqal_config.h>
#endif

#ifdef WIN32
#include <win32_rasqal_config.h>
#endif

#include <stdio.h>
#include <string.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#include <stdarg.h>

#include "rasqal.h"
#include "rasqal_internal.h"


struct rasqal_map_node_s
{
  struct rasqal_map_s* map;
  struct rasqal_map_node_s* prev;
  struct rasqal_map_node_s* next;
  void* key;
  void* value;
};

struct rasqal_map_s {
  struct rasqal_map_node_s* root;
  rasqal_compare_fn* compare;
  rasqal_kv_free_fn* free;
  raptor_sequence_print_handler* print_key;
  raptor_sequence_print_handler* print_value;
  int allow_duplicates;
};

typedef struct rasqal_map_node_s rasqal_map_node;


static rasqal_map_node*
rasqal_new_map_node(rasqal_map* map, void *key, void *value)
{
  rasqal_map_node *node;
  node=(rasqal_map_node*)RASQAL_CALLOC(rasqal_map_node, 1, sizeof(rasqal_map_node));
  if(!node)
    return NULL;
  node->map=map;
  node->key=key;
  node->value=value;
  return node;
}


static void
rasqal_free_map_node(rasqal_map_node *node, rasqal_kv_free_fn* free_fn) 
{
  RASQAL_ASSERT_OBJECT_POINTER_RETURN(node, rasqal_map_node);
  
  if(node->prev)
    rasqal_free_map_node(node->prev, free_fn);
  if(node->next)
    rasqal_free_map_node(node->next, free_fn);

  free_fn(node->key, node->value);

  RASQAL_FREE(rasqal_map_node, node);
}


/**
 * rasqal_new_map:
 * @compare_fn: comparison function for keys
 * @free_fn: free function for (key, value) pair
 * @print_key_fn: print a key function (or NULL)
 * @print_value_fn: print a value function (or NULL)
 * @flags: non-0 to allow duplicates
 *
 * Constructor - Create a (key:value) map.
 * 
 * Return value: a new #rasqal_map or NULL on failure
 **/
rasqal_map*
rasqal_new_map(rasqal_compare_fn* compare_fn,
               rasqal_kv_free_fn* free_fn,
               raptor_sequence_print_handler* print_key_fn,
               raptor_sequence_print_handler* print_value_fn,
               int flags)
{
  rasqal_map *map;
  map=(rasqal_map*)RASQAL_CALLOC(rasqal_map, 1, sizeof(rasqal_map));
  if(!map)
    return NULL;

  map->compare=compare_fn;
  map->free=free_fn;
  map->print_key=print_key_fn;
  map->print_value=print_value_fn;
  map->allow_duplicates=flags;
  
  return map;
}


/**
 * rasqal_free_map:
 * @map: the #rasqal_map to free
 *
 * Destructor - Destroy a (key:value) map.
 * 
 **/
void
rasqal_free_map(rasqal_map *map)
{
  RASQAL_ASSERT_OBJECT_POINTER_RETURN(map, rasqal_map);
  
  if(map->root)
    rasqal_free_map_node(map->root, map->free);

  RASQAL_FREE(rasqal_map, map);
}


static int
rasqal_map_node_add_kv(rasqal_map_node* node, void *key, void *value) 
{
  int result;

  result=node->map->compare(&key, &node->key);
  if(result < 0) {
    if(node->prev)
      return rasqal_map_node_add_kv(node->prev, key, value);
    node->prev=rasqal_new_map_node(node->map, key, value);
    return node->prev ? 0 : -1;
  } else if(!result) {
    if(!node->map->allow_duplicates) {
      /* duplicate and not allowed */
      return 1;
    }
    /* duplicate, fall through  */
  } 

  /* result > 0 */
  if(node->next)
    return rasqal_map_node_add_kv(node->next, key, value);

  node->next=rasqal_new_map_node(node->map, key, value);
  return node->next ? 0 : -1;
}


/**
 * rasqal_map_add_kv:
 * @map: #rasqal_map adding to
 * @key: key data
 * @value: value data (or NULL)
 *
 * Add a (key, value) pair to the map.
 * 
 * Return value: non-0 on failure including adding a duplicate.
 **/
int
rasqal_map_add_kv(rasqal_map* map, void* key, void *value)
{
  if(!map->root) {
    map->root=rasqal_new_map_node(map, key, value);
    return map->root ? 0 : -1;
  }
  
  return rasqal_map_node_add_kv(map->root, key, value);
}


#define SPACES_LENGTH 80
static const char rasqal_map_node_spaces[SPACES_LENGTH+1]="                                                                                ";


static void
rasqal_map_node_write_indent(FILE *fh, int indent) 
{
  while(indent > 0) {
    int sp=(indent > SPACES_LENGTH) ? SPACES_LENGTH : indent;
    (void)fwrite(rasqal_map_node_spaces, sizeof(char), sp, fh);
    indent -= sp;
  }
}

  

static void
rasqal_map_node_visit(rasqal_map_node* node, 
                      rasqal_map_visit_fn fn, void *user_data)
{
  if(node->prev)
    rasqal_map_node_visit(node->prev, fn, user_data);
  fn(node->key, node->value, user_data);
  if(node->next)
    rasqal_map_node_visit(node->next, fn, user_data);
}


/**
 * rasqal_map_visit:
 * @map: the #rasqal_map to visit
 * @fn: user function to call with key, value and @user_data
 * @user_data: user data to pass to visit function
 *
 * Walk all entries in a (key:value) map.
 * 
 **/
void
rasqal_map_visit(rasqal_map* map, rasqal_map_visit_fn fn, void *user_data)
{
  if(map->root)
    rasqal_map_node_visit(map->root, fn, user_data);
}


struct print_info 
{
  rasqal_map* map;
  FILE *fh;
  int indent;
};
  

static void 
rasqal_map_node_print_visit(void *key, void *value, void *user_data)
{
  struct print_info* pi=(struct print_info*)user_data;
  FILE* fh;
  int indent;
  
  fh=pi->fh;
  indent=pi->indent;

  rasqal_map_node_write_indent(fh, indent);
  fputs("{key: ", fh);
  if(!key)
    fputs("NULL", fh);
  else if(pi->map->print_key)
    pi->map->print_key(key, fh);
  else
    fprintf(fh, "%p", key);

  fputs(", value: ", fh);

  if(!value)
    fputs("NULL", fh);
  else if(pi->map->print_value)
    pi->map->print_value(value, fh);
  else
    fprintf(fh, "%p", value);

  fputs("}\n", fh);
}


/**
 * rasqal_map_print:
 * @map: #rasqal_map to print
 * @fh: FILE handle to write to.
 *
 * Print a (key:value) map in a debug format.
 * 
 **/
void
rasqal_map_print(rasqal_map* map, FILE* fh)
{
  fprintf(fh, "map duplicates=%s {\n", map->allow_duplicates ? "yes" : "no");
  if(map->root) {
    struct print_info pi;
    pi.map=map;
    pi.fh=fh;
    pi.indent=2;
    rasqal_map_visit(map, rasqal_map_node_print_visit, &pi);
  }
  
  fputs("}\n", fh);
}
