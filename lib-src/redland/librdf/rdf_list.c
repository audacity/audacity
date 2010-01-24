/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_list.c - RDF List Implementation
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

#include <sys/types.h>

#include <redland.h>

#include <rdf_list_internal.h>


/* prototypes for local functions */
static librdf_list_node* librdf_list_find_node(librdf_list* list, void *data);

static int librdf_list_iterator_is_end(void* iterator);
static int librdf_list_iterator_next_method(void* iterator);
static void* librdf_list_iterator_get_method(void* iterator, int flags);
static void librdf_list_iterator_finished(void* iterator);

static void librdf_list_iterators_replace_node(librdf_list* list, librdf_list_node* old_node, librdf_list_node* new_node);


/* helper functions */
static librdf_list_node*
librdf_list_find_node(librdf_list* list, void *data)
{
  librdf_list_node* node;
  
  for(node=list->first; node; node=node->next) {
    if(list->equals) {
      if(list->equals(node->data, data))
        break;
    } else {
      if(node->data == data)
        break;
    }
  }
  return node;
}


/**
 * librdf_new_list:
 * @world: redland world object
 *
 * Constructor - create a new #librdf_list.
 * 
 * Return value: a new #librdf_list or NULL on failure
 **/
librdf_list*
librdf_new_list(librdf_world *world)
{
  librdf_list* new_list;
  
  librdf_world_open(world);

  new_list=(librdf_list*)LIBRDF_CALLOC(librdf_list, 1, sizeof(librdf_list));
  if(!new_list)
    return NULL;
  
  new_list->world=world;
  
  return new_list;
}


/**
 * librdf_free_list:
 * @list: #librdf_list object
 *
 * Destructor - destroy a #librdf_list object.
 * 
 **/
void
librdf_free_list(librdf_list* list) 
{
  LIBRDF_ASSERT_RETURN(list->iterator_count,
                       "Iterators were active on freeing list", );

  librdf_list_clear(list);
  LIBRDF_FREE(librdf_list, list);
}


/**
 * librdf_list_clear:
 * @list: #librdf_list object
 *
 * Empty an librdf_list.
 * 
 **/
void
librdf_list_clear(librdf_list* list) 
{
  librdf_list_node *node, *next;
  
  for(node=list->first; node; node=next) {
    next=node->next;
    LIBRDF_FREE(librdf_list_node, node);
  }
}


/**
 * librdf_list_add:
 * @list: #librdf_list object
 * @data: the data value
 *
 * Add a data item to the end of a librdf_list.
 * 
 * Equivalent to the list 'push' notion, thus if librdf_list_pop()
 * is called after this, it will return the value added here.
 *
 * Return value: non 0 on failure
 **/
int
librdf_list_add(librdf_list* list, void *data) 
{
  librdf_list_node* node;
  
  /* need new node */
  node=(librdf_list_node*)LIBRDF_CALLOC(librdf_list_node, 1,
                                        sizeof(librdf_list_node));
  if(!node)
    return 1;
  
  node->data=data;

  /* if there is a list, connect the new node to the last node  */
  if(list->last) {
    node->prev=list->last;
    list->last->next=node;
  }

  /* make this node the last node always */
  list->last=node;
  
  /* if there is no list at all, make this the first to */
  if(!list->first)
    list->first=node;

  /* node->next = NULL implicitly */

  list->length++;
  return 0;
}


/**
 * librdf_list_unshift:
 * @list: #librdf_list object
 * @data: the data value
 *
 * Add a data item to the start of a librdf_list.
 * 
 * if librdf_list_shift() is called after this, it will return the value
 * added here.
 *
 * Return value: non 0 on failure
 **/
int
librdf_list_unshift(librdf_list* list, void *data) 
{
  librdf_list_node* node;
  
  /* need new node */
  node=(librdf_list_node*)LIBRDF_CALLOC(librdf_list_node, 1,
                                        sizeof(librdf_list_node));
  if(!node)
    return 1;
  
  node->data=data;

  /* if there is a list, connect the new node to the first node  */
  if(list->first) {
    node->next=list->first;
    list->first->prev=node;
  }

  /* make this node the first node always */
  list->first=node;
  
  /* if there is no list at all, make this the last too */
  if(!list->last)
    list->last=node;

  /* node->next = NULL implicitly */

  list->length++;
  return 0;
}


/**
 * librdf_list_remove:
 * @list: #librdf_list object
 * @data: the data item
 *
 * Remove a data item from an librdf_list.
 * 
 * The search is done using the 'equals' function which may be set
 * by librdf_list_set_equals() or by straight comparison of pointers
 * if not set.
 * 
 * Return value: the data stored or NULL on failure (not found or list empty)
 **/
void *
librdf_list_remove(librdf_list* list, void *data) 
{
  librdf_list_node *node;
  
  node=librdf_list_find_node(list, data);
  if(!node)
    /* not found */
    return NULL;

  librdf_list_iterators_replace_node(list, node, node->next);
  
  if(node == list->first)
    list->first=node->next;
  if(node->prev)
    node->prev->next=node->next;

  if(node == list->last)
    list->last=node->prev;
  if(node->next)
    node->next->prev=node->prev;

  /* retrieve actual stored data */
  data=node->data;
  
  /* free node */
  LIBRDF_FREE(librdf_list_node, node);
  list->length--;

  return data;
}


/**
 * librdf_list_shift:
 * @list: #librdf_list object
 *
 * Remove and return the data at the start of the list.
 * 
 * Return value: the data object or NULL if the list is empty
 **/
void*
librdf_list_shift(librdf_list* list)
{
  librdf_list_node *node;
  void *data;

  node=list->first;
  if(!node)
    return NULL;
     
  list->first=node->next;

  if(list->first)
    /* if list not empty, fix pointers */
    list->first->prev=NULL;
  else
    /* list is now empty, zap last pointer */
    list->last=NULL;
  
  /* save data */
  data=node->data;

  /* free node */
  LIBRDF_FREE(librdf_list_node, node);

  list->length--;
  return data;
}


/**
 * librdf_list_pop:
 * @list: #librdf_list object
 *
 * Remove and return the data at the end of the list.
 * 
 * Return value: the data object or NULL if the list is empty
 **/
void*
librdf_list_pop(librdf_list* list)
{
  librdf_list_node *node;
  void *data;

  node=list->last;
  if(!node)
    return NULL;
     
  list->last=node->prev;

  if(list->last)
    /* if list not empty, fix pointers */
    list->last->next=NULL;
  else
    /* list is now empty, zap last pointer */
    list->first=NULL;
  
  /* save data */
  data=node->data;

  /* free node */
  LIBRDF_FREE(librdf_list_node, node);

  list->length--;
  return data;
}


/**
 * librdf_list_contains:
 * @list: #librdf_list object
 * @data: the data value
 *
 * Check for presence of data item in list.
 * 
 * The search is done using the 'equals' function which may be set
 * by librdf_list_set_equals() or by straight comparison of pointers
 * if not set.
 * 
 * Return value: non 0 if item was found
 **/
int
librdf_list_contains(librdf_list* list, void *data) 
{
  librdf_list_node *node;
  
  node=librdf_list_find_node(list, data);
  return (node != NULL);
}


/**
 * librdf_list_size:
 * @list: #librdf_list object
 *
 * Return the length of the list.
 * 
 * Return value: length of the list
 **/
int
librdf_list_size(librdf_list* list) 
{
  return list->length;
}


/**
 * librdf_list_set_equals:
 * @list: #librdf_list object
 * @equals: the equals function
 *
 * Set the equals function for the list.
 * 
 * The function given is used when comparing items in the list
 * during searches such as those done in librdf_list_remove() or
 * librdf_list_contains().
 * 
 **/
void
librdf_list_set_equals(librdf_list* list, 
                       int (*equals) (void* data1, void *data2)) 
{
  list->equals=equals;
}


static void
librdf_list_add_iterator_context(librdf_list* list, 
                                 librdf_list_iterator_context* node)
{
  if(list->last_iterator) {
    node->prev_ic=list->last_iterator;
    list->last_iterator->next_ic=node;
  }

  list->last_iterator=node;
  
  if(!list->first_iterator)
    list->first_iterator=node;

  list->iterator_count++;
#if LIBRDF_DEBUG > 2
  LIBRDF_DEBUG4("Added iterator %p to list %p giving %d iterators\n",
                node->iterator, list, list->iterator_count);
#endif
}


static void
librdf_list_remove_iterator_context(librdf_list* list,
                                    librdf_list_iterator_context* node)
{
  if(node == list->first_iterator)
    list->first_iterator=node->next_ic;
  if(node->prev_ic)
    node->prev_ic->next_ic=node->next_ic;

  if(node == list->last_iterator)
    list->last_iterator=node->prev_ic;
  if(node->next_ic)
    node->next_ic->prev_ic=node->prev_ic;

  list->iterator_count--;
#if LIBRDF_DEBUG > 2
  LIBRDF_DEBUG4("Removed iterator %p from list %p leaving %d iterators\n",
                node->iterator, list, list->iterator_count);
#endif
}


static void
librdf_list_iterators_replace_node(librdf_list* list, 
                                   librdf_list_node* old_node,
                                   librdf_list_node* new_node)
{
  librdf_list_iterator_context *node, *next;
  
  if(!list->iterator_count)
    return;
  
  for(node=list->first_iterator; node; node=next) {
    next=node->next_ic;
    if(node->next == old_node) {
#if LIBRDF_DEBUG > 2
      LIBRDF_DEBUG3("Moved iterator %p pointing from next node %p to %p\n", 
                    node->iterator, old_node, new_node);
#endif
      node->next = new_node;
    }
  }
}



/**
 * librdf_list_get_iterator:
 * @list: #librdf_list object
 *
 * Get an iterator for the list.
 * 
 * Return value: a new #librdf_iterator object or NULL on failure
 **/
librdf_iterator*
librdf_list_get_iterator(librdf_list* list)
{
  librdf_list_iterator_context* context;
  librdf_iterator* iterator;

  context=(librdf_list_iterator_context*)LIBRDF_CALLOC(librdf_list_iterator_context, 1, sizeof(librdf_list_iterator_context));
  if(!context)
    return NULL;

  context->list=list;
  context->current=list->first;
  context->next=context->current != NULL ? context->current->next : NULL;

  /* librdf_list_iterator_finished() calls librdf_list_remove_iterator_context(),
   * librdf_list_iterator_finished() is called if librdf_new_iterator() fails */
  librdf_list_add_iterator_context(list, context);

  iterator=librdf_new_iterator(list->world, 
                               (void*)context,
                               librdf_list_iterator_is_end,
                               librdf_list_iterator_next_method,
                               librdf_list_iterator_get_method,
                               librdf_list_iterator_finished);
  if(!iterator)
    librdf_list_iterator_finished(context);
  else
    context->iterator=iterator;

  return iterator;
}


static int
librdf_list_iterator_is_end(void* iterator) 
{
  librdf_list_iterator_context* context=(librdf_list_iterator_context*)iterator;
  librdf_list_node *node=context->current;
  
  return (node == NULL);
}


static int
librdf_list_iterator_next_method(void* iterator) 
{
  librdf_list_iterator_context* context=(librdf_list_iterator_context*)iterator;
  librdf_list_node *node=context->current;
  
  if(!node)
    return 1;
  
  context->current = context->next;
  context->next = context->current != NULL ? context->current->next : NULL;
  
  return (context->current == NULL);
}


static void*
librdf_list_iterator_get_method(void* iterator, int flags) 
{
  librdf_list_iterator_context* context=(librdf_list_iterator_context*)iterator;
  librdf_list_node *node=context->current;
  
  if(!node)
    return NULL;

  if(flags == LIBRDF_ITERATOR_GET_METHOD_GET_OBJECT)
    return node->data;

  librdf_log(context->list->world,
             0, LIBRDF_LOG_ERROR, LIBRDF_FROM_LIST, NULL,
             "Unsupported iterator method flag %d", flags);
  return NULL;
}


static void
librdf_list_iterator_finished(void* iterator)
{
  librdf_list_iterator_context* context=(librdf_list_iterator_context*)iterator;

  if(!context)
    return;
  
  librdf_list_remove_iterator_context(context->list, context);

  LIBRDF_FREE(librdf_list_iterator_context, context);
}



/**
 * librdf_list_foreach:
 * @list: #librdf_list object
 * @fn: pointer to function to apply that takes data pointer and user data parameters
 * @user_data: user data for applied function 
 *
 * Apply a function for each data item in a librdf_list.
 * 
 **/
void
librdf_list_foreach(librdf_list* list, void (*fn)(void *, void *),
                    void *user_data) 
{
  librdf_list_node *node, *next;
  
  for(node=list->first; node; node=next) {
    next=node->next;
    (*fn)(node->data, user_data);
  }
}


