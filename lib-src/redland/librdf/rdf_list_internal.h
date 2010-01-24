/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_list_internal.h - Internal RDF List Interface definitions
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



#ifndef LIBRDF_LIST_INTERNAL_H
#define LIBRDF_LIST_INTERNAL_H

#ifndef LIBRDF_OBJC_FRAMEWORK
#include <rdf_iterator.h>
#else
#include <Redland/rdf_iterator.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* private structure */
struct librdf_list_node_s
{
  struct librdf_list_node_s* next;
  struct librdf_list_node_s* prev;
  void *data;
};
typedef struct librdf_list_node_s librdf_list_node;


struct librdf_list_iterator_context_s {
  librdf_iterator* iterator;
  librdf_list* list;
  librdf_list_node *current;
  librdf_list_node *next;
  struct librdf_list_iterator_context_s* next_ic;
  struct librdf_list_iterator_context_s* prev_ic;
};

typedef struct librdf_list_iterator_context_s librdf_list_iterator_context;

struct librdf_list_s
{
  librdf_world *world;
  librdf_list_node* first;
  librdf_list_node* last;
  int length;
  int (*equals) (void* data1, void *data2);
  int iterator_count;
  librdf_list_iterator_context* first_iterator;
  librdf_list_iterator_context* last_iterator;
};

#ifdef __cplusplus
}
#endif

#endif
