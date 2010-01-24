/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_iterator_internal.h - Internal RDF Iterator definitions
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



#ifndef LIBRDF_ITERATOR_INTERNAL_H
#define LIBRDF_ITERATOR_INTERNAL_H

#ifdef __cplusplus
extern "C" {
#endif

/* used in map_list below */
typedef struct {
  void *context; /* context to pass on to map */
  librdf_iterator_map_handler fn;
  librdf_iterator_map_free_context_handler free_context;
} librdf_iterator_map;

struct librdf_iterator_s {
  librdf_world *world;
  void *context;
  int is_finished; /* 1 when have no more elements */
  int is_updated; /* 1 when we know there is a current item */
  int is_updating; /* 1 when are in the middle of update process */ 

  /* Used when mapping */
  void *current;            /* stores current element */
  librdf_list *map_list; /* non-empty means there is a list of maps */
  
  int (*is_end_method)(void*);
  int (*next_method)(void*);
  void* (*get_method)(void*, int); /* flags: type of get */
  void (*finished_method)(void*);
};

/* FIXME - should all short lists be enums */
#define LIBRDF_ITERATOR_GET_METHOD_GET_OBJECT  0
#define LIBRDF_ITERATOR_GET_METHOD_GET_CONTEXT 1
#define LIBRDF_ITERATOR_GET_METHOD_GET_KEY     2
#define LIBRDF_ITERATOR_GET_METHOD_GET_VALUE   3

#ifdef __cplusplus
}
#endif

#endif
