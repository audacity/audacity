/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_list.h - RDF List Interface definition
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



#ifndef LIBRDF_LIST_H
#define LIBRDF_LIST_H

#ifndef LIBRDF_OBJC_FRAMEWORK
#include <rdf_iterator.h>
#else
#include <Redland/rdf_iterator.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

librdf_list* librdf_new_list(librdf_world *world);
void librdf_free_list(librdf_list* list);

void librdf_list_clear(librdf_list* list);
/* add to end of list (push) */
int librdf_list_add(librdf_list* list, void *data);
/* add to start of list */
int librdf_list_unshift(librdf_list* list, void *data);
/* remove from start of list */
void* librdf_list_shift(librdf_list* list);
/* remove from end of list (pop) */
void* librdf_list_pop(librdf_list* list);
void *librdf_list_remove(librdf_list* list, void *data);
int librdf_list_contains(librdf_list* list, void *data);
int librdf_list_size(librdf_list* list);

void librdf_list_set_equals(librdf_list* list, int (*equals) (void* data1, void *data2));

librdf_iterator* librdf_list_get_iterator(librdf_list* list);

void librdf_list_foreach(librdf_list* list, void (*fn)(void *, void *), void *user_data);

#ifdef __cplusplus
}
#endif

#endif
