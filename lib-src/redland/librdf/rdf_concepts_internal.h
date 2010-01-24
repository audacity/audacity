/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_concepts_internal.h - Internal RDF concept URIs and nodes definitions
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


#ifndef LIBRDF_CONCEPTS_INTERNAL_H
#define LIBRDF_CONCEPTS_INTERNAL_H

#ifdef __cplusplus
extern "C" {
#endif

/* class methods */
void librdf_init_concepts(librdf_world *world);
void librdf_finish_concepts(librdf_world *world);

void librdf_get_concept_by_name(librdf_world *world, int is_ms, const char *name, librdf_uri **uri_p, librdf_node **node_p);


#ifdef __cplusplus
}
#endif

#endif
