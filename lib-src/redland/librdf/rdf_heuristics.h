/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_heuristics.h - Heuristic routines to guess things about RDF prototypes
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


#ifndef LIBRDF_HEURISTICS_H
#define LIBRDF_HEURISTICS_H

#ifdef __cplusplus
extern "C" {
#endif

char* librdf_heuristic_gen_name(const char *name);
int librdf_heuristic_is_blank_node(const char *node);
const char* librdf_heuristic_get_blank_node(const char *node);
int librdf_heuristic_object_is_literal(const char *object);

#ifdef __cplusplus
}
#endif

#endif
