/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_hash.h - RDF Hash Factory and Hash interfaces and definitions
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


#ifndef LIBRDF_HASH_H
#define LIBRDF_HASH_H

#ifdef LIBRDF_INTERNAL
#include <rdf_hash_internal.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* public constructors */
REDLAND_API
librdf_hash* librdf_new_hash_from_string(librdf_world *world, const char *name, const char *string);
REDLAND_API
librdf_hash* librdf_new_hash_from_array_of_strings(librdf_world *world, const char *name, const char **array);

/* public copy constructor */
REDLAND_API
librdf_hash* librdf_new_hash_from_hash (librdf_hash* old_hash);

/* public destructor */
REDLAND_API
void librdf_free_hash(librdf_hash *hash);

/* public methods */

/* retrieve one value for a given hash key */
REDLAND_API
char* librdf_hash_get(librdf_hash* hash, const char *key);

/* lookup a hash key and decode value as a boolean */
REDLAND_API
int librdf_hash_get_as_boolean(librdf_hash* hash, const char *key);

/* lookup a hash key and decode value as a long */
REDLAND_API
long librdf_hash_get_as_long(librdf_hash* hash, const char *key);

/* retrieve one value for key and delete from hash all other values */
REDLAND_API
char* librdf_hash_get_del(librdf_hash* hash, const char *key);

/* insert a key/value pair */
REDLAND_API
int librdf_hash_put_strings(librdf_hash* hash, const char *key, const char *value);

REDLAND_API
void librdf_hash_print(librdf_hash* hash, FILE *fh);
REDLAND_API
void librdf_hash_print_keys(librdf_hash* hash, FILE *fh);
REDLAND_API
void librdf_hash_print_values(librdf_hash* hash, const char *key_string, FILE *fh);

REDLAND_API
unsigned char* librdf_hash_interpret_template(const unsigned char* template_string, librdf_hash* dictionary, const unsigned char* prefix,  const unsigned char* suffix);

#ifdef __cplusplus
}
#endif

#endif
