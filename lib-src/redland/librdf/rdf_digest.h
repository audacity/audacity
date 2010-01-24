/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_digest.h - RDF Digest Factory / Digest interfaces and definition
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



#ifndef LIBRDF_DIGEST_H
#define LIBRDF_DIGEST_H

#ifdef LIBRDF_INTERNAL
#include <rdf_digest_internal.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* public constructor */
REDLAND_API
librdf_digest* librdf_new_digest(librdf_world *world, const char *name);

/* destructor */
REDLAND_API
void librdf_free_digest(librdf_digest *digest);


/* methods */
REDLAND_API
void librdf_digest_init(librdf_digest* digest);
REDLAND_API
void librdf_digest_update(librdf_digest* digest, const unsigned char *buf, size_t length);
REDLAND_API
void librdf_digest_update_string(librdf_digest* digest, const unsigned char *string);
REDLAND_API
void librdf_digest_final(librdf_digest* digest);
REDLAND_API
void* librdf_digest_get_digest(librdf_digest* digest);
REDLAND_API
size_t librdf_digest_get_digest_length(librdf_digest* digest);

REDLAND_API
char* librdf_digest_to_string(librdf_digest* digest);
REDLAND_API
void librdf_digest_print(librdf_digest* digest, FILE* fh);

#ifdef __cplusplus
}
#endif

#endif
