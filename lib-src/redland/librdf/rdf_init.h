/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_init.h - Overall library initialisation / termination prototypes
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


#ifndef LIBRDF_INIT_H
#define LIBRDF_INIT_H

#ifdef LIBRDF_INTERNAL
#include <rdf_init_internal.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

REDLAND_API
librdf_world* librdf_new_world(void);
REDLAND_API
void librdf_free_world(librdf_world *world);
REDLAND_API
void librdf_world_open(librdf_world *world);

REDLAND_API
void librdf_world_init_mutex(librdf_world *world);
  
REDLAND_API
void librdf_world_set_error(librdf_world* world, void *user_data, librdf_log_level_func error_handler);
REDLAND_API
void librdf_world_set_warning(librdf_world* world, void *user_data, librdf_log_level_func warning_handler);
REDLAND_API
void librdf_world_set_logger(librdf_world* world, void *user_data, librdf_log_func log_handler);

REDLAND_API
void librdf_world_set_digest(librdf_world* world, const char *name);


/**
 * LIBRDF_WORLD_FEATURE_GENID_BASE:
 *
 * World feature to set the generated ID base.
 *
 * Must be set before the world is opened with librdf_world_open().
 */
#define LIBRDF_WORLD_FEATURE_GENID_BASE "http://feature.librdf.org/genid-base"

/**
 * LIBRDF_WORLD_FEATURE_GENID_COUNTER:
 *
 * World feature to set the generated ID counter.
 *
 * Must be set before the world is opened with librdf_world_open().
 */
#define LIBRDF_WORLD_FEATURE_GENID_COUNTER "http://feature.librdf.org/genid-counter"

REDLAND_API
librdf_node* librdf_world_get_feature(librdf_world* world, librdf_uri *feature);
REDLAND_API
int librdf_world_set_feature(librdf_world* world, librdf_uri *feature, librdf_node* value);

/* OLD INTERFACES */
REDLAND_API REDLAND_DEPRECATED
void librdf_init_world(char *digest_factory_name, void* not_used2);
REDLAND_API REDLAND_DEPRECATED
void librdf_destroy_world(void);

#ifdef LIBRDF_INTERNAL
const char* librdf_basename(const char *name);
#endif

#ifdef __cplusplus
}
#endif

#endif
