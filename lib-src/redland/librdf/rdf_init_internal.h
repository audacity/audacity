/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_init_internal.h - Internal library initialisation / termination
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


#ifndef LIBRDF_INIT_INTERNAL_H
#define LIBRDF_INIT_INTERNAL_H

#ifdef __cplusplus
extern "C" {
#endif

#ifdef WITH_THREADS
#include <pthread.h>
#endif

#ifdef RASQAL_H
/* rasqal.h will have defined this */
#else
/* provide a forward reference */
#ifndef RASQAL_WORLD_DECLARED
#define RASQAL_WORLD_DECLARED 1
typedef struct rasqal_world_s rasqal_world;
#endif
#endif

struct librdf_world_s
{
  void *error_user_data;
  librdf_log_level_func error_handler;

  void *warning_user_data;
  librdf_log_level_func warning_handler;

  void *log_user_data;
  librdf_log_func log_handler;

  /* static (last) log message */
  librdf_log_message log;

  char *digest_factory_name;
  librdf_digest_factory* digest_factory;

  /* URI interning */
  librdf_hash* uris_hash;
  int uris_hash_allocated_here;

  /* Node interning */
  librdf_hash* nodes_hash[3]; /* resource, literal, blank */

  /* Sequence of model factories */
  raptor_sequence* models;
  
  /* Sequence of storage factories */
  raptor_sequence* storages;
  
  /* Sequence of parser factories */
  raptor_sequence* parsers;

  /* Sequence of serializer factories */
  raptor_sequence* serializers;

  /* List of query factories */
  librdf_query_factory* query_factories;

  /* List of digest factories */
  librdf_digest_factory *digests;

  /* list of hash factories */
  librdf_hash_factory* hashes;

  /* list of free librdf_hash_datums is kept */
  librdf_hash_datum* hash_datums_list;

   /* hash load_factor out of 1000 */
  int hash_load_factor;

  /* ID base from startup time */
  long genid_base;

  /* Unique counter from there */
  long genid_counter;

#ifdef WITH_THREADS
  /* mutex so we can lock around this when we need to */
  pthread_mutex_t* mutex;

  /* mutex to lock the nodes class */
  pthread_mutex_t* nodes_mutex;

  /* mutex to lock the statements class */
  pthread_mutex_t* statements_mutex;
#else
  /* !WITH_THREADS - pad structure to same size */
  void* mutex_fake;
  void* nodes_mutex_fake;
  void* statements_mutex_fake;
#endif

  /* non-0 if librdf_world_open() has been called */
  int opened;

  /* Sequence of storage modules
   * Used with --enable-modular / MODULAR_LIBRDF
   */
  raptor_sequence* storage_modules;

  /* If libtdl has been opened with lt_dlinit()
   * Used with --enable-modular / MODULAR_LIBRDF
   */
  int ltdl_opened;

  /* librdf_concepts RDF syntax and Schema namespace uris */
  librdf_uri* concept_ms_namespace_uri;
  librdf_uri* concept_schema_namespace_uri;

  /* librdf_concepts nodes and uris */
  librdf_uri** concept_uris;
  librdf_node** concept_resources;

  /* rasqal world object */
  rasqal_world* rasqal_world_ptr;
};

unsigned char* librdf_world_get_genid(librdf_world* world);


#ifdef __cplusplus
}
#endif

#endif
