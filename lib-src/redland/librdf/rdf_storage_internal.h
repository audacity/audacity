/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_storage_internal.h - Internal RDF Storage definitions
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


#ifndef LIBRDF_STORAGE_INTERNAL_H
#define LIBRDF_STORAGE_INTERNAL_H

#ifdef __cplusplus
extern "C" {
#endif

/** A storage object */
struct librdf_storage_s
{
  librdf_world *world;

  /* usage count of this instance
   * Used by other redland classes such as model, iterator, stream
   * via  librdf_storage_add_reference librdf_storage_remove_reference
   * The usage count of storage after construction is 1.
   */
  int usage;
  
  librdf_model *model;
  void *context;
  int index_contexts;
  struct librdf_storage_factory_s* factory;
};


/** A Storage Factory */
struct librdf_storage_factory_s {
  char* name;
  char* label;
  
  /* the rest of this structure is populated by the
     storage-specific register function */
  size_t context_length;
  
  /* create a new storage */
  int (*init)(librdf_storage* storage, const char *name, librdf_hash* options);
  
  /* copy a storage */
  /* clone is assumed to do leave the new storage in the same state
   * after an init() method on an existing storage - i.e ready to
   * use but closed.
   */
  int (*clone)(librdf_storage* new_storage, librdf_storage* old_storage);

  /* destroy a storage */
  void (*terminate)(librdf_storage* storage);
  
  /* make storage be associated with model */
  int (*open)(librdf_storage* storage, librdf_model* model);
  
  /* close storage/model context */
  int (*close)(librdf_storage* storage);
  
  /* return the number of statements in the storage for model */
  int (*size)(librdf_storage* storage);
  
  /* add a statement to the storage from the given model - OPTIONAL */
  int (*add_statement)(librdf_storage* storage, librdf_statement* statement);
  
  /* add a statement to the storage from the given model - OPTIONAL */
  int (*add_statements)(librdf_storage* storage, librdf_stream* statement_stream);
  
  /* remove a statement from the storage - OPTIONAL */
  int (*remove_statement)(librdf_storage* storage, librdf_statement* statement);
  
  /* check if statement in storage  */
  int (*contains_statement)(librdf_storage* storage, librdf_statement* statement);
  /* check for [node, property, ?] */
  int (*has_arc_in)(librdf_storage *storage, librdf_node *node, librdf_node *property);
  /* check for [?, property, node] */
  int (*has_arc_out)(librdf_storage *storage, librdf_node *node, librdf_node *property);

  
  /* serialise the model in storage  */
  librdf_stream* (*serialise)(librdf_storage* storage);
  
  /* serialise the results of a query */
  librdf_stream* (*find_statements)(librdf_storage* storage, librdf_statement* statement);
  /* OPTIONAL */
  librdf_stream* (*find_statements_with_options)(librdf_storage* storage, librdf_statement* statement, librdf_node* context_node, librdf_hash* options);

  /* return a list of Nodes marching given arc, target */
  librdf_iterator* (*find_sources)(librdf_storage* storage, librdf_node *arc, librdf_node *target);

  /* return a list of Nodes marching given source, target */
  librdf_iterator* (*find_arcs)(librdf_storage* storage, librdf_node *source, librdf_node *target);

  /* return a list of Nodes marching given source, target */
  librdf_iterator* (*find_targets)(librdf_storage* storage, librdf_node *source, librdf_node *target);

  /* return list of properties to/from a node */
  librdf_iterator* (*get_arcs_in)(librdf_storage *storage, librdf_node *node);
  librdf_iterator* (*get_arcs_out)(librdf_storage *storage, librdf_node *node);


  /* add a statement to the storage from the context - OPTIONAL */
  /* NOTE: if context is NULL, this MUST be equivalent to add_statement */
  int (*context_add_statement)(librdf_storage* storage, librdf_node* context, librdf_statement *statement);
  
  /* remove a statement from the context - OPTIONAL */
  /* NOTE: if context is NULL, this MUST be equivalent to remove_statement */
  int (*context_remove_statement)(librdf_storage* storage, librdf_node* context, librdf_statement *statement);

  /* list statements in a context - OPTIONAL */
  librdf_stream* (*context_serialise)(librdf_storage* storage, librdf_node* context);

  /* synchronise to underlying storage - OPTIONAL */
  int (*sync)(librdf_storage* storage);

  /* add statements to the context - OPTIONAL (rdf_storage will do it
   * using context_add_statement if missing)
   * NOTE: if context is NULL, this MUST be equivalent to add_statements
  */
  int (*context_add_statements)(librdf_storage* storage, librdf_node* context, librdf_stream *stream);

  /* remove statements from the context - OPTIONAL (rdf_storage will do it
   * using context_remove_statement if missing)
   */
  int (*context_remove_statements)(librdf_storage* storage, librdf_node* context);

  /* search for statement in a context - OPTIONAL (rdf_storage will do
   * it using find_statements if missing)
   */
  librdf_stream* (*find_statements_in_context)(librdf_storage* storage, librdf_statement* statement, librdf_node* context_node);

  /* return an iterator of context nodes in the store - OPTIONAL
   * (returning NULL)
   */
  librdf_iterator* (*get_contexts)(librdf_storage* storage);

  /* features - OPTIONAL */
  librdf_node* (*get_feature)(librdf_storage* storaage, librdf_uri* feature);
  int (*set_feature)(librdf_storage* storage, librdf_uri* feature, librdf_node* value);

  /* transactions - OPTIONAL */
  int (*transaction_start)(librdf_storage* storage);
  int (*transaction_start_with_handle)(librdf_storage* storage, void* handle);
  int (*transaction_commit)(librdf_storage* storage);
  int (*transaction_rollback)(librdf_storage* storage);
  void* (*transaction_get_handle)(librdf_storage* storage);

};

void librdf_init_storage_list(librdf_world *world);

void librdf_init_storage_hashes(librdf_world *world);

void librdf_init_storage_trees(librdf_world *world);

void librdf_init_storage_file(librdf_world *world);

#ifdef STORAGE_MYSQL
void librdf_init_storage_mysql(librdf_world *world);
#endif

#ifdef STORAGE_POSTGRESQL
void librdf_init_storage_postgresql(librdf_world *world);
#endif

#ifdef STORAGE_TSTORE
void librdf_init_storage_tstore(librdf_world *world);
#endif

#ifdef STORAGE_SQLITE
void librdf_init_storage_sqlite(librdf_world *world);
#endif


/* module init */
void librdf_init_storage(librdf_world *world);

/* module terminate */
void librdf_finish_storage(librdf_world *world);

/* class methods */
librdf_storage_factory* librdf_get_storage_factory(librdf_world* world, const char *name);


/* rdf_storage_sql.c */
typedef struct  
{
  char* filename;
  
  const char** predicate_uri_strings;
  int predicates_count;

  /* array of char* with NULL at end - size predicates_count */
  char** values;
} librdf_sql_config;

librdf_sql_config* librdf_new_sql_config(librdf_world* world, const char *storage_name, const char* layout, const char* config_dir, const char** predicate_uri_strings);
librdf_sql_config* librdf_new_sql_config_for_storage(librdf_storage* storage, const char* layout, const char* dir);
void librdf_free_sql_config(librdf_sql_config* config);

typedef enum {
  DBCONFIG_CREATE_TABLE_STATEMENTS,
  DBCONFIG_CREATE_TABLE_LITERALS,
  DBCONFIG_CREATE_TABLE_RESOURCES,
  DBCONFIG_CREATE_TABLE_BNODES,
  DBCONFIG_CREATE_TABLE_MODELS,
  DBCONFIG_CREATE_TABLE_LAST = DBCONFIG_CREATE_TABLE_MODELS
} librdf_dbconfig;

extern const char* librdf_storage_sql_dbconfig_predicates[DBCONFIG_CREATE_TABLE_LAST+2];



#ifdef __cplusplus
}
#endif

#endif
