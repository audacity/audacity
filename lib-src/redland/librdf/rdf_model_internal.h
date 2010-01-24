/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_model_internal.h - Internal RDF Model definitions
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



#ifndef LIBRDF_MODEL_INTERNAL_H
#define LIBRDF_MODEL_INTERNAL_H

#ifdef __cplusplus
extern "C" {
#endif

struct librdf_model_s {
  librdf_world *world;

  /* usage: usage count of this instance
   *
   * Used by other redland classes such as iterator, stream
   * via librdf_model_add_reference librdf_model_remove_reference
   * The usage count of model after construction is 1.
   */
  int usage;
  
  /* sub_models: list of sub librdf_model* */
  librdf_list*     sub_models;

  /* supports_contexts : does the storage model support redland contexts? */
  int supports_contexts;

  /* context : model implementation user data */
  void *context;

  struct librdf_model_factory_s* factory;
};

/* A Model Factory */
struct librdf_model_factory_s {
  char* name;
  char* label;
  
  /* the rest of this structure is populated by the
     model-specific register function */
  size_t context_length;
  
  /* init the factory */
  void (*init)(void);

  /* terminate the factory */
  void (*terminate)(void);
  
  /* create a new model */
  int (*create)(librdf_model* model, librdf_storage* storage, librdf_hash* options);
  
  /* copy a model */
  /* clone is assumed to do leave the new model in the same state
   * after an init() method on an existing model - i.e ready to
   * use but closed.
   */
  librdf_model* (*clone)(librdf_model* new_model);

  /* destroy model */
  void (*destroy)(librdf_model* model);
  
  /* return the number of statements in the model for model */
  int (*size)(librdf_model* model);
  
  /* add a statement to the model from the given model */
  int (*add_statement)(librdf_model* model, librdf_statement* statement);
  
  /* add a statement to the model from the given model */
  int (*add_statements)(librdf_model* model, librdf_stream* statement_stream);
  
  /* remove a statement from the model  */
  int (*remove_statement)(librdf_model* model, librdf_statement* statement);
  
  /* check if statement in model  */
  int (*contains_statement)(librdf_model* model, librdf_statement* statement);
  /* check for [node, property, ?] */
  int (*has_arc_in)(librdf_model *model, librdf_node *node, librdf_node *property);
  /* check for [?, property, node] */
  int (*has_arc_out)(librdf_model *model, librdf_node *node, librdf_node *property);

  
  /* serialise the model in model  */
  librdf_stream* (*serialise)(librdf_model* model);
  
  /* serialise the results of a query */
  librdf_stream* (*find_statements)(librdf_model* model, librdf_statement* statement);
  librdf_stream* (*find_statements_with_options)(librdf_model* model, librdf_statement* statement, librdf_node* context_node, librdf_hash* options);

  /* return a list of Nodes marching given arc, target */
  librdf_iterator* (*get_sources)(librdf_model* model, librdf_node *arc, librdf_node *target);

  /* return a list of Nodes marching given source, target */
  librdf_iterator* (*get_arcs)(librdf_model* model, librdf_node *source, librdf_node *target);

  /* return a list of Nodes marching given source, target */
  librdf_iterator* (*get_targets)(librdf_model* model, librdf_node *source, librdf_node *target);

  /* return list of properties to/from a node */
  librdf_iterator* (*get_arcs_in)(librdf_model *model, librdf_node *node);
  librdf_iterator* (*get_arcs_out)(librdf_model *model, librdf_node *node);

  /* add a statement to the model from the context */
  int (*context_add_statement)(librdf_model* model, librdf_node* context, librdf_statement *statement);
  
  /* remove a statement from the context  */
  int (*context_remove_statement)(librdf_model* model, librdf_node* context, librdf_statement *statement);

  /* list statements in a context  */
  librdf_stream* (*context_serialize)(librdf_model* model, librdf_node* context);

  /* query the model */
  librdf_query_results* (*query_execute)(librdf_model* model, librdf_query* query);

  /* sync the model to the storage - OPTIONAL */
  int (*sync)(librdf_model* model);

  /* add a statement from the context - OPTIONAL (librdf_model will
   * implement using context_add_statement if missing) 
   */
  int (*context_add_statements)(librdf_model* model, librdf_node* context, librdf_stream *stream);

  /* remove a statement from the context - OPTIONAL (librdf_model will
   * implement using context_remove_statement if missing) 
   */
  int (*context_remove_statements)(librdf_model* model, librdf_node* context);

  /* get the single storage for this model if there is one - OPTIONAL */
  librdf_storage* (*get_storage)(librdf_model* model);

  /* search for statement in a context - OPTIONAL (rdf_model will do
   * it using find_statements if missing)
   */
  librdf_stream* (*find_statements_in_context)(librdf_model* model, librdf_statement* statement, librdf_node* context_node);

  /* return an iterator of context nodes in the store - OPTIONAL
   * (returning NULL)
   */
  librdf_iterator* (*get_contexts)(librdf_model* model);

  /* features - OPTIONAL */
  librdf_node* (*get_feature)(librdf_model* model, librdf_uri* feature);
  int (*set_feature)(librdf_model* model, librdf_uri* feature, librdf_node* value);

  /* transactions - OPTIONAL */
  int (*transaction_start)(librdf_model* model);
  int (*transaction_start_with_handle)(librdf_model* model, void* handle);
  int (*transaction_commit)(librdf_model* model);
  int (*transaction_rollback)(librdf_model* model);
  void* (*transaction_get_handle)(librdf_model* model);

};

/* module init */
void librdf_init_model(librdf_world *world);

  /* module terminate */
void librdf_finish_model(librdf_world *world);

/* class methods */
void librdf_model_register_factory(librdf_world *world, const char *name, const char *label, void (*factory) (librdf_model_factory*));
librdf_model_factory* librdf_get_model_factory(librdf_world* world, const char *name);

void librdf_model_add_reference(librdf_model *model);
void librdf_model_remove_reference(librdf_model *model);


/* model storage factory initialise (the only model factory at present) */
void librdf_init_model_storage(librdf_world *world);


#ifdef __cplusplus
}
#endif

#endif
