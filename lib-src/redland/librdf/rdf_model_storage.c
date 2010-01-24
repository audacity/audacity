/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_model_storage.c - RDF Model Storage implementation
 *
 * Copyright (C) 2003-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2003-2004, University of Bristol, UK http://www.bristol.ac.uk/
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


#ifdef HAVE_CONFIG_H
#include <rdf_config.h>
#endif

#ifdef WIN32
#include <win32_rdf_config.h>
#endif

#include <stdio.h>
#include <string.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h> /* for exit()  */
#endif

#include <redland.h>


typedef struct 
{
  librdf_storage *storage;
} librdf_model_storage_context;


static void
librdf_model_storage_init(void) {

}


static void
librdf_model_storage_terminate(void) {
  
}


/**
 * librdf_model_storage_create:
 * @model: #librdf_model to initialise
 * @storage: #librdf_storage storage to use
 * @options: #librdf_hash of options to use
 *
 * Constructor - Create a new #librdf_model with storage.
 * 
 * Options are presently not used.
 *
 * Return value: non 0 on failure
 **/
static int
librdf_model_storage_create(librdf_model *model, librdf_storage *storage, 
                            librdf_hash* options)
{
  librdf_model_storage_context *context=(librdf_model_storage_context *)model->context;
  
  if(!storage)
    return 1;
  
  if(librdf_storage_open(storage, model))
    return 1;
  
  context->storage=storage;

  librdf_storage_add_reference(storage);
  
  return 0;
}


/**
 * librdf_model_storage_clone:
 * @old_model: the existing #librdf_model
 *
 * Copy constructor - create a new librdf_model from an existing one.
 * 
 * Creates a new model as a copy of the existing model in the same
 * storage context.
 * 
 * Return value: a new #librdf_model or NULL on failure
 **/
static librdf_model*
librdf_model_storage_clone(librdf_model* old_model)
{
  librdf_model_storage_context *old_context=(librdf_model_storage_context *)old_model->context;
  librdf_storage *new_storage;
  librdf_model *new_model;
  
  new_storage=librdf_new_storage_from_storage(old_context->storage);
  if(!new_storage)
    return NULL;

  new_model=librdf_new_model_with_options(old_model->world, new_storage, NULL);
  if(!new_model)
    librdf_free_storage(new_storage);

  return new_model;
}


/**
 * librdf_model_storage_destroy:
 * @model: #librdf_model model to destroy
 *
 * Destructor - Destroy a #librdf_model object.
 * 
 **/
static void
librdf_model_storage_destroy(librdf_model *model)
{
  librdf_model_storage_context *context=(librdf_model_storage_context *)model->context;

  if(context->storage) {
    librdf_storage_close(context->storage);
    librdf_storage_remove_reference(context->storage);
  }
  
}



/**
 * librdf_model_storage_size:
 * @model: #librdf_model object
 *
 * Get the number of statements in the model.
 * 
 * Return value: the number of statements
 **/
static int
librdf_model_storage_size(librdf_model* model)
{
  librdf_model_storage_context *context=(librdf_model_storage_context *)model->context;
  return librdf_storage_size(context->storage);
}


/**
 * librdf_model_storage_add_statement:
 * @model: model object
 * @statement: statement object
 *
 * Add a statement to the model.
 * 
 * The passed-in statement is copied when added to the model, not
 * shared with the model.
 *
 * Return value: non 0 on failure
 **/
static int
librdf_model_storage_add_statement(librdf_model* model, 
                                   librdf_statement* statement)
{
  librdf_model_storage_context *context=(librdf_model_storage_context *)model->context;
  return librdf_storage_add_statement(context->storage, statement);
}


/**
 * librdf_model_storage_add_statements:
 * @model: model object
 * @statement_stream: stream of statements to use
 *
 * Add a stream of statements to the model.
 * 
 * Return value: non 0 on failure
 **/
static int
librdf_model_storage_add_statements(librdf_model* model, librdf_stream* statement_stream)
{
  librdf_model_storage_context *context=(librdf_model_storage_context *)model->context;
  return librdf_storage_add_statements(context->storage, statement_stream);
}


/**
 * librdf_model_storage_remove_statement:
 * @model: the model object
 * @statement: the statement
 *
 * Remove a known statement from the model.
 *
 * Return value: non 0 on failure
 **/
static int
librdf_model_storage_remove_statement(librdf_model* model, 
                                      librdf_statement* statement)
{
  librdf_model_storage_context *context=(librdf_model_storage_context *)model->context;
  return librdf_storage_remove_statement(context->storage, statement);
}


/**
 * librdf_model_storage_contains_statement:
 * @model: the model object
 * @statement: the statement
 *
 * Check for a statement in the model.
 * 
 * Return value: non 0 if the model contains the statement
 **/
static int
librdf_model_storage_contains_statement(librdf_model* model, librdf_statement* statement)
{
  librdf_model_storage_context *context=(librdf_model_storage_context *)model->context;
  return librdf_storage_contains_statement(context->storage, statement);
}


/**
 * librdf_model_storage_serialise:
 * @model: the model object
 *
 * Serialise the entire model as a stream.
 * 
 * Return value: a #librdf_stream or NULL on failure
 **/
static librdf_stream*
librdf_model_storage_serialise(librdf_model* model)
{
  librdf_model_storage_context *context=(librdf_model_storage_context *)model->context;
  return librdf_storage_serialise(context->storage);
}


/**
 * librdf_model_storage_find_statements:
 * @model: the model object
 * @statement: the partial statement to match
 *
 * Find matching statements in the model.
 * 
 * The partial statement is a statement where the subject, predicate
 * and/or object can take the value NULL which indicates a match with
 * any value in the model
 * 
 * Return value: a #librdf_stream of statements (can be empty) or NULL
 * on failure.
 **/
static librdf_stream*
librdf_model_storage_find_statements(librdf_model* model, 
                                     librdf_statement* statement)
{
  librdf_model_storage_context *context=(librdf_model_storage_context *)model->context;
  return librdf_storage_find_statements(context->storage, statement);
}


/**
 * librdf_model_storage_get_sources:
 * @model: #librdf_model object
 * @arc: #librdf_node arc
 * @target: #librdf_node target
 *
 * Return the sources (subjects) of arc in an RDF graph given arc (predicate) and target (object).
 * 
 * Searches the model for arcs matching the given arc and target
 * and returns a list of the source #librdf_node objects as an iterator
 * 
 * Return value:  #librdf_iterator of #librdf_node objects (may be empty) or NULL on failure
 **/
static librdf_iterator*
librdf_model_storage_get_sources(librdf_model *model,
                                 librdf_node *arc, librdf_node *target) 
{
  librdf_model_storage_context *context=(librdf_model_storage_context *)model->context;
  return librdf_storage_get_sources(context->storage, arc, target);
}


/**
 * librdf_model_storage_get_arcs:
 * @model: #librdf_model object
 * @source: #librdf_node source
 * @target: #librdf_node target
 *
 * Return the arcs (predicates) of an arc in an RDF graph given source (subject) and target (object).
 * 
 * Searches the model for arcs matching the given source and target
 * and returns a list of the arc #librdf_node objects as an iterator
 * 
 * Return value:  #librdf_iterator of #librdf_node objects (may be empty) or NULL on failure
 **/
static librdf_iterator*
librdf_model_storage_get_arcs(librdf_model *model,
                              librdf_node *source, librdf_node *target) 
{
  librdf_model_storage_context *context=(librdf_model_storage_context *)model->context;
  return librdf_storage_get_arcs(context->storage, source, target);
}


/**
 * librdf_model_storage_get_targets:
 * @model: #librdf_model object
 * @source: #librdf_node source
 * @arc: #librdf_node arc
 *
 * Return the targets (objects) of an arc in an RDF graph given source (subject) and arc (predicate).
 * 
 * Searches the model for targets matching the given source and arc
 * and returns a list of the source #librdf_node objects as an iterator
 * 
 * Return value:  #librdf_iterator of #librdf_node objects (may be empty) or NULL on failure
 **/
static librdf_iterator*
librdf_model_storage_get_targets(librdf_model *model,
                                 librdf_node *source, librdf_node *arc) 
{
  librdf_model_storage_context *context=(librdf_model_storage_context *)model->context;
  return librdf_storage_get_targets(context->storage, source, arc);
}



/**
 * librdf_model_storage_get_arcs_in:
 * @model: #librdf_model object
 * @node: #librdf_node resource node
 *
 * Return the properties pointing to the given resource.
 * 
 * Return value:  #librdf_iterator of #librdf_node objects (may be empty) or NULL on failure
 **/
static librdf_iterator*
librdf_model_storage_get_arcs_in(librdf_model *model, librdf_node *node) 
{
  librdf_model_storage_context *context=(librdf_model_storage_context *)model->context;
  return librdf_storage_get_arcs_in(context->storage, node);
}


/**
 * librdf_model_storage_get_arcs_out:
 * @model: #librdf_model object
 * @node: #librdf_node resource node
 *
 * Return the properties pointing from the given resource.
 * 
 * Return value:  #librdf_iterator of #librdf_node objects (may be empty) or NULL on failure
 **/
static librdf_iterator*
librdf_model_storage_get_arcs_out(librdf_model *model, librdf_node *node) 
{
  librdf_model_storage_context *context=(librdf_model_storage_context *)model->context;
  return librdf_storage_get_arcs_out(context->storage, node);
}


/**
 * librdf_model_storage_has_arc_in:
 * @model: #librdf_model object
 * @node: #librdf_node resource node
 * @property: #librdf_node property node
 *
 * Check if a node has a given property pointing to it.
 * 
 * Return value: non 0 if arc property does point to the resource node
 **/
static int
librdf_model_storage_has_arc_in(librdf_model *model, librdf_node *node, 
                                librdf_node *property) 
{
  librdf_model_storage_context *context=(librdf_model_storage_context *)model->context;
  return librdf_storage_has_arc_in(context->storage, node, property);
}


/**
 * librdf_model_storage_has_arc_out:
 * @model: #librdf_model object
 * @node: #librdf_node resource node
 * @property: #librdf_node property node
 *
 * Check if a node has a given property pointing from it.
 * 
 * Return value: non 0 if arc property does point from the resource node
 **/
static int
librdf_model_storage_has_arc_out(librdf_model *model, librdf_node *node,
                                 librdf_node *property) 
{
  librdf_model_storage_context *context=(librdf_model_storage_context *)model->context;
  return librdf_storage_has_arc_out(context->storage, node, property);
}



/**
 * librdf_model_storage_context_add_statement:
 * @model: #librdf_model object
 * @context: #librdf_node context
 * @statement: #librdf_statement statement object
 *
 * Add a statement to a model with a context.
 * 
 * Return value: Non 0 on failure
 **/
static int
librdf_model_storage_context_add_statement(librdf_model* model, 
                                           librdf_node* context,
                                           librdf_statement* statement) 
{
  librdf_model_storage_context *mcontext=(librdf_model_storage_context *)model->context;
  return librdf_storage_context_add_statement(mcontext->storage,
                                              context, statement);
}


/**
 * librdf_model_storage_context_add_statements:
 * @model: #librdf_model object
 * @context: #librdf_node context
 * @stream: #librdf_stream stream object
 *
 * Add statements to a model with a context.
 * 
 * Return value: Non 0 on failure
 **/
static int
librdf_model_storage_context_add_statements(librdf_model* model, 
                                            librdf_node* context,
                                            librdf_stream* stream) 
{
  librdf_model_storage_context *mcontext=(librdf_model_storage_context *)model->context;
  return librdf_storage_context_add_statements(mcontext->storage, 
                                               context, stream);
}


/**
 * librdf_model_storage_context_remove_statement:
 * @model: #librdf_model object
 * @context: #librdf_uri context
 * @statement: #librdf_statement statement
 *
 * Remove a statement from a model in a context.
 * 
 * Return value: Non 0 on failure
 **/
static int
librdf_model_storage_context_remove_statement(librdf_model* model,
                                              librdf_node* context,
                                              librdf_statement* statement) 
{
  librdf_model_storage_context *mcontext=(librdf_model_storage_context *)model->context;
  return librdf_storage_context_remove_statement(mcontext->storage,
                                                 context, statement);
}


/**
 * librdf_model_storage_context_remove_statements:
 * @model: #librdf_model object
 * @context: #librdf_uri context
 *
 * Remove statements from a model with the given context.
 * 
 * Return value: Non 0 on failure
 **/
static int
librdf_model_storage_context_remove_statements(librdf_model* model,
                                               librdf_node* context) 
{
  librdf_model_storage_context *mcontext=(librdf_model_storage_context *)model->context;
  return librdf_storage_context_remove_statements(mcontext->storage, context);
}


/**
 * librdf_model_storage_context_serialize:
 * @model: #librdf_model object
 * @context: #librdf_uri context
 *
 * List all statements in a model context.
 * 
 * Return value: #librdf_stream of statements or NULL on failure
 **/
static librdf_stream*
librdf_model_storage_context_serialize(librdf_model* model, 
                                       librdf_node* context) 
{
  librdf_model_storage_context *mcontext=(librdf_model_storage_context *)model->context;
  return librdf_storage_context_as_stream(mcontext->storage, context);
}


/**
 * librdf_model_storage_find_statements_in_context:
 * @model: #librdf_model object
 * @statement: #librdf_statement partial statement to find
 * @context_node: context #librdf_node (or NULL)
 *
 * Search the model for matching statements in a given context.
 * 
 * Searches the model for a (partial) statement as described in
 * librdf_statement_match() in the given context and returns a
 * #librdf_stream of matching #librdf_statement objects.  If
 * context is NULL, this is equivalent to librdf_model_find_statements.
 * 
 * Return value: #librdf_stream of matching statements (may be empty) or NULL on failure
 **/
static librdf_stream*
librdf_model_storage_find_statements_in_context(librdf_model* model,
                                                librdf_statement* statement,
                                                librdf_node* context_node) 
{
  librdf_model_storage_context *mcontext=(librdf_model_storage_context *)model->context;
  librdf_stream *stream;

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(statement, librdf_statement, NULL);

  if(mcontext->storage->factory->find_statements_in_context)
    return mcontext->storage->factory->find_statements_in_context(mcontext->storage,
                                                                  statement,
                                                                  context_node);

  statement=librdf_new_statement_from_statement(statement);
  if(!statement)
    return NULL;

  stream=librdf_model_context_as_stream(model, context_node);
  if(!stream) {
    librdf_free_statement(statement);
    return librdf_new_empty_stream(model->world);
  }

  librdf_stream_add_map(stream,
                        &librdf_stream_statement_find_map,
                        (librdf_stream_map_free_context_handler)&librdf_free_statement, (void*)statement);

  return stream;
}


/**
 * librdf_model_storage_query_execute:
 * @model: #librdf_model object
 * @query: #librdf_query object
 *
 * Run a query against the model returning librdf_query_results.
 * 
 * Run the given query against the model and return a #librdf_query_results
 * 
 * Return value: #librdf_query_results or NULL on failure
 **/
static librdf_query_results*
librdf_model_storage_query_execute(librdf_model* model,
                                   librdf_query *query) 
{
  librdf_model_storage_context *mcontext=(librdf_model_storage_context *)model->context;
  if(librdf_storage_supports_query(mcontext->storage, query))
    return librdf_storage_query_execute(mcontext->storage, query);
  else
    return librdf_query_execute(query,model);
}



/**
 * librdf_model_storage_sync:
 * @model: #librdf_model object
 *
 * Synchronise the model to the storage.
 *
 * Return-value: Non-0 on failure
 **/
static int
librdf_model_storage_sync(librdf_model* model) 
{
  librdf_model_storage_context *context=(librdf_model_storage_context *)model->context;
  return librdf_storage_sync(context->storage);
}


/**
 * librdf_model_storage_get_storage:
 * @model: #librdf_model object
 *
 * Get the storage for the model.
 * 
 **/
static librdf_storage*
librdf_model_storage_get_storage(librdf_model* model) 
{
  librdf_model_storage_context *context=(librdf_model_storage_context *)model->context;
  return context->storage;
}


/**
 * librdf_model_storage_get_contexts:
 * @storage: #librdf_storage object
 *
 * Return the list of contexts in the store.
 * 
 * Returns an iterator of #librdf_node context nodes for each
 * context in the store.
 *
 * Return value: #librdf_iterator of context nodes or NULL on failure or if contexts are not supported
 **/
static librdf_iterator*
librdf_model_storage_get_contexts(librdf_model* model) 
{
  librdf_model_storage_context *context=(librdf_model_storage_context *)model->context;
  return librdf_storage_get_contexts(context->storage);
}



/**
 * librdf_model_storage_get_feature:
 * @storage: #librdf_storage object
 * @feature: #librdf_uri feature property
 *
 * Get the value of a model storage feature.
 * 
 * Return value: new #librdf_node feature value or NULL if no such feature
 * exists or the value is empty.
 **/
static librdf_node*
librdf_model_storage_get_feature(librdf_model* model, librdf_uri* feature)
{
  librdf_model_storage_context *context=(librdf_model_storage_context *)model->context;
  return librdf_storage_get_feature(context->storage, feature);
}


/**
 * librdf_model_storage_set_feature:
 * @storage: #librdf_storage object
 * @feature: #librdf_uri feature property
 * @value: #librdf_node feature property value
 *
 * Get the value of a model storage feature.
 * 
 * Return value: #librdf_node feature value or NULL if no such feature
 * exists or the value is empty.
 **/
static int
librdf_model_storage_set_feature(librdf_model* model, librdf_uri* feature,
                                 librdf_node* value)
{
  librdf_model_storage_context *context=(librdf_model_storage_context *)model->context;
  return librdf_storage_set_feature(context->storage, feature, value);
}


static librdf_stream*
librdf_model_storage_find_statements_with_options(librdf_model* model,
                                                  librdf_statement* statement,
                                                  librdf_node* context_node,
                                                  librdf_hash* options) 
{
  librdf_model_storage_context *context=(librdf_model_storage_context *)model->context;
  return librdf_storage_find_statements_with_options(context->storage, statement, context_node, options);
}


/**
 * librdf_model_storage_transaction_start:
 * @storage: the storage object
 * 
 * Start a transaction
 * 
 * Return value: non-0 on failure
 **/
static int
librdf_model_storage_transaction_start(librdf_model* model) 
{
  librdf_model_storage_context *context=(librdf_model_storage_context *)model->context;
  return librdf_storage_transaction_start(context->storage);
}


/**
 * librdf_model_storage_transaction_start_with_handle:
 * @storage: the storage object
 * @handle: the transaction object
 * 
 * Start a transaction using an existing external transaction object.
 * 
 * Return value: non-0 on failure
 **/
static int
librdf_model_storage_transaction_start_with_handle(librdf_model* model,
                                                   void* handle)
{
  librdf_model_storage_context *context=(librdf_model_storage_context *)model->context;
  return librdf_storage_transaction_start_with_handle(context->storage, handle);
}


/**
 * librdf_model_storage_transaction_commit:
 * @storage: the storage object
 * 
 * Commit a transaction.
 * 
 * Return value: non-0 on failure 
 **/
static int
librdf_model_storage_transaction_commit(librdf_model* model) 
{
  librdf_model_storage_context *context=(librdf_model_storage_context *)model->context;
  return librdf_storage_transaction_commit(context->storage);
}


/**
 * librdf_model_storage_transaction_rollback:
 * @storage: the storage object
 * 
 * Rollback a transaction.
 * 
 * Return value: non-0 on failure 
 **/
static int
librdf_model_storage_transaction_rollback(librdf_model* model) 
{
  librdf_model_storage_context *context=(librdf_model_storage_context *)model->context;
  return librdf_storage_transaction_rollback(context->storage);
}


/**
 * librdf_model_storage_transaction_get_handle:
 * @storage: the storage object
 * 
 * Get the current transaction handle.
 * 
 * Return value: non-0 on failure 
 **/
static void*
librdf_model_storage_transaction_get_handle(librdf_model* model) 
{
  librdf_model_storage_context *context=(librdf_model_storage_context *)model->context;
  return librdf_storage_transaction_get_handle(context->storage);
}



/* local function to register model_storage functions */

static void
librdf_model_storage_register_factory(librdf_model_factory *factory) 
{
  factory->context_length     = sizeof(librdf_model_storage_context);
  
  factory->init               = librdf_model_storage_init;
  factory->terminate          = librdf_model_storage_terminate;
  factory->create             = librdf_model_storage_create;
  factory->clone              = librdf_model_storage_clone;
  factory->destroy            = librdf_model_storage_destroy;
  factory->size               = librdf_model_storage_size;
  factory->add_statement      = librdf_model_storage_add_statement;
  factory->add_statements     = librdf_model_storage_add_statements;
  factory->remove_statement   = librdf_model_storage_remove_statement;
  factory->contains_statement = librdf_model_storage_contains_statement;
  factory->serialise          = librdf_model_storage_serialise;

  factory->find_statements    = librdf_model_storage_find_statements;
  factory->get_sources        = librdf_model_storage_get_sources;
  factory->get_arcs           = librdf_model_storage_get_arcs;
  factory->get_targets        = librdf_model_storage_get_targets;

  factory->get_arcs_in        = librdf_model_storage_get_arcs_in;
  factory->get_arcs_out       = librdf_model_storage_get_arcs_out;
  factory->has_arc_in         = librdf_model_storage_has_arc_in;
  factory->has_arc_out        = librdf_model_storage_has_arc_out;

  factory->context_add_statement    = librdf_model_storage_context_add_statement;
  factory->context_add_statements    = librdf_model_storage_context_add_statements;
  factory->context_remove_statement = librdf_model_storage_context_remove_statement;
  factory->context_remove_statements    = librdf_model_storage_context_remove_statements;
  factory->context_serialize        = librdf_model_storage_context_serialize;
  factory->find_statements_in_context = librdf_model_storage_find_statements_in_context;


  factory->query_execute      = librdf_model_storage_query_execute;
  factory->sync               = librdf_model_storage_sync;
  factory->get_storage        = librdf_model_storage_get_storage;
  factory->get_contexts       = librdf_model_storage_get_contexts;
  factory->get_feature        = librdf_model_storage_get_feature;
  factory->set_feature        = librdf_model_storage_set_feature;
  factory->find_statements_with_options = librdf_model_storage_find_statements_with_options;

  factory->transaction_start             = librdf_model_storage_transaction_start;
  factory->transaction_start_with_handle = librdf_model_storage_transaction_start_with_handle;
  factory->transaction_commit            = librdf_model_storage_transaction_commit;
  factory->transaction_rollback          = librdf_model_storage_transaction_rollback;
  factory->transaction_get_handle        = librdf_model_storage_transaction_get_handle;

}


/**
 * librdf_init_model_storage:
 * @world: world object
 * 
 * INTERNAL - Initialise the model_storage module
 **/
void
librdf_init_model_storage(librdf_world *world)
{
  librdf_model_register_factory(world, 
                                "storage", "Model backed by a Redland storage",
                                &librdf_model_storage_register_factory);
}
