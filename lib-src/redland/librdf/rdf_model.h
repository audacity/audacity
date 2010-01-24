/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_model.h - RDF Model definition
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



#ifndef LIBRDF_MODEL_H
#define LIBRDF_MODEL_H

#ifndef LIBRDF_OBJC_FRAMEWORK
#include <rdf_uri.h>
#else
#include <Redland/rdf_uri.h>
#endif

#ifdef LIBRDF_INTERNAL
#include <rdf_model_internal.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* class methods */

REDLAND_API
int librdf_model_enumerate(librdf_world* world, const unsigned int counter, const char **name, const char **label);

/* constructors */

/* Create a new Model */
REDLAND_API
librdf_model* librdf_new_model(librdf_world *world, librdf_storage *storage, const char* options_string);
REDLAND_API
librdf_model* librdf_new_model_with_options(librdf_world *world, librdf_storage *storage, librdf_hash* options);

/* Create a new Model from an existing Model - CLONE */
REDLAND_API
librdf_model* librdf_new_model_from_model(librdf_model* model);

/* destructor */
REDLAND_API
void librdf_free_model(librdf_model *model);


/* functions / methods */
REDLAND_API
int librdf_model_size(librdf_model* model);

/* add statements */
REDLAND_API
int librdf_model_add(librdf_model* model, librdf_node* subject, librdf_node* predicate, librdf_node* object);
REDLAND_API
int librdf_model_add_string_literal_statement(librdf_model* model, librdf_node* subject, librdf_node* predicate, const unsigned char* literal, const char *xml_language, int is_wf_xml);
REDLAND_API
int librdf_model_add_typed_literal_statement(librdf_model* model, librdf_node* subject, librdf_node* predicate, const unsigned char* literal, const char *xml_language, librdf_uri *datatype_uri);
REDLAND_API
int librdf_model_add_statement(librdf_model* model, librdf_statement* statement);
REDLAND_API
int librdf_model_add_statements(librdf_model* model, librdf_stream* statement_stream);

/* remove statements */
REDLAND_API
int librdf_model_remove_statement(librdf_model* model, librdf_statement* statement);

/* containment */
/* check for exact statement match */
REDLAND_API
int librdf_model_contains_statement(librdf_model* model, librdf_statement* statement);
/* check for [node, property, ?] */
REDLAND_API
int librdf_model_has_arc_in(librdf_model *model, librdf_node *node, librdf_node *property);
/* check for [?, property, node] */
REDLAND_API
int librdf_model_has_arc_out(librdf_model *model, librdf_node *node, librdf_node *property);


/* list the entire model as a stream of statements */
REDLAND_API
librdf_stream* librdf_model_as_stream(librdf_model* model);
/* DEPRECATED serialise the entire model */
REDLAND_API REDLAND_DEPRECATED
librdf_stream* librdf_model_serialise(librdf_model* model);

/* queries */

REDLAND_API
librdf_stream* librdf_model_find_statements(librdf_model* model, librdf_statement* statement);

/**
 * LIBRDF_MODEL_FIND_OPTION_MATCH_SUBSTRING_LITERAL:
 *
 * Model find statement option.
 *
 * If set, the find statement uses substring matching.
 */
#define LIBRDF_MODEL_FIND_OPTION_MATCH_SUBSTRING_LITERAL "http://feature.librdf.org/model-find-match-substring-literal"

REDLAND_API
librdf_stream* librdf_model_find_statements_with_options(librdf_model* model, librdf_statement* statement, librdf_node* context_node, librdf_hash* options);
REDLAND_API
librdf_iterator* librdf_model_get_sources(librdf_model *model, librdf_node *arc, librdf_node *target);
REDLAND_API
librdf_iterator* librdf_model_get_arcs(librdf_model *model, librdf_node *source, librdf_node *target);
REDLAND_API
librdf_iterator* librdf_model_get_targets(librdf_model *model, librdf_node *source, librdf_node *arc);
REDLAND_API
librdf_node* librdf_model_get_source(librdf_model *model, librdf_node *arc, librdf_node *target);
REDLAND_API
librdf_node* librdf_model_get_arc(librdf_model *model, librdf_node *source, librdf_node *target);
REDLAND_API
librdf_node* librdf_model_get_target(librdf_model *model, librdf_node *source, librdf_node *arc);

/* return list of properties to/from a node */
REDLAND_API
librdf_iterator* librdf_model_get_arcs_in(librdf_model *model, librdf_node *node);
REDLAND_API
librdf_iterator* librdf_model_get_arcs_out(librdf_model *model, librdf_node *node);



/* submodels */
REDLAND_API
int librdf_model_add_submodel(librdf_model* model, librdf_model* sub_model);
REDLAND_API
int librdf_model_remove_submodel(librdf_model* model, librdf_model* sub_model);


REDLAND_API
void librdf_model_print(librdf_model *model, FILE *fh);

/* statement contexts */
REDLAND_API
int librdf_model_context_add_statement(librdf_model* model, librdf_node* context, librdf_statement* statement);
REDLAND_API
int librdf_model_context_add_statements(librdf_model* model, librdf_node* context, librdf_stream* stream);
REDLAND_API
int librdf_model_context_remove_statement(librdf_model* model, librdf_node* context, librdf_statement* statement);
REDLAND_API
int librdf_model_context_remove_statements(librdf_model* model, librdf_node* context);
REDLAND_API
librdf_stream* librdf_model_context_as_stream(librdf_model* model, librdf_node* context);
REDLAND_API REDLAND_DEPRECATED
librdf_stream* librdf_model_context_serialize(librdf_model* model, librdf_node* context);
REDLAND_API
int librdf_model_contains_context(librdf_model* model, librdf_node* context);

/* query language */
REDLAND_API
librdf_query_results* librdf_model_query_execute(librdf_model* model, librdf_query* query);

REDLAND_API
int librdf_model_sync(librdf_model* model);

REDLAND_API
librdf_storage* librdf_model_get_storage(librdf_model *model);

REDLAND_API
int librdf_model_load(librdf_model* model, librdf_uri *uri, const char *name, const char *mime_type, librdf_uri *type_uri);
REDLAND_API
unsigned char* librdf_model_to_counted_string(librdf_model* model, librdf_uri *uri, const char *name, const char *mime_type, librdf_uri *type_uri, size_t* string_length_p);
REDLAND_API
unsigned char* librdf_model_to_string(librdf_model* model, librdf_uri *uri, const char *name, const char *mime_type, librdf_uri *type_uri);

/* find statements in a given context */
REDLAND_API
librdf_stream* librdf_model_find_statements_in_context(librdf_model* model, librdf_statement* statement, librdf_node* context_node);

REDLAND_API
librdf_iterator* librdf_model_get_contexts(librdf_model* model);

REDLAND_API
int librdf_model_transaction_start(librdf_model* model);
REDLAND_API
int librdf_model_transaction_start_with_handle(librdf_model* model, void* handle);
REDLAND_API
int librdf_model_transaction_commit(librdf_model* model);
REDLAND_API
int librdf_model_transaction_rollback(librdf_model* model);
REDLAND_API
void* librdf_model_transaction_get_handle(librdf_model* model);


/**
 * LIBRDF_MODEL_FEATURE_CONTEXTS:
 *
 * Model feature contexts.
 *
 * If set, the model has redland contexts.
 */
#define LIBRDF_MODEL_FEATURE_CONTEXTS "http://feature.librdf.org/model-contexts"

/* features */
REDLAND_API
librdf_node* librdf_model_get_feature(librdf_model* model, librdf_uri* feature);
REDLAND_API
int librdf_model_set_feature(librdf_model* model, librdf_uri* feature, librdf_node* value);

#ifdef __cplusplus
}
#endif

#endif
