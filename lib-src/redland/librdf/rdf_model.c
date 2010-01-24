/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_model.c - RDF Graph (Model) interface
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

#ifndef STANDALONE

/**
 * librdf_init_model:
 * @world: redland world object
 *
 * INTERNAL - Initialise the model module.
 *
 **/
void
librdf_init_model(librdf_world *world)
{
  /* Always have model storage - must always be the default model */
  librdf_init_model_storage(world);
}


/**
 * librdf_finish_model:
 * @world: redland world object
 *
 * INTERNAL - Terminate the model module.
 *
 **/
void
librdf_finish_model(librdf_world *world)
{
  if(world->models) {
    raptor_free_sequence(world->models);
    world->models=NULL;
  }
}


/*
 * librdf_model_supports_contexts - helper function to determine if this model supports contexts
 **/
static REDLAND_INLINE int
librdf_model_supports_contexts(librdf_model* model) {
  return model->supports_contexts;
}



/* class methods */

static void
librdf_free_model_factory(librdf_model_factory* factory)
{
  if(factory->name)
    LIBRDF_FREE(librdf_model_factory, factory->name);
  if(factory->label)
    LIBRDF_FREE(librdf_model_factory, factory->label);
  LIBRDF_FREE(librdf_model_factory, factory);
}


/**
 * librdf_model_register_factory:
 * @world: redland world object
 * @name: the model factory name
 * @label: the storage factory label
 * @factory: pointer to function to call to register the factory
 *
 * Register a model factory.
 * 
 **/
void
librdf_model_register_factory(librdf_world *world, 
                              const char *name, const char *label,
                              void (*factory) (librdf_model_factory*)) 
{
  librdf_model_factory *model;
  int i;

  librdf_world_open(world);

#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
  LIBRDF_DEBUG2("Received registration for model %s\n", name);
#endif

  if(!world->models) {
    world->models=raptor_new_sequence((raptor_sequence_free_handler *)librdf_free_model_factory, NULL);
    if(!world->models)
      goto oom;
  }
  
  for(i=0;
      (model=(librdf_model_factory*)raptor_sequence_get_at(world->models, i));
      i++) {
    if(!strcmp(model->name, name)) {
      librdf_log(world,
                 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_MODEL, NULL,
                 "model %s already registered", model->name);
      return;
    }
  }

  model=(librdf_model_factory*)LIBRDF_CALLOC(librdf_model_factory, 1,
                                             sizeof(librdf_model_factory));
  if(!model)
    goto oom;

  model->name=(char*)LIBRDF_MALLOC(cstring, strlen(name)+1);
  if(!model->name)
    goto oom_tidy;
  strcpy(model->name, name);

  model->label=(char*)LIBRDF_MALLOC(cstring, strlen(label)+1);
  if(!model->label)
    goto oom_tidy;
  strcpy(model->label, label);

  if(raptor_sequence_push(world->models, model))
    goto oom;

  /* Call the model registration function on the new object */
  (*factory)(model);
  
#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
  LIBRDF_DEBUG3("%s has context size %d\n", name, model->context_length);
#endif

  return;

  oom_tidy:
  librdf_free_model_factory(model);
  oom:
  LIBRDF_FATAL1(world, LIBRDF_FROM_MODEL, "Out of memory");
}


/**
 * librdf_get_model_factory:
 * @world: redland world object
 * @name: the factory name or NULL for the default factory
 *
 * Get a model factory by name.
 * 
 * Return value: the factory object or NULL if there is no such factory
 **/
librdf_model_factory*
librdf_get_model_factory(librdf_world* world, const char *name) 
{
  librdf_model_factory *factory;

  librdf_world_open(world);

  /* return 1st model if no particular one wanted - why? */
  if(!name) {
    factory=(librdf_model_factory *)raptor_sequence_get_at(world->models, 0);
    if(!factory) {
      LIBRDF_DEBUG1("No (default) models registered\n");
      return NULL;
    }
  } else {
    int i;
    
    for(i=0;
        (factory=(librdf_model_factory*)raptor_sequence_get_at(world->models, i));
        i++) {
      if(!strcmp(factory->name, name))
        break;
    }
    /* else FACTORY name not found */
    if(!factory) {
      LIBRDF_DEBUG2("No model with name %s found\n", name);
      return NULL;
    }
  }
        
  return factory;
}


/**
 * librdf_model_enumerate:
 * @world: redland world object
 * @counter: index into the list of models
 * @name: pointer to store the name of the model (or NULL)
 * @label: pointer to store syntax readable label (or NULL)
 *
 * Get information on models.
 * 
 * Return value: non 0 on failure of if counter is out of range
 **/
int
librdf_model_enumerate(librdf_world* world,
                       const unsigned int counter,
                       const char **name, const char **label)
{
  librdf_model_factory *factory;
  
  librdf_world_open(world);

  factory=(librdf_model_factory*)raptor_sequence_get_at(world->models,
                                                        counter);
  if(!factory)
    return 1;
  
  if(name)
    *name=factory->name;
  if(label)
    *label=factory->label;
  return 0;
}


/**
 * librdf_new_model:
 * @world: redland world object
 * @storage: #librdf_storage to use
 * @options_string: options to initialise model
 *
 * Constructor - create a new storage #librdf_model object.
 *
 * The options are encoded as described in librdf_hash_from_string()
 * and can be NULL if none are required.
 *
 * Return value: a new #librdf_model object or NULL on failure
 */
librdf_model*
librdf_new_model (librdf_world *world,
                  librdf_storage *storage, const char *options_string) {
  librdf_hash* options_hash;
  librdf_model *model;

  librdf_world_open(world);

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(storage, librdf_storage, NULL);

  if(!storage)
    return NULL;
  
  options_hash=librdf_new_hash(world, NULL);
  if(!options_hash)
    return NULL;
  
  if(librdf_hash_from_string(options_hash, options_string)) {
    librdf_free_hash(options_hash);
    return NULL;
  }

  model=librdf_new_model_with_options(world, storage, options_hash);
  librdf_free_hash(options_hash);
  return model;
}


/**
 * librdf_new_model_with_options:
 * @world: redland world object
 * @storage: #librdf_storage storage to use
 * @options: #librdf_hash of options to use
 * 
 * Constructor - Create a new #librdf_model with storage.
 *
 * Options are presently not used.
 *
 * Return value: a new #librdf_model object or NULL on failure
 **/
librdf_model*
librdf_new_model_with_options(librdf_world *world,
                              librdf_storage *storage, librdf_hash* options)
{
  librdf_model *model;
  librdf_uri *uri;
  
  librdf_world_open(world);

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(storage, librdf_storage, NULL);

  if(!storage)
    return NULL;
  
  model=(librdf_model*)LIBRDF_CALLOC(librdf_model, 1, sizeof(librdf_model));
  if(!model)
    return NULL;
  
  model->world=world;

  model->factory=librdf_get_model_factory(world, "storage");
  if(!model->factory) {
    LIBRDF_FREE(librdf_model, model);
    return NULL;
  }
    
  model->context=LIBRDF_CALLOC(data, 1, model->factory->context_length);

  if(!model->context || model->factory->create(model, storage, options)) {
    if(model->context)
      LIBRDF_FREE(data, model->context);
    LIBRDF_FREE(librdf_model, model);
    return NULL;
  }

  uri=librdf_new_uri(world, (const unsigned char*)LIBRDF_MODEL_FEATURE_CONTEXTS);
  if(uri) {
    librdf_node *node=librdf_model_get_feature(model, uri);
    if(node) {
      model->supports_contexts=atoi((const char*)librdf_node_get_literal_value(node));
      librdf_free_node(node);
    }
    librdf_free_uri(uri);
  }

  model->usage=1;

  return model;
}


/**
 * librdf_new_model_from_model:
 * @model: the existing #librdf_model
 *
 * Copy constructor - create a new librdf_model from an existing one.
 * 
 * Creates a new model as a copy of the existing model in the same
 * storage context.
 * 
 * Return value: a new #librdf_model or NULL on failure
 **/
librdf_model*
librdf_new_model_from_model(librdf_model* model)
{
  librdf_model *new_model;

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, NULL);

  new_model=model->factory->clone(model);
  if(new_model) {
    new_model->supports_contexts=model->supports_contexts;
    new_model->usage=1;
  }
  return new_model;
}


/**
 * librdf_free_model:
 * @model: #librdf_model model to destroy
 *
 * Destructor - Destroy a #librdf_model object.
 * 
 **/
void
librdf_free_model(librdf_model *model)
{
  librdf_iterator* iterator;
  librdf_model* m;

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN(model, librdf_model);

  if(--model->usage)
    return;
  
  if(model->sub_models) {
    iterator=librdf_list_get_iterator(model->sub_models);
    if(iterator) {
      while(!librdf_iterator_end(iterator)) {
        m=(librdf_model*)librdf_iterator_get_object(iterator);
        if(m)
          librdf_free_model(m);
        librdf_iterator_next(iterator);
      }
      librdf_free_iterator(iterator);
    }
    librdf_free_list(model->sub_models);
  } else {
    model->factory->destroy(model);
  }
  LIBRDF_FREE(data, model->context);

  LIBRDF_FREE(librdf_model, model);
}


void
librdf_model_add_reference(librdf_model *model)
{
  model->usage++;
}

void
librdf_model_remove_reference(librdf_model *model)
{
  model->usage--;
}


/* methods */

/**
 * librdf_model_size:
 * @model: #librdf_model object
 *
 * Get the number of statements in the model.
 * 
 * WARNING: Not all underlying stores can return the size of the graph
 * In which case the return value will be negative.
 *
 * Return value: the number of statements or <0 if not possible
 **/
int
librdf_model_size(librdf_model* model)
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, -1);

  return model->factory->size(model);
}


/**
 * librdf_model_add_statement:
 * @model: model object
 * @statement: statement object
 *
 * Add a statement to the model.
 * 
 * The passed-in statement is copied when added to the model, not
 * shared with the model.  It must be a complete statement - all
 * of subject, predicate, object parts must be present.
 *
 * Only statements that are legal RDF can be added: URI or blank subject,
 * URI predicate and URI or blank or literal object (i.e. anything).
 *
 * If the statement already exists in the model, it is not added.
 * Duplicate statements can be added when used with Redland Contexts
 * such as with #librdf_model_context_add_statement
 *
 * Return value: non 0 on failure
 **/
int
librdf_model_add_statement(librdf_model* model, librdf_statement* statement)
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(statement, librdf_statement, 1);

  if(!librdf_statement_is_complete(statement))
    return 1;

  return model->factory->add_statement(model, statement);
}


/**
 * librdf_model_add_statements:
 * @model: model object
 * @statement_stream: stream of statements to use
 *
 * Add a stream of statements to the model.
 * 
 * If any of the statements are illegal RDF statements they will
 * be skipped and not added.  See #librdf_model_add_statement for the detail.
 *
 * If any of the statements already exists in the store, they are not
 * added unless Redland contexts are being used.  See also
 * #librdf_model_context_add_statements
 *
 * Return value: non 0 on failure
 **/
int
librdf_model_add_statements(librdf_model* model, librdf_stream* statement_stream)
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(statement_stream, librdf_statement, 1);

  return model->factory->add_statements(model, statement_stream);
}


/**
 * librdf_model_add:
 * @model: model object
 * @subject: #librdf_node of subject
 * @predicate: #librdf_node of predicate
 * @object: #librdf_node of object (literal or resource)
 *
 * Create and add a new statement about a resource to the model.
 * 
 * After this method, the #librdf_node objects become owned by the model.
 * All of subject, predicate and object must be non-NULL.
 * 
 * Return value: non 0 on failure
 **/
int
librdf_model_add(librdf_model* model, librdf_node* subject, 
		 librdf_node* predicate, librdf_node* object)
{
  librdf_statement *statement;
  int result;

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(subject, librdf_node, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(predicate, librdf_node, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(object, librdf_node, 1);

  if(!subject ||
     (!librdf_node_is_resource(subject) && !librdf_node_is_blank(subject)))
    return 1;

  if(!predicate || !librdf_node_is_resource(predicate))
     return 1;

  if(!object)
    return 1;

  statement=librdf_new_statement(model->world);
  if(!statement)
    return 1;

  librdf_statement_set_subject(statement, subject);
  librdf_statement_set_predicate(statement, predicate);
  librdf_statement_set_object(statement, object);

  result=librdf_model_add_statement(model, statement);
  librdf_free_statement(statement);
  
  return result;
}


/**
 * librdf_model_add_typed_literal_statement:
 * @model: model object
 * @subject: #librdf_node of subject
 * @predicate: #librdf_node of predicate
 * @literal: string literal content
 * @xml_language: language of literal
 * @datatype_uri: datatype #librdf_uri
 *
 * Create and add a new statement about a typed literal to the model.
 * 
 * After this method, the #librdf_node subject and predicate become
 * owned by the model.
 * 
 * The language can be set to NULL if not used.
 * All of subject, predicate and literal must be non-NULL.
 *
 * Return value: non 0 on failure
 **/
int
librdf_model_add_typed_literal_statement(librdf_model* model, 
                                         librdf_node* subject, 
                                         librdf_node* predicate, 
                                         const unsigned char* literal,
                                         const char *xml_language,
                                         librdf_uri *datatype_uri)
{
  librdf_node* object;

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(subject, librdf_node, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(predicate, librdf_node, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(literal, string, 1);

  if(!subject ||
     (!librdf_node_is_resource(subject) && !librdf_node_is_blank(subject)))
    return 1;

  if(!predicate || !librdf_node_is_resource(predicate))
     return 1;

  if(!literal)
    return 1;

  object=librdf_new_node_from_typed_literal(model->world,
                                            literal, xml_language, 
                                            datatype_uri);
  if(!object)
    return 1;
  
  return librdf_model_add(model, subject, predicate, object);
}


/**
 * librdf_model_add_string_literal_statement:
 * @model: model object
 * @subject: #librdf_node of subject
 * @predicate: #librdf_node of predicate
 * @literal: string literal conten
 * @xml_language: language of literal
 * @is_wf_xml: literal is XML
 *
 * Create and add a new statement about a literal to the model.
 * 
 * The language can be set to NULL if not used.
 * All of subject, predicate and literal must be non-NULL.
 *
 * 0.9.12: xml_space argument deleted
 *
 * Return value: non 0 on failure
 **/
int
librdf_model_add_string_literal_statement(librdf_model* model, 
                                          librdf_node* subject, 
                                          librdf_node* predicate, 
                                          const unsigned char* literal,
                                          const char *xml_language,
                                          int is_wf_xml)
{
  librdf_node* object;
  int result;
  
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(subject, librdf_node, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(predicate, librdf_node, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(literal, string, 1);

  if(!subject ||
     (!librdf_node_is_resource(subject) && !librdf_node_is_blank(subject)))
    return 1;

  if(!predicate || !librdf_node_is_resource(predicate))
     return 1;

  if(!literal)
    return 1;

  object=librdf_new_node_from_literal(model->world,
                                      literal, xml_language, 
                                      is_wf_xml);
  if(!object)
    return 1;
  
  result=librdf_model_add(model, subject, predicate, object);
  if(result)
    librdf_free_node(object);
  
  return result;
}


/**
 * librdf_model_remove_statement:
 * @model: the model object
 * @statement: the statement
 *
 * Remove a known statement from the model.
 *
 * It must be a complete statement - all of subject, predicate, object
 * parts must be present and a legal RDF triple.
 *
 * Return value: non 0 on failure
 **/
int
librdf_model_remove_statement(librdf_model* model, librdf_statement* statement)
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(statement, librdf_statement, 1);

  if(!librdf_statement_is_complete(statement))
    return 1;

  return model->factory->remove_statement(model, statement);
}


/**
 * librdf_model_contains_statement:
 * @model: the model object
 * @statement: the statement
 *
 * Check for a statement in the model.
 * 
 * It must be a complete statement - all of subject, predicate,
 * object parts must be present and a legal RDF triple.  Use
 * librdf_model_find_statements to search for partial statement
 * matches.
 *
 * WARNING: librdf_model_contains_statement may not work correctly
 * with stores using contexts.  In this case, a search using
 * librdf_model_find_statements for a non-empty list will
 * return the correct result.
 *
 * Return value: non 0 if the model contains the statement (>0 if the statement is illegal)
 **/
int
librdf_model_contains_statement(librdf_model* model, librdf_statement* statement)
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, 0);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(statement, librdf_statement, 0);

  if(!librdf_statement_is_complete(statement))
    return 1;

  return model->factory->contains_statement(model, statement);
}


/**
 * librdf_model_as_stream:
 * @model: the model object
 *
 * List the model contents as a stream of statements.
 * 
 * Return value: a #librdf_stream or NULL on failure
 **/
librdf_stream*
librdf_model_as_stream(librdf_model* model)
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, NULL);

  return model->factory->serialise(model);
}


/**
 * librdf_model_serialise:
 * @model: the model object
 *
 * Serialise the entire model as a stream (DEPRECATED).
 * 
 * DEPRECATED to reduce confusion with the librdf_serializer class.
 * Please use librdf_model_as_stream.
 *
 * Return value: a #librdf_stream or NULL on failure
 **/
librdf_stream*
librdf_model_serialise(librdf_model* model)
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, NULL);

  return model->factory->serialise(model);
}


/**
 * librdf_model_find_statements:
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
librdf_stream*
librdf_model_find_statements(librdf_model* model, 
                             librdf_statement* statement)
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(statement, librdf_statement, NULL);

  return model->factory->find_statements(model, statement);
}


/**
 * librdf_model_get_sources:
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
librdf_iterator*
librdf_model_get_sources(librdf_model *model,
                         librdf_node *arc, librdf_node *target) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(arc, librdf_node, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(target, librdf_node, NULL);

  return model->factory->get_sources(model, arc, target);
}


/**
 * librdf_model_get_arcs:
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
librdf_iterator*
librdf_model_get_arcs(librdf_model *model,
                      librdf_node *source, librdf_node *target) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(source, librdf_node, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(target, librdf_node, NULL);

  return model->factory->get_arcs(model, source, target);
}


/**
 * librdf_model_get_targets:
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
librdf_iterator*
librdf_model_get_targets(librdf_model *model,
                         librdf_node *source, librdf_node *arc) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(source, librdf_node, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(arc, librdf_node, NULL);

  return model->factory->get_targets(model, source, arc);
}


/**
 * librdf_model_get_source:
 * @model: #librdf_model object
 * @arc: #librdf_node arc
 * @target: #librdf_node target
 *
 * Return one source (subject) of arc in an RDF graph given arc (predicate) and target (object).
 * 
 * Searches the model for arcs matching the given arc and target
 * and returns one #librdf_node object
 * 
 * Return value:  a new #librdf_node object or NULL on failure
 **/
librdf_node*
librdf_model_get_source(librdf_model *model,
                        librdf_node *arc, librdf_node *target) 
{
  librdf_iterator *iterator;
  librdf_node *node;

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(arc, librdf_node, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(target, librdf_node, NULL);

  iterator=librdf_model_get_sources(model, arc, target);
  if(!iterator)
    return NULL;
  
  node=(librdf_node*)librdf_iterator_get_object(iterator);
  if(node)
    node=librdf_new_node_from_node(node);
  librdf_free_iterator(iterator);
  return node;
}


/**
 * librdf_model_get_arc:
 * @model: #librdf_model object
 * @source: #librdf_node source
 * @target: #librdf_node target
 *
 * Return one arc (predicate) of an arc in an RDF graph given source (subject) and target (object).
 * 
 * Searches the model for arcs matching the given source and target
 * and returns one #librdf_node object
 * 
 * Return value:  a new #librdf_node object or NULL on failure
 **/
librdf_node*
librdf_model_get_arc(librdf_model *model,
                     librdf_node *source, librdf_node *target) 
{
  librdf_iterator *iterator;
  librdf_node *node;

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(source, librdf_node, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(target, librdf_node, NULL);

  iterator=librdf_model_get_arcs(model, source, target);
  if(!iterator)
    return NULL;
  
  node=(librdf_node*)librdf_iterator_get_object(iterator);
  if(node)
    node=librdf_new_node_from_node(node);
  librdf_free_iterator(iterator);
  return node;
}


/**
 * librdf_model_get_target:
 * @model: #librdf_model object
 * @source: #librdf_node source
 * @arc: #librdf_node arc
 *
 * Return one target (object) of an arc in an RDF graph given source (subject) and arc (predicate).
 * 
 * Searches the model for targets matching the given source and arc
 * and returns one #librdf_node object
 * 
 * Return value:  a new #librdf_node object or NULL on failure
 **/
librdf_node*
librdf_model_get_target(librdf_model *model,
                        librdf_node *source, librdf_node *arc) 
{
  librdf_iterator *iterator;
  librdf_node *node;

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(source, librdf_node, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(arc, librdf_node, NULL);

  iterator=librdf_model_get_targets(model, source, arc);
  if(!iterator)
    return NULL;
  
  node=(librdf_node*)librdf_iterator_get_object(iterator);
  if(node)
    node=librdf_new_node_from_node(node);
  librdf_free_iterator(iterator);
  return node;
}


/**
 * librdf_model_add_submodel:
 * @model: the model object
 * @sub_model: the sub model to add
 *
 * Add a sub-model to the model.
 * 
 * FIXME: Not tested
 * 
 * Return value: non 0 on failure
 **/
int
librdf_model_add_submodel(librdf_model* model, librdf_model* sub_model)
{
  librdf_list *l=model->sub_models;
  
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(sub_model, librdf_model, 1);

  if(!l) {
    l=librdf_new_list(model->world);
    if(!l)
      return 1;
    model->sub_models=l;
  }
  
  if(librdf_list_add(l, sub_model))
    return 1;
  
  return 0;
}



/**
 * librdf_model_remove_submodel:
 * @model: the model object
 * @sub_model: the sub model to remove
 *
 * Remove a sub-model from the model.
 * 
 * FIXME: Not tested
 * 
 * Return value: non 0 on failure
 **/
int
librdf_model_remove_submodel(librdf_model* model, librdf_model* sub_model)
{
  librdf_list *l=model->sub_models;
  
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(sub_model, librdf_model, 1);

  if(!l)
    return 1;
  if(!librdf_list_remove(l, sub_model))
    return 1;
  
  return 0;
}



/**
 * librdf_model_get_arcs_in:
 * @model: #librdf_model object
 * @node: #librdf_node resource node
 *
 * Return the properties pointing to the given resource.
 * 
 * Return value:  #librdf_iterator of #librdf_node objects (may be empty) or NULL on failure
 **/
librdf_iterator*
librdf_model_get_arcs_in(librdf_model *model, librdf_node *node) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(node, librdf_node, NULL);

  return model->factory->get_arcs_in(model, node);
}


/**
 * librdf_model_get_arcs_out:
 * @model: #librdf_model object
 * @node: #librdf_node resource node
 *
 * Return the properties pointing from the given resource.
 * 
 * Return value:  #librdf_iterator of #librdf_node objects (may be empty) or NULL on failure
 **/
librdf_iterator*
librdf_model_get_arcs_out(librdf_model *model, librdf_node *node) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(node, librdf_node, NULL);

  return model->factory->get_arcs_out(model, node);
}


/**
 * librdf_model_has_arc_in:
 * @model: #librdf_model object
 * @node: #librdf_node resource node
 * @property: #librdf_node property node
 *
 * Check if a node has a given property pointing to it.
 * 
 * Return value: non 0 if arc property does point to the resource node
 **/
int
librdf_model_has_arc_in(librdf_model *model, librdf_node *node, 
                        librdf_node *property) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, 0);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(node, librdf_node, 0);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(property, librdf_node, 0);

  return model->factory->has_arc_in(model, node, property);
}


/**
 * librdf_model_has_arc_out:
 * @model: #librdf_model object
 * @node: #librdf_node resource node
 * @property: #librdf_node property node
 *
 * Check if a node has a given property pointing from it.
 * 
 * Return value: non 0 if arc property does point from the resource node
 **/
int
librdf_model_has_arc_out(librdf_model *model, librdf_node *node,
                         librdf_node *property) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, 0);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(node, librdf_node, 0);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(property, librdf_node, 0);

  return model->factory->has_arc_out(model, node, property);
}




/**
 * librdf_model_print:
 * @model: the model object
 * @fh: the FILE stream to print to
 *
 * Print the model.
 * 
 * This method is for debugging and the format of the output should
 * not be relied on.
 **/
void
librdf_model_print(librdf_model *model, FILE *fh)
{
  librdf_stream* stream;
  
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN(model, librdf_model);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN(fh, FILE*);

  stream=librdf_model_as_stream(model);
  if(!stream)
    return;
  fputs("[[\n", fh);
  librdf_stream_print(stream, fh);
  fputs("]]\n", fh);
  librdf_free_stream(stream);
}


/**
 * librdf_model_context_add_statement:
 * @model: #librdf_model object
 * @context: #librdf_node context
 * @statement: #librdf_statement statement object
 *
 * Add a statement to a model with a context.
 * 
 * It must be a complete statement - all
 * of subject, predicate, object parts must be present.
 *
 * If @context is NULL, this is equivalent to librdf_model_add_statement
 *
 * Return value: Non 0 on failure
 **/
int
librdf_model_context_add_statement(librdf_model* model, 
                                   librdf_node* context,
                                   librdf_statement* statement) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(statement, librdf_statement, 1);

  if(!librdf_statement_is_complete(statement))
    return 1;

  if(!librdf_model_supports_contexts(model)) {
    librdf_log(model->world, 0, LIBRDF_LOG_WARN, LIBRDF_FROM_MODEL, NULL,
               "Model does not support contexts");
    return 1;
  }

  return model->factory->context_add_statement(model, context, statement);
}



/**
 * librdf_model_context_add_statements:
 * @model: #librdf_model object
 * @context: #librdf_node context
 * @stream: #librdf_stream stream object
 *
 * Add statements to a model with a context.
 * 
 * If @context is NULL, this is equivalent to librdf_model_add_statements
 *
 * Return value: Non 0 on failure
 **/
int
librdf_model_context_add_statements(librdf_model* model, 
                                    librdf_node* context,
                                    librdf_stream* stream) 
{
  int status=0;
  
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(stream, librdf_stream, 1);

  if(!stream)
    return 1;

  if(!librdf_model_supports_contexts(model)) {
    librdf_log(model->world, 0, LIBRDF_LOG_WARN, LIBRDF_FROM_MODEL, NULL,
               "Model does not support contexts");
    return 1;
  }

  if(model->factory->context_add_statements)
    return model->factory->context_add_statements(model, context, stream);

  while(!librdf_stream_end(stream)) {
    librdf_statement* statement=librdf_stream_get_object(stream);
    if(!statement)
      break;
    status=librdf_model_context_add_statement(model, context, statement);
    if(status)
      break;
    librdf_stream_next(stream);
  }

  return status;
}



/**
 * librdf_model_context_remove_statement:
 * @model: #librdf_model object
 * @context: #librdf_node context
 * @statement: #librdf_statement statement
 *
 * Remove a statement from a model in a context.
 * 
 * It must be a complete statement - all of subject, predicate, object
 * parts must be present.
 *
 * If @context is NULL, this is equivalent to librdf_model_remove_statement
 *
 * Return value: Non 0 on failure
 **/
int
librdf_model_context_remove_statement(librdf_model* model,
                                      librdf_node* context,
                                      librdf_statement* statement) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(statement, librdf_statement, 1);

  if(!librdf_statement_is_complete(statement))
    return 1;

  if(!librdf_model_supports_contexts(model)) {
    librdf_log(model->world, 0, LIBRDF_LOG_WARN, LIBRDF_FROM_MODEL, NULL,
               "Model does not support contexts");
    return 1;
  }

  return model->factory->context_remove_statement(model, context, statement);
}


/**
 * librdf_model_context_remove_statements:
 * @model: #librdf_model object
 * @context: #librdf_node context
 *
 * Remove statements from a model with the given context.
 * 
 * Return value: Non 0 on failure
 **/
int
librdf_model_context_remove_statements(librdf_model* model,
                                       librdf_node* context) 
{
  librdf_stream *stream;

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(context, librdf_node, 1);

  if(!librdf_model_supports_contexts(model)) {
    librdf_log(model->world, 0, LIBRDF_LOG_WARN, LIBRDF_FROM_MODEL, NULL,
               "Model does not support contexts");
    return 1;
  }

  if(model->factory->context_remove_statements)
    return model->factory->context_remove_statements(model, context);

  stream=librdf_model_context_as_stream(model, context);
  if(!stream)
    return 1;

  while(!librdf_stream_end(stream)) {
    librdf_statement *statement=librdf_stream_get_object(stream);
    if(!statement)
      break;
    librdf_model_context_remove_statement(model, context, statement);
    librdf_stream_next(stream);
  }
  librdf_free_stream(stream);  
  return 0;
}


/**
 * librdf_model_context_as_stream:
 * @model: #librdf_model object
 * @context: #librdf_node context
 *
 * List all statements in a model context.
 * 
 * Return value: #librdf_stream of statements or NULL on failure
 **/
librdf_stream*
librdf_model_context_as_stream(librdf_model* model, librdf_node* context) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(context, librdf_node, NULL);

  if(!librdf_model_supports_contexts(model)) {
    librdf_log(model->world, 0, LIBRDF_LOG_WARN, LIBRDF_FROM_MODEL, NULL,
               "Model does not support contexts");
    return NULL;
  }

  return model->factory->context_serialize(model, context);
}


/**
 * librdf_model_context_serialize:
 * @model: #librdf_model object
 * @context: #librdf_node context
 *
 * List all statements in a model context.
 * 
 * DEPRECATED to reduce confusion with the librdf_serializer class.
 * Please use librdf_model_context_as_stream.
 *
 * Return value: #librdf_stream of statements or NULL on failure
 **/
librdf_stream*
librdf_model_context_serialize(librdf_model* model, librdf_node* context) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(context, librdf_node, NULL);

  if(!librdf_model_supports_contexts(model)) {
    librdf_log(model->world, 0, LIBRDF_LOG_WARN, LIBRDF_FROM_MODEL, NULL,
               "Model does not support contexts");
    return NULL;
  }

  return model->factory->context_serialize(model, context);
}


/**
 * librdf_model_query_execute:
 * @model: #librdf_model object
 * @query: #librdf_query object
 *
 * Execute a query against the model.
 * 
 * Run the given query against the model and return a #librdf_stream of
 * matching #librdf_statement objects
 * 
 * Return value: #librdf_query_results or NULL on failure
 **/
librdf_query_results*
librdf_model_query_execute(librdf_model* model, librdf_query* query) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(query, librdf_query, NULL);

  return model->factory->query_execute(model, query);
}


/**
 * librdf_model_sync:
 * @model: #librdf_model object
 *
 * Synchronise the model to the model implementation.
 * 
 * Return value: non-0 on failure
 **/
int
librdf_model_sync(librdf_model* model) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, 1);

  if(model->factory->sync)
    return model->factory->sync(model);

  return 0;
}


/**
 * librdf_model_get_storage:
 * @model: #librdf_model object
 *
 * Return the storage of this model.
 * 
 * Note: this can only return one storage, so model implementations
 * that have multiple #librdf_storage internally may chose not to
 * implement this.
 *
 * Return value:  #librdf_storage or NULL if this has no store
 **/
librdf_storage*
librdf_model_get_storage(librdf_model *model)
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, NULL);

  if(model->factory->get_storage)
    return model->factory->get_storage(model);
  else
    return NULL;
}


/**
 * librdf_model_find_statements_in_context:
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
librdf_stream*
librdf_model_find_statements_in_context(librdf_model* model, librdf_statement* statement, librdf_node* context_node) 
{
  librdf_stream *stream;

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(statement, librdf_statement, NULL);

  if(!librdf_model_supports_contexts(model)) {
    librdf_log(model->world, 0, LIBRDF_LOG_WARN, LIBRDF_FROM_MODEL, NULL,
               "Model does not support contexts");
    return NULL;
  }

  if(model->factory->find_statements_in_context)
    return model->factory->find_statements_in_context(model, statement, context_node);

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
 * librdf_model_get_contexts:
 * @model: #librdf_model object
 *
 * Return the list of contexts in the graph.
 * 
 * Returns an iterator of #librdf_node context nodes for each
 * context in the graph.
 *
 * Return value: #librdf_iterator of context nodes or NULL on failure or if contexts are not supported
 **/
librdf_iterator*
librdf_model_get_contexts(librdf_model* model) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, NULL);

  if(!librdf_model_supports_contexts(model)) {
    librdf_log(model->world, 0, LIBRDF_LOG_WARN, LIBRDF_FROM_MODEL, NULL,
               "Model does not support contexts");
    return NULL;
  }

  if(model->factory->get_contexts)
    return model->factory->get_contexts(model);
  else
    return NULL;
}


/**
 * librdf_model_get_feature:
 * @model: #librdf_model object
 * @feature: #librdf_uri feature property
 *
 * Get the value of a graph feature .
 * 
 * Return value: new #librdf_node feature value or NULL if no such feature
 * exists or the value is empty.
 **/
librdf_node*
librdf_model_get_feature(librdf_model* model, librdf_uri* feature)
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(feature, librdf_uri, NULL);

  if(model->factory->get_feature)
    return model->factory->get_feature(model, feature);
  return NULL;
}


/**
 * librdf_model_set_feature:
 * @model: #librdf_model object
 * @feature: #librdf_uri feature property
 * @value: #librdf_node feature property value
 *
 * Set the value of a graph feature.
 * 
 * Return value: non 0 on failure (negative if no such feature)
 **/
int
librdf_model_set_feature(librdf_model* model, librdf_uri* feature,
                         librdf_node* value)
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, -1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(feature, librdf_uri, -1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(value, librdf_node, -1);

  if(model->factory->set_feature)
    return model->factory->set_feature(model, feature, value);
  return -1;
}


/**
 * librdf_model_find_statements_with_options:
 * @model: #librdf_model object
 * @statement: #librdf_statement partial statement to find
 * @context_node: #librdf_node context node or NULL.
 * @options: #librdf_hash of matching options or NULL
 *
 * Search the model for matching statements with match options.
 * 
 * Searches the model for a (partial) statement as described in
 * librdf_statement_match() and returns a #librdf_stream of
 * matching #librdf_statement objects.
 * 
 * If options is given then the match is made according to
 * the given options.  If options is NULL, this is equivalent
 * to librdf_model_find_statements_in_context.
 * 
 * Return value:  #librdf_stream of matching statements (may be empty) or NULL on failure
 **/
librdf_stream*
librdf_model_find_statements_with_options(librdf_model* model,
                                          librdf_statement* statement,
                                          librdf_node* context_node,
                                          librdf_hash* options) 
{
  if(context_node && !librdf_model_supports_contexts(model)) {
    librdf_log(model->world, 0, LIBRDF_LOG_WARN, LIBRDF_FROM_MODEL, NULL,
               "Model does not support contexts");
    return NULL;
  }

  if(model->factory->find_statements_with_options)
    return model->factory->find_statements_with_options(model, statement, context_node, options);
  else
    return librdf_model_find_statements_in_context(model, statement, context_node);
}


/**
 * librdf_model_load:
 * @model: #librdf_model object
 * @uri: the URI to read the content
 * @name: the name of the parser (or NULL)
 * @mime_type: the MIME type of the syntax (NULL if not used)
 * @type_uri: URI identifying the syntax (NULL if not used)
 *
 * Load content from a URI into the model.
 *
 * If the name field is NULL, the library will try to guess
 * the parser to use from the uri, mime_type and type_uri fields.
 * This is done via the raptor_guess_parser_name function.
 * 
 * Return value: non 0 on failure
 **/
int
librdf_model_load(librdf_model* model, librdf_uri *uri,
                  const char *name, const char *mime_type, 
                  librdf_uri *type_uri)
{
  int rc=0;
  librdf_parser* parser;

  if(name && !*name)
    name=NULL;
  if(mime_type && !*mime_type)
    mime_type=NULL;

  if(!name)
    name=raptor_guess_parser_name((raptor_uri*)type_uri, mime_type,
                                  NULL, 0, librdf_uri_as_string(uri));
  parser=librdf_new_parser(model->world, name, NULL, NULL);
  if(!parser)
    return 1;

  rc=librdf_parser_parse_into_model(parser, uri, NULL, model);
  librdf_free_parser(parser);
  return rc;
}


/**
 * librdf_model_to_counted_string:
 * @model: #librdf_model object
 * @uri: base URI to use in serializing (or NULL if not used)
 * @name: the name of the serializer (or NULL for default)
 * @mime_type: the MIME type of the syntax (NULL if not used)
 * @type_uri: URI identifying the syntax (NULL if not used)
 * @string_length_p: pointer to location to store string length (or NULL)
 *
 * Write serialized model to a string.
 *
 * If the name field is NULL, the default serializer will be used.
 *
 * Note: the returned string must be freed by the caller.
 *
 * Return value: new string or NULL on failure
 **/
unsigned char*
librdf_model_to_counted_string(librdf_model* model, librdf_uri *uri,
                               const char *name, const char *mime_type, 
                               librdf_uri *type_uri, size_t* string_length_p)
{
  unsigned char *string=NULL;
  librdf_serializer* serializer;
  
  if(name && !*name)
    name=NULL;
  if(mime_type && !*mime_type)
    mime_type=NULL;

  serializer=librdf_new_serializer(model->world, name, mime_type, type_uri);
  if(!serializer)
    return NULL;

  string=librdf_serializer_serialize_model_to_counted_string(serializer,
                                                             uri, model,
                                                             string_length_p);
  librdf_free_serializer(serializer);
  return string;
}


/**
 * librdf_model_to_string:
 * @model: #librdf_model object
 * @uri: base URI to use in serializing (or NULL if not used)
 * @name: the name of the serializer (or NULL for default)
 * @mime_type: the MIME type of the syntax (NULL if not used)
 * @type_uri: URI identifying the syntax (NULL if not used)
 *
 * Write serialized model to a string.
 *
 * If the name field is NULL, the default serializer will be used.
 *
 * Note: the returned string must be freed by the caller.
 *
 * Return value: new string or NULL on failure
 **/
unsigned char*
librdf_model_to_string(librdf_model* model, librdf_uri *uri,
                       const char *name, const char *mime_type, 
                       librdf_uri *type_uri) {
  return librdf_model_to_counted_string(model, uri, 
                                        name, mime_type, type_uri,
                                        NULL);
}


/**
 * librdf_model_contains_context:
 * @model: the model object
 * @context: the contest
 *
 * Check for a context in the model.
 * 
 * Return value: non 0 if the model contains the context node
 **/
int
librdf_model_contains_context(librdf_model* model, librdf_node* context) {
  librdf_stream* stream;
  int result;

  stream=librdf_model_context_as_stream(model, context);
  if(!stream)
    return 0;
  
  result=!librdf_stream_end(stream);
  librdf_free_stream(stream);

  return result;
}


/**
 * librdf_model_transaction_start:
 * @model: the model object
 * 
 * Start a transaction
 * 
 * Return value: non-0 on failure
 **/
int
librdf_model_transaction_start(librdf_model* model) 
{
  if(model->factory->transaction_start)
    return model->factory->transaction_start(model);
  else
    return 1;
}


/**
 * librdf_model_transaction_start_with_handle:
 * @model: the model object
 * @handle: the transaction object
 * 
 * Start a transaction using an existing external transaction object.
 * 
 * Return value: non-0 on failure
 **/
int
librdf_model_transaction_start_with_handle(librdf_model* model, void* handle)
{
  if(model->factory->transaction_start_with_handle)
    return model->factory->transaction_start_with_handle(model, handle);
  else
    return 1;
}


/**
 * librdf_model_transaction_commit:
 * @model: the model object
 * 
 * Commit a transaction.
 * 
 * Return value: non-0 on failure 
 **/
int
librdf_model_transaction_commit(librdf_model* model) 
{
  if(model->factory->transaction_commit)
    return model->factory->transaction_commit(model);
  else
    return 1;
}


/**
 * librdf_model_transaction_rollback:
 * @model: the model object
 * 
 * Rollback a transaction.
 * 
 * Return value: non-0 on failure 
 **/
int
librdf_model_transaction_rollback(librdf_model* model) 
{
  if(model->factory->transaction_rollback)
    return model->factory->transaction_rollback(model);
  else
    return 1;
}


/**
 * librdf_model_transaction_get_handle:
 * @model: the model object
 * 
 * Get the current transaction handle.
 * 
 * Return value: non-0 on failure 
 **/
void*
librdf_model_transaction_get_handle(librdf_model* model) 
{
  if(model->factory->transaction_get_handle)
    return model->factory->transaction_get_handle(model);
  else
    return NULL;
}

#endif


/* TEST CODE */


#ifdef STANDALONE

/* one more prototype */
int main(int argc, char *argv[]);

#define EX1_CONTENT \
"<?xml version=\"1.0\"?>\n" \
"<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\n" \
"         xmlns:dc=\"http://purl.org/dc/elements/1.1/\">\n" \
"  <rdf:Description rdf:about=\"http://purl.org/net/dajobe/\">\n" \
"    <dc:title>Dave Beckett's Home Page</dc:title>\n" \
"    <dc:creator>Dave Beckett</dc:creator>\n" \
"    <dc:description>The generic home page of Dave Beckett.</dc:description>\n" \
"  </rdf:Description>\n" \
"</rdf:RDF>"

#define EX2_CONTENT \
"<?xml version=\"1.0\"?>\n" \
"<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\n" \
"         xmlns:dc=\"http://purl.org/dc/elements/1.1/\">\n" \
"  <rdf:Description rdf:about=\"http://purl.org/net/dajobe/\">\n" \
"    <dc:title>Dave Beckett's Home Page</dc:title>\n" \
"    <dc:creator>Dave Beckett</dc:creator>\n" \
"    <dc:description>I do development-based research on RDF, metadata and web searching.</dc:description>\n" \
"    <dc:rights>Copyright &#169; 2002 Dave Beckett</dc:rights>\n" \
"  </rdf:Description>\n" \
"</rdf:RDF>"

int test_model_cloning(char const *program, librdf_world *);

int
main(int argc, char *argv[]) 
{
  librdf_storage* storage;
  librdf_model* model;
  librdf_statement *statement;
  librdf_parser* parser;
  librdf_stream* stream;
  const char *parser_name="rdfxml";
  #define URI_STRING_COUNT 2
  const unsigned char *file_uri_strings[URI_STRING_COUNT]={(const unsigned char*)"http://example.org/test1.rdf", (const unsigned char*)"http://example.org/test2.rdf"};
  const unsigned char *file_content[URI_STRING_COUNT]={(const unsigned char*)EX1_CONTENT, (const unsigned char*)EX2_CONTENT};
  librdf_uri* uris[URI_STRING_COUNT];
  librdf_node* nodes[URI_STRING_COUNT];
  int i;
  const char *program=librdf_basename((const char*)argv[0]);
  /* initialise dependent modules - all of them! */
  librdf_world *world=librdf_new_world();
  librdf_iterator *iterator, *second_iterator;
  librdf_node *n1, *n2;
  int count;
  int expected_count;
#define EXPECTED_BAD_STRING_LENGTH 317
  librdf_uri* base_uri;
  unsigned char *string;
  size_t string_length=0;
  int remove_count=0;
  int status=0;
  const char* storage_type;
  const char* storage_name;
  const char* storage_options;

  librdf_world_open(world);

  /* Test model cloning first */
  if(test_model_cloning(program, world))
    return(1);
  
  /* Get storage configuration */
  storage_type=getenv("REDLAND_TEST_STORAGE_TYPE");
  storage_name=getenv("REDLAND_TEST_STORAGE_NAME");
  storage_options=getenv("REDLAND_TEST_STORAGE_OPTIONS");
  if(!(storage_type && storage_name && storage_options)) {
    /* default is to test in memory */
    storage_type="memory";
    storage_name=NULL;
    storage_options="write='yes',new='yes',contexts='yes'";
  }

  fprintf(stderr, "%s: Creating new %s storage\n", program, storage_type);
  storage=librdf_new_storage(world, storage_type, storage_name, storage_options);
  if(!storage) {
    fprintf(stderr, "%s: Failed to create new %s storage name %s with options %s\n", program, storage_type, storage_name, storage_options);
    return(1);
  }

  fprintf(stderr, "%s: Creating model\n", program);
  model=librdf_new_model(world, storage, NULL);
  if(!model) {
    fprintf(stderr, "%s: Failed to create new model\n", program);
    return(1);
  }

  statement=librdf_new_statement(world);
  /* after this, nodes become owned by model */
  librdf_statement_set_subject(statement, librdf_new_node_from_uri_string(world, (const unsigned char*)"http://www.dajobe.org/"));
  librdf_statement_set_predicate(statement, librdf_new_node_from_uri_string(world, (const unsigned char*)"http://purl.org/dc/elements/1.1/creator"));

  if(!librdf_model_add_statement(model, statement)) {
    fprintf(stderr, "%s: librdf_model_add_statement unexpectedly succeeded adding a partial statement\n", program);
    return(1);
  }

  librdf_statement_set_object(statement, librdf_new_node_from_literal(world, (const unsigned char*)"Dave Beckett", NULL, 0));

  librdf_model_add_statement(model, statement);
  librdf_free_statement(statement);

  /* make it illegal */
  statement=librdf_new_statement(world);
  librdf_statement_set_subject(statement, librdf_new_node_from_literal(world, (const unsigned char*)"Bad Subject", NULL, 0));
  librdf_statement_set_predicate(statement, librdf_new_node_from_uri_string(world, (const unsigned char*)"http://example.org/pred"));
  librdf_statement_set_object(statement, librdf_new_node_from_literal(world, (const unsigned char*)"Good Object", NULL, 0));

  if(!librdf_model_add_statement(model, statement)) {
    fprintf(stderr, "%s: librdf_model_add_statement unexpectedly succeeded adding an illegal triple\n", program);
    return(1);
  }
  librdf_free_statement(statement);

  fprintf(stderr, "%s: Printing model\n", program);
  librdf_model_print(model, stderr);
  
  parser=librdf_new_parser(world, parser_name, NULL, NULL);
  if(!parser) {
    fprintf(stderr, "%s: Failed to create new parser type %s\n", program,
            parser_name);
    exit(1);
  }

  for (i=0; i<URI_STRING_COUNT; i++) {
    uris[i]=librdf_new_uri(world, file_uri_strings[i]);
    nodes[i]=librdf_new_node_from_uri_string(world, file_uri_strings[i]);

    fprintf(stderr, "%s: Adding content from %s into statement context\n", program,
            librdf_uri_as_string(uris[i]));
    if(!(stream=librdf_parser_parse_string_as_stream(parser, 
                                                     file_content[i], uris[i]))) {
      fprintf(stderr, "%s: Failed to parse RDF from %s as stream\n", program,
              librdf_uri_as_string(uris[i]));
      exit(1);
    }
    librdf_model_context_add_statements(model, nodes[i], stream);
    librdf_free_stream(stream);

    fprintf(stderr, "%s: Printing model\n", program);
    librdf_model_print(model, stderr);
  }


  fprintf(stderr, "%s: Freeing Parser\n", program);
  librdf_free_parser(parser);


  /* sync - probably a NOP */
  librdf_model_sync(model);


  /* sources */
  n1=librdf_new_node_from_uri_string(world, (const unsigned char*)"http://purl.org/dc/elements/1.1/creator");
  n2=librdf_new_node_from_literal(world, (const unsigned char*)"Dave Beckett", NULL, 0);

  fprintf(stderr, "%s: Looking for sources of arc=", program);
  librdf_node_print(n1, stderr);
  fputs(" target=", stderr);
  librdf_node_print(n2, stderr);
  fputs("\n", stderr);

  iterator=librdf_model_get_sources(model, n1, n2);
  if(!iterator) {
    fprintf(stderr, "%s: librdf_model_get_sources failed\n", program);
    status=1;
  }

  expected_count=3;
  for(count=0; !librdf_iterator_end(iterator); librdf_iterator_next(iterator), count++) {
    librdf_node* n=(librdf_node*)librdf_iterator_get_object(iterator);
    fputs("  ", stderr);
    librdf_node_print(n, stderr);
    fputs("\n", stderr);
  }
  librdf_free_iterator(iterator);
  if(count != expected_count) {
    fprintf(stderr, "%s: librdf_model_get_sources returned %d nodes, expected %d\n", program, count, expected_count);
    status=1;
  }
  librdf_free_node(n1);
  librdf_free_node(n2);
  

  /* targets */
  n1=librdf_new_node_from_uri_string(world, (const unsigned char*)"http://purl.org/net/dajobe/");
  n2=librdf_new_node_from_uri_string(world, (const unsigned char*)"http://purl.org/dc/elements/1.1/description");

  fprintf(stderr, "%s: Looking for targets of source=", program);
  librdf_node_print(n1, stderr);
  fputs(" arc=", stderr);
  librdf_node_print(n2, stderr);
  fputs("\n", stderr);

  iterator=librdf_model_get_targets(model, n1, n2);
  if(!iterator) {
    fprintf(stderr, "%s: librdf_model_get_targets failed\n", program);
    status=1;
  }

  expected_count=2;
  for(count=0; !librdf_iterator_end(iterator); librdf_iterator_next(iterator), count++) {
    librdf_node* n=(librdf_node*)librdf_iterator_get_object(iterator);
    fputs("  ", stderr);
    librdf_node_print(n, stderr);
    fputs("\n", stderr);
  }
  librdf_free_iterator(iterator);
  if(count != expected_count) {
    fprintf(stderr, "%s: librdf_model_get_targets returned %d nodes, expected %d\n", program, count, expected_count);
    status=1;
  }
  librdf_free_node(n1);
  librdf_free_node(n2);
  

  /* arcs */
  n1=librdf_new_node_from_uri_string(world, (const unsigned char*)"http://purl.org/net/dajobe/");
  n2=librdf_new_node_from_literal(world, (const unsigned char*)"Dave Beckett", NULL, 0);

  fprintf(stderr, "%s: Looking for arcs of source=", program);
  librdf_node_print(n1, stderr);
  fputs(" target=", stderr);
  librdf_node_print(n2, stderr);
  fputs("\n", stderr);

  iterator=librdf_model_get_arcs(model, n1, n2);
  if(!iterator) {
    fprintf(stderr, "%s: librdf_model_get_arcs failed\n", program);
    status=1;
  }

  expected_count=2;
  for(count=0; !librdf_iterator_end(iterator); librdf_iterator_next(iterator), count++) {
    librdf_node* n=(librdf_node*)librdf_iterator_get_object(iterator);
    fputs("  ", stderr);
    librdf_node_print(n, stderr);
    fputs("\n", stderr);
  }
  librdf_free_iterator(iterator);
  if(count != expected_count) {
    fprintf(stderr, "%s: librdf_model_get_arcs returned %d nodes, expected %d\n", program, count, expected_count);
    status=1;
  }
  librdf_free_node(n1);
  librdf_free_node(n2);


  fprintf(stderr, "%s: Listing contexts\n", program);
  iterator=librdf_model_get_contexts(model);
  if(!iterator) {
    fprintf(stderr, "%s: librdf_model_get_contexts failed (optional method)\n", program);
  } else {
    expected_count=2;
    for(count=0; !librdf_iterator_end(iterator); librdf_iterator_next(iterator), count++) {
      librdf_node* n=(librdf_node*)librdf_iterator_get_object(iterator);
      fputs("  ", stderr);
      librdf_node_print(n, stderr);
      fputs("\n", stderr);
    }
    librdf_free_iterator(iterator);
    if(count != expected_count) {
      fprintf(stderr, "%s: librdf_model_get_contexts returned %d context nodes, expected %d\n", program, count, expected_count);
      status=1;
    }
  }

#define TEST_CONTEXT_URI_INDEX 0

  if(librdf_model_contains_context(model, nodes[TEST_CONTEXT_URI_INDEX])) {
    fprintf(stderr, "%s: Model contains context %s\n", program, 
            librdf_uri_as_string(uris[TEST_CONTEXT_URI_INDEX]));
  } else {
    fprintf(stderr, "%s: librdf_model_contains_contexts failed to find context URI %s\n", 
            program, librdf_uri_as_string(uris[TEST_CONTEXT_URI_INDEX]));
    status=1;
  }
  

  for (i=0; i<URI_STRING_COUNT; i++) {
    fprintf(stderr, "%s: Removing statement context %s\n", program, 
            librdf_uri_as_string(uris[i]));
    librdf_model_context_remove_statements(model, nodes[i]);

    fprintf(stderr, "%s: Printing model\n", program);
    librdf_model_print(model, stderr);
  }

  fprintf(stderr, "%s: Serializing model\n", program);
  base_uri=librdf_new_uri(world, (const unsigned char*)"http://example.org/base#");
  string=librdf_model_to_counted_string(model, base_uri,
                                        "rdfxml", 
                                        NULL, NULL,
                                        &string_length);
  if(string_length != EXPECTED_BAD_STRING_LENGTH) {
    fprintf(stderr, "%s: Serialising to RDF/XML returned string size %d, expected %d\n", program,
            (int)string_length, EXPECTED_BAD_STRING_LENGTH);
    return 1;
  }
  librdf_free_uri(base_uri);
  free(string);
  fprintf(stderr, "%s: Serialized OK\n", program);


#define TEST_SIMILAR_COUNT 9

  /* add some similar statements */
  fprintf(stderr, "%s: Adding %d similar statements\n", program, TEST_SIMILAR_COUNT);
  for(i=0; i < TEST_SIMILAR_COUNT; i++) {
    char literal[6];
    strncpy(literal, "DaveX", 6);
    literal[4]='0'+i;
    statement=librdf_new_statement(world);
    librdf_statement_set_subject(statement, librdf_new_node_from_uri_string(world, (const unsigned char*)"http://example.org/"));
    librdf_statement_set_predicate(statement, librdf_new_node_from_uri_string(world, (const unsigned char*)"http://purl.org/dc/elements/1.1/creator"));
    librdf_statement_set_object(statement, librdf_new_node_from_literal(world, (const unsigned char*)literal, NULL, 0));
    
    librdf_model_add_statement(model, statement);
    librdf_free_statement(statement);
  }

  /* targets */
  fprintf(stderr, "%s: iterating similar statements\n", program);
  n1=librdf_new_node_from_uri_string(world, (const unsigned char*)"http://example.org/");
  n2=librdf_new_node_from_uri_string(world, (const unsigned char*)"http://purl.org/dc/elements/1.1/creator");
  iterator=librdf_model_get_targets(model, n1, n2);
  if(!iterator) {
    fprintf(stderr, "%s: librdf_model_get_targets failed\n", program);
    status=1;
  }

  fprintf(stderr, "%s: iterating similar statements again\n", program);
  second_iterator=librdf_model_get_targets(model, n1, n2);

  /* Deleting 2 statements but only 1 before counting */
  expected_count= TEST_SIMILAR_COUNT - 1;
  for(count=0;
      !librdf_iterator_end(iterator); 
      librdf_iterator_next(iterator), librdf_iterator_next(second_iterator), count++) {
    librdf_node* n;
    char literal[6];

    if(count == 2 ) {
      strncpy(literal, "DaveX", 6);
      literal[4]='0'+count;
      statement=librdf_new_statement(world);
      librdf_statement_set_subject(statement, librdf_new_node_from_uri_string(world, (const unsigned char*)"http://example.org/"));
      librdf_statement_set_predicate(statement, librdf_new_node_from_uri_string(world, (const unsigned char*)"http://purl.org/dc/elements/1.1/creator"));
      librdf_statement_set_object(statement, librdf_new_node_from_literal(world, (const unsigned char*)literal, NULL, 0));

      fprintf(stderr, "%s: Removing statement with literal '%s'\n",
              program, literal);
      
      librdf_model_remove_statement(model, statement);
      librdf_free_statement(statement);
      remove_count++;
    }
  
    n=(librdf_node*)librdf_iterator_get_object(iterator);
    fputs("  ", stderr);
    librdf_node_print(n, stderr);
    fputs("\n", stderr);

    if(count == 5 ) {
      strncpy(literal, "DaveX", 6);
      literal[4]='0'+count+remove_count;
      statement=librdf_new_statement(world);
      librdf_statement_set_subject(statement, librdf_new_node_from_uri_string(world, (const unsigned char*)"http://example.org/"));
      librdf_statement_set_predicate(statement, librdf_new_node_from_uri_string(world, (const unsigned char*)"http://purl.org/dc/elements/1.1/creator"));
      librdf_statement_set_object(statement, librdf_new_node_from_literal(world, (const unsigned char*)literal, NULL, 0));

      fprintf(stderr, "%s: Removing statement with literal '%s'\n",
              program, literal);
      
      librdf_model_remove_statement(model, statement);
      librdf_free_statement(statement);
      remove_count++;
    }
  
  }
  librdf_free_iterator(iterator);
  if(count != expected_count) {
    fprintf(stderr, "%s: librdf_model_get_targets returned %d nodes, expected %d\n", program, count, expected_count);
    status=1;
  }
  librdf_free_node(n1);
  librdf_free_node(n2);
  

  librdf_free_iterator(second_iterator);

  fprintf(stderr, "%s: Freeing URIs and Nodes\n", program);
  for (i=0; i<URI_STRING_COUNT; i++) {
    librdf_free_uri(uris[i]);
    librdf_free_node(nodes[i]);
  }
  
  fprintf(stderr, "%s: Freeing model\n", program);
  librdf_free_model(model);

  fprintf(stderr, "%s: Freeing storage\n", program);
  librdf_free_storage(storage);

  librdf_free_world(world);
  
  return status;
}

int test_model_cloning(char const *program, librdf_world *world) {
  librdf_storage *storage;
  librdf_model *model1;
  librdf_model *model2;
  librdf_model *model3;
  const char* storage_type;
  const char* storage_name;
  const char* storage_options;

  fprintf(stderr, "%s: Testing model cloning\n", program);

  /* Get storage configuration */
  storage_type=getenv("REDLAND_TEST_CLONING_STORAGE_TYPE");
  storage_name=getenv("REDLAND_TEST_CLONING_STORAGE_NAME");
  storage_options=getenv("REDLAND_TEST_CLONING_STORAGE_OPTIONS");
  if(!(storage_type && storage_name && storage_options)) {
    /* default is to test bdb disk hashes for cloning */
    storage_type="hashes";
    storage_name="test";
    storage_options="hash-type='bdb',dir='.',write='yes',new='yes',contexts='yes'";
  }

  fprintf(stderr, "%s: Creating new %s storage\n", program, storage_type);
  storage=librdf_new_storage(world, storage_type, storage_name, storage_options);
  if(!storage) {
    fprintf(stderr, "%s: Failed to create new %s storage name %s with options %s\n", program, storage_type, storage_name, storage_options);
    return 1;
  }
  
  fprintf(stderr, "%s: Creating new model for cloning\n", program);
  model1=librdf_new_model(world, storage, NULL);
  if(!model1) {
    fprintf(stderr, "%s: Failed to create new model\n", program);
    /* ok to leak memory */    
    return 1;
  }
  
  fprintf(stderr, "%s: Cloning model\n", program);
  model2=librdf_new_model_from_model(model1);
  if(!model2) {
    fprintf(stderr, "%s: Failed to clone model\n", program);
    /* ok to leak memory */    
    return 1;
  }

  /* Free original model now so we can test whether the clone is self-sufficient */
  fprintf(stderr, "%s: Freeing original model\n", program);
  librdf_free_model(model1);

  fprintf(stderr, "%s: Cloning cloned model\n", program);
  model3=librdf_new_model_from_model(model2);
  if(!model3) {
    fprintf(stderr, "%s: Failed to clone cloned model\n", program);
    /* ok to leak memory */    
    return 1;
  }

  fprintf(stderr, "%s: Freeing cloned models\n", program);  
  librdf_free_model(model3);
  librdf_free_model(model2);
  
  fprintf(stderr, "%s: Freeing %s storage\n", program, storage_type);  
  librdf_free_storage(storage);
  
  return 0;
}

#endif

