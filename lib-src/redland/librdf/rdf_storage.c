/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_storage.c - RDF Storage (Triple store) interface
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
#include <ctype.h>
#include <sys/types.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h> /* for abort() as used in errors */
#endif

#ifdef MODULAR_LIBRDF
#include <ltdl.h>
#endif

#include <redland.h>
#include <rdf_storage.h>


#ifndef STANDALONE

/* prototypes for functions implementing get_sources, arcs, targets
 * librdf_iterator via conversion from a librdf_stream of librdf_statement
 */
static int librdf_storage_stream_to_node_iterator_is_end(void* iterator);
static int librdf_storage_stream_to_node_iterator_next_method(void* iterator);
static void* librdf_storage_stream_to_node_iterator_get_method(void* iterator, int flags);
static void librdf_storage_stream_to_node_iterator_finished(void* iterator);

/* helper function for creating iterators for get sources, targets, arcs */
static librdf_iterator* librdf_storage_node_stream_to_node_create(librdf_storage* storage, librdf_node* node1, librdf_node *node2, librdf_statement_part want);

#ifdef MODULAR_LIBRDF
/* helper function for dynamically loading storage modules */
static lt_dlhandle
librdf_storage_load_module(librdf_world *world,
                           const char* lib_name,
                           const char* init_func_name);
#endif


/**
 * librdf_init_storage:
 * @world: redland world object
 *
 * INTERNAL - Initialise the storage module.
 * 
 * Initialises and registers all
 * compiled storage modules.  Must be called before using any of the storage
 * factory functions such as librdf_get_storage_factory()
 **/
void
librdf_init_storage(librdf_world *world)
{
#ifdef MODULAR_LIBRDF
  lt_dlhandle module = NULL;

  if (!world->storage_modules)
    world->storage_modules = raptor_new_sequence(
        (raptor_sequence_free_handler *)lt_dlclose, NULL);
#endif

  /* Always have storage memory - must always be the default storage */
  librdf_init_storage_list(world);

  /* Always have storage list, hashes, file implementations available */
  librdf_init_storage_hashes(world);

#ifdef STORAGE_TREES
  librdf_init_storage_trees(world);
#endif

#ifdef MODULAR_LIBRDF

  #ifdef STORAGE_FILE
    module = librdf_storage_load_module(world, "librdf_storage_file",
                                        "librdf_init_storage_file");
    if (module)
      raptor_sequence_push(world->storage_modules, module);
  #endif
  
  #ifdef STORAGE_MYSQL
    module = librdf_storage_load_module(world, "librdf_storage_mysql",
                                        "librdf_init_storage_mysql");
    if (module)
      raptor_sequence_push(world->storage_modules, module);
  #endif
  
  #ifdef STORAGE_POSTGRESQL
    module = librdf_storage_load_module(world, "librdf_storage_postgresql",
                                        "librdf_init_storage_postgresql");
    if (module)
      raptor_sequence_push(world->storage_modules, module);
  #endif
  
  #ifdef STORAGE_TSTORE
    module = librdf_storage_load_module(world, "librdf_storage_tstore",
                                        "librdf_init_storage_tstore");
    if (module)
      raptor_sequence_push(world->storage_modules, module);
  #endif
  
  #ifdef STORAGE_SQLITE
    module = librdf_storage_load_module(world, "librdf_storage_sqlite",
                                        "librdf_init_storage_sqlite");
    if (module)
      raptor_sequence_push(world->storage_modules, module);
  #endif

#else /* if !MODULAR_LIBRDF */
  
  #ifdef STORAGE_FILE
    librdf_init_storage_file(world);
  #endif
  #ifdef STORAGE_MYSQL
    librdf_init_storage_mysql(world);
  #endif
  #ifdef STORAGE_POSTGRESQL
    librdf_init_storage_postgresql(world);
  #endif
  #ifdef STORAGE_TSTORE
    librdf_init_storage_tstore(world);
  #endif
  #ifdef STORAGE_SQLITE
    librdf_init_storage_sqlite(world);
  #endif

#endif
}


/**
 * librdf_finish_storage:
 * @world: redland world object
 *
 * INTERNAL - Terminate the storage module.
 *
 **/
void
librdf_finish_storage(librdf_world *world) 
{
#ifdef MODULAR_LIBRDF
  if(world->storage_modules) {
    raptor_free_sequence(world->storage_modules);
    world->storage_modules=NULL;
  }
#endif
  
  if(world->storages) {
    raptor_free_sequence(world->storages);
    world->storages=NULL;
  }
}



/* helper functions */


static void
librdf_free_storage_factory(librdf_storage_factory* factory)
{
  if(factory->name)
    LIBRDF_FREE(librdf_storage_factory, factory->name);
  if(factory->label)
    LIBRDF_FREE(librdf_storage_factory, factory->label);
  LIBRDF_FREE(librdf_storage_factory, factory);
}


#ifdef MODULAR_LIBRDF
/**
 * librdf_storage_load_module:
 * @world: redland world object
 * @lib_name: base name of shared library file
 * @init_func_name: name of initialization function in library
 *
 * INTERNAL - Load and initialize/register a storage module
 **/
static lt_dlhandle
librdf_storage_load_module(librdf_world *world,
                           const char* lib_name,
                           const char* init_func_name)
{
  typedef void init_func_t(librdf_world* world);
  init_func_t* init;

  lt_dlhandle module = lt_dlopenext(lib_name);
  if (module) {
    init = (init_func_t*)lt_dlsym(module, init_func_name);
    if (init) {
      init(world);
    } else {
      LIBRDF_DEBUG2("Failed to initialize storage module %s\n", lib_name);
      lt_dlclose(module);
      module = NULL;
    }
  } else {
    LIBRDF_DEBUG2("Failed to load storage module %s\n", lib_name);
  }

  return module;
}
#endif


/* class methods */

/**
 * librdf_storage_register_factory:
 * @world: redland world object
 * @name: the storage factory name
 * @label: the storage factory label
 * @factory: pointer to function to call to register the factory
 *
 * Register a storage factory.
 **/
void
librdf_storage_register_factory(librdf_world* world,
                                const char *name, const char *label,
                                void (*factory) (librdf_storage_factory*)) 
{
  librdf_storage_factory *storage;
  int i;

  librdf_world_open(world);

#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
  LIBRDF_DEBUG2("Received registration for storage %s\n", name);
#endif

  if(!world->storages) {
    world->storages=raptor_new_sequence((raptor_sequence_free_handler *)librdf_free_storage_factory, NULL);
    if(!world->storages)
      goto oom;
  }

  for(i=0;
      (storage=(librdf_storage_factory*)raptor_sequence_get_at(world->storages, i));
      i++) {
    if(!strcmp(storage->name, name)) {
      librdf_log(world,
                 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
                 "storage %s already registered", storage->name);
      return;
    }
  }

  storage=(librdf_storage_factory*)LIBRDF_CALLOC(librdf_storage_factory, 1,
                                                 sizeof(librdf_storage_factory));
  if(!storage)
    goto oom;

  storage->name=(char*)LIBRDF_MALLOC(cstring, strlen(name)+1);
  if(!storage->name)
    goto oom_tidy;
  strcpy(storage->name, name);

  storage->label=(char*)LIBRDF_MALLOC(cstring, strlen(label)+1);
  if(!storage->label)
    goto oom_tidy;
  strcpy(storage->label, label);

  if(raptor_sequence_push(world->storages, storage))
    goto oom;

  /* Call the storage registration function on the new object */
  (*factory)(storage);

#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
  LIBRDF_DEBUG3("%s has context size %d\n", name, storage->context_length);
#endif

  return;

  oom_tidy:
  librdf_free_storage_factory(storage);
  oom:
  LIBRDF_FATAL1(world, LIBRDF_FROM_STORAGE, "Out of memory");
}


/**
 * librdf_get_storage_factory:
 * @world: redland world object
 * @name: the factory name or NULL for the default factory
 *
 * Get a storage factory by name.
 * 
 * Return value: the factory object or NULL if there is no such factory
 **/
librdf_storage_factory*
librdf_get_storage_factory(librdf_world* world, const char *name) 
{
  librdf_storage_factory *factory;

  librdf_world_open(world);

  /* return 1st storage if no particular one wanted - why? */
  if(!name) {
    factory=(librdf_storage_factory *)raptor_sequence_get_at(world->storages, 0);
    if(!factory) {
      LIBRDF_DEBUG1("No (default) storages registered\n");
      return NULL;
    }
  } else {
    int i;
    
    for(i=0;
        (factory=(librdf_storage_factory*)raptor_sequence_get_at(world->storages, i));
        i++) {
      if(!strcmp(factory->name, name))
        break;
    }
    /* else FACTORY name not found */
    if(!factory) {
      LIBRDF_DEBUG2("No storage with name %s found\n", name);
      return NULL;
    }
  }
        
  return factory;
}


/**
 * librdf_storage_enumerate:
 * @world: redland world object
 * @counter: index into the list of storages
 * @name: pointer to store the name of the storage (or NULL)
 * @label: pointer to store syntax readable label (or NULL)
 *
 * Get information on storages.
 * 
 * Return value: non 0 on failure of if counter is out of range
 **/
int
librdf_storage_enumerate(librdf_world* world,
                         const unsigned int counter,
                         const char **name, const char **label)
{
  librdf_storage_factory *factory;
  
  librdf_world_open(world);

  factory=(librdf_storage_factory*)raptor_sequence_get_at(world->storages,
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
 * librdf_new_storage:
 * @world: redland world object
 * @storage_name: the storage factory name
 * @name: an identifier for the storage
 * @options_string: options to initialise storage
 *
 * Constructor - create a new #librdf_storage object.
 *
 * The options are encoded as described in librdf_hash_from_string()
 * and can be NULL if none are required.
 *
 * Return value: a new #librdf_storage object or NULL on failure
 *
 */
librdf_storage*
librdf_new_storage(librdf_world *world, 
                   const char *storage_name, const char *name, 
                   const char *options_string)
{
  librdf_storage_factory* factory;
  librdf_hash* options_hash;
  
  librdf_world_open(world);

  factory=librdf_get_storage_factory(world, storage_name);
  if(!factory)
    return NULL;

  options_hash=librdf_new_hash(world, NULL);
  if(!options_hash)
    return NULL;

  if(librdf_hash_open(options_hash, NULL, 0, 1, 1, NULL)) {
    librdf_free_hash(options_hash);
    return NULL;
  }
  
  if(librdf_hash_from_string(options_hash, options_string)) {
    librdf_free_hash(options_hash);
    return NULL;
  }

  return librdf_new_storage_from_factory(world, factory, name, options_hash);
}


/**
 * librdf_new_storage_with_options:
 * @world: redland world object
 * @storage_name: the storage factory name
 * @name: an identifier for the storage
 * @options: #librdf_hash of options to use
 *
 * Constructor - create a new #librdf_storage object.
 *
 * The options can be NULL if none are required.
 *
 * Return value: a new #librdf_storage object or NULL on failure
 *
 */
librdf_storage*
librdf_new_storage_with_options(librdf_world *world, 
                                const char *storage_name, const char *name, 
                                librdf_hash *options)
{
  librdf_storage_factory* factory;
  librdf_hash* options_hash;
  
  librdf_world_open(world);

  factory=librdf_get_storage_factory(world, storage_name);
  if(!factory)
    return NULL;

  options_hash=librdf_new_hash_from_hash(options);
  if(!options_hash)
    return NULL;

  if(librdf_hash_open(options_hash, NULL, 0, 1, 1, NULL)) {
    librdf_free_hash(options_hash);
    return NULL;
  }
  
  return librdf_new_storage_from_factory(world, factory, name, options_hash);
}


/**
 * librdf_new_storage_from_storage - Copy constructor - create a new librdf_storage object from an existing one
 * @old_storage: the existing storage #librdf_storage to use
 *
 * Should create a new storage in the same context as the existing one
 * as appropriate for the storage.  For example, in a RDBMS storage
 * it would be a new database, or in on disk it would be a new
 * set of files.  This will mean automatically generating
 * a new identifier for the storage, maybe based on the existing
 * storage identifier.
 *
 * Return value: a new #librdf_storage object or NULL on failure
 *
 */
librdf_storage*
librdf_new_storage_from_storage(librdf_storage* old_storage) 
{
  librdf_storage* new_storage;

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(old_storage, librdf_storage, NULL);

  if(!old_storage->factory->clone) {
    librdf_log(old_storage->world,
               0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
               "clone method not implemented for storage factory %s", 
               old_storage->factory->name);
    return NULL;
  }

  new_storage=(librdf_storage*)LIBRDF_CALLOC(librdf_storage, 1,
                                             sizeof(librdf_storage));
  if(!new_storage)
    return NULL;
  
  /* set usage to 1 early to allow cleanup with librdf_free_storage() */
  new_storage->usage=1;

  new_storage->context=(char*)LIBRDF_CALLOC(librdf_storage_context, 1,
                                            old_storage->factory->context_length);
  if(!new_storage->context) {
    librdf_free_storage(new_storage);
    return NULL;
  }

  new_storage->world=old_storage->world;

  /* do this now so librdf_free_storage won't call new factory on
   * partially copied storage 
   */
  new_storage->factory=old_storage->factory;

  /* clone is assumed to do leave the new storage in the same state
   * after an init() method on an existing storage - i.e ready to
   * use but closed.
   */
  if(old_storage->factory->clone(new_storage, old_storage)) {
    librdf_free_storage(new_storage);
    return NULL;
  }
 
  return new_storage;
}


/**
 * librdf_new_storage_from_factory:
 * @world: redland world object
 * @factory: the factory to use to construct the storage
 * @name: name to use for storage
 * @options: #librdf_hash of options to initialise storage
 *
 * Constructor - create a new #librdf_storage object.
 *
 * If the options are present, they become owned by the storage
 * and should no longer be used.
 *
 * Return value: a new #librdf_storage object or NULL on failure
 *
 */
librdf_storage*
librdf_new_storage_from_factory(librdf_world *world,
                                librdf_storage_factory* factory,
                                const char *name,
                                librdf_hash* options)
{
  librdf_storage* storage;

  librdf_world_open(world);

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(factory, librdf_storage_factory, NULL);

  if(!factory) {
    librdf_free_hash(options);
    return NULL;
  }
  
  storage=(librdf_storage*)LIBRDF_CALLOC(librdf_storage, 1,
                                         sizeof(librdf_storage));
  if(!storage) {
    librdf_free_hash(options);
    return NULL;
  }
  
  storage->world=world;

  /* set usage to 1 early to allow cleanup with librdf_free_storage() */
  storage->usage=1; 
  
  storage->context=(char*)LIBRDF_CALLOC(librdf_storage_context, 1,
                                        factory->context_length);
  if(!storage->context) {
    librdf_free_hash(options);
    librdf_free_storage(storage);
    return NULL;
  }
  
  storage->factory=factory;

  if(factory->init(storage, name, options)) {
    librdf_free_storage(storage);
    return NULL;
  }
  
  return storage;
}


/**
 * librdf_free_storage:
 * @storage: #librdf_storage object
 *
 * Destructor - destroy a #librdf_storage object.
 **/
void
librdf_free_storage(librdf_storage* storage) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN(storage, librdf_storage);

  if(--storage->usage)
    return;

  if(storage->factory)
    storage->factory->terminate(storage);

  if(storage->context)
    LIBRDF_FREE(librdf_storage_context, storage->context);
  LIBRDF_FREE(librdf_storage, storage);
}


/**
 * librdf_storage_add_reference:
 * @storage: #librdf_storage object
 *
 * Increment storage reference count by one.
 * This function is intended to be internal to librdf storage modules.
 **/
void
librdf_storage_add_reference(librdf_storage *storage)
{
  storage->usage++;
}


/**
 * librdf_storage_remove_reference(libdf_storage *storage)
 * @storage: #librdf_storage object
 *
 * Decrement storage reference count by one.
 * Free the storage if reference count becomes zero.
 * This function is intended to be internal to librdf storage modules.
 **/
void
librdf_storage_remove_reference(librdf_storage *storage)
{
  librdf_free_storage(storage);
}


/* methods */

/**
 * librdf_storage_open:
 * @storage: #librdf_storage object
 * @model: model stored
 *
 * Start a model / storage association.
 *
 * This is ended with librdf_storage_close()
 * 
 * Return value: non 0 on failure
 **/
int
librdf_storage_open(librdf_storage* storage, librdf_model* model) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(storage, librdf_storage, 1);

  return storage->factory->open(storage, model);
}


/**
 * librdf_storage_close:
 * @storage: #librdf_storage object
 *
 * End a model / storage association.
 * 
 * Return value: non 0 on failure
 **/
int
librdf_storage_close(librdf_storage* storage)
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(storage, librdf_storage, 1);

  return storage->factory->close(storage);
}


/**
 * librdf_storage_size:
 * @storage: #librdf_storage object
 *
 * Get the number of statements stored.
 * 
 * Return value: The number of statements or < 0 if cannot be determined
 **/
int
librdf_storage_size(librdf_storage* storage) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(storage, librdf_storage, -1);

  return storage->factory->size(storage);
}


/**
 * librdf_storage_add_statement:
 * @storage: #librdf_storage object
 * @statement: #librdf_statement statement to add
 *
 * Add a statement to a storage.
 * 
 * The passed-in statement is copied when added to the store, not
 * shared with the store.  
 *
 * If the statement already exists in the store, it is not added
 * unless Redland contexts are being used.
 *
 * Enforces that the statement is legal for RDF - URI or blank subject,
 * URI predicate and URI or blank or literal object (i.e. anything).
 *
 * Return value: non 0 on failure, <0 on error, >0 if statement was illegal
 **/
int
librdf_storage_add_statement(librdf_storage* storage,
                             librdf_statement* statement) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(storage, librdf_storage, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(statement, librdf_statement, 1);

  /* subject can be a URI or blank node */
  if(!librdf_node_is_resource(statement->subject) &&
     !librdf_node_is_blank(statement->subject))
    return 1;
  
  /* predicate can only be a URI */
  if(!librdf_node_is_resource(statement->predicate))
     return 1;

  /* object can be any node - no check needed */

  if(storage->factory->add_statement)
    return storage->factory->add_statement(storage, statement);

  return -1;
}


/**
 * librdf_storage_add_statements:
 * @storage: #librdf_storage object
 * @statement_stream: #librdf_stream of statements
 *
 * Add a stream of statements to the storage.
 * 
 * If any of the statements already exists in the store, they are not
 * added unless Redland contexts are being used.
 *
 * Return value: non 0 on failure
 **/
int
librdf_storage_add_statements(librdf_storage* storage,
                              librdf_stream* statement_stream) 
{
  int status=0;
  
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(storage, librdf_storage, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(statement_stream, librdf_stream, 1);

  if(storage->factory->add_statements)
    return storage->factory->add_statements(storage, statement_stream);

  while(!librdf_stream_end(statement_stream)) {
    librdf_statement* statement=librdf_stream_get_object(statement_stream);

    if(statement) {
      status=librdf_storage_add_statement(storage, statement);
      if(status > 0)
        /* just skip illegal statements */
        status=0;
    }
    else
      status=1;

    if(status)
      break;

    librdf_stream_next(statement_stream);
  }
  
  return status;
}


/**
 * librdf_storage_remove_statement:
 * @storage: #librdf_storage object
 * @statement: #librdf_statement statement to remove
 *
 * Remove a statement from the storage.
 * 
 * Return value: non 0 on failure
 **/
int
librdf_storage_remove_statement(librdf_storage* storage, 
                                librdf_statement* statement) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(storage, librdf_storage, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(statement, librdf_statement, 1);

  if(storage->factory->remove_statement)
    return storage->factory->remove_statement(storage, statement);
  return 1;
}


/**
 * librdf_storage_contains_statement:
 * @storage: #librdf_storage object
 * @statement: #librdf_statement statement to check
 *
 * Test if a given statement is present in the storage.
 *
 * Return value: non 0 if the storage contains the statement (>0 if illegal statement)
 **/
int
librdf_storage_contains_statement(librdf_storage* storage,
                                  librdf_statement* statement) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(storage, librdf_storage, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(statement, librdf_statement, 1);

  /* subject can be a URI or blank node */
  if(!statement->subject ||
     (!librdf_node_is_resource(statement->subject) &&
      !librdf_node_is_blank(statement->subject)))
    return 1;
  
  /* predicate can only be a URI */
  if(!statement->predicate || !librdf_node_is_resource(statement->predicate))
     return 1;

  if(!statement->object)
    return 1;

  return storage->factory->contains_statement(storage, statement);
}


/**
 * librdf_storage_serialise:
 * @storage: #librdf_storage object
 *
 * Serialise the storage as a librdf_stream of statemetns.
 * 
 * Return value: #librdf_stream of statements or NULL on failure
 **/
librdf_stream*
librdf_storage_serialise(librdf_storage* storage) 
{
  return storage->factory->serialise(storage);
}


/**
 * librdf_storage_find_statements:
 * @storage: #librdf_storage object
 * @statement: #librdf_statement partial statement to find
 *
 * Search the storage for matching statements.
 * 
 * Searches the storage for a (partial) statement as described in
 * librdf_statement_match() and returns a #librdf_stream of
 * matching #librdf_statement objects.
 * 
 * Return value:  #librdf_stream of matching statements (may be empty) or NULL on failure
 **/
librdf_stream*
librdf_storage_find_statements(librdf_storage* storage,
                               librdf_statement* statement) 
{
  librdf_node *subject, *predicate, *object;
  librdf_iterator *iterator;
  
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(storage, librdf_storage, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(statement, librdf_statement, NULL);

  subject=librdf_statement_get_subject(statement);
  predicate=librdf_statement_get_predicate(statement);
  object=librdf_statement_get_object(statement);

  /* try to pick the most efficient storage back end */

  /* only subject/source field blank -> use find_sources */
  if(storage->factory->find_sources && !subject && predicate && object) {
    iterator=storage->factory->find_sources(storage, predicate, object);
    if(iterator)
      return librdf_new_stream_from_node_iterator(iterator, statement,
                                                  LIBRDF_STATEMENT_SUBJECT);
    return NULL;
  }
  
  /* only predicate/arc field blank -> use find_arcs */
  if(storage->factory->find_arcs && subject && !predicate && object) {
    iterator=storage->factory->find_arcs(storage, subject, object);
    if(iterator)
      return librdf_new_stream_from_node_iterator(iterator, statement,
                                                  LIBRDF_STATEMENT_PREDICATE);
    return NULL;
  }
  
  /* only object/target field blank -> use find_targets */
  if(storage->factory->find_targets && subject && predicate && !object) {
    iterator=storage->factory->find_targets(storage, subject, predicate);
    if(iterator)
      return librdf_new_stream_from_node_iterator(iterator, statement,
                                                  LIBRDF_STATEMENT_OBJECT);
    return NULL;
  }
  
  return storage->factory->find_statements(storage, statement);
}


typedef struct {
  librdf_storage *storage;
  librdf_stream *stream;
  librdf_statement *partial_statement;
  librdf_statement_part want;
  librdf_node *object_node;
  librdf_node *context_node;
} librdf_storage_stream_to_node_iterator_context;


static int
librdf_storage_stream_to_node_iterator_is_end(void* iterator)
{
  librdf_storage_stream_to_node_iterator_context* context=(librdf_storage_stream_to_node_iterator_context*)iterator;

  return librdf_stream_end(context->stream);
}


static int
librdf_storage_stream_to_node_iterator_next_method(void* iterator) 
{
  librdf_storage_stream_to_node_iterator_context* context=(librdf_storage_stream_to_node_iterator_context*)iterator;

  if(context->object_node) {
    librdf_free_node(context->object_node);
    context->object_node=NULL;
  }
  if(context->context_node) {
    librdf_free_node(context->context_node);
    context->context_node=NULL;
  }

  return librdf_stream_next(context->stream);
}


static void*
librdf_storage_stream_to_node_iterator_get_method(void* iterator, int flags) 
{
  librdf_storage_stream_to_node_iterator_context* context=(librdf_storage_stream_to_node_iterator_context*)iterator;
  librdf_node* node;
  librdf_statement* statement=librdf_stream_get_object(context->stream);

  if(!statement)
    return NULL;

  switch(flags) {
    case LIBRDF_ITERATOR_GET_METHOD_GET_OBJECT:

      if(!context->object_node) {
        switch(context->want) {
          case LIBRDF_STATEMENT_SUBJECT: /* SOURCES (subjects) */
            node=librdf_statement_get_subject(statement);
            break;
            
          case LIBRDF_STATEMENT_PREDICATE: /* ARCS (predicates) */
            node=librdf_statement_get_predicate(statement);
            break;
            
          case LIBRDF_STATEMENT_OBJECT: /* TARGETS (objects) */
            node=librdf_statement_get_object(statement);
            break;

          case LIBRDF_STATEMENT_ALL:
            default: /* error */
              librdf_log(statement->world,
                         0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
                         "Unknown statement part %d", context->want);
              node=NULL;
        }
        context->object_node=librdf_new_node_from_node(node);
      }
      node=context->object_node;
      break;
      
    case LIBRDF_ITERATOR_GET_METHOD_GET_CONTEXT:
      if(!context->context_node) {
        node=(librdf_node*)librdf_stream_get_context(context->stream);
        context->context_node=node ? librdf_new_node_from_node(node) : NULL;
      }
      node=context->context_node;
      break;
      
    default:
      librdf_log(statement->world,
                 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
                 "Unknown iterator method flag %d", flags);
      node=NULL;
  }
  
  return (void*)node;
}


static void
librdf_storage_stream_to_node_iterator_finished(void* iterator) 
{
  librdf_storage_stream_to_node_iterator_context* context=(librdf_storage_stream_to_node_iterator_context*)iterator;
  librdf_statement *partial_statement=context->partial_statement;

  if(partial_statement)
    librdf_free_statement(partial_statement);

  if(context->stream)
    librdf_free_stream(context->stream);

  if(context->storage)
    librdf_storage_remove_reference(context->storage);

  if(context->object_node)
    librdf_free_node(context->object_node);

  if(context->context_node)
    librdf_free_node(context->context_node);
  
  LIBRDF_FREE(librdf_storage_stream_to_node_iterator_context, context);
}


/*
 * librdf_storage_node_stream_to_node_create - Create a stream for get sources, targets or arcs methods using find_statements method
 * @storage: the storage object to use
 * @node1: the first node to encode in the key (or NULL if not needed)
 * @node2: the second node to encode in the key (or NULL if not needed)
 * @want: the field required from the statement
 *
 * node1 and node2 cannot both be NULL
 * 
 * Return value: a new #librdf_iterator or NULL on failure
 **/
static librdf_iterator*
librdf_storage_node_stream_to_node_create(librdf_storage* storage,
                                          librdf_node *node1,
                                          librdf_node *node2,
                                          librdf_statement_part want)
{
  librdf_statement *partial_statement;
  librdf_stream *stream;
  librdf_storage_stream_to_node_iterator_context* context;
  librdf_iterator* iterator;
  
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(storage, librdf_storage, NULL);
  LIBRDF_ASSERT_RETURN(node1 == NULL && node2 == NULL, "both node objects are NULL", NULL);

  partial_statement=librdf_new_statement(storage->world);
  if(!partial_statement)
    return NULL;
  
  context=(librdf_storage_stream_to_node_iterator_context*)LIBRDF_CALLOC(librdf_storage_stream_to_node_iterator_context, 1, sizeof(librdf_storage_stream_to_node_iterator_context));
  if(!context) {
    librdf_free_statement(partial_statement);
    return NULL;
  }


  if(node1)
    node1=librdf_new_node_from_node(node1);
  if(node2)
    node2=librdf_new_node_from_node(node2);
  
  switch(want) {
    case LIBRDF_STATEMENT_SUBJECT:
      librdf_statement_set_predicate(partial_statement, node1);
      librdf_statement_set_object(partial_statement, node2);
      break;
    case LIBRDF_STATEMENT_PREDICATE:
      librdf_statement_set_subject(partial_statement, node1);
      librdf_statement_set_object(partial_statement, node2);
      break;
    case LIBRDF_STATEMENT_OBJECT:
      librdf_statement_set_subject(partial_statement, node1);
      librdf_statement_set_predicate(partial_statement, node2);
      break;

    case LIBRDF_STATEMENT_ALL:
    default:
      librdf_free_node(node1);
      librdf_free_node(node2);
      librdf_free_statement(partial_statement);
      librdf_log(storage->world,
                 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
                 "Illegal statement part %d seen", want);
      return NULL;
  }
  
  stream=storage->factory->find_statements(storage, partial_statement);
  if(!stream) {
    librdf_storage_stream_to_node_iterator_finished(context);
    return librdf_new_empty_iterator(storage->world);
  }
  
  /* initialise context */
  context->partial_statement=partial_statement;
  context->stream=stream;
  context->want=want;

  context->storage=storage;
  librdf_storage_add_reference(context->storage);

  iterator=librdf_new_iterator(storage->world,
                               (void*)context,
                               librdf_storage_stream_to_node_iterator_is_end,
                               librdf_storage_stream_to_node_iterator_next_method,
                               librdf_storage_stream_to_node_iterator_get_method,
                               librdf_storage_stream_to_node_iterator_finished);
  if(!iterator)
    librdf_storage_stream_to_node_iterator_finished(context);
  return iterator;
}


/**
 * librdf_storage_get_sources:
 * @storage: #librdf_storage object
 * @arc: #librdf_node arc
 * @target: #librdf_node target
 *
 * Return the sources (subjects) of arc in an RDF graph given arc (predicate) and target (object).
 * 
 * Searches the storage for arcs matching the given arc and target
 * and returns a list of the source #librdf_node objects as an iterator
 * 
 * Return value:  #librdf_iterator of #librdf_node objects (may be empty) or NULL on failure
 **/
librdf_iterator*
librdf_storage_get_sources(librdf_storage *storage,
                           librdf_node *arc, librdf_node *target) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(storage, librdf_storage, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(arc, librdf_node, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(target, librdf_node, NULL);

  if (storage->factory->find_sources)
    return storage->factory->find_sources(storage, arc, target);

  return librdf_storage_node_stream_to_node_create(storage, arc, target,
                                                   LIBRDF_STATEMENT_SUBJECT);
}


/**
 * librdf_storage_get_arcs:
 * @storage: #librdf_storage object
 * @source: #librdf_node source
 * @target: #librdf_node target
 *
 * Return the arcs (predicates) of an arc in an RDF graph given source (subject) and target (object).
 * 
 * Searches the storage for arcs matching the given source and target
 * and returns a list of the arc #librdf_node objects as an iterator
 * 
 * Return value:  #librdf_iterator of #librdf_node objects (may be empty) or NULL on failure
 **/
librdf_iterator*
librdf_storage_get_arcs(librdf_storage *storage,
                        librdf_node *source, librdf_node *target) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(storage, librdf_storage, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(source, librdf_node, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(target, librdf_node, NULL);

  if (storage->factory->find_arcs)
    return storage->factory->find_arcs(storage, source, target);

  return librdf_storage_node_stream_to_node_create(storage, source, target,
                                                   LIBRDF_STATEMENT_PREDICATE);
}


/**
 * librdf_storage_get_targets:
 * @storage: #librdf_storage object
 * @source: #librdf_node source
 * @arc: #librdf_node arc
 *
 * Return the targets (objects) of an arc in an RDF graph given source (subject) and arc (predicate).
 * 
 * Searches the storage for targets matching the given source and arc
 * and returns a list of the source #librdf_node objects as an iterator
 * 
 * Return value:  #librdf_iterator of #librdf_node objects (may be empty) or NULL on failure
 **/
librdf_iterator*
librdf_storage_get_targets(librdf_storage *storage,
                           librdf_node *source, librdf_node *arc) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(storage, librdf_storage, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(source, librdf_node, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(arc, librdf_node, NULL);

  if (storage->factory->find_targets)
    return storage->factory->find_targets(storage, source, arc);

  return librdf_storage_node_stream_to_node_create(storage, source, arc,
                                                   LIBRDF_STATEMENT_OBJECT);
}


/**
 * librdf_storage_get_arcs_in:
 * @storage: #librdf_storage object
 * @node: #librdf_node resource node
 *
 * Return the properties pointing to the given resource.
 * 
 * Return value:  #librdf_iterator of #librdf_node objects (may be empty) or NULL on failure
 **/
librdf_iterator*
librdf_storage_get_arcs_in(librdf_storage *storage, librdf_node *node) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(storage, librdf_storage, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(node, librdf_node, NULL);

  if (storage->factory->get_arcs_in)
    return storage->factory->get_arcs_in(storage, node);

  return librdf_storage_node_stream_to_node_create(storage, NULL, node,
                                                   LIBRDF_STATEMENT_PREDICATE);
}


/**
 * librdf_storage_get_arcs_out:
 * @storage: #librdf_storage object
 * @node: #librdf_node resource node
 *
 * Return the properties pointing from the given resource.
 * 
 * Return value:  #librdf_iterator of #librdf_node objects (may be empty) or NULL on failure
 **/
librdf_iterator*
librdf_storage_get_arcs_out(librdf_storage *storage, librdf_node *node) 
{
  if (storage->factory->get_arcs_out)
    return storage->factory->get_arcs_out(storage, node);
  return librdf_storage_node_stream_to_node_create(storage, node, NULL,
                                                   LIBRDF_STATEMENT_PREDICATE);
}


/**
 * librdf_storage_has_arc_in:
 * @storage: #librdf_storage object
 * @node: #librdf_node resource node
 * @property: #librdf_node property node
 *
 * Check if a node has a given property pointing to it.
 * 
 * Return value: non 0 if arc property does point to the resource node
 **/
int
librdf_storage_has_arc_in(librdf_storage *storage, librdf_node *node,
                          librdf_node *property) 
{
  librdf_iterator *iterator;
  int status;
  
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(storage, librdf_storage, 0);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(node, librdf_node, 0);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(property, librdf_node, 0);

  if (storage->factory->has_arc_in)
    return storage->factory->has_arc_in(storage, node, property);
  
  iterator=librdf_storage_get_sources(storage, property, node);
  if(!iterator)
    return 0;

  /* a non-empty list of sources is success */
  status=!librdf_iterator_end(iterator);
  librdf_free_iterator(iterator);

  return status;
}


/**
 * librdf_storage_has_arc_out:
 * @storage: #librdf_storage object
 * @node: #librdf_node resource node
 * @property: #librdf_node property node
 *
 * Check if a node has a given property pointing from it.
 * 
 * Return value: non 0 if arc property does point from the resource node
 **/
int
librdf_storage_has_arc_out(librdf_storage *storage, librdf_node *node, 
                           librdf_node *property) 
{
  librdf_iterator *iterator;
  int status;
  
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(storage, librdf_storage, 0);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(node, librdf_node, 0);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(property, librdf_node, 0);

  if (storage->factory->has_arc_out)
    return storage->factory->has_arc_out(storage, node, property);
  
  iterator=librdf_storage_get_targets(storage, node, property);
  if(!iterator)
    return 0;

  /* a non-empty list of targets is success */
  status=!librdf_iterator_end(iterator);
  librdf_free_iterator(iterator);

  return status;
}



/**
 * librdf_storage_context_add_statement:
 * @storage: #librdf_storage object
 * @context: #librdf_node context node
 * @statement: #librdf_statement statement to add
 *
 * Add a statement to a storage in a context.
 * 
 * If @context is NULL, this is equivalent to librdf_storage_add_statement
 *
 * Return value: non 0 on failure
 **/
int
librdf_storage_context_add_statement(librdf_storage* storage,
                                     librdf_node* context,
                                     librdf_statement* statement) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(storage, librdf_storage, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(statement, librdf_statement, 1);

  if(!context)
    return librdf_storage_add_statement(storage, statement);

  if(storage->factory->context_add_statement)
    return storage->factory->context_add_statement(storage, context, statement);
  return 1;
}


/**
 * librdf_storage_context_add_statements:
 * @storage: #librdf_storage object
 * @context: #librdf_node context
 * @stream: #librdf_stream stream object
 *
 * Add statements to a storage with a context.
 * 
 * If @context is NULL, this is equivalent to librdf_storage_add_statements
 *
 * Return value: Non 0 on failure
 **/
int
librdf_storage_context_add_statements(librdf_storage* storage, 
                                      librdf_node* context,
                                      librdf_stream* stream) 
{
  int status=0;
  
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(storage, librdf_storage, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(stream, librdf_stream, 1);

  if(!context)
    return librdf_storage_add_statements(storage, stream);

  if(storage->factory->context_add_statements)
    return storage->factory->context_add_statements(storage, context, stream);

  if(!storage->factory->context_add_statement)
    return 1;
  
  if(!stream)
    return 1;

  while(!librdf_stream_end(stream)) {
    librdf_statement* statement=librdf_stream_get_object(stream);
    if(!statement)
      break;
    status=librdf_storage_context_add_statement(storage, context, statement);
    if(status)
      break;
    librdf_stream_next(stream);
  }

  return status;
}



/**
 * librdf_storage_context_remove_statement:
 * @storage: #librdf_storage object
 * @context: #librdf_node context node
 * @statement: #librdf_statement statement to remove
 *
 * Remove a statement from a storage in a context.
 * 
 * If @context is NULL, this is equivalent to librdf_storage_remove_statement
 *
 * Return value: non 0 on failure
 **/
int
librdf_storage_context_remove_statement(librdf_storage* storage, 
                                        librdf_node* context,
                                        librdf_statement* statement) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(storage, librdf_storage, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(context, librdf_statement, 1);

  if(!storage->factory->context_remove_statement)
    return 1;
  
  return storage->factory->context_remove_statement(storage, context, statement);
}


/**
 * librdf_storage_context_remove_statements:
 * @storage: #librdf_storage object
 * @context: #librdf_uri context
 *
 * Remove statements from a storage with the given context.
 * 
 * Return value: Non 0 on failure
 **/
int
librdf_storage_context_remove_statements(librdf_storage* storage,
                                         librdf_node* context) 
{
  librdf_stream *stream;

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(storage, librdf_storage, 1);

  if(storage->factory->context_remove_statements)
    return storage->factory->context_remove_statements(storage, context);
  
  if(!storage->factory->context_remove_statement)
    return 1;
  
  stream=librdf_storage_context_as_stream(storage, context);
  if(!stream)
    return 1;

  while(!librdf_stream_end(stream)) {
    librdf_statement *statement=librdf_stream_get_object(stream);
    if(!statement)
      break;
    librdf_storage_context_remove_statement(storage, context, statement);
    librdf_stream_next(stream);
  }
  librdf_free_stream(stream);  
  return 0;
}


/**
 * librdf_storage_context_as_stream:
 * @storage: #librdf_storage object
 * @context: #librdf_node context node
 *
 * List all statements in a storage context.
 * 
 * Return value: #librdf_stream of statements or NULL on failure or context is empty
 **/
librdf_stream*
librdf_storage_context_as_stream(librdf_storage* storage, librdf_node* context)
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(storage, librdf_storage, NULL);

  return storage->factory->context_serialise(storage, context);
}


/**
 * librdf_storage_context_serialise:
 * @storage: #librdf_storage object
 * @context: #librdf_node context node
 *
 * List all statements in a storage context (DEPRECATED).
 * 
 * DEPRECATED to reduce confusion with the librdf_serializer class.
 * Please use librdf_storage_context_as_stream.
 *
 * Return value: #librdf_stream of statements or NULL on failure or context is empty
 **/
librdf_stream*
librdf_storage_context_serialise(librdf_storage* storage,
                                 librdf_node* context)
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(storage, librdf_storage, NULL);

  return librdf_storage_context_as_stream(storage, context);
}


/**
 * librdf_storage_supports_query:
 * @storage: #librdf_storage object
 * @query: #librdf_query query object
 * 
 * Check if a storage system supports a query language.
 * 
 * Not implemented.
 *
 * Return value: non-0 if the query is supported.
 **/
int
librdf_storage_supports_query(librdf_storage* storage, librdf_query *query)
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(storage, librdf_storage, 0);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(query, librdf_query, 0);

  return 0;
}


/**
 * librdf_storage_query_execute:
 * @storage: #librdf_storage object
 * @query: #librdf_query query object
 * 
 * Run the given query against the storage.
 * 
 * Not implemented.
 *
 * Return value: #librdf_query_results or NULL on failure
 **/
librdf_query_results*
librdf_storage_query_execute(librdf_storage* storage, librdf_query *query) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(storage, librdf_storage, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(query, librdf_query, NULL);

  return NULL;
}


/**
 * librdf_storage_sync:
 * @storage: #librdf_storage object
 * 
 * Synchronise the storage to the storage implementation.
 * 
 * Return value: non-0 on failure
 **/
int
librdf_storage_sync(librdf_storage* storage) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(storage, librdf_storage, 1);

  if(storage->factory->sync)
    return storage->factory->sync(storage);
  return 0;
}


/**
 * librdf_storage_find_statements_in_context:
 * @storage: #librdf_storage object
 * @statement: #librdf_statement partial statement to find
 * @context_node: context #librdf_node (or NULL)
 *
 * Search the storage for matching statements in a given context.
 * 
 * Searches the storage for a (partial) statement as described in
 * librdf_statement_match() in the given context and returns a
 * #librdf_stream of matching #librdf_statement objects.  If
 * context is NULL, this is equivalent to librdf_storage_find_statements.
 * 
 * Return value: #librdf_stream of matching statements (may be empty) or NULL on failure
 **/
librdf_stream*
librdf_storage_find_statements_in_context(librdf_storage* storage, librdf_statement* statement, librdf_node* context_node) 
{
  librdf_stream *stream;

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(storage, librdf_storage, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(statement, librdf_statement, NULL);

  if(storage->factory->find_statements_in_context)
    return storage->factory->find_statements_in_context(storage, statement, context_node);

  statement=librdf_new_statement_from_statement(statement);
  if(!statement)
    return NULL;

  stream=librdf_storage_context_as_stream(storage, context_node);
  if(!stream) {
    librdf_free_statement(statement);
    return NULL;
  }

  librdf_stream_add_map(stream, 
                        &librdf_stream_statement_find_map,
                        (librdf_stream_map_free_context_handler)&librdf_free_statement, (void*)statement);

  return stream;
}


/**
 * librdf_storage_get_contexts:
 * @storage: #librdf_storage object
 *
 * Return the list of contexts in the store.
 * 
 * Returns an iterator of #librdf_node context nodes for each
 * context in the store.
 *
 * Return value: #librdf_iterator of context nodes or NULL on failure or if contexts are not supported
 **/
librdf_iterator*
librdf_storage_get_contexts(librdf_storage* storage) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(storage, librdf_storage, NULL);

  if(storage->factory->get_contexts)
    return storage->factory->get_contexts(storage);
  else
    return NULL;
}



/**
 * librdf_storage_get_feature:
 * @storage: #librdf_storage object
 * @feature: #librdf_uri feature property
 *
 * Get the value of a storage feature.
 * 
 * Return value: new #librdf_node feature value or NULL if no such feature
 * exists or the value is empty.
 **/
librdf_node*
librdf_storage_get_feature(librdf_storage* storage, librdf_uri* feature)
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(storage, librdf_storage, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(feature, librdf_uri, NULL);

  if(storage->factory->get_feature)
    return storage->factory->get_feature(storage, feature);
  return NULL;
}


/**
 * librdf_storage_set_feature:
 * @storage: #librdf_storage object
 * @feature: #librdf_uri feature property
 * @value: #librdf_node feature property value
 *
 * Set the value of a storage feature.
 * 
 * Return value: non 0 on failure (negative if no such feature)
 **/
int
librdf_storage_set_feature(librdf_storage* storage, librdf_uri* feature,
                           librdf_node* value)
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(storage, librdf_storage, -1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(feature, librdf_uri, -1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(value, librdf_node, -1);

  if(storage->factory->set_feature)
    return storage->factory->set_feature(storage, feature, value);
  return -1;
}


/**
 * librdf_storage_find_statements_with_options:
 * @storage: #librdf_storage object
 * @statement: #librdf_statement partial statement to find
 * @context_node: #librdf_node context node or NULL.
 * @options: #librdf_hash of matching options or NULL
 *
 * Search the storage for matching statements with match options.
 * 
 * Searches the storage for a (partial) statement as described in
 * librdf_statement_match() and returns a #librdf_stream of
 * matching #librdf_statement objects.
 * 
 * If options is given then the match is made according to
 * the given options.  If options is NULL, this is equivalent
 * to librdf_storage_find_statements_in_context.
 * 
 * Return value:  #librdf_stream of matching statements (may be empty) or NULL on failure
 **/
librdf_stream*
librdf_storage_find_statements_with_options(librdf_storage* storage,
                                            librdf_statement* statement,
                                            librdf_node* context_node,
                                            librdf_hash* options) 
{
  if(storage->factory->find_statements_with_options)
    return storage->factory->find_statements_with_options(storage, statement, context_node, options);
  else
    return librdf_storage_find_statements_in_context(storage, statement, context_node);
}



/**
 * librdf_storage_transaction_start:
 * @storage: the storage object
 * 
 * Start a transaction
 * 
 * Return value: non-0 on failure
 **/
int
librdf_storage_transaction_start(librdf_storage* storage) 
{
  if(storage->factory->transaction_start)
    return storage->factory->transaction_start(storage);
  else
    return 1;
}


/**
 * librdf_storage_transaction_start_with_handle:
 * @storage: the storage object
 * @handle: the transaction object
 * 
 * Start a transaction using an existing external transaction object.
 * 
 * Return value: non-0 on failure
 **/
int
librdf_storage_transaction_start_with_handle(librdf_storage* storage, void* handle)
{
  if(storage->factory->transaction_start_with_handle)
    return storage->factory->transaction_start_with_handle(storage, handle);
  else
    return 1;
}


/**
 * librdf_storage_transaction_commit:
 * @storage: the storage object
 * 
 * Commit a transaction.
 * 
 * Return value: non-0 on failure 
 **/
int
librdf_storage_transaction_commit(librdf_storage* storage) 
{
  if(storage->factory->transaction_commit)
    return storage->factory->transaction_commit(storage);
  else
    return 1;
}


/**
 * librdf_storage_transaction_rollback:
 * @storage: the storage object
 * 
 * Rollback a transaction.
 * 
 * Return value: non-0 on failure 
 **/
int
librdf_storage_transaction_rollback(librdf_storage* storage) 
{
  if(storage->factory->transaction_rollback)
    return storage->factory->transaction_rollback(storage);
  else
    return 1;
}


/**
 * librdf_storage_transaction_get_handle:
 * @storage: the storage object
 * 
 * Get the current transaction handle.
 * 
 * Return value: non-0 on failure 
 **/
void*
librdf_storage_transaction_get_handle(librdf_storage* storage) 
{
  if(storage->factory->transaction_get_handle)
    return storage->factory->transaction_get_handle(storage);
  else
    return NULL;
}


#endif


/* TEST CODE */


#ifdef STANDALONE

/* one more prototype */
int main(int argc, char *argv[]);


int
main(int argc, char *argv[]) 
{
  librdf_storage* storage;
  const char *program=librdf_basename((const char*)argv[0]);
  librdf_world *world;
  
  /* triples of arguments to librdf_new_storage */
  const char* const storages[] = {
	"memory", NULL, "contexts='yes'",
	"hashes", "test", "hash-type='bdb',dir='.',write='yes',new='yes',contexts='yes'",
    #ifdef STORAGE_TREES
	    "trees", "test", "contexts='yes'",
    #endif
    #ifdef STORAGE_FILE
      "file", "file://../redland.rdf", NULL,
	    "uri", "http://librdf.org/redland.rdf", NULL,
    #endif
    #ifdef STORAGE_MYSQL
      "mysql", "test", "host='localhost',database='test'",
    #endif
    #ifdef STORAGE_POSTGRESQL
      "postgresql", "test", "host='localhost',database='test'",
    #endif
    #ifdef STORAGE_TSTORE
      "tstore", "test", "host='localhost',database='test'",
    #endif
    #ifdef STORAGE_SQLITE
      "sqlite", "test", "new='yes'",
    #endif
	NULL, NULL, NULL
  };

  int test = 0;
  int ret  = 0;
  
  world=librdf_new_world();
  librdf_world_open(world);

  for ( ; storages[test] != NULL; test += 3) {

    fprintf(stdout, "%s: Creating storage %s\n", program, storages[test]);
    storage=librdf_new_storage(world,
                               storages[test], /* type */
                               storages[test+1], /* name */
                               storages[test+2]); /* options */
    if(!storage) {
      fprintf(stderr, "%s: WARNING: Failed to create new storage %s\n",
              program, storages[test]);
      continue;
    }


    fprintf(stdout, "%s: Opening storage\n", program);
    if(librdf_storage_open(storage, NULL)) {
      fprintf(stderr, "%s: Failed to open storage type %s\n",
              program, storages[test]);
      ret++;
      continue;
    }


    /* Can do nothing here since need model and storage working */

    fprintf(stdout, "%s: Closing storage\n", program);
    librdf_storage_close(storage);

    fprintf(stdout, "%s: Freeing storage\n", program);
    librdf_free_storage(storage);

  }
  

  librdf_free_world(world);
  
  return ret;
}

#endif
