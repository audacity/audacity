/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_storage_file.c - RDF Storage in a file, using an in-memory store
 *
 * Copyright (C) 2004-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2004-2004, University of Bristol, UK http://www.bristol.ac.uk/
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
#include <stdlib.h> /* for abort() as used in errors */
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#include <sys/types.h>

#include <redland.h>


typedef struct
{
  librdf_model* model;
  librdf_storage* storage;
  int changed;

  /* 'uri' factory only */
  librdf_uri* uri;
  /* 'name' factory only */
  size_t name_len;  
  char *name;
} librdf_storage_file_context;


/* prototypes for local functions */
static int librdf_storage_file_init(librdf_storage* storage, const char *name, librdf_hash* options);
static int librdf_storage_file_open(librdf_storage* storage, librdf_model* model);
static int librdf_storage_file_close(librdf_storage* storage);
static int librdf_storage_file_size(librdf_storage* storage);
static int librdf_storage_file_add_statement(librdf_storage* storage, librdf_statement* statement);
static int librdf_storage_file_add_statements(librdf_storage* storage, librdf_stream* statement_stream);
static int librdf_storage_file_remove_statement(librdf_storage* storage, librdf_statement* statement);
static int librdf_storage_file_contains_statement(librdf_storage* storage, librdf_statement* statement);
static librdf_stream* librdf_storage_file_serialise(librdf_storage* storage);
static librdf_stream* librdf_storage_file_find_statements(librdf_storage* storage, librdf_statement* statement);

static int librdf_storage_file_sync(librdf_storage *storage);

static void librdf_storage_file_register_factory(librdf_storage_factory *factory);



/* functions implementing storage api */
static int
librdf_storage_file_init(librdf_storage* storage, const char *name,
                         librdf_hash* options)
{
  librdf_storage_file_context *context=(librdf_storage_file_context*)storage->context;
  char *name_copy;
  char *contexts;
  int rc=1;

  int is_uri=!strcmp(storage->factory->name, "uri");

  /* Cannot save contexts in a file; pass everything else on */
  contexts=librdf_hash_get_del(options, "contexts");
  if(contexts)
    LIBRDF_FREE(cstring, contexts);
  
  if(is_uri)
    context->uri=librdf_new_uri(storage->world, (const unsigned char*)name);
  else {
    context->name_len=strlen(name);
    name_copy=(char*)LIBRDF_MALLOC(cstring, context->name_len+1);
    if(!name_copy)
      goto done;
    strcpy(name_copy,name);
    context->name=name_copy;
    context->uri=librdf_new_uri_from_filename(storage->world, context->name);
  }
  
  context->storage=librdf_new_storage_with_options(storage->world, 
                                                   NULL, NULL, 
                                                   options);
  if(!context->storage)
    goto done;
  
  context->model=librdf_new_model(storage->world, context->storage, NULL);
  if(!context->model)
    goto done;

  if(is_uri || !access((const char*)context->name, F_OK)) {
    librdf_parser* parser;

    parser=librdf_new_parser(storage->world, "rdfxml", NULL, NULL);
    librdf_parser_parse_into_model(parser, context->uri, NULL, context->model);
    librdf_free_parser(parser);
  }

  context->changed=0;

  rc=0;

  done:

  /* no more options, might as well free them now */
  if(options)
    librdf_free_hash(options);

  return rc;
}


static void
librdf_storage_file_terminate(librdf_storage* storage)
{
  librdf_storage_file_context *context=(librdf_storage_file_context*)storage->context;

  librdf_storage_file_sync(storage);

  if(context->name)
    LIBRDF_FREE(cstring, context->name);

  if(context->uri)
    librdf_free_uri(context->uri);

  if(context->model)
     librdf_free_model(context->model);

  if(context->storage)
     librdf_free_storage(context->storage);
}


static int
librdf_storage_file_open(librdf_storage* storage, librdf_model* model)
{
  return 0;
}


static int
librdf_storage_file_close(librdf_storage* storage)
{
  return librdf_storage_file_sync(storage);
}


static int
librdf_storage_file_size(librdf_storage* storage)
{
  librdf_storage_file_context* context=(librdf_storage_file_context*)storage->context;
  return librdf_model_size(context->model);
}


static int
librdf_storage_file_add_statement(librdf_storage* storage, librdf_statement* statement)
{
  librdf_storage_file_context* context=(librdf_storage_file_context*)storage->context;
  context->changed=1;
  return librdf_model_add_statement(context->model, statement);
}


static int
librdf_storage_file_add_statements(librdf_storage* storage,
                                   librdf_stream* statement_stream)
{
  librdf_storage_file_context* context=(librdf_storage_file_context*)storage->context;
  context->changed=1;
  return librdf_model_add_statements(context->model, statement_stream);
}


static int
librdf_storage_file_remove_statement(librdf_storage* storage, librdf_statement* statement)
{
  librdf_storage_file_context* context=(librdf_storage_file_context*)storage->context;
  context->changed=1;
  return librdf_model_remove_statement(context->model, statement);
}


static int
librdf_storage_file_contains_statement(librdf_storage* storage, librdf_statement* statement)
{
  librdf_storage_file_context* context=(librdf_storage_file_context*)storage->context;
  return librdf_model_contains_statement(context->model, statement);
}


static librdf_stream*
librdf_storage_file_serialise(librdf_storage* storage)
{
  librdf_storage_file_context* context=(librdf_storage_file_context*)storage->context;
  return librdf_model_as_stream(context->model);
}


static librdf_stream*
librdf_storage_file_find_statements(librdf_storage* storage, librdf_statement* statement)
{
  librdf_storage_file_context* context=(librdf_storage_file_context*)storage->context;
  return librdf_model_find_statements(context->model, statement);
}


static int
librdf_storage_file_sync(librdf_storage *storage)
{
  librdf_storage_file_context* context=(librdf_storage_file_context*)storage->context;
  char *backup_name;
  char *new_name;
  librdf_serializer* serializer;
  FILE *fh;
  int rc=0;

  if(!context->changed)
    return 0;

  if(!context->name) {
    /* FIXME - URI cannot be written */
    context->changed=0;
    return 0;
  }
  
  backup_name=NULL;

  if(!access((const char*)context->name, F_OK)) {
    /* name"~\0" */
    backup_name=(char*)LIBRDF_MALLOC(cstring, context->name_len+2);
    if(!backup_name)
      return 1;
    strcpy(backup_name, (const char*)context->name);
    backup_name[context->name_len]='~';
    backup_name[context->name_len+1]='\0';

    if(rename(context->name, backup_name) < 0) {
      librdf_log(storage->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
                 "rename of '%s' to '%s' failed - %s",
                 context->name, backup_name, strerror(errno));
      LIBRDF_FREE(cstring, backup_name);
      return 1;
    }
  }
  
  /* name".new\0" */
  new_name=(char*)LIBRDF_MALLOC(cstring, context->name_len+5);
  if(!new_name)
    return 1;
  strcpy(new_name, (const char*)context->name);
  strcpy(new_name+context->name_len, ".new");

  serializer=librdf_new_serializer(storage->world, "rdfxml", NULL, NULL);
  if(!serializer) {
    LIBRDF_FREE(cstring, new_name);
    if(backup_name)
      LIBRDF_FREE(cstring, backup_name);
    return 1;
  }
  
  fh=fopen(new_name, "w+");
  if(!fh) {
    librdf_log(storage->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
               "failed to open file '%s' for writing - %s",
               new_name, strerror(errno));
    rc=1;
  } else {
    librdf_serializer_serialize_model_to_file_handle(serializer, fh,
                                                     context->uri,
                                                     context->model);
    fclose(fh);
  }
  librdf_free_serializer(serializer);

  if(fh && rename(new_name, context->name) < 0) {
    librdf_log(storage->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
               "rename of '%s' to '%s' failed - %s (%d)",
               new_name, context->name, strerror(errno), errno);
    fh=NULL;
    rc=1;
  }

  LIBRDF_FREE(cstring, new_name);
  
  /* restore backup on failure (fh=NULL) */
  if(!fh && backup_name && rename(backup_name, context->name) < 0) {
    librdf_log(storage->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
               "rename of '%s' to '%s' failed - %s",
               backup_name, context->name, strerror(errno));
    rc=1;
  }

  if(backup_name)
    LIBRDF_FREE(cstring, backup_name);

  context->changed=0;

  return rc;
}


static librdf_node*
librdf_storage_file_get_feature(librdf_storage* storage, librdf_uri* feature)
{
  return NULL;
}


/* local function to register list storage functions */

static void
librdf_storage_file_register_factory(librdf_storage_factory *factory) 
{
  factory->context_length     = sizeof(librdf_storage_file_context);
  
  factory->init               = librdf_storage_file_init;
  factory->terminate          = librdf_storage_file_terminate;
  factory->open               = librdf_storage_file_open;
  factory->close              = librdf_storage_file_close;
  factory->size               = librdf_storage_file_size;
  factory->add_statement      = librdf_storage_file_add_statement;
  factory->add_statements     = librdf_storage_file_add_statements;
  factory->remove_statement   = librdf_storage_file_remove_statement;
  factory->contains_statement = librdf_storage_file_contains_statement;
  factory->serialise          = librdf_storage_file_serialise;
  factory->find_statements    = librdf_storage_file_find_statements;
  factory->sync                     = librdf_storage_file_sync;
  factory->get_feature              = librdf_storage_file_get_feature;
}


/**
 * librdf_init_storage_file:
 * @world: world object
 * 
 * INTERNAL - initialise the storage_file module.
 **/
void
librdf_init_storage_file(librdf_world *world)
{
  librdf_storage_register_factory(world, "file", "Local file based store",
                                  &librdf_storage_file_register_factory);
  librdf_storage_register_factory(world, "uri",  "URI store (read-only)",
                                  &librdf_storage_file_register_factory);
}
