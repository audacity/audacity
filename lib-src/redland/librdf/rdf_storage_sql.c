/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_storage.c - RDF Storage SQL support functions
 *
 * Copyright (C) 2008, David Beckett http://www.dajobe.org/
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
/* for access() and R_OK */
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif


#include <redland.h>


static void librdf_sql_config_store_triple(void *user_data,  const raptor_statement *triple);



static void
librdf_sql_config_store_triple(void *user_data,
                               const raptor_statement *triple) 
{
  librdf_sql_config* config=(librdf_sql_config*)user_data;
  int i;
  
  for(i=0; i < config->predicates_count; i++) {
    if(triple->predicate_type != RAPTOR_IDENTIFIER_TYPE_RESOURCE ||
       triple->object_type != RAPTOR_IDENTIFIER_TYPE_LITERAL)
      continue;
    
    if(!strcmp((const char*)librdf_uri_as_string((librdf_uri*)triple->predicate),
               config->predicate_uri_strings[i])) {
      config->values[i]=strdup((char*)triple->object);
#if LIBRDF_DEBUG > 1
      LIBRDF_DEBUG3("Set config value %d to '%s'\n", i, config->values[i]);
#endif
    }
  }
  
  return;
}


/**
 * librdf_new_sql_config:
 * @world: librdf_world
 * @storage_name: SQL storage name
 * @layout: SQL schema variant
 * @config_dir: directory for configuration files
 * @predicate_uri_strings: configuration predicate URIs to look for
 * 
 * Constructor - Make a new SQL configuration for a layout from a file
 *
 * Uses SQL storage name @storage_name and with database schema
 * @layout to give a configuration that will contain an array of
 * string values in the #librdf_sql_config field values array.
 * 
 * Return value: configuration or NULL on failure
 **/
librdf_sql_config*
librdf_new_sql_config(librdf_world* world,
                      const char* storage_name,
                      const char* layout,
                      const char* config_dir,
                      const char** predicate_uri_strings)
{
  raptor_parser* rdf_parser=NULL;
  unsigned char *uri_string=NULL;
  raptor_uri *base_uri;
  raptor_uri *uri;
  librdf_sql_config* config;
  size_t len;
  int i;
  
  librdf_world_open(world);

  config=(librdf_sql_config*)LIBRDF_MALLOC(librdf_sql_config,
                                           sizeof(librdf_sql_config));

  len=strlen(config_dir) + 1 + strlen(storage_name) + 4 + 1;
  if(layout)
    len+= strlen(layout) + 1;
  config->filename=(char*)LIBRDF_MALLOC(cstring, len);
  if(layout)
    sprintf(config->filename, "%s/%s-%s.ttl", config_dir, storage_name, layout);
  else
    sprintf(config->filename, "%s/%s.ttl", config_dir, storage_name);

  config->predicate_uri_strings=predicate_uri_strings;
  for(i=0; config->predicate_uri_strings[i]; i++)
    ;
  config->predicates_count=i;
  config->values=(char**)LIBRDF_CALLOC(cstring, sizeof(char*), 
                                       config->predicates_count);
  
  LIBRDF_DEBUG4("Attempting to open %s layout %s storage config file %s\n", 
                storage_name, (layout ? layout: "(default)"), config->filename);
  
  if(access((const char*)config->filename, R_OK)) {
    librdf_log(world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
               "Failed to open configuration file %s for storage %s layout %s - %s",
               config->filename, storage_name, (layout ? layout: "(default)"),
               strerror(errno));
    librdf_free_sql_config(config);
    return NULL;
  }
  
  uri_string=raptor_uri_filename_to_uri_string(config->filename);
  uri=raptor_new_uri(uri_string);
  base_uri=raptor_uri_copy(uri);
  
  rdf_parser=raptor_new_parser("turtle");
  raptor_set_statement_handler(rdf_parser, config,
                               librdf_sql_config_store_triple);
  raptor_parse_file(rdf_parser, uri, base_uri);
  raptor_free_parser(rdf_parser);
  
  raptor_free_uri(base_uri);
  raptor_free_memory(uri_string);
  raptor_free_uri(uri);

  /* Check all values are given */
  for(i=0; i < config->predicates_count; i++) {
    if(!config->values[i]) {
      librdf_log(world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
                 "Configuration %s missing for storage %s",
                 config->predicate_uri_strings[i], storage_name);
      librdf_free_sql_config(config);
      return NULL;
    }
  }
  
  return config;
}


const char* librdf_storage_sql_dbconfig_predicates[DBCONFIG_CREATE_TABLE_LAST+2]={
  "http://schemas.librdf.org/2006/dbconfig#createTableStatements",
  "http://schemas.librdf.org/2006/dbconfig#createTableLiterals",
  "http://schemas.librdf.org/2006/dbconfig#createTableResources",
  "http://schemas.librdf.org/2006/dbconfig#createTableBnodes",
  "http://schemas.librdf.org/2006/dbconfig#createTableModels",
  NULL
};


/**
 * librdf_new_sql_config_for_storage:
 * @storage: SQL #librdf_storage to configure
 * @layout: layout variant
 * @dir: directory to look for configuration or NULL for default dir
 * 
 * Constructor - Create a new SQL storage configuration for a given database layout.
 * 
 * Return value: new configuration or NULL on failure
 **/
librdf_sql_config*
librdf_new_sql_config_for_storage(librdf_storage* storage, const char* layout,
                                  const char *dir)
{
  if(!dir)
    dir=PKGDATADIR;
  
  return librdf_new_sql_config(storage->world, storage->factory->name,
                               layout, dir,
                               librdf_storage_sql_dbconfig_predicates);
}


/**
 * librdf_free_sql_config:
 * @config: SQL storage configuration
 * 
 * Destructor - free a SQL configuration.
 **/
void
librdf_free_sql_config(librdf_sql_config* config)
{
  int i;
  
  if(config->values) {
    for(i=0; i < config->predicates_count; i++) {
      if(config->values[i])
        LIBRDF_FREE(cstring, config->values[i]);
    }
    LIBRDF_FREE(cstring, config->values);
  }

  if(config->filename)
    LIBRDF_FREE(cstring, config->filename);

  LIBRDF_FREE(cstring, config);
}
