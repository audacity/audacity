/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * db_upgrade.c - Upgrade a redland on-disk BDB store
 *
 * Copyright (C) 2003-2006, David Beckett http://purl.org/net/dajobe/
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


#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>

#include <redland.h>


/* one prototype needed */
int main(int argc, char *argv[]);



int
main(int argc, char *argv[]) 
{
  librdf_world* world;
  librdf_storage *storage, *new_storage;
  librdf_model *model, *new_model;
  librdf_stream *stream;
  char *program=argv[0];
  char *name;
  char *new_name;
  int count;

  if(argc < 2 || argc >3) {
    fprintf(stderr, "USAGE: %s: <Redland BDB name> [new DB name]\n", program);
    return(1);
  }

  name=argv[1];

  if(argc < 3) {
    new_name=librdf_heuristic_gen_name(name);
    if(!new_name) {
      fprintf(stderr, "%s: Failed to create new name from '%s'\n", program,
              name);
      return(1);
    }
  } else {
    new_name=argv[2];
  }
  
  fprintf(stderr, "%s: Upgrading DB '%s' to '%s'\n", program, name, new_name);

  world=librdf_new_world();
  librdf_world_open(world);

  storage=librdf_new_storage(world, "hashes", name, 
                             "hash-type='bdb',dir='.',write='no',new='no'");
  if(!storage) {
    fprintf(stderr, "%s: Failed to open old storage '%s'\n", program, name);
    return(1);
  }

  new_storage=librdf_new_storage(world, "hashes", new_name,
                                 "hash-type='bdb',dir='.',write='yes',new='yes'");
  if(!storage) {
    fprintf(stderr, "%s: Failed to create new storage '%s'\n", program, new_name);
    return(1);
  }

  model=librdf_new_model(world, storage, NULL);
  if(!model) {
    fprintf(stderr, "%s: Failed to create model for '%s'\n", program, name);
    return(1);
  }

  new_model=librdf_new_model(world, new_storage, NULL);
  if(!new_model) {
    fprintf(stderr, "%s: Failed to create new model for '%s'\n", program, new_name);
    return(1);
  }
  
  stream=librdf_model_as_stream(model);
  if(!stream) {
    fprintf(stderr, "%s: librdf_model_as_stream returned NULL stream\n", 
            program);
    return(1);
  } else {
    count=0;
    while(!librdf_stream_end(stream)) {
      librdf_statement *statement=librdf_stream_get_object(stream);
      if(!statement) {
        fprintf(stderr, "%s: librdf_stream_next returned NULL\n", program);
        break;
      }
      
      librdf_model_add_statement(new_model, statement);

      librdf_stream_next(stream);
      count++;
    }
    librdf_free_stream(stream);  
  }

      
  librdf_free_model(model);
  librdf_free_model(new_model);

  librdf_free_storage(storage);
  librdf_free_storage(new_storage);

  librdf_free_world(world);

  if(argc < 3)
    free(new_name);


#ifdef LIBRDF_MEMORY_DEBUG
  librdf_memory_report(stderr);
#endif
	
  /* keep gcc -Wall happy */
  return(0);
}
