/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_storage_sql_test.c - RDF Storage SQL config test program
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
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <redland.h>

/* one prototype needed */
int main(int argc, char *argv[]);


int
main(int argc, char *argv[])
{
  librdf_world* world;
  int failures=0;
  int i;
  const char *program=librdf_basename((const char*)argv[0]);
  const char* storage="mysql";
  const char* dir;
  
  if(argc>1)
    dir=argv[1];
  else
    dir=".";
  
  world=librdf_new_world();
  librdf_world_open(world);

  for(i=0; i<2; i++) {
    const char* layout=(i == 0) ? "v1" : "v2";
    librdf_sql_config* config;

    fprintf(stderr, "%s: Opening SQL config for storage %s layout %s\n",
            program, storage, layout);
    
    config=librdf_new_sql_config(world, storage, layout, dir, 
                                 librdf_storage_sql_dbconfig_predicates);
    if(config) {
      const char* value=config->values[DBCONFIG_CREATE_TABLE_BNODES];
      if(value)
        fprintf(stderr, "%s: Bnode table declaration found of %zu bytes size\n",
                program, strlen(value));
      else {
        fprintf(stderr, "%s: FAILED Bnode table declaration not found\n",
                program);
        failures++;
      }
      
      librdf_free_sql_config(config);
    } else {
      fprintf(stderr, "%s: FAILED to get complete SQL configuration\n",
              program);
      failures++;
    }
  }

  librdf_free_world(world);

  return failures;
}
