/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rasqal_order_test.c - Rasqal RDF Query Order Tests
 *
 * Copyright (C) 2006, David Beckett http://purl.org/net/dajobe/
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
#include <rasqal_config.h>
#endif

#ifdef WIN32
#include <win32_rasqal_config.h>
#endif

#include <stdio.h>
#include <string.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#include <stdarg.h>

#include "rasqal.h"
#include "rasqal_internal.h"

#ifdef RASQAL_QUERY_SPARQL

#define QUERY_LANGUAGE "sparql"

static const char* animalsList[27]={ "aardvark", "badger", "cow", "dog",
  "elephant", "fox", "goat", "horse", "iguana", "jackal", "koala", "lemur",
  "mouse", "newt", "owl", "penguin", "quail", "rat", "snake", "tiger",
  "uakari", "vole", "whale", "xantus", "yak", "zebra", NULL };

#define QUERY_FORMAT "\
PREFIX ex: <http://ex.example.org#> \n\
SELECT $animal \n\
FROM <%s/%s> \n\
WHERE { \n\
  $zoo ex:hasAnimal $animal \n\
} ORDER BY $animal LIMIT %d OFFSET %d"

#else
#define NO_QUERY_LANGUAGE
#endif


#ifdef NO_QUERY_LANGUAGE
int
main(int argc, char **argv) {
  const char *program=rasqal_basename(argv[0]);
  fprintf(stderr, "%s: No supported query language available, skipping test\n", program);
  return(0);
}
#else

int
main(int argc, char **argv) {
  const char *program=rasqal_basename(argv[0]);
  raptor_uri *base_uri;
  unsigned char *uri_string;
  const char *query_language_name=QUERY_LANGUAGE;
  const char *query_format=QUERY_FORMAT;
  static const char* animals="animals.nt";
  int failures=0;
  int i;
  rasqal_world *world;
  
  world=rasqal_new_world();
  if(!world) {
    fprintf(stderr, "%s: rasqal_new_world() failed\n", program);
    return(1);
  }
  
  if(argc != 2) {
    fprintf(stderr, "USAGE: %s <path to data directory>\n", program);
    return(1);
  }

  uri_string=raptor_uri_filename_to_uri_string("");
  base_uri=raptor_new_uri(uri_string);  
  raptor_free_memory(uri_string);


  for(i=0; i<6; i++) {
    rasqal_query *query = NULL;
    rasqal_query_results *results = NULL;
    unsigned char *data_dir_string;
    unsigned char *query_string;
    int count;
    int limit=9-3*i;
    int offset=0+3*i;

    if(i==4) {
      limit=8;
      offset=1;
    } else if (i==5) {
      limit=1;
      offset=8;
    }
    

    data_dir_string=raptor_uri_filename_to_uri_string(argv[1]);
    query_string=(unsigned char*)RASQAL_MALLOC(cstring, strlen((const char*)data_dir_string)+strlen(animals)+strlen(query_format)+1);
    sprintf((char*)query_string, query_format, data_dir_string, animals,
            limit, offset);
    raptor_free_memory(data_dir_string);

    query=rasqal_new_query(world, query_language_name, NULL);
    if(!query) {
      fprintf(stderr, "%s: creating query %d in language %s FAILED\n", 
              program, i, query_language_name);
      return(1);
    }

    printf("%s: preparing %s query %d\n", program, query_language_name, i);
    if(rasqal_query_prepare(query, query_string, base_uri)) {
      fprintf(stderr, "%s: %s query prepare %d '%s' FAILED\n", program, 
              query_language_name, i, query_string);
      return(1);
    }

    RASQAL_FREE(cstring, query_string);

    printf("%s: executing query %d limit %d offset %d\n", program, i,
           limit, offset);
    results=rasqal_query_execute(query);
    if(!results) {
      fprintf(stderr, "%s: query execution %d FAILED\n", program, i);
      return(1);
    }

    printf("%s: checking query %d results\n", program, i);
    count=0;
    while(results && !rasqal_query_results_finished(results)) {
      const unsigned char *name=(const unsigned char *)"animal";
      rasqal_literal *value=rasqal_query_results_get_binding_value_by_name(results, name);
      const char *answer=animalsList[count+offset];

      if(strcmp((const char*)rasqal_literal_as_string(value), answer)) {
        printf("result %d FAILED: %s=", count, (char*)name);
        rasqal_literal_print(value, stdout);
        printf(" expected value '%s'\n", answer);
        failures++;
      }
      rasqal_query_results_next(results);
      count++;
    }
    if(results)
      rasqal_free_query_results(results);

    printf("%s: checking query %d results count\n", program, i);
    if(count != limit) {
      printf("%s: query execution %d FAILED returning %d results, expected %d\n", 
             program, i, count, limit);
      return(1);
    }

    rasqal_free_query(query);

    printf("%s: query %d OK\n", program, i);
  }
  
  raptor_free_uri(base_uri);

  rasqal_free_world(world);

  return failures;
}

#endif
