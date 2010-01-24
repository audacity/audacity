/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rasqal_graph_test.c - Rasqal RDF Query GRAPH Tests
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


#define DATA_GRAPH_COUNT 3
static const char* graph_files[DATA_GRAPH_COUNT] = {
  "graph-a.ttl",
  "graph-b.ttl",
  "graph-c.ttl"
};


#define QUERY_VARIABLES_MAX_COUNT 3

struct test
{
  const char *query_language;
  const char *query_string;
  /* expected result count */
  int expected_count;
  /* data graph offsets (<0 for not used) */
  int data_graphs[DATA_GRAPH_COUNT];
  /* expected value answers */
  const char* value_answers[QUERY_VARIABLES_MAX_COUNT];
  /* expected graph answers */
  int graph_answers[QUERY_VARIABLES_MAX_COUNT];
  const char* value_var;
  const char* graph_var;
};



#define QUERY_COUNT 2


static const struct test tests[QUERY_COUNT] = {
  { /* query_language */ "sparql",
    /* query_string */ "\
PREFIX : <http://example.org/>\
SELECT ?graph ?value \
WHERE\
{\
  GRAPH ?graph { :x :b ?value } \
}\
",  
    /* expected_count */  2,
    /* data_graphs */ { 0, 1, -1 },
    /* value_answers */ { "mercury", "orange" },
    /* graph_answers */ { 0, 1 },
    /* value_var */ "value",
    /* graph_var */ "graph",
  },
  { /* query_language */ "sparql",
    /* query_string */ "\
PREFIX : <http://example.org/>\
SELECT ?graph ?value \
WHERE\
{\
  GRAPH ?graph { ?var :a ?value } \
}\
",  
    /* expected_count */  3,
    /* data_graphs */ { 0, 1, 2 },
    /* value_answers */ { "venus", "apple", "red" },
    /* graph_answers */ { 0, 1, 2 },
    /* value_var */ "value",
    /* graph_var */ "graph",
  }
};


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
  int failures=0;
  raptor_uri *base_uri;
  unsigned char *data_dir_string;
  raptor_uri* data_dir_uri;
  unsigned char *uri_string;
  int i;
  raptor_uri* graph_uris[DATA_GRAPH_COUNT];
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


  data_dir_string=raptor_uri_filename_to_uri_string(argv[1]);
  data_dir_uri=raptor_new_uri(data_dir_string);
  for(i=0; i < DATA_GRAPH_COUNT; i++)
    graph_uris[i]=raptor_new_uri_relative_to_base(data_dir_uri,
                                                  (const unsigned char*)graph_files[i]);
  
  

  for(i=0; i < QUERY_COUNT; i++) {
    rasqal_query *query = NULL;
    rasqal_query_results *results = NULL;
    const char *query_language_name=tests[i].query_language;
    const unsigned char *query_string=(const unsigned char *)tests[i].query_string;
    int count;
    int query_failed=0;
    int j;

    query=rasqal_new_query(world, query_language_name, NULL);
    if(!query) {
      fprintf(stderr, "%s: creating query %d in language %s FAILED\n", 
              program, i, query_language_name);
      query_failed=1;
      goto tidy_query;
    }

    printf("%s: preparing %s query %d\n", program, query_language_name, i);
    if(rasqal_query_prepare(query, query_string, base_uri)) {
      fprintf(stderr, "%s: %s query prepare %d FAILED\n", program, 
              query_language_name, i);
      query_failed=1;
      goto tidy_query;
    }

    for(j=0; j < DATA_GRAPH_COUNT; j++) {
      int offset=tests[i].data_graphs[j];
      if(offset >= 0)
        rasqal_query_add_data_graph(query, 
                                    graph_uris[offset], graph_uris[offset],
                                    RASQAL_DATA_GRAPH_NAMED);
    }
    

    printf("%s: executing query %d\n", program, i);
    results=rasqal_query_execute(query);
    if(!results) {
      fprintf(stderr, "%s: query execution %d FAILED\n", program, i);
      query_failed=1;
      goto tidy_query;
    }

    printf("%s: checking query %d results\n", program, i);
    count=0; 
    query_failed=0;
    while(results && !rasqal_query_results_finished(results)) {
      rasqal_literal *value;
      rasqal_literal *graph_value;
      raptor_uri* graph_uri;
      const char *value_answer=tests[i].value_answers[count];
      raptor_uri* graph_answer=graph_uris[tests[i].graph_answers[count]];
      const unsigned char* graph_var=(const unsigned char*)tests[i].graph_var;
      const unsigned char* value_var=(const unsigned char*)tests[i].value_var;
      
      value=rasqal_query_results_get_binding_value_by_name(results, 
                                                           value_var);
      if(strcmp((const char*)rasqal_literal_as_string(value), value_answer)) {
        printf("result %d FAILED: %s=", count, (char*)value_var);
        rasqal_literal_print(value, stdout);
        printf(" expected value '%s'\n", value_answer);
        query_failed=1;
        break;
      }

      graph_value=rasqal_query_results_get_binding_value_by_name(results, 
                                                                 graph_var);
      if(!graph_value) {
        printf("variable '%s' is not in the result\n", graph_var);
        query_failed=1;
        break;
      }
      
      if(graph_value->type != RASQAL_LITERAL_URI) {
        printf("variable '%s' is type %d expected %d\n", graph_var,
               graph_value->type, RASQAL_LITERAL_URI);
        query_failed=1;
        break;
      }
      
      graph_uri=graph_value->value.uri;
      if(!raptor_uri_equals(graph_uri, graph_answer)) {
        printf("result %d FAILED: %s=", count, (char*)graph_var);
        rasqal_literal_print(graph_value, stdout);
        printf(" expected URI value <%s>\n", 
               raptor_uri_as_string(graph_answer));
        query_failed=1;
        break;
      }

      rasqal_query_results_next(results);
      count++;
    }
    if(results)
      rasqal_free_query_results(results);

    printf("%s: query %d results count returned %d results\n", program, i,
           count);
    if(count != tests[i].expected_count) {
      printf("%s: query execution %d FAILED returning %d results, expected %d\n", 
             program, i, count, tests[i].expected_count);
      query_failed=1;
    }

  tidy_query:

    rasqal_free_query(query);

    if(!query_failed)
      printf("%s: query %d OK\n", program, i);
    else {
      printf("%s: query %d FAILED\n", program, i);
      failures++;
    }
  }

  for(i=0; i < DATA_GRAPH_COUNT; i++) {
    if(graph_uris[i])
      raptor_free_uri(graph_uris[i]);
  }
  raptor_free_uri(data_dir_uri);
  raptor_free_memory(data_dir_string);
  
  raptor_free_uri(base_uri);

  rasqal_free_world(world);

  return failures;
}

#endif
