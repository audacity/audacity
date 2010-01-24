/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rasqal_limit_test.c - Rasqal RDF Query LIMIT Tests
 *
 * Copyright (C) 2007-2008, David Beckett http://www.dajobe.org/
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
#define QUERY_UNSORTED "\
 SELECT $letter \
 FROM <%s> \
 WHERE { <http://example.org/> <http://example.org#pred> $letter } \
"
#define QUERY_SORTED QUERY_UNSORTED " ORDER BY $letter \
"
#else
#define NO_QUERY_LANGUAGE
#endif


#ifdef NO_QUERY_LANGUAGE
int
main(int argc, char **argv) {
  const char *program=rasqal_basename(argv[0]);
  fprintf(stderr, "%s: SPARQL query language not available, skipping test\n", program);
  return(0);
}
#else

#define NQUERIES 2
static const char* limit_queries[NQUERIES]= { QUERY_UNSORTED, QUERY_SORTED };


typedef struct 
{
  int limit;
  int offset;
  int expected_count;
  const char *expected_results[NQUERIES];
} limit_test;

#define NONE (-1)
static limit_test limit_offset_tests[]={
/* limit offset count results */
  { NONE, NONE,   26, { "jcthzguxwpnefbioqadmrvykls", "abcdefghijklmnopqrstuvwxyz" } },
  { 0,    NONE,    0, { "", "" } },
  { NONE,    0,   26, { "jcthzguxwpnefbioqadmrvykls", "abcdefghijklmnopqrstuvwxyz" } },
  { 10,   NONE,   10, { "jcthzguxwp", "abcdefghij" } },
  { NONE,    5,   21, { "guxwpnefbioqadmrvykls", "fghijklmnopqrstuvwxyz" } },
  { 10,      5,   10, { "guxwpnefbi", "fghijklmno" } },
  { 5,      10,    5, { "nefbi", "klmno" } },
  { NONE, NONE, NONE, { NULL, NULL } }
};

    


int
main(int argc, char **argv) {
  const char *program=rasqal_basename(argv[0]);
  const char *query_language_name=QUERY_LANGUAGE;
  raptor_uri *base_uri;
  unsigned char *uri_string;
  int test_i;
  limit_test* test;
  int tests_failed_count=0;
  int single_shot= -1;
  int query_i;
  rasqal_world *world;
  
  world=rasqal_new_world();
  if(!world) {
    fprintf(stderr, "%s: rasqal_new_world() failed\n", program);
    return(1);
  }
  
  if(argc < 2 || argc > 3) {
    fprintf(stderr, "USAGE: %s data-filename [test number]\n", program);
    return(1);
  }
    
  if(argv[2])
    single_shot=atoi(argv[2]);
  
  uri_string=raptor_uri_filename_to_uri_string("");
  base_uri=raptor_new_uri(uri_string);  
  raptor_free_memory(uri_string);

  for(query_i=0; query_i < NQUERIES; query_i++) {
    const char *query_format=limit_queries[query_i];
    unsigned char *data_string;
    unsigned char *query_string;
    
    data_string=raptor_uri_filename_to_uri_string(argv[1]);
    query_string=(unsigned char*)RASQAL_MALLOC(cstring, strlen((const char*)data_string)+strlen(query_format)+1);
    sprintf((char*)query_string, query_format, data_string);
    raptor_free_memory(data_string);

    for(test_i=(single_shot >=0 ? single_shot : 0);
        (test=&limit_offset_tests[test_i]) && (test->expected_count >=0);
        test_i++) {
      int result_i;
      rasqal_query *query = NULL;
      rasqal_query_results *results = NULL;
      const char* expected_results;
      int test_ok=1;

      query=rasqal_new_query(world, query_language_name, NULL);
      if(!query) {
        fprintf(stderr, "%s: creating query in language %s FAILED\n", program,
                query_language_name);
        return(1);
      }

#if RASQAL_DEBUG > 1
      fprintf(stderr, 
              "%s: preparing query %d test %d\n", program, query_i, test_i);
#endif
      if(rasqal_query_prepare(query, query_string, base_uri)) {
        fprintf(stderr, "%s: query %d test %d prepare FAILED\n", program, 
                query_i, test_i);
        return(1);
      }

      rasqal_query_set_limit(query, test->limit);
      rasqal_query_set_offset(query, test->offset);

#if RASQAL_DEBUG > 1
      fprintf(stderr, "%s: executing query %d test %d\n", program, query_i,
              test_i);
#endif
      results=rasqal_query_execute(query);
      if(!results) {
        fprintf(stderr, "%s: query %d test %d FAILED\n", program,
                query_i, test_i);
        return(1);
      }


      /* check results */
      expected_results=test->expected_results[query_i];

      result_i=0;
      tests_failed_count=0;
      while(results && !rasqal_query_results_finished(results)) {
        rasqal_literal *value;
        const char* str;
        char expected_str[2]={0,0};
        int failed=1;

        if(result_i < test->expected_count)
          expected_str[0]=expected_results[result_i];

        value=rasqal_query_results_get_binding_value(results, 0);
        if(value) {
          int error=0;
          str=(const char*)rasqal_literal_as_string_flags(value, 0, &error);
          if(!error && str) {
            size_t len=strlen(str);
            if(len == 1 && str[0] == expected_str[0])
              failed=0;
          }
        }
        if(failed) {
          fprintf(stderr,
                  "%s: query %d test %d result %d FAILED returning value '%s' expected value '%s'\n",
                  program, query_i, test_i, result_i, (str ? str: "NULL"),
                  expected_str);
          test_ok=0;
        }

        rasqal_query_results_next(results);
        result_i++;
      }
      if(results)
        rasqal_free_query_results(results);

      if(!test_ok) {
        fprintf(stderr, "%s: query %d test %d limit %d offset %d FAILED\n",
                program, query_i, test_i, test->limit, test->offset);
      } else {
        if(result_i != test->expected_count) {
          fprintf(stderr,
                  "%s: query %d test %d limit %d offset %d FAILED returned %d results, expected %d\n",
                  program, 
                  query_i, test_i, test->limit, test->offset, result_i,
                  test->expected_count);
          test_ok=0;
        }
      }


      if(!test_ok) {
        tests_failed_count++;
      } else {
#if RASQAL_DEBUG > 1
        fprintf(stderr, "%s: query %d test %d OK\n", program, query_i, test_i);
#endif
      }

      rasqal_free_query(query);

      if(single_shot >=0)
        break;
    } /* end test loop */


    RASQAL_FREE(cstring, query_string);

  } /* end query loop */
  
  raptor_free_uri(base_uri);

  rasqal_free_world(world);

  return tests_failed_count;
}

#endif
