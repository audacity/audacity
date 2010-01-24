/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * srxread.c - SPARQL Results XML Format reading test program
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
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif


#include <raptor.h>

/* Rasqal includes */
#include <rasqal.h>
#include <rasqal_internal.h>

static char *program=NULL;

int main(int argc, char *argv[]);

int
main(int argc, char *argv[]) 
{ 
  int rc=0;
  const char* srx_filename=NULL;
  raptor_iostream* iostr=NULL;
  char* p;
  unsigned char* uri_string=NULL;
  raptor_uri* base_uri=NULL;
  rasqal_query_results* results=NULL;
  const char* results_formatter_name=NULL;
  rasqal_query_results_formatter* formatter=NULL;
  raptor_iostream *write_iostr=NULL;
  rasqal_world *world;
  
  program=argv[0];
  if((p=strrchr(program, '/')))
    program=p+1;
  else if((p=strrchr(program, '\\')))
    program=p+1;
  argv[0]=program;
  
  world=rasqal_new_world();
  if(!world) {
    fprintf(stderr, "%s: rasqal_new_world() failed\n", program);
    return(1);
  }

  if(argc != 2) {
    fprintf(stderr, "USAGE: %s SRX file\n", program);

    rc=1;
    goto tidy;
  }

  srx_filename=argv[1];

  uri_string=raptor_uri_filename_to_uri_string((const char*)srx_filename);
  if(!uri_string)
    goto tidy;
  
  base_uri=raptor_new_uri(uri_string);
  raptor_free_memory(uri_string);

  results=rasqal_new_query_results(NULL);
  if(!results) {
    fprintf(stderr, "%s: Failed to create query results", program);
    rc=1;
    goto tidy;
  }
  
  iostr=raptor_new_iostream_from_filename(srx_filename);
  if(!iostr) {
    fprintf(stderr, "%s: Failed to open iostream to file %s", program,
            srx_filename);
    rc=1;
    goto tidy;
  }

  formatter=rasqal_new_query_results_formatter(world, results_formatter_name,
                                               NULL);
  if(!formatter) {
    fprintf(stderr, "%s: Failed to create query results formatter '%s'",
            program, results_formatter_name);
    rc=1;
    goto tidy;
  }
  
  rc=rasqal_query_results_formatter_read(world, iostr, formatter, results,
                                         base_uri);
  if(rc)
    goto tidy;

  RASQAL_DEBUG2("Made query results with %d results\n",
                rasqal_query_results_get_count(results));

  write_iostr=raptor_new_iostream_to_file_handle(stdout);
  if(!write_iostr) {
    fprintf(stderr, "%s: Creating output iostream failed\n", program);
  } else {
    rasqal_query_results_formatter_write(write_iostr, formatter,
                                         results, base_uri);
    raptor_free_iostream(write_iostr);
  }


  tidy:
  if(formatter)
    rasqal_free_query_results_formatter(formatter);

  if(iostr)
    raptor_free_iostream(iostr);
  
  if(results)
    rasqal_free_query_results(results);

  if(base_uri)
    raptor_free_uri(base_uri);

  rasqal_free_world(world);

  return (rc);
}
