/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_www_test.c - Raptor WWW retrieval test code
 *
 * Copyright (C) 2003-2006, David Beckett http://www.dajobe.org/
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


#ifdef HAVE_CONFIG_H
#include <raptor_config.h>
#endif

#ifdef WIN32
#include <win32_raptor_config.h>
#endif

#include <stdio.h>
#include <string.h>
#include <stdarg.h>

/* Raptor includes */
#include "raptor.h"
#include "raptor_internal.h"


static void
write_content_type(raptor_www* www,
                   void *userdata, const char *content_type) 
{
  fprintf((FILE*)userdata, "Content Type: %s\n", content_type);
}


int main (int argc, char *argv[]) 
{
  const char *uri_string;
  raptor_www *www;
  const char *user_agent="raptor-www-test";
  raptor_uri *uri;
  void *string=NULL;
  size_t string_length=0;
  
  if(argc>1)
    uri_string=argv[1];
  else
    uri_string="http://librdf.org/";

  raptor_uri_init();
  raptor_www_init();

  uri=raptor_new_uri((const unsigned char*)uri_string);
  if(!uri) {
    fprintf(stderr, "Failed to create Raptor URI for %s\n", uri_string);
    exit(1);
  }
  
  www=raptor_www_new();

  raptor_www_set_content_type_handler(www, write_content_type, (void*)stderr);
  raptor_www_set_user_agent(www, user_agent);

  /* start retrieval (always a GET) */
  
  if(raptor_www_fetch_to_string(www, uri,
                                &string, &string_length, malloc)) {
    printf("WWW fetch failed\n");
  } else {
    printf("HTTP response status %d\n", www->status_code);
    
    printf("Returned %d bytes of content\n", (int)string_length);
  }
  if(string)
    free(string);
  
  raptor_www_free(www);

  raptor_free_uri(uri);

  raptor_www_finish();
  
  return 0;
}
