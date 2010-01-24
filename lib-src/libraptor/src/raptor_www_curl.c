/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_www_curl.c - Raptor WWW retrieval via libcurl
 *
 * Copyright (C) 2003-2008, David Beckett http://purl.org/net/dajobe/
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

#ifdef RAPTOR_WWW_LIBCURL

#include <stdio.h>
#include <string.h>
#include <stdarg.h>

/* Raptor includes */
#include "raptor.h"
#include "raptor_internal.h"


static void
raptor_www_curl_update_status(raptor_www* www) 
{
  char* final_uri;
    
  if(www->failed)
    return;
  
  if(www->checked_status++)
    return;
  
  if(curl_easy_getinfo(www->curl_handle, CURLINFO_EFFECTIVE_URL, 
                       &final_uri) == CURLE_OK) {
    www->final_uri=raptor_new_uri((const unsigned char*)final_uri);
    if(www->final_uri_handler)
      www->final_uri_handler(www, www->final_uri_userdata, www->final_uri);
  }

}


static size_t
raptor_www_curl_write_callback(void *ptr, size_t size, size_t nmemb, void *userdata) 
{
  raptor_www* www=(raptor_www*)userdata;
  int bytes=size*nmemb;

  /* If WWW has been aborted, return nothing so that
   * libcurl will abort the transfer
   */
  if(www->failed)
    return 0;
  
  raptor_www_curl_update_status(www);

#if RAPTOR_DEBUG > 2
  RAPTOR_DEBUG2("Got %d bytes\n", bytes);
#endif

  if(www->write_bytes)
    www->write_bytes(www, www->write_bytes_userdata, ptr, size, nmemb);
  www->total_bytes += bytes;
  return bytes;
}


static size_t 
raptor_www_curl_header_callback(void* ptr,  size_t  size, size_t nmemb,
                                void *userdata) 
{
  raptor_www* www=(raptor_www*)userdata;
  int bytes=size*nmemb;

  /* If WWW has been aborted, return nothing so that
   * libcurl will abort the transfer
   */
  if(www->failed)
    return 0;
  
  if(!strncmp((char*)ptr, "Content-Type: ", 14)) {
    int len=bytes-16;
    char *type_buffer=(char*)RAPTOR_MALLOC(cstring, len+1);
    strncpy(type_buffer, (char*)ptr+14, len);
    type_buffer[len]='\0';
    if(www->type)
      RAPTOR_FREE(cstring, www->type);
    www->type=type_buffer;
    www->free_type=1;

#if RAPTOR_DEBUG > 2
    RAPTOR_DEBUG3("Got content type '%s' (%d bytes)\n", type_buffer, len);
#endif
    if(www->content_type)
      www->content_type(www, www->content_type_userdata, www->type);
  }
  
  return bytes;
}


void
raptor_www_curl_init(raptor_www *www)
{
  if(!www->curl_handle) {
    www->curl_handle=curl_easy_init();
    www->curl_init_here=1;
  }


#ifndef CURLOPT_WRITEDATA
#define CURLOPT_WRITEDATA CURLOPT_FILE
#endif

  /* send all data to this function  */
  curl_easy_setopt(www->curl_handle, CURLOPT_WRITEFUNCTION, 
                   raptor_www_curl_write_callback);
  /* ... using this data pointer */
  curl_easy_setopt(www->curl_handle, CURLOPT_WRITEDATA, www);


  /* send all headers to this function */
  curl_easy_setopt(www->curl_handle, CURLOPT_HEADERFUNCTION, 
                   raptor_www_curl_header_callback);
  /* ... using this data pointer */
  curl_easy_setopt(www->curl_handle, CURLOPT_WRITEHEADER, www);

  /* Make it follow Location: headers */
  curl_easy_setopt(www->curl_handle, CURLOPT_FOLLOWLOCATION, 1);

#if RAPTOR_DEBUG > 2
  curl_easy_setopt(www->curl_handle, CURLOPT_VERBOSE, (void*)1);
#endif

  curl_easy_setopt(www->curl_handle, CURLOPT_ERRORBUFFER, www->error_buffer);

  /* Connection timeout in seconds */
  curl_easy_setopt(www->curl_handle, CURLOPT_CONNECTTIMEOUT,
                   www->connection_timeout);
  curl_easy_setopt(www->curl_handle, CURLOPT_NOSIGNAL, 1);
}


void
raptor_www_curl_free(raptor_www *www)
{
    /* only tidy up if we did all the work */
  if(www->curl_init_here && www->curl_handle) {
    curl_easy_cleanup(www->curl_handle);
    www->curl_handle=NULL;
  }
}


int
raptor_www_curl_fetch(raptor_www *www) 
{
  struct curl_slist *slist=NULL;
    
  if(www->proxy)
    curl_easy_setopt(www->curl_handle, CURLOPT_PROXY, www->proxy);

  if(www->user_agent)
    curl_easy_setopt(www->curl_handle, CURLOPT_USERAGENT, www->user_agent);

  if(www->http_accept)
    slist=curl_slist_append(slist, (const char*)www->http_accept);

  /* ALWAYS disable curl default "Pragma: no-cache" */
  slist=curl_slist_append(slist, "Pragma:");
  if(www->cache_control)
    slist=curl_slist_append(slist, (const char*)www->cache_control);

  if(slist)
    curl_easy_setopt(www->curl_handle, CURLOPT_HTTPHEADER, slist);

  /* specify URL to get */
  curl_easy_setopt(www->curl_handle, CURLOPT_URL, 
                   raptor_uri_as_string(www->uri));

  if(curl_easy_perform(www->curl_handle)) {
    /* failed */
    www->failed=1;
    raptor_www_error(www, "Resolving URI failed: %s", www->error_buffer);
  } else {
    long lstatus;

#ifndef CURLINFO_RESPONSE_CODE
#define CURLINFO_RESPONSE_CODE CURLINFO_HTTP_CODE
#endif

    /* Requires pointer to a long */
    if(curl_easy_getinfo(www->curl_handle, CURLINFO_RESPONSE_CODE, &lstatus) == CURLE_OK)
      www->status_code=lstatus;

  }

  if(slist)
    curl_slist_free_all(slist);
  
  return www->failed;
}

#endif /* RAPTOR_WWW_LIBCURL */
