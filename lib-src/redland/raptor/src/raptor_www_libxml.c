/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_www_libxml.c - Raptor WWW retrieval via libxml2
 *
 * Copyright (C) 2003-2008, David Beckett http://www.dajobe.org/
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

#ifdef RAPTOR_WWW_LIBXML

void
raptor_www_libxml_init(raptor_www *www)
{
  www->error_handlers.handlers[RAPTOR_LOG_LEVEL_NONE].user_data=www;
  www->old_xmlGenericErrorContext=xmlGenericErrorContext;
  xmlSetGenericErrorFunc(&www->error_handlers, raptor_libxml_generic_error);
  www->ctxt=NULL;
}


void
raptor_www_libxml_free(raptor_www *www)
{
  xmlSetGenericErrorFunc(www->old_xmlGenericErrorContext,
                         raptor_libxml_generic_error);
}


int
raptor_www_libxml_fetch(raptor_www *www) 
{
  char* headers=NULL;
  
  if(www->proxy)
    xmlNanoHTTPScanProxy(www->proxy);

  if(www->http_accept || www->user_agent) {
    size_t accept_len=0;
    size_t ua_len=0;
    size_t cc_len=0;
    size_t len=0;
    char *p;
    
    if(www->http_accept) {
      accept_len=strlen(www->http_accept);
      len+=accept_len+2; /* \r\n */
    }
    
    if(www->user_agent) {
      ua_len=strlen(www->user_agent);
      len+=12+ua_len+2; /* strlen("User-Agent: ") + \r\n */
    }

    if(www->cache_control) {
      cc_len=strlen(www->cache_control);
      len+=cc_len+2; /* \r\n */
    }

    headers=(char*)RAPTOR_MALLOC(cstring, len+1);
    if(!headers)
      return 1;
    
    p=headers;
    if(www->http_accept) {
      strncpy(p, www->http_accept, accept_len);
      p+= accept_len;
      *p++='\r';
      *p++='\n';
    }
    if(www->user_agent) {
      strncpy(p, "User-Agent: ", 12);
      p+=12;
      strncpy(p, www->user_agent, ua_len);
      p+= ua_len;
      *p++='\r';
      *p++='\n';
    }
    if(www->cache_control) {
      strncpy(p, www->cache_control, cc_len);
      p+= cc_len;
      *p++='\r';
      *p++='\n';
    }
    *p='\0';
  }

  www->ctxt=xmlNanoHTTPMethod((const char*)raptor_uri_as_string(www->uri),
                              NULL, /* HTTP method (default GET) */
                              NULL, /* input string */
                              &www->type,
                              headers,
                              0); /* input length - ilen */

  if(headers)
    RAPTOR_FREE(cstring, headers);

  if(!www->ctxt)
    return 1;
  
  if(www->type) {
    if(www->content_type) {
      www->content_type(www, www->content_type_userdata, www->type);
      if(www->failed) {
        xmlNanoHTTPClose(www->ctxt);
        return 1;
      }
    }
    xmlFree(www->type);
    www->type=NULL;
  }

  www->status_code=xmlNanoHTTPReturnCode(www->ctxt);
  
  while(1) {
    int len=xmlNanoHTTPRead(www->ctxt, www->buffer, RAPTOR_WWW_BUFFER_SIZE);
    if(len<0)
      break;
    
    www->total_bytes += len;

    if(www->write_bytes)
      www->write_bytes(www, www->write_bytes_userdata, www->buffer, len, 1);
    
    if(len < RAPTOR_WWW_BUFFER_SIZE || www->failed)
      break;
  }
  
  xmlNanoHTTPClose(www->ctxt);

  return www->failed;
}

#endif /* #ifdef RAPTOR_WWW_LIBXML*/
