/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_www_libfetch.c - Raptor WWW retrieval via libfetch
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

#ifdef RAPTOR_WWW_LIBFETCH

#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <errno.h>

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#include <fetch.h>

/* Raptor includes */
#include "raptor.h"
#include "raptor_internal.h"

void
raptor_www_libfetch_init(raptor_www *www)
{
}


void
raptor_www_libfetch_free(raptor_www *www)
{
}


int
raptor_www_libfetch_fetch(raptor_www *www) 
{
  FILE *stream;

  if(www->proxy) {
    setenv("HTTP_PROXY", www->proxy, 0);
    setenv("FTP_PROXY", www->proxy, 0);
  }

  if(www->user_agent)
    setenv("HTTP_USER_AGENT", www->user_agent, 0);

  stream=fetchXGetURL((const char*)raptor_uri_as_string(www->uri), NULL, NULL);
  if(!stream) {
    www->failed=1;
    raptor_www_error(www, "%s", fetchLastErrString);
    return 1;
  }
  
  /* fetch does not give us access to this */
  www->status_code=200;
  
  while(!feof(stream)) {
    size_t len=fread(www->buffer, 1, RAPTOR_WWW_BUFFER_SIZE, stream);
    
    www->total_bytes += len;

    if(www->write_bytes)
      www->write_bytes(www, www->write_bytes_userdata, www->buffer, len, 1);
    
    if(len < RAPTOR_WWW_BUFFER_SIZE)
      break;
  }
  fclose(stream);
  
  return www->failed;
}

#endif /* RAPTOR_WWW_LIBFETCH */
