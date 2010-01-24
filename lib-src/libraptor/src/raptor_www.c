/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_www.c - Raptor WWW retrieval core
 *
 * Copyright (C) 2003-2007, David Beckett http://purl.org/net/dajobe/
 * Copyright (C) 2003-2005, University of Bristol, UK http://www.bristol.ac.uk/
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
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

/* Raptor includes */
#include "raptor.h"
#include "raptor_internal.h"



static int raptor_www_file_fetch(raptor_www* www);


/* should raptor_www do initializing and cleanup of the WWW library */
static int raptor_www_do_www_init_finish=1;

static int raptor_www_initialized=0;


/**
 * raptor_www_init:
 * 
 * Initialise the WWW class.
 *
 * Must be called before creating any #raptor_www object.
 **/
void
raptor_www_init(void)
{
  if(raptor_www_initialized)
    return;

  if(raptor_www_do_www_init_finish) {
#ifdef RAPTOR_WWW_LIBCURL
    curl_global_init(CURL_GLOBAL_ALL);
#endif
  }
  raptor_www_initialized=1;
}


/**
 * raptor_www_no_www_library_init_finish:
 *
 * Do not initialise or finish the lower level WWW library.
 *
 * If this is called then the raptor_www library will neither
 * initialise or terminate the lower level WWW library.  Usually in
 * raptor_init either curl_global_init (for libcurl)
 * are called and in raptor_finish curl_global_cleanup is called.
 *
 * This allows the application finer control over these libraries such
 * as setting other global options or potentially calling and terminating
 * raptor several times.  It does mean that applications which use
 * this call must do their own extra work in order to allocate and free
 * all resources to the system.
 * 
 * This function must be called before raptor_init.
 *
 **/
void
raptor_www_no_www_library_init_finish(void)
{
  raptor_www_do_www_init_finish=0;
}


/**
 * raptor_www_finish:
 * 
 * Terminate the WWW class.
 *
 * Must be called to clean any resources used by the WWW implementation.
 **/
void
raptor_www_finish(void)
{
  if(raptor_www_do_www_init_finish) {
#ifdef RAPTOR_WWW_LIBCURL
    curl_global_cleanup();
#endif
  }
}



/**
 * raptor_www_new_with_connection:
 * @connection: external WWW connection object.
 * 
 * Constructor - create a new #raptor_www object over an existing WWW connection.
 *
 * At present this only works with a libcurl CURL handle object
 * when raptor is compiled with libcurl suppport. Otherwise the
 * @connection is ignored.  This allows such things as setting
 * up special flags on the curl handle before passing into the constructor.
 * 
 * Return value: a new #raptor_www object or NULL on failure.
 **/
raptor_www* 
raptor_www_new_with_connection(void *connection)
{
  raptor_www* www=(raptor_www* )RAPTOR_CALLOC(www, 1, sizeof(raptor_www));
  if(!www)
    return NULL;
  
  www->type=NULL;
  www->free_type=1; /* default is to free content type */
  www->total_bytes=0;
  www->failed=0;
  www->status_code=0;
  www->write_bytes=NULL;
  www->content_type=NULL;
  www->uri_filter=NULL;
  www->connection_timeout=10;
  www->cache_control=NULL;

#ifdef RAPTOR_WWW_LIBCURL
  www->curl_handle=(CURL*)connection;
  raptor_www_curl_init(www);
#endif
#ifdef RAPTOR_WWW_LIBXML
  raptor_www_libxml_init(www);
#endif
#ifdef RAPTOR_WWW_LIBFETCH
  raptor_www_libfetch_init(www);
#endif

  www->error_handlers.locator=&www->locator;
  raptor_error_handlers_init(&www->error_handlers);

  return www;
}


/**
 * raptor_www_new:
 * 
 * Constructor - create a new #raptor_www object.
 * 
 * Return value: a new #raptor_www or NULL on failure.
 **/
raptor_www*
raptor_www_new(void)
{
  return raptor_www_new_with_connection(NULL);
}


/**
 * raptor_www_free: 
 * @www: WWW object.
 * 
 * Destructor - destroy a #raptor_www object.
 **/
void
raptor_www_free(raptor_www* www)
{
  /* free context */
  if(www->type) {
    if(www->free_type)
      RAPTOR_FREE(cstring, www->type);
    www->type=NULL;
  }
  
  if(www->user_agent) {
    RAPTOR_FREE(cstring, www->user_agent);
    www->user_agent=NULL;
  }

  if(www->cache_control) {
    RAPTOR_FREE(cstring, www->cache_control);
    www->cache_control=NULL;
  }

  if(www->proxy) {
    RAPTOR_FREE(cstring, www->proxy);
    www->proxy=NULL;
  }

  if(www->http_accept) {
    RAPTOR_FREE(cstring, www->http_accept);
    www->http_accept=NULL;
  }

#ifdef RAPTOR_WWW_LIBCURL
  raptor_www_curl_free(www);
#endif
#ifdef RAPTOR_WWW_LIBXML
  raptor_www_libxml_free(www);
#endif
#ifdef RAPTOR_WWW_LIBFETCH
  raptor_www_libfetch_free(www);
#endif

  if(www->uri)
    raptor_free_uri(www->uri);

  if(www->final_uri)
    raptor_free_uri(www->final_uri);

  RAPTOR_FREE(www, www);
}



/**
 * raptor_www_set_error_handler:
 * @www: WWW object
 * @error_handler: error handler function
 * @error_data: error handler data
 * 
 * Set the error handler routine for the raptor_www class.
 *
 * This takes the same arguments as the raptor_parser_set_error() and
 * raptor_parser_set_warning_handler() methods.
 **/
void
raptor_www_set_error_handler(raptor_www* www, 
                             raptor_message_handler error_handler,
                             void *error_data)
{
  www->error_handlers.handlers[RAPTOR_LOG_LEVEL_ERROR].user_data=error_data;
  www->error_handlers.handlers[RAPTOR_LOG_LEVEL_ERROR].handler=error_handler;
}


/**
 * raptor_www_set_write_bytes_handler:
 * @www: WWW object
 * @handler: bytes handler function
 * @user_data: bytes handler data
 * 
 * Set the handler to receive bytes written by the #raptor_www implementation.
 *
 **/
void
raptor_www_set_write_bytes_handler(raptor_www* www, 
                                   raptor_www_write_bytes_handler handler, 
                                   void *user_data)
{
  www->write_bytes=handler;
  www->write_bytes_userdata=user_data;
}


/**
 * raptor_www_set_content_type_handler:
 * @www: WWW object
 * @handler: content type handler function
 * @user_data: content type handler data
 * 
 * Set the handler to receive the HTTP Content-Type header value.
 *
 * This is called if or when the value is discovered during retrieval
 * by the raptor_www implementation.  Not all implementations provide
 * access to this.
 **/
void
raptor_www_set_content_type_handler(raptor_www* www, 
                                    raptor_www_content_type_handler handler, 
                                    void *user_data)
{
  www->content_type=handler;
  www->content_type_userdata=user_data;
}


/**
 * raptor_www_set_user_agent:
 * @www: WWW object
 * @user_agent: User-Agent string
 * 
 * Set the user agent value, for HTTP requests typically.
 **/
void
raptor_www_set_user_agent(raptor_www* www, const char *user_agent)
{
  char *ua_copy=NULL;

  if(!user_agent || !*user_agent) {
    www->user_agent=NULL;
    return;
  }
  
  ua_copy=(char*)RAPTOR_MALLOC(cstring, strlen(user_agent)+1);
  if(!ua_copy)
    return;
  strcpy(ua_copy, user_agent);
  
  www->user_agent=ua_copy;
}


/**
 * raptor_www_set_proxy:
 * @www: WWW object
 * @proxy: proxy string.
 * 
 * Set the proxy for the WWW object.
 *
 * The @proxy usually a string of the form http://server.domain:port.
 **/
void
raptor_www_set_proxy(raptor_www* www, const char *proxy)
{
  char *proxy_copy;

  if(!proxy)
    return;
  
  proxy_copy=(char*)RAPTOR_MALLOC(cstring, strlen(proxy)+1);
  if(!proxy_copy)
    return;
  strcpy(proxy_copy, proxy);
  
  www->proxy=proxy_copy;
}


/**
 * raptor_www_set_http_accept:
 * @www: #raptor_www class
 * @value: Accept: header value or NULL to have an empty one.
 *
 * Set HTTP Accept header.
 * 
 **/
void
raptor_www_set_http_accept(raptor_www* www, const char *value)
{
  char *value_copy;
  size_t len=8; /* strlen("Accept:")+1 */
  
  if(value)
    len+=1+strlen(value); /* " "+value */
  
  value_copy=(char*)RAPTOR_MALLOC(cstring, len);
  if(!value_copy)
    return;
  www->http_accept=value_copy;

  strcpy(value_copy, "Accept:");
  value_copy+=7;
  if(value) {
    *value_copy++=' ';
    strcpy(value_copy, value);
  }

#if RAPTOR_DEBUG > 1
  RAPTOR_DEBUG2("Using Accept header: '%s'\n", www->http_accept);
#endif
}


/**
 * raptor_www_set_connection_timeout:
 * @www: WWW object
 * @timeout: Timeout in seconds
 * 
 * Set WWW connection timeout
 **/
void
raptor_www_set_connection_timeout(raptor_www* www, int timeout)
{
  www->connection_timeout=timeout;
}


/**
 * raptor_www_set_http_cache_control:
 * @www: WWW object
 * @cache_control: Cache-Control header value (or NULL to disable)
 *
 * Set HTTP Cache-Control:header (default none)
 *
 * The @cache_control value can be a string to set it, "" to send
 * a blank header or NULL to not set the header at all.
 *
 * Return value: non-0 on failure
 **/
int
raptor_www_set_http_cache_control(raptor_www* www, const char* cache_control)
{
  char *cache_control_copy;
  const char* const header="Cache-Control:";
  const size_t header_len=14; /* strlen("Cache-Control:") */
  size_t len;
  
  RAPTOR_ASSERT((strlen(header) != header_len), "Cache-Control header length is wrong");

  if(www->cache_control)
    RAPTOR_FREE(cstring, www->cache_control);

  if(!cache_control) {
    www->cache_control=NULL;
    return 0;
  }
  
  len=header_len + 1 +strlen(cache_control) + 1; /* header+" "+cache_control+"\0" */
  
  cache_control_copy=(char*)RAPTOR_MALLOC(cstring, len);
  if(!cache_control_copy)
    return 1;
  
  www->cache_control=cache_control_copy;

  strncpy(cache_control_copy, header, header_len);
  cache_control_copy+= header_len;
  if(*cache_control) {
    *cache_control_copy++=' ';
    strcpy(cache_control_copy, cache_control);
  }
  
#if RAPTOR_DEBUG > 1
  RAPTOR_DEBUG2("Using Cache-Control header: '%s'\n", www->cache_control);
#endif

  return 0;
}


/**
 * raptor_www_set_uri_filter:
 * @www: WWW object
 * @filter: URI filter function
 * @user_data: User data to pass to filter function
 * 
 * Set URI filter function for WWW retrieval.
 **/
void
raptor_www_set_uri_filter(raptor_www* www, 
                          raptor_uri_filter_func filter,
                          void *user_data)
{
  www->uri_filter=filter;
  www->uri_filter_user_data=user_data;
}


/**
 * raptor_www_get_connection:
 * @www: #raptor_www object 
 *
 * Get WWW library connection object.
 * 
 * Return the internal WWW connection handle.  For libcurl, this
 * returns the CURL handle and for libxml the context.  Otherwise
 * it returns NULL.
 *
 * Return value: connection pointer
 **/
void*
raptor_www_get_connection(raptor_www* www) 
{
#ifdef RAPTOR_WWW_NONE
  return NULL;
#endif

#ifdef RAPTOR_WWW_LIBCURL
  return www->curl_handle;
#endif

#ifdef RAPTOR_WWW_LIBXML
  return www->ctxt;
#endif

#ifdef RAPTOR_WWW_LIBFETCH
  return NULL;
#endif

  return NULL;
}


/**
 * raptor_www_abort:
 * @www: WWW object
 * @reason: abort reason message
 * 
 * Abort an ongoing raptor WWW operation and pass back a reason.
 *
 * This is typically used within one of the raptor WWW handlers
 * when retrieval need no longer continue due to another
 * processing issue or error.
 **/
void
raptor_www_abort(raptor_www* www, const char *reason)
{
  www->failed=1;
}


void
raptor_www_error(raptor_www* www, const char *message, ...) 
{
  va_list arguments;

  va_start(arguments, message);

  raptor_log_error_varargs(RAPTOR_LOG_LEVEL_ERROR,
                           www->error_handlers.handlers[RAPTOR_LOG_LEVEL_ERROR].handler,
                           www->error_handlers.handlers[RAPTOR_LOG_LEVEL_ERROR].user_data,
                           &www->locator,
                           message, arguments);

  va_end(arguments);
}

  
static int 
raptor_www_file_handle_fetch(raptor_www* www, FILE* fh) 
{
  unsigned char buffer[RAPTOR_WWW_BUFFER_SIZE+1];
  
  while(!feof(fh)) {
    int len=fread(buffer, 1, RAPTOR_WWW_BUFFER_SIZE, fh);
    if(len > 0) {
      www->total_bytes += len;
      buffer[len]='\0';
      
      if(www->write_bytes)
        www->write_bytes(www, www->write_bytes_userdata, buffer, len, 1);
    }

    if(feof(fh) || www->failed)
      break;
  }
  
  if(!www->failed)
    www->status_code=200;
  
  return www->failed;
}


static int 
raptor_www_file_fetch(raptor_www* www) 
{
  char *filename;
  FILE *fh;
  unsigned char *uri_string=raptor_uri_as_string(www->uri);
#if defined(HAVE_UNISTD_H) && defined(HAVE_SYS_STAT_H)
  struct stat buf;
#endif
  
  www->status_code=200;

  filename=raptor_uri_uri_string_to_filename(uri_string);
  if(!filename) {
    raptor_www_error(www, "Not a file: URI");
    return 1;
  }

#if defined(HAVE_UNISTD_H) && defined(HAVE_SYS_STAT_H)
  if(!stat(filename, &buf) && S_ISDIR(buf.st_mode)) {
    raptor_www_error(www, "Cannot read from a directory '%s'", filename);
    RAPTOR_FREE(cstring, filename);
    www->status_code=404;
    return 1;
  }
#endif

  fh=fopen(filename, "rb");
  if(!fh) {
    raptor_www_error(www, "file '%s' open failed - %s",
                     filename, strerror(errno));
    RAPTOR_FREE(cstring, filename);
    www->status_code=(errno == EACCES) ? 403: 404;
    www->failed=1;
    
    return www->failed;
  }

  raptor_www_file_handle_fetch(www, fh);
  fclose(fh);

  RAPTOR_FREE(cstring, filename);
  
  return www->failed;
}


/**
* raptor_www_fetch:
* @www: WWW object
* @uri: URI to read from
* 
* Start a WWW content retrieval for the given URI, returning data via the write_bytes handler.
* 
* Return value: non-0 on failure.
**/
int
raptor_www_fetch(raptor_www *www, raptor_uri *uri) 
{
  int status=1;
  
  www->uri=raptor_new_uri_for_retrieval(uri);
  
  www->locator.uri=uri;
  www->locator.line= -1;
  www->locator.column= -1;

  if(www->uri_filter)
    if(www->uri_filter(www->uri_filter_user_data, uri))
      return status;
  
#ifdef RAPTOR_WWW_NONE
  status=raptor_www_file_fetch(www);
#else

  if(raptor_uri_uri_string_is_file_uri(raptor_uri_as_string(www->uri)))
    status=raptor_www_file_fetch(www);
  else {
#ifdef RAPTOR_WWW_LIBCURL
    status=raptor_www_curl_fetch(www);
#endif

#ifdef RAPTOR_WWW_LIBXML
    status=raptor_www_libxml_fetch(www);
#endif

#ifdef RAPTOR_WWW_LIBFETCH
    status=raptor_www_libfetch_fetch(www);
#endif
  }
  
#endif
  if(!status && www->status_code && www->status_code != 200){
    raptor_www_error(www, "Resolving URI failed with HTTP status %d",
                     www->status_code);
    status=1;
  }

  www->failed=status;
  
  return www->failed;
}


static void
raptor_www_fetch_to_string_write_bytes(raptor_www* www, void *userdata,
                                       const void *ptr, size_t size,
                                       size_t nmemb)
{
  raptor_stringbuffer* sb=(raptor_stringbuffer*)userdata;
  int len=size*nmemb;

  raptor_stringbuffer_append_counted_string(sb, (unsigned char*)ptr, len, 1);
}


/**
 * raptor_www_fetch_to_string:
 * @www: raptor_www object
 * @uri: raptor_uri to retrieve
 * @string_p: pointer to location to hold string
 * @length_p: pointer to location to hold length of string (or NULL)
 * @malloc_handler: pointer to malloc to use to make string (or NULL)
 *
 * Start a WWW content retrieval for the given URI, returning the data in a new string.
 *
 * If malloc_handler is null, raptor will allocate it using it's
 * own memory allocator.  *string_p is set to NULL on failure (and
 * *length_p to 0 if length_p is not NULL).
 * 
 * Return value: non-0 on failure
 **/
int
raptor_www_fetch_to_string(raptor_www *www, raptor_uri *uri,
                           void **string_p, size_t *length_p,
                           void *(*malloc_handler)(size_t size))
{
  raptor_stringbuffer *sb=NULL;
  void *str=NULL;
  raptor_www_write_bytes_handler saved_write_bytes;
  void *saved_write_bytes_userdata;
  
  sb=raptor_new_stringbuffer();
  if(!sb)
    return 1;

  if(length_p)
    *length_p=0;

  saved_write_bytes=www->write_bytes;
  saved_write_bytes_userdata=www->write_bytes_userdata;
  raptor_www_set_write_bytes_handler(www, raptor_www_fetch_to_string_write_bytes, sb);

  if(raptor_www_fetch(www, uri))
    str=NULL;
  else {
    size_t len=raptor_stringbuffer_length(sb);
    if(len) {
      str=(void*)malloc_handler(len+1);
      if(str) {
        raptor_stringbuffer_copy_to_string(sb, (unsigned char*)str, len+1);
        *string_p=str;
        if(length_p)
          *length_p=len;
      }
    }
  }

  if(sb)
    raptor_free_stringbuffer(sb);

  raptor_www_set_write_bytes_handler(www, saved_write_bytes, saved_write_bytes_userdata);

  return (str == NULL);
}


/**
 * raptor_www_get_final_uri:
 * @www: #raptor_www object 
 *
 * Get the WWW final resolved URI.
 * 
 * This returns the URI used after any protocol redirection.
 *
 * Return value: a new URI or NULL if not known.
 **/
raptor_uri*
raptor_www_get_final_uri(raptor_www* www) 
{
  return www->final_uri ? raptor_uri_copy(www->final_uri) : NULL;
}


/**
 * raptor_www_set_final_uri_handler:
 * @www: WWW object
 * @handler: content type handler function
 * @user_data: content type handler data
 * 
 * Set the handler to receive the HTTP Content-Type header value.
 *
 * This is called if or when the value is discovered during retrieval
 * by the raptor_www implementation.  Not all implementations provide
 * access to this.
 **/
void
raptor_www_set_final_uri_handler(raptor_www* www, 
                                 raptor_www_final_uri_handler handler, 
                                 void *user_data)
{
  www->final_uri_handler=handler;
  www->final_uri_userdata=user_data;
}
