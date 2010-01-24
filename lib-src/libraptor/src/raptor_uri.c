/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_uri.c - Raptor URI resolving implementation
 *
 * Copyright (C) 2002-2006, David Beckett http://purl.org/net/dajobe/
 * Copyright (C) 2002-2005, University of Bristol, UK http://www.bristol.ac.uk/
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
#include <ctype.h>
#include <stdarg.h>
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif

/* Raptor includes */
#include "raptor.h"
#include "raptor_internal.h"


#ifdef WIN32_URI_TEST
#define WIN32
#endif


static const raptor_uri_handler *raptor_uri_current_uri_handler;
static void *raptor_uri_current_uri_context;

/* Symbian OS uses similar path mappings as Windows but does not necessarily have the WIN32 flag defined */
#if defined(__SYMBIAN32__) && !defined(WIN32)
#define WIN32
#endif

/**
 * raptor_uri_set_handler:
 * @handler: URI handler structure
 * @context: URI handler context
 * 
 * Change the URI class implementation to the functions provided by the
 *
 * The URI interface in @handler->initialised should be either 1
 * or 2 (if raptor_uri_compare_func is implemented).
 **/
void
raptor_uri_set_handler(const raptor_uri_handler *handler, void *context) 
{
  RAPTOR_ASSERT_OBJECT_POINTER_RETURN(handler, raptor_uri_handler);
  /* RAPTOR_ASSERT is the negative of ordinary asserts - it fails if the condition is true */
  RAPTOR_ASSERT(!(handler->initialised >= 1 && handler->initialised <= 2),
    "raptor_uri_handler->initialised not 1..2");

  raptor_uri_current_uri_handler=handler;
  raptor_uri_current_uri_context=context;
}

/**
 * raptor_uri_get_handler:
 * @handler: URI handler to return
 * @context: URI context to return
 * 
 * Return the current raptor URI class implementation @handler and @context
 **/
void
raptor_uri_get_handler(const raptor_uri_handler **handler, void **context) 
{
  if(handler)
    *handler=raptor_uri_current_uri_handler;
  if(context)
    *context=raptor_uri_current_uri_context;
}


static raptor_uri*
raptor_default_new_uri(void *context, const unsigned char *uri_string) 
{
  unsigned char *p;
  size_t len;
  
  /* If uri_string is "file:path-to-file", turn it into a correct file:URI */
  if(raptor_uri_uri_string_is_file_uri(uri_string)) {
    unsigned char *fragment=NULL;
    char *filename;
    raptor_uri* uri=NULL;

    filename=raptor_uri_uri_string_to_filename_fragment(uri_string, &fragment);
    if(filename && !access(filename, R_OK)) {
      uri=(raptor_uri*)raptor_uri_filename_to_uri_string(filename);
      /* If there was a fragment, reattach it to the new URI */
      if(fragment) {
        unsigned char *new_fragment;
        raptor_uri* new_uri;

        new_fragment=(unsigned char*)RAPTOR_MALLOC(cstring, strlen((const char*)fragment) + 1 + sizeof(char*));
        if(!new_fragment)
          return NULL;
        *new_fragment='#';
        strcpy((char*)new_fragment+1, (const char*)fragment);
        new_uri=raptor_new_uri_relative_to_base(uri, new_fragment);
        RAPTOR_FREE(cstring, new_fragment);
        raptor_free_uri(uri);
        uri=new_uri;
      }
    }
    if(filename)
      RAPTOR_FREE(cstring, filename);
    if(fragment)
      RAPTOR_FREE(cstring, fragment);
    if(uri)
      return uri;
  }

  len=strlen((const char*)uri_string);
  p=(unsigned char*)RAPTOR_MALLOC(raptor_uri, len + sizeof(char*));
  if(!p)
    return NULL;
  strcpy((char*)p, (const char*)uri_string);
  return (raptor_uri*)p;
}


/**
 * raptor_new_uri:
 * @uri_string: URI string.
 * 
 * Constructor - create a raptor URI from a UTF-8 encoded Unicode string.
 * 
 * Return value: a new #raptor_uri object or NULL on failure.
 **/
raptor_uri*
raptor_new_uri(const unsigned char *uri_string) 
{
  if(!uri_string || !*uri_string)
    return NULL;
  
  return (*raptor_uri_current_uri_handler->new_uri)(raptor_uri_current_uri_context, uri_string);
}


static raptor_uri*
raptor_default_new_uri_from_uri_local_name(void *context,
                                           raptor_uri *uri,
                                           const unsigned char *local_name)
{
  int uri_length=strlen((char*)uri);
  unsigned char *p=(unsigned char*)RAPTOR_MALLOC(cstring, 
                                                 uri_length + strlen((const char*)local_name) + sizeof(char*));
  if(!p)
    return NULL;
  
  strcpy((char*)p, (const char*)uri);
  strcpy((char*)p + uri_length, (const char*)local_name);
  
  return (raptor_uri*)p;
}


/**
 * raptor_new_uri_from_uri_local_name:
 * @uri: existing #raptor_uri
 * @local_name: local name
 * 
 * Constructor - create a raptor URI from an existing URI and a local name.
 *
 * Creates a new URI from the concatenation of the @local_name to the
 * @uri.  This is NOT relative URI resolution, which is done by the
 * raptor_new_uri_relative_to_base() constructor.
 * 
 * Return value: a new #raptor_uri object or NULL on failure.
 **/
raptor_uri*
raptor_new_uri_from_uri_local_name(raptor_uri *uri, const unsigned char *local_name)
{
  if(!uri || !local_name)
    return NULL;
  
  return (*raptor_uri_current_uri_handler->new_uri_from_uri_local_name)(raptor_uri_current_uri_context, uri, local_name);
}


static raptor_uri*
raptor_default_new_uri_relative_to_base(void *context,
                                        raptor_uri *base_uri,
                                        const unsigned char *uri_string) 
{
  raptor_uri* new_uri;
  size_t new_uri_len=strlen((const char*)base_uri)+strlen((const char*)uri_string) + sizeof(char*);

  /* +2 is for \0 plus an extra 1 for adding any missing URI path '/' */
  new_uri=(raptor_uri*)RAPTOR_MALLOC(cstring, new_uri_len+2);
  if(!new_uri)
    return NULL;
  
  /* If URI string is empty, just copy base URI */
  if(!*uri_string) {
    strcpy((char*)new_uri, (char*)base_uri);
    return new_uri;
  }

  raptor_uri_resolve_uri_reference((const unsigned char*)base_uri, uri_string,
                                   (unsigned char*)new_uri, new_uri_len);
  return new_uri;
}


/**
 * raptor_new_uri_relative_to_base:
 * @base_uri: existing base URI
 * @uri_string: relative URI string
 * 
 * Constructor - create a raptor URI from a base URI and a relative URI string.
 * 
 * Return value: a new #raptor_uri object or NULL on failure.
 **/
raptor_uri*
raptor_new_uri_relative_to_base(raptor_uri *base_uri, 
                                const unsigned char *uri_string) 
{
  if(!base_uri || !uri_string)
    return NULL;

  return (*raptor_uri_current_uri_handler->new_uri_relative_to_base)(raptor_uri_current_uri_context, base_uri, uri_string);
}


/**
 * raptor_new_uri_from_id:
 * @base_uri: existing base URI
 * @id: RDF ID
 * 
 * Constructor - create a new URI from a base URI and RDF ID.
 *
 * This creates a URI equivalent to concatenating @base_uri with
 * ## and @id.
 * 
 * Return value: a new #raptor_uri object or NULL on failure.
 **/
raptor_uri*
raptor_new_uri_from_id(raptor_uri *base_uri, const unsigned char *id) 
{
  raptor_uri *new_uri;
  unsigned char *local_name;
  int len;

  if(!base_uri || !id)
    return NULL;

#if defined(RAPTOR_DEBUG) && RAPTOR_DEBUG > 1
  RAPTOR_DEBUG2("Using ID %s\n", id);
#endif

  /* "#id\0" */
  len=1+strlen((char*)id) + sizeof(char*);
  local_name=(unsigned char*)RAPTOR_MALLOC(cstring, len);
  if(!local_name)
    return NULL;
  *local_name='#';
  strcpy((char*)local_name+1, (char*)id);
  new_uri=raptor_new_uri_relative_to_base(base_uri, local_name);
  RAPTOR_FREE(cstring, local_name);
  return new_uri;
}


static raptor_uri*
raptor_default_new_uri_for_rdf_concept(void *context, const char *name) 
{
  raptor_uri *new_uri;
  const unsigned char *base_uri=raptor_rdf_namespace_uri;
  unsigned int base_uri_len=raptor_rdf_namespace_uri_len;
  unsigned int new_uri_len;

  new_uri_len=base_uri_len+strlen(name) + sizeof(char*);
  new_uri=(raptor_uri*)RAPTOR_MALLOC(cstring, new_uri_len);
  if(!new_uri)
    return NULL;
  strcpy((char*)new_uri, (const char*)base_uri);
  strcpy((char*)new_uri+base_uri_len, name);
  return new_uri;
}


/**
 * raptor_new_uri_for_rdf_concept:
 * @name: RDF namespace concept
 * 
 * Constructor - create a raptor URI for the RDF namespace concept name.
 *
 * Example: u=raptor_new_uri_for_rdf_concept("value") creates a new
 * URI for the rdf:value term.
 * 
 * Return value: a new #raptor_uri object or NULL on failure
 **/
raptor_uri*
raptor_new_uri_for_rdf_concept(const char *name) 
{
  if(!name)
    return NULL;
  
  return (*raptor_uri_current_uri_handler->new_uri_for_rdf_concept)(raptor_uri_current_uri_context, name);
}


static void
raptor_default_free_uri(void *context, raptor_uri *uri) 
{
  RAPTOR_FREE(raptor_uri, uri);
}


/**
 * raptor_free_uri:
 * @uri: URI to destroy
 * 
 * Destructor - destroy a #raptor_uri object
 **/
void
raptor_free_uri(raptor_uri *uri)
{
  (*raptor_uri_current_uri_handler->free_uri)(raptor_uri_current_uri_context, uri);
}


static int
raptor_default_uri_equals(void *context, raptor_uri* uri1, raptor_uri* uri2)
{
  return strcmp((char*)uri1, (char*)uri2)==0;
}


static int
raptor_default_uri_compare(void *context, raptor_uri* uri1, raptor_uri* uri2)
{
  return strcmp((char*)uri1, (char*)uri2);
}


/**
 * raptor_uri_equals:
 * @uri1: URI 1 (may be NULL)
 * @uri2: URI 2 (may be NULL)
 * 
 * Check if two URIs are equal.
 * 
 * A NULL URI is not equal to a non-NULL URI.
 *
 * Return value: non-0 if the URIs are equal
 **/
int
raptor_uri_equals(raptor_uri* uri1, raptor_uri* uri2)
{
  if(uri1 && uri2)
    /* Both not-NULL - check with handler */
    return (*raptor_uri_current_uri_handler->uri_equals)(raptor_uri_current_uri_context, uri1, uri2);
  else if(uri1 || uri2)
    /* Only one is NULL - not equal */
    return 0;
  else
    /* both NULL - equal */
    return 1;
}


/**
 * raptor_uri_compare:
 * @uri1: URI 1 (may be NULL)
 * @uri2: URI 2 (may be NULL)
 * 
 * Compare two URIs, ala strcmp.
 * 
 * A NULL URI is always less than (never equal to) a non-NULL URI.
 *
 * Return value: -1 if uri1 < uri2, 0 if equal, 1 if uri1 > uri2
 **/
int
raptor_uri_compare(raptor_uri* uri1, raptor_uri* uri2)
{
  if(uri1 && uri2) {
    /* string compare function is available in API V2 or newer */
    if(raptor_uri_current_uri_handler->initialised >= 2)
      return (*raptor_uri_current_uri_handler->uri_compare)(raptor_uri_current_uri_context, uri1, uri2);
    else
      return raptor_default_uri_compare(raptor_uri_current_uri_context, 
                                        uri1, uri2);
  } else if(uri1)
    /* uri1 > uri2 (NULL) */
    return 1;
  else
    /* uri1 (NULL) < uri2 */
    return -1;
}


static raptor_uri*
raptor_default_uri_copy(void *context, raptor_uri *uri)
{
  raptor_uri* new_uri=(raptor_uri*)RAPTOR_MALLOC(cstring, strlen((char*)uri) + sizeof(char*));
  if(!new_uri)
    return NULL;
  strcpy((char*)new_uri, (char*)uri);
  return new_uri;
}


/**
 * raptor_uri_copy:
 * @uri: URI object
 * 
 * Constructor - get a copy of a URI.
 * 
 *
 * Return value: a new #raptor_uri object or NULL on failure
 **/
raptor_uri*
raptor_uri_copy(raptor_uri *uri) 
{
  if(!uri)
    return NULL;
  
  return (*raptor_uri_current_uri_handler->uri_copy)(raptor_uri_current_uri_context, uri);
}


static unsigned char*
raptor_default_uri_as_string(void *context, raptor_uri *uri)
{
  return (unsigned char*)uri;
}


/**
 * raptor_uri_as_string:
 * @uri: #raptor_uri object
 * 
 * Get a string representation of a URI.
 *
 * Returns a shared pointer to a string representation of @uri.  This
 * string is shared and must not be freed, otherwise see use the
 * raptor_uri_to_string() or raptor_uri_to_counted_string() methods.
 * 
 * Return value: shared string representation of URI
 **/
unsigned char*
raptor_uri_as_string(raptor_uri *uri) 
{
  if(!uri)
    return NULL;
  
  return (*raptor_uri_current_uri_handler->uri_as_string)(raptor_uri_current_uri_context, uri);
}


static unsigned char*
raptor_default_uri_as_counted_string(void *context, raptor_uri *uri,
                                     size_t* len_p)
{
  if(len_p)
    *len_p=strlen((char*)uri);
  return (unsigned char*)uri;
}


/**
 * raptor_uri_as_counted_string:
 * @uri: URI object
 * @len_p: address of length variable or NULL
 * 
 * Get a string representation of a URI with count.
 *
 * Returns a shared pointer to a string representation of @uri along
 * with the length of the string in @len_p, if not NULL.  This
 * string is shared and must not be freed, otherwise see use the
 * raptor_uri_to_string() or raptor_uri_to_counted_string() methods.
 * 
 * Return value: shared string representation of URI
 **/
unsigned char*
raptor_uri_as_counted_string(raptor_uri *uri, size_t* len_p) 
{
  if(!uri)
    return NULL;
  
  return (*raptor_uri_current_uri_handler->uri_as_counted_string)(raptor_uri_current_uri_context, uri, len_p);
}



/**
 * raptor_uri_filename_to_uri_string:
 * @filename: The filename to convert
 *
 * Converts a filename to a file: URI.
 * 
 * Handles the OS-specific escaping on turning filenames into URIs
 * and returns a new buffer that the caller must free().  Turns
 * a space in the filname into %20 and '%' into %25.
 *
 * Return value: A newly allocated string with the URI or NULL on failure
 **/
unsigned char *
raptor_uri_filename_to_uri_string(const char *filename) 
{
  unsigned char *buffer=NULL;
  const char *from;
  char *to;
#ifndef WIN32
  char *path=NULL;
#endif
  /*     "file://" ... \0 */
  size_t len=7 + sizeof(char*);
  
  if(!filename)
    return NULL;
  
#ifdef WIN32
/*
 * On WIN32, filenames turn into
 *   "file://" + translated filename
 * where the translation is \\ turns into / and ' ' into %20, '%' into %25
 * and if the filename does not start with '\', it is relative
 * in which case, a . is appended to the authority
 *
 * e.g
 *  FILENAME              URI
 *  c:\windows\system     file:///c:/windows/system
 *  \\server\dir\file.doc file://server/dir/file.doc
 *  a:foo                 file:///a:./foo
 *  C:\Documents and Settings\myapp\foo.bat
 *                        file:///C:/Documents%20and%20Settings/myapp/foo.bat
 *
 * There are also UNC names \\server\share\blah
 * that turn into file:///server/share/blah
 * using the above algorithm.
 */
  if(filename[1] == ':' && filename[2] != '\\')
    len+=3; /* relative filename - add / and ./ */
  else if(*filename == '\\')
    len-=2; /* two // from not needed in filename */
  else
    len++; /* / at start of path */

#else
/* others - unix: turn spaces into %20, '%' into %25 */

  if(*filename != '/') {
    size_t path_max;
#ifdef PATH_MAX
    path_max=PATH_MAX;
#else
    path_max=1024; /* an initial guess at the length */
#endif
    path=(char*)malloc(path_max);
    while(1) {
      /* malloc() failed or getcwd() succeeded */
      if(!path || getcwd(path, path_max))
        break;

      /* failed */
      if(errno != ERANGE)
        break;

      /* try again with a bigger buffer */
      path_max *= 2;
      path=(char*)realloc(path, path_max);
    }
    if(!path)
      goto path_done;

    strcat(path, "/");
    strcat(path, filename);
    filename=(const char*)path;
  }
#endif

  /* add URI-escaped filename length */
  for(from=filename; *from ; from++) {
    len++;
    if(*from == ' ' || *from == '%')
      len+=2; /* strlen(%xx)-1 */
  }

  buffer=(unsigned char*)RAPTOR_MALLOC(cstring, len);
  if(!buffer)
    goto path_done;

  strcpy((char*)buffer, "file://");
  from=filename;
  to=(char*)(buffer+7);
#ifdef WIN32
  if(*from == '\\' && from[1] == '\\')
    from+=2;
  else
    *to++ ='/';
#endif
  while(*from) {
    char c=*from++;
#ifdef WIN32
    if (c == '\\')
      *to++ ='/';
    else if(c == ':') {
      *to++ = c;
      if(*from != '\\') {
        *to++ ='.';
        *to++ ='/';
      }
    } else
#endif
    if(c == ' ' || c == '%') {
      *to++ ='%';
      *to++ ='2';
      *to++ =(c == ' ') ? '0' : '5';
    } else
      *to++ =c;
  }
  *to='\0';

  path_done:
#ifndef WIN32
  if(path)
    free(path);
#endif
  
  return buffer;
}


/**
 * raptor_uri_uri_string_to_filename_fragment:
 * @uri_string: The file: URI to convert
 * @fragment_p: Address of pointer to store any URI fragment or NULL
 *
 * Convert a file: URI to a filename and fragment.
 * 
 * Handles the OS-specific file: URIs to filename mappings.  Returns
 * a new buffer containing the filename that the caller must free.
 *
 * If @fragment_p is given, a new string containing the URI fragment
 * is returned, or NULL if none is present
 * 
 * Return value: A newly allocated string with the filename or NULL on failure
 **/
char *
raptor_uri_uri_string_to_filename_fragment(const unsigned char *uri_string,
                                           unsigned char **fragment_p) 
{
  char *filename;
  size_t len=0;
  raptor_uri_detail *ud=NULL;
  unsigned char *from;
  char *to;
#ifdef WIN32
  unsigned char *p;
#endif

  if(!uri_string || !*uri_string)
    return NULL;
  
  ud=raptor_new_uri_detail(uri_string);
  if(!ud)
    return NULL;
  

  if(!ud->scheme || raptor_strcasecmp((const char*)ud->scheme, "file")) {
    raptor_free_uri_detail(ud);
    return NULL;
  }

  if(ud->authority) {
    if(!*ud->authority)
      ud->authority=NULL;
    else if(!raptor_strcasecmp((const char*)ud->authority, "localhost"))
      ud->authority=NULL;
  }

  /* Cannot do much if there is no path */
  if(!ud->path || (ud->path && !*ud->path)) {
    raptor_free_uri_detail(ud);
    return NULL;
  }

  /* See raptor_uri_filename_to_uri_string for details of the mapping */
#ifdef WIN32
  if(ud->authority)
    len+=ud->authority_len+3;

  p=ud->path;
  /* remove leading slash from path if there is one */
  if(*p && p[0] == '/') {
	  p++;
	  len--;
  }
  /* handle case where path starts with drive letter */
  if(*p && (p[1] == '|' || p[1] == ':')) {
    /* Either 
     *   "a:" like in file://a|/... or file://a:/... 
     * or
     *   "a:." like in file://a:./foo
     * giving device-relative path a:foo
     */
    if(p[2]=='.') {
      p[2]=*p;
      p[3]=':';
      p+= 2;
      len-= 2; /* remove 2 for ./ */
    } else
      p[1]=':';
  }
#endif


  /* add URI-escaped filename length */
  for(from=ud->path; *from ; from++) {
    len++;
    if(*from == '%')
      from+= 2;
  }


  /* Something is wrong */
  if(!len) {
    raptor_free_uri_detail(ud);
    return NULL;
  }
    
  filename=(char*)RAPTOR_MALLOC(cstring, len + sizeof(char*));
  if(!filename) {
    raptor_free_uri_detail(ud);
    return NULL;
  }


  to=filename;

#ifdef WIN32
  if(ud->authority) {
    *to++ = '\\';
    *to++ = '\\';
    from=ud->authority;
    while( (*to++ = *from++) )
      ;
    to--;
    *to++ = '\\';
  }
  
  /* copy path after all /s */
  from=p;
#else
  from=ud->path;
#endif

  while(*from) {
    char c=*from++;
#ifdef WIN32
    if(c == '/')
      *to++ ='\\';
    else
#endif
    if(c == '%') {
      if(*from && from[1]) {
        char hexbuf[3];
        char *endptr=NULL;
        hexbuf[0]=(char)*from;
        hexbuf[1]=(char)from[1];
        hexbuf[2]='\0';
        c=(char)strtol((const char*)hexbuf, &endptr, 16);
        if(endptr == &hexbuf[2])
          *to++ = c;
      }
      from+= 2;
    } else
      *to++ =c;
  }
  *to='\0';

  if(fragment_p) {
    if(ud->fragment) {
      len=ud->fragment_len;
      *fragment_p=(unsigned char*)RAPTOR_MALLOC(cstring, len + sizeof(char*));
      if(*fragment_p)
        strncpy((char*)*fragment_p, (const char*)ud->fragment, len+1);
    } else
      *fragment_p=NULL;
  }

  raptor_free_uri_detail(ud);

  return filename;
}


/**
 * raptor_uri_uri_string_to_filename:
 * @uri_string: The file: URI to convert
 *
 * Convert a file: URI to a filename.
 * 
 * Handles the OS-specific file: URIs to filename mappings.  Returns
 * a new buffer containing the filename that the caller must free.
 *
 * Return value: A newly allocated string with the filename or NULL on failure
 **/
char *
raptor_uri_uri_string_to_filename(const unsigned char *uri_string) 
{
  return raptor_uri_uri_string_to_filename_fragment(uri_string, NULL);
}


/**
 * raptor_uri_is_file_uri:
 * @uri_string: The URI string to check
 *
 * @Deprecated: use raptor_uri_uri_string_is_file_uri
 *
 * Check if a URI string is a file: URI.
 *
 * Return value: Non zero if URI string is a file: URI
 **/
int
raptor_uri_is_file_uri(const unsigned char* uri_string) {
  return raptor_uri_uri_string_is_file_uri(uri_string);
}


/**
 * raptor_uri_uri_string_is_file_uri:
 * @uri_string: The URI string to check
 *
 * Check if a URI string is a file: URI.
 * 
 * Return value: Non zero if URI string is a file: URI
 **/
int
raptor_uri_uri_string_is_file_uri(const unsigned char* uri_string) {
  if(!uri_string || !*uri_string)
    return 1;

  return raptor_strncasecmp((const char*)uri_string, "file:", 5)==0;
}


/**
 * raptor_new_uri_for_xmlbase:
 * @old_uri: URI to transform
 *
 * Constructor - create a URI suitable for use as an XML Base.
 * 
 * Takes an existing URI and ensures it has a path (default /) and has
 * no fragment or query arguments - XML base does not use these.
 * 
 * Return value: new #raptor_uri object or NULL on failure.
 **/
raptor_uri*
raptor_new_uri_for_xmlbase(raptor_uri* old_uri)
{
  unsigned char *uri_string;
  unsigned char *new_uri_string;
  raptor_uri* new_uri;
  raptor_uri_detail *ud;
  
  if(!old_uri)
    return NULL;

  uri_string=raptor_uri_as_string(old_uri);

  ud=raptor_new_uri_detail(uri_string);
  if(!ud)
    return NULL;

  if(!ud->path) {
    ud->path=(unsigned char*)"/";
    ud->path_len=1;
  }
  
  ud->query=NULL; ud->query_len=0;
  ud->fragment=NULL; ud->fragment_len=0;
  new_uri_string=raptor_uri_detail_to_string(ud, NULL);
  raptor_free_uri_detail(ud);
  if(!new_uri_string)
    return NULL;
  
  new_uri=raptor_new_uri(new_uri_string);
  RAPTOR_FREE(cstring, new_uri_string);

  return new_uri;
}


/**
 * raptor_new_uri_for_retrieval:
 * @old_uri: URI to transform
 *
 * Constructor - create a URI suitable for retrieval.
 * 
 * Takes an existing URI and ensures it has a path (default /) and has
 * no fragment - URI retrieval does not use the fragment part.
 * 
 * Return value: new #raptor_uri object or NULL on failure.
 **/
raptor_uri*
raptor_new_uri_for_retrieval(raptor_uri* old_uri)
{
  unsigned char *uri_string;
  unsigned char *new_uri_string;
  raptor_uri* new_uri;
  raptor_uri_detail *ud;
  
  if(!old_uri)
    return NULL;

  uri_string=raptor_uri_as_string(old_uri);

  ud=raptor_new_uri_detail(uri_string);
  if(!ud)
    return NULL;

  if(!ud->path) {
    ud->path=(unsigned char*)"/";
    ud->path_len=1;
  }

  ud->fragment=NULL; ud->fragment_len=0;
  new_uri_string=raptor_uri_detail_to_string(ud, NULL);
  raptor_free_uri_detail(ud);
  if(!new_uri_string)
    return NULL;
  
  new_uri=raptor_new_uri(new_uri_string);
  RAPTOR_FREE(cstring, new_uri_string);

  return new_uri;
}


static const raptor_uri_handler raptor_uri_default_handler = {
  raptor_default_new_uri,
  raptor_default_new_uri_from_uri_local_name,
  raptor_default_new_uri_relative_to_base,
  raptor_default_new_uri_for_rdf_concept,
  raptor_default_free_uri,
  raptor_default_uri_equals,
  raptor_default_uri_copy,
  raptor_default_uri_as_string,
  raptor_default_uri_as_counted_string,
  2, /* URI Interface Version */
  raptor_default_uri_compare /* URI Interface V2 */
};


int
raptor_uri_init(void)
{
  raptor_uri_set_handler(&raptor_uri_default_handler, NULL);
  return 0;
}


/*
 * raptor_uri_path_common_base_length:
 * @first_path: The first path (path only, not a full URI)
 * @first_path_len: Length of first_path
 * @second_path: The second path (path only, not a full URI)
 * @second_path_len: Length of second_path
 *
 * Find the common base length of two URI path components.
 * 
 * Return value: Length of the common base path
 **/

static int 
raptor_uri_path_common_base_length(const unsigned char *first_path, size_t first_path_len,
                                   const unsigned char *second_path, size_t second_path_len)
{
  int common_len=0;
  const unsigned char *cur_ptr=first_path;
  const unsigned char *prev_ptr=first_path;
  
  /* Compare each path component of first_path and second_path until there is
     a mismatch. Then return the length from the start of the path to the last
     successful match. */
  while((cur_ptr=(const unsigned char*)memchr(cur_ptr, '/', first_path_len))) {
    cur_ptr++;
    if(strncmp((const char*)first_path+common_len,
               (const char*)second_path+common_len, cur_ptr-prev_ptr))
      break;

    first_path_len -= cur_ptr - prev_ptr;
    prev_ptr=cur_ptr;
    common_len = prev_ptr - first_path;
  }
  return prev_ptr - first_path;
}


/*
 * raptor_uri_path_make_relative_path:
 * @from_path: The base path (path only, not a full URI)
 * @from_path_len: Length of the base path
 * @to_path: The reference path (path only, not a full URI)
 * @to_path_len: Length of the reference path
 * @suffix: String to be appended to the final relative path
 * @suffix_len: Length of the suffix
 * @result_length_p: Location to store the length of the string or NULL
 *
 * Make a relative URI path.
 *
 * Return value: A newly allocated relative path string or NULL on failure.
 **/

static unsigned char *
raptor_uri_path_make_relative_path(const unsigned char *from_path, size_t from_path_len,
                                   const unsigned char *to_path, size_t to_path_len,
                                   const unsigned char *suffix, size_t suffix_len,
                                   size_t *result_length_p)
{
  int common_len, cur_len, final_len, up_dirs = 0, to_dir_len = 0;
  const unsigned char *cur_ptr, *prev_ptr;
  unsigned char *final_path, *final_path_cur;

  common_len=raptor_uri_path_common_base_length(from_path, from_path_len,
                                                to_path, to_path_len);
  
  if(result_length_p)
    *result_length_p=0;

  /* Count how many directories we have to go up */
  cur_ptr = from_path + common_len;
  prev_ptr=cur_ptr;
  cur_len = from_path_len - common_len;
  while((cur_ptr = (const unsigned char*)memchr(cur_ptr, '/', cur_len))) {
    cur_ptr++;
    up_dirs++;
    cur_len -= cur_ptr - prev_ptr;
    prev_ptr=cur_ptr;
  }
  
  /* Calculate how many characters of to_path subdirs (counted from the 
     common base) we have to add. */
  cur_ptr = to_path + common_len;
  prev_ptr=cur_ptr;
  cur_len = to_path_len - common_len;
  while((cur_ptr = (const unsigned char*)memchr(cur_ptr, '/', cur_len))) {
    cur_ptr++;
    cur_len -= cur_ptr - prev_ptr;
    prev_ptr=cur_ptr;
  }
  to_dir_len = prev_ptr - (to_path + common_len);
  
  /* Create the final relative path */
  final_len = up_dirs*3 + to_dir_len + suffix_len; /* 3 for each "../" */
  final_path=(unsigned char*)RAPTOR_MALLOC(cstring, final_len + sizeof(char*));
  if(!final_path)
    return NULL;
  *final_path=0;
  
  /* First, add the necessary "../" parts */
  final_path_cur=final_path;
  while (up_dirs--) {
    *final_path_cur++='.';
    *final_path_cur++='.';
    *final_path_cur++='/';
  }
  
  /* Then, add the path from the common base to the to_path */
  memcpy(final_path_cur, to_path + common_len, to_dir_len);
  final_path_cur+=to_dir_len;
  
  /* Finally, add the suffix */
  if(suffix && suffix_len) {
    /* As a special case, if the suffix begins with a dot (".") and the final 
       output string so far is non-empty, skip the dot. */
    if (*suffix == '.' && final_path_cur != final_path) {
      /* Make sure that the dot really represents a directory and it's not
         just part of a file name like ".foo". In other words, the dot must
         either be the only character or the next character must be the 
         fragment or the query character. */
      if ((suffix_len == 1) || 
          (suffix_len > 1 && (suffix[1] == '#' || suffix[1] == '?'))) {
        suffix++;
        suffix_len--;
        final_len--;
      }
    }
    if(suffix_len)
      memcpy(final_path_cur, suffix, suffix_len);
  }
  
  final_path[final_len]=0;
  
  if (result_length_p)
    *result_length_p=final_len;
  
  return final_path;
}


/**
 * raptor_uri_to_relative_counted_uri_string:
 * @base_uri: The base absolute URI to resolve against (or NULL)
 * @reference_uri: The reference absolute URI to use
 * @length_p: Location to store the length of the relative URI string or NULL
 *
 * Get the counted relative URI string of a URI against a base URI.
 * 
 * Return value: A newly allocated relative URI string or NULL on failure
 **/

unsigned char*
raptor_uri_to_relative_counted_uri_string(raptor_uri *base_uri, 
                                          raptor_uri *reference_uri,
                                          size_t *length_p) {
  raptor_uri_detail *base_detail=NULL, *reference_detail;
  const unsigned char *base, *reference_str, *base_file, *reference_file;
  unsigned char *suffix, *cur_ptr;
  size_t base_len, reference_len, reference_file_len, suffix_len;
  unsigned char *result=NULL;

  if(!reference_uri)
    return NULL;
    
  if(length_p)
    *length_p=0;

  reference_str=raptor_uri_as_counted_string(reference_uri, &reference_len);
  reference_detail=raptor_new_uri_detail(reference_str);
  if(!reference_detail)
    goto err;
  
  if(!base_uri)
    goto buildresult;
  
  base=raptor_uri_as_counted_string(base_uri, &base_len);
  base_detail=raptor_new_uri_detail(base);
  if(!base_detail)
    goto err;
  
  /* Check if the whole URIs are equal */
  if(raptor_uri_equals(base_uri, reference_uri)) {
    reference_len=0;
    goto buildresult;
  }
  
  /* Check if scheme and authority of the URIs are equal */
  if(base_detail->scheme_len == reference_detail->scheme_len &&
     base_detail->authority_len == reference_detail->authority_len &&
     !strncmp((const char*)base_detail->scheme, 
              (const char*)reference_detail->scheme,
              base_detail->scheme_len) &&
     !strncmp((const char*)base_detail->authority, 
              (const char*)reference_detail->authority,
              base_detail->authority_len)) {
    
    if(!base_detail->path)
      goto buildresult;
    
    /* Find the file name components */
    base_file = (const unsigned char*)strrchr((const char*)base_detail->path, '/');
    if(!base_file)
      goto buildresult;
    base_file++;

    if(!reference_detail->path)
      goto buildresult;
    reference_file=(const unsigned char*)strrchr((const char*)reference_detail->path, '/');
    if(!reference_file)
      goto buildresult;
    reference_file++;
    
    reference_file_len=reference_detail->path_len -
                       (reference_file - reference_detail->path);
    
    if(!strcmp((const char*)base_file, (const char*)reference_file)) {
      /* If the file names are equal, don't put them in the relative URI */
      reference_file=NULL;
      reference_file_len=0;
    } else if(*base_file && !*reference_file) {
      /* If the base file is non-empty, but the reference file is
       * empty, use "."  as the file name.
       */
      reference_file=(const unsigned char*)".";
      reference_file_len=1;
    }
    
    /* Calculate the length of the suffix (file name + query + fragment) */
    suffix_len=reference_file_len + reference_detail->query_len + 
               reference_detail->fragment_len;
    
    if (reference_detail->query)
      suffix_len++; /* add one char for the '?' */
    if (reference_detail->fragment)
      suffix_len++; /* add one char for the '#' */
    
    /* Assemble the suffix */
    suffix=(unsigned char*)RAPTOR_MALLOC(cstring, suffix_len + sizeof(char*));
    if(!suffix)
      goto err;
    cur_ptr=suffix;
    if(reference_file) {
      memcpy(suffix, reference_file, reference_file_len);
      cur_ptr+= reference_file_len;
    }

    if(reference_detail->query) {
      *cur_ptr++='?';
      memcpy(cur_ptr, reference_detail->query, reference_detail->query_len);
      cur_ptr+= reference_detail->query_len;
    }

    if(reference_detail->fragment) {
      *cur_ptr++='#';
      memcpy(cur_ptr, reference_detail->fragment, reference_detail->fragment_len);
      cur_ptr+= reference_detail->fragment_len;
    }
    *cur_ptr=0;
    
    /* Finally, create the full relative path */
    result = raptor_uri_path_make_relative_path(base_detail->path,
                                                base_detail->path_len,
                                                reference_detail->path,
                                                reference_detail->path_len,
                                                suffix,
                                                suffix_len,
                                                length_p);
    RAPTOR_FREE(cstring, suffix);
  }

  
 buildresult:
  /* If result is NULL at this point, it means that we were unable to find a
     relative URI, so we'll return a full absolute URI instead. */
  if(!result) {
    result=(unsigned char*)RAPTOR_MALLOC(cstring, reference_len + sizeof(char*));
    if(result) {
      if(reference_len)
        memcpy(result, reference_str, reference_len);
      result[reference_len] = 0;
      if(length_p)
        *length_p=reference_len;
    }
  }
  
  err:
  if(base_detail)
    raptor_free_uri_detail(base_detail);
  raptor_free_uri_detail(reference_detail);
  
  return result;
}


/**
 * raptor_uri_to_relative_uri_string:
 * @base_uri: The base absolute URI to resolve against
 * @reference_uri: The reference absolute URI to use
 *
 * Get the relative URI string of a URI against a base URI.
 * 
 * Return value: A newly allocated relative URI string or NULL on failure
 **/
unsigned char*
raptor_uri_to_relative_uri_string(raptor_uri *base_uri, 
                                  raptor_uri *reference_uri) {
  return raptor_uri_to_relative_counted_uri_string(base_uri,
                                                   reference_uri, NULL);
}


/**
 * raptor_uri_print:
 * @uri: URI to print
 * @stream: The file handle to print to
 *
 * Print a URI to a file handle.
 *
 **/
void
raptor_uri_print(const raptor_uri* uri, FILE *stream) {
  if(uri) {
    size_t len;
    unsigned char *string=raptor_uri_as_counted_string((raptor_uri*)uri, &len);
    (void)fwrite(string, len, 1, stream);
  } else 
    (void)fwrite("(NULL URI)", 10, 1, stream);
}


/**
 * raptor_uri_to_counted_string:
 * @uri: #raptor_uri object
 * @len_p: Pointer to length (or NULL)
 *
 * Get a new counted string for a URI.
 *
 * If @len_p is not NULL, the length of the string is stored in it.
 *
 * The memory allocated must be freed by the caller and
 * raptor_free_memory() should be used for best portability.
 *
 * Return value: new string or NULL on failure
 **/
unsigned char*
raptor_uri_to_counted_string(raptor_uri *uri, size_t *len_p)
{
  size_t len;
  unsigned char *string;
  unsigned char *new_string;

  if(!uri)
    return NULL;
  
  string=raptor_uri_as_counted_string(uri, &len);
  if(!string)
    return NULL;
  
  new_string=(unsigned char*)RAPTOR_MALLOC(cstring, len + 1); /* +1 for NUL termination */
  if(!new_string)
    return NULL;
  
  memcpy(new_string, string, len+1);

  if(len_p)
    *len_p=len;
  return new_string;
}


/**
 * raptor_uri_to_string:
 * @uri: #raptor_uri object
 *
 * Get a new string for a URI.
 *
 * The memory allocated must be freed by the caller and
 * raptor_free_memory() should be used for best portability.
 *
 * Return value: new string or NULL on failure
 **/
unsigned char*
raptor_uri_to_string(raptor_uri *uri)
{
  return raptor_uri_to_counted_string(uri, NULL);
}


/**
 * raptor_new_uri_from_rdf_ordinal:
 * @ordinal: integer rdf:_n
 * 
 * Internal - convert an integer rdf:_n ordinal to the resource URI
 * 
 * Return value: new URI object or NULL on failure
 **/
raptor_uri*
raptor_new_uri_from_rdf_ordinal(int ordinal)
{
  /* 55 = strlen(rdf namespace URI) + _ + 10-digit number + \0 */
  unsigned char uri_string[55];
  strncpy((char*)uri_string, (const char*)raptor_rdf_namespace_uri, 
          raptor_rdf_namespace_uri_len);
  sprintf((char*)uri_string+raptor_rdf_namespace_uri_len, "_%d",
          ordinal);
  return raptor_new_uri(uri_string);
}


#ifdef STANDALONE

#include <stdio.h>
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

/* one more prototype */
int main(int argc, char *argv[]);

static const char *program;


static int
assert_filename_to_uri (const char *filename, const char *reference_uri)
{
  unsigned char *uri;

  uri=raptor_uri_filename_to_uri_string(filename);

  if (!uri || strcmp((const char*)uri, (const char*)reference_uri)) {
    fprintf(stderr, 
            "%s: raptor_uri_filename_to_uri_string(%s) FAILED gaving URI %s != %s\n",
            program, filename, uri, reference_uri);
    if(uri)
      RAPTOR_FREE(cstring, uri);
    return 1;
  }

  RAPTOR_FREE(cstring, uri);
  return 0;
}


static int
assert_uri_to_filename (const char *uri, const char *reference_filename)
{
  char *filename;

  filename=raptor_uri_uri_string_to_filename((const unsigned char*)uri);

  if(filename && !reference_filename) {
    fprintf(stderr, 
            "%s: raptor_uri_uri_string_to_filename(%s) FAILED giving filename %s != NULL\n", 
            program, uri, filename);
    if(filename)
      RAPTOR_FREE(cstring, filename);
    return 1;
  } else if (filename && strcmp(filename, reference_filename)) {
    fprintf(stderr,
            "%s: raptor_uri_uri_string_to_filename(%s) FAILED gaving filename %s != %s\n",
            program, uri, filename, reference_filename);
    if(filename)
      RAPTOR_FREE(cstring, filename);
    return 1;
  }

  RAPTOR_FREE(cstring, filename);
  return 0;
}


static int
assert_uri_to_relative(const char *base, const char *uri, const char *relative)
{
  unsigned char *output;
  int result;
  raptor_uri* base_uri=NULL;
  raptor_uri* reference_uri=raptor_new_uri((const unsigned char*)uri);
  size_t length=0;

  if(base)
    base_uri=raptor_new_uri((const unsigned char*)base);
  
  output=raptor_uri_to_relative_counted_uri_string(base_uri, reference_uri, 
                                                   &length);
  result=strcmp(relative, (const char*)output);
  if (result) {
    fprintf(stderr,
            "%s: raptor_uri_string_to_relative_uri_string FAILED: base='%s', uri='%s', expected='%s', got='%s'\n",
            program, base, uri, relative, output);
    RAPTOR_FREE(cstring, output);
    return 1;
  }
  RAPTOR_FREE(cstring, output);
  raptor_free_uri(base_uri);
  raptor_free_uri(reference_uri);
  return 0;
}


static int
raptor_test_uri_compare(void *context, raptor_uri* uri1, raptor_uri* uri2)
{
  int* called_p=(int*)context;
  *called_p=1;
  return strcmp((char*)uri1, (char*)uri2);
}


int
main(int argc, char *argv[]) 
{
  const char *base_uri = "http://example.org/bpath/cpath/d;p?querystr#frag";
  const char *base_uri_xmlbase = "http://example.org/bpath/cpath/d;p";
  const char *base_uri_retrievable = "http://example.org/bpath/cpath/d;p?querystr";
#ifndef WIN32
#if defined(HAVE_UNISTD_H) && defined(HAVE_SYS_STAT_H)
  const char* dirs[6] = { "/etc", "/bin", "/tmp", "/lib", "/var", NULL };
  unsigned char uri_buffer[16]; /* strlen("file:///DIR/foo")+1 */  
  int i;
  const char *dir;
#endif
#endif
  unsigned char *str;
  raptor_uri *uri1, *uri2, *uri3;

  int failures=0;

  if((program=strrchr(argv[0], '/')))
    program++;
  else if((program=strrchr(argv[0], '\\')))
    program++;
  else
    program=argv[0];
  
#ifdef WIN32
  failures += assert_filename_to_uri ("c:\\windows\\system", "file:///c:/windows/system");
  failures += assert_filename_to_uri ("\\\\server\\share\\file.doc", "file://server/share/file.doc");
  failures += assert_filename_to_uri ("a:foo", "file:///a:./foo");

  failures += assert_filename_to_uri ("C:\\Documents and Settings\\myapp\\foo.bat", "file:///C:/Documents%20and%20Settings/myapp/foo.bat");
  failures += assert_filename_to_uri ("C:\\My Documents\\%age.txt", "file:///C:/My%20Documents/%25age.txt");

  failures += assert_uri_to_filename ("file:///c|/windows/system", "c:\\windows\\system");
  failures += assert_uri_to_filename ("file:///c:/windows/system", "c:\\windows\\system");
  failures += assert_uri_to_filename ("file://server/share/file.doc", "\\\\server\\share\\file.doc");
  failures += assert_uri_to_filename ("file:///a:./foo", "a:foo");
  failures += assert_uri_to_filename ("file:///C:/Documents%20and%20Settings/myapp/foo.bat", "C:\\Documents and Settings\\myapp\\foo.bat");
  failures += assert_uri_to_filename ("file:///C:/My%20Documents/%25age.txt", "C:\\My Documents\\%age.txt");


  failures += assert_uri_to_filename ("file:c:\\thing",     "c:\\thing");
  failures += assert_uri_to_filename ("file:/c:\\thing",    "c:\\thing");
  failures += assert_uri_to_filename ("file://c:\\thing",   NULL);
  failures += assert_uri_to_filename ("file:///c:\\thing",  "c:\\thing");
  failures += assert_uri_to_filename ("file://localhost/",  NULL);
  failures += assert_uri_to_filename ("file://c:\\foo\\bar\\x.rdf",  NULL);

#else

  failures += assert_filename_to_uri ("/path/to/file", "file:///path/to/file");
  failures += assert_filename_to_uri ("/path/to/file with spaces", "file:///path/to/file%20with%20spaces");
  failures += assert_uri_to_filename ("file:///path/to/file", "/path/to/file");
  failures += assert_uri_to_filename ("file:///path/to/file%20with%20spaces", "/path/to/file with spaces");

#if defined(HAVE_UNISTD_H) && defined(HAVE_SYS_STAT_H)
  /* Need to test this with a real dir (preferably not /)
   * This is just a test so pretty likely to work on all development systems
   * that are not WIN32
   */

  for(i=0; (dir=dirs[i]); i++) {
    struct stat buf;
    if(!lstat(dir, &buf) && S_ISDIR(buf.st_mode) && !S_ISLNK(buf.st_mode)) {
      if(!chdir(dir))
        break;
    }
  }
  if(!dir)
    fprintf(stderr,
            "%s: WARNING: Found no convenient directory - not testing relative files\n",
            program);
  else {
    sprintf((char*)uri_buffer, "file://%s/foo", dir);
    fprintf(stderr,
            "%s: Checking relative file name 'foo' in dir %s expecting URI %s\n",
            program, dir, uri_buffer);
    failures += assert_filename_to_uri ("foo", (const char*)uri_buffer);
  }
#endif
 
#endif

  raptor_uri_init();
  
  uri1=raptor_new_uri((const unsigned char*)base_uri);

  str=raptor_uri_as_string(uri1);
  if(strcmp((const char*)str, base_uri)) {
    fprintf(stderr,
            "%s: raptor_uri_as_string(%s) FAILED gaving %s != %s\n",
            program, base_uri, str, base_uri);
    failures++;
  }
  
  uri2=raptor_new_uri_for_xmlbase(uri1);
  str=raptor_uri_as_string(uri2);
  if(strcmp((const char*)str, base_uri_xmlbase)) {
    fprintf(stderr, 
            "%s: raptor_new_uri_for_xmlbase(URI %s) FAILED giving %s != %s\n",
            program, base_uri, str, base_uri_xmlbase);
    failures++;
  }
  
  uri3=raptor_new_uri_for_retrieval(uri1);

  str=raptor_uri_as_string(uri3);
  if(strcmp((const char*)str, base_uri_retrievable)) {
    fprintf(stderr,
            "%s: raptor_new_uri_for_retrievable(%s) FAILED gaving %s != %s\n",
            program, base_uri, str, base_uri_retrievable);
    failures++;
  }
  
  raptor_free_uri(uri3);
  raptor_free_uri(uri2);
  raptor_free_uri(uri1);
  
  failures += assert_uri_to_relative(NULL, "http://example.com/foo/bar", "http://example.com/foo/bar");
  failures += assert_uri_to_relative("", "http://example.com/foo/bar", "http://example.com/foo/bar");
  failures += assert_uri_to_relative("foo:", "http://example.com/foo/bar", "http://example.com/foo/bar");
  failures += assert_uri_to_relative("http://example.com/base/foo?foo#foo", "http://example.com/base/bar?bar#bar", "bar?bar#bar");
  failures += assert_uri_to_relative("http://example.com/base/foo", "http://example.com/base/foo/", "foo/");
  failures += assert_uri_to_relative("http://example.com/base/foo", "http://example.com/base/foo/.foo", "foo/.foo");
  failures += assert_uri_to_relative("http://example.com/base/foo", "http://example.com/base/foo/.foo#bar", "foo/.foo#bar");
  failures += assert_uri_to_relative("http://example.com/base/foo", "http://example.com/base/foo/bar", "foo/bar");
  failures += assert_uri_to_relative("http://example.com/base/foo", "http://example.com/base/foo#bar", "#bar");
  failures += assert_uri_to_relative("http://example.com/base/foo", "http://example.com/base/bar#foo", "bar#foo");
  failures += assert_uri_to_relative("http://example.com/base/foo", "http://example.com/otherbase/bar", "../otherbase/bar");
  failures += assert_uri_to_relative("http://example.com/base/foo", "http://example.com/base/#foo", ".#foo");
  failures += assert_uri_to_relative("http://example.com/base/foo", "http://example2.com/base/bar", "http://example2.com/base/bar");
  failures += assert_uri_to_relative("http://example.com/base/one?path=/should/be/ignored", "http://example.com/base/two?path=/should/be/ignored", "two?path=/should/be/ignored");
  failures += assert_uri_to_relative("http://example.org/base#", "http://www.foo.org", "http://www.foo.org");
  failures += assert_uri_to_relative("http://example.org", "http://a.example.org/", "http://a.example.org/");
  failures += assert_uri_to_relative("http://example.org", "http://a.example.org", "http://a.example.org");
  failures += assert_uri_to_relative("http://abcdefgh.example.org/foo/bar/", "http://ijklmnop.example.org/", "http://ijklmnop.example.org/");


  if(1) {
    raptor_uri_handler uri_handler;
    int ret;
    raptor_uri* u1;
    raptor_uri* u2;
    int called;
    
    /* URI Interface V1 */
    uri_handler.new_uri     = raptor_default_new_uri;
    uri_handler.free_uri    = raptor_default_free_uri;
    uri_handler.uri_compare = raptor_test_uri_compare;
    uri_handler.initialised=1;
    called=0;
    raptor_uri_set_handler(&uri_handler, &called);

    u1=raptor_new_uri((const unsigned char *)"http://example.org/abc");
    u2=raptor_new_uri((const unsigned char *)"http://example.org/def");

    ret=raptor_uri_compare(u1, u2);
    if(!(ret < 0)) {
      fprintf(stderr,
              "%s: raptor_uri_compare(%s, %s) FAILED V1 gave %d expected <0\n",
              program, raptor_uri_as_string(u1), raptor_uri_as_string(u2),
              ret);
      failures++;
    }

    if(called) {
      fprintf(stderr,
              "%s: raptor_uri_compare(%s, %s) FAILED V1 called user handler\n",
              program, raptor_uri_as_string(u1), raptor_uri_as_string(u2));
      failures++;
    }
    

    /* URI Interface V2 */
    uri_handler.initialised=2;
    called=0;
    raptor_uri_set_handler(&uri_handler, &called);

    ret=raptor_uri_compare(u1, u2);
    if(!(ret < 0)) {
      fprintf(stderr,
              "%s: raptor_uri_compare(%s, %s) FAILED V2 gave %d expected <0\n",
              program, raptor_uri_as_string(u1), raptor_uri_as_string(u2),
              ret);
      failures++;
    }

    if(!called) {
      fprintf(stderr,
              "%s: raptor_uri_compare(%s, %s) FAILED V2 did not call user handler\n",
              program, raptor_uri_as_string(u1), raptor_uri_as_string(u2));
      failures++;
    }
    
    raptor_free_uri(u1);
    raptor_free_uri(u2);
  }

  return failures ;
}

#endif
