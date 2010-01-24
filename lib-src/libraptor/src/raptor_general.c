/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_general.c - Raptor general routines
 *
 * Copyright (C) 2000-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2000-2005, University of Bristol, UK http://www.bristol.ac.uk/
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

/* Raptor includes */
#include "raptor.h"
#include "raptor_internal.h"


/* statics */
static int raptor_initialised=0;


const char * const raptor_short_copyright_string = "Copyright 2000-2008 David Beckett. Copyright 2000-2005 University of Bristol";

const char * const raptor_copyright_string = "Copyright (C) 2000-2008 David Beckett - http://www.dajobe.org/\nCopyright (C) 2000-2005 University of Bristol - http://www.bristol.ac.uk/";

const char * const raptor_license_string = "LGPL 2.1 or newer, GPL 2 or newer, Apache 2.0 or newer.\nSee http://librdf.org/raptor/LICENSE.html for full terms.";

const char * const raptor_home_url_string = "http://librdf.org/raptor/";

/**
 * raptor_version_string:
 *
 * Library full version as a string.
 *
 * See also #raptor_version_decimal.
 */
const char * const raptor_version_string = VERSION;

/**
 * raptor_version_major:
 *
 * Library major version number as a decimal integer.
 */
const unsigned int raptor_version_major = RAPTOR_VERSION_MAJOR;

/**
 * raptor_version_minor:
 *
 * Library minor version number as a decimal integer.
 */
const unsigned int raptor_version_minor = RAPTOR_VERSION_MINOR;

/**
 * raptor_version_release:
 *
 * Library release version number as a decimal integer.
 */
const unsigned int raptor_version_release = RAPTOR_VERSION_RELEASE;

/**
 * raptor_version_decimal:
 *
 * Library full version as a decimal integer.
 *
 * See also #raptor_version_string.
 */
const unsigned int raptor_version_decimal = RAPTOR_VERSION_DECIMAL;



/**
 * raptor_init:
 *
 * Initialise the raptor library.
 * 
 * This function MUST be called before using any of the raptor APIs.
 **/
void
raptor_init(void) 
{
  if(raptor_initialised)
    return;

  raptor_initialised=1;

  if(raptor_sax2_init())
    goto failure;

  if(raptor_parsers_init())
    goto failure;
  
  if(raptor_serializers_init())
    goto failure;

  if(raptor_uri_init())
    goto failure;

  raptor_www_init(); /* raptor_www_init() is part of raptor API, prototype not changed yet to return an error code */

  return;

  failure:
  raptor_finish();
  RAPTOR_FATAL1("raptor_init() failed");
}


/**
 * raptor_finish:
 *
 * Terminate the raptor library.
 *
 * Cleans up state of the library.  If called, must be used after
 * all other objects are destroyed with their destructor.
 **/
void
raptor_finish(void) 
{
  if(!raptor_initialised)
    return;

  raptor_www_finish();

  raptor_parsers_finish();

  raptor_serializers_finish();

  raptor_sax2_finish();
  
  raptor_initialised=0;
}



/* 
 * Thanks to the patch in this Debian bug for the solution
 * to the crash inside vsnprintf on some architectures.
 *
 * "reuse of args inside the while(1) loop is in violation of the
 * specs and only happens to work by accident on other systems."
 *
 * http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=104325 
 */

#ifndef va_copy
#ifdef __va_copy
#define va_copy(dest,src) __va_copy(dest,src)
#else
#define va_copy(dest,src) (dest) = (src)
#endif
#endif

/**
 * raptor_vsnprintf:
 * @message: printf-style format string
 * @arguments: variable arguments list
 * 
 * Format output for a variable arguments list.
 *
 * This is a wrapper around system versions of vsnprintf with
 * different call and return conventions.
 * 
 * Return value: a newly allocated string as the format result or NULL on failure
 **/
char*
raptor_vsnprintf(const char *message, va_list arguments) 
{
  char empty_buffer[1];
  int len;
  char *buffer=NULL;
  va_list args_copy;

#ifdef HAVE_C99_VSNPRINTF
  /* copy for re-use */
  va_copy(args_copy, arguments);
  len=vsnprintf(empty_buffer, 1, message, args_copy)+1;
  va_end(args_copy);

  if(len<=0)
    return NULL;
  
  buffer=(char*)RAPTOR_MALLOC(cstring, len);
  if(buffer) {
    /* copy for re-use */
    va_copy(args_copy, arguments);
    vsnprintf(buffer, len, message, args_copy);
    va_end(args_copy);
  }
#else
  /* This vsnprintf doesn't return number of bytes required */
  int size=2;
      
  while(1) {
    buffer=(char*)RAPTOR_MALLOC(cstring, size+1);
    if(!buffer)
      break;
    
    /* copy for re-use */
    va_copy(args_copy, arguments);
    len=vsnprintf(buffer, size, message, args_copy);
    va_end(args_copy);

    if(len>=0)
      break;
    RAPTOR_FREE(cstring, buffer);
    size+=4;
  }
#endif

  return buffer;
}


/**
 * raptor_basename:
 * @name: path
 * 
 * Get the basename of a path
 * 
 * Return value: filename part of a pathname
 **/
const char*
raptor_basename(const char *name)
{
  char *p;
  if((p=strrchr(name, '/')))
    name=p+1;
  else if((p=strrchr(name, '\\')))
    name=p+1;

  return name;
}


const unsigned char * const raptor_xml_literal_datatype_uri_string=(const unsigned char *)"http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral";
const unsigned int raptor_xml_literal_datatype_uri_string_len=53;

/**
 * raptor_print_ntriples_string:
 * @stream: FILE* stream to print to
 * @string: UTF-8 string to print
 * @delim: Delimiter character for string (such as ") or \0 for no delim
 * escaping.
 *
 * Print an UTF-8 string using N-Triples escapes.
 * 
 * Return value: non-0 on failure such as bad UTF-8 encoding.
 **/
int
raptor_print_ntriples_string(FILE *stream,
                             const unsigned char *string,
                             const char delim) 
{
  unsigned char c;
  size_t len=strlen((const char*)string);
  int unichar_len;
  raptor_unichar unichar;
  
  for(; (c=*string); string++, len--) {
    if((delim && c == delim) || c == '\\') {
      fprintf(stream, "\\%c", c);
      continue;
    }
    
    /* Note: NTriples is ASCII */
    if(c == 0x09) {
      fputs("\\t", stream);
      continue;
    } else if(c == 0x0a) {
      fputs("\\n", stream);
      continue;
    } else if(c == 0x0d) {
      fputs("\\r", stream);
      continue;
    } else if(c < 0x20|| c == 0x7f) {
      fprintf(stream, "\\u%04X", c);
      continue;
    } else if(c < 0x80) {
      fputc(c, stream);
      continue;
    }
    
    /* It is unicode */
    
    unichar_len=raptor_utf8_to_unicode_char(NULL, string, len);
    if(unichar_len < 0 || unichar_len > (int)len)
      /* UTF-8 encoding had an error or ended in the middle of a string */
      return 1;

    unichar_len=raptor_utf8_to_unicode_char(&unichar, string, len);
    
    if(unichar < 0x10000)
      fprintf(stream, "\\u%04lX", unichar);
    else
      fprintf(stream, "\\U%08lX", unichar);
    
    unichar_len--; /* since loop does len-- */
    string += unichar_len; len -= unichar_len;

  }

  return 0;
}


/**
 * raptor_check_ordinal:
 * @name: ordinal string
 *
 * Check an RDF property ordinal, the n in rdf:_n
 *
 * Return value: ordinal integer or <0 if string is not a valid ordinal
 */
int
raptor_check_ordinal(const unsigned char *name) {
  int ordinal= -1;
  unsigned char c;

  while((c=*name++)) {
    if(c < '0' || c > '9')
      return -1;
    if(ordinal <0)
      ordinal=0;
    ordinal *= 10;
    ordinal += (c - '0');
  }
  return ordinal;
}


/**
 * raptor_error_handlers_init:
 * @error_handlers: error handlers object
 *
 * Initialize #raptor_error_handlers object statically.
 *
 */
void
raptor_error_handlers_init(raptor_error_handlers* error_handlers)
{
  error_handlers->magic=RAPTOR_ERROR_HANDLER_MAGIC;
}


static const char* const raptor_log_level_labels[RAPTOR_LOG_LEVEL_LAST+1]={
  "none",
  "fatal error",
  "error",
  "warning"
};


/* internal */
void
raptor_log_error_to_handlers(raptor_error_handlers* error_handlers,
                             raptor_log_level level,
                             raptor_locator* locator, const char* message)
{
  if(level == RAPTOR_LOG_LEVEL_NONE)
    return;

  raptor_log_error(level, error_handlers->handlers[level].handler,
                   error_handlers->handlers[level].user_data,
                   locator, message);
}


void
raptor_log_error_varargs(raptor_log_level level,
                         raptor_message_handler handler, void* handler_data,
                         raptor_locator* locator,
                         const char* message, va_list arguments)
{
  char *buffer;
  size_t length;
  
  if(level == RAPTOR_LOG_LEVEL_NONE)
    return;

  buffer=raptor_vsnprintf(message, arguments);
  if(!buffer) {
    if(locator) {
      raptor_print_locator(stderr, locator);
      fputc(' ', stderr);
    }
    fputs("raptor ", stderr);
    fputs(raptor_log_level_labels[level], stderr);
    fputs(" - ", stderr);
    vfprintf(stderr, message, arguments);
    fputc('\n', stderr);
    return;
  }

  length=strlen(buffer);
  if(buffer[length-1]=='\n')
    buffer[length-1]='\0';
  
  raptor_log_error(level, handler, handler_data, locator, buffer);

  RAPTOR_FREE(cstring, buffer);
}


/* internal */
void
raptor_log_error(raptor_log_level level,
                 raptor_message_handler handler, void* handler_data,
                 raptor_locator* locator, const char* message)
{
  if(level == RAPTOR_LOG_LEVEL_NONE)
    return;

  if(handler)
    /* This is the place in raptor that MOST of the user error handler
     * functions are called.  Not all, since things that use
     * raptor_simple_message_handler are called in their respective codes.
     *
     * FIXME: In future, this should be the only place but it requires
     * a public API change such as e.g. raptor_new_qname()
     */
    handler(handler_data, locator, message);
  else {
    if(locator) {
      raptor_print_locator(stderr, locator);
      fputc(' ', stderr);
    }
    fputs("raptor ", stderr);
    fputs(raptor_log_level_labels[level], stderr);
    fputs(" - ", stderr);
    fputs(message, stderr);
    fputc('\n', stderr);
  }
}



/**
 * raptor_free_memory:
 * @ptr: memory pointer
 *
 * Free memory allocated inside raptor.
 * 
 * Some systems require memory allocated in a library to
 * be deallocated in that library.  This function allows
 * memory allocated by raptor to be freed.
 *
 * Examples include the result of the '_to_' methods that returns
 * allocated memory such as raptor_uri_filename_to_uri_string,
 * raptor_uri_filename_to_uri_string
 * and raptor_uri_uri_string_to_filename_fragment
 *
 **/
void
raptor_free_memory(void *ptr)
{
  RAPTOR_FREE(void, ptr);
}


/**
 * raptor_alloc_memory:
 * @size: size of memory to allocate
 *
 * Allocate memory inside raptor.
 * 
 * Some systems require memory allocated in a library to
 * be deallocated in that library.  This function allows
 * memory to be allocated inside the raptor shared library
 * that can be freed inside raptor either internally or via
 * raptor_free_memory.
 *
 * Examples include using this in the raptor_parser_generate_id() handler
 * code to create new strings that will be used internally
 * as short identifiers and freed later on by the parsers.
 *
 * Return value: the address of the allocated memory or NULL on failure
 *
 **/
void*
raptor_alloc_memory(size_t size)
{
  return RAPTOR_MALLOC(void, size);
}


/**
 * raptor_calloc_memory:
 * @nmemb: number of members
 * @size: size of item
 *
 * Allocate zeroed array of items inside raptor.
 * 
 * Some systems require memory allocated in a library to
 * be deallocated in that library.  This function allows
 * memory to be allocated inside the raptor shared library
 * that can be freed inside raptor either internally or via
 * raptor_free_memory.
 *
 * Examples include using this in the raptor_parser_generate_id() handler
 * code to create new strings that will be used internally
 * as short identifiers and freed later on by the parsers.
 *
 * Return value: the address of the allocated memory or NULL on failure
 *
 **/
void*
raptor_calloc_memory(size_t nmemb, size_t size)
{
  return RAPTOR_CALLOC(void, nmemb, size);
}


#if defined (RAPTOR_DEBUG) && defined(RAPTOR_MEMORY_SIGN)
void*
raptor_sign_malloc(size_t size)
{
  int *p;
  
  size += sizeof(int);
  
  p=(int*)malloc(size);
  *p++ = RAPTOR_SIGN_KEY;
  return p;
}

void*
raptor_sign_calloc(size_t nmemb, size_t size)
{
  int *p;
  
  /* turn into bytes */
  size = nmemb*size + sizeof(int);
  
  p=(int*)calloc(1, size);
  *p++ = RAPTOR_SIGN_KEY;
  return p;
}

void*
raptor_sign_realloc(void *ptr, size_t size)
{
  int *p;

  if(!ptr)
    return raptor_sign_malloc(size);
  
  p=(int*)ptr;
  p--;

  if(*p != RAPTOR_SIGN_KEY)
    RAPTOR_FATAL3("memory signature %08X != %08X", *p, RAPTOR_SIGN_KEY);

  size += sizeof(int);
  
  p=(int*)realloc(p, size);
  *p++= RAPTOR_SIGN_KEY;
  return p;
}

void
raptor_sign_free(void *ptr)
{
  int *p;

  if(!ptr)
    return;
  
  p=(int*)ptr;
  p--;

  if(*p != RAPTOR_SIGN_KEY)
    RAPTOR_FATAL3("memory signature %08X != %08X", *p, RAPTOR_SIGN_KEY);

  free(p);
}
#endif


#if defined (RAPTOR_DEBUG) && defined(HAVE_DMALLOC_H) && defined(RAPTOR_MEMORY_DEBUG_DMALLOC)

#undef malloc
void*
raptor_system_malloc(size_t size)
{
  return malloc(size);
}

#undef free
void
raptor_system_free(void *ptr)
{
  free(ptr);
}

#endif
