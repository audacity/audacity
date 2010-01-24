/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rasqal_general.c - Rasqal library startup, shutdown and factories
 *
 * Copyright (C) 2004-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2004-2005, University of Bristol, UK http://www.bristol.ac.uk/
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


/* prototypes for helper functions */
static void rasqal_delete_query_engine_factories(rasqal_world*);


/* statics */

const char * const rasqal_short_copyright_string = "Copyright 2003-2008 David Beckett.  Copyright 2003-2005 University of Bristol";

const char * const rasqal_copyright_string = "Copyright (C) 2003-2008 David Beckett - http://www.dajobe.org/\nCopyright (C) 2003-2005 University of Bristol - http://www.bristol.ac.uk/";

const char * const rasqal_license_string = "LGPL 2.1 or newer, GPL 2 or newer, Apache 2.0 or newer.\nSee http://librdf.org/rasqal/LICENSE.html for full terms.";

const char * const rasqal_home_url_string = "http://librdf.org/rasqal/";

/**
 * rasqal_version_string:
 *
 * Library full version as a string.
 *
 * See also #rasqal_version_decimal.
 */
const char * const rasqal_version_string = VERSION;

/**
 * rasqal_version_major:
 *
 * Library major version number as a decimal integer.
 */
const unsigned int rasqal_version_major = RASQAL_VERSION_MAJOR;

/**
 * rasqal_version_minor:
 *
 * Library minor version number as a decimal integer.
 */
const unsigned int rasqal_version_minor = RASQAL_VERSION_MINOR;

/**
 * rasqal_version_release:
 *
 * Library release version number as a decimal integer.
 */
const unsigned int rasqal_version_release = RASQAL_VERSION_RELEASE;

/**
 * rasqal_version_decimal:
 *
 * Library full version as a decimal integer.
 *
 * See also #rasqal_version_string.
 */
const unsigned int rasqal_version_decimal = RASQAL_VERSION_DECIMAL;


/**
 * rasqal_new_world:
 * 
 * Initialise the rasqal library.
 *
 * Creates a rasqal_world object and initializes it.
 *
 * The returned world object is used with subsequent rasqal API calls.
 *
 * Return value: rasqal_world object or NULL on failure
 **/
rasqal_world*
rasqal_new_world(void)
{
  rasqal_world *world;

  world=(rasqal_world*)RASQAL_CALLOC(rasqal_world, sizeof(rasqal_world), 1);
  if(!world)
    return NULL;

  raptor_init();

  if(rasqal_uri_init(world))
    goto failure;

  if(rasqal_xsd_init(world))
    goto failure;

/* FIXME */
#ifndef RAPTOR_ERROR_HANDLER_MAGIC
#define RAPTOR_ERROR_HANDLER_MAGIC 0xD00DB1FF
#endif
  world->error_handlers.magic=RAPTOR_ERROR_HANDLER_MAGIC;


  /* last one declared is the default - RDQL */

#ifdef RASQAL_QUERY_RDQL
  if(rasqal_init_query_engine_rdql(world))
    goto failure;
#endif

#ifdef RASQAL_QUERY_LAQRS
  if(rasqal_init_query_engine_laqrs(world))
    goto failure;
#endif

#ifdef RASQAL_QUERY_SPARQL  
  if(rasqal_init_query_engine_sparql(world))
    goto failure;
#endif

#ifdef RAPTOR_TRIPLES_SOURCE_RAPTOR
  if(rasqal_raptor_init(world))
    goto failure;
#endif
#ifdef RAPTOR_TRIPLES_SOURCE_REDLAND
  if(rasqal_redland_init(world))
    goto failure;
#endif

  if(rasqal_init_query_results())
    goto failure;
  
  if(rasqal_init_result_formats(world))
    goto failure;

  return world;

  failure:
  rasqal_free_world(world);
  return NULL;
}


/**
 * rasqal_free_world:
 * @world: rasqal_world object
 * 
 * Terminate the rasqal library.
 *
 * Destroys a rasqal_world object and all static information.
 *
 **/
void
rasqal_free_world(rasqal_world* world) 
{
  RASQAL_ASSERT_OBJECT_POINTER_RETURN(world, rasqal_world);
  
  rasqal_finish_result_formats(world);
  rasqal_finish_query_results();

  rasqal_delete_query_engine_factories(world);

#ifdef RAPTOR_TRIPLES_SOURCE_REDLAND
  rasqal_redland_finish();
#endif

  rasqal_xsd_finish(world);

  rasqal_uri_finish(world);

  raptor_finish();

  RASQAL_FREE(rasqal_world, world);
}


/* helper functions */

/*
 * rasqal_free_query_engine_factory - delete a query engine factory
 */
static void
rasqal_free_query_engine_factory(rasqal_query_engine_factory *factory)
{
  RASQAL_ASSERT_OBJECT_POINTER_RETURN(factory, rasqal_query_engine_factory);
  
  if(factory) {
    if(factory->finish_factory)
      factory->finish_factory(factory);

    if(factory->name)
      RASQAL_FREE(rasqal_query_engine_factory, (void*)factory->name);
    if(factory->label)
      RASQAL_FREE(rasqal_query_engine_factory, (void*)factory->label);
    if(factory->alias)
      RASQAL_FREE(rasqal_query_engine_factory, (void*)factory->alias);
    if(factory->uri_string)
      RASQAL_FREE(rasqal_query_engine_factory, (void*)factory->uri_string);

    RASQAL_FREE(rasqal_query_engine_factory, factory);
  }
}


/*
 * rasqal_delete_query_engine_factories - helper function to delete all the registered query engine factories
 */
static void
rasqal_delete_query_engine_factories(rasqal_world *world)
{
  rasqal_query_engine_factory *factory, *next;
  
  for(factory=world->query_engines; factory; factory=next) {
    next=factory->next;
    rasqal_free_query_engine_factory(factory);
  }
  world->query_engines=NULL;
}


/* class methods */

/*
 * rasqal_query_engine_register_factory - Register a syntax handled by a query factory
 * @name: the short syntax name
 * @label: readable label for syntax
 * @uri_string: URI string of the syntax (or NULL)
 * @factory: pointer to function to call to register the factory
 * 
 * INTERNAL
 *
 * Return value: non-0 on failure
 **/
int
rasqal_query_engine_register_factory(rasqal_world *world,
                                     const char *name, const char *label,
                                     const char *alias,
                                     const unsigned char *uri_string,
                                     void (*factory) (rasqal_query_engine_factory*)) 
{
  rasqal_query_engine_factory *query, *h;
  char *name_copy, *label_copy, *alias_copy;
  unsigned char *uri_string_copy;
  
#if defined(RASQAL_DEBUG) && RASQAL_DEBUG > 1
  RASQAL_DEBUG4("Received registration for syntax %s '%s' with alias '%s'\n", 
                name, label, (alias ? alias : "none"));
  RASQAL_DEBUG2("URI %s\n", (uri_string ? (const char*)uri_string : (const char*)"none"));
#endif
  
  query=(rasqal_query_engine_factory*)RASQAL_CALLOC(rasqal_query_engine_factory, 1,
                                                    sizeof(rasqal_query_engine_factory));
  if(!query)
    goto tidy_noquery;

  for(h = world->query_engines; h; h = h->next ) {
    if(!strcmp(h->name, name) ||
       (alias && !strcmp(h->name, alias))) {
      RASQAL_FATAL2("query %s already registered\n", h->name);
    }
  }
  
  name_copy=(char*)RASQAL_CALLOC(cstring, strlen(name)+1, 1);
  if(!name_copy)
    goto tidy;
  strcpy(name_copy, name);
  query->name=name_copy;
        
  label_copy=(char*)RASQAL_CALLOC(cstring, strlen(label)+1, 1);
  if(!label_copy)
    goto tidy;
  strcpy(label_copy, label);
  query->label=label_copy;

  if(uri_string) {
    uri_string_copy=(unsigned char*)RASQAL_CALLOC(cstring, strlen((const char*)uri_string)+1, 1);
    if(!uri_string_copy)
      goto tidy;
    strcpy((char*)uri_string_copy, (const char*)uri_string);
    query->uri_string=uri_string_copy;
  }
        
  if(alias) {
    alias_copy=(char*)RASQAL_CALLOC(cstring, strlen(alias)+1, 1);
    if(!alias_copy)
      goto tidy;
    strcpy(alias_copy, alias);
    query->alias=alias_copy;
  }

  /* Call the query registration function on the new object */
  (*factory)(query);
  
#if defined(RASQAL_DEBUG) && RASQAL_DEBUG > 1
  RASQAL_DEBUG3("%s has context size %d\n", name, (int)query->context_length);
#endif
  
  query->next = world->query_engines;
  world->query_engines = query;

  return 0;

  tidy:
  rasqal_free_query_engine_factory(query);
  tidy_noquery:
  rasqal_log_error_simple(world, RAPTOR_LOG_LEVEL_FATAL, NULL,
                          "Out of memory in rasqal_query_engine_register_factory()");
  return 1;
}


/**
 * rasqal_get_query_engine_factory:
 * @name: the factory name or NULL for the default factory
 * @uri: the query syntax URI or NULL
 *
 * Get a query factory by name.
 * 
 * Return value: the factory object or NULL if there is no such factory
 **/
rasqal_query_engine_factory*
rasqal_get_query_engine_factory(rasqal_world *world, const char *name,
                                const unsigned char *uri)
{
  rasqal_query_engine_factory *factory;

  /* return 1st query if no particular one wanted - why? */
  if(!name && !uri) {
    factory=world->query_engines;
    if(!factory) {
      RASQAL_DEBUG1("No (default) query_engines registered\n");
      return NULL;
    }
  } else {
    for(factory=world->query_engines; factory; factory=factory->next) {
      if((name && !strcmp(factory->name, name)) ||
         (factory->alias && !strcmp(factory->alias, name)))
        break;
      if(uri && !strcmp((const char*)factory->uri_string, (const char*)uri))
        break;
    }
    /* else FACTORY name not found */
    if(!factory) {
      RASQAL_DEBUG2("No query language with name %s found\n", name);
      return NULL;
    }
  }
        
  return factory;
}


/**
 * rasqal_languages_enumerate:
 * @world: rasqal_world object
 * @counter: index into the list of syntaxes
 * @name: pointer to store the name of the syntax (or NULL)
 * @label: pointer to store syntax readable label (or NULL)
 * @uri_string: pointer to store syntax URI string (or NULL)
 *
 * Get information on query languages.
 * 
 * Return value: non 0 on failure of if counter is out of range
 **/
int
rasqal_languages_enumerate(rasqal_world *world,
                           unsigned int counter,
                           const char **name, const char **label,
                           const unsigned char **uri_string)
{
  unsigned int i;
  rasqal_query_engine_factory *factory=world->query_engines;

  if(!factory)
    return 1;

  for(i=0; factory && i<=counter ; i++, factory=factory->next) {
    if(i == counter) {
      if(name)
        *name=factory->name;
      if(label)
        *label=factory->label;
      if(uri_string)
        *uri_string=factory->uri_string;
      return 0;
    }
  }
        
  return 1;
}


/**
 * rasqal_language_name_check:
 * @world: rasqal_world object
 * @name: the query language name
 *
 * Check name of a query language.
 *
 * Return value: non 0 if name is a known query language
 */
int
rasqal_language_name_check(rasqal_world* world, const char *name) {
  return (rasqal_get_query_engine_factory(world, name, NULL) != NULL);
}


static const char* const rasqal_log_level_labels[RAPTOR_LOG_LEVEL_LAST+1]={
  "none",
  "fatal error",
  "error",
  "warning"
};


/* internal */
void
rasqal_log_error_simple(rasqal_world* world, raptor_log_level level,
                        raptor_locator* locator, const char* message, ...)
{
  va_list arguments;

  if(level == RAPTOR_LOG_LEVEL_NONE)
    return;

  va_start(arguments, message);
  rasqal_log_error_varargs(world, level, locator, message, arguments);
  va_end(arguments);
}


void
rasqal_log_error_varargs(rasqal_world* world, raptor_log_level level,
                         raptor_locator* locator,
                         const char* message, va_list arguments)
{
  char *buffer;
  size_t length;
  raptor_message_handler handler=world->error_handlers.handlers[level].handler;
  void* handler_data=world->error_handlers.handlers[level].user_data;
  
  if(level == RAPTOR_LOG_LEVEL_NONE)
    return;

  buffer=raptor_vsnprintf(message, arguments);
  if(!buffer) {
    if(locator) {
      raptor_print_locator(stderr, locator);
      fputc(' ', stderr);
    }
    fputs("rasqal ", stderr);
    fputs(rasqal_log_level_labels[level], stderr);
    fputs(" - ", stderr);
    vfprintf(stderr, message, arguments);
    fputc('\n', stderr);
    return;
  }

  length=strlen(buffer);
  if(buffer[length-1]=='\n')
    buffer[length-1]='\0';
  
  if(handler)
    /* This is the single place in rasqal that the user error handler
     * functions are called.
     */
    handler(handler_data, locator, buffer);
  else {
    if(locator) {
      raptor_print_locator(stderr, locator);
      fputc(' ', stderr);
    }
    fputs("rasqal ", stderr);
    fputs(rasqal_log_level_labels[level], stderr);
    fputs(" - ", stderr);
    fputs(buffer, stderr);
    fputc('\n', stderr);
  }

  RASQAL_FREE(cstring, buffer);
}


/*
 * rasqal_query_simple_error - Error from a query - Internal
 *
 * Matches the raptor_simple_message_handler API but same as
 * rasqal_query_error 
 */
void
rasqal_query_simple_error(void* user_data, const char *message, ...)
{
  rasqal_query* query=(rasqal_query*)user_data;

  va_list arguments;

  va_start(arguments, message);

  query->failed=1;
  rasqal_log_error_varargs(query->world,
                           RAPTOR_LOG_LEVEL_ERROR, NULL,
                           message, arguments);
  
  va_end(arguments);
}


/* wrapper */
const char*
rasqal_basename(const char *name)
{
  char *p;
  if((p=strrchr(name, '/')))
    name=p+1;
  else if((p=strrchr(name, '\\')))
    name=p+1;

  return name;
}


/**
 * rasqal_escaped_name_to_utf8_string:
 * @src: source name string
 * @len: length of source name string
 * @dest_lenp: pointer to store result string (or NULL)
 * @error_handler: error handling function
 * @error_data: data for error handle
 *
 * Get a UTF-8 and/or \u-escaped name as UTF-8.
 *
 * If dest_lenp is not NULL, the length of the resulting string is
 * stored at the pointed size_t.
 *
 * Return value: new UTF-8 string or NULL on failure.
 */
unsigned char*
rasqal_escaped_name_to_utf8_string(const unsigned char *src, size_t len,
                                   size_t *dest_lenp,
                                   raptor_simple_message_handler error_handler,
                                   void *error_data)
{
  const unsigned char *p=src;
  size_t ulen=0;
  unsigned long unichar=0;
  unsigned char *result;
  unsigned char *dest;
  int n;
  
  result=(unsigned char*)RASQAL_MALLOC(cstring, len+1);
  if(!result)
    return NULL;

  dest=result;

  /* find end of string, fixing backslashed characters on the way */
  while(len > 0) {
    unsigned char c=*p;

    if(c > 0x7f) {
      /* just copy the UTF-8 bytes through */
      size_t unichar_len=raptor_utf8_to_unicode_char(NULL, (const unsigned char*)p, len+1);
      if(unichar_len > len) {
        if(error_handler)
          error_handler(error_data, "UTF-8 encoding error at character %d (0x%02X) found.", c, c);
        /* UTF-8 encoding had an error or ended in the middle of a string */
        RASQAL_FREE(cstring, result);
        return NULL;
      }
      memcpy(dest, p, unichar_len);
      dest+= unichar_len;
      p += unichar_len;
      len -= unichar_len;
      continue;
    }

    p++; len--;
    
    if(c != '\\') {
      /* not an escape - store and move on */
      *dest++=c;
      continue;
    }

    if(!len) {
      RASQAL_FREE(cstring, result);
      return NULL;
    }

    c = *p++; len--;

    switch(c) {
      case '"':
      case '\\':
        *dest++=c;
        break;
      case 'u':
      case 'U':
        ulen=(c == 'u') ? 4 : 8;
        
        if(len < ulen) {
          if(error_handler)
            error_handler(error_data, "%c over end of line", c);
          RASQAL_FREE(cstring, result);
          return 0;
        }
        
        n=sscanf((const char*)p, ((ulen == 4) ? "%04lx" : "%08lx"), &unichar);
        if(n != 1) {
          if(error_handler)
            error_handler(error_data, "Bad %c escape", c);
          break;
        }
        
        p+=ulen;
        len-=ulen;
        
        if(unichar > 0x10ffff) {
          if(error_handler)
            error_handler(error_data, "Illegal Unicode character with code point #x%lX.", unichar);
          break;
        }
          
        dest+=raptor_unicode_char_to_utf8(unichar, dest);
        break;

      default:
        if(error_handler)
          error_handler(error_data, "Illegal string escape \\%c in \"%s\"", c, src);
        RASQAL_FREE(cstring, result);
        return 0;
    }

  } /* end while */

  
  /* terminate dest, can be shorter than source */
  *dest='\0';

  if(dest_lenp)
    *dest_lenp=dest-result;

  return result;
}


int
rasqal_uri_init(rasqal_world* world) 
{
  world->rdf_namespace_uri=raptor_new_uri(raptor_rdf_namespace_uri);
  if(!world->rdf_namespace_uri)
    goto oom;

  world->rdf_first_uri=raptor_new_uri_from_uri_local_name(world->rdf_namespace_uri, (const unsigned char*)"first");
  world->rdf_rest_uri=raptor_new_uri_from_uri_local_name(world->rdf_namespace_uri, (const unsigned char*)"rest");
  world->rdf_nil_uri=raptor_new_uri_from_uri_local_name(world->rdf_namespace_uri, (const unsigned char*)"nil");

  if(!world->rdf_first_uri || !world->rdf_rest_uri || !world->rdf_nil_uri)
    goto oom;

  return 0;

  oom:
  rasqal_log_error_simple(world, RAPTOR_LOG_LEVEL_FATAL,
                          NULL,
                          "Out of memory");
  return 1;
}


void
rasqal_uri_finish(rasqal_world* world) 
{
  if(world->rdf_first_uri) {
    raptor_free_uri(world->rdf_first_uri);
    world->rdf_first_uri=NULL;
  }
  if(world->rdf_rest_uri) {
    raptor_free_uri(world->rdf_rest_uri);
    world->rdf_rest_uri=NULL;
  }
  if(world->rdf_nil_uri) {
    raptor_free_uri(world->rdf_nil_uri);
    world->rdf_nil_uri=NULL;
  }
  if(world->rdf_namespace_uri) {
    raptor_free_uri(world->rdf_namespace_uri);
    world->rdf_namespace_uri=NULL;
  }
}



/**
 * rasqal_query_set_default_generate_bnodeid_parameters - Set default bnodeid generation parameters
 * @rdf_query: #rasqal_query object
 * @prefix: prefix string
 * @base: integer base identifier
 *
 * Sets the parameters for the default algorithm used to generate
 * blank node IDs.  The default algorithm uses both @prefix and @base
 * to generate a new identifier.  The exact identifier generated is
 * not guaranteed to be a strict concatenation of @prefix and @base
 * but will use both parts.
 *
 * For finer control of the generated identifiers, use
 * rasqal_set_default_generate_bnodeid_handler()
 *
 * If prefix is NULL, the default prefix is used (currently "bnodeid")
 * If base is less than 1, it is initialised to 1.
 * 
 **/
void
rasqal_query_set_default_generate_bnodeid_parameters(rasqal_query* rdf_query, 
                                                     char *prefix, int base)
{
  char *prefix_copy=NULL;
  size_t length=0;

  if(--base<0)
    base=0;

  if(prefix) {
    length=strlen(prefix);
    
    prefix_copy=(char*)RASQAL_MALLOC(cstring, length+1);
    if(!prefix_copy)
      return;
    strcpy(prefix_copy, prefix);
  }
  
  if(rdf_query->default_generate_bnodeid_handler_prefix)
    RASQAL_FREE(cstring, rdf_query->default_generate_bnodeid_handler_prefix);

  rdf_query->default_generate_bnodeid_handler_prefix=prefix_copy;
  rdf_query->default_generate_bnodeid_handler_prefix_length=length;
  rdf_query->default_generate_bnodeid_handler_base=base;
}


/**
 * rasqal_query_set_generate_bnodeid_handler:
 * @query: #rasqal_query query object
 * @user_data: user data pointer for callback
 * @handler: generate blank ID callback function
 *
 * Set the generate blank node ID handler function for the query.
 *
 * Sets the function to generate blank node IDs for the query.
 * The handler is called with a pointer to the rasqal_query, the
 * @user_data pointer and a user_bnodeid which is the value of
 * a user-provided blank node identifier (may be NULL).
 * It can either be returned directly as the generated value when present or
 * modified.  The passed in value must be free()d if it is not used.
 *
 * If handler is NULL, the default method is used
 * 
 **/
void
rasqal_query_set_generate_bnodeid_handler(rasqal_query* query,
                                          void *user_data,
                                          rasqal_generate_bnodeid_handler handler)
{
  query->generate_bnodeid_handler_user_data=user_data;
  query->generate_bnodeid_handler=handler;
}


static unsigned char*
rasqal_default_generate_bnodeid_handler(void *user_data,
                                        unsigned char *user_bnodeid) 
{
  rasqal_query *rdf_query=(rasqal_query *)user_data;
  int id;
  unsigned char *buffer;
  int length;
  int tmpid;

  if(user_bnodeid)
    return user_bnodeid;

  id=++rdf_query->default_generate_bnodeid_handler_base;

  tmpid=id;
  length=2; /* min length 1 + \0 */
  while(tmpid/=10)
    length++;

  if(rdf_query->default_generate_bnodeid_handler_prefix)
    length += rdf_query->default_generate_bnodeid_handler_prefix_length;
  else
    length += 7; /* bnodeid */
  
  buffer=(unsigned char*)RASQAL_MALLOC(cstring, length);
  if(!buffer)
    return NULL;
  if(rdf_query->default_generate_bnodeid_handler_prefix) {
    strncpy((char*)buffer, rdf_query->default_generate_bnodeid_handler_prefix,
            rdf_query->default_generate_bnodeid_handler_prefix_length);
    sprintf((char*)buffer + rdf_query->default_generate_bnodeid_handler_prefix_length,
            "%d", id);
  } else 
    sprintf((char*)buffer, "bnodeid%d", id);

  return buffer;
}


/*
 * rasqal_query_generate_bnodeid - Default generate id - internal
 */
unsigned char*
rasqal_query_generate_bnodeid(rasqal_query* rdf_query,
                              unsigned char *user_bnodeid)
{
  if(rdf_query->generate_bnodeid_handler)
    return rdf_query->generate_bnodeid_handler(rdf_query, 
                                               rdf_query->generate_bnodeid_handler_user_data, user_bnodeid);
  else
    return rasqal_default_generate_bnodeid_handler(rdf_query, user_bnodeid);
}




/**
 * rasqal_free_memory:
 * @ptr: memory pointer
 *
 * Free memory allocated inside rasqal.
 * 
 * Some systems require memory allocated in a library to
 * be deallocated in that library.  This function allows
 * memory allocated by rasqal to be freed.
 *
 **/
void
rasqal_free_memory(void *ptr)
{
  RASQAL_ASSERT_OBJECT_POINTER_RETURN(ptr, memory);
  
  RASQAL_FREE(void, ptr);
}


/**
 * rasqal_alloc_memory:
 * @size: size of memory to allocate
 *
 * Allocate memory inside rasqal.
 * 
 * Some systems require memory allocated in a library to
 * be deallocated in that library.  This function allows
 * memory to be allocated inside the rasqal shared library
 * that can be freed inside rasqal either internally or via
 * rasqal_free_memory().
 *
 * Return value: the address of the allocated memory or NULL on failure
 *
 **/
void*
rasqal_alloc_memory(size_t size)
{
  return RASQAL_MALLOC(void, size);
}


/**
 * rasqal_calloc_memory:
 * @nmemb: number of members
 * @size: size of item
 *
 * Allocate zeroed array of items inside rasqal.
 * 
 * Some systems require memory allocated in a library to
 * be deallocated in that library.  This function allows
 * memory to be allocated inside the rasqal shared library
 * that can be freed inside rasqal either internally or via
 * rasqal_free_memory().
 *
 * Return value: the address of the allocated memory or NULL on failure
 *
 **/
void*
rasqal_calloc_memory(size_t nmemb, size_t size)
{
  return RASQAL_CALLOC(void, nmemb, size);
}


#if defined (RASQAL_DEBUG) && defined(RASQAL_MEMORY_SIGN)
void*
rasqal_sign_malloc(size_t size)
{
  int *p;
  
  size += sizeof(int);
  
  p=(int*)malloc(size);
  *p++ = RASQAL_SIGN_KEY;
  return p;
}

void*
rasqal_sign_calloc(size_t nmemb, size_t size)
{
  int *p;
  
  /* turn into bytes */
  size = nmemb*size + sizeof(int);
  
  p=(int*)calloc(1, size);
  *p++ = RASQAL_SIGN_KEY;
  return p;
}

void*
rasqal_sign_realloc(void *ptr, size_t size)
{
  int *p;

  if(!ptr)
    return rasqal_sign_malloc(size);
  
  p=(int*)ptr;
  p--;

  if(*p != RASQAL_SIGN_KEY)
    RASQAL_FATAL3("memory signature %08X != %08X", *p, RASQAL_SIGN_KEY);

  size += sizeof(int);
  
  p=(int*)realloc(p, size);
  *p++= RASQAL_SIGN_KEY;
  return p;
}

void
rasqal_sign_free(void *ptr)
{
  int *p;

  if(!ptr)
    return;
  
  p=(int*)ptr;
  p--;

  if(*p != RASQAL_SIGN_KEY)
    RASQAL_FATAL3("memory signature %08X != %08X", *p, RASQAL_SIGN_KEY);

  free(p);
}
#endif


#if defined (RASQAL_DEBUG) && defined(HAVE_DMALLOC_H) && defined(RASQAL_MEMORY_DEBUG_DMALLOC)

#undef malloc
void*
rasqal_system_malloc(size_t size)
{
  return malloc(size);
}

#undef free
void
rasqal_system_free(void *ptr)
{
  return free(ptr);
  
}

#endif

