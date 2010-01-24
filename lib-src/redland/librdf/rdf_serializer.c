/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_serializer.c - RDF Serializer (RDF triples to syntax) interface
 *
 * Copyright (C) 2002-2008, David Beckett http://www.dajobe.org/
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
#include <rdf_config.h>
#endif

#ifdef WIN32
#include <win32_rdf_config.h>
#endif

#include <stdio.h>
#include <string.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h> /* for abort() as used in errors */
#endif
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#include <redland.h>


#ifndef STANDALONE

/**
 * librdf_init_serializer:
 * @world: redland world object
 *
 * INTERNAL - Initialise the serializer module.
 *
 **/
void
librdf_init_serializer(librdf_world *world) 
{
  librdf_serializer_raptor_constructor(world);
}


/**
 * librdf_finish_serializer:
 * @world: redland world object
 *
 * INTERNAL - Terminate the serializer module.
 *
 **/
void
librdf_finish_serializer(librdf_world *world) 
{
  if(world->serializers) {
    raptor_free_sequence(world->serializers);
    world->serializers=NULL;
  }
#ifdef HAVE_RAPTOR_RDF_SERIALIZER
  librdf_serializer_raptor_destructor();
#endif
}


/* helper functions */
static void
librdf_free_serializer_factory(librdf_serializer_factory *factory) 
{
  if(factory->name)
    LIBRDF_FREE(cstring, factory->name);
  if(factory->label)
    LIBRDF_FREE(cstring, factory->label);
  if(factory->mime_type)
    LIBRDF_FREE(cstring, factory->mime_type);
  if(factory->type_uri)
    librdf_free_uri(factory->type_uri);
  LIBRDF_FREE(librdf_serializer_factory, factory);
}


/**
 * librdf_serializer_register_factory:
 * @world: redland world object
 * @name: the name of the serializer
 * @label: the label of the serializer (optional)
 * @mime_type: MIME type of the syntax (optional)
 * @uri_string: URI of the syntax (optional)
 * @factory: function to be called to register the factor parameters
 *
 * Register a serializer factory .
 * 
 **/
void
librdf_serializer_register_factory(librdf_world *world,
                                   const char *name, const char *label,
                                   const char *mime_type,
                                   const unsigned char *uri_string,
                                   void (*factory) (librdf_serializer_factory*))
{
  librdf_serializer_factory *serializer;

  librdf_world_open(world);

#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
  LIBRDF_DEBUG2("Received registration for serializer %s\n", name);
#endif

  if(!world->serializers) {
    world->serializers=raptor_new_sequence((raptor_sequence_free_handler *)librdf_free_serializer_factory, NULL);
    if(!world->serializers)
      goto oom;
  }

  serializer=(librdf_serializer_factory*)LIBRDF_CALLOC(librdf_serializer_factory, 1,
                                                       sizeof(librdf_serializer_factory));
  if(!serializer)
    goto oom;

  serializer->name=(char*)LIBRDF_MALLOC(cstring, strlen(name)+1);
  if(!serializer->name)
    goto oom_tidy;
  strcpy(serializer->name, name);

  if(label) {
    serializer->label=(char*)LIBRDF_MALLOC(cstring, strlen(label)+1);
    if(!serializer->label)
      goto oom_tidy;
    strcpy(serializer->label, label);
  }

  /* register mime type if any */
  if(mime_type) {
    serializer->mime_type=(char*)LIBRDF_MALLOC(cstring, strlen(mime_type)+1);
    if(!serializer->mime_type)
      goto oom_tidy;
    strcpy(serializer->mime_type, mime_type);
  }

  /* register URI if any */
  if(uri_string) {
    serializer->type_uri=librdf_new_uri(world, uri_string);
    if(!serializer->type_uri)
      goto oom_tidy;
  }

  if(raptor_sequence_push(world->serializers, serializer)) 
    goto oom;

  /* Call the serializer registration function on the new object */
  (*factory)(serializer);

#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
  LIBRDF_DEBUG3("%s has context size %d\n", name, serializer->context_length);
#endif

  return;

  oom_tidy:
  librdf_free_serializer_factory(serializer);
  oom:
  LIBRDF_FATAL1(world, LIBRDF_FROM_SERIALIZER, "Out of memory");
}


/**
 * librdf_get_serializer_factory:
 * @world: redland world object
 * @name: the name of the factory (NULL or empty string if don't care)
 * @mime_type: the MIME type of the syntax (NULL or empty string if not used)
 * @type_uri: URI of syntax (NULL if not used)
 *
 * Get a serializer factory by name.
 * 
 * If all fields are NULL, this means any parser supporting
 * MIME Type "application/rdf+xml"
 *
 * Return value: the factory or NULL if not found
 **/
librdf_serializer_factory*
librdf_get_serializer_factory(librdf_world *world,
                              const char *name, const char *mime_type,
                              librdf_uri *type_uri) 
{
  librdf_serializer_factory *factory;
  
  librdf_world_open(world);

  if(name && !*name)
    name=NULL;
  if(!mime_type || (mime_type && !*mime_type)) {
    if(!name && !type_uri)
      name="rdfxml";
    else
      mime_type=NULL;
  }

  /* return 1st serializer if no particular one wanted */
  if(!name && !mime_type && !type_uri) {
    factory=(librdf_serializer_factory*)raptor_sequence_get_at(world->serializers, 0);
    if(!factory) {
      LIBRDF_DEBUG1("No serializers available\n");
      return NULL;
    }
  } else {
    int i;
    
    for(i=0;
        (factory=(librdf_serializer_factory*)raptor_sequence_get_at(world->serializers, i));
        i++) {
      /* next if name does not match */
      if(name && strcmp(factory->name, name))
	continue;

      /* MIME type may need to match */
      if(mime_type) {
        if(!factory->mime_type)
          continue;
        if(strcmp(factory->mime_type, mime_type))
          continue;
      }
      
      /* URI may need to match */
      if(type_uri) {
        if(!factory->type_uri)
          continue;
        
        if(!librdf_uri_equals(factory->type_uri, type_uri))
          continue;
      }

      /* found it */
      break;
    }
    /* else FACTORY with given arguments not found */
    if(!factory)
      return NULL;
  }
  
  return factory;
}


/**
 * librdf_serializer_enumerate:
 * @world: redland world object
 * @counter: index into the list of serializers
 * @name: pointer to store the name of the serializer (or NULL)
 * @label: pointer to store syntax readable label (or NULL)
 *
 * Get information on serializers.
 * 
 * Return value: non 0 on failure of if counter is out of range
 **/
int
librdf_serializer_enumerate(librdf_world* world,
                        const unsigned int counter,
                        const char **name, const char **label)
{
  librdf_serializer_factory *factory;
  
  librdf_world_open(world);

  factory=(librdf_serializer_factory*)raptor_sequence_get_at(world->serializers,
                                                         counter);
  if(!factory)
    return 1;
  
  if(name)
    *name=factory->name;
  if(label)
    *label=factory->label;
  return 0;
}


/**
 * librdf_new_serializer:
 * @world: redland world object
 * @name: the serializer factory name
 * @mime_type: the MIME type of the syntax (NULL if not used)
 * @type_uri: URI of syntax (NULL if not used)
 *
 * Constructor - create a new #librdf_serializer object.
 * 
 * Return value: new #librdf_serializer object or NULL
 **/
librdf_serializer*
librdf_new_serializer(librdf_world *world, 
                      const char *name, const char *mime_type,
                      librdf_uri *type_uri)
{
  librdf_serializer_factory* factory;

  librdf_world_open(world);

  factory=librdf_get_serializer_factory(world, name, mime_type, type_uri);
  if(!factory)
    return NULL;

  return librdf_new_serializer_from_factory(world, factory);
}


/**
 * librdf_new_serializer_from_factory:
 * @world: redland world object
 * @factory: the serializer factory to use to create this serializer
 *
 * Constructor - create a new #librdf_serializer object.
 * 
 * Return value: new #librdf_serializer object or NULL
 **/
librdf_serializer*
librdf_new_serializer_from_factory(librdf_world *world, 
                                   librdf_serializer_factory *factory)
{
  librdf_serializer* d;

  librdf_world_open(world);

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(factory, librdf_serializer_factory, NULL);

  d=(librdf_serializer*)LIBRDF_CALLOC(librdf_serializer, 1, sizeof(librdf_serializer));
  if(!d)
    return NULL;
        
  d->context=(char*)LIBRDF_CALLOC(serializer_context, 1, factory->context_length);
  if(!d->context) {
    librdf_free_serializer(d);
    return NULL;
  }

  d->world=world;
  
  d->factory=factory;

  if(factory->init)
    if(factory->init(d, d->context)) {
      librdf_free_serializer(d);
      return NULL;
    }

  return d;
}


/**
 * librdf_free_serializer:
 * @serializer: the serializer
 *
 * Destructor - destroys a #librdf_serializer object.
 * 
 **/
void
librdf_free_serializer(librdf_serializer *serializer) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN(serializer, librdf_serializer);

  if(serializer->context) {
    if(serializer->factory->terminate)
      serializer->factory->terminate(serializer->context);
    LIBRDF_FREE(serializer_context, serializer->context);
  }
  LIBRDF_FREE(librdf_serializer, serializer);
}



/* methods */

/**
 * librdf_serializer_serialize_model:
 * @serializer: the serializer
 * @handle: file handle to serialize to
 * @base_uri: the base URI to use (or NULL)
 * @model: the #librdf_model model to use
 *
 * @Deprecated: Use librdf_serializer_serialize_model_to_file_handle()
 *
 * Write a serialized #librdf_model to a FILE*.
 *
 * Return value: non 0 on failure
 **/
int
librdf_serializer_serialize_model(librdf_serializer* serializer,
                                  FILE *handle, librdf_uri* base_uri,
                                  librdf_model* model) 
{
  return librdf_serializer_serialize_model_to_file_handle(serializer,
                                                          handle, base_uri,
                                                          model);
}


/**
 * librdf_serializer_serialize_stream_to_file_handle:
 * @serializer: the serializer
 * @handle: file handle to serialize to
 * @base_uri: the base URI to use (or NULL)
 * @stream: the #librdf_stream model to use
 *
 * Write a #librdf_stream to a FILE*.
 * 
 * Return value: non 0 on failure
 **/
int
librdf_serializer_serialize_stream_to_file_handle(librdf_serializer* serializer,
                                                  FILE *handle, 
                                                  librdf_uri* base_uri,
                                                  librdf_stream* stream) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(serializer, librdf_serializer, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(handle, FILE*, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(stream, librdf_stream, 1);

  return serializer->factory->serialize_stream_to_file_handle(serializer->context,
                                                              handle, base_uri, stream);
}


/**
 * librdf_serializer_serialize_model_to_file_handle:
 * @serializer: the serializer
 * @handle: file handle to serialize to
 * @base_uri: the base URI to use (or NULL)
 * @model: the #librdf_model model to use
 *
 * Write a serialized #librdf_model to a FILE*.
 * 
 * Return value: non 0 on failure
 **/
int
librdf_serializer_serialize_model_to_file_handle(librdf_serializer* serializer,
                                                 FILE *handle, 
                                                 librdf_uri* base_uri,
                                                 librdf_model* model) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(serializer, librdf_serializer, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(handle, FILE*, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, 1);

  return serializer->factory->serialize_model_to_file_handle(serializer->context,
                                                             handle, base_uri, model);
}


/**
 * librdf_serializer_serialize_stream_to_file:
 * @serializer: the serializer
 * @name: filename to serialize to
 * @base_uri: the base URI to use (or NULL)
 * @stream: the #librdf_stream stream to use
 *
 * Write a #librdf_stream to a file.
 * 
 * Return value: non 0 on failure
 **/
int
librdf_serializer_serialize_stream_to_file(librdf_serializer* serializer,
                                           const char *name, 
                                           librdf_uri* base_uri,
                                           librdf_stream* stream) 
{
  FILE* fh;
  int status;
  
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(serializer, librdf_serializer, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(name, string, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(stream, librdf_stream, 1);

  fh=fopen(name, "w+");
  if(!fh) {
    librdf_log(serializer->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_SERIALIZER,
               NULL, "failed to open file '%s' for writing - %s",
               name, strerror(errno));
    return 1;
  }
  
  status=librdf_serializer_serialize_stream_to_file_handle(serializer, fh, 
                                                           base_uri, stream);
  fclose(fh);
  return status;
}


/**
 * librdf_serializer_serialize_model_to_file:
 * @serializer: the serializer
 * @name: filename to serialize to
 * @base_uri: the base URI to use (or NULL)
 * @model: the #librdf_model model to use
 *
 * Write a serialized #librdf_model to a file.
 * 
 * Return value: non 0 on failure
 **/
int
librdf_serializer_serialize_model_to_file(librdf_serializer* serializer,
                                          const char *name, 
                                          librdf_uri* base_uri,
                                          librdf_model* model) 
{
  FILE* fh;
  int status;
  
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(serializer, librdf_serializer, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(name, string, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, 1);

  fh=fopen(name, "w+");
  if(!fh) {
    librdf_log(serializer->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_SERIALIZER,
               NULL, "failed to open file '%s' for writing - %s",
               name, strerror(errno));
    return 1;
  }
  
  status=librdf_serializer_serialize_model_to_file_handle(serializer, fh, 
                                                          base_uri, model);
  fclose(fh);
  return status;
}


/**
 * librdf_serializer_serialize_stream_to_counted_string:
 * @serializer: the serializer
 * @base_uri: the base URI to use (or NULL)
 * @stream: the #librdf_stream stream to use
 * @length_p: pointer to store length or NULL
 *
 * Write a #librdf_stream to a counted string.
 * 
 * Return value: stream as string or NULL on failure
 **/
unsigned char*
librdf_serializer_serialize_stream_to_counted_string(librdf_serializer* serializer,
                                                     librdf_uri* base_uri,
                                                     librdf_stream* stream,
                                                     size_t* length_p) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(serializer, librdf_serializer, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(stream, librdf_stream, NULL);

  if(length_p)
    *length_p=0;
  
  return serializer->factory->serialize_stream_to_counted_string(serializer->context,
                                                                 base_uri,
                                                                 stream,
                                                                 length_p);
}


/**
 * librdf_serializer_serialize_model_to_counted_string:
 * @serializer: the serializer
 * @base_uri: the base URI to use (or NULL)
 * @model: the #librdf_model model to use
 * @length_p: pointer to store length or NULL
 *
 * Write a serialized #librdf_model to a counted string.
 * 
 * Return value: non 0 on failure
 **/
unsigned char*
librdf_serializer_serialize_model_to_counted_string(librdf_serializer* serializer,
                                                    librdf_uri* base_uri,
                                                    librdf_model* model,
                                                    size_t* length_p) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(serializer, librdf_serializer, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, NULL);

  if(length_p)
    *length_p=0;
  
  return serializer->factory->serialize_model_to_counted_string(serializer->context,
                                                                base_uri, model,
                                                                length_p);
}


/**
 * librdf_serializer_serialize_stream_to_string:
 * @serializer: the serializer
 * @base_uri: the base URI to use (or NULL)
 * @stream: the #librdf_stream stream to use
 *
 * Write a #librdf_stream to a string.
 * 
 * Return value: NULL on failure
 **/
unsigned char*
librdf_serializer_serialize_stream_to_string(librdf_serializer* serializer,
                                             librdf_uri* base_uri,
                                             librdf_stream* stream) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(serializer, librdf_serializer, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(stream, librdf_stream, NULL);

  return serializer->factory->serialize_stream_to_counted_string(serializer->context,
                                                                 base_uri,
                                                                 stream,
                                                                 NULL);
}


/**
 * librdf_serializer_serialize_model_to_string:
 * @serializer: the serializer
 * @base_uri: the base URI to use (or NULL)
 * @model: the #librdf_model model to use
 *
 * Write a serialized #librdf_model to a string.
 * 
 * Return value: NULL on failure
 **/
unsigned char*
librdf_serializer_serialize_model_to_string(librdf_serializer* serializer,
                                            librdf_uri* base_uri,
                                            librdf_model* model) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(serializer, librdf_serializer, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, NULL);

  return serializer->factory->serialize_model_to_counted_string(serializer->context,
                                                                base_uri, model,
                                                                NULL);
}


/**
 * librdf_serializer_serialize_stream_to_iostream:
 * @serializer: the serializer
 * @base_uri: the base URI to use (or NULL)
 * @stream: the #librdf_stream stream to use
 * @iostr: the #raptor_iostream to write to
 *
 * Write a #librdf_stream to a #raptor_iostream.
 * 
 * Return value: non-0 on failure
 **/
int
librdf_serializer_serialize_stream_to_iostream(librdf_serializer* serializer,
                                              librdf_uri* base_uri,
                                              librdf_stream *stream,
                                              raptor_iostream* iostr)
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(serializer, librdf_serializer, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(stream, librdf_stream, 1);

  return serializer->factory->serialize_stream_to_iostream(serializer->context,
                                                           base_uri, stream,
                                                           iostr);
}



/**
 * librdf_serializer_serialize_model_to_iostream:
 * @serializer: the serializer
 * @base_uri: the base URI to use (or NULL)
 * @model: the #librdf_model model to use
 * @iostr: the #raptor_iostream to write to
 *
 * Write a serialized #librdf_model to a #raptor_iostream.
 * 
 * Return value: non-0 on failure
 **/
int
librdf_serializer_serialize_model_to_iostream(librdf_serializer* serializer,
                                              librdf_uri* base_uri,
                                              librdf_model *model,
                                              raptor_iostream* iostr)
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(serializer, librdf_serializer, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, 1);

  return serializer->factory->serialize_model_to_iostream(serializer->context,
                                                          base_uri, model,
                                                          iostr);
}



/**
 * librdf_serializer_set_error:
 * @serializer: the serializer
 * @user_data: user data to pass to function
 * @error_fn: pointer to the function
 *
 * @Deprecated: Does nothing
 * 
 * Set the serializer error handling function.
 * 
 **/
void
librdf_serializer_set_error(librdf_serializer* serializer, void *user_data,
                            void (*error_fn)(void *user_data, const char *msg, ...))
{
}


/**
 * librdf_serializer_set_warning:
 * @serializer: the serializer
 * @user_data: user data to pass to function
 * @warning_fn: pointer to the function
 *
 * @Deprecated: Does nothing
 *
 * Set the serializer warning handling function.
 * 
 **/
void
librdf_serializer_set_warning(librdf_serializer* serializer, void *user_data,
                              void (*warning_fn)(void *user_data, const char *msg, ...))
{
}


/**
 * librdf_serializer_get_feature:
 * @serializer: serializer object
 * @feature: URI of feature
 *
 * Get the value of a serializer feature.
 * 
 * Return value: the value of the feature or NULL if no such feature
 * exists or the value is empty.
 **/
librdf_node*
librdf_serializer_get_feature(librdf_serializer* serializer, librdf_uri *feature) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(serializer, librdf_serializer, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(feature, librdf_uri, NULL);

  if(serializer->factory->get_feature)
    return serializer->factory->get_feature(serializer->context, feature);

  return NULL;
}

/**
 * librdf_serializer_set_feature:
 * @serializer: serializer object
 * @feature: URI of feature
 * @value: value to set
 *
 * Set the value of a serializer feature.
 * 
 * Return value: non 0 on failure (negative if no such feature)
 **/
  
int
librdf_serializer_set_feature(librdf_serializer* serializer,
                              librdf_uri *feature, librdf_node* value) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(serializer, librdf_serializer, -1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(feature, librdf_uri, -1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(value, librdf_node, -1);

  if(serializer->factory->set_feature)
    return serializer->factory->set_feature(serializer->context, feature, value);

  return (-1);
}

/**
 * librdf_serializer_set_namespace:
 * @serializer: serializer object
 * @uri: URI of namespace or NULL
 * @prefix: prefix to use or NULL
 *
 * Set a namespace URI/prefix mapping.
 * 
 * Return value: non 0 on failure
 **/
  
int
librdf_serializer_set_namespace(librdf_serializer* serializer,
                                librdf_uri *uri, const char *prefix) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(serializer, librdf_serializer, 1);
  if(uri && !*librdf_uri_as_string(uri))
    uri=NULL;
  if(prefix && !*prefix)
    prefix=NULL;

  if(serializer->factory->set_namespace)
    return serializer->factory->set_namespace(serializer->context, uri, prefix);
  return 1;
}

#endif


/* TEST CODE */


#ifdef STANDALONE

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* one more prototype */
int main(int argc, char *argv[]);


struct log_data {
  int errors;
  int warnings;
} LogData;


static int
log_handler(void *user_data, librdf_log_message *message) 
{
  struct log_data* ld=(struct log_data*)user_data;

  switch(message->level) {
    case LIBRDF_LOG_ERROR:
      ld->errors++;
      break;
    case LIBRDF_LOG_WARN:
      ld->warnings++;
      break;

    case LIBRDF_LOG_NONE:
    case LIBRDF_LOG_DEBUG:
    case LIBRDF_LOG_INFO:
    case LIBRDF_LOG_FATAL:
    default:
      break;
  }

  return 1;
}


#define EXPECTED_ERRORS 3
#define EXPECTED_WARNINGS 0

#define SYNTAX_TYPE "ntriples"
#define SYNTAX_CONTENT \
"<http://purl.org/net/dajobe/> <http://purl.org/dc/elements/1.1/creator> \"Dave Beckett\" .\n" \
"<http://purl.org/net/dajobe/> <http://purl.org/dc/elements/1.1/description> \"The generic home page of Dave Beckett.\" .\n" \
"<http://purl.org/net/dajobe/> <http://purl.org/dc/elements/1.1/title> \"Dave Beckett's Home Page\" . \n"


int
main(int argc, char *argv[]) 
{
  const char *program=librdf_basename((const char*)argv[0]);
  const char *test_serializer_types[]={"rdfxml", "ntriples", NULL};
  int i;
  const char *type;
  unsigned char *string;
  size_t string_length;
  librdf_world *world;
  librdf_storage *storage;
  librdf_model* model;
  librdf_uri* base_uri;
  librdf_statement* statement;
  librdf_serializer* serializer;
  librdf_parser* parser;
  librdf_stream* stream;
  FILE *fh;
  struct stat st_buf;

  world=librdf_new_world();
  librdf_world_open(world);

  librdf_world_set_logger(world, &LogData, log_handler);

  for(i=0; (type=test_serializer_types[i]); i++) {
    fprintf(stderr, "%s: Trying to create new %s serializer\n", program, type);
    serializer=librdf_new_serializer(world, type, NULL, NULL);
    if(!serializer) {
      fprintf(stderr, "%s: Failed to create new serializer type %s\n", program, type);
      continue;
    }
    
    fprintf(stderr, "%s: Freeing serializer\n", program);
    librdf_free_serializer(serializer);
  }
  

  storage=librdf_new_storage(world, NULL, NULL, NULL);
  model=librdf_new_model(world, storage, NULL);

  /* ERROR: Subject URI is bad UTF-8 */
  statement=librdf_new_statement_from_nodes(world,
    librdf_new_node_from_uri_string(world, (const unsigned char*)"http://example.org/foo\xfc"),
    librdf_new_node_from_uri_string(world, (const unsigned char*)"http://example.org/bar"),
    librdf_new_node_from_literal(world, (const unsigned char*)"blah", NULL, 0));

  librdf_model_add_statement(model, statement);
  librdf_free_statement(statement);

  /* ERROR: Predicate URI is not serializable */
  statement=librdf_new_statement_from_nodes(world,
    librdf_new_node_from_uri_string(world, (const unsigned char*)"http://example.org/foo"),
    librdf_new_node_from_uri_string(world, (const unsigned char*)"http://bad.example.org/"),
    librdf_new_node_from_literal(world, (const unsigned char*)"blah", NULL, 0));

  librdf_model_add_statement(model, statement);
  librdf_free_statement(statement);

  /* ERROR: Object literal is bad UTF-8 */
  statement=librdf_new_statement_from_nodes(world,
    librdf_new_node_from_uri_string(world, (const unsigned char*)"http://example.org/foo"),
    librdf_new_node_from_uri_string(world, (const unsigned char*)"http://example.org/abc"),
    librdf_new_node_from_literal(world, (const unsigned char*)"\xfc", NULL, 0));

  librdf_model_add_statement(model, statement);
  librdf_free_statement(statement);

  serializer=librdf_new_serializer(world, "rdfxml", NULL, NULL);
  base_uri=librdf_new_uri(world, (const unsigned char*)"http://example.org/base#");

  string=librdf_serializer_serialize_model_to_counted_string(serializer,
                                                             base_uri, model,
                                                             &string_length);
#define EXPECTED_BAD_STRING_LENGTH 382
  if(string_length != EXPECTED_BAD_STRING_LENGTH) {
    fprintf(stderr, "%s: Serialising model to RDF/XML returned string '%s' size %d, expected %d\n", program, string,
            (int)string_length, EXPECTED_BAD_STRING_LENGTH);
    return 1;
  }

  if(string)
    free(string);

  librdf_free_uri(base_uri); base_uri=NULL;
  librdf_free_model(model); model=NULL;
  librdf_free_storage(storage); storage=NULL;
  

  if(LogData.errors != EXPECTED_ERRORS) {
    fprintf(stderr, "%s: Serialising to RDF/XML returned %d errors, expected %d\n", program,
            LogData.errors, EXPECTED_ERRORS);
    return 1;
  }

  if(LogData.warnings != EXPECTED_WARNINGS) {
    fprintf(stderr, "%s: Serialising to RDF/XML returned %d warnings, expected %d\n", program,
            LogData.warnings, EXPECTED_WARNINGS);
    return 1;
  }
  

  /* Good model to serialize */
  storage=librdf_new_storage(world, NULL, NULL, NULL);
  model=librdf_new_model(world, storage, NULL);

  parser=librdf_new_parser(world, SYNTAX_TYPE, NULL, NULL);
  if(!parser) {
    fprintf(stderr, "%s: Failed to create new parser type %s\n", program, 
            SYNTAX_TYPE);
    return 1;
  }

  fprintf(stderr, "%s: Adding %s string content\n", program, SYNTAX_TYPE);
  if(librdf_parser_parse_string_into_model(parser, 
                                           (const unsigned char*)SYNTAX_CONTENT,
                                           NULL /* no base URI*/, 
                                           model)) {
    fprintf(stderr, "%s: Failed to parse RDF from %s string into model\n", 
            SYNTAX_TYPE, program);
    return 1;
  }
  librdf_free_parser(parser);
  

  fprintf(stderr, "%s: Serializing stream to a string\n", program);

  stream=librdf_model_as_stream(model);
  string_length=0;
  string=librdf_serializer_serialize_stream_to_counted_string(serializer,
                                                              NULL, stream,
                                                              &string_length);
#define EXPECTED_GOOD_STRING_LENGTH 668
  if(string_length != EXPECTED_GOOD_STRING_LENGTH) {
    fprintf(stderr, "%s: Serialising stream to RDF/XML returned string '%s' size %d, expected %d\n", program, string,
            (int)string_length, EXPECTED_GOOD_STRING_LENGTH);
    return 1;
  }
  librdf_free_stream(stream);

  if(string)
    free(string);


  fprintf(stderr, "%s: Serializing stream to a file handle\n", program);

  stream=librdf_model_as_stream(model);

#define FILENAME "test.rdf"
  fh=fopen(FILENAME, "w");
  if(!fh) {
    fprintf(stderr, "%s: Failed to fopen for writing '%s' - %s\n",
            program, FILENAME, strerror(errno));
    return 1;
  }
  librdf_serializer_serialize_stream_to_file_handle(serializer, fh, NULL, 
                                                    stream);
  fclose(fh);
  stat(FILENAME, &st_buf);
  
  if((int)st_buf.st_size != EXPECTED_GOOD_STRING_LENGTH) {
    fprintf(stderr, "%s: Serialising stream to file handle returned file '%s' of size %d bytes, expected %d\n", program, FILENAME, (int)st_buf.st_size, 
            EXPECTED_GOOD_STRING_LENGTH);
    return 1;
  }
  unlink(FILENAME);
  
  librdf_free_stream(stream);


  librdf_free_serializer(serializer); serializer=NULL;
  librdf_free_model(model); model=NULL;
  librdf_free_storage(storage); storage=NULL;


  librdf_free_world(world);
  
  /* keep gcc -Wall happy */
  return(0);
}

#endif
