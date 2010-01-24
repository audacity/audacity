/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_parser.c - RDF Parser (syntax to RDF triples) interface
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

#include <redland.h>


#ifndef STANDALONE

/**
 * librdf_init_parser:
 * @world: redland world object
 *
 * INTERNAL - Initialise the parser module.
 *
 **/
void
librdf_init_parser(librdf_world *world) 
{
  librdf_parser_raptor_constructor(world);
}


/**
 * librdf_finish_parser:
 * @world: redland world object
 *
 * INTERNAL - Terminate the parser module.
 *
 **/
void
librdf_finish_parser(librdf_world *world) 
{
  if(world->parsers) {
    raptor_free_sequence(world->parsers);
    world->parsers=NULL;
  }

  librdf_parser_raptor_destructor();
}


/* helper functions */
static void
librdf_free_parser_factory(librdf_parser_factory *factory) 
{
  if(factory->name)
    LIBRDF_FREE(cstring, factory->name);
  if(factory->label)
    LIBRDF_FREE(cstring, factory->label);
  if(factory->mime_type)
    LIBRDF_FREE(cstring, factory->mime_type);
  if(factory->type_uri)
    librdf_free_uri(factory->type_uri);
  LIBRDF_FREE(librdf_parser_factory, factory);
}


/**
 * librdf_parser_register_factory:
 * @world: redland world object
 * @name: the name of the parser
 * @label: the label of the parser (optional)
 * @mime_type: MIME type of the syntax (optional)
 * @uri_string: URI of the syntax (optional)
 * @factory: function to be called to register the factor parameters
 *
 * Register a parser factory .
 * 
 **/
void
librdf_parser_register_factory(librdf_world *world,
                               const char *name, const char *label, 
                               const char *mime_type,
                               const unsigned char *uri_string,
                               void (*factory) (librdf_parser_factory*))
{
  librdf_parser_factory *parser;

  librdf_world_open(world);

#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
  LIBRDF_DEBUG2("Received registration for parser %s\n", name);
#endif

  if(!world->parsers) {
    world->parsers=raptor_new_sequence((raptor_sequence_free_handler *)librdf_free_parser_factory, NULL);
    if(!world->parsers)
      goto oom;
  }

  parser=(librdf_parser_factory*)LIBRDF_CALLOC(librdf_parser_factory, 1,
                                               sizeof(librdf_parser_factory));
  if(!parser)
    goto oom;

  parser->name=(char*)LIBRDF_MALLOC(cstring, strlen(name)+1);
  if(!parser->name)
    goto oom_tidy;
  strcpy(parser->name, name);

  if(label) {
    parser->label=(char*)LIBRDF_MALLOC(cstring, strlen(label)+1);
    if(!parser->label)
      goto oom_tidy;
    strcpy(parser->label, label);
  }

  /* register mime type if any */
  if(mime_type) {
    parser->mime_type=(char*)LIBRDF_MALLOC(cstring, strlen(mime_type)+1);
    if(!parser->mime_type)
      goto oom_tidy;
    strcpy(parser->mime_type, mime_type);
  }

  /* register URI if any */
  if(uri_string) {
    parser->type_uri=librdf_new_uri(world, uri_string);
    if(!parser->type_uri)
      goto oom_tidy;
  }

  if(raptor_sequence_push(world->parsers, parser))
    goto oom;

  /* Call the parser registration function on the new object */
  (*factory)(parser);

#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
  LIBRDF_DEBUG3("%s has context size %d\n", name, parser->context_length);
#endif

  return;

  oom_tidy:
  librdf_free_parser_factory(parser);
  oom:
  LIBRDF_FATAL1(world, LIBRDF_FROM_PARSER, "Out of memory");
}


/**
 * librdf_get_parser_factory:
 * @world: redland world object
 * @name: the name of the factory (NULL or empty string if don't care)
 * @mime_type: the MIME type of the syntax (NULL or empty if don't care)
 * @type_uri: URI of syntax (NULL if not used)
 *
 * Get a parser factory by name.
 *
 * If all fields are NULL, this means any parser supporting
 * MIME Type "application/rdf+xml"
 * 
 * Return value: the factory or NULL if not found
 **/
librdf_parser_factory*
librdf_get_parser_factory(librdf_world *world,
                          const char *name, const char *mime_type,
                          librdf_uri *type_uri) 
{
  librdf_parser_factory *factory;
  
  librdf_world_open(world);

  if(name && !*name)
    name=NULL;
  if(!mime_type || (mime_type && !*mime_type)) {
    if(!name && !type_uri)
      mime_type="application/rdf+xml";
    else
      mime_type=NULL;
  }

  /* return 1st parser if no particular one wanted */
  if(!name && !mime_type && !type_uri) {
    factory=(librdf_parser_factory*)raptor_sequence_get_at(world->parsers, 0);
    if(!factory) {
      LIBRDF_DEBUG1("No parsers available\n");
      return NULL;
    }
  } else {
    int i;
    
    for(i=0;
        (factory=(librdf_parser_factory*)raptor_sequence_get_at(world->parsers, i));
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
 * librdf_parser_enumerate:
 * @world: redland world object
 * @counter: index into the list of parsers
 * @name: pointer to store the name of the parser (or NULL)
 * @label: pointer to store syntax readable label (or NULL)
 *
 * Get information on parsers.
 * 
 * Return value: non 0 on failure of if counter is out of range
 **/
int
librdf_parser_enumerate(librdf_world* world,
                        const unsigned int counter,
                        const char **name, const char **label)
{
  librdf_parser_factory *factory;
  
  librdf_world_open(world);

  factory=(librdf_parser_factory*)raptor_sequence_get_at(world->parsers,
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
 * librdf_new_parser:
 * @world: redland world object
 * @name: the parser factory name
 * @mime_type: the MIME type of the syntax (NULL if not used)
 * @type_uri: URI of syntax (NULL if not used)
 *
 * Constructor - create a new #librdf_parser object.
 * 
 * If all fields are NULL, this means any parser supporting
 * MIME Type "application/rdf+xml"
 * 
 * Return value: new #librdf_parser object or NULL
 **/
librdf_parser*
librdf_new_parser(librdf_world *world, 
                  const char *name, const char *mime_type,
                  librdf_uri *type_uri)
{
  librdf_parser_factory* factory;

  librdf_world_open(world);

  factory=librdf_get_parser_factory(world, name, mime_type, type_uri);
  if(!factory)
    return NULL;

  return librdf_new_parser_from_factory(world, factory);
}


/**
 * librdf_new_parser_from_factory:
 * @world: redland world object
 * @factory: the parser factory to use to create this parser
 *
 * Constructor - create a new #librdf_parser object.
 * 
 * Return value: new #librdf_parser object or NULL
 **/
librdf_parser*
librdf_new_parser_from_factory(librdf_world *world, 
                               librdf_parser_factory *factory)
{
  librdf_parser* d;

  librdf_world_open(world);

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(factory, librdf_parser_factory, NULL);

  d=(librdf_parser*)LIBRDF_CALLOC(librdf_parser, 1, sizeof(librdf_parser));
  if(!d)
    return NULL;
        
  d->context=(char*)LIBRDF_CALLOC(parser_context, 1, factory->context_length);
  if(!d->context) {
    librdf_free_parser(d);
    return NULL;
  }

  d->world=world;
  
  d->factory=factory;

  if(factory->init && factory->init(d, d->context)) {
    /* factory init failed - clean up */
    librdf_free_parser(d);
    d=NULL;
  }

  return d;
}


/**
 * librdf_free_parser:
 * @parser: the parser
 *
 * Destructor - destroys a #librdf_parser object.
 * 
 **/
void
librdf_free_parser(librdf_parser *parser) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN(parser, librdf_parser);

  if(parser->context) {
    if(parser->factory->terminate)
      parser->factory->terminate(parser->context);
    LIBRDF_FREE(parser_context, parser->context);
  }
  LIBRDF_FREE(librdf_parser, parser);
}



/* methods */

/**
 * librdf_parser_parse_as_stream:
 * @parser: the parser
 * @uri: the URI to read
 * @base_uri: the base URI to use or NULL
 *
 * Parse a URI to a librdf_stream of statements.
 * 
 * Return value: #librdf_stream of statements or NULL
 **/
librdf_stream*
librdf_parser_parse_as_stream(librdf_parser* parser, librdf_uri* uri,
                              librdf_uri* base_uri) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(parser, librdf_parser, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(uri, librdf_uri, NULL);

  if(parser->factory->parse_uri_as_stream)
    return parser->factory->parse_uri_as_stream(parser->context,
                                                uri, base_uri);

  if(!librdf_uri_is_file_uri(uri)) {
    LIBRDF_DEBUG2("%s parser can only handle file: URIs\n", parser->factory->name);
    return NULL;
  }
  return parser->factory->parse_file_as_stream(parser->context,
                                               uri, base_uri);
}


/**
 * librdf_parser_parse_into_model:
 * @parser: the parser
 * @uri: the URI to read the content
 * @base_uri: the base URI to use or NULL
 * @model: the model to use
 *
 * Parse a URI of content into an librdf_model.
 * 
 * Return value: non 0 on failure
 **/
int
librdf_parser_parse_into_model(librdf_parser* parser, librdf_uri* uri,
                               librdf_uri* base_uri, librdf_model* model) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(parser, librdf_parser, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(uri, librdf_uri, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, 1);

  if(parser->factory->parse_uri_into_model)
    return parser->factory->parse_uri_into_model(parser->context,
                                                 uri, base_uri, model);
  
  if(!librdf_uri_is_file_uri(uri)) {
    LIBRDF_DEBUG2("%s parser can only handle file: URIs\n", parser->factory->name);
    return 1;
  }
  return parser->factory->parse_file_into_model(parser->context,
                                                uri, base_uri, model);
}


/**
 * librdf_parser_parse_string_as_stream:
 * @parser: the parser
 * @string: the string to parse
 * @base_uri: the base URI to use or NULL
 *
 * Parse a string of content to a librdf_stream of statements.
 * 
 * Return value: #librdf_stream of statements or NULL
 **/
librdf_stream*
librdf_parser_parse_string_as_stream(librdf_parser* parser, 
                                     const unsigned char *string,
                                     librdf_uri* base_uri) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(parser, librdf_parser, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(string, string, NULL);

  if(parser->factory->parse_string_as_stream)
    return parser->factory->parse_string_as_stream(parser->context,
                                                   string, base_uri);

  return NULL;
}


/**
 * librdf_parser_parse_string_into_model:
 * @parser: the parser
 * @string: the content to parse
 * @base_uri: the base URI to use or NULL
 * @model: the model to use
 *
 * Parse a string of content into an librdf_model.
 * 
 * Return value: non 0 on failure
 **/
int
librdf_parser_parse_string_into_model(librdf_parser* parser, 
                                      const unsigned char *string,
                                      librdf_uri* base_uri, librdf_model* model) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(parser, librdf_parser, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(string, string, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, 1);

  if(parser->factory->parse_string_into_model)
    return parser->factory->parse_string_into_model(parser->context,
                                                    string, base_uri, model);
  
  return 1;
}


/**
 * librdf_parser_parse_counted_string_as_stream:
 * @parser: the parser
 * @string: the string to parse
 * @length: length of the string content (must be >0)
 * @base_uri: the base URI to use or NULL
 *
 * Parse a counted string of content to a librdf_stream of statements.
 * 
 * Return value: #librdf_stream of statements or NULL
 **/
librdf_stream*
librdf_parser_parse_counted_string_as_stream(librdf_parser* parser, 
                                             const unsigned char *string,
                                             size_t length,
                                             librdf_uri* base_uri)
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(parser, librdf_parser, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(string, string, NULL);
  LIBRDF_ASSERT_RETURN(length < 1, "string length is not greater than zero", 
                       NULL);

  if(parser->factory->parse_counted_string_as_stream)
    return parser->factory->parse_counted_string_as_stream(parser->context,
                                                           string, length,
                                                           base_uri);

  return NULL;
}


/**
 * librdf_parser_parse_counted_string_into_model:
 * @parser: the parser
 * @string: the content to parse
 * @length: length of content (must be >0)
 * @base_uri: the base URI to use or NULL
 * @model: the model to use
 *
 * Parse a counted string of content into an librdf_model.
 * 
 * Return value: non 0 on failure
 **/
int
librdf_parser_parse_counted_string_into_model(librdf_parser* parser, 
                                              const unsigned char *string,
                                              size_t length,
                                              librdf_uri* base_uri, 
                                              librdf_model* model)
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(parser, librdf_parser, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(string, string, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, 1);
  LIBRDF_ASSERT_RETURN(length < 1, "string length is not greater than zero", 1);

  if(parser->factory->parse_counted_string_into_model)
    return parser->factory->parse_counted_string_into_model(parser->context,
                                                            string, length,
                                                            base_uri, model);
  
  return 1;
}


/**
 * librdf_parser_parse_file_handle_as_stream:
 * @parser: the parser
 * @fh: FILE* to read content source
 * @close_fh: non-0 to fclose() the file handle on finishing
 * @base_uri: the base URI to use (or NULL)
 *
 * Parse a FILE* handle of content to a #librdf_stream of statements.
 * 
 * Return value: #librdf_stream of statements or NULL
 **/
librdf_stream*
librdf_parser_parse_file_handle_as_stream(librdf_parser* parser, FILE *fh,
                                          int close_fh, librdf_uri* base_uri)
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(parser, librdf_parser, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(fh, FILE, NULL);

  if(parser->factory->parse_file_handle_as_stream)
    return parser->factory->parse_file_handle_as_stream(parser->context, fh,
                                                        close_fh, base_uri);

  return NULL;
}


/**
 * librdf_parser_parse_file_handle_into_model:
 * @parser: the parser
 * @fh: FILE* to read content source
 * @close_fh: non-0 to fclose() the file handle on finishing
 * @base_uri: the base URI to use (or NULL)
 * @model: the model to write to
 *
 * Parse a FILE* handle of content into an #librdf_model.
 * 
 * Return value: non 0 on failure
 **/
int
librdf_parser_parse_file_handle_into_model(librdf_parser* parser, FILE *fh,
                                           int close_fh, librdf_uri* base_uri,
					   librdf_model* model) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(parser, librdf_parser, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(fh, FILE, 1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(model, librdf_model, 1);

  if(parser->factory->parse_file_handle_into_model)
    return parser->factory->parse_file_handle_into_model(parser->context, fh,
                                                         close_fh, base_uri,
							 model);

  return 1;
}


/**
 * librdf_parser_set_error:
 * @parser: the parser
 * @user_data: user data to pass to function
 * @error_fn: pointer to the function
 *
 * @Deprecated: Does nothing
 *
 * Set the parser error handling function.
 * 
 **/
void
librdf_parser_set_error(librdf_parser* parser, void *user_data,
                        void (*error_fn)(void *user_data, const char *msg, ...))
{
}


/**
 * librdf_parser_set_warning:
 * @parser: the parser
 * @user_data: user data to pass to function
 * @warning_fn: pointer to the function
 *
 * @Deprecated: Does nothing.
 *
 * Set the parser warning handling function.
 *
 **/
void
librdf_parser_set_warning(librdf_parser* parser, void *user_data,
                          void (*warning_fn)(void *user_data, const char *msg, ...))
{
}


/**
 * librdf_parser_get_feature:
 * @parser: #librdf_parser object
 * @feature: #librdf_Uuri feature property
 *
 * Get the value of a parser feature.
 * 
 * Return value: new #librdf_node feature value or NULL if no such feature
 * exists or the value is empty.
 **/
librdf_node*
librdf_parser_get_feature(librdf_parser* parser, librdf_uri* feature) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(parser, librdf_parser, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(feature, librdf_uri, NULL);

  if(parser->factory->get_feature)
    return parser->factory->get_feature(parser->context, feature);

  return NULL;
}

/**
 * librdf_parser_set_feature:
 * @parser: #librdf_parser object
 * @feature: #librdf_uri feature property
 * @value: #librdf_node feature property value
 *
 * Set the value of a parser feature.
 * 
 * Return value: non 0 on failure (negative if no such feature)
 **/
  
int
librdf_parser_set_feature(librdf_parser* parser, librdf_uri* feature, 
                          librdf_node* value) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(parser, librdf_parser, -1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(feature, librdf_uri, -1);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(value, librdf_node, -1);

  if(parser->factory->set_feature)
    return parser->factory->set_feature(parser->context, feature, value);

  return (-1);
}


/**
 * librdf_parser_get_accept_header:
 * @parser: parser
 * 
 * Get an HTTP Accept value for the parser.
 *
 * The returned string must be freed by the caller such as with
 * raptor_free_memory().
 *
 * Return value: a new Accept: header string or NULL on failure
 **/
char*
librdf_parser_get_accept_header(librdf_parser* parser)
{
  if(parser->factory->get_accept_header)
    return parser->factory->get_accept_header(parser->context);
  return NULL;
}


/**
 * librdf_parser_guess_name:
 * @mime_type: MIME type of syntax or NULL
 * @buffer: content buffer or NULL
 * @identifier: content identifier or NULL
 * 
 * Get a parser name for content with type or identiifer
 * 
 * Return value: a parser name or NULL if nothing was guessable
 **/
const char*
librdf_parser_guess_name(const char *mime_type,
                         const unsigned char *buffer, 
                         const unsigned char *identifier)
{
  int len = buffer ? strlen((const char *)buffer) : 0;
  return raptor_guess_parser_name(NULL, mime_type, buffer,
                                  len,
                                  identifier);
}


/**
 * librdf_parser_get_namespaces_seen_prefix:
 * @parser: #librdf_parser object
 * @offset: index into list of namespaces
 * 
 * Get the prefix of namespaces seen during parsing
 * 
 * Return value: prefix or NULL if no such namespace prefix
 **/
const char*
librdf_parser_get_namespaces_seen_prefix(librdf_parser* parser, int offset)
{
  if(parser->factory->get_namespaces_seen_prefix)
    return parser->factory->get_namespaces_seen_prefix(parser->context, offset);
  return NULL;
}


/**
 * librdf_parser_get_namespaces_seen_uri:
 * @parser: #librdf_parser object
 * @offset: index into list of namespaces
 * 
 * Get the uri of namespaces seen during parsing
 * 
 * Return value: uri or NULL if no such namespace uri
 **/
librdf_uri*
librdf_parser_get_namespaces_seen_uri(librdf_parser* parser, int offset)
{
  if(parser->factory->get_namespaces_seen_uri)
    return parser->factory->get_namespaces_seen_uri(parser->context, offset);
  return NULL;
}

/**
 * librdf_parser_get_namespaces_seen_count:
 * @parser: #librdf_parser object
 * 
 * Get the number of namespaces seen during parsing
 * 
 * Return value: uri or NULL if no such namespace uri
 **/
int
librdf_parser_get_namespaces_seen_count(librdf_parser* parser)
{
  if(parser->factory->get_namespaces_seen_count)
    return parser->factory->get_namespaces_seen_count(parser->context);
  return 0;
}


/**
 * librdf_parser_set_uri_filter:
 * @parser: #librdf_parser object
 * @filter: URI filter function
 * @user_data: User data to pass to filter function
 * 
 * Set URI filter function for retrieval during parsing.
**/
void
librdf_parser_set_uri_filter(librdf_parser* parser,
                             librdf_uri_filter_func filter, void* user_data)
{
  parser->uri_filter=filter;
  parser->uri_filter_user_data=user_data;
}

/**
 * librdf_parser_get_uri_filter:
 * @parser: #librdf_parser object
 * @user_data_p: Pointer to user data to return
 * 
 * Get the current URI filter function for retrieval during parsing.
 *
 * Return value: current URI filter function
**/
librdf_uri_filter_func
librdf_parser_get_uri_filter(librdf_parser* parser, void** user_data_p)
{
  if(user_data_p)
    *user_data_p=parser->uri_filter_user_data;
  return parser->uri_filter;
}

#endif


/* TEST CODE */


#ifdef STANDALONE

/* one more prototype */
int main(int argc, char *argv[]);


#define RDFXML_CONTENT \
"<?xml version=\"1.0\"?>\n" \
"<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\n" \
"         xmlns:dc=\"http://purl.org/dc/elements/1.1/\">\n" \
"  <rdf:Description rdf:about=\"http://purl.org/net/dajobe/\">\n" \
"    <dc:title>Dave Beckett's Home Page</dc:title>\n" \
"    <dc:creator>Dave Beckett</dc:creator>\n" \
"    <dc:description>The generic home page of Dave Beckett.</dc:description>\n" \
"  </rdf:Description>\n" \
"</rdf:RDF>"

#define NTRIPLES_CONTENT \
"<http://purl.org/net/dajobe/> <http://purl.org/dc/elements/1.1/creator> \"Dave Beckett\" .\n" \
"<http://purl.org/net/dajobe/> <http://purl.org/dc/elements/1.1/description> \"The generic home page of Dave Beckett.\" .\n" \
"<http://purl.org/net/dajobe/> <http://purl.org/dc/elements/1.1/title> \"Dave Beckett's Home Page\" . \n"


#define TURTLE_CONTENT \
"@prefix dc: <http://purl.org/dc/elements/1.1/> .\n" \
"\n" \
"<http://purl.org/net/dajobe/>\n" \
"      dc:creator \"Dave Beckett\" ;\n" \
"      dc:description \"The generic home page of Dave Beckett.\" ; \n" \
"      dc:title \"Dave Beckett's Home Page\" . \n"

/* All the examples above give the same three triples */
#define EXPECTED_TRIPLES_COUNT 3


int
main(int argc, char *argv[]) 
{
  const char *test_parser_types[]={"rdfxml", "ntriples", "turtle", NULL};
  #define URI_STRING_COUNT 3
  const unsigned char *file_uri_strings[URI_STRING_COUNT]={(const unsigned char*)"http://example.org/test1.rdf", (const unsigned char*)"http://example.org/test2.nt", (const unsigned char*)"http://example.org/test3.ttl"};
  const unsigned char *file_content[URI_STRING_COUNT]={(const unsigned char*)RDFXML_CONTENT, (const unsigned char*)NTRIPLES_CONTENT, (const unsigned char*)TURTLE_CONTENT};
  librdf_uri* uris[URI_STRING_COUNT];
  int i;
  const char *type;
  const char *program=librdf_basename((const char*)argv[0]);
  librdf_world *world;
  int failures;
  
  world=librdf_new_world();
  librdf_world_open(world);

  for (i=0; i<URI_STRING_COUNT; i++) {
    uris[i]=librdf_new_uri(world, file_uri_strings[i]);
  }

  failures=0;
  for(i=0; (type=test_parser_types[i]); i++) {
    librdf_storage* storage=NULL;
    librdf_model *model=NULL;
    librdf_parser* parser=NULL;
    librdf_stream *stream=NULL;
    size_t length=strlen((const char*)file_content[i]);
    int size;
    char *accept_h;

    if(i>0)
      break;
    
    fprintf(stderr, "%s: Testing parsing syntax '%s'\n", program, type);

    fprintf(stderr, "%s: Creating storage and model\n", program);
    storage=librdf_new_storage(world, NULL, NULL, NULL);
    if(!storage) {
      fprintf(stderr, "%s: Failed to create new storage\n", program);
      return(1);
    }
    model=librdf_new_model(world, storage, NULL);
    if(!model) {
      fprintf(stderr, "%s: Failed to create new model\n", program);
      return(1);
    }

    fprintf(stderr, "%s: Creating %s parser\n", program, type);
    parser=librdf_new_parser(world, type, NULL, NULL);
    if(!parser) {
      fprintf(stderr, "%s: Failed to create new parser type %s\n", program, type);
      failures++;
      goto tidy_test;
    }


    accept_h=librdf_parser_get_accept_header(parser);
    if(accept_h) {
      fprintf(stderr, "%s: Parser accept header: '%s'\n", program, accept_h);
      free(accept_h);
    } else
      fprintf(stderr, "%s: Parser has no accept header\n", program);
    

    fprintf(stderr, "%s: Adding %s counted string content as stream\n", 
            program, type);
    if(!(stream=librdf_parser_parse_counted_string_as_stream(parser, 
                                                             file_content[i],
                                                             length,
                                                             uris[i]))) {
      fprintf(stderr, "%s: Failed to parse RDF from counted string %d as stream\n", program, i);
      failures++;
      goto tidy_test;
    }
    librdf_model_add_statements(model, stream);
    librdf_free_stream(stream);
    stream=NULL;


    size=librdf_model_size(model);
    fprintf(stderr, "%s: Model size is %d triples\n", program, size);
    if(size != EXPECTED_TRIPLES_COUNT) {
      fprintf(stderr, "%s: Returned %d triples, not %d as expected\n",
              program, size, EXPECTED_TRIPLES_COUNT);
      failures++;
      goto tidy_test;
    }


    fprintf(stderr, "%s: Adding %s string content as stream\n", program, type);
    if(!(stream=librdf_parser_parse_string_as_stream(parser, 
                                                     file_content[i],
                                                     uris[i]))) {
      fprintf(stderr, "%s: Failed to parse RDF from string %d as stream\n", program, i);
      failures++;
      goto tidy_test;
    }
    librdf_model_add_statements(model, stream);
    librdf_free_stream(stream);
    stream=NULL;
    
    size=librdf_model_size(model);
    fprintf(stderr, "%s: Model size is %d triples\n", program, size);
    if(size != EXPECTED_TRIPLES_COUNT) {
      fprintf(stderr, "%s: Returned %d triples, not %d as expected\n",
              program, size, EXPECTED_TRIPLES_COUNT);
      failures++;
    }
    

    fprintf(stderr, "%s: Adding %s counted string content\n", program, type);
    if(librdf_parser_parse_counted_string_into_model(parser, 
                                                     file_content[i],
                                                     length,
                                                     uris[i], model)) {
      fprintf(stderr, "%s: Failed to parse RDF from counted string %d into model\n", program, i);
      failures++;
      goto tidy_test;
    }
    
    size=librdf_model_size(model);
    fprintf(stderr, "%s: Model size is %d triples\n", program, size);
    if(size != EXPECTED_TRIPLES_COUNT) {
      fprintf(stderr, "%s: Returned %d triples, not %d as expected\n",
              program, size, EXPECTED_TRIPLES_COUNT);
      failures++;
      goto tidy_test;
    }


    fprintf(stderr, "%s: Adding %s string content\n", program, type);
    if(librdf_parser_parse_string_into_model(parser, 
                                             file_content[i],
                                             uris[i], model)) {
      fprintf(stderr, "%s: Failed to parse RDF from string %d into model\n", program, i);
      failures++;
      goto tidy_test;
    }


    for(i=0; i < librdf_parser_get_namespaces_seen_count(parser); i++) {
      const char* prefix=librdf_parser_get_namespaces_seen_prefix(parser, i);
      librdf_uri* uri=librdf_parser_get_namespaces_seen_uri(parser, i);

      fprintf(stderr, "%s: Saw namespace %d): prefix:%s URI:%s\n", program, i,
              (!prefix ? "" : (const char*)prefix),
              (!uri ? "(none)" : (const char*)librdf_uri_as_string(uri)));
      
    }
    

    size=librdf_model_size(model);
    fprintf(stderr, "%s: Model size is %d triples\n", program, size);
    if(size != EXPECTED_TRIPLES_COUNT) {
      fprintf(stderr, "%s: Returned %d triples, not %d as expected\n",
              program, size, EXPECTED_TRIPLES_COUNT);
      failures++;
      goto tidy_test;
    }

    
    fprintf(stderr, "%s: Freeing parser, model and storage\n", program);
  tidy_test:
    if(stream)
      librdf_free_stream(stream);
    if(parser)
      librdf_free_parser(parser);
    if(model)
      librdf_free_model(model);
    if(storage)
      librdf_free_storage(storage);
  }
  

  fprintf(stderr, "%s: Freeing URIs\n", program);
  for (i=0; i<URI_STRING_COUNT; i++) {
    librdf_free_uri(uris[i]);
  }
  
  librdf_free_world(world);
  
  return failures;
}

#endif
