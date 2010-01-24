/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_node.c - RDF Node (RDF URI, Literal, Blank Node) Interface
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
/* needed for utf8 functions and definition of 'byte' */
#include <rdf_utf8.h>


#ifndef STANDALONE

/* hashes - resource, literal, blank */
enum {
  H_RESOURCE,
  H_LITERAL,
  H_BLANK,
  H_LAST=H_BLANK
};

#define H_COUNT (H_LAST+1)


/* class functions */

/**
 * librdf_init_node:
 * @world: redland world object
 *
 * INTERNAL - Initialise the node module.
 * 
 **/
void
librdf_init_node(librdf_world* world) 
{
  int i;
  
  for(i=0; i<H_COUNT; i++) {
    world->nodes_hash[i]=librdf_new_hash(world, NULL);
    if(!world->nodes_hash[i])
      LIBRDF_FATAL1(world, LIBRDF_FROM_NODE, "Failed to create Nodes hash from factory");
    
    if(librdf_hash_open(world->nodes_hash[i], NULL, 0, 1, 1, NULL))
      LIBRDF_FATAL1(world, LIBRDF_FROM_NODE, "Failed to open Nodes hash");
  }
}


/**
 * librdf_finish_node:
 * @world: redland world object
 *
 * INTERNAL - Terminate the node module.
 *
 **/
void
librdf_finish_node(librdf_world *world)
{
  int i;

  for(i=0; i<H_COUNT; i++) {
    if(world->nodes_hash[i]) {
      librdf_hash_close(world->nodes_hash[i]);
      librdf_free_hash(world->nodes_hash[i]);
    }
  }
}



/* constructors */

/**
 * librdf_new_node:
 * @world: redland world object
 *
 * Constructor - create a new #librdf_node object with a private identifier.
 * 
 * Calls librdf_new_node_from_blank_identifier(world, NULL) to
 * construct a new redland blank node identifier and make a
 * new librdf_node object for it.
 *
 * Return value: a new #librdf_node object or NULL on failure
 **/
librdf_node*
librdf_new_node(librdf_world *world)
{
  librdf_world_open(world);

  return librdf_new_node_from_blank_identifier(world, (unsigned char*)NULL);
}

    

/**
 * librdf_new_node_from_uri_string_or_uri:
 * @world: redland world object
 * @uri_string: string representing a URI
 * @uri: #librdf_uri object
 *
 * INTERNAL Constructor - create a new #librdf_node object from a URI string or URI object.
 * 
 * Return value: a new #librdf_node object or NULL on failure
 **/
static librdf_node*
librdf_new_node_from_uri_string_or_uri(librdf_world *world, 
                                       const unsigned char *uri_string, 
                                       librdf_uri *uri) 
{
  librdf_node* new_node;
  librdf_uri *new_uri;
  librdf_hash_datum key, value; /* on stack - not allocated */
  librdf_hash_datum *old_value;

  librdf_world_open(world);

  LIBRDF_ASSERT_RETURN((uri_string == NULL && uri == NULL), 
                       "both uri_string and uri are NULL", NULL);

  if(!uri_string && !uri)
    return NULL;

  if(uri_string && uri) {
    LIBRDF_DEBUG3("Called with both a URI string %s and object URI %s\n", uri_string, librdf_uri_as_string(uri));
    return NULL;
  }

  if(uri_string) {
    new_uri=librdf_new_uri(world, uri_string);
    if(!new_uri)
      return NULL;
  } else
    new_uri=librdf_new_uri_from_uri(uri);
  

#ifdef WITH_THREADS
  pthread_mutex_lock(world->nodes_mutex);
#endif
  
  key.data=&new_uri;
  key.size=sizeof(librdf_uri*);

  /* if the existing node found in resource hash, return it */
  if((old_value=librdf_hash_get_one(world->nodes_hash[H_RESOURCE], &key))) {
    new_node=*(librdf_node**)old_value->data;

    librdf_free_uri(new_uri);
    
#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
    LIBRDF_DEBUG3("Found existing resource node with URI %s in hash with current usage %d\n", uri_string, new_node->usage);
#endif

    librdf_free_hash_datum(old_value);
    new_node->usage++;

    goto unlock;
  }


  /* otherwise create a new one */

#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
  LIBRDF_DEBUG2("Creating new resource node with URI %s in hash\n", uri_string);
#endif

  new_node = (librdf_node*)LIBRDF_CALLOC(librdf_node, 1, sizeof(librdf_node));
  if(!new_node) {
    librdf_free_uri(new_uri);
    goto unlock;
  }
  
  new_node->world=world;
  new_node->value.resource.uri=new_uri;
  new_node->type = LIBRDF_NODE_TYPE_RESOURCE;

  new_node->usage=1;

  value.data=&new_node; value.size=sizeof(librdf_node*);

  /* store in hash: (librdf_uri*)uri => (librdf_node*) */
  if(librdf_hash_put(world->nodes_hash[H_RESOURCE], &key, &value)) {
    LIBRDF_FREE(librdf_node, new_node);
    librdf_free_uri(new_uri);
    new_node=NULL;
  }

  
 unlock:
#ifdef WITH_THREADS
  pthread_mutex_unlock(world->nodes_mutex);
#endif

  return new_node;
}

    

/**
 * librdf_new_node_from_uri_string:
 * @world: redland world object
 * @uri_string: string representing a URI
 *
 * Constructor - create a new #librdf_node object from a URI string.
 * 
 * Return value: a new #librdf_node object or NULL on failure
 **/
librdf_node*
librdf_new_node_from_uri_string(librdf_world *world, 
                                const unsigned char *uri_string) 
{
  librdf_world_open(world);

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(uri_string, string, NULL);

  return librdf_new_node_from_uri_string_or_uri(world, uri_string, NULL);
}

    

/* Create a new (Resource) Node and set the URI. */

/**
 * librdf_new_node_from_uri:
 * @world: redland world object
 * @uri: #librdf_uri object
 *
 * Constructor - create a new resource #librdf_node object with a given URI.
 *
 * Return value: a new #librdf_node object or NULL on failure
 **/
librdf_node*
librdf_new_node_from_uri(librdf_world *world, librdf_uri *uri) 
{
  librdf_world_open(world);

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(uri, librdf_uri, NULL);

  return librdf_new_node_from_uri_string_or_uri(world, NULL, uri);
}


/**
 * librdf_new_node_from_uri_local_name:
 * @world: redland world object
 * @uri: #librdf_uri object
 * @local_name: local name to append to URI
 *
 * Constructor - create a new resource #librdf_node object with a given URI and local name.
 *
 * Return value: a new #librdf_node object or NULL on failure
 **/
librdf_node*
librdf_new_node_from_uri_local_name(librdf_world *world, 
                                    librdf_uri *uri, 
                                    const unsigned char *local_name) 
{
  librdf_uri *new_uri;
  librdf_node* new_node;

  librdf_world_open(world);

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(uri, librdf_uri, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(local_name, string, NULL);

  new_uri=librdf_new_uri_from_uri_local_name(uri, local_name);
  if(!new_uri)
    return NULL;

  new_node=librdf_new_node_from_uri_string_or_uri(world, NULL, new_uri);

  librdf_free_uri(new_uri);

  return new_node;
}


/**
 * librdf_new_node_from_normalised_uri_string:
 * @world: redland world object
 * @uri_string: string representing a URI
 * @source_uri: source URI
 * @base_uri: base URI
 *
 * Constructor - create a new #librdf_node object from a URI string normalised to a new base URI.
 * 
 * Return value: a new #librdf_node object or NULL on failure
 **/
librdf_node*
librdf_new_node_from_normalised_uri_string(librdf_world *world, 
                                           const unsigned char *uri_string,
                                           librdf_uri *source_uri,
                                           librdf_uri *base_uri)
{
  librdf_uri* new_uri;
  librdf_node* new_node;
  
  librdf_world_open(world);

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(uri_string, string, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(source_uri, librdf_uri, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(base_uri, librdf_uri, NULL);

  new_uri=librdf_new_uri_normalised_to_base(uri_string, source_uri, base_uri);
  if(!new_uri)
    return NULL;

  new_node=librdf_new_node_from_uri_string_or_uri(world, NULL, new_uri);
  librdf_free_uri(new_uri);
  
  return new_node;
}


/**
 * librdf_new_node_from_literal:
 * @world: redland world object
 * @string: literal string value
 * @xml_language: literal XML language (or NULL, empty string)
 * @is_wf_xml: non 0 if literal is XML
 *
 * Constructor - create a new literal #librdf_node object.
 * 
 * 0.9.12: xml_space argument deleted
 *
 * An @xml_language cannot be used when @is_wf_xml is non-0. If both
 * are given, NULL is returned.  If @xml_language is the empty string,
 * it is the equivalent to NULL.
 *
 * Return value: new #librdf_node object or NULL on failure
 **/
librdf_node*
librdf_new_node_from_literal(librdf_world *world, 
                             const unsigned char *string, 
                             const char *xml_language, 
                             int is_wf_xml) 
{
  size_t xml_language_len=0;
  
  librdf_world_open(world);

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(string, string, NULL);

  if(xml_language && !*xml_language)
    xml_language=NULL;

  if(xml_language) {
    if(is_wf_xml)
      return NULL;
    xml_language_len=strlen(xml_language);
  }
    

  return librdf_new_node_from_typed_counted_literal(world,
                                                    string, 
                                                    strlen((const char*)string),
                                                    xml_language,
                                                    xml_language_len,
                                                    (is_wf_xml ? 
                                                     LIBRDF_RS_XMLLiteral_URI(world) : NULL)
                                                    );
}


/**
 * librdf_new_node_from_typed_counted_literal:
 * @world: redland world object
 * @value: literal string value
 * @value_len: literal string value length
 * @xml_language: literal XML language (or NULL, empty string)
 * @xml_language_len: literal XML language length (not used if @xml_language is NULL)
 * @datatype_uri: URI of typed literal datatype or NULL
 *
 * Constructor - create a new typed literal #librdf_node object.
 * 
 * Only one of @xml_language or @datatype_uri may be given.  If both
 * are given, NULL is returned.  If @xml_language is the empty string,
 * it is the equivalent to NULL.
 *
 * Return value: new #librdf_node object or NULL on failure
 **/
librdf_node*
librdf_new_node_from_typed_counted_literal(librdf_world *world, 
                                           const unsigned char *value,
                                           size_t value_len,
                                           const char *xml_language, 
                                           size_t xml_language_len,
                                           librdf_uri* datatype_uri) 
{
  librdf_node* new_node;
  unsigned char *new_value;
  char *new_xml_language;
  librdf_hash_datum key, value_hd; /* on stack - not allocated */
  librdf_hash_datum *old_value;
  size_t size;
  unsigned char *buffer;
  
  librdf_world_open(world);

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(value, string, NULL);

  if(xml_language && !*xml_language)
    xml_language=NULL;

  if(xml_language && datatype_uri)
    return NULL;
  
#ifdef WITH_THREADS
  pthread_mutex_lock(world->nodes_mutex);
#endif

  new_node = (librdf_node*)LIBRDF_CALLOC(librdf_node, 1, sizeof(librdf_node));
  if(!new_node)
    goto unlock;

  new_node->world=world;
  
  /* set type */
  new_node->type=LIBRDF_NODE_TYPE_LITERAL;

  /* the only time the string literal length should ever be measured */
  new_node->value.literal.string_len = value_len;
  
  new_value=(unsigned char*)LIBRDF_MALLOC(cstring, value_len + 1);
  if(!new_value) {
    LIBRDF_FREE(librdf_node, new_node);
    new_node=NULL;
    goto unlock;
  }
  strncpy((char*)new_value, (const char*)value, value_len);
  new_value[value_len]='\0';
  new_node->value.literal.string=new_value;
  
  if(xml_language && *xml_language) {
    new_xml_language=(char*)LIBRDF_MALLOC(cstring, xml_language_len + 1);
    if(!new_xml_language) {
      LIBRDF_FREE(cstring, new_value);
      LIBRDF_FREE(librdf_node, new_node);
      new_node=NULL;
      goto unlock;
    }
    strncpy(new_xml_language, xml_language, xml_language_len);
    new_xml_language[xml_language_len]='\0';
    new_node->value.literal.xml_language=new_xml_language;
    new_node->value.literal.xml_language_len=xml_language_len;
  } else
    new_xml_language=NULL;
  
  if(datatype_uri) {
    datatype_uri=librdf_new_uri_from_uri(datatype_uri);
    new_node->value.literal.datatype_uri=datatype_uri;
  }
  

  size=librdf_node_encode(new_node, NULL, 0);
  if(size)
    buffer=(unsigned char*)LIBRDF_MALLOC(cstring, size);
  else
    buffer=NULL;
  
  if(!buffer) {
    if(new_xml_language)
      LIBRDF_FREE(cstring, new_xml_language);
    if(datatype_uri)
      librdf_free_uri(datatype_uri);
    LIBRDF_FREE(cstring, new_value);
    LIBRDF_FREE(librdf_node, new_node);
    return NULL;
  }

  new_node->value.literal.size=size;
  new_node->value.literal.key=buffer;
  librdf_node_encode(new_node, buffer, size);

  key.data=buffer;
  key.size=size;

  /* if the existing node found in resource hash, return it */
  if((old_value=librdf_hash_get_one(world->nodes_hash[H_LITERAL], &key))) {
    LIBRDF_FREE(cstring, buffer);
    if(new_xml_language)
      LIBRDF_FREE(cstring, new_xml_language);
    if(datatype_uri)
      librdf_free_uri(datatype_uri);
    LIBRDF_FREE(cstring, new_value);
    LIBRDF_FREE(librdf_node, new_node);

    new_node=*(librdf_node**)old_value->data;

#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
    LIBRDF_DEBUG3("Found existing resource node with typed literal %s in hash with current usage %d\n", value, new_node->usage);
#endif

    librdf_free_hash_datum(old_value);
    new_node->usage++;

    goto unlock;
  }
    
  /* otherwise add the new node */
  new_node->usage=1;

  value_hd.data=&new_node; value_hd.size=sizeof(librdf_node*);

  /* store in hash: (serialised node) => (librdf_node*) */
  if(librdf_hash_put(world->nodes_hash[H_LITERAL], &key, &value_hd)) {
    LIBRDF_FREE(cstring, buffer);
    if(new_xml_language)
      LIBRDF_FREE(cstring, new_xml_language);
    if(datatype_uri)
      librdf_free_uri(datatype_uri);
    LIBRDF_FREE(cstring, new_value);
    LIBRDF_FREE(librdf_node, new_node);

    new_node=NULL;
  }

  
 unlock:
#ifdef WITH_THREADS
    pthread_mutex_unlock(world->nodes_mutex);
#endif

  return new_node;
}


/**
 * librdf_new_node_from_typed_literal:
 * @world: redland world object
 * @value: literal string value
 * @xml_language: literal XML language (or NULL, empty string)
 * @datatype_uri: URI of typed literal datatype or NULL
 *
 * Constructor - create a new typed literal #librdf_node object.
 * 
 * Only one of @xml_language or @datatype_uri may be given.  If both
 * are given, NULL is returned.  If @xml_language is the empty string,
 * it is the equivalent to NULL.
 *
 * Return value: new #librdf_node object or NULL on failure
 **/
librdf_node*
librdf_new_node_from_typed_literal(librdf_world *world, 
                                   const unsigned char *value,
                                   const char *xml_language, 
                                   librdf_uri* datatype_uri) 
{
  size_t xml_language_len=0;

  librdf_world_open(world);

  if(xml_language)
    xml_language_len=strlen(xml_language);
  
  return librdf_new_node_from_typed_counted_literal(world, value,
                                                    strlen((const char*)value),
                                                    xml_language,
                                                    xml_language_len,
                                                    datatype_uri);
}

/**
 * librdf_new_node_from_blank_identifier:
 * @world: redland world object
 * @identifier: blank node identifier or NULL
 *
 * Constructor - create a new blank node #librdf_node object from a blank node identifier.
 *
 * If no identifier string is given, creates a new internal identifier
 * and assigns it.
 * 
 * Return value: new #librdf_node object or NULL on failure
 **/
librdf_node*
librdf_new_node_from_blank_identifier(librdf_world *world,
                                      const unsigned char *identifier)
{
  librdf_node* new_node;
  unsigned char *new_identifier;
  int len;
  librdf_hash_datum key, value; /* on stack - not allocated */
  librdf_hash_datum *old_value;

  librdf_world_open(world);

#ifdef WITH_THREADS
  pthread_mutex_lock(world->nodes_mutex);
#endif

  if(!identifier) {
    new_identifier=librdf_world_get_genid(world);
    if(!new_identifier) {
      new_node=NULL;
      goto unlock;
    }
    len=strlen((const char *)new_identifier);
  } else {
    len=strlen((const char*)identifier);
    
    new_identifier=(unsigned char*)LIBRDF_MALLOC(cstring, len+1);
    if(!new_identifier) {
      new_node=NULL;
      goto unlock;
    }
    strcpy((char*)new_identifier, (const char*)identifier);
  }

  key.data=new_identifier;
  key.size=len;

  /* if the existing node found in resource hash, return it */
  if((old_value=librdf_hash_get_one(world->nodes_hash[H_BLANK], &key))) {
    new_node=*(librdf_node**)old_value->data;

    LIBRDF_FREE(cstring, new_identifier);
    
#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
    LIBRDF_DEBUG3("Found existing blank node identifier %s in hash with current usage %d\n", new_identifier, new_node->usage);
#endif

    librdf_free_hash_datum(old_value);
    new_node->usage++;

    goto unlock;
  }


  new_node = (librdf_node*)LIBRDF_CALLOC(librdf_node, 1, sizeof(librdf_node));
  if(!new_node) {
    LIBRDF_FREE(cstring, new_identifier);
    goto unlock;
  }

  new_node->world=world;
  new_node->value.blank.identifier=new_identifier;
  new_node->value.blank.identifier_len=len;
  new_node->type=LIBRDF_NODE_TYPE_BLANK;

  new_node->usage=1;

  value.data=&new_node; value.size=sizeof(librdf_node*);

  /* store in hash: (blank node ID string) => (librdf_node*) */
  if(librdf_hash_put(world->nodes_hash[H_BLANK], &key, &value)) {
    LIBRDF_FREE(librdf_node, new_node);
    LIBRDF_FREE(cstring, new_identifier);
    new_node=NULL;
  }


 unlock:
#ifdef WITH_THREADS
    pthread_mutex_unlock(world->nodes_mutex);
#endif

  return new_node;
}


/**
 * librdf_new_node_from_node:
 * @node: #librdf_node object to copy
 *
 * Copy constructor - create a new librdf_node object from an existing librdf_node object.
 * 
 * Return value: a new #librdf_node object or NULL on failure
 **/
librdf_node*
librdf_new_node_from_node(librdf_node *node) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(node, librdf_node, NULL);

  node->usage++;
  return node;
}


/**
 * librdf_free_node:
 * @node: #librdf_node object
 *
 * Destructor - destroy an #librdf_node object.
 * 
 **/
void
librdf_free_node(librdf_node *node) 
{
  librdf_hash_datum key; /* on stack */
#ifdef WITH_THREADS
  librdf_world *world;
#endif

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN(node, librdf_node);

#ifdef WITH_THREADS
  world = node->world;
  pthread_mutex_lock(world->nodes_mutex);
#endif

  node->usage--;
  
#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
  LIBRDF_DEBUG3("Node %p usage count now %d\n", node, node->usage);
#endif

  /* decrement usage, don't free if not 0 yet*/
  if(node->usage) {
#ifdef WITH_THREADS
    pthread_mutex_unlock(world->nodes_mutex);
#endif
    return;
  }

#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
  LIBRDF_DEBUG2("Deleting Node %p from hash\n", node);
#endif

  switch(node->type) {
    case LIBRDF_NODE_TYPE_RESOURCE:
      key.data=&node->value.resource.uri;
      key.size=sizeof(librdf_uri*);
      /* Hash deletion fails only if the key is not found.
         This is not a fatal error so do not check for return value. */
      librdf_hash_delete_all(node->world->nodes_hash[H_RESOURCE], &key);
      librdf_free_uri(node->value.resource.uri);
      break;
      
    case LIBRDF_NODE_TYPE_LITERAL:
      if(node->value.literal.key) {
        key.data=node->value.literal.key;
        key.size=node->value.literal.size;
        librdf_hash_delete_all(node->world->nodes_hash[H_LITERAL], &key); /* see above */
        LIBRDF_FREE(cstring, node->value.literal.key);
      }
      
      if(node->value.literal.string != NULL)
        LIBRDF_FREE(cstring, node->value.literal.string);
      if(node->value.literal.xml_language != NULL)
        LIBRDF_FREE(cstring, node->value.literal.xml_language);
      if(node->value.literal.datatype_uri != NULL)
        librdf_free_uri(node->value.literal.datatype_uri);
      break;

    case LIBRDF_NODE_TYPE_BLANK:
      key.data=node->value.blank.identifier;
      key.size=node->value.blank.identifier_len;
      librdf_hash_delete_all(node->world->nodes_hash[H_BLANK], &key); /* see above */
      if(node->value.blank.identifier != NULL)
        LIBRDF_FREE(cstring, node->value.blank.identifier);
      break;

    case LIBRDF_NODE_TYPE_UNKNOWN:
    default:
      break;
  }

#ifdef WITH_THREADS
  pthread_mutex_unlock(world->nodes_mutex);
#endif

  LIBRDF_FREE(librdf_node, node);
}


/* functions / methods */

/**
 * librdf_node_get_uri:
 * @node: the node object
 *
 * Get the URI for a node object.
 *
 * Returns a pointer to the URI object held by the node, it must be
 * copied if it is wanted to be used by the caller.
 * 
 * Return value: URI object or NULL if node has no URI.
 **/
librdf_uri*
librdf_node_get_uri(librdf_node* node) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(node, librdf_node, NULL);

  if(node->type != LIBRDF_NODE_TYPE_RESOURCE)
    return NULL;
  
  return node->value.resource.uri;
}


/**
 * librdf_node_get_type:
 * @node: the node object
 *
 * Get the type of the node.
 * 
 * Return value: the node type
 **/
librdf_node_type
librdf_node_get_type(librdf_node* node) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(node, librdf_node, LIBRDF_NODE_TYPE_UNKNOWN);

  return node->type;
}


#ifdef LIBRDF_DEBUG
/* FIXME: For debugging purposes only */
static const char* const librdf_node_type_names[] =
{"Unknown", "Resource", "Literal", "<Unused1>", "Blank"};


/*
 * librdf_node_get_type_as_string - Get a string representation for the type of the node
 * @type: the node type 
 * 
 * The type is that returned by the librdf_node_get_type method
 *
 * Return value: a pointer to a shared copy of the string or NULL if unknown.
 **/
const char*
librdf_node_get_type_as_string(int type)
{
  if(type < 0 || type > LIBRDF_NODE_TYPE_LAST)
    return NULL;
  return librdf_node_type_names[type];
}
#endif





/**
 * librdf_node_get_literal_value:
 * @node: the node object
 *
 * Get the string literal value of the node.
 * 
 * Returns a pointer to the literal value held by the node, it must be
 * copied if it is wanted to be used by the caller.
 *
 * Return value: the literal string or NULL if node is not a literal
 **/
unsigned char*
librdf_node_get_literal_value(librdf_node* node) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(node, librdf_node, NULL);

  if(node->type != LIBRDF_NODE_TYPE_LITERAL)
    return NULL;
  return node->value.literal.string;
}


/**
 * librdf_node_get_literal_value_as_counted_string:
 * @node: the node object
 * @len_p: pointer to location to store length (or NULL)
 *
 * Get the string literal value of the node as a counted string.
 * 
 * Returns a pointer to the literal value held by the node, it must be
 * copied if it is wanted to be used by the caller.
 *
 * Return value: the literal string or NULL if node is not a literal
 **/
unsigned char*
librdf_node_get_literal_value_as_counted_string(librdf_node* node, 
                                                size_t *len_p) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(node, librdf_node, NULL);
  LIBRDF_ASSERT_RETURN((node->type != LIBRDF_NODE_TYPE_LITERAL),
                       "node is not type literal", NULL);

  if(node->type != LIBRDF_NODE_TYPE_LITERAL)
    return NULL;
  if(len_p)
    *len_p=node->value.literal.string_len;
  return node->value.literal.string;
}


/**
 * librdf_node_get_literal_value_as_latin1:
 * @node: the node object
 *
 * Get the string literal value of the node as ISO Latin-1.
 * 
 * Returns a newly allocated string containing the conversion of the
 * UTF-8 literal value held by the node.
 *
 * Return value: the literal string or NULL if node is not a literal
 **/
char*
librdf_node_get_literal_value_as_latin1(librdf_node* node) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(node, librdf_node, NULL);
  LIBRDF_ASSERT_RETURN((node->type != LIBRDF_NODE_TYPE_LITERAL),
                       "node is not type literal", NULL);

  if(node->type != LIBRDF_NODE_TYPE_LITERAL)
    return NULL;
  return (char*)librdf_utf8_to_latin1((const byte*)node->value.literal.string,
                                      node->value.literal.string_len, NULL);
}


/**
 * librdf_node_get_literal_value_language:
 * @node: the node object
 *
 * Get the XML language of the node.
 * 
 * Returns a pointer to the literal language value held by the node, it must
 * be copied if it is wanted to be used by the caller.
 *
 * Return value: the XML language string or NULL if node is not a literal
 * or there is no XML language defined.
 **/
char*
librdf_node_get_literal_value_language(librdf_node* node) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(node, librdf_node, NULL);
  LIBRDF_ASSERT_RETURN((node->type != LIBRDF_NODE_TYPE_LITERAL),
                       "node is not type literal", NULL);

  if(node->type != LIBRDF_NODE_TYPE_LITERAL)
    return NULL;
  return node->value.literal.xml_language;
}


/**
 * librdf_node_get_literal_value_is_wf_xml:
 * @node: the node object
 *
 * Get the XML well-formness property of the node.
 * 
 * Return value: 0 if the XML literal is NOT well formed XML content, or the node is not a literal
 **/
int
librdf_node_get_literal_value_is_wf_xml(librdf_node* node) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(node, librdf_node, 0);
  LIBRDF_ASSERT_RETURN((node->type != LIBRDF_NODE_TYPE_LITERAL),
                       "node is not type literal", 0);

  if(node->type != LIBRDF_NODE_TYPE_LITERAL)
    return 0;

  if(!node->value.literal.datatype_uri)
    return 0;
  
  return librdf_uri_equals(node->value.literal.datatype_uri,
                           LIBRDF_RS_XMLLiteral_URI(node->world));
}


/**
 * librdf_node_get_literal_value_datatype_uri:
 * @node: the node object
 *
 * Get the typed literal datatype URI of the literal node.
 * 
 * Return value: shared URI of the datatyped literal or NULL if the node is not a literal, or has no datatype URI
 **/
librdf_uri*
librdf_node_get_literal_value_datatype_uri(librdf_node* node)
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(node, librdf_node, NULL);
  LIBRDF_ASSERT_RETURN((node->type != LIBRDF_NODE_TYPE_LITERAL),
                       "node is not type literal", NULL);

  if(node->type != LIBRDF_NODE_TYPE_LITERAL)
    return NULL;
  return node->value.literal.datatype_uri;
}


/**
 * librdf_node_get_li_ordinal:
 * @node: the node object
 *
 * Get the node li object ordinal value.
 *
 * Return value: the li ordinal value or < 1 on failure
 **/
int
librdf_node_get_li_ordinal(librdf_node* node) {
  unsigned char *uri_string;
  
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(node, librdf_node, 0);
  LIBRDF_ASSERT_RETURN((node->type != LIBRDF_NODE_TYPE_RESOURCE),
                       "node is not type resource", 0);

  if(node->type != LIBRDF_NODE_TYPE_RESOURCE)
    return -1;

  uri_string=librdf_uri_as_string(node->value.resource.uri); 
  if(strncmp((const char*)uri_string,
             (const char*)"http://www.w3.org/1999/02/22-rdf-syntax-ns#_", 44))
    return -1;
  
  return atoi((const char*)uri_string+44);
}


/**
 * librdf_node_get_blank_identifier:
 * @node: the node object
 *
 * Get the blank node identifier.
 *
 * Return value: the identifier value
 **/
unsigned char *
librdf_node_get_blank_identifier(librdf_node* node) {
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(node, librdf_node, NULL);
  LIBRDF_ASSERT_RETURN((node->type != LIBRDF_NODE_TYPE_BLANK),
                       "node is not type blank", NULL);

  return node->value.blank.identifier;
}


/**
 * librdf_node_is_resource:
 * @node: the node object
 *
 * Check node is a resource.
 * 
 * Return value: non-zero if the node is a resource (URI)
 **/
int
librdf_node_is_resource(librdf_node* node) {
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(node, librdf_node, 0);

  return (node->type == LIBRDF_NODE_TYPE_RESOURCE);
}


/**
 * librdf_node_is_literal:
 * @node: the node object
 *
 * Check node is a literal.
 * 
 * Return value: non-zero if the node is a literal
 **/
int
librdf_node_is_literal(librdf_node* node) {
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(node, librdf_node, 0);

  return (node->type == LIBRDF_NODE_TYPE_LITERAL);
}


/**
 * librdf_node_is_blank:
 * @node: the node object
 *
 * Check node is a blank nodeID.
 * 
 * Return value: non-zero if the node is a blank nodeID
 **/
int
librdf_node_is_blank(librdf_node* node) {
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(node, librdf_node, 0);

  return (node->type == LIBRDF_NODE_TYPE_BLANK);
}


/**
 * librdf_node_to_string:
 * @node: the node object
 *
 * Format the node as a string.
 * 
 * Note a new string is allocated which must be freed by the caller.
 * 
 * Return value: a string value representing the node or NULL on failure
 **/
unsigned char*
librdf_node_to_string(librdf_node* node) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(node, librdf_node, NULL);

  return librdf_node_to_counted_string(node, NULL);
}


/**
 * librdf_node_to_counted_string:
 * @node: the node object
 * @len_p: pointer to location to store length
 *
 * Format the node as a counted string.
 * 
 * Note a new string is allocated which must be freed by the caller.
 * 
 * Return value: a string value representing the node or NULL on failure
 **/
unsigned char*
librdf_node_to_counted_string(librdf_node* node, size_t* len_p) 
{
  unsigned char *uri_string;
  unsigned char *datatype_uri_string=NULL;
  size_t len, datatype_len, language_len=0;
  unsigned char *s, *d;

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(node, librdf_node, NULL);

  switch(node->type) {
  case LIBRDF_NODE_TYPE_RESOURCE:
    uri_string=librdf_uri_to_counted_string(node->value.resource.uri, &len);
    if(!uri_string)
      return NULL;
    len +=2;
    if(len_p)
      *len_p=len;
    s=(unsigned char*)LIBRDF_MALLOC(cstring, len+1);
    if(!s) {
      LIBRDF_FREE(cstring, uri_string);
      return NULL;
    }
    sprintf((char*)s, "[%s]", uri_string);
    LIBRDF_FREE(cstring, uri_string);
    break;
  case LIBRDF_NODE_TYPE_LITERAL:
    len=node->value.literal.string_len;
    if(node->value.literal.xml_language) {
      language_len=node->value.literal.xml_language_len;
      len+=1+language_len;
    }
    
    if(node->value.literal.datatype_uri) {
      datatype_uri_string=librdf_uri_to_counted_string(node->value.literal.datatype_uri, &datatype_len);
      len+=4+datatype_len;
    }
    if(len_p)
      *len_p=len;
    s=(unsigned char*)LIBRDF_MALLOC(cstring, len+1);
    if(!s)
      return NULL;
    /* use strcpy here to add \0 to end of literal string */
    d=s;
    strncpy((char*)d, (const char*)node->value.literal.string, node->value.literal.string_len);
    d+= node->value.literal.string_len;
    
    if(node->value.literal.xml_language) {
      *d++='@';
      strncpy((char*)d, node->value.literal.xml_language, language_len);
      d+= language_len;
    }
    if(datatype_uri_string) {
      strncpy((char*)d, "^^<", 3);
      d+= 3;
      strncpy((char*)d, (const char*)datatype_uri_string, datatype_len);
      LIBRDF_FREE(cstring, datatype_uri_string);
      d+= datatype_len;
      *d++='>';
    }
    *d='\0';
    break;
  case LIBRDF_NODE_TYPE_BLANK:
    len=node->value.blank.identifier_len + 2;
    if(len_p)
      *len_p=len;
    s=(unsigned char*)LIBRDF_MALLOC(cstring, len+1);
    if(!s)
      return NULL;
    sprintf((char*)s, "(%s)", node->value.blank.identifier);
    break;

  case LIBRDF_NODE_TYPE_UNKNOWN:
  default:
      librdf_log(node->world,
                 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_NODE, NULL,
                 "Do not know how to print node type %d", node->type);
    return NULL;
  }
  return s;
}


/**
 * librdf_node_print:
 * @node: the node
 * @fh: file handle
 *
 * Pretty print the node to a file descriptor.
 * 
 * This method is for debugging and the format of the output should
 * not be relied on.
 * 
 **/
void
librdf_node_print(librdf_node* node, FILE *fh)
{
  unsigned char* s;

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN(node, librdf_node);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN(fh, FILE*);

  if(!node)
    return;
  
  s=librdf_node_to_string(node);
  if(!s)
    return;
  fputs((const char*)s, fh);
  LIBRDF_FREE(cstring, s);
}



/**
 * librdf_node_get_digest:
 * @node: the node object
 *
 * Get a digest representing a librdf_node.
 * 
 * A new digest object is created which must be freed by the caller.
 * 
 * Return value: a new #librdf_digest object or NULL on failure
 **/
librdf_digest*
librdf_node_get_digest(librdf_node* node) 
{
  librdf_digest* d=NULL;
  unsigned char *s;
  librdf_world* world=node->world;
  
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(node, librdf_node, NULL);

  switch(node->type) {
    case LIBRDF_NODE_TYPE_RESOURCE:
      d=librdf_uri_get_digest(node->value.resource.uri);
      break;
      
    case LIBRDF_NODE_TYPE_LITERAL:
      s=node->value.literal.string;
      d=librdf_new_digest_from_factory(world, world->digest_factory);
      if(!d)
        return NULL;
      
      librdf_digest_init(d);
      librdf_digest_update(d, (unsigned char*)s, node->value.literal.string_len);
      librdf_digest_final(d);
      break;

    case LIBRDF_NODE_TYPE_BLANK:
    case LIBRDF_NODE_TYPE_UNKNOWN:
    default:
      librdf_log(world,
                 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_NODE, NULL,
                 "Do not know how to make digest for node type %d",
                 node->type);
      return NULL;
  }
  
  return d;
}


/**
 * librdf_node_equals:
 * @first_node: first #librdf_node node
 * @second_node: second #librdf_node node
 *
 * Compare two librdf_node objects for equality.
 * 
 * Note - for literal nodes, XML language, XML space and well-formness are 
 * presently ignored in the comparison.
 * 
 * Return value: non 0 if nodes are equal.  0 if not-equal or failure
 **/
int
librdf_node_equals(librdf_node* first_node, librdf_node* second_node) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(first_node, librdf_node, 0);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(second_node, librdf_node, 0);

  if(!first_node || !second_node)
    return 0;
  
  return (first_node == second_node);
}



/**
 * librdf_node_encode:
 * @node: the node to serialise
 * @buffer: the buffer to use
 * @length: buffer size
 *
 * Serialise a node into a buffer.
 * 
 * Encodes the given node in the buffer, which must be of sufficient
 * size.  If buffer is NULL, no work is done but the size of buffer
 * required is returned.
 * 
 * If the node cannot be encoded due to restrictions of the encoding
 * format, a redland error is generated
 *
 * Return value: the number of bytes written or 0 on failure.
 **/
size_t
librdf_node_encode(librdf_node* node, unsigned char *buffer, size_t length)
{
  size_t total_length=0;
  unsigned char *string;
  size_t string_length;
  size_t language_length=0;
  unsigned char *datatype_uri_string=NULL;
  size_t datatype_uri_length=0;
  
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(node, librdf_node, 0);

  switch(node->type) {
    case LIBRDF_NODE_TYPE_RESOURCE:
      string=(unsigned char*)librdf_uri_as_counted_string(node->value.resource.uri, &string_length);
      
      total_length= 3 + string_length + 1; /* +1 for \0 at end */
      
      if(length && total_length > length)
        return 0;    

      if(string_length > 0xFFFF) {
          librdf_log(node->world,
                     0, LIBRDF_LOG_ERROR, LIBRDF_FROM_NODE, NULL,
                     "Cannot encode a URI string of %d bytes length",
                     (int)string_length);
        return 0;
      }

      if(buffer) {
        buffer[0]='R';
        buffer[1]=(string_length & 0xff00) >> 8;
        buffer[2]=(string_length & 0x00ff);
        strcpy((char*)buffer+3, (char*)string);
      }
      break;
      
    case LIBRDF_NODE_TYPE_LITERAL:
      string=(unsigned char*)node->value.literal.string;
      string_length=node->value.literal.string_len;
      if(node->value.literal.xml_language)
        language_length=node->value.literal.xml_language_len;
      if(node->value.literal.datatype_uri) {
        datatype_uri_string=librdf_uri_as_counted_string(node->value.literal.datatype_uri, &datatype_uri_length);
      }
      
      total_length= 6 + string_length + 1; /* +1 for \0 at end */
      if(string_length > 0xFFFF) /* for long literal - type 'N' */
        total_length+= 2;

      if(language_length)
        total_length += language_length+1;
      if(datatype_uri_length)
        total_length += datatype_uri_length+1;
      
      if(length && total_length > length)
        return 0;    

      if(datatype_uri_length > 0xFFFF) {
        librdf_log(node->world,
                   0, LIBRDF_LOG_ERROR, LIBRDF_FROM_NODE, NULL,
                   "Cannot encode a datatype URI string of %d bytes length",
                   (int)datatype_uri_length);
        return 0;
      }

      if(language_length > 0xFF) {
        librdf_log(node->world,
                   0, LIBRDF_LOG_ERROR, LIBRDF_FROM_NODE, NULL,
                   "Cannot encode a language string of %d bytes length",
                   (int)language_length);
        return 0;
      }


      if(buffer) {
        if(string_length > 0xFFFF) {
          /* long literal type N (string length > 0x10000) */
          buffer[0]='N';
          buffer[1]=(string_length & 0xff000000) >> 24;
          buffer[2]=(string_length & 0x00ff0000) >> 16;
          buffer[3]=(string_length & 0x0000ff00) >> 8;
          buffer[4]=(string_length & 0x000000ff);
          buffer[5]=(datatype_uri_length & 0xff00) >> 8;
          buffer[6]=(datatype_uri_length & 0x00ff);
          buffer[7]=(language_length & 0x00ff);
          buffer += 8;
        } else {
          /* short literal type M (string length <= 0xFFFF) */
          buffer[0]='M';
          buffer[1]=(string_length & 0xff00) >> 8;
          buffer[2]=(string_length & 0x00ff);
          buffer[3]=(datatype_uri_length & 0xff00) >> 8;
          buffer[4]=(datatype_uri_length & 0x00ff);
          buffer[5]=(language_length & 0x00ff);
          buffer += 6;
        }
        strcpy((char*)buffer, (const char*)string);
        buffer += string_length+1;
        if(datatype_uri_length) {
          strcpy((char*)buffer, (const char*)datatype_uri_string);
          buffer += datatype_uri_length+1;
        }
        if(language_length)
          strcpy((char*)buffer, (const char*)node->value.literal.xml_language);
      } /* end if buffer */

      break;
      
    case LIBRDF_NODE_TYPE_BLANK:
      string=(unsigned char*)node->value.blank.identifier;
      string_length=node->value.blank.identifier_len;
      
      total_length= 3 + string_length + 1; /* +1 for \0 at end */
      
      if(length && total_length > length)
        return 0;    
      
      if(string_length > 0xFFFF) {
        librdf_log(node->world,
                   0, LIBRDF_LOG_ERROR, LIBRDF_FROM_NODE, NULL,
                   "Cannot encode a blank node identifier string of %d bytes length",
                   (int)string_length);
        return 0;
      }

      if(buffer) {
        buffer[0]='B';
        buffer[1]=(string_length & 0xff00) >> 8;
        buffer[2]=(string_length & 0x00ff);
        strcpy((char*)buffer+3, (const char*)string);
      }
      break;
      
    case LIBRDF_NODE_TYPE_UNKNOWN:
    default:
      librdf_log(node->world,
                 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_NODE, NULL,
                 "Do not know how to encode node type %d", node->type);
      return 0;
  }
  
  return total_length;
}


/**
 * librdf_node_decode:
 * @world: librdf_world
 * @size_p: pointer to bytes used or NULL
 * @buffer: the buffer to use
 * @length: buffer size
 *
 * Deserialise a node from a buffer.
 * 
 * Decodes the serialised node (as created by librdf_node_encode() )
 * from the given buffer.
 * 
 * Return value: new node or NULL on failure (bad encoding, allocation failure)
 **/
librdf_node*
librdf_node_decode(librdf_world *world,
                   size_t* size_p, unsigned char *buffer, size_t length)
{
  int is_wf_xml;
  size_t string_length;
  size_t total_length;
  size_t language_length;
  unsigned char *datatype_uri_string=NULL;
  size_t datatype_uri_length;
  librdf_uri* datatype_uri=NULL;
  unsigned char *language=NULL;
  int status=0;
  librdf_node* node=NULL;

  librdf_world_open(world);

  /* absolute minimum - first byte is type */
  if (length < 1)
    return NULL;

  total_length=0;
  switch(buffer[0]) {
    case 'R': /* LIBRDF_NODE_TYPE_RESOURCE */
      /* min */
      if(length < 3)
        return NULL;

      string_length=(buffer[1] << 8) | buffer[2];
      total_length = 3 + string_length + 1;
      
      node = librdf_new_node_from_uri_string(world, buffer+3);

      break;

    case 'L': /* Old encoding form for LIBRDF_NODE_TYPE_LITERAL */
      /* min */
      if(length < 6)
        return NULL;
      
      is_wf_xml=(buffer[1] & 0xf0)>>8;
      string_length=(buffer[2] << 8) | buffer[3];
      language_length=buffer[5];

      total_length= 6 + string_length + 1; /* +1 for \0 at end */
      if(language_length) {
        language = buffer + total_length;
        total_length += language_length+1;
      }
      
      node=librdf_new_node_from_typed_counted_literal(world,
                                                      buffer+6,
                                                      string_length,
                                                      (const char*)language,
                                                      language_length,
                                                      is_wf_xml ? LIBRDF_RS_XMLLiteral_URI(world) : NULL);
    
    break;

    case 'M': /* LIBRDF_NODE_TYPE_LITERAL 0.9.12+ */
      /* min */
      if(length < 6)
        return NULL;
      
      string_length=(buffer[1] << 8) | buffer[2];
      datatype_uri_length=(buffer[3] << 8) | buffer[4];
      language_length=buffer[5];

      total_length= 6 + string_length + 1; /* +1 for \0 at end */
      if(datatype_uri_length) {
        datatype_uri_string = buffer + total_length;
        total_length += datatype_uri_length+1;
      }
      if(language_length) {
        language = buffer + total_length;
        total_length += language_length+1;
      }

      if(datatype_uri_string)
        datatype_uri=librdf_new_uri(world, datatype_uri_string);
      
      node=librdf_new_node_from_typed_counted_literal(world,
                                                      buffer+6,
                                                      string_length,
                                                      (const char*)language,
                                                      language_length,
                                                      datatype_uri);
      if(datatype_uri)
        librdf_free_uri(datatype_uri);
      
      if(status)
        return NULL;
      
    break;

    case 'N': /* LIBRDF_NODE_TYPE_LITERAL - redland 1.0.5+ (long literal) */
      /* min */
      if(length < 8)
        return NULL;
      
      string_length=(buffer[1] << 24) | (buffer[2] << 16) | (buffer[3] << 8) | buffer[4];
      datatype_uri_length=(buffer[5] << 8) | buffer[6];
      language_length=buffer[7];

      total_length= 8 + string_length + 1; /* +1 for \0 at end */
      if(datatype_uri_length) {
        datatype_uri_string = buffer + total_length;
        total_length += datatype_uri_length+1;
      }
      if(language_length) {
        language = buffer + total_length;
        total_length += language_length+1;
      }

      if(datatype_uri_string)
        datatype_uri=librdf_new_uri(world, datatype_uri_string);
      
      node=librdf_new_node_from_typed_counted_literal(world,
                                                      buffer+8,
                                                      string_length,
                                                      (const char*)language,
                                                      language_length,
                                                      datatype_uri);
      if(datatype_uri)
        librdf_free_uri(datatype_uri);
      
      if(status)
        return NULL;
      
    break;

    case 'B': /* LIBRDF_NODE_TYPE_BLANK */
      /* min */
      if(length < 3)
        return NULL;
      
      string_length=(buffer[1] << 8) | buffer[2];

      total_length= 3 + string_length + 1; /* +1 for \0 at end */
      
      node = librdf_new_node_from_blank_identifier(world, buffer+3);
    
    break;

  default:
      librdf_log(world,
                 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_NODE, NULL,
                 "Illegal node encoding '%c' seen", buffer[0]);
    return NULL;
  }
  
  if(size_p)
    *size_p=total_length;

  return node;
}



/* iterator over a static array of nodes; - mostly for testing */
static int librdf_node_static_iterator_is_end(void* iterator);
static int librdf_node_static_iterator_next_method(void* iterator);
static void* librdf_node_static_iterator_get_method(void* iterator, int flags);
static void librdf_node_static_iterator_finished(void* iterator);

typedef struct {
  librdf_world *world;
  librdf_node** nodes; /* static array of nodes; shared */
  int size;            /* size of above array */
  int current;         /* index into above array */
} librdf_node_static_iterator_context;


static int
librdf_node_static_iterator_is_end(void* iterator)
{
  librdf_node_static_iterator_context* context=(librdf_node_static_iterator_context*)iterator;

  return (context->current > context->size-1);
}


static int
librdf_node_static_iterator_next_method(void* iterator) 
{
  librdf_node_static_iterator_context* context=(librdf_node_static_iterator_context*)iterator;

  if(context->current > context->size-1)
    return 1;

  context->current++;
  return 0;
}


static void*
librdf_node_static_iterator_get_method(void* iterator, int flags) 
{
  librdf_node_static_iterator_context* context=(librdf_node_static_iterator_context*)iterator;
  
  if(context->current > context->size-1)
    return NULL;

  switch(flags) {
    case LIBRDF_ITERATOR_GET_METHOD_GET_OBJECT:
       return (void*)context->nodes[context->current];

    case LIBRDF_ITERATOR_GET_METHOD_GET_CONTEXT:
      return NULL;

    default:
      librdf_log(context->world,
                 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_NODE, NULL,
                 "Unknown iterator method flag %d", flags);
      return NULL;
  }
}


static void
librdf_node_static_iterator_finished(void* iterator) 
{
  librdf_node_static_iterator_context* context=(librdf_node_static_iterator_context*)iterator;
  LIBRDF_FREE(librdf_node_static_iterator_context, context);
}


/**
 * librdf_node_static_iterator_create:
 * @nodes: static array of #librdf_node objects
 * @size: size of array
 *
 * Create an iterator over an array of nodes.
 * 
 * This creates an iterator for an existing static array of librdf_node
 * objects.  It is mostly intended for testing iterator code.
 * 
 * Return value: a #librdf_iterator serialization of the nodes or NULL on failure
 **/
librdf_iterator*
librdf_node_static_iterator_create(librdf_node** nodes,
                                   int size)
{
  librdf_node_static_iterator_context* context;
  librdf_iterator* iterator;
  
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(nodes, librdf_node**, NULL);

  context=(librdf_node_static_iterator_context*)LIBRDF_CALLOC(librdf_node_static_iterator_context, 1, sizeof(librdf_node_static_iterator_context));
  if(!context)
    return NULL;

  context->nodes=nodes;
  context->size=size;
  context->current=0;

  iterator=librdf_new_iterator(nodes[0]->world,
                               (void*)context,
                               librdf_node_static_iterator_is_end,
                               librdf_node_static_iterator_next_method,
                               librdf_node_static_iterator_get_method,
                               librdf_node_static_iterator_finished);
  if(!iterator)
    librdf_node_static_iterator_finished(context);
  return iterator;
}

#endif


/* TEST CODE */


#ifdef STANDALONE

/* one more prototype */
int main(int argc, char *argv[]);


static void
dump_node_as_C(FILE* fh, const char* var, void *buffer, int size) {
  int i;
  unsigned char* p=(unsigned char*)buffer;
  
  fprintf(fh, "const unsigned char %s[%d] = {", var, size);
  for (i=0; i < size; i++) {
    if(i)
      fputs(", ", fh);
    fprintf(fh, "0x%02x", p[i]);
  }
  fputs("};\n", fh);
}

  
static int
check_node(const char* program, const unsigned char *expected, 
           void *buffer, size_t size) {
  unsigned int i;
  for(i=0; i< size; i++) {
    unsigned char c=((unsigned char*)buffer)[i];
    if(c != expected[i]) {
      fprintf(stderr, "%s: Encoding node byte %d: 0x%02x expected 0x%02x\n",
              program, i, c, expected[i]);
      return(1);
    }
  }
  return(0);
}


static const char *hp_string1="http://purl.org/net/dajobe/";
static const char *hp_string2="http://purl.org/net/dajobe/";
static const char *lit_string="Dave Beckett";
static const char *genid="genid42";
static const char *datatype_lit_string="Datatyped literal value";
static const char *datatype_uri_string="http://example.org/datatypeURI";

/* Node Encoded (type R) version of hp_string1 */
static const unsigned char hp_uri_encoded[31] = {0x52, 0x00, 0x1b, 0x68, 0x74, 0x74, 0x70, 0x3a, 0x2f, 0x2f, 0x70, 0x75, 0x72, 0x6c, 0x2e, 0x6f, 0x72, 0x67, 0x2f, 0x6e, 0x65, 0x74, 0x2f, 0x64, 0x61, 0x6a, 0x6f, 0x62, 0x65, 0x2f, 0x00};

/* Node Encoded (type M) version of typed literal with literal value
 * datatype_lit_string and datatype URI datatype_uri_string */
static const unsigned char datatyped_literal_M_encoded[61] = {0x4d, 0x00, 0x17, 0x00, 0x1e, 0x00, 0x44, 0x61, 0x74, 0x61, 0x74, 0x79, 0x70, 0x65, 0x64, 0x20, 0x6c, 0x69, 0x74, 0x65, 0x72, 0x61, 0x6c, 0x20, 0x76, 0x61, 0x6c, 0x75, 0x65, 0x00, 0x68, 0x74, 0x74, 0x70, 0x3a, 0x2f, 0x2f, 0x65, 0x78, 0x61, 0x6d, 0x70, 0x6c, 0x65, 0x2e, 0x6f, 0x72, 0x67, 0x2f, 0x64, 0x61, 0x74, 0x61, 0x74, 0x79, 0x70, 0x65, 0x55, 0x52, 0x49, 0x00};

/* Node Encoded (type N) version of big 100,000-length literal
 * (just the first 32 bytes, the rest are 0x58 'X')
 */
const unsigned char big_literal_N_encoded[32] = {0x4e, 0x00, 0x01, 0x86, 0xa0, 0x00, 0x00, 0x00, 0x58, 0x58, 0x58, 0x58, 0x58, 0x58, 0x58, 0x58, 0x58, 0x58, 0x58, 0x58, 0x58, 0x58, 0x58, 0x58, 0x58, 0x58, 0x58, 0x58, 0x58, 0x58, 0x58, 0x58};


int
main(int argc, char *argv[]) 
{
  librdf_node *node, *node2, *node3, *node4, *node5, *node6, *node7, *node8;
  librdf_uri *uri, *uri2;
  int size, size2;
  unsigned char *buffer;
  librdf_world *world;
  size_t big_literal_length;
  unsigned char *big_literal;
  unsigned int i;
  
  const char *program=librdf_basename((const char*)argv[0]);
	
  world=librdf_new_world();
  librdf_world_open(world);

  fprintf(stdout, "%s: Creating home page node from string\n", program);
  node=librdf_new_node_from_uri_string(world, (const unsigned char*)hp_string1);
  if(!node) {
    fprintf(stderr, "%s: librdf_new_node_from_uri_string failed\n", program);
    return(1);
  }
  
  fprintf(stdout, "%s: Home page URI is ", program);
  librdf_uri_print(librdf_node_get_uri(node), stdout);
  fputc('\n', stdout);
  
  fprintf(stdout, "%s: Creating URI from string '%s'\n", program, 
          hp_string2);
  uri=librdf_new_uri(world, (const unsigned char*)hp_string2);
  fprintf(stdout, "%s: Setting node URI to new URI ", program);
  librdf_uri_print(uri, stdout);
  fputc('\n', stdout);
  librdf_free_uri(uri);
  
  fprintf(stdout, "%s: Node is: ", program);
  librdf_node_print(node, stdout);
  fputc('\n', stdout);

  size=librdf_node_encode(node, NULL, 0);
  fprintf(stdout, "%s: Encoding node requires %d bytes\n", program, size);
  buffer=(unsigned char*)LIBRDF_MALLOC(cstring, size);

  fprintf(stdout, "%s: Encoding node in buffer\n", program);
  size2=librdf_node_encode(node, buffer, size);
  if(size2 != size) {
    fprintf(stderr, "%s: Encoding node used %d bytes, expected it to use %d\n", program, size2, size);
    return(1);
  }

  if(0)
    dump_node_as_C(stdout, "hp_uri_encoded", buffer, size);
  if(check_node(program, hp_uri_encoded, buffer, size))
    return(1);
  
    
  fprintf(stdout, "%s: Creating new node\n", program);

  fprintf(stdout, "%s: Decoding node from buffer\n", program);
  if(!(node2=librdf_node_decode(world, NULL, buffer, size))) {
    fprintf(stderr, "%s: Decoding node failed\n", program);
    return(1);
  }
  LIBRDF_FREE(cstring, buffer);
   
  fprintf(stdout, "%s: New node is: ", program);
  librdf_node_print(node2, stdout);
  fputc('\n', stdout);
 
  
  fprintf(stdout, "%s: Creating new literal string node\n", program);
  node3=librdf_new_node_from_literal(world, (const unsigned char*)lit_string, NULL, 0);
  if(!node3) {
    fprintf(stderr, "%s: librdf_new_node_from_literal failed\n", program);
    return(1);
  }

  buffer=(unsigned char*)librdf_node_get_literal_value_as_latin1(node3);
  if(!buffer) {
    fprintf(stderr, "%s: Failed to get literal string value as Latin-1\n", program);
    return(1);
  }
  fprintf(stdout, "%s: Node literal string value (Latin-1) is: '%s'\n",
          program, buffer);
  LIBRDF_FREE(cstring, buffer);

  
  fprintf(stdout, "%s: Creating new blank node with identifier %s\n", program, genid);
  node4=librdf_new_node_from_blank_identifier(world, (const unsigned char*)genid);
  if(!node4) {
    fprintf(stderr, "%s: librdf_new_node_from_blank_identifier failed\n", program);
    return(1);
  }

  buffer=librdf_node_get_blank_identifier(node4);
  if(!buffer) {
    fprintf(stderr, "%s: Failed to get blank node identifier\n", program);
    return(1);
  }
  fprintf(stdout, "%s: Node identifier is: '%s'\n", program, buffer);
  
  node5=librdf_new_node_from_node(node4);
  if(!node5) {
    fprintf(stderr, "%s: Failed to make new blank node from old one\n", program);
    return(1);
  } 

  buffer=librdf_node_get_blank_identifier(node5);
  if(!buffer) {
    fprintf(stderr, "%s: Failed to get copied blank node identifier\n", program);
    return(1);
  }
  fprintf(stdout, "%s: Copied node identifier is: '%s'\n", program, buffer);


  uri2=librdf_new_uri(world, (const unsigned char*)datatype_uri_string);
  node6=librdf_new_node_from_typed_literal(world, 
                                           (const unsigned char*)datatype_lit_string,
                                           NULL, uri2);
  librdf_free_uri(uri2);

  size=librdf_node_encode(node6, NULL, 0);
  fprintf(stdout, "%s: Encoding typed node requires %d bytes\n", program, size);
  buffer=(unsigned char*)LIBRDF_MALLOC(cstring, size);

  fprintf(stdout, "%s: Encoding typed node in buffer\n", program);
  size2=librdf_node_encode(node6, (unsigned char*)buffer, size);
  if(size2 != size) {
    fprintf(stderr, "%s: Encoding typed node used %d bytes, expected it to use %d\n", program, size2, size);
    return(1);
  }

  if(0)
    dump_node_as_C(stdout, "datatyped_literal_M_encoded", buffer, size);
  if(check_node(program, datatyped_literal_M_encoded, buffer, size))
    return(1);
  
  fprintf(stdout, "%s: Decoding typed node from buffer\n", program);
  if(!(node7=librdf_node_decode(world, NULL, (unsigned char*)buffer, size))) {
    fprintf(stderr, "%s: Decoding typed node failed\n", program);
    return(1);
  }
  LIBRDF_FREE(cstring, buffer);
   
  if(librdf_new_node_from_typed_literal(world, 
                                        (const unsigned char*)"Datatyped literal value",
                                        "en-GB", uri2)) {
    fprintf(stderr, "%s: Unexpected success allowing a datatyped literal with a language\n", program);
    return(1);
  }
    
  if(librdf_new_node_from_literal(world, 
                                  (const unsigned char*)"XML literal value",
                                  "en-GB", 1)) {
    fprintf(stderr, "%s: Unexpected success allowing an XML literal with a language\n", program);
    return(1);
  }
    
  big_literal_length=100000;
  big_literal=(unsigned char *)LIBRDF_MALLOC(cstring, big_literal_length+1);
  for(i=0; i<big_literal_length; i++)
     big_literal[i]='X';

  node8=librdf_new_node_from_typed_counted_literal(world, 
                                                   big_literal, big_literal_length,
                                                   NULL, 0, NULL);
  if(!node8) {
    fprintf(stderr, "%s: Failed to make big %d byte literal\n", program,
            (int)big_literal_length);
    return(1);
  }
  LIBRDF_FREE(cstring, big_literal);

  size=librdf_node_encode(node8, NULL, 0);
  fprintf(stdout, "%s: Encoding big literal node requires %d bytes\n", program, size);
  buffer=(unsigned char*)LIBRDF_MALLOC(cstring, size);
  fprintf(stdout, "%s: Encoding big literal node in buffer\n", program);
  size2=librdf_node_encode(node8, (unsigned char*)buffer, size);
  if(size2 != size) {
    fprintf(stderr, "%s: Encoding big literal node used %d bytes, expected it to use %d\n", program, size2, size);
    return(1);
  }

  /* Just check first 32 bytes */
  if(0)
    dump_node_as_C(stdout, "big_literal_N_encoded", buffer, 32);
  if(check_node(program, big_literal_N_encoded, buffer, 32))
    return(1);
  LIBRDF_FREE(cstring, buffer);
    

  fprintf(stdout, "%s: Freeing nodes\n", program);
  librdf_free_node(node8);
  librdf_free_node(node7);
  librdf_free_node(node6);
  librdf_free_node(node5);
  librdf_free_node(node4);
  librdf_free_node(node3);
  librdf_free_node(node2);
  librdf_free_node(node);

  librdf_free_world(world);

  /* keep gcc -Wall happy */
  return(0);
}

#endif
