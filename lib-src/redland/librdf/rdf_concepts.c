/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_concepts.c - Nodes representing concepts from the RDF Model
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
#include <stdarg.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h> /* for abort() as used in errors */
#endif

#include <redland.h>
#include <rdf_node.h>

#ifndef STANDALONE

/* FIXME: All the stuff here and in rdf_concepts.h should be machine
 * generated from the schemas but there is a catch-22 here - can't do
 * it without representing it 
 */


/* TAKE CARE: Tokens != Labels */

/* Tokens used by the RDF world */
static const char* const librdf_concept_tokens[LIBRDF_CONCEPT_LAST+1]={
  /* RDF M&S */
  "Alt", "Bag", "Property", "Seq", "Statement", "object", "predicate", "subject", "type", "value", "li",
  "RDF", "Description",
  "aboutEach", "aboutEachPrefix",
  /* all new in RDF/XML revised */
  "nodeID",
  "List", "first", "rest", "nil", 
  "XMLLiteral",

  /* RDF S */
  "Class", "ConstraintProperty", "ConstraintResource", "Container", "ContainerMembershipProperty", "Literal", "Resource", "comment", "domain", "isDefinedBy", "label", "range", "seeAlso", "subClassOf", "subPropertyOf"
};


static const unsigned char * const librdf_concept_ms_namespace=(const unsigned char *)"http://www.w3.org/1999/02/22-rdf-syntax-ns#";
static const unsigned char * const librdf_concept_schema_namespace=(const unsigned char *)"http://www.w3.org/2000/01/rdf-schema#";

/**
 * librdf_init_concepts:
 * @world: redland world object
 *
 * INTERNAL - Initialise the concepts module.
 * 
 **/
void
librdf_init_concepts(librdf_world *world)
{
  int i;

  /* Create the Unique URI objects */
  world->concept_ms_namespace_uri=librdf_new_uri(world, librdf_concept_ms_namespace);
  world->concept_schema_namespace_uri=librdf_new_uri(world, librdf_concept_schema_namespace);
  if(!world->concept_ms_namespace_uri || !world->concept_schema_namespace_uri)
    LIBRDF_FATAL1(world, LIBRDF_FROM_CONCEPTS, "Failed to create M&S or Schema URIs");

  /* Create arrays for the M&S and Schema resource nodes and uris */
  world->concept_uris=
    LIBRDF_CALLOC(ptrarray, LIBRDF_CONCEPT_LAST+1, sizeof(librdf_uri*));
  world->concept_resources=
    LIBRDF_CALLOC(ptrarray, LIBRDF_CONCEPT_LAST+1, sizeof(librdf_node*));
  if(!world->concept_uris || !world->concept_resources)
    LIBRDF_FATAL1(world, LIBRDF_FROM_CONCEPTS, "Out of memory creating node/uri arrays");

  /* Create the M&S and Schema resource nodes */
  for (i=0; i<= LIBRDF_CONCEPT_LAST; i++) {
    librdf_uri* ns_uri=(i < LIBRDF_CONCEPT_FIRST_S_ID) ? world->concept_ms_namespace_uri :
      world->concept_schema_namespace_uri;
    const unsigned char * token=(const unsigned char *)librdf_concept_tokens[i];

    world->concept_resources[i]=librdf_new_node_from_uri_local_name(world, ns_uri, token);
    if(!world->concept_resources[i])
      LIBRDF_FATAL1(world, LIBRDF_FROM_CONCEPTS, "Failed to create Node from URI\n");

    /* keep shared copy of URI from node */
    world->concept_uris[i]=librdf_node_get_uri(world->concept_resources[i]);
  }
}


/**
 * librdf_get_concept_ms_namespace:
 * @world: librdf world object
 *
 * Get a shared #librdf_uri for the RDF Syntax namespace.
 *
 * Return value: #librdf_uri pointer or NULL on failure.
 */
librdf_uri *
librdf_get_concept_ms_namespace(librdf_world *world)
{
  librdf_world_open(world);
  return world->concept_ms_namespace_uri;
}


/**
 * librdf_get_concept_schema_namespace:
 * @world: librdf world object
 *
 * Get a shared #librdf_uri for the RDF Schema namespace.
 *
 * Return value: #librdf_uri pointer or NULL on failure.
 */
librdf_uri *
librdf_get_concept_schema_namespace(librdf_world *world)
{
  librdf_world_open(world);
  return world->concept_schema_namespace_uri;
}


/**
 * librdf_get_concept_by_name:
 * @world: redland world object
 * @is_ms: non zero if name is a RDF namespace concept (else is RDF schema)
 * @name: the name to look up
 * @uri_p: pointer to variable to hold #librdf_uri of concept or NULL if not required
 * @node_p: pointer to variable to hold #librdf_node of concept or NULL if not required
 *
 * Get Redland uri and/or node objects for RDF concepts.
 * 
 * Allows the dynamic look-up of an RDF concept by the local_name of
 * the concept in either the RDF or RDF Schema namespace.  Returns
 * the #librdf_uri and/or #librdf_node found as required.
 **/
void
librdf_get_concept_by_name(librdf_world *world, int is_ms,
                           const char *name,
                           librdf_uri **uri_p, librdf_node **node_p)
{
  int i;
  int start=is_ms ? 0 : LIBRDF_CONCEPT_FIRST_S_ID;
  int last=is_ms ? LIBRDF_CONCEPT_FIRST_S_ID : LIBRDF_CONCEPT_LAST;

  librdf_world_open(world);

  for (i=start; i< last; i++)
    if(!strcmp(librdf_concept_tokens[i], name)) {
      if(uri_p)
        *uri_p=world->concept_uris[i];
      if(node_p)
        *node_p=world->concept_resources[i];
    }
}


/**
 * librdf_get_concept_resource_by_index:
 * @world: redland world object
 * @idx: #librdf_concepts_index
 *
 * Get Redland node object for RDF concepts.
 *
 * Return value: #librdf_node pointer or NULL on failure. 
 **/
librdf_node*
librdf_get_concept_resource_by_index(librdf_world *world,
                                     librdf_concepts_index idx)
{
  librdf_world_open(world);

  if ((int)idx < 0 || idx > LIBRDF_CONCEPT_LAST)
    return NULL;

  return world->concept_resources[idx];
}


/**
 * librdf_get_concept_uri_by_index:
 * @world: redland world object
 * @idx: #librdf_concepts_index
 *
 * Get Redland uri object for RDF concepts.
 *
 * Return value: #librdf_uri pointer or NULL on failure. 
 **/
librdf_uri*
librdf_get_concept_uri_by_index(librdf_world *world,
                                librdf_concepts_index idx)
{
  librdf_world_open(world);

  if ((int)idx < 0 || idx > LIBRDF_CONCEPT_LAST)
    return NULL;

  return world->concept_uris[idx];
}


/**
 * librdf_finish_concepts:
 * @world: redland world object
 *
 * INTERNAL - Terminate the concepts module.
 *
 **/
void
librdf_finish_concepts(librdf_world *world)
{
  int i;

  /* Free resources and set pointers to NULL so that they are cleared
   * in case the concepts module is initialised again in the same process. */

  if(world->concept_ms_namespace_uri) {
    librdf_free_uri(world->concept_ms_namespace_uri);
    world->concept_ms_namespace_uri=NULL;
  }

  if(world->concept_schema_namespace_uri) {
    librdf_free_uri(world->concept_schema_namespace_uri);
    world->concept_schema_namespace_uri=NULL;
  }

  if(world->concept_resources) {
    for (i=0; i<= LIBRDF_CONCEPT_LAST; i++) {
      /* deletes associated URI too */
      if(world->concept_resources[i])
        librdf_free_node(world->concept_resources[i]);
    }
    LIBRDF_FREE(ptrarray, world->concept_resources);
    world->concept_resources=NULL;
  }

  if(world->concept_uris) {
    /* uris were freed above, now just free the array */
    LIBRDF_FREE(ptrarray, world->concept_uris);
    world->concept_uris=NULL;
  }
}

#endif


/* TEST CODE */


#ifdef STANDALONE

/* one more prototype */
int main(int argc, char *argv[]);


#define EXPECTED_STRING "http://www.w3.org/1999/02/22-rdf-syntax-ns#Seq"
int
main(int argc, char *argv[]) 
{
  const char *program=librdf_basename((const char*)argv[0]);
  librdf_world *world;
  librdf_uri* uri;
  librdf_node* node;
  unsigned char* actual;
  
  world=librdf_new_world();
  librdf_world_open(world);
  
  uri=LIBRDF_MS_Seq_URI(world);
  if(!uri) {
    fprintf(stderr, "%s: Got no concept URI for rdf:Seq\n", program);
    exit(1);
  }
  
  actual=librdf_uri_as_string(uri);
  if(strcmp(EXPECTED_STRING, (const char*)actual)) {
    fprintf(stderr, "%s: Expected URI: <%s> Got: <%s>\n", program,
            EXPECTED_STRING, actual);
    exit(1);
  }

  node=librdf_get_concept_resource_by_index(world, LIBRDF_CONCEPT_LAST);
  if(!node) {
    fprintf(stderr, "%s: Got no concept node for the last concept\n", program);
    exit(1);
  }

  librdf_free_world(world);

  /* keep gcc -Wall happy */
  return(0);
}

#endif
