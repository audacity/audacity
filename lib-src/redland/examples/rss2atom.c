/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rss2atom.c - read from a file 'file.rdf' (rdfxml) and serialize to atom
 *
 * Compile with:
 * gcc -o rss2atom `redland-config --cflags` rss2atom.c `redland-config --libs`
 *
 * Copyright (C) 2008, David Beckett http://www.dajobe.org/
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
 */

#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>

#include <redland.h>

#define NSPACE_COUNT 3
static struct 
{
  const char* prefix;
  const char* ns_uri;
  librdf_uri* uri;
} local_nspaces[NSPACE_COUNT]= {
  { "my",  "http://example.org/my-ns#" },
  { "owl",  "http://www.w3.org/2002/07/owl#",  },
  { "rdfs", "http://www.w3.org/2000/01/rdf-schema#" }
};
    

#define FEATURE_COUNT 3
static struct 
{
  const char* predicate;
  const char* value;
} local_features[NSPACE_COUNT]= {
  /* Enable atom triples output for atom */
  { "http://feature.librdf.org/raptor-rssTriples" , "atom-triples" },
  /* Tell it to not write xml:base */
  { "http://feature.librdf.org/raptor-writeBaseURI", "0" },
  /* This is used to generate an atom entry feed */
  { "http://feature.librdf.org/raptor-atomEntryUri", "http://example.org/2006/03/28/blog-item" }
};
    

int
main(int argc, char *argv[]) 
{
  librdf_world* world;
  librdf_storage* storage;
  librdf_parser* parser;
  librdf_model* model;
  char *program;
  char *p;
  librdf_uri *uri;
  const char *parser_name="rdfxml";
  librdf_serializer* serializer;
  const char* serializer_name="atom";
  unsigned char *string;
  size_t length;
  int i;
  
  program=argv[0];
  if((p=strrchr(program, '/')))
    program=p+1;
  else if((p=strrchr(program, '\\')))
    program=p+1;
  argv[0]=program;

  if(argc <2 || argc >2) {
    fprintf(stderr, "USAGE: %s: <RSS RDF/XML file>\n", program);
    return(1);
  }


  world=librdf_new_world();
  librdf_world_open(world);

  uri=librdf_new_uri_from_filename(world, argv[1]);
  if(!uri) {
    fprintf(stderr, "%s: Failed to create URI\n", program);
    return(1);
  }

  storage=librdf_new_storage(world, "memory", "test", NULL);
  if(!storage) {
    fprintf(stderr, "%s: Failed to create new storage\n", program);
    return(1);
  }

  model=librdf_new_model(world, storage, NULL);
  if(!model) {
    fprintf(stderr, "%s: Failed to create model\n", program);
    return(1);
  }
  
  parser=librdf_new_parser(world, parser_name, NULL, NULL);
  if(!parser) {
    fprintf(stderr, "%s: Failed to create new %s parser\n", program,
            parser_name);
    return(1);
  }

  /* PARSE the file as RDF/XML */
  fprintf(stderr, "%s: Parsing file %s\n", program, librdf_uri_as_string(uri));
  if(librdf_parser_parse_into_model(parser, uri, uri, model)) {
    fprintf(stderr, "%s: Failed to parse RDF into model\n", program);
    return(1);
  }
  librdf_free_parser(parser);

  fprintf(stderr, "%s: Resulting model is %d triples\n", program,
          librdf_model_size(model));


  /* SERIALIZE the file as ATOM 1.0 */
  serializer = librdf_new_serializer(world, serializer_name, NULL, NULL);
  if(!serializer) {
    fprintf(stderr, "%s: Failed to create %s serialize\n", program,
            serializer_name);
    return(1);
  }

  /* set the atom serializer namespaces */ 
  for(i=0; i < NSPACE_COUNT; i++) {
    local_nspaces[i].uri=librdf_new_uri(world, (const unsigned char*)local_nspaces[i].ns_uri);
    librdf_serializer_set_namespace(serializer, local_nspaces[i].uri,
                                    local_nspaces[i].prefix);
  }

  /* set the atom serializer features */ 
  for(i=0; i < FEATURE_COUNT; i++) {
    librdf_uri* predicate_uri;
    librdf_node* value_node;

    predicate_uri=librdf_new_uri(world,
                                 (const unsigned char*)local_features[i].predicate);
    value_node=librdf_new_node_from_literal(world, (const unsigned char*)local_features[i].value, NULL, 0);

    librdf_serializer_set_feature(serializer, predicate_uri, value_node);
    librdf_free_uri(predicate_uri);
    librdf_free_node(value_node);
  }
  
  string=librdf_serializer_serialize_model_to_counted_string(serializer, 
                                                             NULL /* librdf_uri* base_uri */,
                                                             model,
                                                             &length);

  fprintf(stderr, "%s: Serialized model (%d bytes)\n", program, (int)length);
  fputs((const char*)string, stdout);
  free(string);


  for(i=0; i < NSPACE_COUNT; i++) {
    if(local_nspaces[i].uri)
      librdf_free_uri(local_nspaces[i].uri);
  }
  

  librdf_free_serializer(serializer);

  librdf_free_model(model);

  librdf_free_storage(storage);

  librdf_free_uri(uri);

  librdf_free_world(world);

#ifdef LIBRDF_MEMORY_DEBUG
  librdf_memory_report(stderr);
#endif
	
  /* keep gcc -Wall happy */
  return(0);
}
