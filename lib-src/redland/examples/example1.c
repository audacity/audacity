/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * example.c - Redland example parsing RDF from a URI, storing on disk as BDB hashes and querying the results
 *
 * Copyright (C) 2000-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2000-2004, University of Bristol, UK http://www.bristol.ac.uk/
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


#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#include <redland.h>


/* one prototype needed */
int main(int argc, char *argv[]);



int
main(int argc, char *argv[]) 
{
  librdf_world* world;
  librdf_storage* storage;
  librdf_parser* parser;
  librdf_model* model;
  librdf_stream* stream;
  librdf_node *subject, *predicate;
  librdf_iterator* iterator;
  librdf_statement *partial_statement, *statement2;
  char *program=argv[0];
  librdf_uri *uri;
  char *parser_name=NULL;
  int count;

  if(argc <2 || argc >3) {
    fprintf(stderr, "USAGE: %s: <RDF source URI> [rdf parser name]\n", program);
    return(1);
  }


  world=librdf_new_world();
  librdf_world_open(world);

  uri=librdf_new_uri(world, (const unsigned char*)argv[1]);
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
  
      
  if(argc==3)
    parser_name=argv[2];

  parser=librdf_new_parser(world, parser_name, NULL, NULL);
  if(!parser) {
    fprintf(stderr, "%s: Failed to create new parser\n", program);
    return(1);
  }


  /* PARSE the URI as RDF/XML*/
  fprintf(stdout, "%s: Parsing URI %s\n", program, librdf_uri_as_string(uri));
  if(librdf_parser_parse_into_model(parser, uri, uri, model)) {
    fprintf(stderr, "%s: Failed to parse RDF into model\n", program);
    return(1);
  }
  librdf_free_parser(parser);

  
  statement2=librdf_new_statement_from_nodes(world, librdf_new_node_from_uri_string(world, (const unsigned char*)"http://www.dajobe.org/"),
                                             librdf_new_node_from_uri_string(world, (const unsigned char*)"http://purl.org/dc/elements/1.1/title"),
                                             librdf_new_node_from_literal(world, (const unsigned char*)"My Home Page", NULL, 0)
                                             );
  librdf_model_add_statement(model, statement2);

  /* Free what we just used to add to the model - now it should be stored */
  librdf_free_statement(statement2);


  /* Print out the model*/


  fprintf(stdout, "%s: Resulting model is:\n", program);
  librdf_model_print(model, stdout);

  /* Construct the query predicate (arc) and object (target) 
   * and partial statement bits
   */
  subject=librdf_new_node_from_uri_string(world, (const unsigned char*)"http://www.dajobe.org/");
  predicate=librdf_new_node_from_uri_string(world, (const unsigned char*)"http://purl.org/dc/elements/1.1/title");
  if(!subject || !predicate) {
    fprintf(stderr, "%s: Failed to create nodes for searching\n", program);
    return(1);
  }
  partial_statement=librdf_new_statement(world);
  librdf_statement_set_subject(partial_statement, subject);
  librdf_statement_set_predicate(partial_statement, predicate);


  /* QUERY TEST 1 - use find_statements to match */

  fprintf(stdout, "%s: Trying to find_statements\n", program);
  stream=librdf_model_find_statements(model, partial_statement);
  if(!stream) {
    fprintf(stderr, "%s: librdf_model_find_statements returned NULL stream\n", program);
  } else {
    count=0;
    while(!librdf_stream_end(stream)) {
      librdf_statement *statement=librdf_stream_get_object(stream);
      if(!statement) {
        fprintf(stderr, "%s: librdf_stream_next returned NULL\n", program);
        break;
      }
      
      fputs("  Matched statement: ", stdout);
      librdf_statement_print(statement, stdout);
      fputc('\n', stdout);
      
      librdf_stream_next(stream);
      count++;
    }
    librdf_free_stream(stream);  
    fprintf(stderr, "%s: got %d matching statements\n", program, count);
  }


  /* QUERY TEST 2 - use get_targets to do match */
  fprintf(stdout, "%s: Trying to get targets\n", program);
  iterator=librdf_model_get_targets(model, subject, predicate);
  if(!iterator)  {
    fprintf(stderr, "%s: librdf_model_get_targets failed to return iterator for searching\n", program);
    return(1);
  }

  count=0;
  while(!librdf_iterator_end(iterator)) {
    librdf_node *target;
    
    target=(librdf_node*)librdf_iterator_get_object(iterator);
    if(!target) {
      fprintf(stderr, "%s: librdf_iterator_get_object returned NULL\n", program);
      break;
    }

    fputs("  Matched target: ", stdout);
    librdf_node_print(target, stdout);
    fputc('\n', stdout);

    count++;
    librdf_iterator_next(iterator);
  }
  librdf_free_iterator(iterator);
  fprintf(stderr, "%s: got %d target nodes\n", program, count);

  librdf_free_statement(partial_statement);
  /* the above does this since they are still attached */
  /* librdf_free_node(predicate); */
  /* librdf_free_node(object); */

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
