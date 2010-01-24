/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * example2.c - Redland example code parsing RDF/XML from a string in memory and adding/checking/removing a statement
 *
 * $Id: example2.c,v 1.1 2008-07-08 10:39:59 larsl Exp $
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
#include <ctype.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>

#include <redland.h>

/* one prototype needed */
int main(int argc, char *argv[]);


static unsigned const char *rdfxml_content=(unsigned const char *)
"<?xml version=\"1.0\"?>\
<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
     xmlns:dc=\"http://purl.org/dc/elements/1.1/\">\
  <rdf:Description rdf:about=\"http://www.dajobe.org/\">\
    <dc:title>Dave Beckett's Home Page</dc:title>\
    <dc:creator>Dave Beckett</dc:creator>\
    <dc:description>The generic home page of Dave Beckett.</dc:description>\
  </rdf:Description> \
</rdf:RDF>\
";

int
main(int argc, char *argv[]) 
{
  librdf_world* world;
  librdf_storage* storage;
  librdf_model* model;
  librdf_parser* parser;
  librdf_statement* statement;
  librdf_uri* uri;
  char *program=argv[0];
  int rc=0;
  
  world=librdf_new_world();
  librdf_world_open(world);


  uri=librdf_new_uri(world, (const unsigned char*)"http://example.librdf.org/");
  
  if(!uri) {
    fprintf(stderr, "%s: Failed to create URI\n", program);
    return(1);
  }

  storage=librdf_new_storage(world, "memory", "test", NULL);
  if(!storage) {
    fprintf(stderr, "%s: Failed to create new storage\n", program);
    rc=1;
    goto tidyworld;
  }

  model=librdf_new_model(world, storage, NULL);
  if(!model) {
    fprintf(stderr, "%s: Failed to create model\n", program);
    rc=1;
    goto tidystorage;
  }

  parser=librdf_new_parser(world, "rdfxml", NULL, NULL);
  if(!parser) {
    fprintf(stderr, "%s: Failed to create new parser 'rdfxml'\n", program);
    rc=1;
    goto tidystorage;
  }


  if(librdf_parser_parse_string_into_model(parser, rdfxml_content, uri, model)) {
    fprintf(stderr, "%s: Failed to parse RDF into model\n", program);
    librdf_free_uri(uri);
    rc=1;
    goto tidymodel;
  }

  librdf_free_parser(parser); parser=NULL;
  
  librdf_free_uri(uri); uri=NULL;


  statement=librdf_new_statement(world);
  if(!statement) {
    fprintf(stderr, "%s: Failed to parse RDF into model\n", program);
    rc=1;
    goto tidymodel;
  }
  
  librdf_statement_set_subject(statement, 
                               librdf_new_node_from_uri_string(world, (const unsigned char*)"http://example.org/subject"));
  
  librdf_statement_set_predicate(statement,
                                   librdf_new_node_from_uri_string(world, (const unsigned char*)"http://example.org/pred1"));
  
  librdf_statement_set_object(statement,
                              librdf_new_node_from_literal(world, (const unsigned char*)"object", NULL, 0));
  
  librdf_model_add_statement(model, statement);

  fprintf(stdout, "%s: Resulting model is:\n", program);
  librdf_model_print(model, stdout);

  if(!librdf_model_contains_statement(model, statement)) {
    fprintf(stdout, "%s: Model does not contain statement\n", program);
    rc=1;
    goto tidystatement;
  } else
    fprintf(stdout, "%s: Model contains the statement\n", program);


  fprintf(stdout, "%s: Removing the statement\n", program);
  librdf_model_remove_statement(model, statement);

  fprintf(stdout, "%s: Resulting model is:\n", program);
  librdf_model_print(model, stdout);

 tidystatement:
  librdf_free_statement(statement);

 tidymodel:
  librdf_free_model(model);

 tidystorage:
  librdf_free_storage(storage);

 tidyworld:
  librdf_free_world(world);

#ifdef LIBRDF_MEMORY_DEBUG
  librdf_memory_report(stderr);
#endif
	
  /* keep gcc -Wall happy */
  return(rc);
}
