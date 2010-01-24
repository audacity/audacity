/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * example4.c - Redland example code using the serializing
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

#include <redland.h>

/* one prototype needed */
int main(int argc, char *argv[]);


int
main(int argc, char *argv[]) 
{
  librdf_world* world;
  librdf_storage *storage;
  librdf_model* model;
  librdf_uri* uri, *base_uri;
  librdf_parser* parser;
  librdf_serializer* serializer;
  
  world=librdf_new_world();
  librdf_world_open(world);

  model=librdf_new_model(world, storage=librdf_new_storage(world, "hashes", "test", "hash-type='bdb',dir='.'"), NULL);

  parser=librdf_new_parser(world,"rdfxml","application/rdf+xml",NULL);
  uri=librdf_new_uri(world,"file:../data/dc.rdf");
  librdf_parser_parse_into_model(parser,uri,uri,model);
  librdf_free_uri(uri);
  librdf_free_parser(parser);

  serializer=librdf_new_serializer(world, "rdfxml", NULL, NULL);
  base_uri=librdf_new_uri(world,"http://exampe.org/base.rdf");
  librdf_serializer_serialize_model(serializer, stdout, base_uri, model);
  librdf_free_serializer(serializer);
  librdf_free_uri(base_uri);
  
  librdf_free_model(model);
  librdf_free_storage(storage);

  librdf_free_world(world);

#ifdef LIBRDF_MEMORY_DEBUG
  librdf_memory_report(stderr);
#endif
	
  /* keep gcc -Wall happy */
  return(0);
}
