/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * example7.c - read from a file 'file.rdf', add a triple, write it again
 *
 * Compile with:
 * gcc -o example7 `redland-config --cflags` example7.c `redland-config --libs`
 *
 * Copyright (C) 2005-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2005-2005, University of Bristol, UK http://www.bristol.ac.uk/
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

#include <redland.h>

int
main(int argc, char *argv[]) 
{
  librdf_world* world;
  librdf_storage* storage;
  librdf_model* model;
  librdf_statement *statement;

  /* See example1.c for the error checking that should be here */
  world=librdf_new_world();
  librdf_world_open(world);
  storage=librdf_new_storage(world, "file", "file.rdf", NULL);
  model=librdf_new_model(world, storage, NULL);
  statement=librdf_new_statement_from_nodes(world, librdf_new_node_from_uri_string(world, (const unsigned char*)"http://example.org/"),
                                            librdf_new_node_from_uri_string(world, (const unsigned char*)"http://purl.org/dc/elements/1.1/title"),
                                            librdf_new_node_from_literal(world, (const unsigned char*)"Blah", NULL, 0)
                                            );
  librdf_model_add_statement(model, statement);
  librdf_free_statement(statement);
  librdf_free_model(model);
  librdf_free_storage(storage);
  librdf_free_world(world);
}
