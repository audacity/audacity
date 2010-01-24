/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_statement.h - RDF Statement definition
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



#ifndef LIBRDF_STATEMENT_H
#define LIBRDF_STATEMENT_H

#ifdef LIBRDF_INTERNAL
#include <rdf_statement_internal.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

/**
 * librdf_statement_part:
 * @LIBRDF_STATEMENT_SUBJECT: Subject of a statement.
 * @LIBRDF_STATEMENT_PREDICATE: Predicate of a statement.
 * @LIBRDF_STATEMENT_OBJECT: Object of a statement.
 * @LIBRDF_STATEMENT_ALL: All parts of a statement.
 *
 * Flags that are or-ed to indicate statement parts.
 *
 * Used in fields arguments to methods such as the public
 * librdf_statement_encode_parts() librdf_statement_decode_parts()
 * librdf_new_stream_from_node_iterator() and the internal
 * librdf_storage_node_stream_to_node_create()
 */
typedef enum {
  LIBRDF_STATEMENT_SUBJECT   = 1 << 0,
  LIBRDF_STATEMENT_PREDICATE = 1 << 1,
  LIBRDF_STATEMENT_OBJECT    = 1 << 2,

  /* must be a combination of all of the above */
  LIBRDF_STATEMENT_ALL       = (LIBRDF_STATEMENT_SUBJECT|
                                LIBRDF_STATEMENT_PREDICATE|
                                LIBRDF_STATEMENT_OBJECT)
} librdf_statement_part;


/* initialising functions / constructors */

/* Create a new Statement. */
REDLAND_API
librdf_statement* librdf_new_statement(librdf_world* world);

/* Create a new Statement from an existing Statement - CLONE */
REDLAND_API
librdf_statement* librdf_new_statement_from_statement(librdf_statement* statement);
/* Create a new Statement from existing Nodes */
REDLAND_API
librdf_statement* librdf_new_statement_from_nodes(librdf_world *world, librdf_node* subject, librdf_node* predicate, librdf_node* object);

/* Init a statically allocated statement */
REDLAND_API
void librdf_statement_init(librdf_world *world, librdf_statement *statement);

/* Clear a statically allocated statement */
REDLAND_API
void librdf_statement_clear(librdf_statement *statement);

/* destructor */
REDLAND_API
void librdf_free_statement(librdf_statement* statement);


/* functions / methods */

REDLAND_API
librdf_node* librdf_statement_get_subject(librdf_statement *statement);
REDLAND_API
void librdf_statement_set_subject(librdf_statement *statement, librdf_node *node);

REDLAND_API
librdf_node* librdf_statement_get_predicate(librdf_statement *statement);
REDLAND_API
void librdf_statement_set_predicate(librdf_statement *statement, librdf_node *node);

REDLAND_API
librdf_node* librdf_statement_get_object(librdf_statement *statement);
REDLAND_API
void librdf_statement_set_object(librdf_statement *statement, librdf_node *node);

/* if statement has all fields */
REDLAND_API
int librdf_statement_is_complete(librdf_statement *statement);

/* convert to a string */
REDLAND_API
unsigned char *librdf_statement_to_string(librdf_statement *statement);
/* print it prettily */
REDLAND_API
void librdf_statement_print(librdf_statement *statement, FILE *fh);

/* compare two statements */
REDLAND_API
int librdf_statement_equals(librdf_statement* statement1, librdf_statement* statement2);
/* match statement against one with partial content */
REDLAND_API
int librdf_statement_match(librdf_statement* statement, librdf_statement* partial_statement);

/* serialising/deserialising */
REDLAND_API
size_t librdf_statement_encode(librdf_statement* statement, unsigned char *buffer, size_t length);
REDLAND_API
size_t librdf_statement_encode_parts(librdf_statement* statement, librdf_node* context_node, unsigned char *buffer, size_t length, librdf_statement_part fields);
REDLAND_API
size_t librdf_statement_decode(librdf_statement* statement, unsigned char *buffer, size_t length);
REDLAND_API
size_t librdf_statement_decode_parts(librdf_statement* statement, librdf_node** context_node, unsigned char *buffer, size_t length);


#ifdef __cplusplus
}
#endif

#endif
