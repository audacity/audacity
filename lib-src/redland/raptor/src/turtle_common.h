/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * turtle_common.h - Turtle lexer/parser shared internals
 *
 * Copyright (C) 2003-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2003-2004, University of Bristol, UK http://www.bristol.ac.uk/
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

#ifndef TURTLE_COMMON_H
#define TURTLE_COMMON_H

#ifdef __cplusplus
extern "C" {
#endif


/* turtle_parser.y */
int turtle_syntax_error(raptor_parser *rdf_parser, const char *message, ...) RAPTOR_PRINTF_FORMAT(2, 3);
raptor_uri* turtle_qname_to_uri(raptor_parser *rdf_parser, unsigned char *name, size_t name_len);


/*
 * Turtle parser object
 */
struct raptor_turtle_parser_s {
  /* buffer */
  char *buffer;

  /* buffer length */
  int buffer_length;
  
  /* static statement for use in passing to user code */
  raptor_statement statement;

  raptor_namespace_stack namespaces;

  /* for lexer to store result in */
  YYSTYPE lval;

  /* STATIC lexer */
  yyscan_t scanner;

  int scanner_set;

  int lineno;

  raptor_uri* nil_uri;
  raptor_uri* first_uri;
  raptor_uri* rest_uri;

  /* for creating long literals */
  raptor_stringbuffer* sb;

  /* Allow TRIG extensions */
  int trig;

  /* count of errors in current parse */
  int error_count;
};


#ifdef __cplusplus
}
#endif

#endif
