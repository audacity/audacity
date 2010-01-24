/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * turtle_parser.y - Raptor Turtle parser - over tokens from turtle grammar lexer
 *
 * Copyright (C) 2003-2007, David Beckett http://purl.org/net/dajobe/
 * Copyright (C) 2003-2005, University of Bristol, UK http://www.bristol.ac.uk/
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
 * Turtle is defined in http://www.dajobe.org/2004/01/turtle/
 *
 * Made from a subset of the terms in
 *   http://www.w3.org/DesignIssues/Notation3.html
 *
 */

%{
#ifdef HAVE_CONFIG_H
#include <raptor_config.h>
#endif

#ifdef WIN32
#include <win32_raptor_config.h>
#endif

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include <raptor.h>
#include <raptor_internal.h>

#include <turtle_parser.h>

#define YY_DECL int turtle_lexer_lex (YYSTYPE *turtle_parser_lval, yyscan_t yyscanner)
#define YY_NO_UNISTD_H 1
#include <turtle_lexer.h>

#include <turtle_common.h>


/* Make verbose error messages for syntax errors */
#ifdef RAPTOR_DEBUG
#define YYERROR_VERBOSE 1
#endif

/* Slow down the grammar operation and watch it work */
#if RAPTOR_DEBUG > 2
#define YYDEBUG 1
#endif

/* the lexer does not seem to track this */
#undef RAPTOR_TURTLE_USE_ERROR_COLUMNS

/* Prototypes */ 
int turtle_parser_error(void* rdf_parser, const char *msg);

/* Missing turtle_lexer.c/h prototypes */
int turtle_lexer_get_column(yyscan_t yyscanner);
/* Not used here */
/* void turtle_lexer_set_column(int  column_no , yyscan_t yyscanner);*/


/* What the lexer wants */
extern int turtle_lexer_lex (YYSTYPE *turtle_parser_lval, yyscan_t scanner);
#define YYLEX_PARAM ((raptor_turtle_parser*)(((raptor_parser*)rdf_parser)->context))->scanner

/* Pure parser argument (a void*) */
#define YYPARSE_PARAM rdf_parser

/* Make the yyerror below use the rdf_parser */
#undef yyerror
#define yyerror(message) turtle_parser_error(rdf_parser, message)

/* Make lex/yacc interface as small as possible */
#undef yylex
#define yylex turtle_lexer_lex


static raptor_triple* raptor_turtle_new_triple(raptor_identifier *subject, raptor_identifier *predicate, raptor_identifier *object);
static void raptor_turtle_free_triple(raptor_triple *triple);

#ifdef RAPTOR_DEBUG
static void raptor_triple_print(raptor_triple *data, FILE *fh);
#endif


/* Prototypes for local functions */
static void raptor_turtle_generate_statement(raptor_parser *parser, raptor_triple *triple);

%}


/* directives */



%pure-parser


/* Interface between lexer and parser */
%union {
  unsigned char *string;
  raptor_identifier *identifier;
  raptor_sequence *sequence;
  raptor_uri *uri;
  int integer; /* 0+ for a xsd:integer datatyped RDF literal */
}

%expect 0


/* others */

%token A "a"
%token AT "@"
%token HAT "^"
%token DOT "."
%token COMMA ","
%token SEMICOLON ";"
%token LEFT_SQUARE "["
%token RIGHT_SQUARE "]"
%token LEFT_ROUND "("
%token RIGHT_ROUND ")"
%token LEFT_CURLY "{"
%token RIGHT_CURLY "}"
%token COLONMINUS ":-"
%token TRUE_TOKEN "true"
%token FALSE_TOKEN "false"

/* literals */
%token <string> STRING_LITERAL "string literal"
%token <uri> URI_LITERAL "URI literal"
%token <string> BLANK_LITERAL "blank node"
%token <uri> QNAME_LITERAL "QName"
%token <string> PREFIX "@prefix"
%token <string> BASE "@base"
%token <string> IDENTIFIER "identifier"
%token <string> INTEGER_LITERAL "integer literal"
%token <string> FLOATING_LITERAL "floating point literal"
%token <string> DECIMAL_LITERAL "decimal literal"

/* syntax error */
%token ERROR_TOKEN

%type <identifier> subject predicate object verb literal resource blank collection graphName
%type <sequence> objectList itemList propertyList

/* tidy up tokens after errors */
%destructor { if($$) RAPTOR_FREE(cstring, $$); } STRING_LITERAL BLANK_LITERAL INTEGER_LITERAL FLOATING_LITERAL DECIMAL_LITERAL IDENTIFIER
%destructor { if($$) raptor_free_uri($$); } URI_LITERAL QNAME_LITERAL
%destructor { if($$) raptor_free_identifier($$); } subject predicate object verb literal resource blank collection graphName
%destructor { if($$) raptor_free_sequence($$); } objectList itemList propertyList

%%

Document : statementList
;

colonMinusOpt: COLONMINUS 
{
  raptor_parser* parser=(raptor_parser *)rdf_parser;
  raptor_turtle_parser* turtle_parser=(raptor_turtle_parser*)parser->context;
  if(!turtle_parser->trig)
    turtle_parser_error(rdf_parser, ":- is not allowed in Turtle");
}
| /* empty */
;

graph: graphName colonMinusOpt LEFT_CURLY
  {
    /* action in mid-rule so this is run BEFORE the triples in graphBody */
    raptor_parser* parser=(raptor_parser *)rdf_parser;
    raptor_turtle_parser* turtle_parser=(raptor_turtle_parser*)parser->context;
    if(!turtle_parser->trig)
      turtle_parser_error(rdf_parser, "{ ... } is not allowed in Turtle");
    else
      raptor_parser_set_graph_name(parser, $1->uri);
  }
  graphBody RIGHT_CURLY
{
  /* free graph name in final action */
  raptor_free_identifier($1);
}
|
LEFT_CURLY
  {
    /* action in mid-rule so this is run BEFORE the triples in graphBody */
    raptor_parser* parser=(raptor_parser *)rdf_parser;
    raptor_turtle_parser* turtle_parser=(raptor_turtle_parser*)parser->context;
    if(!turtle_parser->trig)
      turtle_parser_error(rdf_parser, "{ ... } is not allowed in Turtle");
    else
      raptor_parser_set_graph_name(parser, NULL);
  }
  graphBody RIGHT_CURLY
;

graphName: resource
;

graphBody: triplesList
|
/* empty */
;

triplesList: triples
|
terminatedTriples triplesList
|
terminatedTriples
;

terminatedTriples: triples DOT
;

statementList: statementList statement
| /* empty line */
;

statement: directive
| graph
| terminatedTriples
;

triples: subject propertyList
{
  int i;

#if RAPTOR_DEBUG > 1  
  printf("statement 2\n subject=");
  if($1)
    raptor_identifier_print(stdout, $1);
  else
    fputs("NULL", stdout);
  if($2) {
    printf("\n propertyList (reverse order to syntax)=");
    raptor_sequence_print($2, stdout);
    printf("\n");
  } else     
    printf("\n and empty propertyList\n");
#endif

  if($1 && $2) {
    /* have subject and non-empty property list, handle it  */
    for(i=0; i<raptor_sequence_size($2); i++) {
      raptor_triple* t2=(raptor_triple*)raptor_sequence_get_at($2, i);
      raptor_identifier *i2=(raptor_identifier*)RAPTOR_CALLOC(raptor_identifier, 1, sizeof(raptor_identifier));
      if(!i2) {
        raptor_free_sequence($2);
        raptor_free_identifier($1);
        YYERROR;
      }
      raptor_copy_identifier(i2, $1);
      t2->subject=i2;
      t2->subject->is_malloced=1;
    }
#if RAPTOR_DEBUG > 1  
    printf(" after substitution propertyList=");
    raptor_sequence_print($2, stdout);
    printf("\n\n");
#endif
    for(i=0; i<raptor_sequence_size($2); i++) {
      raptor_triple* t2=(raptor_triple*)raptor_sequence_get_at($2, i);
      raptor_turtle_generate_statement((raptor_parser*)rdf_parser, t2);
    }
  }

  if($2)
    raptor_free_sequence($2);

  if($1)
    raptor_free_identifier($1);
}
| error DOT
;


objectList: objectList COMMA object
{
  raptor_triple *triple;

#if RAPTOR_DEBUG > 1  
  printf("objectList 1\n");
  if($3) {
    printf(" object=\n");
    raptor_identifier_print(stdout, $3);
    printf("\n");
  } else  
    printf(" and empty object\n");
  if($1) {
    printf(" objectList=");
    raptor_sequence_print($1, stdout);
    printf("\n");
  } else
    printf(" and empty objectList\n");
#endif

  if(!$3)
    $$=NULL;
  else {
    triple=raptor_turtle_new_triple(NULL, NULL, $3);
    if(!triple) {
      raptor_free_sequence($1);
      YYERROR;
    }
    if(raptor_sequence_push($1, triple)) {
      raptor_free_sequence($1);
      YYERROR;
    }
    $$=$1;
#if RAPTOR_DEBUG > 1  
    printf(" objectList is now ");
    raptor_sequence_print($$, stdout);
    printf("\n\n");
#endif
  }
}
| object
{
  raptor_triple *triple;

#if RAPTOR_DEBUG > 1  
  printf("objectList 2\n");
  if($1) {
    printf(" object=\n");
    raptor_identifier_print(stdout, $1);
    printf("\n");
  } else  
    printf(" and empty object\n");
#endif

  if(!$1)
    $$=NULL;
  else {
    triple=raptor_turtle_new_triple(NULL, NULL, $1);
    if(!triple)
      YYERROR;
#ifdef RAPTOR_DEBUG
    $$=raptor_new_sequence((raptor_sequence_free_handler*)raptor_turtle_free_triple,
                           (raptor_sequence_print_handler*)raptor_triple_print);
#else
    $$=raptor_new_sequence((raptor_sequence_free_handler*)raptor_turtle_free_triple, NULL);
#endif
    if(!$$) {
      raptor_turtle_free_triple(triple);
      YYERROR;
    }
    if(raptor_sequence_push($$, triple)) {
      raptor_free_sequence($$);
      $$=NULL;
      YYERROR;
    }
#if RAPTOR_DEBUG > 1  
    printf(" objectList is now ");
    raptor_sequence_print($$, stdout);
    printf("\n\n");
#endif
  }
}
;

itemList: itemList object
{
  raptor_triple *triple;

#if RAPTOR_DEBUG > 1  
  printf("objectList 1\n");
  if($2) {
    printf(" object=\n");
    raptor_identifier_print(stdout, $2);
    printf("\n");
  } else  
    printf(" and empty object\n");
  if($1) {
    printf(" objectList=");
    raptor_sequence_print($1, stdout);
    printf("\n");
  } else
    printf(" and empty objectList\n");
#endif

  if(!$2)
    $$=NULL;
  else {
    triple=raptor_turtle_new_triple(NULL, NULL, $2);
    if(!triple) {
      raptor_free_sequence($1);
      YYERROR;
    }
    if(raptor_sequence_push($1, triple)) {
      raptor_free_sequence($1);
      YYERROR;
    }
    $$=$1;
#if RAPTOR_DEBUG > 1  
    printf(" objectList is now ");
    raptor_sequence_print($$, stdout);
    printf("\n\n");
#endif
  }
}
| object
{
  raptor_triple *triple;

#if RAPTOR_DEBUG > 1  
  printf("objectList 2\n");
  if($1) {
    printf(" object=\n");
    raptor_identifier_print(stdout, $1);
    printf("\n");
  } else  
    printf(" and empty object\n");
#endif

  if(!$1)
    $$=NULL;
  else {
    triple=raptor_turtle_new_triple(NULL, NULL, $1);
    if(!triple)
      YYERROR;
#ifdef RAPTOR_DEBUG
    $$=raptor_new_sequence((raptor_sequence_free_handler*)raptor_turtle_free_triple,
                           (raptor_sequence_print_handler*)raptor_triple_print);
#else
    $$=raptor_new_sequence((raptor_sequence_free_handler*)raptor_turtle_free_triple, NULL);
#endif
    if(!$$) {
      raptor_turtle_free_triple(triple);
      YYERROR;
    }
    if(raptor_sequence_push($$, triple)) {
      raptor_free_sequence($$);
      $$=NULL;
      YYERROR;
    }
#if RAPTOR_DEBUG > 1  
    printf(" objectList is now ");
    raptor_sequence_print($$, stdout);
    printf("\n\n");
#endif
  }
}
;

verb: predicate
{
#if RAPTOR_DEBUG > 1  
  printf("verb predicate=");
  raptor_identifier_print(stdout, $1);
  printf("\n");
#endif

  $$=$1;
}
| A
{
  raptor_uri *uri;

#if RAPTOR_DEBUG > 1  
  printf("verb predicate=rdf:type (a)\n");
#endif

  uri=raptor_new_uri_for_rdf_concept("type");
  if(!uri)
    YYERROR;
  $$=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_RESOURCE, uri, RAPTOR_URI_SOURCE_URI, NULL, NULL, NULL, NULL);
  if(!$$)
    YYERROR;
}
;


propertyList: propertyList SEMICOLON verb objectList
{
  int i;
  
#if RAPTOR_DEBUG > 1  
  printf("propertyList 1\n verb=");
  raptor_identifier_print(stdout, $3);
  printf("\n objectList=");
  raptor_sequence_print($4, stdout);
  printf("\n propertyList=");
  raptor_sequence_print($1, stdout);
  printf("\n\n");
#endif
  
  if($4 == NULL) {
#if RAPTOR_DEBUG > 1  
    printf(" empty objectList not processed\n");
#endif
  } else if($3 && $4) {
    /* non-empty property list, handle it  */
    for(i=0; i<raptor_sequence_size($4); i++) {
      raptor_triple* t2=(raptor_triple*)raptor_sequence_get_at($4, i);
      raptor_identifier *i2=(raptor_identifier*)RAPTOR_CALLOC(raptor_identifier, 1, sizeof(raptor_identifier));
      if(!i2) {
        if($1)
          raptor_free_sequence($1);
        raptor_free_identifier($3);
        raptor_free_sequence($4);
        YYERROR;
      }
      if(raptor_copy_identifier(i2, $3)) {
        if($1)
          raptor_free_sequence($1);
        raptor_free_identifier($3);
        raptor_free_sequence($4);
        YYERROR;
      }
      t2->predicate=i2;
      t2->predicate->is_malloced=1;
    }
  
#if RAPTOR_DEBUG > 1  
    printf(" after substitution objectList=");
    raptor_sequence_print($4, stdout);
    printf("\n");
#endif
  }

  if($1 == NULL) {
#if RAPTOR_DEBUG > 1  
    printf(" empty propertyList not copied\n\n");
#endif
  } else if ($3 && $4 && $1) {
    while(raptor_sequence_size($4)) {
      raptor_triple* t2=(raptor_triple*)raptor_sequence_unshift($4);
      if(raptor_sequence_push($1, t2)) {
        raptor_free_sequence($1);
        raptor_free_identifier($3);
        raptor_free_sequence($4);
        YYERROR;
      }
    }

#if RAPTOR_DEBUG > 1  
    printf(" after appending objectList (reverse order)=");
    raptor_sequence_print($1, stdout);
    printf("\n\n");
#endif

    raptor_free_sequence($4);
  }

  if($3)
    raptor_free_identifier($3);

  $$=$1;
}
| verb objectList
{
  int i;
#if RAPTOR_DEBUG > 1  
  printf("propertyList 2\n verb=");
  raptor_identifier_print(stdout, $1);
  if($2) {
    printf("\n objectList=");
    raptor_sequence_print($2, stdout);
    printf("\n");
  } else
    printf("\n and empty objectList\n");
#endif

  if($1 && $2) {
    for(i=0; i<raptor_sequence_size($2); i++) {
      raptor_triple* t2=(raptor_triple*)raptor_sequence_get_at($2, i);
      raptor_identifier *i2=(raptor_identifier*)RAPTOR_CALLOC(raptor_identifier, 1, sizeof(raptor_identifier));
      if(!i2) {
        raptor_free_identifier($1);
        raptor_free_sequence($2);
        YYERROR;
      }
      if(raptor_copy_identifier(i2, $1)) {
        raptor_free_identifier($1);
        raptor_free_sequence($2);
        YYERROR;
      }
      t2->predicate=i2;
      t2->predicate->is_malloced=1;
    }

#if RAPTOR_DEBUG > 1  
    printf(" after substitution objectList=");
    raptor_sequence_print($2, stdout);
    printf("\n\n");
#endif
  }

  if($1)
    raptor_free_identifier($1);

  $$=$2;
}
| /* empty */
{
#if RAPTOR_DEBUG > 1  
  printf("propertyList 4\n empty returning NULL\n\n");
#endif
  $$=NULL;
}
| propertyList SEMICOLON
{
  $$=$1;
#if RAPTOR_DEBUG > 1  
  printf("propertyList 5\n trailing semicolon returning existing list ");
  raptor_sequence_print($$, stdout);
  printf("\n\n");
#endif
}
;

directive : prefix | base
;

prefix: PREFIX IDENTIFIER URI_LITERAL DOT
{
  unsigned char *prefix=$2;
  raptor_turtle_parser* turtle_parser=(raptor_turtle_parser*)(((raptor_parser*)rdf_parser)->context);
  raptor_namespace *ns;

#if 0
  Get around bison complaining about not using $1
#endif

#if RAPTOR_DEBUG > 1  
  printf("directive @prefix %s %s\n",($2 ? (char*)$2 : "(default)"),raptor_uri_as_string($3));
#endif

  if(prefix) {
    size_t len=strlen((const char*)prefix);
    if(prefix[len-1] == ':') {
      if(len == 1)
         /* declaring default namespace prefix @prefix : ... */
        prefix=NULL;
      else
        prefix[len-1]='\0';
    }
  }

  ns=raptor_new_namespace_from_uri(&turtle_parser->namespaces, prefix, $3, 0);
  if(ns) {
    raptor_namespaces_start_namespace(&turtle_parser->namespaces, ns);
    raptor_parser_start_namespace((raptor_parser*)rdf_parser, ns);
  }

  if($2)
    RAPTOR_FREE(cstring, $2);
  raptor_free_uri($3);

  if(!ns)
    YYERROR;
}
;


base: BASE URI_LITERAL DOT
{
  raptor_uri *uri=$2;
  /*raptor_turtle_parser* turtle_parser=(raptor_turtle_parser*)(((raptor_parser*)rdf_parser)->context);*/
  raptor_parser* parser=(raptor_parser*)rdf_parser;
  if(parser->base_uri)
    raptor_free_uri(parser->base_uri);
  parser->base_uri=uri;
}
;

subject: resource
{
  $$=$1;
}
| blank
{
  $$=$1;
}
;


predicate: resource
{
  $$=$1;
}
;


object: resource
{
  $$=$1;
}
| blank
{
  $$=$1;
}
| literal
{
#if RAPTOR_DEBUG > 1  
  printf("object literal=");
  raptor_identifier_print(stdout, $1);
  printf("\n");
#endif

  $$=$1;
}
;


literal: STRING_LITERAL AT IDENTIFIER
{
#if RAPTOR_DEBUG > 1  
  printf("literal + language string=\"%s\"\n", $1);
#endif

  $$=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_LITERAL, NULL, RAPTOR_URI_SOURCE_ELEMENT, NULL, $1, NULL, $3);
  if(!$$)
    YYERROR;
}
| STRING_LITERAL AT IDENTIFIER HAT URI_LITERAL
{
#if RAPTOR_DEBUG > 1  
  printf("literal + language=\"%s\" datatype string=\"%s\" uri=\"%s\"\n", $1, $3, raptor_uri_as_string($5));
#endif

  if($5) {
    $$=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_LITERAL, NULL, RAPTOR_URI_SOURCE_ELEMENT, NULL, $1, $5, $3);
    if(!$$)
      YYERROR;
  } else
    $$=NULL;
    
}
| STRING_LITERAL AT IDENTIFIER HAT QNAME_LITERAL
{
#if RAPTOR_DEBUG > 1  
  printf("literal + language=\"%s\" datatype string=\"%s\" qname URI=<%s>\n", $1, $3, raptor_uri_as_string($5));
#endif

  if($5) {
    $$=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_LITERAL, NULL, RAPTOR_URI_SOURCE_ELEMENT, NULL, (const unsigned char*)$1, $5, $3);
    if(!$$)
      YYERROR;
  } else
    $$=NULL;

}
| STRING_LITERAL HAT URI_LITERAL
{
#if RAPTOR_DEBUG > 1  
  printf("literal + datatype string=\"%s\" uri=\"%s\"\n", $1, raptor_uri_as_string($3));
#endif

  if($3) {
    $$=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_LITERAL, NULL, RAPTOR_URI_SOURCE_ELEMENT, NULL, $1, $3, NULL);
    if(!$$)
      YYERROR;
  } else
    $$=NULL;
    
}
| STRING_LITERAL HAT QNAME_LITERAL
{
#if RAPTOR_DEBUG > 1  
  printf("literal + datatype string=\"%s\" qname URI=<%s>\n", $1, raptor_uri_as_string($3));
#endif

  if($3) {
    $$=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_LITERAL, NULL, RAPTOR_URI_SOURCE_ELEMENT, NULL, $1, $3, NULL);
    if(!$$)
      YYERROR;
  } else
    $$=NULL;
}
| STRING_LITERAL
{
#if RAPTOR_DEBUG > 1  
  printf("literal string=\"%s\"\n", $1);
#endif

  $$=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_LITERAL, NULL, RAPTOR_URI_SOURCE_ELEMENT, NULL, $1, NULL, NULL);
  if(!$$)
    YYERROR;
}
| INTEGER_LITERAL
{
  raptor_uri *uri;
#if RAPTOR_DEBUG > 1  
  printf("resource integer=%s\n", $1);
#endif
  uri=raptor_new_uri((const unsigned char*)"http://www.w3.org/2001/XMLSchema#integer");
  if(!uri) {
    RAPTOR_FREE(cstring, $1);
    YYERROR;
  }
  $$=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_LITERAL, NULL, RAPTOR_URI_SOURCE_ELEMENT, NULL, $1, uri, NULL);
  if(!$$)
    YYERROR;
}
| FLOATING_LITERAL
{
  raptor_uri *uri;
#if RAPTOR_DEBUG > 1  
  printf("resource double=%s\n", $1);
#endif
  uri=raptor_new_uri((const unsigned char*)"http://www.w3.org/2001/XMLSchema#double");
  if(!uri) {
    RAPTOR_FREE(cstring, $1);
    YYERROR;
  }
  $$=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_LITERAL, NULL, RAPTOR_URI_SOURCE_ELEMENT, NULL, $1, uri, NULL);
  if(!$$)
    YYERROR;
}
| DECIMAL_LITERAL
{
  raptor_uri *uri;
#if RAPTOR_DEBUG > 1  
  printf("resource decimal=%s\n", $1);
#endif
  uri=raptor_new_uri((const unsigned char*)"http://www.w3.org/2001/XMLSchema#decimal");
  if(!uri) {
    RAPTOR_FREE(cstring, $1);
    YYERROR;
  }
  $$=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_LITERAL, NULL, RAPTOR_URI_SOURCE_ELEMENT, NULL, $1, uri, NULL);
  if(!$$)
    YYERROR;
}
| TRUE_TOKEN
{
  unsigned char *string;
  raptor_uri *uri;
#if RAPTOR_DEBUG > 1  
  fputs("resource boolean true\n", stderr);
#endif
  uri=raptor_new_uri((const unsigned char*)"http://www.w3.org/2001/XMLSchema#boolean");
  if(!uri)
    YYERROR;
  string=(unsigned char*)RAPTOR_MALLOC(cstring, 5);
  if(!string) {
    raptor_free_uri(uri);
    YYERROR;
  }
  strncpy((char*)string, "true", 5);
  $$=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_LITERAL, NULL, RAPTOR_URI_SOURCE_ELEMENT, NULL, string, uri, NULL);
  if(!$$)
    YYERROR;
}
| FALSE_TOKEN
{
  unsigned char *string;
  raptor_uri *uri;
#if RAPTOR_DEBUG > 1  
  fputs("resource boolean false\n", stderr);
#endif
  uri=raptor_new_uri((const unsigned char*)"http://www.w3.org/2001/XMLSchema#boolean");
  if(!uri)
    YYERROR;
  string=(unsigned char*)RAPTOR_MALLOC(cstring, 6);
  if(!string) {
    raptor_free_uri(uri);
    YYERROR;
  }
  strncpy((char*)string, "false", 6);
  $$=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_LITERAL, NULL, RAPTOR_URI_SOURCE_ELEMENT, NULL, string, uri, NULL);
  if(!$$)
    YYERROR;
}
;


resource: URI_LITERAL
{
#if RAPTOR_DEBUG > 1  
  printf("resource URI=<%s>\n", raptor_uri_as_string($1));
#endif

  if($1) {
    $$=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_RESOURCE, $1, RAPTOR_URI_SOURCE_URI, NULL, NULL, NULL, NULL);
    if(!$$)
      YYERROR;
  } else
    $$=NULL;
}
| QNAME_LITERAL
{
#if RAPTOR_DEBUG > 1  
  printf("resource qname URI=<%s>\n", raptor_uri_as_string($1));
#endif

  if($1) {
    $$=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_RESOURCE, $1, RAPTOR_URI_SOURCE_ELEMENT, NULL, NULL, NULL, NULL);
    if(!$$)
      YYERROR;
  } else
    $$=NULL;
}
;


blank: BLANK_LITERAL
{
  const unsigned char *id;
#if RAPTOR_DEBUG > 1  
  printf("subject blank=\"%s\"\n", $1);
#endif
  id=raptor_parser_internal_generate_id((raptor_parser*)rdf_parser, RAPTOR_GENID_TYPE_BNODEID, $1);
  if(!id)
    YYERROR;

  $$=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_ANONYMOUS, NULL, RAPTOR_URI_SOURCE_BLANK_ID, id, NULL, NULL, NULL);
  if(!$$)
    YYERROR;
}
| LEFT_SQUARE propertyList RIGHT_SQUARE
{
  int i;
  const unsigned char *id;

  id=raptor_parser_internal_generate_id((raptor_parser*)rdf_parser, RAPTOR_GENID_TYPE_BNODEID, NULL);
  if(!id) {
    if($2)
      raptor_free_sequence($2);
    YYERROR;
  }

  $$=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_ANONYMOUS, NULL, RAPTOR_URI_SOURCE_GENERATED, id, NULL, NULL, NULL);
  if(!$$) {
    if($2)
      raptor_free_sequence($2);
    YYERROR;
  }

  if($2 == NULL) {
#if RAPTOR_DEBUG > 1  
    printf("resource\n propertyList=");
    raptor_identifier_print(stdout, $$);
    printf("\n");
#endif
  } else {
    /* non-empty property list, handle it  */
#if RAPTOR_DEBUG > 1  
    printf("resource\n propertyList=");
    raptor_sequence_print($2, stdout);
    printf("\n");
#endif

    for(i=0; i<raptor_sequence_size($2); i++) {
      raptor_triple* t2=(raptor_triple*)raptor_sequence_get_at($2, i);
      raptor_identifier *i2=(raptor_identifier*)RAPTOR_CALLOC(raptor_identifier, 1, sizeof(raptor_identifier));
      if(!i2) {
        raptor_free_sequence($2);
        raptor_free_identifier($$);
        $$=NULL;
        YYERROR;
      }
      if(raptor_copy_identifier(i2, $$)) {
        RAPTOR_FREE(raptor_identifier, i2);
        raptor_free_sequence($2);
        raptor_free_identifier($$);
        $$=NULL;
        YYERROR;
      }
      t2->subject=i2;
      t2->subject->is_malloced=1;
      raptor_turtle_generate_statement((raptor_parser*)rdf_parser, t2);
    }

#if RAPTOR_DEBUG > 1
    printf(" after substitution objectList=");
    raptor_sequence_print($2, stdout);
    printf("\n\n");
#endif

    raptor_free_sequence($2);

  }
  
}
| collection
{
  $$=$1;
}
;


collection: LEFT_ROUND itemList RIGHT_ROUND
{
  int i;
  raptor_turtle_parser* turtle_parser=(raptor_turtle_parser*)(((raptor_parser*)rdf_parser)->context);
  raptor_identifier* first_identifier=NULL;
  raptor_identifier* rest_identifier=NULL;
  raptor_identifier* object=NULL;
  raptor_identifier* blank=NULL;

#if RAPTOR_DEBUG > 1  
  printf("collection\n objectList=");
  raptor_sequence_print($2, stdout);
  printf("\n");
#endif

  first_identifier=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_RESOURCE, raptor_uri_copy(turtle_parser->first_uri), RAPTOR_URI_SOURCE_URI, NULL, NULL, NULL, NULL);
  if(!first_identifier)
    goto err_collection;
  rest_identifier=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_RESOURCE, raptor_uri_copy(turtle_parser->rest_uri), RAPTOR_URI_SOURCE_URI, NULL, NULL, NULL, NULL);
  if(!rest_identifier)
    goto err_collection;
  
  /* non-empty property list, handle it  */
#if RAPTOR_DEBUG > 1  
  printf("resource\n propertyList=");
  raptor_sequence_print($2, stdout);
  printf("\n");
#endif

  object=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_RESOURCE, raptor_uri_copy(turtle_parser->nil_uri), RAPTOR_URI_SOURCE_URI, NULL, NULL, NULL, NULL);
  if(!object)
    goto err_collection;

  for(i=raptor_sequence_size($2)-1; i>=0; i--) {
    raptor_identifier* temp;
    raptor_triple* t2=(raptor_triple*)raptor_sequence_get_at($2, i);
    const unsigned char *blank_id;

    blank_id=raptor_parser_internal_generate_id((raptor_parser*)rdf_parser, RAPTOR_GENID_TYPE_BNODEID, NULL);
    if(!blank_id)
      goto err_collection;

    blank=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_ANONYMOUS, NULL, RAPTOR_URI_SOURCE_GENERATED, blank_id, NULL, NULL, NULL);
    if(!blank)
      goto err_collection;
    
    t2->subject=blank;
    t2->predicate=first_identifier;
    /* t2->object already set to the value we want */
    raptor_turtle_generate_statement((raptor_parser*)rdf_parser, t2);
    
    temp=t2->object;
    
    t2->subject=blank;
    t2->predicate=rest_identifier;
    t2->object=object;
    raptor_turtle_generate_statement((raptor_parser*)rdf_parser, t2);

    t2->subject=NULL;
    t2->predicate=NULL;
    t2->object=temp;

    raptor_free_identifier(object);
    object=blank;
    blank=NULL;
  }
  
#if RAPTOR_DEBUG > 1
  printf(" after substitution objectList=");
  raptor_sequence_print($2, stdout);
  printf("\n\n");
#endif

  raptor_free_sequence($2);

  raptor_free_identifier(first_identifier);
  raptor_free_identifier(rest_identifier);

  $$=object;

  break; /* success */

  err_collection:

  if(blank)
    raptor_free_identifier(blank);

  if(object)
    raptor_free_identifier(object);

  if(rest_identifier)
    raptor_free_identifier(rest_identifier);

  if(first_identifier)
    raptor_free_identifier(first_identifier);

  raptor_free_sequence($2);

  YYERROR;
}
|  LEFT_ROUND RIGHT_ROUND 
{
  raptor_turtle_parser* turtle_parser=(raptor_turtle_parser*)(((raptor_parser*)rdf_parser)->context);

#if RAPTOR_DEBUG > 1  
  printf("collection\n empty\n");
#endif

  $$=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_RESOURCE, raptor_uri_copy(turtle_parser->nil_uri), RAPTOR_URI_SOURCE_URI, NULL, NULL, NULL, NULL);
  if(!$$)
    YYERROR;
}
;


%%


/* Support functions */

/* This is declared in turtle_lexer.h but never used, so we always get
 * a warning unless this dummy code is here.  Used once below as a return.
 */
static int yy_init_globals (yyscan_t yyscanner ) { return 0; };


/* helper - everything passed in is now owned by triple */
static raptor_triple*
raptor_turtle_new_triple(raptor_identifier *subject,
                         raptor_identifier *predicate,
                         raptor_identifier *object) 
{
  raptor_triple* t;
  
  t=(raptor_triple*)RAPTOR_MALLOC(raptor_triple, sizeof(raptor_triple));
  if(!t) {
    if(subject)
      raptor_free_identifier(subject);
    if(predicate)
      raptor_free_identifier(predicate);
    if(object)
      raptor_free_identifier(object);
    return NULL;
  }
  
  t->subject=subject;
  t->predicate=predicate;
  t->object=object;

  return t;
}

static void
raptor_turtle_free_triple(raptor_triple *t) {
  if(t->subject)
    raptor_free_identifier(t->subject);
  if(t->predicate)
    raptor_free_identifier(t->predicate);
  if(t->object)
    raptor_free_identifier(t->object);
  RAPTOR_FREE(raptor_triple, t);
}
 
#ifdef RAPTOR_DEBUG
static void
raptor_triple_print(raptor_triple *t, FILE *fh) 
{
  fputs("triple(", fh);
  raptor_identifier_print(fh, t->subject);
  fputs(", ", fh);
  raptor_identifier_print(fh, t->predicate);
  fputs(", ", fh);
  raptor_identifier_print(fh, t->object);
  fputc(')', fh);
}
#endif


int
turtle_parser_error(void* ctx, const char *msg)
{
  raptor_parser* rdf_parser=(raptor_parser *)ctx;
  raptor_turtle_parser* turtle_parser=(raptor_turtle_parser*)rdf_parser->context;
  
  rdf_parser->locator.line=turtle_parser->lineno;
#ifdef RAPTOR_TURTLE_USE_ERROR_COLUMNS
  rdf_parser->locator.column=turtle_lexer_get_column(yyscanner);
#endif

  raptor_parser_simple_error(rdf_parser, "%s", msg);
  return yy_init_globals(NULL); /* 0 but a way to use yy_init_globals */
}


int
turtle_syntax_error(raptor_parser *rdf_parser, const char *message, ...)
{
  raptor_turtle_parser* turtle_parser=(raptor_turtle_parser*)rdf_parser->context;
  va_list arguments;

  rdf_parser->locator.line=turtle_parser->lineno;
#ifdef RAPTOR_TURTLE_USE_ERROR_COLUMNS
  rdf_parser->locator.column=turtle_lexer_get_column(yyscanner);
#endif

  va_start(arguments, message);
  
  raptor_parser_error_varargs(((raptor_parser*)rdf_parser), message, arguments);

  va_end(arguments);

  return (0);
}


raptor_uri*
turtle_qname_to_uri(raptor_parser *rdf_parser, unsigned char *name, size_t name_len) 
{
  raptor_turtle_parser* turtle_parser=(raptor_turtle_parser*)rdf_parser->context;

  rdf_parser->locator.line=turtle_parser->lineno;
#ifdef RAPTOR_TURTLE_USE_ERROR_COLUMNS
  rdf_parser->locator.column=turtle_lexer_get_column(yyscanner);
#endif

  return raptor_qname_string_to_uri(&turtle_parser->namespaces,
                                    name, name_len,
                                    (raptor_simple_message_handler)raptor_parser_simple_error, rdf_parser);
}



static int
turtle_parse(raptor_parser *rdf_parser, const char *string) {
  raptor_turtle_parser* turtle_parser=(raptor_turtle_parser*)rdf_parser->context;
  void *buffer;
  
  if(!string || !*string)
    return 0;
  
  if(turtle_lexer_lex_init(&turtle_parser->scanner))
    return 1;
  turtle_parser->scanner_set=1;

  turtle_lexer_set_extra(rdf_parser, turtle_parser->scanner);
  buffer= turtle_lexer__scan_string(string, turtle_parser->scanner);

  turtle_parser_parse(rdf_parser);

  turtle_lexer_lex_destroy(turtle_parser->scanner);
  turtle_parser->scanner_set=0;

  return 0;
}


/**
 * raptor_turtle_parse_init - Initialise the Raptor Turtle parser
 *
 * Return value: non 0 on failure
 **/

static int
raptor_turtle_parse_init(raptor_parser* rdf_parser, const char *name) {
  raptor_turtle_parser* turtle_parser=(raptor_turtle_parser*)rdf_parser->context;
  const raptor_uri_handler *uri_handler;
  void *uri_context;

  raptor_uri_get_handler(&uri_handler, &uri_context);

  if(raptor_namespaces_init(&turtle_parser->namespaces,
                            uri_handler, uri_context,
                            (raptor_simple_message_handler)raptor_parser_simple_error, rdf_parser, 
                            0))
    return 1;

  turtle_parser->nil_uri=raptor_new_uri_for_rdf_concept("nil");
  turtle_parser->first_uri=raptor_new_uri_for_rdf_concept("first");
  turtle_parser->rest_uri=raptor_new_uri_for_rdf_concept("rest");

  if(!turtle_parser->nil_uri || !turtle_parser->first_uri || !turtle_parser->rest_uri)
    return 1;

  turtle_parser->trig=!strcmp(name, "trig");

  return 0;
}


/* PUBLIC FUNCTIONS */


/*
 * raptor_turtle_parse_terminate - Free the Raptor Turtle parser
 * @rdf_parser: parser object
 * 
 **/
static void
raptor_turtle_parse_terminate(raptor_parser *rdf_parser) {
  raptor_turtle_parser *turtle_parser=(raptor_turtle_parser*)rdf_parser->context;

  if(turtle_parser->nil_uri)
    raptor_free_uri(turtle_parser->nil_uri);
  if(turtle_parser->first_uri)
    raptor_free_uri(turtle_parser->first_uri);
  if(turtle_parser->rest_uri)
    raptor_free_uri(turtle_parser->rest_uri);

  raptor_namespaces_clear(&turtle_parser->namespaces);

  if(turtle_parser->scanner_set) {
    turtle_lexer_lex_destroy(turtle_parser->scanner);
    turtle_parser->scanner_set=0;
  }

  if(turtle_parser->buffer)
    RAPTOR_FREE(cdata, turtle_parser->buffer);
}


static void
raptor_turtle_generate_statement(raptor_parser *parser, raptor_triple *t)
{
  /* raptor_turtle_parser *turtle_parser=(raptor_turtle_parser*)parser->context; */
  raptor_statement *statement=&parser->statement;

  if(!t->subject || !t->predicate || !t->object)
    return;

  /* Two choices for subject for Turtle */
  statement->subject_type=t->subject->type;
  if(t->subject->type == RAPTOR_IDENTIFIER_TYPE_ANONYMOUS) {
    statement->subject=t->subject->id;
  } else {
    /* RAPTOR_IDENTIFIER_TYPE_RESOURCE */
    RAPTOR_ASSERT(t->subject->type != RAPTOR_IDENTIFIER_TYPE_RESOURCE,
                  "subject type is not resource");
    statement->subject=t->subject->uri;
  }

  /* Predicates are URIs but check for bad ordinals */
  if(!strncmp((const char*)raptor_uri_as_string(t->predicate->uri),
              "http://www.w3.org/1999/02/22-rdf-syntax-ns#_", 44)) {
    unsigned char* predicate_uri_string=raptor_uri_as_string(t->predicate->uri);
    int predicate_ordinal=raptor_check_ordinal(predicate_uri_string+44);
    if(predicate_ordinal <= 0)
      raptor_parser_error(parser, "Illegal ordinal value %d in property '%s'.", predicate_ordinal, predicate_uri_string);
  }
  
  statement->predicate_type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
  statement->predicate=t->predicate->uri;
  

  /* Three choices for object for Turtle */
  statement->object_type=t->object->type;
  statement->object_literal_language=NULL;
  statement->object_literal_datatype=NULL;

  if(t->object->type == RAPTOR_IDENTIFIER_TYPE_RESOURCE) {
    statement->object=t->object->uri;
  } else if(t->object->type == RAPTOR_IDENTIFIER_TYPE_ANONYMOUS) {
    statement->object=t->object->id;
  } else {
    /* RAPTOR_IDENTIFIER_TYPE_LITERAL */
    RAPTOR_ASSERT(t->object->type != RAPTOR_IDENTIFIER_TYPE_LITERAL,
                  "object type is not literal");
    statement->object=t->object->literal;
    statement->object_literal_language=t->object->literal_language;
    statement->object_literal_datatype=t->object->literal_datatype;

    if(statement->object_literal_datatype)
      statement->object_literal_language=NULL;
  }

  if(!parser->statement_handler)
    return;

  /* Generate the statement */
  (*parser->statement_handler)(parser->user_data, statement);
}



static int
raptor_turtle_parse_chunk(raptor_parser* rdf_parser, 
                      const unsigned char *s, size_t len,
                      int is_end)
{
  char *ptr;
  raptor_turtle_parser *turtle_parser=(raptor_turtle_parser*)rdf_parser->context;
  
#if defined(RAPTOR_DEBUG) && RAPTOR_DEBUG > 1
  RAPTOR_DEBUG2("adding %d bytes to line buffer\n", (int)len);
#endif

  if(len) {
    turtle_parser->buffer=(char*)RAPTOR_REALLOC(cstring, turtle_parser->buffer, turtle_parser->buffer_length + len + 1);
    if(!turtle_parser->buffer) {
      raptor_parser_fatal_error(rdf_parser, "Out of memory");
      return 1;
    }

    /* move pointer to end of cdata buffer */
    ptr=turtle_parser->buffer+turtle_parser->buffer_length;

    /* adjust stored length */
    turtle_parser->buffer_length += len;

    /* now write new stuff at end of cdata buffer */
    strncpy(ptr, (char*)s, len);
    ptr += len;
    *ptr = '\0';

#if defined(RAPTOR_DEBUG) && RAPTOR_DEBUG > 1
    RAPTOR_DEBUG3("buffer buffer now '%s' (%d bytes)\n", 
                  turtle_parser->buffer, turtle_parser->buffer_length);
#endif
  }
  
  /* if not end, wait for rest of input */
  if(!is_end)
    return 0;

  /* Nothing to do */
  if(!turtle_parser->buffer_length)
    return 0;
  
  turtle_parse(rdf_parser, turtle_parser->buffer);
  
  return 0;
}


static int
raptor_turtle_parse_start(raptor_parser *rdf_parser) 
{
  raptor_locator *locator=&rdf_parser->locator;
  raptor_turtle_parser *turtle_parser=(raptor_turtle_parser*)rdf_parser->context;

  /* base URI required for Turtle */
  if(!rdf_parser->base_uri)
    return 1;

  locator->line=1;
  locator->column= -1; /* No column info */
  locator->byte= -1; /* No bytes info */

  if(turtle_parser->buffer_length) {
    RAPTOR_FREE(cdata, turtle_parser->buffer);
    turtle_parser->buffer=NULL;
    turtle_parser->buffer_length=0;
  }
  
  turtle_parser->lineno=1;

  return 0;
}


static int
raptor_turtle_parse_recognise_syntax(raptor_parser_factory* factory, 
                                     const unsigned char *buffer, size_t len,
                                     const unsigned char *identifier, 
                                     const unsigned char *suffix, 
                                     const char *mime_type)
{
  int score= 0;
  
  if(suffix) {
    if(!strcmp((const char*)suffix, "ttl"))
      score=8;
    if(!strcmp((const char*)suffix, "n3"))
      score=3;
  }
  
  if(mime_type) {
    if(strstr((const char*)mime_type, "turtle"))
      score+=6;
#ifndef RAPTOR_PARSER_N3
    if(strstr((const char*)mime_type, "n3"))
      score+=3;
#endif
  }

  /* FIXME: Should do this as long as N3 is not also present since
   * shares the same syntax */
  if(buffer && len) {
#define  HAS_TURTLE_PREFIX (strstr((const char*)buffer, "@prefix ") != NULL)
/* The following could also be found with N-Triples but not with @prefix */
#define  HAS_TURTLE_RDF_URI (strstr((const char*)buffer, ": <http://www.w3.org/1999/02/22-rdf-syntax-ns#>") != NULL)

    if(HAS_TURTLE_PREFIX) {
      score=6;
      if(HAS_TURTLE_RDF_URI)
        score+=2;
    }
  }
  
  return score;
}


#ifdef RAPTOR_PARSER_TRIG
static int
raptor_trig_parse_recognise_syntax(raptor_parser_factory* factory, 
                                   const unsigned char *buffer, size_t len,
                                   const unsigned char *identifier, 
                                   const unsigned char *suffix, 
                                   const char *mime_type)
{
  int score= 0;
  
  if(suffix) {
    if(!strcmp((const char*)suffix, "trig"))
      score=9;
#ifndef RAPTOR_PARSER_TURTLE
    if(!strcmp((const char*)suffix, "ttl"))
      score=8;
#endif
    if(!strcmp((const char*)suffix, "n3"))
      score=3;
  }
  
  if(mime_type) {
#ifndef RAPTOR_PARSER_TURTLE
    if(strstr((const char*)mime_type, "turtle"))
      score+=6;
#endif
#ifndef RAPTOR_PARSER_N3
    if(strstr((const char*)mime_type, "n3"))
      score+=3;
#endif
  }

#ifndef RAPTOR_PARSER_TURTLE
  /* FIXME: Should do this as long as N3 is not also present since
   * shares the same syntax */
  if(buffer && len) {
#define  HAS_TRIG_PREFIX (strstr((const char*)buffer, "@prefix ") != NULL)
/* The following could also be found with N-Triples but not with @prefix */
#define  HAS_TRIG_RDF_URI (strstr((const char*)buffer, ": <http://www.w3.org/1999/02/22-rdf-syntax-ns#>") != NULL)

    if(HAS_TRIG_PREFIX) {
      score=6;
      if(HAS_TRIG_RDF_URI)
        score+=2;
    }
  }
#endif
  
  return score;
}
#endif


#ifdef RAPTOR_PARSER_TURTLE
static int
raptor_turtle_parser_register_factory(raptor_parser_factory *factory) 
{
  int rc=0;

  factory->context_length     = sizeof(raptor_turtle_parser);
  
  factory->need_base_uri = 1;
  
  factory->init      = raptor_turtle_parse_init;
  factory->terminate = raptor_turtle_parse_terminate;
  factory->start     = raptor_turtle_parse_start;
  factory->chunk     = raptor_turtle_parse_chunk;
  factory->recognise_syntax = raptor_turtle_parse_recognise_syntax;

  rc+= raptor_parser_factory_add_alias(factory, "ntriples-plus") != 0;

  rc+= raptor_parser_factory_add_uri(factory, (const unsigned char*)"http://www.dajobe.org/2004/01/turtle/") != 0;

  /* first one is the default */
  rc+= raptor_parser_factory_add_mime_type(factory, "application/x-turtle", 10) != 0;
  rc+= raptor_parser_factory_add_mime_type(factory, "application/turtle", 10) != 0;

#ifndef RAPTOR_PARSER_N3
  rc+= raptor_parser_factory_add_mime_type(factory, "text/n3", 3) != 0;
  rc+= raptor_parser_factory_add_mime_type(factory, "text/rdf+n3", 3) != 0;
  rc+= raptor_parser_factory_add_mime_type(factory, "application/rdf+n3", 3) != 0;
#endif

  return rc;
}
#endif


#ifdef RAPTOR_PARSER_TRIG
static int
raptor_trig_parser_register_factory(raptor_parser_factory *factory) 
{
  int rc=0;

  factory->context_length     = sizeof(raptor_turtle_parser);
  
  factory->need_base_uri = 1;
  
  factory->init      = raptor_turtle_parse_init;
  factory->terminate = raptor_turtle_parse_terminate;
  factory->start     = raptor_turtle_parse_start;
  factory->chunk     = raptor_turtle_parse_chunk;
  factory->recognise_syntax = raptor_trig_parse_recognise_syntax;

  rc+= raptor_parser_factory_add_uri(factory, (const unsigned char*)"http://www.wiwiss.fu-berlin.de/suhl/bizer/TriG/Spec/") != 0;

  rc+= raptor_parser_factory_add_mime_type(factory, "application/x-trig", 10) != 0;

  return rc;
}
#endif


#ifdef RAPTOR_PARSER_TURTLE
int
raptor_init_parser_turtle(void)
{
  return !raptor_parser_register_factory("turtle", "Turtle Terse RDF Triple Language",
                                         &raptor_turtle_parser_register_factory);
}
#endif

#ifdef RAPTOR_PARSER_TRIG
int
raptor_init_parser_trig(void)
{
  return !raptor_parser_register_factory("trig", "TriG - Turtle with Named Graphs",
                                         &raptor_trig_parser_register_factory);
}
#endif


#ifdef STANDALONE
#include <stdio.h>
#include <locale.h>

#define TURTLE_FILE_BUF_SIZE 2048

static
void turtle_parser_print_statement(void *user, const raptor_statement *statement) 
{
  FILE* stream=(FILE*)user;
  raptor_print_statement(statement, stream);
  putc('\n', stream);
}
  


int
main(int argc, char *argv[]) 
{
  char string[TURTLE_FILE_BUF_SIZE];
  raptor_parser rdf_parser; /* static */
  raptor_turtle_parser turtle_parser; /* static */
  raptor_locator *locator=&rdf_parser.locator;
  FILE *fh;
  char *filename;
  int rc;
  
#if RAPTOR_DEBUG > 2
  turtle_parser_debug=1;
#endif

  if(argc > 1) {
    filename=argv[1];
    fh = fopen(filename, "r");
    if(!fh) {
      fprintf(stderr, "%s: Cannot open file %s - %s\n", argv[0], filename,
              strerror(errno));
      exit(1);
    }
  } else {
    filename="<stdin>";
    fh = stdin;
  }

  memset(string, 0, TURTLE_FILE_BUF_SIZE);
  rc=fread(string, TURTLE_FILE_BUF_SIZE, 1, fh);
  if(rc < TURTLE_FILE_BUF_SIZE) {
    if(ferror(fh)) {
      fprintf(stderr, "%s: file '%s' read failed - %s\n",
              argv[0], filename, strerror(errno));
      fclose(fh);
      return(1);
    }
  }
  
  if(argc>1)
    fclose(fh);

  raptor_uri_init();

  memset(&rdf_parser, 0, sizeof(raptor_parser));
  memset(&turtle_parser, 0, sizeof(raptor_turtle_parser));

  locator->line= locator->column = -1;
  locator->file= filename;

  turtle_parser.lineno= 1;

  rdf_parser.context=&turtle_parser;
  rdf_parser.base_uri=raptor_new_uri((const unsigned char*)"http://example.org/fake-base-uri/");

  raptor_set_statement_handler(&rdf_parser, stdout, turtle_parser_print_statement);
  raptor_turtle_parse_init(&rdf_parser, "turtle");
  
  turtle_parse(&rdf_parser, string);

  raptor_free_uri(rdf_parser.base_uri);

  return (0);
}
#endif
