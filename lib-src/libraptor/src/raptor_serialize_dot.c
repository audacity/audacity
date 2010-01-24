/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_serialize_dot.c - Serialize RDF graph to GraphViz DOT format
 *
 * Copyright (C) 2004-2006, David Beckett http://purl.org/net/dajobe/
 * Copyright (C) 2004-2005, University of Bristol, UK http://www.bristol.ac.uk/
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

/* Raptor includes */
#include "raptor.h"
#include "raptor_internal.h"

/*
 * Raptor dot serializer object
 */
typedef struct {
  raptor_namespace_stack *nstack;
  raptor_sequence *namespaces;

  raptor_sequence *resources;
  raptor_sequence *literals;
  raptor_sequence *bnodes;
} raptor_dot_context;

typedef struct {
  raptor_identifier_type type;  /* node type */
  union {

    struct {
      raptor_uri *uri;
    } resource;

    struct {
      unsigned char *string;
      raptor_uri *datatype;
      unsigned char *language;
    } literal;

    struct {
      unsigned char *string;
    } blank;
    
  } value;
} raptor_dot_serializer_node;


/* Free a node */
static void
raptor_dot_serializer_free_node(raptor_dot_serializer_node *node)
{
  if(!node)
    return;
  
  switch (node->type) {
      case RAPTOR_IDENTIFIER_TYPE_RESOURCE:
      case RAPTOR_IDENTIFIER_TYPE_PREDICATE:
        raptor_free_uri(node->value.resource.uri);
        break;
          
      case RAPTOR_IDENTIFIER_TYPE_ANONYMOUS:
        RAPTOR_FREE(blank, node->value.blank.string);
        break;
          
      case RAPTOR_IDENTIFIER_TYPE_LITERAL:
      case RAPTOR_IDENTIFIER_TYPE_XML_LITERAL:
        RAPTOR_FREE(literal, node->value.literal.string);

        if(node->value.literal.datatype)
          raptor_free_uri(node->value.literal.datatype);

        if(node->value.literal.language)
          RAPTOR_FREE(language, node->value.literal.language);

        break;
          
      case RAPTOR_IDENTIFIER_TYPE_ORDINAL:
      case RAPTOR_IDENTIFIER_TYPE_UNKNOWN: 
      default:
        /* Nothing to do */
        break;
  }

  RAPTOR_FREE(raptor_node, node);
}


/*
 * raptor_dot_serializer_node_matches:
 * @node: #raptor_node to compare
 * @node_type: Raptor identifier type
 * @node_data: For node_type RAPTOR_IDENTIFIER_TYPE_ORDINAL, int* to the
 *             ordinal.
 * @datatype: Literal datatype or NULL
 * @language: Literal language or NULL
 *
 * Return value: non-zero if @node matches the node described by the rest of
 *   the parameters.
 */
static int
raptor_dot_serializer_node_matches(raptor_dot_serializer_node* node,
                                   raptor_identifier_type node_type,
				   const void* node_data, raptor_uri* datatype,
				   const unsigned char* language)
{
  int rv = 0;
  
  if(node->type != node_type)
    return 0;

  switch (node->type) {
      case RAPTOR_IDENTIFIER_TYPE_RESOURCE:
      case RAPTOR_IDENTIFIER_TYPE_PREDICATE:
        rv = raptor_uri_equals(node->value.resource.uri,
                               (raptor_uri*)node_data);
        break;
          
      case RAPTOR_IDENTIFIER_TYPE_ANONYMOUS:
        rv = !strcmp((const char*)node->value.blank.string,
                     (const char*)node_data);
        break;
          
      case RAPTOR_IDENTIFIER_TYPE_LITERAL:
      case RAPTOR_IDENTIFIER_TYPE_XML_LITERAL:

        if((char*)node->value.literal.string != NULL &&
           (char*)node_data != NULL) {

          /* string */
          rv = (strcmp((char*)node->value.literal.string,
                       (char*)node_data) == 0);

          /* language */
          if((char*)node->value.literal.language != NULL &&
             (char*)language != NULL)
            rv &= (strcmp((char*)node->value.literal.language,
                          (char*)language) == 0);
          else if((char*)node->value.literal.language != NULL ||
                  (char*)language != NULL)
            rv= 0;

          /* datatype */
          if(node->value.literal.datatype != NULL && datatype != NULL)
            rv &= (raptor_uri_equals(node->value.literal.datatype,datatype) !=0);
          else if(node->value.literal.datatype != NULL || datatype != NULL)
            rv = 0;
          
        } else {
          RAPTOR_FATAL1("string must be non-NULL for literal or xml literal\n");
          rv = 0;
        }
        
        break;
          
      case RAPTOR_IDENTIFIER_TYPE_ORDINAL:
      case RAPTOR_IDENTIFIER_TYPE_UNKNOWN: 
      default:
        /* Nothing to do */
        break;
  }

  return rv;
}


/*
 * raptor_dot_serializer_new_node implementation:
 * @node_type: Raptor identifier type
 * @node_data: For node_type RAPTOR_IDENTIFIER_TYPE_ORDINAL, int* to the
 *             ordinal.
 * @datatype: Literal datatype or NULL
 * @language: Literal language or NULL
 *
 * Parts of this is taken from redland librdf_node.h and librdf_node.c
 *
 * Return value: a new raptor_dot_serializer_node
 *
 **/
static raptor_dot_serializer_node *
raptor_dot_serializer_new_node(raptor_identifier_type node_type,
                               const void* node_data,
			       raptor_uri* datatype,
                               const unsigned char *language)
{
  unsigned char *string;
  raptor_dot_serializer_node* node;
  
  if(node_type == RAPTOR_IDENTIFIER_TYPE_UNKNOWN)
    return 0;

  node = (raptor_dot_serializer_node *)RAPTOR_CALLOC(raptor_dot_serializer_node, 1, sizeof(raptor_dot_serializer_node));

  if(node) {
    node->type = node_type;
    
    switch (node_type) {
      case RAPTOR_IDENTIFIER_TYPE_PREDICATE:
        node->type = RAPTOR_IDENTIFIER_TYPE_RESOURCE;
        /* intentional fall through */

      case RAPTOR_IDENTIFIER_TYPE_RESOURCE:
        node->value.resource.uri = raptor_uri_copy((raptor_uri*)node_data);
        break;
        
      case RAPTOR_IDENTIFIER_TYPE_ANONYMOUS:
        string=(unsigned char*)RAPTOR_MALLOC(blank,
                                             strlen((char*)node_data)+1);
        strcpy((char*)string, (const char*) node_data);
        node->value.blank.string = string;
        break;
        
      case RAPTOR_IDENTIFIER_TYPE_LITERAL:
      case RAPTOR_IDENTIFIER_TYPE_XML_LITERAL:
        string = (unsigned char*)RAPTOR_MALLOC(literal,
                                               strlen((char*)node_data)+1);
        strcpy((char*)string, (const char*)node_data);
        node->value.literal.string = string;
        
        if(datatype)
          node->value.literal.datatype = raptor_uri_copy(datatype);
        
        if(language) {
          unsigned char *lang;
          lang = (unsigned char*)RAPTOR_MALLOC(language,
                                               strlen((const char*)language)+1);
          strcpy((char*)lang, (const char*)language);
          node->value.literal.language = lang;
        }
        break;
        
      case RAPTOR_IDENTIFIER_TYPE_ORDINAL:
      case RAPTOR_IDENTIFIER_TYPE_UNKNOWN: 
      default:
        RAPTOR_FREE(raptor_dot_serializer_node, node);
    }
    
  }

  return node;
}


/* add a namespace */
static int
raptor_dot_serializer_declare_namespace_from_namespace(raptor_serializer* serializer,
						       raptor_namespace *nspace)
{
  raptor_dot_context * context = (raptor_dot_context *)serializer->context;
  int i;

  for( i = 0 ; i < raptor_sequence_size(context->namespaces) ; i++ ) {
    raptor_namespace * ns;
    ns = (raptor_namespace *)raptor_sequence_get_at(context->namespaces, i);

    /* If prefix is already declared, ignore it */
    if((!ns->prefix && !nspace->prefix) ||
       (ns->prefix && nspace->prefix &&
        !strcmp((const char*)ns->prefix, (const char*)nspace->prefix)) ||
       (ns->uri && nspace->uri &&
        raptor_uri_equals(ns->uri, nspace->uri)) )
      return 1;
  }

  nspace = raptor_new_namespace_from_uri(context->nstack, nspace->prefix,
					 nspace->uri, 0);

  if(!nspace)
    return 1;
  
  raptor_sequence_push(context->namespaces, nspace);

  return 0;
}


/* add a namespace */
static int
raptor_dot_serializer_declare_namespace(raptor_serializer* serializer,
					raptor_uri* uri,
					const unsigned char *prefix)
{
  raptor_dot_context * context = (raptor_dot_context *)serializer->context;
  raptor_namespace *ns;
  int rc;

  ns = raptor_new_namespace_from_uri(context->nstack, prefix, uri, 0);
  rc = raptor_dot_serializer_declare_namespace_from_namespace(serializer, ns);

  raptor_free_namespace(ns);

  return rc;
}


/* create a new serializer */
static int
raptor_dot_serializer_init(raptor_serializer *serializer, const char *name)
{
  raptor_dot_context * context = (raptor_dot_context *)serializer->context;
  const raptor_uri_handler *uri_handler;
  void* uri_context;

  raptor_uri_get_handler(&uri_handler, &uri_context);

  /* Setup namespace handling */
  context->nstack =
    raptor_new_namespaces(uri_handler, uri_context,
			  (raptor_simple_message_handler)raptor_serializer_simple_error,
			  serializer, 1);
  context->namespaces=raptor_new_sequence((raptor_sequence_free_handler *)raptor_free_namespace, NULL);

  /* We keep a list of nodes to avoid duplication (which isn't
   * critical in graphviz, but why bloat the file?)
   */
  context->resources =
    raptor_new_sequence((raptor_sequence_free_handler *)raptor_dot_serializer_free_node, NULL);
  context->literals =
    raptor_new_sequence((raptor_sequence_free_handler *)raptor_dot_serializer_free_node, NULL);
  context->bnodes =
    raptor_new_sequence((raptor_sequence_free_handler *)raptor_dot_serializer_free_node, NULL);

  return 0;
}


/**
 * raptor_dot_iostream_write_string:
 * @iostr: #raptor_iostream to write to
 * @string: UTF-8 string to write
 * @len: length of UTF-8 string
 * or \0 for no escaping.
 *
 * Write an UTF-8 string, escaped for graphviz.
 *
 * Return value: non-0 on failure.
 **/
static int
raptor_dot_iostream_write_string(raptor_iostream *iostr,
				 const unsigned char *string)
{
  unsigned char c;

  for( ; (c = *string) ; string++ ) {
    if( (c == '\\') || (c == '"') || (c == '|') ||
        (c == '{')  || (c == '}') ) {
      raptor_iostream_write_byte(iostr, '\\');
      raptor_iostream_write_byte(iostr, c);
    } else if( c == '\n' ) {
      raptor_iostream_write_byte(iostr, '\\');
      raptor_iostream_write_byte(iostr, 'n');
    } else
      raptor_iostream_write_byte(iostr, c);
  }

  return 0;
}


static void
raptor_dot_serializer_write_node_type(raptor_serializer * serializer,
				      raptor_identifier_type type)
{
  switch(type) {
    case RAPTOR_IDENTIFIER_TYPE_LITERAL:
    case RAPTOR_IDENTIFIER_TYPE_XML_LITERAL:
      raptor_iostream_write_byte(serializer->iostream, 'L');
      break;

    case RAPTOR_IDENTIFIER_TYPE_ANONYMOUS:
      raptor_iostream_write_byte(serializer->iostream, 'B');
      break;

    case RAPTOR_IDENTIFIER_TYPE_RESOURCE:
    case RAPTOR_IDENTIFIER_TYPE_PREDICATE:
      raptor_iostream_write_byte(serializer->iostream, 'R');
      break;

    case RAPTOR_IDENTIFIER_TYPE_ORDINAL:
    case RAPTOR_IDENTIFIER_TYPE_UNKNOWN:
      raptor_iostream_write_byte(serializer->iostream, '?');
      break;
  }
}


static void
raptor_dot_serializer_write_uri(raptor_serializer* serializer,
				raptor_uri* uri)
{
  raptor_dot_context* context = (raptor_dot_context*)serializer->context;
  unsigned char* full = raptor_uri_as_string(uri);
  int i;

  for( i = 0 ; i < raptor_sequence_size(context->namespaces) ; i++ ) {
    raptor_namespace* ns =
      (raptor_namespace*)raptor_sequence_get_at(context->namespaces, i);
    const unsigned char* ns_uri_string;
    size_t ns_uri_string_len;
    ns_uri_string=raptor_uri_as_counted_string(ns->uri, &ns_uri_string_len);

    if(!strncmp((char*)full, (char*)ns_uri_string, ns_uri_string_len) ) {
      const unsigned char* prefix = raptor_namespace_get_prefix(ns);
      
      if(prefix) {	
        raptor_iostream_write_string(serializer->iostream, prefix);
        raptor_iostream_write_byte(serializer->iostream, ':');
      }

      raptor_iostream_write_string(serializer->iostream,
                                   full + ns_uri_string_len);

      return;
    }
  }

  raptor_iostream_write_string(serializer->iostream, full);
}


static void
raptor_dot_serializer_write_node(raptor_serializer * serializer,
				 const void* term,
				 raptor_identifier_type type,
				 raptor_uri* literal_datatype,
				 const unsigned char * literal_language) {
  switch(type) {
    case RAPTOR_IDENTIFIER_TYPE_LITERAL:
    case RAPTOR_IDENTIFIER_TYPE_XML_LITERAL:
      raptor_dot_iostream_write_string(serializer->iostream, (const unsigned char*)term);
      if(literal_language && type == RAPTOR_IDENTIFIER_TYPE_LITERAL) {
        raptor_iostream_write_byte(serializer->iostream, '|');
        raptor_iostream_write_string(serializer->iostream, "Language: ");
        raptor_iostream_write_string(serializer->iostream, literal_language);
      }
      if(type == RAPTOR_IDENTIFIER_TYPE_XML_LITERAL) {
        raptor_iostream_write_byte(serializer->iostream, '|');
        raptor_iostream_write_string(serializer->iostream, "Datatype: ");
        raptor_iostream_write_string(serializer->iostream, raptor_xml_literal_datatype_uri_string);
      } else if(literal_datatype) {
        raptor_iostream_write_byte(serializer->iostream, '|');
        raptor_iostream_write_string(serializer->iostream, "Datatype: ");
	raptor_dot_serializer_write_uri(serializer, (raptor_uri*)literal_datatype);
      }

      break;
      
    case RAPTOR_IDENTIFIER_TYPE_ANONYMOUS:
      raptor_iostream_write_counted_string(serializer->iostream, "_:", 2);
      raptor_iostream_write_string(serializer->iostream, term);
      break;
  
    case RAPTOR_IDENTIFIER_TYPE_RESOURCE:
    case RAPTOR_IDENTIFIER_TYPE_PREDICATE:
      raptor_dot_serializer_write_uri(serializer, (raptor_uri*)term);
      break;
      
    case RAPTOR_IDENTIFIER_TYPE_ORDINAL:
    case RAPTOR_IDENTIFIER_TYPE_UNKNOWN:
    default:
      RAPTOR_FATAL2("Unknown type %d", type);
  }
}


/* Check the list to see if the node is a duplicate. If not, add it
 * to the list.
 */
static void
raptor_dot_serializer_assert_node(raptor_serializer* serializer,
                                  raptor_identifier_type node_type,
                                  const void* node_data,
                                  raptor_uri* datatype,
                                  const unsigned char* language)
{
  raptor_dot_context* context = (raptor_dot_context*)serializer->context;
  raptor_sequence* seq = NULL;
  int i;

  /* Which list are we searching? */
  switch(node_type) {
    case RAPTOR_IDENTIFIER_TYPE_RESOURCE:
    case RAPTOR_IDENTIFIER_TYPE_PREDICATE:
      seq = context->resources;
      break;

    case RAPTOR_IDENTIFIER_TYPE_ANONYMOUS:
      seq = context->bnodes;
      break;

    case RAPTOR_IDENTIFIER_TYPE_LITERAL:
    case RAPTOR_IDENTIFIER_TYPE_XML_LITERAL:
      seq = context->literals;
      break;

    case RAPTOR_IDENTIFIER_TYPE_UNKNOWN:
    case RAPTOR_IDENTIFIER_TYPE_ORDINAL:
      break;
  }

  for( i = 0 ; i < raptor_sequence_size(seq) ; i++ ) {
    raptor_dot_serializer_node* node =
      (raptor_dot_serializer_node*)raptor_sequence_get_at(seq, i);

    if(raptor_dot_serializer_node_matches(node, node_type, node_data,
                                          datatype, language) )
      return;
  }

  raptor_sequence_push(seq,
		       raptor_dot_serializer_new_node(node_type, node_data,
						      datatype, language));
}


/* start a serialize */
static int
raptor_dot_serializer_start(raptor_serializer* serializer)
{
  raptor_iostream_write_string(serializer->iostream, (const unsigned char *)
			       "digraph {\n\trankdir=LR;\n\tcharset=\"utf-8\";\n\n");

  return 0;
}


static int
raptor_dot_serializer_write_colors(raptor_serializer* serializer,
				   raptor_identifier_type type)
{
  switch(type) {
    case RAPTOR_IDENTIFIER_TYPE_RESOURCE:
      if(serializer->feature_resource_border) {
	raptor_iostream_write_string(serializer->iostream,
                                     (const unsigned char *)", color=");
	raptor_iostream_write_string(serializer->iostream,
                                     (const unsigned char *)serializer->feature_resource_border);
      }
      else
	raptor_iostream_write_string(serializer->iostream,
                                     (const unsigned char *)", color=blue");

      if(serializer->feature_resource_fill) {
	raptor_iostream_write_string(serializer->iostream,
                                     (const unsigned char *)", style=filled, fillcolor=");
	raptor_iostream_write_string(serializer->iostream,
                                     (const unsigned char *)
				     serializer->feature_resource_fill);
      }

      break;

    case RAPTOR_IDENTIFIER_TYPE_ANONYMOUS:
      if(serializer->feature_bnode_border) {
	raptor_iostream_write_string(serializer->iostream,
                                     (const unsigned char *)", color=");
	raptor_iostream_write_string(serializer->iostream,
                                     (const unsigned char *)serializer->feature_bnode_border);
      }
      else
	raptor_iostream_write_string(serializer->iostream,
                                     (const unsigned char *)", color=green");

      if(serializer->feature_bnode_fill) {
	raptor_iostream_write_string(serializer->iostream,
                                     (const unsigned char *)", style=filled, fillcolor=");
	raptor_iostream_write_string(serializer->iostream,
                                     (const unsigned char *)serializer->feature_bnode_fill);
      }

      break;

    case RAPTOR_IDENTIFIER_TYPE_LITERAL:
      if(serializer->feature_literal_border) {
	raptor_iostream_write_string(serializer->iostream,
                                     (const unsigned char *)", color=");
	raptor_iostream_write_string(serializer->iostream,
                                     (const unsigned char *)serializer->feature_literal_border);
      }

      if(serializer->feature_literal_fill) {
	raptor_iostream_write_string(serializer->iostream,
                                     (const unsigned char *)", style=filled, fillcolor=");
	raptor_iostream_write_string(serializer->iostream,
                                     (const unsigned char *)serializer->feature_literal_fill);
      }

      break;

    case RAPTOR_IDENTIFIER_TYPE_PREDICATE:
    case RAPTOR_IDENTIFIER_TYPE_ORDINAL:
    case RAPTOR_IDENTIFIER_TYPE_XML_LITERAL:
    case RAPTOR_IDENTIFIER_TYPE_UNKNOWN:
    default:
      break;
  }

  return 0;
}


/* end a serialize */
static int
raptor_dot_serializer_end(raptor_serializer* serializer)
{
  raptor_dot_context* context=(raptor_dot_context*)serializer->context;
  raptor_dot_serializer_node* node;
  int i;

  /* Print our nodes. */
  raptor_iostream_write_string(serializer->iostream,
                               (const unsigned char *)"\n\t// Resources\n");
  for( i = 0 ; i < raptor_sequence_size(context->resources) ; i++ ) {
    node = (raptor_dot_serializer_node*)raptor_sequence_get_at(context->resources, i);
    raptor_iostream_write_string(serializer->iostream,
                                 (const unsigned char *)"\t\"R");
    raptor_dot_serializer_write_node(serializer, node->value.resource.uri,
				     RAPTOR_IDENTIFIER_TYPE_RESOURCE, NULL, NULL);
    raptor_iostream_write_string(serializer->iostream,
                                 (const unsigned char *)"\" [ label=\"");
    raptor_dot_serializer_write_node(serializer, node->value.resource.uri,
				     RAPTOR_IDENTIFIER_TYPE_RESOURCE, NULL, NULL);
    raptor_iostream_write_string(serializer->iostream,
                                 (const unsigned char *)"\", shape=ellipse");
    raptor_dot_serializer_write_colors(serializer,
                                       RAPTOR_IDENTIFIER_TYPE_RESOURCE);
    raptor_iostream_write_string(serializer->iostream,
                                 (const unsigned char *)" ];\n");
    
  }
  raptor_free_sequence(context->resources);

  raptor_iostream_write_string(serializer->iostream,
                               (const unsigned char *)"\n\t// Anonymous nodes\n");
  for( i = 0 ; i < raptor_sequence_size(context->bnodes) ; i++ ) {
    node = (raptor_dot_serializer_node *)raptor_sequence_get_at(context->bnodes, i);
    raptor_iostream_write_string(serializer->iostream,
                                 (const unsigned char *)"\t\"B");
    raptor_dot_serializer_write_node(serializer, node->value.resource.uri,
				   RAPTOR_IDENTIFIER_TYPE_ANONYMOUS, NULL, NULL);
    raptor_iostream_write_string(serializer->iostream,
                                 (const unsigned char *)"\" [ label=\"");
    raptor_iostream_write_string(serializer->iostream,
                                 (const unsigned char *)"\", shape=circle");
    raptor_dot_serializer_write_colors(serializer,
                                       RAPTOR_IDENTIFIER_TYPE_ANONYMOUS);
    raptor_iostream_write_string(serializer->iostream,
                                 (const unsigned char *)" ];\n");
  }
  raptor_free_sequence(context->bnodes);

  raptor_iostream_write_string(serializer->iostream,
                               (const unsigned char *)"\n\t// Literals\n");
  for( i = 0 ; i < raptor_sequence_size(context->literals) ; i++ ) {
    node = (raptor_dot_serializer_node *)raptor_sequence_get_at(context->literals, i);
    raptor_iostream_write_string(serializer->iostream,
                                 (const unsigned char *)"\t\"L");
    raptor_dot_serializer_write_node(serializer, node->value.literal.string,
				     RAPTOR_IDENTIFIER_TYPE_LITERAL,
                                     node->value.literal.datatype,
				     node->value.literal.language);
    raptor_iostream_write_string(serializer->iostream,
                                 (const unsigned char *)"\" [ label=\"");
    raptor_dot_serializer_write_node(serializer, node->value.literal.string,
				     RAPTOR_IDENTIFIER_TYPE_LITERAL,
                                     node->value.literal.datatype,
				     node->value.literal.language);
    raptor_iostream_write_string(serializer->iostream,
                                 (const unsigned char *)"\", shape=record");
    raptor_dot_serializer_write_colors(serializer,
                                       RAPTOR_IDENTIFIER_TYPE_LITERAL);
    raptor_iostream_write_string(serializer->iostream,
                                 (const unsigned char *)" ];\n");
  }
  raptor_free_sequence(context->literals);

  raptor_iostream_write_string(serializer->iostream,
                               (const unsigned char *)"\n\tlabel=\"\\n\\nModel:\\n");
  if(serializer->base_uri)
    raptor_iostream_write_string(serializer->iostream,
                                 raptor_uri_as_string(serializer->base_uri));
  else
    raptor_iostream_write_string(serializer->iostream, "(Unknown)");

  if(raptor_sequence_size(context->namespaces)) {
    raptor_iostream_write_string(serializer->iostream,
                                 (const unsigned char *)"\\n\\nNamespaces:\\n");

    for( i = 0 ; i < raptor_sequence_size(context->namespaces) ; i++ ) {
      raptor_namespace* ns =
	(raptor_namespace*)raptor_sequence_get_at(context->namespaces, i);
      const unsigned char* prefix = raptor_namespace_get_prefix(ns);

      if(prefix) {
	raptor_iostream_write_string(serializer->iostream,
				     (const unsigned char *)ns->prefix);
	raptor_iostream_write_string(serializer->iostream,
				     (const unsigned char *)": ");
      }
      raptor_iostream_write_string(serializer->iostream,
				   raptor_uri_as_string(ns->uri));
      raptor_iostream_write_string(serializer->iostream,
				   (const unsigned char *)"\\n");
    }

    raptor_free_sequence(context->namespaces);
  }

  raptor_iostream_write_string(serializer->iostream,
                               (const unsigned char *)"\";\n");

  raptor_iostream_write_string(serializer->iostream,
                               (const unsigned char *) "}\n");

  return 0;
}


/* destroy a serializer */
static void
raptor_dot_serializer_terminate(raptor_serializer* serializer)
{
  /* raptor_dot_context* context=(raptor_dot_context*)serializer->context; */

  /* Everything should have been freed in raptor_dot_serializer_end */
}

/* serialize a statement */
static int
raptor_dot_serializer_statement(raptor_serializer* serializer,
				const raptor_statement *statement)
{
  /* Cache the nodes for later. */
  raptor_dot_serializer_assert_node(serializer, statement->subject_type,
				    statement->subject, NULL, NULL);
  raptor_dot_serializer_assert_node(serializer, statement->object_type,
				    statement->object, statement->object_literal_datatype,
				    statement->object_literal_language);

  raptor_iostream_write_string(serializer->iostream,
                               (const unsigned char *)"\t\"");
  raptor_dot_serializer_write_node_type(serializer, statement->subject_type);
  raptor_dot_serializer_write_node(serializer,
				   statement->subject,
				   statement->subject_type,
				   NULL, NULL);
  raptor_iostream_write_string(serializer->iostream,
                               (const unsigned char *)"\" -> \"");
  raptor_dot_serializer_write_node_type(serializer, statement->object_type);
  raptor_dot_serializer_write_node(serializer,
				   statement->object,
				   statement->object_type,
				   statement->object_literal_datatype,
				   statement->object_literal_language);
  raptor_iostream_write_string(serializer->iostream,
                               (const unsigned char *)"\" [ label=\"");
  raptor_dot_serializer_write_node(serializer,
				   statement->predicate,
				   statement->predicate_type,
				   NULL, NULL);
  raptor_iostream_write_string(serializer->iostream,
                               (const unsigned char *)"\" ];\n");

  return 0;
}


static int
raptor_dot_serializer_register_factory(raptor_serializer_factory *factory)
{
  factory->context_length = sizeof(raptor_dot_context);

  factory->init = raptor_dot_serializer_init;
  factory->declare_namespace = raptor_dot_serializer_declare_namespace;
  factory->declare_namespace_from_namespace =
    raptor_dot_serializer_declare_namespace_from_namespace;
  factory->serialize_start     = raptor_dot_serializer_start;
  factory->serialize_statement = raptor_dot_serializer_statement;
  factory->serialize_end       = raptor_dot_serializer_end;
  factory->finish_factory      = NULL;
  factory->terminate           = raptor_dot_serializer_terminate;

  return 0;
}


int
raptor_init_serializer_dot (void)
{
  return raptor_serializer_register_factory("dot",
                                            "GraphViz DOT format",
                                            "text/x-graphviz",
                                            NULL,
                                            (const unsigned char*)"http://www.graphviz.org/doc/info/lang.html",
                                            &raptor_dot_serializer_register_factory);
}
