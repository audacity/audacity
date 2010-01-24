/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_serialize_rdfxmla.c - RDF/XML with abbreviations serializer
 *
 * Copyright (C) 2004-2008, David Beckett http://purl.org/net/dajobe/
 * Copyright (C) 2004-2005, University of Bristol, UK http://www.bristol.ac.uk/
 * Copyright (C) 2005, Steve Shepard steveshep@gmail.com
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
 * FIXME Duplicate code
 *
 * Parts of this is taken from redland librdf_node.h and librdf_node.c
 */

/*
 * Raptor rdfxml-abbrev serializer object
 */
typedef struct {
  raptor_namespace_stack *nstack;       /* Namespace stack */
  raptor_namespace *xml_nspace;         /* the xml: namespace */
  raptor_namespace *rdf_nspace;         /* the rdf: namespace */
  raptor_xml_element* rdf_RDF_element;  /* the rdf:RDF element */
  raptor_xml_writer *xml_writer;        /* where the xml is being written */
  raptor_sequence *namespaces;          /* User declared namespaces */
  raptor_sequence *subjects;            /* subject items */
  raptor_sequence *blanks;              /* blank subject items */
  raptor_avltree *nodes;               /* nodes */
  raptor_abbrev_node *rdf_type;         /* rdf:type uri */

  /* URI of rdf:XMLLiteral */
  raptor_uri* rdf_xml_literal_uri;

  /* non-zero if is Adobe XMP abbreviated form */
  int is_xmp;

  /* non zero if rdf:RDF has been written (and thus no new namespaces
   * can be declared).
   */
  int written_header;

  /* for labeling namespaces */
  int namespace_count;
} raptor_rdfxmla_context;


/* prototypes for functions */

static int raptor_rdfxmla_emit_resource(raptor_serializer *serializer,
                                        raptor_xml_element *element,
                                        raptor_abbrev_node *node,
                                        int depth);

static int raptor_rdfxmla_emit_literal(raptor_serializer *serializer,
                                       raptor_xml_element *element,
                                       raptor_abbrev_node *node,
                                       int depth);
static int raptor_rdfxmla_emit_xml_literal(raptor_serializer *serializer,
                                           raptor_xml_element *element,
                                           raptor_abbrev_node* node,
                                           int depth);
static int raptor_rdfxmla_emit_blank(raptor_serializer *serializer,
                                     raptor_xml_element *element,
                                     raptor_abbrev_node* node,
                                     int depth);
static int raptor_rdfxmla_emit_subject_list_items(raptor_serializer* serializer,
                                                  raptor_abbrev_subject* subject,
                                                  int depth);
static int raptor_rdfxmla_emit_subject_properties(raptor_serializer *serializer,
                                                  raptor_abbrev_subject* subject,
                                                  int depth);
static int raptor_rdfxmla_emit_subject(raptor_serializer *serializer,
                                       raptor_abbrev_subject* subject,
                                       int depth);
static int raptor_rdfxmla_emit(raptor_serializer *serializer);

static int raptor_rdfxmla_serialize_init(raptor_serializer* serializer,
                                         const char *name);
static void raptor_rdfxmla_serialize_terminate(raptor_serializer* serializer);
static int raptor_rdfxmla_serialize_declare_namespace(raptor_serializer* serializer, 
                                                      raptor_uri *uri,
                                                      const unsigned char *prefix);
static int raptor_rdfxmla_serialize_start(raptor_serializer* serializer);
static int raptor_rdfxmla_serialize_statement(raptor_serializer* serializer, 
                                              const raptor_statement *statement);

static int raptor_rdfxmla_serialize_end(raptor_serializer* serializer);
static void raptor_rdfxmla_serialize_finish_factory(raptor_serializer_factory* factory);


/* helper functions */


/*
 * raptor_rdfxmla_emit_resource:
 * @serializer: #raptor_serializer object
 * @element: XML Element
 * @node: resource node
 * @depth: depth into tree
 * 
 * Emit a description of a resource using an XML Element
 * 
 * Return value: non-0 on failure
 **/
static int
raptor_rdfxmla_emit_resource(raptor_serializer *serializer,
                             raptor_xml_element *element,
                             raptor_abbrev_node* node,
                             int depth) 
{
  raptor_rdfxmla_context* context=(raptor_rdfxmla_context*)serializer->context;
  raptor_xml_writer *xml_writer = context->xml_writer;
  raptor_qname **attrs;
  unsigned char *attr_name;
  unsigned char *attr_value;
  
  RAPTOR_DEBUG5("Emitting resource node %p refcount %d subject %d object %d\n",
                node, 
                node->ref_count, node->count_as_subject, node->count_as_object);

  if(node->type != RAPTOR_IDENTIFIER_TYPE_RESOURCE)
    return 1;

  attrs = (raptor_qname **)RAPTOR_CALLOC(qnamearray, 1, sizeof(raptor_qname *));
  if(!attrs)
    return 1;
    
  attr_name = (unsigned char *)"resource";

  if(serializer->feature_relative_uris)
    /* newly allocated string */
    attr_value = raptor_uri_to_relative_uri_string(serializer->base_uri,
                                                   node->value.resource.uri);
  else
    attr_value = raptor_uri_as_string(node->value.resource.uri);

  attrs[0] = raptor_new_qname_from_namespace_local_name(context->rdf_nspace,
                                                        attr_name, 
                                                        attr_value);
      
  if(serializer->feature_relative_uris)
    RAPTOR_FREE(cstring, attr_value);

  if(!attrs[0]) {
    RAPTOR_FREE(qnamearray, attrs);
    return 1;
  }

  raptor_xml_element_set_attributes(element, attrs, 1);

  raptor_xml_writer_start_element(xml_writer, element);
  raptor_xml_writer_end_element(context->xml_writer, element);

  RAPTOR_DEBUG2("Emitted %p\n", node);
  
  return 0;
}


/*
 * raptor_rdfxmla_emit_literal:
 * @serializer: #raptor_serializer object
 * @element: XML Element
 * @node: literal node
 * @depth: depth into tree
 * 
 * Emit a description of a literal using an XML Element
 * 
 * Return value: non-0 on failure
 **/
static int
raptor_rdfxmla_emit_literal(raptor_serializer *serializer,
                            raptor_xml_element *element,
                            raptor_abbrev_node* node,
                            int depth)
{
  raptor_rdfxmla_context* context=(raptor_rdfxmla_context*)serializer->context;
  raptor_xml_writer *xml_writer = context->xml_writer;
  raptor_qname **attrs;
  int attrs_count;
  
  RAPTOR_DEBUG5("Emitting literal node %p refcount %d subject %d object %d\n",
                node, 
                node->ref_count, node->count_as_subject, node->count_as_object);

  if(node->type != RAPTOR_IDENTIFIER_TYPE_LITERAL)
    return 1;
  
  if(node->value.literal.language || node->value.literal.datatype) {
          
    attrs_count = 0;
    attrs = (raptor_qname **)RAPTOR_CALLOC(qnamearray,2,sizeof(raptor_qname *));
    if(!attrs)
      return 1;

    if(node->value.literal.language) {
      attrs[attrs_count] = raptor_new_qname(context->nstack,
                                            (unsigned char*)"xml:lang",
                                            (unsigned char*)node->value.literal.language,
                                            (raptor_simple_message_handler)raptor_serializer_simple_error,
                                            serializer);
      if(!attrs[attrs_count])
        goto attrs_oom;
      attrs_count++;
    }

    if(node->value.literal.datatype) {
      unsigned char *datatype_value;
      datatype_value = raptor_uri_as_string(node->value.literal.datatype);
      attrs[attrs_count] = raptor_new_qname_from_namespace_local_name(context->rdf_nspace, (const unsigned char*)"datatype",
                                                                      datatype_value);
      if(!attrs[attrs_count])
        goto attrs_oom;
      attrs_count++;

      /* SJS Note: raptor_default_uri_as_string simply returns a
       * pointer to the string. Hope this is also true of alternate
       * uri implementations. */
      /* RAPTOR_FREE(cstring, datatype_value); */

    }

    raptor_xml_element_set_attributes(element, attrs, attrs_count);

  }
      
  raptor_xml_writer_start_element(xml_writer, element);
  raptor_xml_writer_cdata(xml_writer, node->value.literal.string);
  raptor_xml_writer_end_element(xml_writer, element);

  RAPTOR_DEBUG2("Emitted %p\n", node);
  
  return 0;

  attrs_oom:

  raptor_serializer_error(serializer, "Out of memory");

  /* attrs_count has not been incremented yet
   * and it points to the qname the allocation of which failed */
  attrs_count--;
  while(attrs_count>=0)
    raptor_free_qname(attrs[attrs_count--]);

  RAPTOR_FREE(qnamearray, attrs);

  return 1;
}


/*
 * raptor_rdfxmla_emit_xml_literal:
 * @serializer: #raptor_serializer object
 * @element: XML Element
 * @node: XML literal node
 * @depth: depth into tree
 * 
 * Emit a description of a literal using an XML Element
 * 
 * Return value: non-0 on failure
 **/
static int
raptor_rdfxmla_emit_xml_literal(raptor_serializer *serializer,
                                raptor_xml_element *element,
                                raptor_abbrev_node* node,
                                int depth) 
{
  raptor_rdfxmla_context* context=(raptor_rdfxmla_context*)serializer->context;
  raptor_xml_writer *xml_writer = context->xml_writer;
  raptor_qname **attrs;
  
  RAPTOR_DEBUG5("Emitting XML literal node %p refcount %d subject %d object %d\n",
                node, 
                node->ref_count, node->count_as_subject, node->count_as_object);

  if(node->type != RAPTOR_IDENTIFIER_TYPE_XML_LITERAL)
    return 1;
  
  attrs = (raptor_qname **)RAPTOR_CALLOC(qnamearray, 1, sizeof(raptor_qname *));
  if(!attrs)
    return 1;
  
  attrs[0] = raptor_new_qname_from_namespace_local_name(context->rdf_nspace,
                                                        (const unsigned char*)"parseType",
                                                        (const unsigned char*)"Literal");
  raptor_xml_element_set_attributes(element, attrs, 1);
  raptor_xml_writer_start_element(xml_writer, element);
  raptor_xml_writer_raw(xml_writer, node->value.literal.string);
  raptor_xml_writer_end_element(xml_writer, element);

  return 0;  
}


/*
 * raptor_rdfxmla_emit_blank:
 * @serializer: #raptor_serializer object
 * @element: XML Element
 * @node: blank node
 * @depth: depth into tree
 * 
 * Emit a description of a blank node using an XML Element
 * 
 * Return value: non-0 on failure
 **/
static int
raptor_rdfxmla_emit_blank(raptor_serializer *serializer,
                          raptor_xml_element *element, raptor_abbrev_node* node,
                          int depth) 
{
  raptor_rdfxmla_context* context=(raptor_rdfxmla_context*)serializer->context;

  RAPTOR_DEBUG5("Emitting blank node %p refcount %d subject %d object %d\n",
                node, 
                node->ref_count, node->count_as_subject, node->count_as_object);

  if(node->type != RAPTOR_IDENTIFIER_TYPE_ANONYMOUS)
    return 1;
  
  if((node->count_as_subject == 1 && node->count_as_object == 1)) {
    /* If this is only used as a 1 subject and object or never
     * used as a subject or never used as an object, it never need
     * be referenced with an explicit name */
    int idx;
    raptor_abbrev_subject* blank;

    raptor_xml_writer_start_element(context->xml_writer, element);

    blank = raptor_abbrev_subject_find(context->blanks, node->type,
                                       node->value.blank.string, &idx);
          
    if(blank) {
      raptor_rdfxmla_emit_subject(serializer, blank, depth+1);
      raptor_sequence_set_at(context->blanks, idx, NULL);
    }
          
  } else {
    unsigned char *attr_name = (unsigned char*)"nodeID";
    unsigned char *attr_value = node->value.blank.string;
    raptor_qname **attrs;

    attrs = (raptor_qname **)RAPTOR_CALLOC(qnamearray,1,sizeof(raptor_qname *));
    if(!attrs)
      return 1;

    attrs[0] = raptor_new_qname_from_namespace_local_name(context->rdf_nspace,
                                                          attr_name,
                                                          attr_value);

    raptor_xml_element_set_attributes(element, attrs, 1);
    raptor_xml_writer_start_element(context->xml_writer, element);

  }

  raptor_xml_writer_end_element(context->xml_writer, element);

  RAPTOR_DEBUG2("Emitted %p\n", node);
  
  return 0;
}


/*
 * raptor_rdfxmla_emit_subject_list_items:
 * @serializer: #raptor_serializer object
 * @subject: subject node
 * @depth: depth into tree
 * 
 * Emit an rdf list of items (rdf:li) about a subject node.
 * 
 * Return value: non-0 on failure
 **/
static int
raptor_rdfxmla_emit_subject_list_items(raptor_serializer* serializer,
                                       raptor_abbrev_subject* subject,
                                       int depth)
{
  raptor_rdfxmla_context* context=(raptor_rdfxmla_context*)serializer->context;
  int rv = 0;
  int i=0;
  raptor_uri* base_uri=NULL;
  
  RAPTOR_DEBUG5("Emitting subject list items for node %p refcount %d subject %d object %d\n", 
                subject->node,
                subject->node->ref_count, subject->node->count_as_subject, 
                subject->node->count_as_object);

  while (!rv && i < raptor_sequence_size(subject->list_items)) {

    raptor_abbrev_node* object;
    raptor_qname *qname;
    raptor_xml_element *element;
    
    object = (raptor_abbrev_node* )raptor_sequence_get_at(subject->list_items, i++);
    if(!object)
      continue;
    
    qname = raptor_new_qname_from_namespace_local_name(context->rdf_nspace,
                                                       (unsigned char *)"li",
                                                       NULL);
    
    if(serializer->base_uri)
      base_uri=raptor_uri_copy(serializer->base_uri);
    element = raptor_new_xml_element(qname, NULL, base_uri);
    if(!element) {
      raptor_serializer_error(serializer, "Out of memory");
      raptor_free_qname(qname);
      rv=1; /* error */
      break;
    }

    switch (object->type) {
      
      case RAPTOR_IDENTIFIER_TYPE_RESOURCE:
        rv = raptor_rdfxmla_emit_resource(serializer, element, object,
                                          depth+1);
        break;
          
      case RAPTOR_IDENTIFIER_TYPE_LITERAL:
        rv = raptor_rdfxmla_emit_literal(serializer, element, object,
                                         depth+1);
        break;
          
      case RAPTOR_IDENTIFIER_TYPE_XML_LITERAL:
        rv = raptor_rdfxmla_emit_xml_literal(serializer, element, object,
                                             depth+1);
        break;
          
      case RAPTOR_IDENTIFIER_TYPE_ANONYMOUS:
        rv = raptor_rdfxmla_emit_blank(serializer, element, object, depth+1);
        break;

      case RAPTOR_IDENTIFIER_TYPE_ORDINAL:
        /* ordinals should never appear as an object with current parsers */
      case RAPTOR_IDENTIFIER_TYPE_PREDICATE:
        /* predicates should never appear as an object */
      case RAPTOR_IDENTIFIER_TYPE_UNKNOWN:
      default:
        RAPTOR_FATAL1("Unsupported identifier type\n");
        break;

    }
    
    raptor_free_xml_element(element);

  }
  
  return rv;
}


/*
 * raptor_rdfxmla_emit_subject_properties:
 * @serializer: #raptor_serializer object
 * @subject: subject node
 * @depth: depth into tree
 * 
 * Emit the properties about a subject node.
 * 
 * Return value: non-0 on failure
 **/
static int
raptor_rdfxmla_emit_subject_properties(raptor_serializer* serializer,
                                       raptor_abbrev_subject* subject,
                                       int depth)
{
  raptor_rdfxmla_context* context=(raptor_rdfxmla_context*)serializer->context;
  int rv = 0;  
  int i;
  
  RAPTOR_DEBUG5("Emitting subject properties for node %p refcount %d subject %d object %d\n", 
                subject->node, subject->node->ref_count, 
                subject->node->count_as_subject,
                subject->node->count_as_object);

  /* Emit any rdf:_n properties collected */
  if(raptor_sequence_size(subject->list_items) > 0) {
    rv = raptor_rdfxmla_emit_subject_list_items(serializer, subject, depth+1);
    if(rv)
      return rv;
  }

  for(i=0, rv=raptor_avltree_cursor_first(subject->properties);
      !rv;
      i++, (rv=raptor_avltree_cursor_next(subject->properties))) {
    raptor_uri *base_uri=NULL;
    raptor_qname *qname;
    raptor_xml_element *element;
    raptor_abbrev_node** nodes=(raptor_abbrev_node**)raptor_avltree_cursor_get(subject->properties);
    raptor_abbrev_node* predicate = nodes[0];
    raptor_abbrev_node* object = nodes[1];

    if(predicate->type == RAPTOR_IDENTIFIER_TYPE_ORDINAL) {
      /* we should only get here in rare cases -- usually when there
       * are multiple ordinals with the same value. */

      unsigned char uri_string[MAX_ASCII_INT_SIZE + 2];

      sprintf((char*)uri_string, "_%d", predicate->value.ordinal.ordinal);

      qname = raptor_new_qname_from_namespace_local_name(context->rdf_nspace,
                                                         uri_string, NULL);
      if(!qname)
        goto oom;
      
    } else {
      qname = raptor_new_qname_from_resource(context->namespaces,
                                             context->nstack,
                                             &context->namespace_count,
                                             predicate);
      if(!qname) {
        raptor_serializer_error(serializer,
                                "Cannot split URI '%s' into an XML qname",
                                raptor_uri_as_string(predicate->value.resource.uri));
        continue;
      }
    }
    
    if(serializer->base_uri)
      base_uri=raptor_uri_copy(serializer->base_uri);
    element = raptor_new_xml_element(qname, NULL, base_uri);
    if(!element) {
      if(base_uri)
        raptor_free_uri(base_uri);
      raptor_free_qname(qname);
      goto oom;
    }

    switch (object->type) {
      
      case RAPTOR_IDENTIFIER_TYPE_RESOURCE:
        rv = raptor_rdfxmla_emit_resource(serializer, element, object, depth+1);
        break;
          
      case RAPTOR_IDENTIFIER_TYPE_LITERAL:
        rv = raptor_rdfxmla_emit_literal(serializer, element, object, depth+1);
        break;
          
      case RAPTOR_IDENTIFIER_TYPE_ANONYMOUS:
        rv = raptor_rdfxmla_emit_blank(serializer, element, object, depth+1);
        break;
          
      case RAPTOR_IDENTIFIER_TYPE_XML_LITERAL:
        rv = raptor_rdfxmla_emit_xml_literal(serializer, element, object,
                                             depth+1);
        break;

      case RAPTOR_IDENTIFIER_TYPE_ORDINAL:
        /* ordinals should never appear as an object with current parsers */
      case RAPTOR_IDENTIFIER_TYPE_PREDICATE:
        /* predicates should never appear as an object */
      case RAPTOR_IDENTIFIER_TYPE_UNKNOWN:
      default:
        RAPTOR_FATAL1("Unsupported identifier type\n");
        break;
    }    

    raptor_free_xml_element(element);
    
  }
         
  return rv;

  oom:
  raptor_serializer_error(serializer, "Out of memory");
  return 1;
}


/*
 * raptor_rdfxmla_emit_subject:
 * @serializer: #raptor_serializer object
 * @subject: subject node
 * @depth: depth into tree
 * 
 * Emit a subject node
 * 
 * Return value: non-0 on failure
 **/
static int
raptor_rdfxmla_emit_subject(raptor_serializer *serializer,
                            raptor_abbrev_subject* subject,
                            int depth) 
{
  raptor_rdfxmla_context* context=(raptor_rdfxmla_context*)serializer->context;  
  raptor_qname *qname = NULL;    
  raptor_xml_element *element=NULL;
  raptor_qname **attrs;
  unsigned char *attr_name;
  unsigned char *attr_value;
  raptor_uri *base_uri=NULL;

  RAPTOR_DEBUG5("Emitting subject node %p refcount %d subject %d object %d\n", 
                subject->node,
                subject->node->ref_count, 
                subject->node->count_as_subject,
                subject->node->count_as_object);

  if(!depth &&
     subject->node->type == RAPTOR_IDENTIFIER_TYPE_ANONYMOUS &&
     subject->node->count_as_subject == 1 &&
     subject->node->count_as_object == 1) {
    RAPTOR_DEBUG2("Skipping subject node %p\n", subject->node);
    return 0;
  }
  

  if(subject->node_type) { /* if rdf:type was associated with this subject */
    qname = raptor_new_qname_from_resource(context->namespaces,
                                           context->nstack,
                                           &context->namespace_count,
                                           subject->node_type);
    
    if(!qname) {
      raptor_serializer_error(serializer,
                              "Cannot split URI '%s' into an XML qname",
                              raptor_uri_as_string(subject->node_type->value.resource.uri));
      return 1;
    }
    
  } else {
    qname = raptor_new_qname_from_namespace_local_name(context->rdf_nspace,
                                                       (unsigned const char*)"Description",  NULL);
    if(!qname)
      goto oom;    
  }

  if(serializer->base_uri)
    base_uri=raptor_uri_copy(serializer->base_uri);
  element = raptor_new_xml_element(qname, NULL, base_uri);
  if(!element) {
    if(base_uri)
      raptor_free_uri(base_uri);
    raptor_free_qname(qname);
    goto oom;
  }
    
  attrs = (raptor_qname **)RAPTOR_CALLOC(qnamearray, 1, sizeof(raptor_qname *));
  if(!attrs)
    goto oom;
    
  attr_name = NULL;
  attr_value = NULL;
    
  /* emit the subject node */
  if(subject->node->type == RAPTOR_IDENTIFIER_TYPE_RESOURCE) {
    attr_name = (unsigned char*)"about";
    if(context->is_xmp) {
      /* XML rdf:about value is always "" */
      attr_value = (unsigned char *)RAPTOR_CALLOC(string, 1, 
                                                  sizeof(unsigned char*));
    } else if(serializer->feature_relative_uris)
      attr_value = raptor_uri_to_relative_uri_string(serializer->base_uri,
                                                     subject->node->value.resource.uri);
    else
      attr_value = raptor_uri_to_string(subject->node->value.resource.uri);
    
  } else if(subject->node->type == RAPTOR_IDENTIFIER_TYPE_ANONYMOUS) {
    if(subject->node->count_as_subject &&
       subject->node->count_as_object &&
       !(subject->node->count_as_subject == 1 && 
         subject->node->count_as_object == 1)) {
      /* No need for nodeID if this node is never used as a subject
       * or object OR if it is used exactly once as subject and object.
       */
      attr_name = (unsigned char*)"nodeID";
      attr_value = subject->node->value.blank.string;
    }
  } else if(subject->node->type == RAPTOR_IDENTIFIER_TYPE_ORDINAL) {
    attr_name = (unsigned char*)"about";
    attr_value = (unsigned char *)RAPTOR_MALLOC(string,
                                                raptor_rdf_namespace_uri_len + MAX_ASCII_INT_SIZE + 2);
    if(!attr_value) {
      RAPTOR_FREE(qnamearray, attrs);
      goto oom;
    }
    sprintf((char*)attr_value, "%s_%d", raptor_rdf_namespace_uri,
            subject->node->value.ordinal.ordinal);
  } 
    
  if(attr_name) {
    attrs[0] = raptor_new_qname_from_namespace_local_name(context->rdf_nspace,
                                                          attr_name,
                                                          attr_value);
    
    if(subject->node->type != RAPTOR_IDENTIFIER_TYPE_ANONYMOUS)
      RAPTOR_FREE(cstring, attr_value);
    
    if(!attrs[0]) {
      RAPTOR_FREE(qnamearray, attrs);
      goto oom;  
    }

    /* Note: if we were willing to track the in-scope rdf:lang, we
     * could do the "2.5 Property Attributes" abbreviation here */
    raptor_xml_element_set_attributes(element, attrs, 1);
  } else {
    RAPTOR_FREE(qnamearray, attrs);
  }
    
  raptor_xml_writer_start_element(context->xml_writer, element);
    
  raptor_rdfxmla_emit_subject_properties(serializer, subject, depth+1);
    
  raptor_xml_writer_end_element(context->xml_writer, element);
    
  raptor_free_xml_element(element);
    
  return 0;

  oom:
  if(element)
    raptor_free_xml_element(element);
  raptor_serializer_error(serializer, "Out of memory");
  return 1;
}


/*
 * raptor_rdfxmla_emit - 
 * @serializer: #raptor_serializer object
 * 
 * Emit RDF/XML for all stored triples.
 * 
 * Return value: non-0 on failure
 **/
static int
raptor_rdfxmla_emit(raptor_serializer *serializer)
{
  raptor_rdfxmla_context* context=(raptor_rdfxmla_context*)serializer->context;
  raptor_abbrev_subject* subject;
  raptor_abbrev_subject* blank;
  int i;
  
  for(i=0; i < raptor_sequence_size(context->subjects); i++) {
    subject = (raptor_abbrev_subject* )raptor_sequence_get_at(context->subjects, i);
    if(subject)
      raptor_rdfxmla_emit_subject(serializer, subject, 0);
  }
    
  /* Emit any remaining blank nodes */
  for(i=0; i < raptor_sequence_size(context->blanks); i++) {
    blank = (raptor_abbrev_subject* )raptor_sequence_get_at(context->blanks, i);
    if(blank)
      raptor_rdfxmla_emit_subject(serializer, blank, 0);
  }
    
  return 0;
}


/*
 * raptor serializer rdfxml-abbrev implementation
 */


/* create a new serializer */
static int
raptor_rdfxmla_serialize_init(raptor_serializer* serializer, const char *name)
{
  raptor_rdfxmla_context* context=(raptor_rdfxmla_context*)serializer->context;
  const raptor_uri_handler *uri_handler;
  raptor_uri *rdf_type_uri;
  void *uri_context;
  
  raptor_uri_get_handler(&uri_handler, &uri_context);
  context->nstack=raptor_new_namespaces(uri_handler, uri_context,
                                        (raptor_simple_message_handler)raptor_serializer_simple_error,
                                        serializer,
                                        1);
  if(!context->nstack)
    return 1;

  context->xml_nspace=raptor_new_namespace(context->nstack,
                                           (const unsigned char*)"xml",
                                           (const unsigned char*)raptor_xml_namespace_uri,
                                           0);
  
  context->rdf_nspace=raptor_new_namespace(context->nstack,
                                           (const unsigned char*)"rdf",
                                           (const unsigned char*)raptor_rdf_namespace_uri,
                                           0);

  context->namespaces=raptor_new_sequence(NULL, NULL);

  context->subjects =
    raptor_new_sequence((raptor_sequence_free_handler *)raptor_free_abbrev_subject,NULL);

  context->blanks =
    raptor_new_sequence((raptor_sequence_free_handler *)raptor_free_abbrev_subject,NULL);
  
  context->nodes =
    raptor_new_avltree((raptor_data_compare_function)raptor_abbrev_node_cmp,
                       (raptor_data_free_function)raptor_free_abbrev_node, 0);

  rdf_type_uri = raptor_new_uri_for_rdf_concept("type");
  if(rdf_type_uri) {    
    context->rdf_type = raptor_new_abbrev_node(RAPTOR_IDENTIFIER_TYPE_RESOURCE,
                                               rdf_type_uri, NULL, NULL);
    raptor_free_uri(rdf_type_uri);
  }

  context->rdf_xml_literal_uri=raptor_new_uri(raptor_xml_literal_datatype_uri_string);

  if(!context->xml_nspace || !context->rdf_nspace || !context->namespaces ||
     !context->subjects || !context->blanks || !context->nodes ||
     !context->rdf_type || !context->rdf_xml_literal_uri) {
    raptor_rdfxmla_serialize_terminate(serializer);
    return 1;
  }

  context->is_xmp=!strncmp(name, "rdfxml-xmp", 10);
  if(context->is_xmp)
    serializer->feature_write_xml_declaration=0;

  /* Note: item 0 in the list is rdf:RDF's namespace */
  if(raptor_sequence_push(context->namespaces, context->rdf_nspace)) {
    raptor_rdfxmla_serialize_terminate(serializer);
    return 1;
  }

  return 0;
}
  

/* destroy a serializer */
static void
raptor_rdfxmla_serialize_terminate(raptor_serializer* serializer)
{
  raptor_rdfxmla_context* context=(raptor_rdfxmla_context*)serializer->context;

  if(context->xml_writer) {
    raptor_free_xml_writer(context->xml_writer);
    context->xml_writer=NULL;
  }

  if(context->rdf_RDF_element) {
    raptor_free_xml_element(context->rdf_RDF_element);
    context->rdf_RDF_element=NULL;
  }

  if(context->rdf_nspace) {
    raptor_free_namespace(context->rdf_nspace);
    context->rdf_nspace=NULL;
  }

  if(context->xml_nspace) {
    raptor_free_namespace(context->xml_nspace);
    context->xml_nspace=NULL;
  }

  if(context->namespaces) {
    int i;
    
    /* Note: item 0 in the list is rdf:RDF's namespace and freed above */
    for(i=1; i< raptor_sequence_size(context->namespaces); i++) {
      raptor_namespace* ns;
      ns =(raptor_namespace*)raptor_sequence_get_at(context->namespaces, i);
      if(ns)
        raptor_free_namespace(ns);
    }
    raptor_free_sequence(context->namespaces);
    context->namespaces=NULL;
  }

  if(context->subjects) {
    raptor_free_sequence(context->subjects);
    context->subjects=NULL;
  }
  
  if(context->blanks) {
    raptor_free_sequence(context->blanks);
    context->blanks=NULL;
  }
  
  if(context->nodes) {
    raptor_free_avltree(context->nodes);
    context->nodes=NULL;
  }
  
  if(context->nstack) {
    raptor_free_namespaces(context->nstack);
    context->nstack=NULL;
  }

  if(context->rdf_type) {
    raptor_free_abbrev_node(context->rdf_type);
    context->rdf_type=NULL;
  }
  
  if(context->rdf_xml_literal_uri) {
    raptor_free_uri(context->rdf_xml_literal_uri);
    context->rdf_xml_literal_uri=NULL;
  }
}


#define RDFXMLA_NAMESPACE_DEPTH 0

/* add a namespace */
static int
raptor_rdfxmla_serialize_declare_namespace_from_namespace(raptor_serializer* serializer, 
                                                          raptor_namespace *nspace)
{
  raptor_rdfxmla_context* context=(raptor_rdfxmla_context*)serializer->context;
  int i;
  
  if(context->written_header)
    return 1;
  
  for(i=0; i< raptor_sequence_size(context->namespaces); i++) {
    raptor_namespace* ns;
    ns=(raptor_namespace*)raptor_sequence_get_at(context->namespaces, i);

    /* If prefix is already declared, ignore it */
    if(!ns->prefix && !nspace->prefix)
      return 1;
    
    if(ns->prefix && nspace->prefix && 
       !strcmp((const char*)ns->prefix, (const char*)nspace->prefix))
      return 1;

    if(ns->uri && nspace->uri &&
       raptor_uri_equals(ns->uri, nspace->uri))
      return 1;
  }

  nspace=raptor_new_namespace_from_uri(context->nstack,
                                       nspace->prefix, nspace->uri,
                                       RDFXMLA_NAMESPACE_DEPTH);
  if(!nspace)
    return 1;
  
  raptor_sequence_push(context->namespaces, nspace);
  return 0;
}


/* add a namespace */
static int
raptor_rdfxmla_serialize_declare_namespace(raptor_serializer* serializer, 
                                           raptor_uri *uri,
                                           const unsigned char *prefix)
{
  raptor_rdfxmla_context* context=(raptor_rdfxmla_context*)serializer->context;
  raptor_namespace *ns;
  int rc;
  
  ns=raptor_new_namespace_from_uri(context->nstack, prefix, uri, 
                                   RDFXMLA_NAMESPACE_DEPTH);

  rc=raptor_rdfxmla_serialize_declare_namespace_from_namespace(serializer, 
                                                               ns);
  raptor_free_namespace(ns);
  
  return rc;
}


/* start a serialize */
static int
raptor_rdfxmla_serialize_start(raptor_serializer* serializer)
{
  raptor_rdfxmla_context* context=(raptor_rdfxmla_context*)serializer->context;
  raptor_xml_writer* xml_writer;
  const raptor_uri_handler *uri_handler;
  void *uri_context;

  raptor_uri_get_handler(&uri_handler, &uri_context);

  if(context->xml_writer)
    raptor_free_xml_writer(context->xml_writer);

  xml_writer=raptor_new_xml_writer(context->nstack,
                                   uri_handler, uri_context,
                                   serializer->iostream,
                                   (raptor_simple_message_handler)raptor_serializer_simple_error,
                                   serializer,
                                   1);
  if(!xml_writer)
    return 1;

  raptor_xml_writer_set_feature(xml_writer,RAPTOR_FEATURE_WRITER_AUTO_INDENT,1);
  raptor_xml_writer_set_feature(xml_writer,RAPTOR_FEATURE_WRITER_AUTO_EMPTY, 1);
  raptor_xml_writer_set_feature(xml_writer,RAPTOR_FEATURE_WRITER_INDENT_WIDTH,2);
  raptor_xml_writer_set_feature(xml_writer, RAPTOR_FEATURE_WRITER_XML_VERSION,
                                serializer->xml_version);
  raptor_xml_writer_set_feature(xml_writer, 
                                RAPTOR_FEATURE_WRITER_XML_DECLARATION, 
                                serializer->feature_write_xml_declaration);
  
  context->xml_writer=xml_writer;

  return 0;
}


static int
raptor_rdfxmla_ensure_writen_header(raptor_serializer* serializer,
                                    raptor_rdfxmla_context* context) 
{
  raptor_xml_writer* xml_writer;
  raptor_qname *qname;
  raptor_uri *base_uri;
  int i;
  raptor_qname **attrs=NULL;
  int attrs_count=0;

  if(context->written_header)
    return 0; /* already succeeded */
  
  xml_writer=context->xml_writer;
  if(context->is_xmp)
    raptor_xml_writer_raw(xml_writer,
                          (const unsigned char*)"<?xpacket begin='﻿' id='W5M0MpCehiHzreSzNTczkc9d'?>\n<x:xmpmeta xmlns:x='adobe:ns:meta/'>");
  
  qname=raptor_new_qname_from_namespace_local_name(context->rdf_nspace,
                                                   (const unsigned char*)"RDF",
                                                   NULL);
  if(!qname)
    goto oom;
  base_uri=serializer->base_uri;
  if(base_uri)
    base_uri=raptor_uri_copy(base_uri);
  context->rdf_RDF_element=raptor_new_xml_element(qname, NULL, base_uri);
  if(!context->rdf_RDF_element) {
    if(base_uri)
      raptor_free_uri(base_uri);
    raptor_free_qname(qname);
    goto oom;
  }
  
  /* NOTE: Starts at item 1 as item 0 is the element's namespace (rdf) 
   * and does not need to be declared
   */
  for(i=1; i< raptor_sequence_size(context->namespaces); i++) {
    raptor_namespace* ns;
    ns=(raptor_namespace*)raptor_sequence_get_at(context->namespaces, i);
    raptor_xml_element_declare_namespace(context->rdf_RDF_element, ns);
  }

  if(base_uri) {
    const unsigned char* base_uri_string;

    attrs=(raptor_qname **)RAPTOR_CALLOC(qnamearray, 1, sizeof(raptor_qname*));
    if(!attrs)
      goto oom;

    base_uri_string=raptor_uri_as_string(base_uri);
    attrs[attrs_count]=raptor_new_qname_from_namespace_local_name(context->xml_nspace, (const unsigned char*)"base",  base_uri_string);
    if(!attrs[attrs_count]) {
      RAPTOR_FREE(qnamearray, attrs);
      goto oom;
    }
    attrs_count++;
  }

  if(attrs_count)
    raptor_xml_element_set_attributes(context->rdf_RDF_element, attrs, 
                                      attrs_count);
  else
    raptor_xml_element_set_attributes(context->rdf_RDF_element, NULL, 0);



  raptor_xml_writer_start_element(xml_writer, context->rdf_RDF_element);
  
  context->written_header=1;

  return 0;

  oom:
  raptor_serializer_error(serializer, "Out of memory");
  return 1;
}
  

/* serialize a statement */
static int
raptor_rdfxmla_serialize_statement(raptor_serializer* serializer, 
                                   const raptor_statement *statement)
{
  raptor_rdfxmla_context* context=(raptor_rdfxmla_context*)serializer->context;
  raptor_abbrev_subject* subject = NULL;
  raptor_abbrev_node* predicate = NULL;
  raptor_abbrev_node* object = NULL;
  int rv = 0;
  raptor_identifier_type object_type;

  if(statement->subject_type == RAPTOR_IDENTIFIER_TYPE_RESOURCE ||
     statement->subject_type == RAPTOR_IDENTIFIER_TYPE_ANONYMOUS ||
     statement->subject_type == RAPTOR_IDENTIFIER_TYPE_ORDINAL) {

    subject = raptor_abbrev_subject_lookup(context->nodes, context->subjects,
                                           context->blanks,
                                           statement->subject_type,
                                           statement->subject);
    if(!subject)
      return 1;

  } else {
    raptor_serializer_error(serializer,
                            "Cannot serialize a triple with subject node type %d\n",
                            statement->subject_type);
    return 1;
  }  
  
  object_type=statement->object_type;
  if(object_type == RAPTOR_IDENTIFIER_TYPE_LITERAL) {
    if(statement->object_literal_datatype &&
       raptor_uri_equals(statement->object_literal_datatype, 
                         context->rdf_xml_literal_uri))
      object_type = RAPTOR_IDENTIFIER_TYPE_XML_LITERAL;
  }

  if(object_type == RAPTOR_IDENTIFIER_TYPE_RESOURCE ||
     object_type == RAPTOR_IDENTIFIER_TYPE_ANONYMOUS ||
     object_type == RAPTOR_IDENTIFIER_TYPE_LITERAL ||
     object_type == RAPTOR_IDENTIFIER_TYPE_XML_LITERAL || 
     object_type == RAPTOR_IDENTIFIER_TYPE_ORDINAL) {

    object = raptor_abbrev_node_lookup(context->nodes, object_type,
                                       statement->object,
                                       statement->object_literal_datatype,
                                       statement->object_literal_language);
    if(!object)
      return 1;          

    if(object_type == RAPTOR_IDENTIFIER_TYPE_RESOURCE ||
       object_type == RAPTOR_IDENTIFIER_TYPE_ANONYMOUS)
      object->count_as_object++;
    
  } else {
    raptor_serializer_error(serializer,
                            "Cannot serialize a triple with object node type %d\n",
                            object_type);
    return 1;
  }

  if((statement->predicate_type == RAPTOR_IDENTIFIER_TYPE_PREDICATE) ||
     (statement->predicate_type == RAPTOR_IDENTIFIER_TYPE_RESOURCE)) {
    predicate = raptor_abbrev_node_lookup(context->nodes,
                                          statement->predicate_type,
                                          statement->predicate, NULL, NULL);
    if(!predicate)
      return 1;

    if(!subject->node_type && 
       raptor_abbrev_node_equals(predicate, context->rdf_type) &&
       statement->object_type == RAPTOR_IDENTIFIER_TYPE_RESOURCE) {

      /* Store the first one as the type for abbreviation 2.14
       * purposes. Note that it is perfectly legal to have
       * multiple type definitions.  All definitions after the
       * first go in the property list */
      subject->node_type = raptor_abbrev_node_lookup(context->nodes,
                                                     object_type,
                                                     statement->object, NULL,
                                                     NULL);
      if(!subject->node_type)
        return 1;
      subject->node_type->ref_count++;
      return 0;
    
    } else {
      int add_property=1;

      if(context->is_xmp && predicate->ref_count > 1) {
        int i;
        for(i=0, raptor_avltree_cursor_first(subject->properties);
            !rv;
            i++, (rv=raptor_avltree_cursor_next(subject->properties))) {
          raptor_abbrev_node** nodes=(raptor_abbrev_node**)raptor_avltree_cursor_get(subject->properties);
          raptor_abbrev_node* node = nodes[0];

          if(node == predicate) {
            add_property=0;
            
            if(object->type == RAPTOR_IDENTIFIER_TYPE_ANONYMOUS) {
              /* look for any generated blank node associated with this
               * statement and free it
               */
              int idx=0;
              if(raptor_abbrev_subject_find(context->blanks, object_type,
                                            statement->object, &idx))
                raptor_sequence_set_at(context->blanks, idx, NULL);
            }
            
            break;
          }
        }
      }

      if(add_property) {
        rv = raptor_abbrev_subject_add_property(subject, predicate, object);
        if(rv) {
          raptor_serializer_error(serializer,
                                  "Unable to add properties to subject %p\n",
                                  subject);
          return rv;
        }
      }
    }
  
  } else if(statement->predicate_type == RAPTOR_IDENTIFIER_TYPE_ORDINAL) {
    int idx = *(int*)statement->predicate;
    rv = raptor_abbrev_subject_add_list_element(subject, idx, object);
    if(rv) {
      /* An ordinal might already exist at that location, the fallback
       * is to just put in the properties list */
      predicate = raptor_abbrev_node_lookup(context->nodes,
                                            statement->predicate_type,
                                            statement->predicate, NULL, NULL);
      if(!predicate)
        return 1;
      rv = raptor_abbrev_subject_add_property(subject, predicate, object);
      if(rv) {
        raptor_serializer_error(serializer,
                                "Unable to add properties to subject %p\n",
                                subject);
        return rv;
      }
    }
  } else {
    raptor_serializer_error(serializer,
                            "Cannot serialize a triple with predicate node type %d\n",
                            statement->predicate_type);
    return 1;
  }
  
  return 0;

}


/* end a serialize */
static int
raptor_rdfxmla_serialize_end(raptor_serializer* serializer)
{

  raptor_rdfxmla_context* context=(raptor_rdfxmla_context*)serializer->context;
  raptor_xml_writer* xml_writer=context->xml_writer;

  if(xml_writer) {
    if(!raptor_rdfxmla_ensure_writen_header(serializer, context)) {

      raptor_rdfxmla_emit(serializer);  

      /* ensure_writen_header() returned success, can assume context->rdf_RDF_element is non-NULL */
      raptor_xml_writer_end_element(xml_writer, context->rdf_RDF_element);

      raptor_xml_writer_raw_counted(xml_writer, (const unsigned char*)"\n", 1);
    }
  }

  if(context->rdf_RDF_element) {
    raptor_free_xml_element(context->rdf_RDF_element);
    context->rdf_RDF_element=NULL;
  }

  if(context->is_xmp && xml_writer)
    raptor_xml_writer_raw(xml_writer, 
                          (const unsigned char*)"</x:xmpmeta>\n<?xpacket end='r'?>\n");
  
  return 0;
}


/* finish the serializer factory */
static void
raptor_rdfxmla_serialize_finish_factory(raptor_serializer_factory* factory)
{
  /* NOP */
}


static int
raptor_rdfxmla_serializer_register_factory(raptor_serializer_factory *factory)
{
  factory->context_length     = sizeof(raptor_rdfxmla_context);
  
  factory->init                = raptor_rdfxmla_serialize_init;
  factory->terminate           = raptor_rdfxmla_serialize_terminate;
  factory->declare_namespace   = raptor_rdfxmla_serialize_declare_namespace;
  factory->declare_namespace_from_namespace   = raptor_rdfxmla_serialize_declare_namespace_from_namespace;
  factory->serialize_start     = raptor_rdfxmla_serialize_start;
  factory->serialize_statement = raptor_rdfxmla_serialize_statement;
  factory->serialize_end       = raptor_rdfxmla_serialize_end;
  factory->finish_factory      = raptor_rdfxmla_serialize_finish_factory;

  return 0;
}


int
raptor_init_serializer_rdfxmla(void)
{
  return 
    raptor_serializer_register_factory("rdfxml-xmp", "RDF/XML (XMP Profile)", 
                                       "application/rdf+xml",
                                       NULL,
                                       (const unsigned char*)"http://www.w3.org/TR/rdf-syntax-grammar",
                                       &raptor_rdfxmla_serializer_register_factory)
    ||
    raptor_serializer_register_factory("rdfxml-abbrev", "RDF/XML (Abbreviated)", 
                                       "application/rdf+xml",
                                       NULL,
                                       (const unsigned char*)"http://www.w3.org/TR/rdf-syntax-grammar",
                                       &raptor_rdfxmla_serializer_register_factory);
}

