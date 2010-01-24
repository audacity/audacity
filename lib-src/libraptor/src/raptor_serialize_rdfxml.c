/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_serialize_rdfxml.c - RDF/XML serializer
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
 * Raptor RDF/XML serializer object
 */
typedef struct {
  /* Namespace stack */
  raptor_namespace_stack *nstack;

  /* the xml: namespace - this is destroyed when nstack above is deleted */
  raptor_namespace *xml_nspace;

  /* the rdf: namespace - this is destroyed when nstack above is deleted */
  raptor_namespace *rdf_nspace;

  /* the rdf:RDF element */
  raptor_xml_element* rdf_RDF_element;

  /* where the xml is being written */
  raptor_xml_writer *xml_writer;

  /* User declared namespaces */
  raptor_sequence *namespaces;

  /* URI of rdf:XMLLiteral */
  raptor_uri* rdf_xml_literal_uri;

  /* non zero if rdf:RDF has been written (and thus no new namespaces
   * can be declared).
   */
  int written_header;
} raptor_rdfxml_serializer_context;


/* local prototypes */

static void
raptor_rdfxml_serialize_terminate(raptor_serializer* serializer);

/* create a new serializer */
static int
raptor_rdfxml_serialize_init(raptor_serializer* serializer, const char *name)
{
  raptor_rdfxml_serializer_context* context=(raptor_rdfxml_serializer_context*)serializer->context;
  const raptor_uri_handler *uri_handler;
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

  context->rdf_xml_literal_uri=raptor_new_uri(raptor_xml_literal_datatype_uri_string);

  if(!context->xml_nspace || !context->rdf_nspace || !context->namespaces ||
     !context->rdf_xml_literal_uri) {
    raptor_rdfxml_serialize_terminate(serializer);
    return 1;
  }

  /* Note: item 0 in the list is rdf:RDF's namespace */
  if(raptor_sequence_push(context->namespaces, context->rdf_nspace)) {
    raptor_rdfxml_serialize_terminate(serializer);
    return 1;
  }

  return 0;
}


/* destroy a serializer */
static void
raptor_rdfxml_serialize_terminate(raptor_serializer* serializer)
{
  raptor_rdfxml_serializer_context* context=(raptor_rdfxml_serializer_context*)serializer->context;

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

  if(context->rdf_xml_literal_uri) {
    raptor_free_uri(context->rdf_xml_literal_uri);
    context->rdf_xml_literal_uri=NULL;
  }

  if(context->namespaces) {
    int i;

    /* Note: item 0 in the list is rdf:RDF's namespace and freed above */
    for(i=1; i< raptor_sequence_size(context->namespaces); i++) {
      raptor_namespace* ns=(raptor_namespace*)raptor_sequence_get_at(context->namespaces, i);
      if(ns)
        raptor_free_namespace(ns);
    }
    raptor_free_sequence(context->namespaces);
    context->namespaces=NULL;
  }

  if(context->nstack) {
    raptor_free_namespaces(context->nstack);
    context->nstack=NULL;
  }
}


#define RDFXML_NAMESPACE_DEPTH 0

/* add a namespace */
static int
raptor_rdfxml_serialize_declare_namespace_from_namespace(raptor_serializer* serializer,
                                                         raptor_namespace *nspace)
{
  raptor_rdfxml_serializer_context* context=(raptor_rdfxml_serializer_context*)serializer->context;
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
                                       RDFXML_NAMESPACE_DEPTH);
  if(!nspace)
    return 1;

  raptor_sequence_push(context->namespaces, nspace);
  return 0;
}


/* add a namespace */
static int
raptor_rdfxml_serialize_declare_namespace(raptor_serializer* serializer,
                                          raptor_uri *uri,
                                          const unsigned char *prefix)
{
  raptor_rdfxml_serializer_context* context=(raptor_rdfxml_serializer_context*)serializer->context;
  raptor_namespace *ns;
  int rc;

  ns=raptor_new_namespace_from_uri(context->nstack, prefix, uri,
                                   RDFXML_NAMESPACE_DEPTH);

  rc=raptor_rdfxml_serialize_declare_namespace_from_namespace(serializer,
                                                               ns);
  raptor_free_namespace(ns);

  return rc;
}


/* start a serialize */
static int
raptor_rdfxml_serialize_start(raptor_serializer* serializer)
{
  raptor_rdfxml_serializer_context* context=(raptor_rdfxml_serializer_context*)serializer->context;
  raptor_xml_writer* xml_writer;
  const raptor_uri_handler *uri_handler;
  void *uri_context;

  raptor_uri_get_handler(&uri_handler, &uri_context);

  if(context->xml_writer) {
    raptor_free_xml_writer(context->xml_writer);
    context->xml_writer=NULL;
  }

  xml_writer=raptor_new_xml_writer(context->nstack,
                                   uri_handler, uri_context,
                                   serializer->iostream,
                                   (raptor_simple_message_handler)raptor_serializer_simple_error,
                                   serializer,
                                   1);
  if(!xml_writer)
    return 1;
  raptor_xml_writer_set_feature(xml_writer, RAPTOR_FEATURE_WRITER_XML_VERSION,
                                serializer->xml_version);
  raptor_xml_writer_set_feature(xml_writer,
                                RAPTOR_FEATURE_WRITER_XML_DECLARATION,
                                serializer->feature_write_xml_declaration);

  context->xml_writer=xml_writer;
  context->written_header=0;

  return 0;
}


static int
raptor_rdfxml_ensure_writen_header(raptor_serializer* serializer,
                                   raptor_rdfxml_serializer_context* context)
{
  raptor_xml_writer* xml_writer;
  raptor_uri *base_uri;
  int i;
  raptor_qname **attrs=NULL;
  int attrs_count=0;
  int rc=1;

  if(context->written_header)
    return 0;

  context->written_header=1;

  xml_writer=context->xml_writer;

  base_uri=serializer->base_uri;
  if(base_uri)
    base_uri=raptor_uri_copy(base_uri);

  context->rdf_RDF_element=raptor_new_xml_element_from_namespace_local_name(context->rdf_nspace, (const unsigned char*)"RDF", NULL, base_uri);
  if(!context->rdf_RDF_element)
    goto tidy;

  /* NOTE: Starts it item 1 as item 0 is the element's namespace (rdf)
   * and does not need to be declared
   */
  for(i=1; i< raptor_sequence_size(context->namespaces); i++) {
    raptor_namespace* ns=(raptor_namespace*)raptor_sequence_get_at(context->namespaces, i);
    if(raptor_xml_element_declare_namespace(context->rdf_RDF_element, ns))
      goto tidy;
  }

  if(base_uri) {
    const unsigned char* base_uri_string;

    attrs=(raptor_qname **)RAPTOR_CALLOC(qnamearray, 1, sizeof(raptor_qname*));
    if(!attrs)
      goto tidy;

    base_uri_string=raptor_uri_as_string(base_uri);
    attrs[attrs_count]=raptor_new_qname_from_namespace_local_name(context->xml_nspace, (const unsigned char*)"base",  base_uri_string);
    if(!attrs[attrs_count]) {
      RAPTOR_FREE(qnamearray, attrs);
      goto tidy;
    }
    attrs_count++;
  }

  if(attrs_count)
    raptor_xml_element_set_attributes(context->rdf_RDF_element, attrs,
                                      attrs_count);
  else
    raptor_xml_element_set_attributes(context->rdf_RDF_element, NULL, 0);


  raptor_xml_writer_start_element(xml_writer, context->rdf_RDF_element);
  raptor_xml_writer_raw_counted(xml_writer, (const unsigned char*)"\n", 1);

  rc=0;

  tidy:
  if(base_uri)
    raptor_free_uri(base_uri);

  return rc;
}


/* serialize a statement */
static int
raptor_rdfxml_serialize_statement(raptor_serializer* serializer,
                                  const raptor_statement *statement)
{
  raptor_rdfxml_serializer_context* context=(raptor_rdfxml_serializer_context*)serializer->context;
  raptor_xml_writer* xml_writer=context->xml_writer;
  unsigned char* uri_string=NULL; /* predicate URI */
  unsigned char ordinal_name[20];
  unsigned char* name=NULL;  /* where to split predicate name */
  unsigned char* subject_uri_string=NULL;
  unsigned char* object_uri_string=NULL;
  const unsigned char* nsprefix=(const unsigned char*)"ns0";
  int rc=1;
  size_t len;
  raptor_xml_element* rdf_Description_element=NULL;
  raptor_uri* predicate_ns_uri=NULL;
  raptor_namespace* predicate_ns=NULL;
  int free_predicate_ns=0;
  raptor_xml_element* predicate_element=NULL;
  int end_predicate_element=0;
  raptor_qname **attrs=NULL;
  int attrs_count=0;
  raptor_uri* base_uri=NULL;
  raptor_identifier_type object_type;
  int allocated=1;

  if(raptor_rdfxml_ensure_writen_header(serializer, context))
    return 1;

  if(statement->predicate_type == RAPTOR_IDENTIFIER_TYPE_ORDINAL) {
    predicate_ns=context->rdf_nspace;
    sprintf((char*)ordinal_name, "_%d", *((int*)statement->predicate));
    name=ordinal_name;
  } else if((statement->predicate_type == RAPTOR_IDENTIFIER_TYPE_PREDICATE) ||
     (statement->predicate_type == RAPTOR_IDENTIFIER_TYPE_RESOURCE)) {
    unsigned char *p;
    size_t uri_len;
    size_t name_len=1;
    unsigned char c;

    /* Do not use raptor_uri_as_counted_string() - we want a modifiable copy */
    uri_string=raptor_uri_to_counted_string((raptor_uri*)statement->predicate,
                                            &uri_len);
    if(!uri_string)
      goto oom;

    p= uri_string;
    name_len=uri_len;
    /* FIXME: this loop could be made smarter */
    while(name_len >0) {
      if(raptor_xml_name_check(p, name_len, 10)) {
        name=p;
        break;
      }
      p++; name_len--;
    }

    if(!name || (name == uri_string)) {
      raptor_serializer_error(serializer, "Cannot split predicate URI %s into an XML qname - skipping statement", uri_string);
      rc=0; /* skip but do not return an error */
      goto tidy;
    }

    c=*name; *name='\0';
    predicate_ns_uri=raptor_new_uri(uri_string);
    if(!predicate_ns_uri)
      goto oom;
    *name=c;

    predicate_ns=raptor_namespaces_find_namespace_by_uri(context->nstack,
                                                         predicate_ns_uri);
    if(!predicate_ns) {
      predicate_ns=raptor_new_namespace_from_uri(context->nstack,
                                                 nsprefix,
                                                 predicate_ns_uri, 0);
      if(!predicate_ns) {
        raptor_free_uri(predicate_ns_uri);
        goto oom;
      }
      free_predicate_ns=1;
    }
    raptor_free_uri(predicate_ns_uri);
  } else {
    raptor_serializer_error(serializer, "Cannot serialize a triple with subject node type %d\n", statement->predicate_type);
    goto tidy;
  }

  /* base uri */
  if(serializer->base_uri)
    base_uri=raptor_uri_copy(serializer->base_uri);


  rdf_Description_element=raptor_new_xml_element_from_namespace_local_name(context->rdf_nspace,
                                                                           (unsigned const char*)"Description",
                                                                           NULL, base_uri);
  if(!rdf_Description_element)
    goto oom;

  attrs=(raptor_qname **)RAPTOR_CALLOC(qnamearray, 3, sizeof(raptor_qname*));
  if(!attrs)
    goto oom;
  attrs_count=0;

  /* subject */
  switch(statement->subject_type) {
    case RAPTOR_IDENTIFIER_TYPE_ANONYMOUS:
      attrs[attrs_count]=raptor_new_qname_from_namespace_local_name(context->rdf_nspace, (const unsigned char*)"nodeID",  (unsigned char*)statement->subject);
      if(!attrs[attrs_count])
        goto oom;
      attrs_count++;
      break;

    case RAPTOR_IDENTIFIER_TYPE_RESOURCE:
    case RAPTOR_IDENTIFIER_TYPE_ORDINAL:
      allocated=1;
      if(statement->subject_type == RAPTOR_IDENTIFIER_TYPE_ORDINAL) {
        subject_uri_string=(unsigned char*)RAPTOR_MALLOC(cstring, raptor_rdf_namespace_uri_len+13);
        if(!subject_uri_string)
          goto oom;
        sprintf((char*)subject_uri_string, "%s_%d",
                raptor_rdf_namespace_uri, *((int*)statement->subject));
      } else {
        if(serializer->feature_relative_uris) {
          subject_uri_string=raptor_uri_to_relative_uri_string(serializer->base_uri,
                                                              (raptor_uri*)statement->subject);
          if(!subject_uri_string)
            goto oom;
        } else {
          subject_uri_string=raptor_uri_as_string((raptor_uri*)statement->subject);
          allocated=0;
        }
      }

      attrs[attrs_count]=raptor_new_qname_from_namespace_local_name(context->rdf_nspace, (const unsigned char*)"about",  subject_uri_string);
      if(!attrs[attrs_count]) {
        if(allocated)
          RAPTOR_FREE(cstring, subject_uri_string);
        goto oom;
      }
      attrs_count++;

      if(allocated)
        RAPTOR_FREE(cstring, subject_uri_string);

      break;

    case RAPTOR_IDENTIFIER_TYPE_LITERAL:
    case RAPTOR_IDENTIFIER_TYPE_XML_LITERAL:
      raptor_serializer_error(serializer, "Cannot serialize a triple with a literal subject\n");
      break;

    case RAPTOR_IDENTIFIER_TYPE_PREDICATE:
    case RAPTOR_IDENTIFIER_TYPE_UNKNOWN:
    default:
      raptor_serializer_error(serializer, "Cannot serialize a triple with subject node type %d\n", statement->subject_type);
  }

  if(attrs_count) {
    raptor_xml_element_set_attributes(rdf_Description_element, attrs, attrs_count);
    attrs=NULL; /* attrs ownership transferred to element */
  }

  raptor_xml_writer_cdata_counted(xml_writer, (const unsigned char*)"  ", 2);
  raptor_xml_writer_start_element(xml_writer, rdf_Description_element);
  raptor_xml_writer_cdata_counted(xml_writer, (const unsigned char*)"\n", 1);


  /* predicate */
  predicate_element=raptor_new_xml_element_from_namespace_local_name(predicate_ns, name, NULL, base_uri);
  if(!predicate_element)
    goto oom;

  /* object */
  attrs=(raptor_qname **)RAPTOR_CALLOC(qnamearray, 3, sizeof(raptor_qname*));
  if(!attrs)
    goto oom;
  attrs_count=0;

  object_type=statement->object_type;
  switch(object_type) {
    case RAPTOR_IDENTIFIER_TYPE_LITERAL:
      if(statement->object_literal_datatype &&
         raptor_uri_equals(statement->object_literal_datatype,
                           context->rdf_xml_literal_uri))
        object_type = RAPTOR_IDENTIFIER_TYPE_XML_LITERAL;

      /* FALLTHROUGH */
    case RAPTOR_IDENTIFIER_TYPE_XML_LITERAL:

      if(statement->object_literal_language) {
        attrs[attrs_count]=raptor_new_qname(context->nstack,
                                            (unsigned char*)"xml:lang",
                                            (unsigned char*)statement->object_literal_language,
                                            (raptor_simple_message_handler)raptor_serializer_simple_error,
                                            serializer);
        if(!attrs[attrs_count])
          goto oom;
        attrs_count++;
      }
      len=strlen((const char*)statement->object);

      if(object_type == RAPTOR_IDENTIFIER_TYPE_XML_LITERAL) {
        attrs[attrs_count]=raptor_new_qname_from_namespace_local_name(context->rdf_nspace, (const unsigned char*)"parseType", (const unsigned char*)"Literal");
        if(!attrs[attrs_count])
          goto oom;
        attrs_count++;

        raptor_xml_element_set_attributes(predicate_element, attrs, attrs_count);
        attrs=NULL; /* attrs ownership transferred to element */

        raptor_xml_writer_cdata_counted(xml_writer, (const unsigned char*)"    ", 4);
        raptor_xml_writer_start_element(xml_writer, predicate_element);
        end_predicate_element=1;

        /* Print without escaping XML */
        if(len)
          raptor_xml_writer_raw_counted(xml_writer,
                                        (const unsigned char*)statement->object,
                                        len);
      } else {
        if(statement->object_literal_datatype) {
          attrs[attrs_count]=raptor_new_qname_from_namespace_local_name(context->rdf_nspace, (const unsigned char*)"datatype", (unsigned char*)raptor_uri_as_string((raptor_uri*)statement->object_literal_datatype));
          if(!attrs[attrs_count])
            goto oom;
          attrs_count++;
        }
        raptor_xml_element_set_attributes(predicate_element, attrs, attrs_count);
        attrs=NULL; /* attrs ownership transferred to element */

        raptor_xml_writer_cdata_counted(xml_writer,
                                        (const unsigned char*)"    ", 4);
        raptor_xml_writer_start_element(xml_writer, predicate_element);
        end_predicate_element=1;

        if(len)
          raptor_xml_writer_cdata_counted(xml_writer,
                                          (const unsigned char*)statement->object, len);
      }

      raptor_xml_writer_end_element(xml_writer, predicate_element);
      end_predicate_element=0;
      raptor_free_xml_element(predicate_element);
      predicate_element=NULL;
      raptor_xml_writer_cdata_counted(xml_writer, (const unsigned char*)"\n", 1);

      break;

    case RAPTOR_IDENTIFIER_TYPE_ANONYMOUS:
      attrs[attrs_count]=raptor_new_qname_from_namespace_local_name(context->rdf_nspace, (const unsigned char*)"nodeID", (unsigned char*)statement->object);
      if(!attrs[attrs_count])
        goto oom;
      attrs_count++;

      raptor_xml_element_set_attributes(predicate_element, attrs, attrs_count);
      attrs=NULL; /* attrs ownership transferred to element */

      raptor_xml_writer_cdata_counted(xml_writer,
                                      (const unsigned char*)"    ", 4);
      raptor_xml_writer_empty_element(xml_writer, predicate_element);
      raptor_xml_writer_cdata_counted(xml_writer, (const unsigned char*)"\n", 1);
      break;

    case RAPTOR_IDENTIFIER_TYPE_RESOURCE:
    case RAPTOR_IDENTIFIER_TYPE_ORDINAL:
      allocated=1;
      if(object_type == RAPTOR_IDENTIFIER_TYPE_ORDINAL) {
        object_uri_string=(unsigned char*)RAPTOR_MALLOC(cstring, raptor_rdf_namespace_uri_len+13);
        if(!object_uri_string)
          goto oom;
        sprintf((char*)object_uri_string, "%s_%d",
                raptor_rdf_namespace_uri, *((int*)statement->object));
      } else {
        /* must be URI */
        if(serializer->feature_relative_uris) {
          object_uri_string=raptor_uri_to_relative_uri_string(serializer->base_uri,
                                                              (raptor_uri*)statement->object);
          if(!object_uri_string)
            goto oom;
        } else {
          object_uri_string=raptor_uri_to_string((raptor_uri*)statement->object);
          allocated=0;
        }
      }

      attrs[attrs_count]=raptor_new_qname_from_namespace_local_name(context->rdf_nspace, (const unsigned char*)"resource", object_uri_string);
      if(!attrs[attrs_count]) {
        if(allocated)
          RAPTOR_FREE(cstring, object_uri_string);
        goto oom;
      }
      attrs_count++;

      if(allocated)
        RAPTOR_FREE(cstring, object_uri_string);

      raptor_xml_element_set_attributes(predicate_element, attrs, 1);
      attrs=NULL; /* attrs ownership transferred to element */

      raptor_xml_writer_cdata_counted(xml_writer,
                                      (const unsigned char*)"    ", 4);
      raptor_xml_writer_empty_element(xml_writer, predicate_element);
      raptor_xml_writer_cdata_counted(xml_writer, (const unsigned char*)"\n", 1);
      break;

    case RAPTOR_IDENTIFIER_TYPE_PREDICATE:
    case RAPTOR_IDENTIFIER_TYPE_UNKNOWN:
    default:
      raptor_serializer_error(serializer, "Cannot serialize a triple with object node type %d\n", object_type);
  }

  raptor_xml_writer_cdata_counted(xml_writer,
                                  (const unsigned char*)"  ", 2);

  rc=0; /* success */
  goto tidy;

  oom:
  raptor_serializer_error(serializer, "Out of memory\n");

  tidy:

  if(attrs)
    RAPTOR_FREE(qnamearray, attrs);

  if(predicate_element) {
    if(end_predicate_element)
      raptor_xml_writer_end_element(xml_writer, predicate_element);
    raptor_free_xml_element(predicate_element);
  }

  if(rdf_Description_element) {
    raptor_xml_writer_end_element(xml_writer, rdf_Description_element);
    raptor_xml_writer_cdata_counted(xml_writer, (const unsigned char*)"\n", 1);
    raptor_free_xml_element(rdf_Description_element);
  }

  if(base_uri)
    raptor_free_uri(base_uri);

  if(free_predicate_ns)
    raptor_free_namespace(predicate_ns);

  if(uri_string)
    RAPTOR_FREE(cstring, uri_string);

  return rc;
}


/* end a serialize */
static int
raptor_rdfxml_serialize_end(raptor_serializer* serializer)
{
  raptor_rdfxml_serializer_context* context=(raptor_rdfxml_serializer_context*)serializer->context;
  raptor_xml_writer* xml_writer=context->xml_writer;

  if(xml_writer) {
    /* Make sure an empty RDF/XML document is written when 0 triples
     * were seen
     */

    /* ignore ret value */
    raptor_rdfxml_ensure_writen_header(serializer, context);

    if(context->rdf_RDF_element) {
      raptor_xml_writer_end_element(xml_writer, context->rdf_RDF_element);
      raptor_xml_writer_raw_counted(xml_writer, (const unsigned char*)"\n", 1);
    }
  }

  if(context->rdf_RDF_element) {
    raptor_free_xml_element(context->rdf_RDF_element);
    context->rdf_RDF_element=NULL;
  }

  return 0;
}


/* finish the serializer factory */
static void
raptor_rdfxml_serialize_finish_factory(raptor_serializer_factory* factory)
{

}

static int
raptor_rdfxml_serializer_register_factory(raptor_serializer_factory *factory)
{
  factory->context_length     = sizeof(raptor_rdfxml_serializer_context);

  factory->init                = raptor_rdfxml_serialize_init;
  factory->terminate           = raptor_rdfxml_serialize_terminate;
  factory->declare_namespace   = raptor_rdfxml_serialize_declare_namespace;
  factory->declare_namespace_from_namespace   = raptor_rdfxml_serialize_declare_namespace_from_namespace;
  factory->serialize_start     = raptor_rdfxml_serialize_start;
  factory->serialize_statement = raptor_rdfxml_serialize_statement;
  factory->serialize_end       = raptor_rdfxml_serialize_end;
  factory->finish_factory      = raptor_rdfxml_serialize_finish_factory;

  return 0;
}



int
raptor_init_serializer_rdfxml(void) {
  return raptor_serializer_register_factory("rdfxml", "RDF/XML",
                                            "application/rdf+xml",
                                            NULL,
                                            (const unsigned char*)"http://www.w3.org/TR/rdf-syntax-grammar",
                                            &raptor_rdfxml_serializer_register_factory);
}


