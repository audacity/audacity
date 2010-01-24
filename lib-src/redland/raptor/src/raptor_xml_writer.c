/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_xml_writer.c - Raptor XML Writer for SAX2 events API
 *
 * Copyright (C) 2003-2008, David Beckett http://www.dajobe.org/
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

#ifndef STANDALONE


typedef enum {
  XML_WRITER_AUTO_INDENT = 1,
  XML_WRITER_AUTO_EMPTY  = 2
} raptor_xml_writer_flags;


#define XML_WRITER_AUTO_INDENT(xml_writer) ((xml_writer->flags & XML_WRITER_AUTO_INDENT) != 0)
#define XML_WRITER_AUTO_EMPTY(xml_writer) ((xml_writer->flags & XML_WRITER_AUTO_EMPTY) != 0)

#define XML_WRITER_FLUSH_CLOSE_BRACKET(xml_writer)              \
  if ((xml_writer->flags & XML_WRITER_AUTO_EMPTY) &&            \
      xml_writer->current_element &&                            \
      !(xml_writer->current_element->content_cdata_seen ||      \
        xml_writer->current_element->content_element_seen)) {   \
    raptor_iostream_write_byte(xml_writer->iostr, '>');         \
  }


/* Define this for far too much output */
#undef RAPTOR_DEBUG_CDATA


struct raptor_xml_writer_s {
  int canonicalize;

  int depth;
  
  int my_nstack;
  raptor_namespace_stack *nstack;
  int nstack_depth;

  const raptor_uri_handler *uri_handler;
  void *uri_context;

  raptor_simple_message_handler error_handler;
  void *error_data;

  raptor_xml_element* current_element;

  /* outputting to this iostream */
  raptor_iostream *iostr;

  /* XML Writer flags - bits defined in enum raptor_xml_writer_flags */
  int flags;

  /* indentation per level if formatting */
  int indent;

  /* XML 1.0 (10) or XML 1.1 (11) */
  int xml_version;

  /* Write XML 1.0 or 1.1 declaration (default 1) */
  int xml_declaration;

  /* Has writing the XML declaration writing been checked? */
  int xml_declaration_checked;

  /* An extra newline is wanted */
  int pending_newline;
};


/* 16 spaces */
#define SPACES_BUFFER_SIZE sizeof(spaces_buffer)
static const unsigned char spaces_buffer[] = {
  ' ', ' ', ' ', ' ',
  ' ', ' ', ' ', ' ',
  ' ', ' ', ' ', ' ',
  ' ', ' ', ' ', ' '
};



/* helper functions */

/* Handle printing a pending newline OR newline with indenting */
static int
raptor_xml_writer_indent(raptor_xml_writer *xml_writer)
{
  int num_spaces;

  if(!XML_WRITER_AUTO_INDENT(xml_writer)) {
    if(xml_writer->pending_newline) {
      raptor_iostream_write_byte(xml_writer->iostr, '\n');
      xml_writer->pending_newline=0;

      if(xml_writer->current_element)
        xml_writer->current_element->content_cdata_seen=1;
    }
    return 0;
  }
  
  num_spaces = xml_writer->depth * xml_writer->indent;

  /* Do not write an extra newline at the start of the document
   * (after the XML declaration or XMP processing instruction has
   * been writtten)
   */
  if(xml_writer->xml_declaration_checked == 1)
    xml_writer->xml_declaration_checked++;
  else {
    raptor_iostream_write_byte(xml_writer->iostr, '\n');
    xml_writer->pending_newline=0;
  }
  
  while (num_spaces > 0) {

    int count = (num_spaces > (int)SPACES_BUFFER_SIZE) ? (int)SPACES_BUFFER_SIZE : num_spaces;

    raptor_iostream_write_counted_string(xml_writer->iostr, spaces_buffer, count);

    num_spaces -= count;
  }

  if(xml_writer->current_element)
    xml_writer->current_element->content_cdata_seen=1;

  return 0;
}


struct nsd {
  const raptor_namespace *nspace;
  unsigned char *declaration;
  size_t length;
};


/*
 * FIXME: This is duplicate code taken from raptor_sax2.c:
 *   struct nsd
 *  raptor_xml_writer_nsd_compare (from raptor_nsd_compare)
 */

static int
raptor_xml_writer_nsd_compare(const void *a, const void *b)
{
  struct nsd* nsd_a=(struct nsd*)a;
  struct nsd* nsd_b=(struct nsd*)b;
  return strcmp((const char*)nsd_a->declaration, (const char*)nsd_b->declaration);
}


static int
raptor_xml_writer_start_element_common(raptor_xml_writer* xml_writer,
                                       raptor_xml_element* element,
                                       int auto_empty)
{
  raptor_iostream* iostr=xml_writer->iostr;
  raptor_namespace_stack *nstack=xml_writer->nstack;
  raptor_simple_message_handler error_handler=xml_writer->error_handler;
  void *error_data=xml_writer->error_data;
  int depth=xml_writer->depth;
  int auto_indent=XML_WRITER_AUTO_INDENT(xml_writer);
  int xml_version=xml_writer->xml_version;
  struct nsd *nspace_declarations=NULL;
  size_t nspace_declarations_count=0;  
  unsigned int i;

  /* max is 1 per element and 1 for each attribute + size of declared */
  if(nstack) {
    int nspace_max_count=element->attribute_count+1;
    if(element->declared_nspaces)
      nspace_max_count += raptor_sequence_size(element->declared_nspaces);
    
    nspace_declarations=(struct nsd*)RAPTOR_CALLOC(nsdarray, nspace_max_count, sizeof(struct nsd));
    if(!nspace_declarations)
      return 1;
  }

  if(element->name->nspace) {
    if(nstack && !raptor_namespaces_namespace_in_scope(nstack, element->name->nspace)) {
      nspace_declarations[0].declaration=
        raptor_namespaces_format(element->name->nspace,
                                 &nspace_declarations[0].length);
      if(!nspace_declarations[0].declaration)
        goto error;
      nspace_declarations[0].nspace=element->name->nspace;
      nspace_declarations_count++;
    }
  }

  if (element->attributes) {
    for(i=0; i < element->attribute_count; i++) {
      /* qname */
      if(element->attributes[i]->nspace) {
        if(nstack && 
           !raptor_namespaces_namespace_in_scope(nstack, element->attributes[i]->nspace) && element->attributes[i]->nspace != element->name->nspace) {
          /* not in scope and not same as element (so already going to be declared)*/
          unsigned int j;
          int declare_me=1;
          
          /* check it wasn't an earlier declaration too */
          for (j=0; j < nspace_declarations_count; j++)
            if(nspace_declarations[j].nspace == element->attributes[j]->nspace) {
              declare_me=0;
              break;
            }
            
          if(declare_me) {
            nspace_declarations[nspace_declarations_count].declaration=
              raptor_namespaces_format(element->attributes[i]->nspace,
                                       &nspace_declarations[nspace_declarations_count].length);
            if(!nspace_declarations[nspace_declarations_count].declaration)
              goto error;
            nspace_declarations[nspace_declarations_count].nspace=element->attributes[i]->nspace;
            nspace_declarations_count++;
          }
        }
      }
    }
  }

  if(nstack && element->declared_nspaces &&
     raptor_sequence_size(element->declared_nspaces) > 0) {
    for(i=0; i< (unsigned int)raptor_sequence_size(element->declared_nspaces); i++) {
      raptor_namespace* nspace=(raptor_namespace*)raptor_sequence_get_at(element->declared_nspaces, i);
      unsigned int j;
      int declare_me=1;
      
      /* check it wasn't an earlier declaration too */
      for (j=0; j < nspace_declarations_count; j++)
        if(nspace_declarations[j].nspace == nspace) {
          declare_me=0;
          break;
        }
      
      if(declare_me) {
        nspace_declarations[nspace_declarations_count].declaration=
          raptor_namespaces_format(nspace,
                                   &nspace_declarations[nspace_declarations_count].length);
        if(!nspace_declarations[nspace_declarations_count].declaration)
          goto error;
        nspace_declarations[nspace_declarations_count].nspace=nspace;
        nspace_declarations_count++;
      }

    }
  }

  raptor_iostream_write_byte(iostr, '<');

  if(element->name->nspace && element->name->nspace->prefix_length > 0) {
    raptor_iostream_write_counted_string(iostr, 
                                         (const char*)element->name->nspace->prefix, 
                                         element->name->nspace->prefix_length);
    raptor_iostream_write_byte(iostr, ':');
  }
  raptor_iostream_write_counted_string(iostr, 
                                       (const char*)element->name->local_name,
                                       element->name->local_name_length);

  /* declare namespaces */
  if(nspace_declarations_count) {
    /* sort them into the canonical order */
    qsort((void*)nspace_declarations, 
          nspace_declarations_count, sizeof(struct nsd),
          raptor_xml_writer_nsd_compare);
    /* add them */
    for (i=0; i < nspace_declarations_count; i++) {
      if(auto_indent && nspace_declarations_count > 1) {
        /* indent xmlns namespace attributes */
        raptor_xml_writer_newline(xml_writer);
        xml_writer->depth++;
        raptor_xml_writer_indent(xml_writer);
        xml_writer->depth--;
      }
      raptor_iostream_write_byte(iostr, ' ');
      raptor_iostream_write_counted_string(iostr, 
                                           (const char*)nspace_declarations[i].declaration,
                                           nspace_declarations[i].length);
      RAPTOR_FREE(cstring, nspace_declarations[i].declaration);
      nspace_declarations[i].declaration=NULL;

      if(raptor_namespace_copy(nstack,
                               (raptor_namespace*)nspace_declarations[i].nspace,
                               depth))
        goto error;
    }
  }


  if(element->attributes) {
    for(i=0; i < element->attribute_count; i++) {
      raptor_iostream_write_byte(iostr, ' ');
      
      if(element->attributes[i]->nspace && 
         element->attributes[i]->nspace->prefix_length > 0) {
        raptor_iostream_write_counted_string(iostr,
                                             (char*)element->attributes[i]->nspace->prefix,
                                             element->attributes[i]->nspace->prefix_length);
        raptor_iostream_write_byte(iostr, ':');
      }

      raptor_iostream_write_counted_string(iostr, 
                                           (const char*)element->attributes[i]->local_name,
                                           element->attributes[i]->local_name_length);
      
      raptor_iostream_write_counted_string(iostr, "=\"", 2);
      
      raptor_iostream_write_xml_any_escaped_string(iostr,
                                                   element->attributes[i]->value, 
                                                   element->attributes[i]->value_length,
                                                   '"',
                                                   xml_version,
                                                   error_handler, error_data);
      raptor_iostream_write_byte(iostr, '"');
    }
  }

  if (!auto_empty)
    raptor_iostream_write_byte(iostr, '>');

  if(nstack)
    RAPTOR_FREE(stringarray, nspace_declarations);

  return 0;

  /* Clean up nspace_declarations on error */
  error:

  for (i=0; i < nspace_declarations_count; i++) {
    if(nspace_declarations[i].declaration)
      RAPTOR_FREE(cstring, nspace_declarations[i].declaration);
  }

  if(nspace_declarations)
    RAPTOR_FREE(stringarray, nspace_declarations);

  return 1;
}


static int
raptor_xml_writer_end_element_common(raptor_xml_writer* xml_writer,
                                     raptor_xml_element *element,
                                     int is_empty)
{
  raptor_iostream* iostr=xml_writer->iostr;

  if (is_empty)
    raptor_iostream_write_byte(iostr, '/');
  else {
    
    raptor_iostream_write_byte(iostr, '<');

    raptor_iostream_write_byte(iostr, '/');

    if(element->name->nspace && element->name->nspace->prefix_length > 0) {
      raptor_iostream_write_counted_string(iostr, 
                                           (const char*)element->name->nspace->prefix, 
                                           element->name->nspace->prefix_length);
      raptor_iostream_write_byte(iostr, ':');
    }
    raptor_iostream_write_counted_string(iostr, 
                                         (const char*)element->name->local_name,
                                         element->name->local_name_length);
  }
  
  raptor_iostream_write_byte(iostr, '>');

  return 0;
  
}


/**
 * raptor_new_xml_writer:
 * @nstack: Namespace stack for the writer to start with (or NULL)
 * @uri_handler: URI handler function
 * @uri_context: URI handler context data
 * @iostr: I/O stream to write to
 * @error_handler: error handler function
 * @error_data: error handler data
 * @canonicalize: unused
 * 
 * Constructor - Create a new XML Writer writing XML to a raptor_iostream
 * 
 * Return value: a new #raptor_xml_writer object or NULL on failure
 **/
raptor_xml_writer*
raptor_new_xml_writer(raptor_namespace_stack *nstack,
                      const raptor_uri_handler *uri_handler,
                      void *uri_context,
                      raptor_iostream* iostr,
                      raptor_simple_message_handler error_handler,
                      void *error_data,
                      int canonicalize)
{
  raptor_xml_writer* xml_writer;
  
  xml_writer=(raptor_xml_writer*)RAPTOR_CALLOC(raptor_xml_writer, 1, sizeof(raptor_xml_writer)+1);
  if(!xml_writer)
    return NULL;

  xml_writer->nstack_depth=0;

  xml_writer->uri_handler=uri_handler;
  xml_writer->uri_context=uri_context;

  xml_writer->error_handler=error_handler;
  xml_writer->error_data=error_data;

  xml_writer->nstack=nstack;
  if(!xml_writer->nstack) {
    xml_writer->nstack=nstack=raptor_new_namespaces(uri_handler, uri_context,
                                                    error_handler, error_data,
                                                    1);
    xml_writer->my_nstack=1;
  }

  xml_writer->iostr=iostr;

  xml_writer->flags = 0;
  xml_writer->indent = 2;
  
  xml_writer->xml_version = 10;

  /* Write XML declaration */
  xml_writer->xml_declaration=1;
  
  return xml_writer;
}


/**
 * raptor_free_xml_writer:
 * @xml_writer: XML writer object
 *
 * Destructor - Free XML Writer
 * 
 **/
void
raptor_free_xml_writer(raptor_xml_writer* xml_writer)
{
  RAPTOR_ASSERT_OBJECT_POINTER_RETURN(xml_writer, raptor_xml_writer);

  if(xml_writer->nstack && xml_writer->my_nstack)
    raptor_free_namespaces(xml_writer->nstack);

  RAPTOR_FREE(raptor_xml_writer, xml_writer);
}


static void
raptor_xml_writer_write_xml_declaration(raptor_xml_writer* xml_writer)
{
  if(!xml_writer->xml_declaration_checked) {
    /* check that it should be written once only */
    xml_writer->xml_declaration_checked=1;

    if(xml_writer->xml_declaration) {
      raptor_iostream_write_string(xml_writer->iostr, 
                                   (const unsigned char*)"<?xml version=\"");
      raptor_iostream_write_counted_string(xml_writer->iostr, 
                                           (xml_writer->xml_version == 10) ?
                                           (const unsigned char*)"1.0" :
                                           (const unsigned char*)"1.1",
                                           3);
      raptor_iostream_write_string(xml_writer->iostr, 
                                   (const unsigned char*)"\" encoding=\"utf-8\"?>\n");
    }
  }

}


/**
 * raptor_xml_writer_empty_element:
 * @xml_writer: XML writer object
 * @element: XML element object
 *
 * Write an empty XML element to the XML writer.
 * 
 * Closes any previous empty element if XML writer feature AUTO_EMPTY
 * is enabled.
 **/
void
raptor_xml_writer_empty_element(raptor_xml_writer* xml_writer,
                                raptor_xml_element *element)
{
  raptor_xml_writer_write_xml_declaration(xml_writer);

  XML_WRITER_FLUSH_CLOSE_BRACKET(xml_writer);
  
  if(xml_writer->pending_newline || XML_WRITER_AUTO_INDENT(xml_writer))
    raptor_xml_writer_indent(xml_writer);
  
  raptor_xml_writer_start_element_common(xml_writer, element, 1);

  raptor_xml_writer_end_element_common(xml_writer, element, 1);
  
  raptor_namespaces_end_for_depth(xml_writer->nstack, xml_writer->depth);
}


/**
 * raptor_xml_writer_start_element:
 * @xml_writer: XML writer object
 * @element: XML element object
 *
 * Write a start XML element to the XML writer.
 *
 * Closes any previous empty element if XML writer feature AUTO_EMPTY
 * is enabled.
 *
 * Indents the start element if XML writer feature AUTO_INDENT is enabled.
 **/
void
raptor_xml_writer_start_element(raptor_xml_writer* xml_writer,
                                raptor_xml_element *element)
{
  raptor_xml_writer_write_xml_declaration(xml_writer);

  XML_WRITER_FLUSH_CLOSE_BRACKET(xml_writer);
  
  if(xml_writer->pending_newline || XML_WRITER_AUTO_INDENT(xml_writer))
    raptor_xml_writer_indent(xml_writer);
  
  raptor_xml_writer_start_element_common(xml_writer, element,
                                         XML_WRITER_AUTO_EMPTY(xml_writer));

  xml_writer->depth++;

  /* SJS Note: This "if" clause is necessary because raptor_rdfxml.c
   * uses xml_writer for parseType="literal" and passes in elements
   * whose parent field is already set. The first time this function
   * is called, it sets element->parent to 0, causing the warn-07.rdf
   * test to fail. Subsequent calls to this function set
   * element->parent to its existing value. 
   */
  if(xml_writer->current_element)
    element->parent = xml_writer->current_element;
  
  xml_writer->current_element=element;
  if(element && element->parent)
    element->parent->content_element_seen=1;
}


/**
 * raptor_xml_writer_end_element:
 * @xml_writer: XML writer object
 * @element: XML element object
 *
 * Write an end XML element to the XML writer.
 *
 * Indents the end element if XML writer feature AUTO_INDENT is enabled.
 **/
void
raptor_xml_writer_end_element(raptor_xml_writer* xml_writer,
                              raptor_xml_element* element)
{
  int is_empty;

  xml_writer->depth--;
  
  if(xml_writer->pending_newline ||
     (XML_WRITER_AUTO_INDENT(xml_writer) && element->content_element_seen))
    raptor_xml_writer_indent(xml_writer);

  is_empty = XML_WRITER_AUTO_EMPTY(xml_writer) ?
    !(element->content_cdata_seen || element->content_element_seen) : 0;
  
  raptor_xml_writer_end_element_common(xml_writer, element, is_empty);
  
  raptor_namespaces_end_for_depth(xml_writer->nstack, xml_writer->depth);

  if(xml_writer->current_element)
    xml_writer->current_element = xml_writer->current_element->parent;
}


/**
 * raptor_xml_writer_newline:
 * @xml_writer: XML writer object
 *
 * Write a newline to the XML writer.
 *
 * Indents the next line if XML writer feature AUTO_INDENT is enabled.
 **/
void
raptor_xml_writer_newline(raptor_xml_writer* xml_writer)
{
  xml_writer->pending_newline=1;
}


/**
 * raptor_xml_writer_cdata:
 * @xml_writer: XML writer object
 * @s: string to XML escape and write
 *
 * Write CDATA XML-escaped to the XML writer.
 *
 * Closes any previous empty element if XML writer feature AUTO_EMPTY
 * is enabled.
 *
 **/
void
raptor_xml_writer_cdata(raptor_xml_writer* xml_writer,
                        const unsigned char *s)
{
  raptor_xml_writer_write_xml_declaration(xml_writer);

  XML_WRITER_FLUSH_CLOSE_BRACKET(xml_writer);
  
  raptor_iostream_write_xml_any_escaped_string(xml_writer->iostr,
                                               s, strlen((const char*)s),
                                               '\0',
                                               xml_writer->xml_version,
                                               xml_writer->error_handler,
                                               xml_writer->error_data);

  if(xml_writer->current_element)
    xml_writer->current_element->content_cdata_seen=1;
}


/**
 * raptor_xml_writer_cdata_counted:
 * @xml_writer: XML writer object
 * @s: string to XML escape and write
 * @len: length of string
 *
 * Write counted CDATA XML-escaped to the XML writer.
 *
 * Closes any previous empty element if XML writer feature AUTO_EMPTY
 * is enabled.
 *
 **/
void
raptor_xml_writer_cdata_counted(raptor_xml_writer* xml_writer,
                                const unsigned char *s, unsigned int len)
{
  raptor_xml_writer_write_xml_declaration(xml_writer);

  XML_WRITER_FLUSH_CLOSE_BRACKET(xml_writer);
  
  raptor_iostream_write_xml_any_escaped_string(xml_writer->iostr,
                                               s, len,
                                               '\0',
                                               xml_writer->xml_version,
                                               xml_writer->error_handler,
                                               xml_writer->error_data);

  if(xml_writer->current_element)
    xml_writer->current_element->content_cdata_seen=1;
}


/**
 * raptor_xml_writer_raw:
 * @xml_writer: XML writer object
 * @s: string to write
 *
 * Write a string raw to the XML writer.
 *
 * Closes any previous empty element if XML writer feature AUTO_EMPTY
 * is enabled.
 *
 **/
void
raptor_xml_writer_raw(raptor_xml_writer* xml_writer,
                      const unsigned char *s)
{
  raptor_xml_writer_write_xml_declaration(xml_writer);

  XML_WRITER_FLUSH_CLOSE_BRACKET(xml_writer);
  
  raptor_iostream_write_string(xml_writer->iostr, s);

  if(xml_writer->current_element)
    xml_writer->current_element->content_cdata_seen=1;
}


/**
 * raptor_xml_writer_raw_counted:
 * @xml_writer: XML writer object
 * @s: string to write
 * @len: length of string
 *
 * Write a counted string raw to the XML writer.
 *
 * Closes any previous empty element if XML writer feature AUTO_EMPTY
 * is enabled.
 *
 **/
void
raptor_xml_writer_raw_counted(raptor_xml_writer* xml_writer,
                              const unsigned char *s, unsigned int len)
{
  raptor_xml_writer_write_xml_declaration(xml_writer);

  XML_WRITER_FLUSH_CLOSE_BRACKET(xml_writer);
  
  raptor_iostream_write_counted_string(xml_writer->iostr, s, len);

  if(xml_writer->current_element)
    xml_writer->current_element->content_cdata_seen=1;
}


/**
 * raptor_xml_writer_comment:
 * @xml_writer: XML writer object
 * @s: comment string to write
 *
 * Write an XML comment to the XML writer.
 *
 * Closes any previous empty element if XML writer feature AUTO_EMPTY
 * is enabled.
 *
 **/
void
raptor_xml_writer_comment(raptor_xml_writer* xml_writer,
                          const unsigned char *s)
{
  XML_WRITER_FLUSH_CLOSE_BRACKET(xml_writer);
  
  raptor_xml_writer_raw_counted(xml_writer, (const unsigned char*)"<!-- ", 5);
  raptor_xml_writer_cdata(xml_writer, s);
  raptor_xml_writer_raw_counted(xml_writer, (const unsigned char*)" -->", 4);
}


/**
 * raptor_xml_writer_comment_counted:
 * @xml_writer: XML writer object
 * @s: comment string to write
 * @len: length of string
 *
 * Write a counted XML comment to the XML writer.
 *
 * Closes any previous empty element if XML writer feature AUTO_EMPTY
 * is enabled.
 *
 **/
void
raptor_xml_writer_comment_counted(raptor_xml_writer* xml_writer,
                                  const unsigned char *s, unsigned int len)
{
  XML_WRITER_FLUSH_CLOSE_BRACKET(xml_writer);
  
  raptor_xml_writer_raw_counted(xml_writer, (const unsigned char*)"<!-- ", 5);
  raptor_xml_writer_cdata_counted(xml_writer, s, len);
  raptor_xml_writer_raw_counted(xml_writer, (const unsigned char*)" -->", 4);
}


/**
 * raptor_xml_writer_features_enumerate:
 * @feature: feature enumeration (0+)
 * @name: pointer to store feature short name (or NULL)
 * @uri: pointer to store feature URI (or NULL)
 * @label: pointer to feature label (or NULL)
 *
 * Get list of xml_writer features.
 * 
 * If uri is not NULL, a pointer to a new raptor_uri is returned
 * that must be freed by the caller with raptor_free_uri().
 *
 * Return value: 0 on success, <0 on failure, >0 if feature is unknown
 **/
int
raptor_xml_writer_features_enumerate(const raptor_feature feature,
                                     const char **name, 
                                     raptor_uri **uri, const char **label)
{
  return raptor_features_enumerate_common(feature, name, uri, label, 8);
}


/**
 * raptor_xml_writer_flush:
 * @xml_writer: XML writer object
 *
 * Finish the XML writer.
 *
 **/
void
raptor_xml_writer_flush(raptor_xml_writer* xml_writer)
{
  if(xml_writer->pending_newline) {
    raptor_iostream_write_byte(xml_writer->iostr, '\n');
    xml_writer->pending_newline=0;
  }
}


/**
 * raptor_xml_writer_set_feature:
 * @xml_writer: #raptor_xml_writer xml_writer object
 * @feature: feature to set from enumerated #raptor_feature values
 * @value: integer feature value (0 or larger)
 *
 * Set xml_writer features with integer values.
 * 
 * The allowed features are available via raptor_features_enumerate().
 *
 * Return value: non 0 on failure or if the feature is unknown
 **/
int
raptor_xml_writer_set_feature(raptor_xml_writer *xml_writer, 
                              raptor_feature feature, int value)
{
  if(value < 0)
    return -1;
  
  switch(feature) {
    case RAPTOR_FEATURE_WRITER_AUTO_INDENT:
      if (value)
        xml_writer->flags |= XML_WRITER_AUTO_INDENT;
      else
        xml_writer->flags &= ~XML_WRITER_AUTO_INDENT;        
      break;

    case RAPTOR_FEATURE_WRITER_AUTO_EMPTY:
      if (value)
        xml_writer->flags |= XML_WRITER_AUTO_EMPTY;
      else
        xml_writer->flags &= ~XML_WRITER_AUTO_EMPTY;        
      break;

    case RAPTOR_FEATURE_WRITER_INDENT_WIDTH:
      xml_writer->indent = value;
      break;
        
    case RAPTOR_FEATURE_WRITER_XML_VERSION:
      if(value == 10 || value == 11)
        xml_writer->xml_version = value;
      break;
        
    case RAPTOR_FEATURE_WRITER_XML_DECLARATION:
      xml_writer->xml_declaration = value;
      break;
        
    /* parser features */
    case RAPTOR_FEATURE_SCANNING:
    case RAPTOR_FEATURE_ASSUME_IS_RDF:
    case RAPTOR_FEATURE_ALLOW_NON_NS_ATTRIBUTES:
    case RAPTOR_FEATURE_ALLOW_OTHER_PARSETYPES:
    case RAPTOR_FEATURE_ALLOW_BAGID:
    case RAPTOR_FEATURE_ALLOW_RDF_TYPE_RDF_LIST:
    case RAPTOR_FEATURE_NORMALIZE_LANGUAGE:
    case RAPTOR_FEATURE_NON_NFC_FATAL:
    case RAPTOR_FEATURE_WARN_OTHER_PARSETYPES:
    case RAPTOR_FEATURE_CHECK_RDF_ID:
    case RAPTOR_FEATURE_HTML_TAG_SOUP:
    case RAPTOR_FEATURE_MICROFORMATS:
    case RAPTOR_FEATURE_HTML_LINK:
    case RAPTOR_FEATURE_WWW_TIMEOUT:

    /* Shared */
    case RAPTOR_FEATURE_NO_NET:

    /* XML writer features */
    case RAPTOR_FEATURE_RELATIVE_URIS:
    case RAPTOR_FEATURE_START_URI:

    /* DOT serializer features */
    case RAPTOR_FEATURE_RESOURCE_BORDER:
    case RAPTOR_FEATURE_LITERAL_BORDER:
    case RAPTOR_FEATURE_BNODE_BORDER:
    case RAPTOR_FEATURE_RESOURCE_FILL:
    case RAPTOR_FEATURE_LITERAL_FILL:
    case RAPTOR_FEATURE_BNODE_FILL:

    /* JSON serializer features */
    case RAPTOR_FEATURE_JSON_CALLBACK:
    case RAPTOR_FEATURE_JSON_EXTRA_DATA:
    case RAPTOR_FEATURE_RSS_TRIPLES:
    case RAPTOR_FEATURE_ATOM_ENTRY_URI:
    
    /* Turtle serializer feature */
    case RAPTOR_FEATURE_WRITE_BASE_URI:

    /* WWW feature */
    case RAPTOR_FEATURE_WWW_HTTP_CACHE_CONTROL:
    case RAPTOR_FEATURE_WWW_HTTP_USER_AGENT:
      
    default:
      return -1;
      break;
  }

  return 0;
}


/**
 * raptor_xml_writer_set_feature_string:
 * @xml_writer: #raptor_xml_writer xml_writer object
 * @feature: feature to set from enumerated #raptor_feature values
 * @value: feature value
 *
 * Set xml_writer features with string values.
 * 
 * The allowed features are available via raptor_xml_writer_features_enumerate().
 * If the feature type is integer, the value is interpreted as an integer.
 *
 * Return value: non 0 on failure or if the feature is unknown
 **/
int
raptor_xml_writer_set_feature_string(raptor_xml_writer *xml_writer, 
                                     raptor_feature feature, 
                                     const unsigned char *value)
{
  int value_is_string=(raptor_feature_value_type(feature) == 1);
  if(!value_is_string)
    return raptor_xml_writer_set_feature(xml_writer, feature, 
                                         atoi((const char*)value));
  else
    return -1;
}


/**
 * raptor_xml_writer_get_feature:
 * @xml_writer: #raptor_xml_writer xml writer object
 * @feature: feature to get value
 *
 * Get various xml_writer features.
 * 
 * The allowed features are available via raptor_features_enumerate().
 *
 * Note: no feature value is negative
 *
 * Return value: feature value or < 0 for an illegal feature
 **/
int
raptor_xml_writer_get_feature(raptor_xml_writer *xml_writer, 
                              raptor_feature feature)
{
  int result= -1;
  
  switch(feature) {
    case RAPTOR_FEATURE_WRITER_AUTO_INDENT:
      result=XML_WRITER_AUTO_INDENT(xml_writer);
      break;

    case RAPTOR_FEATURE_WRITER_AUTO_EMPTY:
      result=XML_WRITER_AUTO_EMPTY(xml_writer);
      break;

    case RAPTOR_FEATURE_WRITER_INDENT_WIDTH:
      result=xml_writer->indent;
      break;

    case RAPTOR_FEATURE_WRITER_XML_VERSION:
      result=xml_writer->xml_version;
      break;

    case RAPTOR_FEATURE_WRITER_XML_DECLARATION:
      result=xml_writer->xml_declaration;
      break;
      
    /* parser features */
    case RAPTOR_FEATURE_SCANNING:
    case RAPTOR_FEATURE_ASSUME_IS_RDF:
    case RAPTOR_FEATURE_ALLOW_NON_NS_ATTRIBUTES:
    case RAPTOR_FEATURE_ALLOW_OTHER_PARSETYPES:
    case RAPTOR_FEATURE_ALLOW_BAGID:
    case RAPTOR_FEATURE_ALLOW_RDF_TYPE_RDF_LIST:
    case RAPTOR_FEATURE_NORMALIZE_LANGUAGE:
    case RAPTOR_FEATURE_NON_NFC_FATAL:
    case RAPTOR_FEATURE_WARN_OTHER_PARSETYPES:
    case RAPTOR_FEATURE_CHECK_RDF_ID:
    case RAPTOR_FEATURE_HTML_TAG_SOUP:
    case RAPTOR_FEATURE_MICROFORMATS:
    case RAPTOR_FEATURE_HTML_LINK:
    case RAPTOR_FEATURE_WWW_TIMEOUT:

    /* Shared */
    case RAPTOR_FEATURE_NO_NET:

    /* XML writer features */
    case RAPTOR_FEATURE_RELATIVE_URIS:
    case RAPTOR_FEATURE_START_URI:

    /* DOT serializer features */
    case RAPTOR_FEATURE_RESOURCE_BORDER:
    case RAPTOR_FEATURE_LITERAL_BORDER:
    case RAPTOR_FEATURE_BNODE_BORDER:
    case RAPTOR_FEATURE_RESOURCE_FILL:
    case RAPTOR_FEATURE_LITERAL_FILL:
    case RAPTOR_FEATURE_BNODE_FILL:

    /* JSON serializer features */
    case RAPTOR_FEATURE_JSON_CALLBACK:
    case RAPTOR_FEATURE_JSON_EXTRA_DATA:
    case RAPTOR_FEATURE_RSS_TRIPLES:
    case RAPTOR_FEATURE_ATOM_ENTRY_URI:
    
    /* Turtle serializer feature */
    case RAPTOR_FEATURE_WRITE_BASE_URI:

    /* WWW feature */
    case RAPTOR_FEATURE_WWW_HTTP_CACHE_CONTROL:
    case RAPTOR_FEATURE_WWW_HTTP_USER_AGENT:
      
    default:
      break;
  }
  
  return result;
}


/**
 * raptor_xml_writer_get_feature_string:
 * @xml_writer: #raptor_xml_writer xml writer object
 * @feature: feature to get value
 *
 * Get xml_writer features with string values.
 * 
 * The allowed features are available via raptor_features_enumerate().
 *
 * Return value: feature value or NULL for an illegal feature or no value
 **/
const unsigned char *
raptor_xml_writer_get_feature_string(raptor_xml_writer *xml_writer, 
                                     raptor_feature feature)
{
  return NULL;
}


/**
 * raptor_xml_writer_get_depth:
 * @xml_writer: #raptor_xml_writer xml writer object
 *
 * Get the current XML Writer element depth
 *
 * Return value: element stack depth
 */
int
raptor_xml_writer_get_depth(raptor_xml_writer *xml_writer)
{
  return xml_writer->depth;
}


#endif



#ifdef STANDALONE

/* one more prototype */
int main(int argc, char *argv[]);


const unsigned char *base_uri_string=(const unsigned char*)"http://example.org/base#";

#define OUT_BYTES_COUNT 135

int
main(int argc, char *argv[]) 
{
  const char *program=raptor_basename(argv[0]);
  const raptor_uri_handler *uri_handler;
  void *uri_context;
  raptor_iostream *iostr;
  raptor_namespace_stack *nstack;
  raptor_namespace* foo_ns;
  raptor_xml_writer* xml_writer;
  raptor_uri* base_uri;
  raptor_qname* el_name;
  raptor_xml_element *element;
  unsigned long offset;
  raptor_qname **attrs;
  raptor_uri* base_uri_copy=NULL;

  /* for raptor_new_iostream_to_string */
  void *string=NULL;
  size_t string_len=0;

  raptor_init();
  
  iostr=raptor_new_iostream_to_string(&string, &string_len, NULL);
  if(!iostr) {
    fprintf(stderr, "%s: Failed to create iostream to string\n", program);
    exit(1);
  }

  raptor_uri_get_handler(&uri_handler, &uri_context);

  nstack=raptor_new_namespaces(uri_handler, uri_context,
                               NULL, NULL, /* errors */
                               1);

  xml_writer=raptor_new_xml_writer(nstack,
                                   uri_handler, uri_context,
                                   iostr,
                                   NULL, NULL, /* errors */
                                   1);
  if(!xml_writer) {
    fprintf(stderr, "%s: Failed to create xml_writer to iostream\n", program);
    exit(1);
  }

  base_uri=raptor_new_uri(base_uri_string);

  foo_ns=raptor_new_namespace(nstack,
                              (const unsigned char*)"foo",
                              (const unsigned char*)"http://example.org/foo-ns#",
                              0);


  el_name=raptor_new_qname_from_namespace_local_name(foo_ns,
                                                     (const unsigned char*)"bar", 
                                                     NULL);
  base_uri_copy=base_uri ? raptor_uri_copy(base_uri) : NULL;
  element=raptor_new_xml_element(el_name,
                                  NULL, /* language */
                                  base_uri_copy);

  raptor_xml_writer_start_element(xml_writer, element);
  raptor_xml_writer_cdata_counted(xml_writer, (const unsigned char*)"hello\n", 6);
  raptor_xml_writer_comment_counted(xml_writer, (const unsigned char*)"comment", 7);
  raptor_xml_writer_cdata(xml_writer, (const unsigned char*)"\n");
  raptor_xml_writer_end_element(xml_writer, element);

  raptor_free_xml_element(element);

  raptor_xml_writer_cdata(xml_writer, (const unsigned char*)"\n");

  el_name=raptor_new_qname(nstack, 
                           (const unsigned char*)"blah", 
                           NULL, /* no attribute value - element */
                           NULL, NULL); /* errors */
  base_uri_copy=base_uri ? raptor_uri_copy(base_uri) : NULL;
  element=raptor_new_xml_element(el_name,
                                  NULL, /* language */
                                  base_uri_copy);

  attrs=(raptor_qname **)RAPTOR_CALLOC(qnamearray, 1, sizeof(raptor_qname*));
  attrs[0]=raptor_new_qname(nstack, 
                            (const unsigned char*)"a",
                            (const unsigned char*)"b", /* attribute value */
                            NULL, NULL); /* errors */
  raptor_xml_element_set_attributes(element, attrs, 1);

  raptor_xml_writer_empty_element(xml_writer, element);

  raptor_xml_writer_cdata(xml_writer, (const unsigned char*)"\n");

  raptor_free_xml_writer(xml_writer);

  raptor_free_xml_element(element);

  raptor_free_namespace(foo_ns);

  raptor_free_namespaces(nstack);

  raptor_free_uri(base_uri);

  
  offset=raptor_iostream_tell(iostr);

#if RAPTOR_DEBUG > 1
  fprintf(stderr, "%s: Freeing iostream\n", program);
#endif
  raptor_free_iostream(iostr);

  if(offset != OUT_BYTES_COUNT) {
    fprintf(stderr, "%s: I/O stream wrote %d bytes, expected %d\n", program,
            (int)offset, (int)OUT_BYTES_COUNT);
    fputs("[[", stderr);
    (void)fwrite(string, 1, string_len, stderr);
    fputs("]]\n", stderr);
    return 1;
  }
  
  if(!string) {
    fprintf(stderr, "%s: I/O stream failed to create a string\n", program);
    return 1;
  }
  string_len=strlen((const char*)string);
  if(string_len != offset) {
    fprintf(stderr, "%s: I/O stream created a string length %d, expected %d\n", program, (int)string_len, (int)offset);
    return 1;
  }

#if RAPTOR_DEBUG > 1
  fprintf(stderr, "%s: Made XML string of %d bytes\n", program, (int)string_len);
  fputs("[[", stderr);
  (void)fwrite(string, 1, string_len, stderr);
  fputs("]]\n", stderr);
#endif

  raptor_free_memory(string);
  

  raptor_finish();
  
  /* keep gcc -Wall happy */
  return(0);
}

#endif
