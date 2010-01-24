/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_xml.c - Raptor XML routines
 *
 * Copyright (C) 2003-2006, David Beckett http://purl.org/net/dajobe/
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

/**
 * raptor_new_xml_element:
 * @name: The XML element name
 * @xml_language: the in-scope XML language (or NULL)
 * @xml_base: the in-scope XML base URI (or NULL)
 * 
 * Constructor - create a new XML element from a QName
 * 
 * Return value: a new #raptor_xml_element or NULL on failure
 **/
raptor_xml_element*
raptor_new_xml_element(raptor_qname *name,
                       const unsigned char *xml_language, 
                       raptor_uri *xml_base)
{
  raptor_xml_element* xml_element;

  xml_element=(raptor_xml_element*)RAPTOR_CALLOC(raptor_xml_element, 1,
                                                 sizeof(raptor_xml_element));
  if(!xml_element)
    return NULL;

  /* Element name */
  xml_element->name=name;
  xml_element->xml_language=xml_language;
  xml_element->base_uri=xml_base;

  xml_element->declared_nspaces=NULL;

  xml_element->content_cdata_sb=raptor_new_stringbuffer();
  if(!xml_element->content_cdata_sb) {
    RAPTOR_FREE(raptor_xml_element, xml_element);
    xml_element=NULL;
  }

  return xml_element;
}


/**
 * raptor_new_xml_element_from_namespace_local_name:
 * @ns: namespace
 * @name: the XML element local name
 * @xml_language: the in-scope XML language (or NULL)
 * @xml_base: base uri (or NULL)
 *
 * Constructor - create a new XML element from an XML namespace and a local name
 *
 * Added in 1.4.16.
 *
 * Return value: a new #raptor_xml_element or NULL on failure
 */
raptor_xml_element*
raptor_new_xml_element_from_namespace_local_name(raptor_namespace *ns,
                                                 const unsigned char *name,
                                                 const unsigned char *xml_language, 
                                                 raptor_uri *xml_base)
{
  raptor_uri *base_uri_copy;
  raptor_qname *qname;
  raptor_xml_element *element=NULL;

  qname=raptor_new_qname_from_namespace_local_name(ns, name, NULL);
  if(qname) {
    base_uri_copy=xml_base ? raptor_uri_copy(xml_base) : NULL;
    element=raptor_new_xml_element(qname, xml_language, base_uri_copy);
    if(!element) {
      raptor_free_qname(qname);
      if(base_uri_copy)
        raptor_free_uri(base_uri_copy);
    }
  }
  return element;
}


/**
 * raptor_free_xml_element:
 * @element: XML Element
 * 
 * Destructor - destroy a raptor_xml_element object.
 **/
void
raptor_free_xml_element(raptor_xml_element *element)
{
  unsigned int i;

  for (i=0; i < element->attribute_count; i++)
    if(element->attributes[i])
      raptor_free_qname(element->attributes[i]);

  if(element->attributes)
    RAPTOR_FREE(raptor_qname_array, element->attributes);

  if(element->content_cdata_sb)
    raptor_free_stringbuffer(element->content_cdata_sb);

  if(element->base_uri)
    raptor_free_uri(element->base_uri);

  if(element->xml_language)
    RAPTOR_FREE(cstring, (void*)element->xml_language);

  raptor_free_qname(element->name);

  if(element->declared_nspaces)
    raptor_free_sequence(element->declared_nspaces);

  RAPTOR_FREE(raptor_element, element);
}


/**
 * raptor_xml_element_get_name:
 * @xml_element: XML Element
 * 
 * Get the XML Name of an XML element
 * 
 * Return value: The Name.
 **/
raptor_qname*
raptor_xml_element_get_name(raptor_xml_element *xml_element)
{
  return xml_element->name;
}


/**
 * raptor_xml_element_set_attributes:
 * @xml_element: XML Element
 * @attributes: Array of XML Qname attributes with values
 * @count: Length of array
 * 
 * Set the attributes on an XML element.
 **/
void
raptor_xml_element_set_attributes(raptor_xml_element* xml_element,
                                   raptor_qname **attributes, int count)
{
  xml_element->attributes=attributes;
  xml_element->attribute_count=count;
}


/**
 * raptor_xml_element_get_attributes:
 * @xml_element: XML Element
 * 
 * Get the array of attributes on the XML element.
 *
 * Use raptor_xml_element_get_attributes_count() to get the count
 * of the array size.
 * 
 * Return value: the array of qnames or NULL if none are present.
 **/
raptor_qname**
raptor_xml_element_get_attributes(raptor_xml_element* xml_element)
{
  return xml_element->attributes;
}


/**
 * raptor_xml_element_get_attributes_count:
 * @xml_element: XML Element
 * 
 * Get the number of attributes on the XML element.
 * 
 * Return value: Integer number of attributes - 0 or more.
 **/
int
raptor_xml_element_get_attributes_count(raptor_xml_element* xml_element)
{
  return xml_element->attribute_count;
}


/**
 * raptor_xml_element_declare_namespace:
 * @xml_element: XML Element
 * @nspace: raptor_namespace to declare
 * 
 * Declare a namespace on the XML Element.
 *
 * Return value: non-0 if namespace cannot be declared 
 **/
int
raptor_xml_element_declare_namespace(raptor_xml_element* xml_element,
                                     raptor_namespace *nspace)
{
  int i;
  const raptor_namespace *ns;

  if(!xml_element->declared_nspaces)
    xml_element->declared_nspaces=raptor_new_sequence(NULL, NULL);

  if((ns = xml_element->name->nspace)) {
    /* Cannot have same namespace already seen */
    if(ns == nspace ||
       /* ... or two default nspaces */
       (!ns->prefix && !nspace->prefix) ||
       /* ... or two same prefixes */
       (ns->prefix && nspace->prefix &&
        !strcmp((const char*)ns->prefix, (const char*)nspace->prefix))
       )
      return 1;
  }

  
  for(i=0;
      (ns = (const raptor_namespace*)raptor_sequence_get_at(xml_element->declared_nspaces, i));
      i++) {
    /* Cannot have same namespace already seen */
    if(ns == nspace ||
       /* ... or two default nspaces */
       (!ns->prefix && !nspace->prefix) ||
       /* ... or two same prefixes */
       (ns->prefix && nspace->prefix &&
        !strcmp((const char*)ns->prefix, (const char*)nspace->prefix))
       )
      return 1;
  }

  raptor_sequence_push(xml_element->declared_nspaces, nspace);

  return 0;
}


#ifdef RAPTOR_DEBUG
void
raptor_print_xml_element(raptor_xml_element *element, FILE* stream)
{
  raptor_qname_print(stream, element->name);
  fputc('\n', stream);

  if(element->attribute_count) {
    unsigned int i;
    int printed=0;

    fputs(" attributes: ", stream);
    for (i = 0; i < element->attribute_count; i++) {
      if(element->attributes[i]) {
        if(printed)
          fputc(' ', stream);
        raptor_qname_print(stream, element->attributes[i]);
        fprintf(stream, "='%s'", element->attributes[i]->value);
        printed=1;
      }
    }
    fputc('\n', stream);
  }
}
#endif


struct nsd
{
  const raptor_namespace *nspace;
  unsigned char *declaration;
  size_t length;
};


static int
raptor_nsd_compare(const void *a, const void *b) 
{
  struct nsd* nsd_a=(struct nsd*)a;
  struct nsd* nsd_b=(struct nsd*)b;
  return strcmp((const char*)nsd_a->declaration, (const char*)nsd_b->declaration);
}


/**
 * raptor_iostream_write_xml_element:
 * @iostr: iostream object
 * @element: XML element to format
 * @nstack: Namespace stack context to use in formatting
 * @is_empty: non-0 if element is empty
 * @is_end: non-0 if this is an end element (else is a start element)
 * @error_handler: error handler function
 * @error_data: error handler function data
 * @depth: XML element depth
 *
 * Write a formatted XML element to a #raptor_iostream
 *
 * Return value: non-0 on failure
*/
int
raptor_iostream_write_xml_element(raptor_iostream* iostr,
                                  raptor_xml_element *element,
                                  raptor_namespace_stack *nstack,
                                  int is_empty,
                                  int is_end,
                                  raptor_simple_message_handler error_handler,
                                  void *error_data,
                                  int depth)
{
  struct nsd *nspace_declarations=NULL;
  size_t nspace_declarations_count=0;  
  unsigned int i;

  /* max is 1 per element and 1 for each attribute + size of declared */
  if(nstack) {
    int nspace_max_count=element->attribute_count+1;
    if(element->declared_nspaces)
      nspace_max_count += raptor_sequence_size(element->declared_nspaces);
    
    nspace_declarations=(struct nsd*)RAPTOR_CALLOC(nsdarray, nspace_max_count, sizeof(struct nsd));
  }

  if(element->name->nspace) {
    if(!is_end && nstack &&
       !raptor_namespaces_namespace_in_scope(nstack, element->name->nspace)) {
      nspace_declarations[0].declaration=
        raptor_namespaces_format(element->name->nspace,
                                 &nspace_declarations[0].length);
      nspace_declarations[0].nspace=element->name->nspace;
      nspace_declarations_count++;
    }
  }

  if (!is_end && element->attributes) {
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
            nspace_declarations[nspace_declarations_count].nspace=element->attributes[i]->nspace;
            nspace_declarations_count++;
          }
        }

      }
    }
  }
  

  if(!is_end && nstack && element->declared_nspaces &&
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
        nspace_declarations[nspace_declarations_count].nspace=nspace;
        nspace_declarations_count++;
      }

    }
  }



  raptor_iostream_write_byte(iostr, '<');
  if(is_end)
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

  /* declare namespaces */
  if(nspace_declarations_count) {
    /* sort them into the canonical order */
    qsort((void*)nspace_declarations, 
          nspace_declarations_count, sizeof(struct nsd),
          raptor_nsd_compare);
    /* add them */
    for (i=0; i < nspace_declarations_count; i++) {
      raptor_iostream_write_byte(iostr, ' ');
      raptor_iostream_write_counted_string(iostr, 
                                           (const char*)nspace_declarations[i].declaration,
                                           nspace_declarations[i].length);
      RAPTOR_FREE(cstring, nspace_declarations[i].declaration);
      nspace_declarations[i].declaration=NULL;

      raptor_namespace_copy(nstack,
                            (raptor_namespace*)nspace_declarations[i].nspace,
                            depth);
    }
  }


  if(!is_end && element->attributes) {
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
      
      raptor_iostream_write_xml_escaped_string(iostr,
                                               element->attributes[i]->value, 
                                               element->attributes[i]->value_length,
                                               '"',
                                               error_handler, error_data);
      raptor_iostream_write_byte(iostr, '"');
    }
  }
  
  if(is_empty)
    raptor_iostream_write_byte(iostr, '/');

  raptor_iostream_write_byte(iostr, '>');

  if(nstack)
    RAPTOR_FREE(stringarray, nspace_declarations);

  return 0;
}


/**
 * raptor_xml_element_get_language:
 * @xml_element: XML Element
 * 
 * Get the XML language of the element.
 * 
 * Return value: XML language or NULL if none in scope
 **/
const unsigned char*
raptor_xml_element_get_language(raptor_xml_element* xml_element)
{
  return xml_element->xml_language;
}


/**
 * raptor_valid_xml_ID:
 * @rdf_parser: RDF parser
 * @string: The string to check.
 *
 * Check the string matches the xml:ID value constraints.
 *
 * This checks the syntax part of the xml:ID validity constraint,
 * that it matches [ VC: Name Token ] as amended by XML Namespaces:
 *
 *   http://www.w3.org/TR/REC-xml-names/#NT-NCName
 * 
 * Return value: non-zero if the ID string is valid
 **/
int
raptor_valid_xml_ID(raptor_parser *rdf_parser, const unsigned char *string)
{
  int len=strlen((const char*)string);
#ifdef RAPTOR_XML_1_1
  #define XML_ID_XML_VERSION 11
#else
  #define XML_ID_XML_VERSION 10
#endif

  return raptor_xml_name_check(string, len, XML_ID_XML_VERSION);
}


/**
 * raptor_xml_any_escape_string:
 * @string: string to XML escape (UTF-8)
 * @len: length of string
 * @buffer: the buffer to use for new string (UTF-8)
 * @length: buffer size
 * @quote: optional quote character to escape for attribute content, or 0
 * @xml_version: XML 1.0 (10) or XML 1.1 (11)
 * @error_handler: error handler function
 * @error_data: error handler user data
 *
 * Return an XML-escaped version a string.
 * 
 * Follows Canonical XML rules on Text Nodes and Attribute Nodes
 *   http://www.w3.org/TR/xml-c14n#ProcessingModel
 *
 * Both:
 *   Replaces <literal>&amp;</literal> and <literal>&lt;</literal>
 *   with <literal>&amp;amp;</literal> and <literal>&amp;lt;</literal>
 * respectively, preserving other characters.
 * 
 * Text Nodes:
 *   <literal>&gt;</literal> is turned into <literal>&amp;gt;</literal>
 *   ##xD is turned into <literal>&amp;##xD;</literal>
 *
 * Attribute Nodes:
 *   <literal>&gt;</literal> is generated not <literal>&amp;gt</literal>.
 *   ##x9, ##xA and ##xD are turned into
 *   <literal>&amp;##x9;</literal>,
 *   <literal>&amp;##xA;</literal> and
 *   <literal>&amp;##xD;</literal>
 *   entities.
 *
 * If @quote is given it can be either of '\'' or '\"'
 * which will be turned into <literal>&amp;apos;</literal> or
 * <literal>&amp;quot;</literal> respectively.
 * ASCII NUL ('\0') or any other character will not be escaped.
 * 
 * If @buffer is NULL, no work is done but the size of buffer
 * required is returned.  The output in buffer remains in UTF-8.
 *
 * If the input @string is empty, a single NUL will be written to the
 * buffer.
 *
 * Return value: the number of bytes required / used or <0 on failure.
 **/
int
raptor_xml_any_escape_string(const unsigned char *string, size_t len,
                             unsigned char *buffer, size_t length,
                             char quote,
                             int xml_version,
                             raptor_simple_message_handler error_handler,
                             void *error_data)
{
  int l;
  size_t new_len=0;
  const unsigned char *p;
  unsigned char *q;
  int unichar_len;
  raptor_unichar unichar;

  if(quote != '\"' && quote != '\'')
    quote='\0';

  for(l=len, p=string; l; p++, l--) {
    if(*p > 0x7f) {
      unichar_len=raptor_utf8_to_unicode_char(&unichar, p, l);
      if(unichar_len < 0 || unichar_len > l) {
        if(error_handler)
          error_handler(error_data, "Bad UTF-8 encoding.");
        return -1;
      }
    } else {
      unichar=*p;
      unichar_len=1;
    }
  
    if(unichar == '&')
      /* &amp; */
      new_len+= 5;
    else if(unichar == '<' || (!quote && unichar == '>'))
      /* &lt; or &gt; */
      new_len+= 4;
    else if (quote && unichar == (unsigned long)quote)
      /* &apos; or &quot; */
      new_len+= 6;
    else if (unichar == 0x0d ||
             (quote && (unichar == 0x09 || unichar == 0x0a)))
      /* &#xD; or &#x9; or &xA; */
      new_len+= 5;
    else if (unichar == 0x7f ||
             (unichar < 0x20 && unichar != 0x09 && unichar != 0x0a)) {
      if(!unichar || xml_version < 11) {
        if(error_handler)
          error_handler(error_data, "Cannot write illegal XML 1.0 character %d.", unichar);
      } else {
        /* &#xX; */
        new_len+= 5;
        if(unichar > 0x0f)
          new_len++;
      }
    } else
      new_len+= unichar_len;

    unichar_len--; /* since loop does len-- */
    p += unichar_len; l -= unichar_len;
  }

  if(length && new_len > length)
    return 0;

  if(!buffer)
    return new_len;
  
  for(l=len, p=string, q=buffer; l; p++, l--) {
    if(*p > 0x7f) {
      unichar_len=raptor_utf8_to_unicode_char(&unichar, p, l);
    } else {
      unichar=*p;
      unichar_len=1;
    }

    if(unichar == '&') {
      strncpy((char*)q, "&amp;", 5);
      q+= 5;
    } else if (unichar == '<') {
      strncpy((char*)q, "&lt;", 4);
      q+= 4;
    } else if (!quote && unichar == '>') {
      strncpy((char*)q, "&gt;", 4);
      q+= 4;
    } else if (quote && unichar == (unsigned long)quote) {
      if(quote == '\'')  
        strncpy((char*)q, "&apos;", 6);
      else
        strncpy((char*)q, "&quot;", 6);
      q+= 6;
    } else if (unichar == 0x0d ||
               (quote && (unichar == 0x09 || unichar == 0x0a))) {
      /* &#xX; */
      *q++='&';
      *q++='#';
      *q++='x';
      if(unichar == 0x09)
        *q++ = '9';
      else
        *q++ = 'A'+ ((char)unichar-0x0a);
      *q++= ';';
    } else if (unichar == 0x7f ||
               (unichar < 0x20 && unichar != 0x09 && unichar != 0x0a)) {
      if(!unichar || xml_version < 11) {
        if(error_handler)
          error_handler(error_data, "Cannot write illegal XML 1.0 character %d.", unichar);
      } else {
        /* &#xX; */
        *q++='&';
        *q++='#';
        *q++='x';
        sprintf((char*)q, "%X", (unsigned int)unichar);
        q+= (unichar < 0x10) ? 1 : 2;
        *q++=';';
      }
    } else {
      strncpy((char*)q, (const char*)p, unichar_len);
      q+= unichar_len;
    }

    unichar_len--; /* since loop does len-- */
    p += unichar_len; l -= unichar_len;
  }

  /* Terminate new string */
  *q = '\0';

  return new_len;
}


/**
 * raptor_xml_escape_string:
 * @string: string to XML 1.0 escape (UTF-8)
 * @len: length of string
 * @buffer: the buffer to use for new string (UTF-8)
 * @length: buffer size
 * @quote: optional quote character to escape for attribute content, or 0
 * @error_handler: error handler function
 * @error_data: error handler user data
 *
 * Return an XML 1.0-escaped version a string.
 * 
 * See raptor_xml_any_escape_string() for the conditions on parameters.
 *
 * Return value: the number of bytes required / used or <0 on failure.
 **/
int
raptor_xml_escape_string(const unsigned char *string, size_t len,
                         unsigned char *buffer, size_t length,
                         char quote,
                         raptor_simple_message_handler error_handler,
                         void *error_data)
{
  return raptor_xml_any_escape_string(string, len,
                                      buffer, length,
                                      quote,
                                      10,
                                      error_handler, error_data);
}


/**
 * raptor_iostream_write_xml_any_escaped_string:
 * @string: string to XML escape (UTF-8)
 * @len: length of string
 * @quote: optional quote character to escape for attribute content, or 0
 * @iostr: the #raptor_iostream to write to
 * @xml_version: XML version - 10 (XML 1.0) or 11 (XML 1.1)
 * @error_handler: error handler function
 * @error_data: error handler data
 *
 * Write an XML-escaped version of a string to an iostream.
 * 
 * See raptor_xml_escape_string() for the escapes performed and
 * the conditions on @quote and @string.  XML 1.1 allows additional
 * characters in XML such as U+0001 to U+001F inclusive.
 *
 * Return value: non 0 on failure
 **/
int
raptor_iostream_write_xml_any_escaped_string(raptor_iostream* iostr,
                                             const unsigned char *string,
                                             size_t len,
                                             char quote,
                                             int xml_version,
                                             raptor_simple_message_handler error_handler,
                                             void *error_data)
{
  int l;
  const unsigned char *p;

  if(xml_version != 10)
    xml_version=11;

  if(quote != '\"' && quote != '\'')
    quote='\0';

  for(l=len, p=string; l; p++, l--) {
    int unichar_len=1;
    raptor_unichar unichar=*p;

    if(*p > 0x7f) {
      unichar_len=raptor_utf8_to_unicode_char(&unichar, p, l);
      if(unichar_len < 0 || unichar_len > l) {
        if(error_handler)
          error_handler(error_data, "Bad UTF-8 encoding.");
        return 1;
      }
    }

    if(unichar == '&')
      raptor_iostream_write_counted_string(iostr, "&amp;", 5);
    else if (unichar == '<')
      raptor_iostream_write_counted_string(iostr, "&lt;", 4);
    else if (!quote && unichar == '>')
      raptor_iostream_write_counted_string(iostr, "&gt;", 4);
    else if (quote && unichar == (unsigned long)quote) {
      if(quote == '\'')  
        raptor_iostream_write_counted_string(iostr, "&apos;", 6);
      else
        raptor_iostream_write_counted_string(iostr, "&quot;", 6);
    } else if (unichar == 0x0d ||
               (quote && (unichar == 0x09 || unichar == 0x0a))) {
      /* &#xX; */
      raptor_iostream_write_counted_string(iostr, "&#x", 3);
      if(unichar == 0x09)
        raptor_iostream_write_byte(iostr, '9');
      else
        raptor_iostream_write_byte(iostr, 'A'+ ((char)unichar-0x0a));
      raptor_iostream_write_byte(iostr, ';');
    } else if (unichar == 0x7f ||
               (unichar < 0x20 && unichar != 0x09 && unichar != 0x0a)) {
      if(!unichar || xml_version < 11) {
        if(error_handler)
          error_handler(error_data, "Cannot write illegal XML 1.0 character %d.", unichar);
      } else {
        int width=(unichar < 0x10) ? 1 : 2;

        /* &#xX; */
        raptor_iostream_write_counted_string(iostr, "&#x", 3);
        raptor_iostream_format_hexadecimal(iostr, unichar, width);
        raptor_iostream_write_byte(iostr,  ';');
      }
    } else
      raptor_iostream_write_counted_string(iostr, (const char*)p, unichar_len);

    unichar_len--; /* since loop does len-- */
    p += unichar_len; l -= unichar_len;
  }

  return 0;
}


/**
 * raptor_iostream_write_xml_escaped_string:
 * @string: string to XML 1.0 escape (UTF-8)
 * @len: length of string
 * @quote: optional quote character to escape for attribute content, or 0
 * @iostr: the #raptor_iostream to write to
 * @error_handler: error handler function
 * @error_data: error handler data
 *
 * Write an XML 1.0-escaped version of a string to an iostream.
 * 
 * See raptor_iostream_write_xml_any_escaped_string() for the escapes
 * performed and the conditions on @quote and @string.
 *
 * Return value: non 0 on failure
 **/
int
raptor_iostream_write_xml_escaped_string(raptor_iostream* iostr,
                                         const unsigned char *string,
                                         size_t len,
                                         char quote,
                                         raptor_simple_message_handler error_handler,
                                         void *error_data)
{
  return raptor_iostream_write_xml_any_escaped_string(iostr, string, len,
                                                      quote, 10,
                                                      error_handler, 
                                                      error_data);
}


/**
 * raptor_xml_name_check:
 * @string: UTF-8 name string
 * @length: length of string
 * @xml_version: XML version
 *
 * Check a string is a legal XML name (and legal UTF8).
 * 
 * xml_version is either 10 (for XML 1.0) or 11 for (XML 1.1). Any
 * other version fails.
 *
 * Return value: Non 0 if the string is a legal XML name
 **/
int
raptor_xml_name_check(const unsigned char *string, size_t length,
                      int xml_version)
{
  int pos;

  if(xml_version != 10 && xml_version != 11)
    return 0;

  for(pos=0; length > 0; pos++) {
    raptor_unichar unichar=0;

    int unichar_len=raptor_utf8_to_unicode_char(&unichar, string, length);
    if(unichar_len < 0 || unichar_len > (int)length)
      return 0;

    if(unichar > 0x10ffff)
      return 0;
  
    if(!pos) {
      /* start of name */
      if(xml_version == 10) {
        if(!raptor_unicode_is_xml10_namestartchar(unichar))
          return 0;
      } else {
        if(!raptor_unicode_is_xml11_namestartchar(unichar))
          return 0;
      }
    } else {
      /* rest of name */
      if(xml_version == 10) {
        if(!raptor_unicode_is_xml10_namechar(unichar))
          return 0;
      } else {
        if(!raptor_unicode_is_xml11_namechar(unichar))
          return 0;
      }
    }

    string += unichar_len;
    length -= unichar_len;
  }
  return 1;
}


#endif




#ifdef STANDALONE

/* static prototypes */
void raptor_bad_string_print(const unsigned char *input, FILE *stream);
int main(int argc, char *argv[]);

void
raptor_bad_string_print(const unsigned char *input, FILE *stream)
{
  while(*input) {
    char c=*input;
    if(isprint(c))
      fputc(c, stream);
    else
      fprintf(stream, "\\x%02X", (c & 0xff));
    input++;
  }
}


int
main(int argc, char *argv[]) 
{
  const char *program=raptor_basename(argv[0]);
  struct tv {
    const char *string;
    const char quote;
    const char *result;
  };
  struct tv *t;
  struct tv test_values[]={
    {"", 0, ""},

    {"&", 0, "&amp;"},
    {"<", 0, "&lt;"},
    {">", 0, "&gt;"},
    {"\x09", 0, "\x09"},
    {"\x0a", 0, "\x0a"},
    {"\x0d", 0, "&#xD;"},

    {"'&'",  '\'', "&apos;&amp;&apos;"},
    {"'<'",  '\'', "&apos;&lt;&apos;"},
    {"'>'",  '\'', "&apos;>&apos;"},
    {"\x09", '\'', "&#x9;"},
    {"\x0a", '\'', "&#xA;"},
    {"\x0d", '\'', "&#xD;"},

    {"\"&\"", '\"', "&quot;&amp;&quot;"},
    {"\"<\"", '\"', "&quot;&lt;&quot;"},
    {"\">\"", '\"', "&quot;>&quot;"},
    {"\x09",  '\"', "&#x9;"},
    {"\x0a",  '\"', "&#xA;"},
    {"\x0d",  '\"', "&#xD;"},

    {"&amp;", 0, "&amp;amp;"},
    {"<foo>", 0, "&lt;foo&gt;"},
#if 0
    {"\x1f", 0, "&#x1F;"},
    {"\xc2\x80", 0, "&#x80;"},
    {"\xe0\xa0\x80", 0, "&#x0800;"},
    {"\xf0\x90\x80\x80", 0, "&#x10000;"},

    {"\x7f", 0, "&#x7F;"},
    {"\xdf\xbf", 0, "&#x07FF;"},
    {"\xef\xbf\xbd", 0, "&#xFFFD;"},
    {"\xf4\x8f\xbf\xbf", 0, "&#x10FFFF;"},

    {"\xc3\xbf", 0, "&#xFF;"},
    {"\xf0\x8f\xbf\xbf", 0, "&#xFFFF;"},
#endif
    {NULL, 0, 0}
  };
  int i;
  int failures=0;

  for(i=0; (t=&test_values[i]) && t->string; i++) {
    const unsigned char *utf8_string=(const unsigned char*)t->string;
    int quote=t->quote;
    size_t utf8_string_len=strlen((const char*)utf8_string);
    unsigned char *xml_string;
    int xml_string_len=0;

    xml_string_len=raptor_xml_escape_string(utf8_string, utf8_string_len,
                                            NULL, 0, quote, NULL, NULL);
    if(xml_string_len < 0) {
      fprintf(stderr, "%s: raptor_xml_escape_string FAILED to escape string '",
              program);
      raptor_bad_string_print(utf8_string, stderr);
      fputs("'\n", stderr);
      failures++;
      continue;
    }
      
    xml_string=(unsigned char*)RAPTOR_MALLOC(cstring, xml_string_len+1);
    
    xml_string_len=raptor_xml_escape_string(utf8_string, utf8_string_len,
                                            xml_string, xml_string_len, quote,
                                            NULL, NULL);
    if(xml_string_len < 0) {
      fprintf(stderr, "%s: raptor_xml_escape_string FAILED to escape string '",
              program);
      raptor_bad_string_print(utf8_string, stderr);
      fputs("'\n", stderr);
      failures++;
      continue;
    }
    if(strcmp((const char*)xml_string, t->result)) {
      fprintf(stderr, "%s: raptor_xml_escape_string FAILED to escape string '",
              program);
      raptor_bad_string_print(utf8_string, stderr);
      fprintf(stderr, "', expected '%s', result was '%s'\n",
              t->result, xml_string);
      failures++;
      continue;
    }

#if RAPTOR_DEBUG > 1    
    fprintf(stderr, "%s: raptor_xml_escape_string escaped string to '%s' ok\n",
            program, xml_string);
#endif
    RAPTOR_FREE(cstring, xml_string);
  }

#if RAPTOR_DEBUG > 1    
  if(!failures)
    fprintf(stderr, "%s: raptor_xml_escape_string all tests OK\n", program);
#endif

  return failures;
}

#endif
