/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_qname.c - Raptor XML qname class
 *
 * Copyright (C) 2002-2006, David Beckett http://purl.org/net/dajobe/
 * Copyright (C) 2002-2004, University of Bristol, UK http://www.bristol.ac.uk/
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


/*
 * Namespaces in XML
 * http://www.w3.org/TR/1999/REC-xml-names-19990114/#defaulting
 * says:
 *
 * --------------------------------------------------------------------
 *  5.2 Namespace Defaulting
 *
 *  A default namespace is considered to apply to the element where it
 *  is declared (if that element has no namespace prefix), and to all
 *  elements with no prefix within the content of that element. 
 *
 *  If the URI reference in a default namespace declaration is empty,
 *  then unprefixed elements in the scope of the declaration are not
 *  considered to be in any namespace.
 *
 *  Note that default namespaces do not apply directly to attributes.
 *
 * [...]
 *
 *  5.3 Uniqueness of Attributes
 *
 *  In XML documents conforming to this specification, no tag may
 *  contain two attributes which:
 *
 *    1. have identical names, or 
 *
 *    2. have qualified names with the same local part and with
 *    prefixes which have been bound to namespace names that are
 *    identical.
 * --------------------------------------------------------------------
 */

/**
 * raptor_new_qname:
 * @nstack: namespace stack to look up for namespaces
 * @name: element or attribute name
 * @value: attribute value (else is an element)
 * @error_handler: function to call on an error
 * @error_data: user data for error function
 *
 * Constructor - create a new XML qname.
 * 
 * Create a new qname from the local element/attribute name,
 * with optional (attribute) value.  The namespace stack is used
 * to look up the name and find the namespace and generate the
 * URI of the qname.
 * 
 * Return value: a new #raptor_qname object or NULL on failure
 **/
raptor_qname*
raptor_new_qname(raptor_namespace_stack *nstack, 
                 const unsigned char *name,
                 const unsigned char *value,
                 raptor_simple_message_handler error_handler,
                 void *error_data)
{
  raptor_qname* qname;
  const unsigned char *p;
  raptor_namespace* ns;
  unsigned char* new_name;
  int prefix_length;
  int local_name_length=0;

#if RAPTOR_DEBUG > 1
  RAPTOR_DEBUG2("name %s\n", name);
#endif  

  qname=(raptor_qname*)RAPTOR_CALLOC(raptor_qname, 1, sizeof(raptor_qname));
  if(!qname)
    return NULL;


  if(value) {
    int value_length=strlen((char*)value);
    unsigned char* new_value=(unsigned char*)RAPTOR_MALLOC(cstring, value_length+1);

    if(!new_value) {
      RAPTOR_FREE(raptor_qname, qname);
      return NULL;
    } 
    strcpy((char*)new_value, (char*)value);
    qname->value=new_value;
    qname->value_length=value_length;
  }


  /* Find : */
  for(p=name; *p && *p != ':'; p++)
    ;


  if(!*p) {
    local_name_length=p-name;

    /* No : in the name */
    new_name=(unsigned char*)RAPTOR_MALLOC(cstring, local_name_length+1);
    if(!new_name) {
      raptor_free_qname(qname);
      return NULL;
    }
    strcpy((char*)new_name, (char*)name);
    qname->local_name=new_name;
    qname->local_name_length=local_name_length;

    /* For elements only, pick up the default namespace if there is one */
    if(!value) {
      ns=raptor_namespaces_get_default_namespace(nstack);
      
      if(ns) {
        qname->nspace=ns;
#if RAPTOR_DEBUG > 1
        RAPTOR_DEBUG2("Found default namespace %s\n", ns->uri);
#endif
      } else {
#if RAPTOR_DEBUG > 1
        RAPTOR_DEBUG1("No default namespace defined\n");
#endif
      }
    } /* if is_element */

  } else {
    /* There is a namespace prefix */

    prefix_length=p-name;
    p++; 

    /* p now is at start of local_name */
    local_name_length=strlen((char*)p);
    new_name=(unsigned char*)RAPTOR_MALLOC(cstring, local_name_length+1);
    if(!new_name) {
      raptor_free_qname(qname);
      return NULL;
    }
    strcpy((char*)new_name, (char*)p);
    qname->local_name=new_name;
    qname->local_name_length=local_name_length;

    /* Find the namespace */
    ns=raptor_namespaces_find_namespace(nstack, name, prefix_length);

    if(!ns) {
      /* failed to find namespace - now what? */
      if(error_handler)
        error_handler((raptor_parser*)error_data, "The namespace prefix in \"%s\" was not declared.", name);
    } else {
#if RAPTOR_DEBUG > 1
      RAPTOR_DEBUG3("Found namespace prefix %s URI %s\n", ns->prefix, ns->uri);
#endif
      qname->nspace=ns;
    }
  }



  /* If namespace has a URI and a local_name is defined, create the URI
   * for this element 
   */
  if(qname->nspace && local_name_length) {
    raptor_uri *uri=raptor_namespace_get_uri(qname->nspace);
    if(uri)
      uri=raptor_new_uri_from_uri_local_name(uri, new_name);

    qname->uri=uri;
  }


  return qname;
}


/**
 * raptor_new_qname_from_namespace_local_name:
 * @ns: namespace of qname
 * @local_name: element or attribute name
 * @value: attribute value (else is an element)
 *
 * Constructor - create a new XML qname.
 * 
 * Create a new qname from the namespace and local element/attribute name,
 * with optional (attribute) value.
 * 
 * Return value: a new #raptor_qname object or NULL on failure
 **/
raptor_qname*
raptor_new_qname_from_namespace_local_name(raptor_namespace *ns, 
                                           const unsigned char *local_name,
                                           const unsigned char *value)
{
  raptor_qname* qname;
  unsigned char* new_name;
  int local_name_length=strlen((char*)local_name);

  if(!ns || !local_name)
    return NULL;

  qname=(raptor_qname*)RAPTOR_CALLOC(raptor_qname, 1, sizeof(raptor_qname));
  if(!qname)
    return NULL;

  if(value) {
    int value_length=strlen((char*)value);
    unsigned char* new_value=(unsigned char*)RAPTOR_MALLOC(cstring, value_length+1);

    if(!new_value) {
      RAPTOR_FREE(raptor_qname, qname);
      return NULL;
    } 
    strcpy((char*)new_value, (char*)value);
    qname->value=new_value;
    qname->value_length=value_length;
  }

  new_name=(unsigned char*)RAPTOR_MALLOC(cstring, local_name_length+1);
  if(!new_name) {
    raptor_free_qname(qname);
    return NULL;
  }
  strcpy((char*)new_name, (char*)local_name);
  qname->local_name=new_name;
  qname->local_name_length=local_name_length;

  qname->nspace=ns;

  qname->uri=raptor_namespace_get_uri(qname->nspace);
  if(qname->uri)
    qname->uri=raptor_new_uri_from_uri_local_name(qname->uri, new_name);
  
  return qname;
}


/**
 * raptor_qname_copy:
 * @qname: existing qname
 *
 * Copy constructor - copy an existing XML qname.
 *
 * Return value: a new #raptor_qname object or NULL on failure
 **/
raptor_qname*
raptor_qname_copy(raptor_qname *qname) {
  raptor_qname* new_qname;
  unsigned char* new_name;

  new_qname=(raptor_qname*)RAPTOR_CALLOC(raptor_qname, 1, sizeof(raptor_qname));
  if(!new_qname)
    return NULL;

  if(qname->value) {
    int value_length=qname->value_length;
    unsigned char* new_value=(unsigned char*)RAPTOR_MALLOC(cstring, value_length+1);

    if(!new_value) {
      RAPTOR_FREE(raptor_qname, qname);
      return NULL;
    } 
    strcpy((char*)new_value, (char*)qname->value);
    new_qname->value=new_value;
    new_qname->value_length=value_length;
  }

  new_name=(unsigned char*)RAPTOR_MALLOC(cstring, qname->local_name_length+1);
  if(!new_name) {
    raptor_free_qname(new_qname);
    return NULL;
  }
  strcpy((char*)new_name, (char*)qname->local_name);
  new_qname->local_name=new_name;
  new_qname->local_name_length=qname->local_name_length;

  new_qname->nspace=qname->nspace;

  new_qname->uri=raptor_namespace_get_uri(new_qname->nspace);
  if(new_qname->uri)
    new_qname->uri=raptor_new_uri_from_uri_local_name(new_qname->uri, new_name);
  
  return new_qname;
}


#ifdef RAPTOR_DEBUG
void
raptor_qname_print(FILE *stream, raptor_qname* name) 
{
  if(name->nspace) {
    const unsigned char *prefix=raptor_namespace_get_prefix(name->nspace);
    if(prefix)
      fprintf(stream, "%s:%s", prefix, name->local_name);
    else
      fprintf(stream, "(default):%s", name->local_name);
  } else
    fputs((char*)name->local_name, stream);
}
#endif


/**
 * raptor_free_qname:
 * @name: #raptor_qname object
 * 
 * Destructor - destroy a raptor_qname object.
 **/
void
raptor_free_qname(raptor_qname* name) 
{
  if(name->local_name)
    RAPTOR_FREE(cstring, (void*)name->local_name);

  if(name->uri)
    raptor_free_uri(name->uri);

  if(name->value)
    RAPTOR_FREE(cstring, (void*)name->value);
  RAPTOR_FREE(raptor_qname, name);
}


/**
 * raptor_qname_equal:
 * @name1: first #raptor_qname
 * @name2: second #raptor_name
 * 
 * Compare two XML Qnames for equality.
 * 
 * Return value: non-0 if the qnames are equal.
 **/
int
raptor_qname_equal(raptor_qname *name1, raptor_qname *name2)
{
  if(name1->nspace != name2->nspace)
    return 0;
  if(name1->local_name_length != name2->local_name_length)
    return 0;
  if(strcmp((char*)name1->local_name, (char*)name2->local_name))
    return 0;
  return 1;
}



/**
 * raptor_qname_string_to_uri:
 * @nstack: #raptor_namespace_stack to decode the namespace
 * @name: QName string or NULL
 * @name_len: QName string length
 * @error_handler: function to call on an error
 * @error_data: user data for error function
 *
 * Get the URI for a qname.
 * 
 * Utility function to turn a string representing a QName in the
 * N3 style, into a new URI representing it.  A NULL name or name ":"
 * returns the default namespace URI.  A name "p:" returns
 * namespace name (URI) for the namespace with prefix "p".
 * 
 * Partially equivalent to 
 *   qname=raptor_new_qname(nstack, name, NULL, error_handler, error_data);
 *   uri=raptor_uri_copy(qname->uri);
 *   raptor_free_qname(qname)
 * but without making the qname, and it also handles the NULL and
 * ":" name cases as well as error checking.
 *
 * Return value: new #raptor_uri object or NULL on failure
 **/
raptor_uri*
raptor_qname_string_to_uri(raptor_namespace_stack *nstack, 
                           const unsigned char *name, size_t name_len,
                           raptor_simple_message_handler error_handler,
                           void *error_data)
{
  raptor_uri *uri=NULL;
  const unsigned char *p;
  const unsigned char *original_name=name;
  const unsigned char *local_name=NULL;
  int local_name_length=0;
  raptor_namespace* ns;

  /* Empty string is default namespace URI */
  if(!name) {
    ns=raptor_namespaces_get_default_namespace(nstack);
  } else {
    /* If starts with :, it is relative to default namespace, so skip it */
    if(*name == ':') {
      name++;
      name_len--;
    }
    
    for(p=name; *p && *p != ':'; p++)
      ;
    
    /* If ends with :, it is the URI of a namespace */
    if(p-name == (int)(name_len-1)) {
      ns=raptor_namespaces_find_namespace(nstack, name, name_len-1);
    } else {
      if(!*p) {
        local_name=name;
        local_name_length=p-name;
        
        /* pick up the default namespace if there is one */
        ns=raptor_namespaces_get_default_namespace(nstack);
      } else {
        /* There is a namespace prefix */
        int prefix_length=p-name;
        p++;

        local_name=p;
        local_name_length=strlen((char*)p);
        
        /* Find the namespace */
        ns=raptor_namespaces_find_namespace(nstack, name, prefix_length);
      }
    }
  }
  
  if(!ns) {
    if(error_handler)
      error_handler((raptor_parser*)error_data, "The namespace prefix in \"%s\" was not declared.", original_name);
  }



  /* If namespace has a URI and a local_name is defined, return the URI
   * for this name
   */
  if(ns && (uri=raptor_namespace_get_uri(ns))) {
    if(local_name_length)
      uri=raptor_new_uri_from_uri_local_name(uri, local_name);
    else
      uri=raptor_uri_copy(uri);
  }

  return uri;
}


/**
 * raptor_iostream_write_qname:
 * @iostr: raptor iosteram
 * @qname: QName to write
 * 
 * Write a formatted qname to an iostream
 * 
 * Return value: non-0 on failure
 **/
int
raptor_iostream_write_qname(raptor_iostream* iostr, raptor_qname *qname)
{
  if(qname->nspace && qname->nspace->prefix_length > 0) {
    raptor_iostream_write_counted_string(iostr, qname->nspace->prefix,
                                         qname->nspace->prefix_length);
    raptor_iostream_write_byte(iostr, ':');
  }
  
  raptor_iostream_write_counted_string(iostr, qname->local_name,
                                       qname->local_name_length);
  return 0;
}


/**
 * raptor_qname_get_namespace:
 * @name: #raptor_qname object
 * 
 * Get the #raptor_namespace of an XML QName.
 * 
 * Return value: the namespace
 **/
const raptor_namespace*
raptor_qname_get_namespace(raptor_qname* name)
{
  return name->nspace;
}


/**
 * raptor_qname_get_local_name:
 * @name: #raptor_qname object
 * 
 * Get the #raptor_local_name of an XML QName.
 * 
 * Return value: the local_name
 **/
const unsigned char*
raptor_qname_get_local_name(raptor_qname* name)
{
  return name->local_name;
}


/**
 * raptor_qname_get_value:
 * @name: #raptor_qname object
 * 
 * Get the #raptor_value of an XML QName.
 * 
 * Return value: the value
 **/
const unsigned char*
raptor_qname_get_value(raptor_qname* name)
{
  return name->value;
}

/**
 * raptor_qname_get_counted_value:
 * @name: #raptor_qname object
 * @length_p: pointer to variable to store length of name (or NULL)
 * 
 * Get the #raptor_value of an XML QName.
 * 
 * Return value: the value
 **/
const unsigned char*
raptor_qname_get_counted_value(raptor_qname* name, size_t* length_p)
{
  if(length_p)
    *length_p=name->value_length;
  return name->value;
}
