/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_node_internal.h - Internal RDF Node definitions
 *
 * Copyright (C) 2000-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2000-2005, University of Bristol, UK http://www.bristol.ac.uk/
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



#ifndef LIBRDF_NODE_INTERNAL_H
#define LIBRDF_NODE_INTERNAL_H

#ifdef __cplusplus
extern "C" {
#endif

struct librdf_node_s
{
  librdf_world *world;
  librdf_node_type type;
  int usage;
  union 
  {
    struct
    {
      /* rdf:Resource and rdf:Property-s have URIs */
      librdf_uri *uri;
    } resource;
    struct
    {
      /* literals are UTF-8 string values ... */
      unsigned char *string;
      /* up to 2^31-1 bytes long */
      unsigned int string_len;

      /* datatype URI or null */
      librdf_uri* datatype_uri;

      /* XML defines these additional attributes for literals */

      /* Language of literal (xml:lang) */
      char *xml_language;
      /* up to 255 bytes long */
      unsigned char xml_language_len;

      /* Hash key & size */
      unsigned char *key;
      size_t size;
    } literal;
    struct 
    {
      /* blank nodes have an identifier */
      unsigned char *identifier;
      int identifier_len;
    } blank;
  } value;
};


/* initialising functions / constructors */

/* class methods */
void librdf_init_node(librdf_world* world);
void librdf_finish_node(librdf_world* world);

#ifdef LIBRDF_DEBUG
const char* librdf_node_get_type_as_string(int type);
#endif

/* exported public in error but never usable */
librdf_digest* librdf_node_get_digest(librdf_node* node);

#ifdef __cplusplus
}
#endif

#endif
