/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_rss.c - Raptor RSS tag soup parser
 *
 * Copyright (C) 2003-2007, David Beckett http://purl.org/net/dajobe/
 * Copyright (C) 2003-2005, University of Bristol, UK http://www.bristol.ac.uk/
 * 
 * Contributions:
 *   Copyright (C) 2004-2005, Suzan Foster <su@islief.nl>
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


/* Raptor includes */
#include "raptor.h"
#include "raptor_internal.h"
#include "raptor_rss.h"


/* local prototypes */
static void raptor_rss_insert_identifiers(raptor_parser* rdf_parser);
static void raptor_rss_uplift_items(raptor_parser* rdf_parser);
static int raptor_rss_emit(raptor_parser* rdf_parser);

static void raptor_rss_start_element_handler(void *user_data, raptor_xml_element* xml_element);
static void raptor_rss_end_element_handler(void *user_data, raptor_xml_element* xml_element);
static void raptor_rss_cdata_handler(void *user_data, raptor_xml_element* xml_element, const unsigned char *s, int len);
static void raptor_rss_comment_handler(void *user_data, raptor_xml_element* xml_element, const unsigned char *s);

/*
 * RSS parser object
 */
struct raptor_rss_parser_s {
  /* static model */
  raptor_rss_model model;
  
  /* current line */
  char *line;
  /* current line length */
  int line_length;
  /* current char in line buffer */
  int offset;
  
  /* static statement for use in passing to user code */
  raptor_statement statement;

  raptor_sax2 *sax2;

  /* rss node type of current item */
  raptor_rss_type current_type;

  /* one place stack */
  raptor_rss_type prev_type;
  raptor_rss_fields_type current_field;

  /* emptyness of current element */
  int element_is_empty;

  /* stack of namespaces */
  raptor_namespace_stack *nstack;

  /* non-0 if this is an atom 1.0 parser */
  int is_atom;

  /* namespaces declared here */
  raptor_namespace* nspaces[RAPTOR_RSS_NAMESPACES_SIZE];
};

typedef struct raptor_rss_parser_s raptor_rss_parser;


typedef enum {
  RAPTOR_RSS_CONTENT_TYPE_NONE,
  RAPTOR_RSS_CONTENT_TYPE_XML,
  RAPTOR_RSS_CONTENT_TYPE_TEXT
} raptor_rss_content_type;


struct raptor_rss_element_s
{
  raptor_uri* uri;
  const unsigned char *rel;

  /* Two types of content */
  raptor_rss_content_type type;

  /* 1) XML */
  raptor_xml_writer* xml_writer;
  /* XML written to this iostream to the xml_content string */
  raptor_iostream* iostream;
  /* ends up here */
  void *xml_content;
  size_t xml_content_length;

  /* 2) cdata */
  raptor_stringbuffer* sb;
};

typedef struct raptor_rss_element_s raptor_rss_element;


static void
raptor_free_rss_element(raptor_rss_element *rss_element)
{
  if(rss_element->uri)
    raptor_free_uri(rss_element->uri);
  if(rss_element->rel)
    raptor_free_memory((void*)rss_element->rel);
  if(rss_element->type == RAPTOR_RSS_CONTENT_TYPE_XML) {
    if(rss_element->xml_writer)
      raptor_free_xml_writer(rss_element->xml_writer);
    if(rss_element->iostream)
      raptor_free_iostream(rss_element->iostream);
    if(rss_element->xml_content)
      raptor_free_memory(rss_element->xml_content);
  }
  if(rss_element->sb)
    raptor_free_stringbuffer(rss_element->sb);

  RAPTOR_FREE(raptor_rss_element, rss_element);
}


static int
raptor_rss_parse_init(raptor_parser* rdf_parser, const char *name)
{
  raptor_rss_parser* rss_parser=(raptor_rss_parser*)rdf_parser->context;
  raptor_sax2* sax2;
  const raptor_uri_handler *uri_handler;
  void *uri_context;
  int n;

  raptor_rss_common_init();

  raptor_rss_model_init(&rss_parser->model);

  rss_parser->prev_type=RAPTOR_RSS_NONE;
  rss_parser->current_field=RAPTOR_RSS_FIELD_NONE;
  rss_parser->current_type=RAPTOR_RSS_NONE;

  if(rss_parser->sax2) {
    raptor_free_sax2(rss_parser->sax2);
    rss_parser->sax2=NULL;
  }

  raptor_uri_get_handler(&uri_handler, &uri_context);

  rss_parser->nstack=raptor_new_namespaces(uri_handler, uri_context,
                                           NULL, NULL, /* errors */
                                           1);

  /* Initialise the namespaces */
  for(n=0; n < RAPTOR_RSS_NAMESPACES_SIZE; n++) {
    unsigned const char* prefix=(unsigned const char*)raptor_rss_namespaces_info[n].prefix;
    raptor_uri* uri=raptor_rss_namespaces_info[n].uri;
    raptor_namespace* nspace=NULL;

    if(prefix && uri)
      nspace=raptor_new_namespace_from_uri(rss_parser->nstack,
                                           prefix, uri, 0);
    rss_parser->nspaces[n]=nspace;
  }

  sax2=raptor_new_sax2(rdf_parser, &rdf_parser->error_handlers);
  rss_parser->sax2=sax2;

  raptor_sax2_set_start_element_handler(sax2, raptor_rss_start_element_handler);
  raptor_sax2_set_end_element_handler(sax2, raptor_rss_end_element_handler);
  raptor_sax2_set_characters_handler(sax2, raptor_rss_cdata_handler);
  raptor_sax2_set_cdata_handler(sax2, raptor_rss_cdata_handler);
  raptor_sax2_set_comment_handler(sax2, raptor_rss_comment_handler);

  return 0;
}


static void
raptor_rss_parse_terminate(raptor_parser *rdf_parser)
{
  raptor_rss_parser *rss_parser=(raptor_rss_parser*)rdf_parser->context;
  int n;
  
  if(rss_parser->sax2)
    raptor_free_sax2(rss_parser->sax2);

  raptor_rss_model_clear(&rss_parser->model);

  /* Initialise the namespaces */
  for(n=0; n < RAPTOR_RSS_NAMESPACES_SIZE; n++) {
    if(rss_parser->nspaces[n])
      raptor_free_namespace(rss_parser->nspaces[n]);
  }

  if(rss_parser->nstack)
    raptor_free_namespaces(rss_parser->nstack);

  raptor_rss_common_terminate();
}


static int
raptor_rss_parse_start(raptor_parser *rdf_parser) 
{
  raptor_uri *uri=rdf_parser->base_uri;
  raptor_rss_parser* rss_parser=(raptor_rss_parser*)rdf_parser->context;
  
  /* base URI required for RSS */
  if(!uri)
    return 1;

  /* Optionally forbid network requests in the XML parser */
  raptor_sax2_set_feature(rss_parser->sax2, 
                          RAPTOR_FEATURE_NO_NET,
                          rdf_parser->features[RAPTOR_FEATURE_NO_NET]);
  
  raptor_sax2_parse_start(rss_parser->sax2, uri);

  return 0;
}



static void
raptor_rss_start_element_handler(void *user_data,
                                 raptor_xml_element* xml_element)
{
  raptor_parser *rdf_parser;
  raptor_rss_parser *rss_parser;
  raptor_rss_enclosure *enclosure=NULL;
  raptor_uri* base_uri;
  raptor_qname *el_qname;
  const unsigned char *name;
  int ns_attributes_count;
  raptor_qname** named_attrs;
  const raptor_namespace* el_nspace;
  raptor_rss_element* rss_element;

  rss_element=(raptor_rss_element*)RAPTOR_CALLOC(raptor_rss_element, sizeof(raptor_rss_element), 1);
  rss_element->sb=raptor_new_stringbuffer();

  xml_element->user_data=rss_element;

  if(xml_element->parent) {
    raptor_rss_element* parent_rss_element=(raptor_rss_element*)(xml_element->parent->user_data);
    if(parent_rss_element->xml_writer)
      rss_element->xml_writer=parent_rss_element->xml_writer;
  }

  if(rss_element->xml_writer) {
    raptor_xml_writer_start_element(rss_element->xml_writer, xml_element);
    return;
  }


  el_qname=raptor_xml_element_get_name(xml_element);
  name=el_qname->local_name;
  el_nspace=el_qname->nspace;

  rdf_parser=(raptor_parser*)user_data;
  rss_parser=(raptor_rss_parser*)rdf_parser->context;
  
  base_uri=raptor_sax2_inscope_base_uri(rss_parser->sax2);

  if(rss_parser->current_type == RAPTOR_RSS_NONE) {
    if(!strcmp((const char*)name, "rss") || 
       !strcmp((const char*)name, "rdf") || 
       !strcmp((const char*)name, "RDF")) {
      /* rss */
      goto check_attributes;
    } else if(!raptor_strcasecmp((const char*)name, "channel")) {
      /* rss or atom 0.3 channel */
      rss_parser->current_type=RAPTOR_RSS_CHANNEL;
    } else if(!strcmp((const char*)name, "feed")) {
      /* atom 1.0 feed */
      rss_parser->current_type=RAPTOR_RSS_CHANNEL;
      rss_parser->is_atom=1;
    } else if(!strcmp((const char*)name, "item")) {
      raptor_rss_model_add_item(&rss_parser->model);
      rss_parser->current_type=RAPTOR_RSS_ITEM;
    } else if(!strcmp((const char*)name, "entry")) {
      raptor_rss_model_add_item(&rss_parser->model);
      rss_parser->current_type=RAPTOR_RSS_ITEM;
      rss_parser->is_atom=1;
    } else {
      int i;
      rss_parser->current_type=RAPTOR_RSS_UNKNOWN;
      for(i=0; i<RAPTOR_RSS_COMMON_SIZE; i++)
        if(!strcmp((const char*)name, raptor_rss_types_info[i].name)) {
          rss_parser->current_type=(raptor_rss_type)i;
          break;
        }
    }
    
    if(rss_parser->current_type == RAPTOR_RSS_UNKNOWN) {
      RAPTOR_DEBUG2("Unknown start element named %s\n", name);
    } else {
      RAPTOR_DEBUG3("FOUND type %d - %s\n", rss_parser->current_type, raptor_rss_types_info[rss_parser->current_type].name);
      if (rss_parser->current_type != RAPTOR_RSS_ITEM)
        raptor_rss_model_add_common(&rss_parser->model, 
                                    rss_parser->current_type);
    }
  } else { /* have current_type, this is an element inside */
    int i;
    raptor_rss_type old_type=rss_parser->current_type;
    
    /* check it is not a type here */
    if(!strcmp((const char*)name, "item") ||
       !strcmp((const char*)name, "entry")) {
      raptor_rss_model_add_item(&rss_parser->model);
      rss_parser->current_type=RAPTOR_RSS_ITEM;
    } else {
      for(i=0; i<RAPTOR_RSS_COMMON_SIZE; i++)
        if(!strcmp((const char*)name, raptor_rss_types_info[i].name)) {
          /* rss and atom clash on the author name field (rss) or type (atom) */
          if(i != RAPTOR_ATOM_AUTHOR ||
             (i == RAPTOR_ATOM_AUTHOR && rss_parser->is_atom)) {
            rss_parser->current_type=(raptor_rss_type)i;
            break;
          }
        }
    }
    
    if(rss_parser->current_type != old_type) {
      RAPTOR_DEBUG6("FOUND element %s for type %d - %s INSIDE current type %d - %s\n", name, rss_parser->current_type, raptor_rss_types_info[rss_parser->current_type].name, old_type, raptor_rss_types_info[old_type].name);
      raptor_rss_model_add_common(&rss_parser->model,
                                  rss_parser->current_type);
      rss_parser->prev_type=old_type;
      goto check_attributes;
    }
    
    rss_parser->current_field=RAPTOR_RSS_FIELD_UNKNOWN;
    for(i=0; i<RAPTOR_RSS_FIELDS_SIZE; i++)
      if(!strcmp((const char*)name, raptor_rss_fields_info[i].name)) {
        raptor_uri* nspace_URI=el_nspace ? raptor_namespace_get_uri(el_nspace) : NULL;

        /* RSS 0.9 and RSS 1.1 namespaces => RSS 1.0 namespace */
        if(nspace_URI &&
           (raptor_uri_equals(nspace_URI, raptor_rss_namespaces_info[RSS0_9_NS].uri) ||
            raptor_uri_equals(nspace_URI, raptor_rss_namespaces_info[RSS1_1_NS].uri))) {
          nspace_URI=raptor_rss_namespaces_info[RSS1_0_NS].uri;
        }
        
        /* Atom 0.3 namespace => Atom 1.0 namespace */
        if(nspace_URI &&
           raptor_uri_equals(nspace_URI, raptor_rss_namespaces_info[ATOM0_3_NS].uri)) {
          nspace_URI=raptor_rss_namespaces_info[ATOM1_0_NS].uri;
        }
        
        if(nspace_URI && raptor_rss_fields_info[i].nspace != RSS_NO_NS) {
          raptor_uri* field_nspace_URI=raptor_rss_namespaces_info[raptor_rss_fields_info[i].nspace].uri;

          if(raptor_uri_equals(nspace_URI, field_nspace_URI)) {
            rss_parser->current_field=(raptor_rss_fields_type)i;
            break;
          }
        } else {
          rss_parser->current_field=(raptor_rss_fields_type)i;
          break;
        }
      }
    
    if(rss_parser->current_field==RAPTOR_RSS_FIELD_UNKNOWN) {
      RAPTOR_DEBUG3("Unknown field element named %s inside type %s\n", name, raptor_rss_types_info[rss_parser->current_type].name);
    } else if (rss_parser->current_field == RAPTOR_RSS_FIELD_ENCLOSURE ){
      raptor_rss_item* update_item;
      RAPTOR_DEBUG1("FOUND new enclosure\n");
      if(rss_parser->current_type == RAPTOR_RSS_ITEM) {
        update_item=rss_parser->model.last;
        enclosure=raptor_rss_new_enclosure();
        raptor_rss_item_add_enclosure(update_item, enclosure);
      }
    } else {
      RAPTOR_DEBUG4("FOUND field %d - %s inside type %s\n", rss_parser->current_field, raptor_rss_fields_info[rss_parser->current_field].name, raptor_rss_types_info[rss_parser->current_type].name);
      
      /* Rewrite item fields */
      for(i=0; raptor_atom_to_rss[i].from != RAPTOR_RSS_FIELD_UNKNOWN; i++) {
        if(raptor_atom_to_rss[i].from == rss_parser->current_field) {
          rss_parser->current_field=raptor_atom_to_rss[i].to;
          
          RAPTOR_DEBUG3("Rewrote into field %d - %s\n", rss_parser->current_field, raptor_rss_fields_info[rss_parser->current_field].name);
          break;
        }
      }
      
    }
  }
  
 check_attributes:
  named_attrs=raptor_xml_element_get_attributes(xml_element);
  ns_attributes_count=raptor_xml_element_get_attributes_count(xml_element);

  /* Now check for attributes */
  if(named_attrs && ns_attributes_count) {
    int i;

    for (i = 0; i < ns_attributes_count; i++) {
      raptor_qname* attr=named_attrs[i];
      const unsigned char* attrName = attr->local_name;
      const unsigned char* attrValue = attr->value;
      RAPTOR_DEBUG3("  attribute %s=%s\n", attrName, attrValue);

      /* Pick a few attributes to care about */
      if(!strcmp((const char*)attrName, "isPermaLink")) {
        raptor_rss_item* update_item=rss_parser->model.last;
        if(!strcmp((const char*)name, "guid")) {
          /* <guid isPermaLink="..."> */
          if(update_item) {
            raptor_rss_field* field=raptor_rss_new_field();
            RAPTOR_DEBUG1("fa1 - ");
            raptor_rss_item_add_field(update_item, RAPTOR_RSS_FIELD_GUID, field);
            if(!strcmp((const char*)attrValue, "true")) {
              RAPTOR_DEBUG2("    setting guid to URI '%s'\n", attrValue);
              field->uri=raptor_new_uri_relative_to_base(base_uri,
                                                         (const unsigned char*)attrValue);
            } else {
              size_t len=strlen((const char*)attrValue);
              RAPTOR_DEBUG2("    setting guid to string '%s'\n", attrValue);
              field->value=(unsigned char*)RAPTOR_MALLOC(cstring, len+1);
              strncpy((char*)field->value, (char*)attrValue, len+1);
            }
          }
        }
      } else if(!strcmp((const char*)attrName, "url")) {
        if(!strcmp((const char*)name, "source")) {
          /* <source url="...">foo</source> */
          if(rss_parser->model.last) {
            /*
              rss_parser->last->source_url=attrValue; 
              attrValue=NULL;
            */
          }
        } else if (!strcmp((const char*)name, "enclosure") && enclosure) {
          RAPTOR_DEBUG2("  setting enclosure URL %s\n", attrValue);
          enclosure->url=raptor_new_uri_relative_to_base(base_uri,
                                                         (const unsigned char*)attrValue);
        }
      } else if(!strcmp((const char*)attrName, "domain")) {
        if(!strcmp((const char*)name, "category")) {
          /* <category domain="URL">foo</source> */
          if(rss_parser->model.last) {
            /*
              rss_parser->last->category_url=attrValue; 
              attrValue=NULL;
            */
          }
        }
      } else if(!strcmp((const char*)attrName, "rel")) {
        size_t len=strlen((const char*)attrValue);
        RAPTOR_DEBUG2("  setting rel length %s\n", attrValue);
        rss_element->rel=(unsigned char*)RAPTOR_MALLOC(cstring, len+1);
        strncpy((char*)rss_element->rel, (const char*)attrValue, len+1);
        attrValue=NULL;
      } else if(!strcmp((const char*)attrName, "href")) {
        if(rss_parser->current_field == RAPTOR_RSS_FIELD_LINK ||
           rss_parser->current_field == RAPTOR_RSS_FIELD_ATOM_LINK) {
          RAPTOR_DEBUG2("  setting href as URI string for type %s\n", raptor_rss_types_info[rss_parser->current_type].name);
          if(rss_element->uri)
            raptor_free_uri(rss_element->uri);
          rss_element->uri=raptor_new_uri_relative_to_base(base_uri,
                                                           (const unsigned char*)attrValue);
        }
      } else if (!strcmp((const char*)attrName, "length")) {
        if (!strcmp((const char*)name, "enclosure") && enclosure) {
          size_t len=strlen((const char*)attrValue);
          RAPTOR_DEBUG2("  setting enclosure length %s\n", attrValue);
          enclosure->length=(char*)RAPTOR_MALLOC(cstring, len+1);
          strncpy(enclosure->length, (char*)attrValue, len+1);
        }
      } else if (!strcmp((const char*)attrName, "type")) {
        if (!strcmp((const char*)name, "enclosure") && enclosure) {
          size_t len=strlen((const char*)attrValue);
          RAPTOR_DEBUG2("  setting enclosure type %s\n", attrValue);
          enclosure->type=(char*)RAPTOR_MALLOC(cstring, len+1);
          strncpy(enclosure->type, (char*)attrValue, len+1);
        } else if(rss_parser->current_field == RAPTOR_RSS_FIELD_ATOM_LINK) {
          /* do nothing with atom link attribute type */
        } else if(rss_parser->is_atom) {
          /* Atom only typing */
          if (!strcmp((const char*)attrValue, "xhtml") ||
              !strcmp((const char*)attrValue, "xml") ||
              strstr((const char*)attrValue, "+xml")) {
            const raptor_uri_handler *uri_handler;
            void *uri_context;

            RAPTOR_DEBUG2("  found type '%s', making an XML writer\n", 
                          attrValue);
            
            raptor_uri_get_handler(&uri_handler, &uri_context);
            rss_element->type=RAPTOR_RSS_CONTENT_TYPE_XML;
            rss_element->iostream=raptor_new_iostream_to_string(&rss_element->xml_content, &rss_element->xml_content_length, raptor_alloc_memory);
            rss_element->xml_writer=raptor_new_xml_writer(NULL,
                                                          uri_handler, uri_context,
                                                          rss_element->iostream,
                                                          (raptor_simple_message_handler)raptor_parser_simple_error, rdf_parser,
                                                          1);
            raptor_xml_writer_set_feature(rss_element->xml_writer, 
                                          RAPTOR_FEATURE_WRITER_XML_DECLARATION, 0);

            raptor_free_stringbuffer(rss_element->sb);
            rss_element->sb=NULL;

          }
        }
      } else if (!strcmp((const char*)attrName, "version")) {
        if(!raptor_strcasecmp((const char*)name, "feed")) {
          if(!strcmp((const char*)attrValue, "0.3"))
            rss_parser->is_atom=1;
        }
      }
    }
  } /* if have attributes */
}


static void
raptor_rss_end_element_handler(void *user_data, 
                               raptor_xml_element* xml_element)
{
  raptor_parser* rdf_parser;
  raptor_rss_parser* rss_parser;
#ifdef RAPTOR_DEBUG
  const unsigned char* name=raptor_xml_element_get_name(xml_element)->local_name;
#endif
  raptor_rss_element* rss_element;
  size_t cdata_len=0;
  unsigned char* cdata=NULL;

  rss_element=(raptor_rss_element*)xml_element->user_data;

  rdf_parser=(raptor_parser*)user_data;
  rss_parser=(raptor_rss_parser*)rdf_parser->context;

  if(rss_element->xml_writer) {
    if(rss_element->type != RAPTOR_RSS_CONTENT_TYPE_XML) {
      raptor_xml_writer_end_element(rss_element->xml_writer, xml_element);
      goto tidy_end_element;
    }

    /* otherwise we are done making XML */
    raptor_free_iostream(rss_element->iostream);
    rss_element->iostream=NULL;
    cdata=(unsigned char*)rss_element->xml_content;
    cdata_len=rss_element->xml_content_length;
  }

  if(rss_element->sb) {
    cdata_len=raptor_stringbuffer_length(rss_element->sb);
    cdata=raptor_stringbuffer_as_string(rss_element->sb);
  }

  if(cdata) {
    raptor_uri* base_uri=NULL;
    
    base_uri=raptor_sax2_inscope_base_uri(rss_parser->sax2);

    if((rss_parser->current_type==RAPTOR_RSS_NONE ||
        rss_parser->current_type==RAPTOR_RSS_UNKNOWN) ||
       (rss_parser->current_field==RAPTOR_RSS_FIELD_NONE ||
        rss_parser->current_field==RAPTOR_RSS_FIELD_UNKNOWN)) {
      unsigned char *p=cdata;
      int i;
      for(i=cdata_len; i>0 && *p; i--) {
        if(!isspace(*p))
          break;
        p++;
      }
      if(i>0 && *p) {
        RAPTOR_DEBUG4("IGNORING non-whitespace text '%s' inside type %s, field %s\n", cdata,
                      raptor_rss_types_info[rss_parser->current_type].name,
                      raptor_rss_fields_info[rss_parser->current_field].name);
      }

      goto do_end_element;
    }

    if(rss_parser->current_type >= RAPTOR_RSS_COMMON_IGNORED) {
      /* skipHours, skipDays common but IGNORED */ 
      RAPTOR_DEBUG2("Ignoring fields for type %s\n", raptor_rss_types_info[rss_parser->current_type].name);
    } else {
      raptor_rss_item* update_item;
      raptor_rss_field* field=raptor_rss_new_field();

      if(rss_parser->current_type == RAPTOR_RSS_ITEM)
        update_item=rss_parser->model.last;
      else
        update_item=raptor_rss_model_get_common(&rss_parser->model,
                                                rss_parser->current_type);

      /* if value is always an uri, make it so */
      if(raptor_rss_fields_info[rss_parser->current_field].flags & 
         RAPTOR_RSS_INFO_FLAG_URI_VALUE) {
        RAPTOR_DEBUG4("Added URI %s to field %s of type %s\n", cdata, raptor_rss_fields_info[rss_parser->current_field].name, raptor_rss_types_info[rss_parser->current_type].name);
        field->uri=raptor_new_uri_relative_to_base(base_uri, cdata);
      } else {
        RAPTOR_DEBUG4("Added text '%s' to field %s of type %s\n", cdata, raptor_rss_fields_info[rss_parser->current_field].name, raptor_rss_types_info[rss_parser->current_type].name);
        field->uri=NULL;
        field->value=(unsigned char*)RAPTOR_MALLOC(cstring, cdata_len+1);
        strncpy((char*)field->value, (const char*)cdata, cdata_len);
        field->value[cdata_len]='\0';
      }

      RAPTOR_DEBUG1("fa3 - ");
      raptor_rss_item_add_field(update_item, rss_parser->current_field, field);
    }
  } /* end if contained cdata */
  

  if(raptor_xml_element_is_empty(xml_element)) {
    /* Empty element, so consider adding one of the attributes as
     * literal or URI content
     */
    if(rss_parser->current_type >= RAPTOR_RSS_COMMON_IGNORED) {
      /* skipHours, skipDays common but IGNORED */ 
      RAPTOR_DEBUG3("Ignoring empty element %s for type %s\n", name, raptor_rss_types_info[rss_parser->current_type].name);
    } else if(rss_element->uri) {
      raptor_rss_item* update_item;
      raptor_rss_field* field=raptor_rss_new_field();

      if(rss_parser->current_type == RAPTOR_RSS_ITEM)
        update_item=rss_parser->model.last;
      else
        update_item=raptor_rss_model_get_common(&rss_parser->model,
                                                rss_parser->current_type);

      if(rss_parser->current_field == RAPTOR_RSS_FIELD_LINK &&
         rss_element->rel && 
         !strcmp((const char*)rss_element->rel, "alternate")) {
        /* RSS with rel != alternate ignored FIXME */
      } else if(rss_parser->current_field == RAPTOR_RSS_FIELD_UNKNOWN) {
        RAPTOR_DEBUG2("Cannot add URI from alternate attribute to type %s unknown field\n", raptor_rss_types_info[rss_parser->current_type].name);
        raptor_rss_field_free(field);
      } else {
        RAPTOR_DEBUG3("Added URI to field %s of type %s\n", raptor_rss_fields_info[rss_parser->current_field].name, raptor_rss_types_info[rss_parser->current_type].name);
        field->uri=rss_element->uri;
        rss_element->uri=NULL;
        RAPTOR_DEBUG1("fa2 - ");
        raptor_rss_item_add_field(update_item, rss_parser->current_field, field);
      }
    }

  }

 do_end_element:
  if(rss_parser->current_type != RAPTOR_RSS_NONE) {
    if(rss_parser->current_field != RAPTOR_RSS_FIELD_NONE) {
      RAPTOR_DEBUG3("Ending element %s field %s\n", name, raptor_rss_fields_info[rss_parser->current_field].name);
      rss_parser->current_field= RAPTOR_RSS_FIELD_NONE;
    } else {
      RAPTOR_DEBUG3("Ending element %s type %s\n", name, raptor_rss_types_info[rss_parser->current_type].name);
      if(rss_parser->prev_type != RAPTOR_RSS_NONE) {
        rss_parser->current_type=rss_parser->prev_type;
        rss_parser->prev_type=RAPTOR_RSS_NONE;
        RAPTOR_DEBUG3("Returning to type %d - %s\n", rss_parser->current_type, raptor_rss_types_info[rss_parser->current_type].name);
      } else
        rss_parser->current_type= RAPTOR_RSS_NONE;
    }
  }

 tidy_end_element:

  if(rss_element)
    raptor_free_rss_element(rss_element);

}



static void
raptor_rss_cdata_handler(void *user_data, raptor_xml_element* xml_element,
                         const unsigned char *s, int len)
{      
  raptor_rss_element* rss_element;

  rss_element=(raptor_rss_element*)xml_element->user_data;

  if(rss_element->xml_writer) {
    raptor_xml_writer_cdata_counted(rss_element->xml_writer, s, len);
    return;
  }

  raptor_stringbuffer_append_counted_string(rss_element->sb, s, len, 1);
}      
      

static void
raptor_rss_comment_handler(void *user_data, raptor_xml_element* xml_element,
                           const unsigned char *s)
{
  raptor_rss_element* rss_element;

  if(!xml_element)
    return;
  
  rss_element=(raptor_rss_element*)xml_element->user_data;

  if(rss_element->xml_writer) {
    raptor_xml_writer_comment(rss_element->xml_writer, s);
    return;
  }
}


static void
raptor_rss_insert_enclosure_identifiers(raptor_parser* rdf_parser, 
                                        raptor_rss_enclosure *enclosure)
{
  raptor_identifier* identifier=&enclosure->identifier;
  if (enclosure->url) { 
    /* emit as URI resource */
    identifier->uri=raptor_uri_copy(enclosure->url);
    identifier->type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
    identifier->uri_source=RAPTOR_URI_SOURCE_URI;
  } else { 
    /* emit as blank node */
    identifier->id=raptor_parser_internal_generate_id(rdf_parser, RAPTOR_GENID_TYPE_BNODEID, NULL);
    identifier->type=RAPTOR_IDENTIFIER_TYPE_ANONYMOUS;
    identifier->uri_source=RAPTOR_URI_SOURCE_GENERATED;
  }
  enclosure->node_type=raptor_rss_types_info[RAPTOR_RSS_ENCLOSURE].uri;
}


static void
raptor_rss_insert_identifiers(raptor_parser* rdf_parser) 
{
  raptor_rss_parser* rss_parser=(raptor_rss_parser*)rdf_parser->context;
  int i;
  raptor_rss_item* item;
  
  for(i=0; i< RAPTOR_RSS_COMMON_SIZE; i++) {
    for(item=rss_parser->model.common[i]; item; item=item->next) {
      raptor_identifier* identifier;
      identifier=&(item->identifier);
	
      if(!item->fields_count)
        continue;
      
      RAPTOR_DEBUG3("Inserting identifiers in common type %d - %s\n", i, raptor_rss_types_info[i].name);
    
      if(item->uri) {
        identifier->uri=raptor_uri_copy(item->uri);
        identifier->type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
        identifier->uri_source=RAPTOR_URI_SOURCE_URI;
      } else {
        int url_fields[2];
        int url_fields_count=1;
        int f;
      
        url_fields[0]=(i== RAPTOR_RSS_IMAGE) ? RAPTOR_RSS_FIELD_URL :
                                             RAPTOR_RSS_FIELD_LINK;
        if(i == RAPTOR_RSS_CHANNEL) {
          url_fields[1]=RAPTOR_RSS_FIELD_ATOM_ID;
          url_fields_count++;
        }

        for(f=0; f < url_fields_count; f++) {
          raptor_rss_field* field;

          for(field=item->fields[url_fields[f]]; field; field=field->next) {
            if(field->value) {
              identifier->uri=raptor_new_uri((const unsigned char*)field->value);
              identifier->type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
              identifier->uri_source=RAPTOR_URI_SOURCE_URI;
              break;
            } else if(field->uri) {
              identifier->uri=raptor_uri_copy(field->uri);
              identifier->type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
              identifier->uri_source=RAPTOR_URI_SOURCE_URI;
              break;
            }
          }
        }
      
        if(!identifier->uri) {
          /* need to make bnode */
          identifier->id=raptor_parser_internal_generate_id(rdf_parser, RAPTOR_GENID_TYPE_BNODEID, NULL);
          identifier->type=RAPTOR_IDENTIFIER_TYPE_ANONYMOUS;
          identifier->uri_source=RAPTOR_URI_SOURCE_GENERATED;
        }
      }
    
      item->node_type=&raptor_rss_types_info[i];
    }
  }
  /* sequence of rss:item */
  for(item=rss_parser->model.items; item; item=item->next) {
    raptor_identifier* identifier=&item->identifier;
    raptor_rss_enclosure* enclosure;
    
    if(item->uri) {
      identifier->uri=raptor_uri_copy(item->uri);
      identifier->type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
      identifier->uri_source=RAPTOR_URI_SOURCE_URI;
    } else {
      if (item->fields[RAPTOR_RSS_FIELD_LINK]) {
        if (item->fields[RAPTOR_RSS_FIELD_LINK]->value) {
          identifier->uri=raptor_new_uri((const unsigned char*)item->fields[RAPTOR_RSS_FIELD_LINK]->value);
          identifier->type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
          identifier->uri_source=RAPTOR_URI_SOURCE_URI;
        } else if(item->fields[RAPTOR_RSS_FIELD_LINK]->uri) {
          identifier->uri=raptor_uri_copy(item->fields[RAPTOR_RSS_FIELD_LINK]->uri);
          identifier->type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
          identifier->uri_source=RAPTOR_URI_SOURCE_URI;
        }
      } else if(item->fields[RAPTOR_RSS_FIELD_ATOM_ID]) {
        if (item->fields[RAPTOR_RSS_FIELD_ATOM_ID]->value) {
          identifier->uri=raptor_new_uri((const unsigned char*)item->fields[RAPTOR_RSS_FIELD_ATOM_ID]->value);
          identifier->type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
          identifier->uri_source=RAPTOR_URI_SOURCE_URI;
        } else if(item->fields[RAPTOR_RSS_FIELD_ATOM_ID]->uri) {
          identifier->uri=raptor_uri_copy(item->fields[RAPTOR_RSS_FIELD_ATOM_ID]->uri);
          identifier->type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
          identifier->uri_source=RAPTOR_URI_SOURCE_URI;
        }
      } else {
        /* need to make bnode */
        identifier->id=raptor_parser_internal_generate_id(rdf_parser, RAPTOR_GENID_TYPE_BNODEID, NULL);
        identifier->type=RAPTOR_IDENTIFIER_TYPE_ANONYMOUS;
        identifier->uri_source=RAPTOR_URI_SOURCE_GENERATED;
      }
    }
    
    for(enclosure=item->enclosure; enclosure; enclosure=enclosure->next)
      raptor_rss_insert_enclosure_identifiers(rdf_parser, enclosure);
    
    item->node_type=&raptor_rss_types_info[RAPTOR_RSS_ITEM];
  }
}


static int
raptor_rss_emit_type_triple(raptor_parser* rdf_parser, 
                            raptor_identifier *resource,
                            raptor_uri *type_uri) 
{
  raptor_rss_parser* rss_parser=(raptor_rss_parser*)rdf_parser->context;

  if(!resource->uri && !resource->id) {
    raptor_parser_error(rdf_parser, "RSS node has no identifier");
    return 1;
  }

  rss_parser->statement.subject=resource->uri ? (void*)resource->uri : (void*)resource->id;
  rss_parser->statement.subject_type=resource->type;
  
  rss_parser->statement.predicate=RAPTOR_RSS_RDF_type_URI(&rss_parser->model);
  rss_parser->statement.predicate_type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;

  rss_parser->statement.object=(void*)type_uri;
  rss_parser->statement.object_type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
  rss_parser->statement.object_literal_language=NULL;
  rss_parser->statement.object_literal_datatype=NULL;
  
  /* Generate the statement */
  (*rdf_parser->statement_handler)(rdf_parser->user_data, &rss_parser->statement);
  return 0;
}


static int
raptor_rss_emit_enclosure(raptor_parser* rdf_parser, 
                          raptor_rss_enclosure *enclosure)
{
  raptor_rss_parser* rss_parser=(raptor_rss_parser*)rdf_parser->context;
  raptor_identifier* identifier=&enclosure->identifier;
  const void* subject=rss_parser->statement.subject;

  if(!identifier->uri && !identifier->id) {
    raptor_parser_error(rdf_parser, "Enclosure has no identifier");
    return 1;
  }

  rss_parser->statement.predicate=raptor_rss_fields_info[RAPTOR_RSS_RDF_ENCLOSURE].uri;
  rss_parser->statement.predicate_type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
  
  if (identifier->uri) { 
    /* emit as resource */
    rss_parser->statement.object=identifier->uri;
    rss_parser->statement.object_type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;	  
  } else { 
    /* emit as blank node */
    rss_parser->statement.object=identifier->id;
    rss_parser->statement.object_type=RAPTOR_IDENTIFIER_TYPE_ANONYMOUS;
  }
  rss_parser->statement.object_literal_language=NULL;
  rss_parser->statement.object_literal_datatype=NULL;

  (*rdf_parser->statement_handler)(rdf_parser->user_data, &rss_parser->statement);

  if(raptor_rss_emit_type_triple(rdf_parser, identifier, enclosure->node_type))
    return 1;

  if (enclosure->url) {
    rss_parser->statement.predicate=raptor_rss_fields_info[RAPTOR_RSS_RDF_ENCLOSURE_URL].uri;
    rss_parser->statement.object=enclosure->url;
    rss_parser->statement.object_type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
    (*rdf_parser->statement_handler)(rdf_parser->user_data, &rss_parser->statement);
  }

  if (enclosure->type) {
    rss_parser->statement.predicate=raptor_rss_fields_info[RAPTOR_RSS_RDF_ENCLOSURE_TYPE].uri;
    rss_parser->statement.object=enclosure->type;
    rss_parser->statement.object_type=RAPTOR_IDENTIFIER_TYPE_LITERAL;
    (*rdf_parser->statement_handler)(rdf_parser->user_data, &rss_parser->statement);
  }

  if (enclosure->length) {
    rss_parser->statement.predicate=raptor_rss_fields_info[RAPTOR_RSS_RDF_ENCLOSURE_LENGTH].uri;
    rss_parser->statement.object=enclosure->length;
    rss_parser->statement.object_type=RAPTOR_IDENTIFIER_TYPE_LITERAL;
    (*rdf_parser->statement_handler)(rdf_parser->user_data, &rss_parser->statement);
  }

  rss_parser->statement.subject=subject;
  return 0;
}


static int
raptor_rss_emit_item(raptor_parser* rdf_parser, raptor_rss_item *item) 
{
  raptor_rss_parser* rss_parser=(raptor_rss_parser*)rdf_parser->context;
  int f;
  raptor_identifier* identifier=&item->identifier;
  raptor_rss_enclosure* enclosure;
    
  if(!item->fields_count)
    return 0;

  if(raptor_rss_emit_type_triple(rdf_parser, identifier, item->node_type->uri))
    return 1;

  for(f=0; f< RAPTOR_RSS_FIELDS_SIZE; f++) {
    raptor_rss_field* field;
    
    /* This is only made by a connection */	  
    if(f == RAPTOR_RSS_FIELD_ITEMS)
      continue;
	
    rss_parser->statement.predicate=raptor_rss_fields_info[f].uri;
    if(!rss_parser->statement.predicate)
      continue;
    
    rss_parser->statement.predicate_type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;

    for (field=item->fields[f]; field; field=field->next) {
      rss_parser->statement.object_literal_language=NULL;
      rss_parser->statement.object_literal_datatype=NULL;
      if(field->value) {
        rss_parser->statement.object=field->value;
        rss_parser->statement.object_type=RAPTOR_IDENTIFIER_TYPE_LITERAL;
        /* FIXME - should store and emit languages */
      } else {
        rss_parser->statement.object=field->uri;
        rss_parser->statement.object_type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
      }
      
      /* Generate the statement */
      (*rdf_parser->statement_handler)(rdf_parser->user_data, &rss_parser->statement);
    }
  }

  for(enclosure=item->enclosure; enclosure; enclosure=enclosure->next) {
    raptor_rss_emit_enclosure(rdf_parser, enclosure);
  }

  return 0;
}


static int
raptor_rss_emit_connection(raptor_parser* rdf_parser,
                           raptor_identifier *subject_identifier,
                           raptor_uri predicate_uri, int predicate_ordinal,
                           raptor_identifier *object_identifier) 
{
  raptor_rss_parser* rss_parser=(raptor_rss_parser*)rdf_parser->context;

  if(!subject_identifier->uri && !subject_identifier->id) {
    raptor_parser_error(rdf_parser, "Connection subject has no identifier");
    return 1;
  }

  rss_parser->statement.subject=subject_identifier->uri ? (void*)subject_identifier->uri : (void*)subject_identifier->id;
  rss_parser->statement.subject_type=subject_identifier->type;

  if(predicate_uri) {
    rss_parser->statement.predicate=predicate_uri;
    rss_parser->statement.predicate_type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
  } else {
    rss_parser->statement.predicate=(void*)&predicate_ordinal;
    rss_parser->statement.predicate_type=RAPTOR_IDENTIFIER_TYPE_ORDINAL;
  }
  
  
  rss_parser->statement.object=object_identifier->uri ? (void*)object_identifier->uri : (void*)object_identifier->id;
  rss_parser->statement.object_type=object_identifier->type;
  rss_parser->statement.object_literal_language=NULL;
  rss_parser->statement.object_literal_datatype=NULL;
  
  /* Generate the statement */
  (*rdf_parser->statement_handler)(rdf_parser->user_data, &rss_parser->statement);

  return 0;
}


static int
raptor_rss_emit(raptor_parser* rdf_parser)
{
  raptor_rss_parser* rss_parser=(raptor_rss_parser*)rdf_parser->context;
  int i;
  raptor_rss_item* item;

  if (!rss_parser->model.common[RAPTOR_RSS_CHANNEL]) {
    raptor_parser_error(rdf_parser, "No RSS channel item present");
    return 1;
  }
  
  if(!rss_parser->model.common[RAPTOR_RSS_CHANNEL]->identifier.uri &&
     !rss_parser->model.common[RAPTOR_RSS_CHANNEL]->identifier.id) {
    raptor_parser_error(rdf_parser, "RSS channel has no identifier");
    return 1;
  }

  for (i=0; i< RAPTOR_RSS_COMMON_SIZE; i++) {
    for (item=rss_parser->model.common[i]; item; item=item->next) {
      if(!item->fields_count)
        continue;
      
      RAPTOR_DEBUG3("Emitting type %i - %s\n", i, raptor_rss_types_info[i].name);
      
      if(!item->identifier.uri && !item->identifier.id) {
        raptor_parser_error(rdf_parser, "RSS %s has no identifier", raptor_rss_types_info[i].name);
        return 1;
      }
    
      if(raptor_rss_emit_item(rdf_parser, item))
        return 1;

      /* Add connections to channel */
      if(i != RAPTOR_RSS_CHANNEL) {
        if(raptor_rss_emit_connection(rdf_parser,
                                      &(rss_parser->model.common[RAPTOR_RSS_CHANNEL]->identifier),
                                      raptor_rss_types_info[i].uri, 0,
                                      &(item->identifier)))
          return 1;
      }
    }
  }

  if(rss_parser->model.items_count) {
    raptor_identifier *items;
    
    /* make a new genid for the <rdf:Seq> node */
    items=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_ANONYMOUS,
                                NULL, RAPTOR_URI_SOURCE_GENERATED,
                                (const unsigned char*)raptor_parser_internal_generate_id(rdf_parser, RAPTOR_GENID_TYPE_BNODEID, NULL),
                                NULL, NULL, NULL);
  
    /* _:genid1 rdf:type rdf:Seq . */
    if(raptor_rss_emit_type_triple(rdf_parser, items,
                                   RAPTOR_RSS_RDF_Seq_URI(&rss_parser->model))) {
      raptor_free_identifier(items);
      return 1;
    }
    
    /* <channelURI> rss:items _:genid1 . */
    if(raptor_rss_emit_connection(rdf_parser,
                                  &(rss_parser->model.common[RAPTOR_RSS_CHANNEL]->identifier),
                                  raptor_rss_fields_info[RAPTOR_RSS_FIELD_ITEMS].uri, 0,
                                  items)) {
      raptor_free_identifier(items);
      return 1;
    }
    
    /* sequence of rss:item */
    for(i=1, item=rss_parser->model.items; item; item=item->next, i++) {
      
      if(raptor_rss_emit_item(rdf_parser, item) ||
         raptor_rss_emit_connection(rdf_parser,
                                    items,
                                    NULL, i,
                                    &(item->identifier))) {
        raptor_free_identifier(items);
        return 1;
      }
    }

    raptor_free_identifier(items);
  }
  return 0;
}


static const raptor_field_pair raptor_rss_uplift_map[]={
  /* from */                  /* to */
#ifdef RAPTOR_PARSEDATE_FUNCTION
  /* convert to ISO date */
  { RAPTOR_RSS_FIELD_PUBDATE,        RAPTOR_RSS_FIELD_DC_DATE },

  /* default action: copy fields */
  { RAPTOR_RSS_FIELD_ATOM_UPDATED,   RAPTOR_RSS_FIELD_DC_DATE },
#endif
  /* default actions: copy fields */
  { RAPTOR_RSS_FIELD_DESCRIPTION,    RAPTOR_RSS_FIELD_CONTENT_ENCODED },

  { RAPTOR_RSS_FIELD_UNKNOWN, RAPTOR_RSS_FIELD_UNKNOWN }
};


static void
raptor_rss_uplift_fields(raptor_rss_item* item) 
{
  int i;
  
  for(i=0; raptor_rss_uplift_map[i].from != RAPTOR_RSS_FIELD_UNKNOWN; i++) {
    raptor_rss_fields_type from_field=raptor_rss_uplift_map[i].from;
    raptor_rss_fields_type to_field=raptor_rss_uplift_map[i].to;
    raptor_rss_field* field=NULL;
    size_t len;
    
    if(!(item->fields[from_field] && item->fields[from_field]->value))
      continue;
  
    if(from_field == to_field) {
      field=item->fields[from_field];
    } else {
      if(item->fields[to_field] && item->fields[to_field]->value)
        continue;
      field=raptor_rss_new_field();
      raptor_rss_item_add_field(item, to_field, field);
    }

#ifdef RAPTOR_PARSEDATE_FUNCTION
    /* Get rid of date soup */
    if(from_field == RAPTOR_RSS_FIELD_PUBDATE
#if 0
       /* or normalize to UTC */
       ||
       from_field == RAPTOR_RSS_FIELD_ATOM_PUBLISHED ||
       from_field == RAPTOR_RSS_FIELD_ATOM_UPDATED
#endif
       )
      raptor_rss_date_uplift(field, item->fields[from_field]->value);
#endif
    
    if(!field->value) {
      /* Otherwise default action is to copy from_field value */
      len=strlen((const char*)item->fields[from_field]->value);
      
      field->value=(unsigned char*)RAPTOR_MALLOC(cstring, len + 1);
      strncpy((char*)field->value, (const char*)item->fields[from_field]->value, len + 1);
    }
    
  }
}


static void
raptor_rss_uplift_items(raptor_parser* rdf_parser)
{
  raptor_rss_parser* rss_parser=(raptor_rss_parser*)rdf_parser->context;
  int i;
  raptor_rss_item* item;
  
  for(i=0; i< RAPTOR_RSS_COMMON_SIZE; i++) {
    for(item=rss_parser->model.common[i]; item; item=item->next) {
      raptor_rss_uplift_fields(item);
    }
  }

  for(item=rss_parser->model.items; item; item=item->next) {
    raptor_rss_uplift_fields(item);
  }
  
}


static int
raptor_rss_parse_chunk(raptor_parser* rdf_parser, 
                       const unsigned char *s, size_t len,
                       int is_end)
{
  raptor_rss_parser* rss_parser=(raptor_rss_parser*)rdf_parser->context;
  
  if(rdf_parser->failed)
    return 1;

  raptor_sax2_parse_chunk(rss_parser->sax2, s, len, is_end);

  if(!is_end)
    return 0;

  if(rdf_parser->failed)
    return 1;

  /* turn strings into URIs, move things around if needed */
  raptor_rss_insert_identifiers(rdf_parser);
  
  /* add some new fields  */
  raptor_rss_uplift_items(rdf_parser);
  
  /* generate the triples */
  raptor_rss_emit(rdf_parser);

  return 0;
}


static int
raptor_rss_parse_recognise_syntax(raptor_parser_factory* factory, 
                                  const unsigned char *buffer, size_t len,
                                  const unsigned char *identifier, 
                                  const unsigned char *suffix, 
                                  const char *mime_type)
{
  int score= 0;
  
  if(suffix) {
    if(!strcmp((const char*)suffix, "rss"))
      score=7;
    if(!strcmp((const char*)suffix, "atom"))
      score=5;
    if(!strcmp((const char*)suffix, "xml"))
      score=4;
  }
  
  if(identifier) {
    if(!strncmp((const char*)identifier, "http://feed", 11))
      score+=5;
    else if(strstr((const char*)identifier, "feed"))
      score+=3;

    if(strstr((const char*)identifier, "rss2"))
      score+=5;
    else if(!suffix && strstr((const char*)identifier, "rss"))
      score+=4;
    else if(!suffix && strstr((const char*)identifier, "atom"))
      score+=4;
    else if(strstr((const char*)identifier, "rss.xml"))
      score+=4;
    else if(strstr((const char*)identifier, "atom.xml"))
      score+=4;
  }
  
  if(mime_type) {
    if(!strstr((const char*)mime_type, "html")) {
      if(strstr((const char*)mime_type, "rss"))
        score+=4;
      else if(strstr((const char*)mime_type, "xml"))
        score+=4;
      else if(strstr((const char*)mime_type, "atom"))
        score+=4;
    }
  }
  
  return score;
}


static int
raptor_rss_parser_register_factory(raptor_parser_factory *factory) 
{
  int rc=0;

  factory->context_length     = sizeof(raptor_rss_parser);
  
  factory->need_base_uri = 1;
  
  factory->init      = raptor_rss_parse_init;
  factory->terminate = raptor_rss_parse_terminate;
  factory->start     = raptor_rss_parse_start;
  factory->chunk     = raptor_rss_parse_chunk;
  factory->recognise_syntax = raptor_rss_parse_recognise_syntax;

  rc+= raptor_parser_factory_add_mime_type(factory, "application/rss", 10) != 0;
  rc+= raptor_parser_factory_add_mime_type(factory, "application/rss+xml", 10) != 0;
  rc+= raptor_parser_factory_add_mime_type(factory, "text/rss", 8) != 0;

  rc+= raptor_parser_factory_add_mime_type(factory, "application/xml", 3) != 0;
  rc+= raptor_parser_factory_add_mime_type(factory, "text/xml", 3) != 0;

  return rc;
}


int
raptor_init_parser_rss(void)
{
  return !raptor_parser_register_factory("rss-tag-soup",  "RSS Tag Soup",
                                         &raptor_rss_parser_register_factory);
}
