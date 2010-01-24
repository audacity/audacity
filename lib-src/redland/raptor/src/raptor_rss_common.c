/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_rss_common.c - Raptor RSS common code
 *
 * Copyright (C) 2003-2008, David Beckett http://www.dajobe.org/
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


raptor_rss_namespace_info raptor_rss_namespaces_info[RAPTOR_RSS_NAMESPACES_SIZE]={
  { NULL,                     NULL,      },
  { NULL,                     NULL,      },
  { RSS0_91_NAMESPACE_URI,    "rss091",  },
  { RSS0_9_NAMESPACE_URI,     NULL,      },
  { RSS1_0_NAMESPACE_URI,     "rss",     },
  { ATOM0_3_NAMESPACE_URI,    NULL,      },
  { DC_NAMESPACE_URI,         "dc",      },
  { RSS2_0_ENC_NAMESPACE_URI, "enc",     },
  { RSS1_1_NAMESPACE_URI,     NULL,      },
  { CONTENT_NAMESPACE_URI,    "content", },
  { ATOM1_0_NAMESPACE_URI,    "atom",    },
  { RDF_NAMESPACE_URI,        "rdf",     },
  { ATOMTRIPLES_NAMESPACE_URI, "at",     },
};


raptor_rss_info raptor_rss_types_info[RAPTOR_RSS_COMMON_SIZE]={
  { "channel",    RSS1_0_NS },
  { "image",      RSS1_0_NS },
  { "textinput",  RSS1_0_NS },
  { "item",       RSS1_0_NS },
  { "author",     ATOM1_0_NS },
  { "skipHours",  RSS0_91_NS },
  { "skipDays",   RSS0_91_NS },
  { "Enclosure",  RSS2_0_ENC_NS }, /* Enclosure class in RDF output */
  { "feed",       ATOM1_0_NS },
  { "entry",      ATOM1_0_NS },
  { "<unknown>",  RSS_UNKNOWN_NS },
};


raptor_rss_info raptor_rss_fields_info[RAPTOR_RSS_FIELDS_SIZE+2]={
  { "title",          RSS1_0_NS },
  { "link",           RSS1_0_NS },
  { "description",    RSS1_0_NS },
  { "url",            RSS1_0_NS },
  { "name",           RSS1_0_NS },
  { "language",       RSS0_91_NS },
  { "rating",         RSS0_91_NS },
  { "copyright",      RSS0_91_NS },
  { "pubDate",        RSS0_91_NS },
  { "lastBuildDate",  RSS0_91_NS },
  { "docs",           RSS0_91_NS },
  { "managingEditor", RSS0_91_NS },
  { "webMaster",      RSS0_91_NS },
  { "cloud",          RSS0_92_NS },
  { "ttl",            RSS2_0_NS },
  { "width",          RSS0_91_NS },
  { "height",         RSS0_91_NS },
  { "hour",           RSS0_91_NS },
  { "day",            RSS0_91_NS },
  { "generator",      RSS0_92_NS },
  { "source",         RSS0_92_NS },
  { "author",         RSS2_0_NS },
  { "guid",           RSS2_0_NS },
  { "enclosure",      RSS2_0_NS },     /* enclosure in RSS */
  { "enclosure",      RSS2_0_ENC_NS }, /* In RDF output, not an RSS field */
  { "url",            RSS2_0_ENC_NS }, /* In RDF output, not an RSS field */
  { "length",         RSS2_0_ENC_NS }, /* In RDF output, not an RSS field */
  { "type",           RSS2_0_ENC_NS }, /* In RDF output, not an RSS field */
  { "length",         RSS2_0_NS },
  { "type",           RSS2_0_NS },
  { "category",       RSS0_92_NS },
  { "comments",       RSS0_92_NS },
  { "items",          RSS1_0_NS },
  { "image",          RSS1_0_NS },
  { "textinput",      RSS1_0_NS },

  { "copyright",      ATOM0_3_NS },
  { "created",        ATOM0_3_NS },
  { "issued",         ATOM0_3_NS },
  { "modified",       ATOM0_3_NS },
  { "tagline",        ATOM0_3_NS },

  /* atom 1.0 required fields */
  { "id",             ATOM1_0_NS, RAPTOR_RSS_INFO_FLAG_URI_VALUE },
  { "title",          ATOM1_0_NS },
  { "updated",        ATOM1_0_NS },
  /* atom 1.0 optional fields */
  { "author",         ATOM1_0_NS },
  { "category",       ATOM1_0_NS },
  { "content",        ATOM1_0_NS },
  { "contributor",    ATOM1_0_NS },
  { "email",          ATOM1_0_NS },
  { "entry",          ATOM1_0_NS },
  { "feed",           ATOM1_0_NS },
  { "generator",      ATOM1_0_NS },
  { "icon",           ATOM1_0_NS, RAPTOR_RSS_INFO_FLAG_URI_VALUE },
  { "link",           ATOM1_0_NS },
  { "logo",           ATOM1_0_NS, RAPTOR_RSS_INFO_FLAG_URI_VALUE },
  { "name",           ATOM1_0_NS },
  { "published",      ATOM1_0_NS },
  { "rights",         ATOM1_0_NS },
  { "source",         ATOM1_0_NS },
  { "subtitle",       ATOM1_0_NS },
  { "summary",        ATOM1_0_NS },
  { "uri",            ATOM1_0_NS },

  { "title",          DC_NS },
  { "contributor",    DC_NS },
  { "creator",        DC_NS },
  { "publisher",      DC_NS },
  { "subject",        DC_NS },
  { "description",    DC_NS },
  { "date",           DC_NS },
  { "type",           DC_NS },
  { "format",         DC_NS },
  { "identifier",     DC_NS },
  { "language",       DC_NS },
  { "relation",       DC_NS },
  { "source",         DC_NS },
  { "coverage",       DC_NS },
  { "rights",         DC_NS },

  { "encoded",        CONTENT_NS },

  { "contentType",    ATOMTRIPLES_NS },

  { "<unknown>",      RSS_UNKNOWN_NS },
  { "<none>",         RSS_UNKNOWN_NS }
};


/* Crude and unofficial mappings from atom fields to RSS */
const raptor_field_pair raptor_atom_to_rss[]={
  /* atom clone of rss fields */
  { RAPTOR_RSS_FIELD_ATOM_SUMMARY,   RAPTOR_RSS_FIELD_DESCRIPTION },
  { RAPTOR_RSS_FIELD_ATOM_ID,        RAPTOR_RSS_FIELD_LINK },
  { RAPTOR_RSS_FIELD_ATOM_UPDATED,   RAPTOR_RSS_FIELD_DC_DATE },
  { RAPTOR_RSS_FIELD_ATOM_RIGHTS,    RAPTOR_RSS_FIELD_DC_RIGHTS },
  { RAPTOR_RSS_FIELD_ATOM_TITLE,     RAPTOR_RSS_FIELD_TITLE },
  { RAPTOR_RSS_FIELD_ATOM_SUMMARY,   RAPTOR_RSS_FIELD_CONTENT_ENCODED },

  /* atom 0.3 to atom 1.0 */
  { RAPTOR_RSS_FIELD_ATOM_COPYRIGHT, RAPTOR_RSS_FIELD_ATOM_RIGHTS },
  { RAPTOR_RSS_FIELD_ATOM_TAGLINE,   RAPTOR_RSS_FIELD_ATOM_SUBTITLE },
#if 0
  /* other old atom 0.3 fields */
  { RAPTOR_RSS_FIELD_ATOM_CREATED,  RAPTOR_RSS_FIELD_UNKNOWN },
  { RAPTOR_RSS_FIELD_ATOM_ISSUED,   RAPTOR_RSS_FIELD_UNKNOWN },
  { RAPTOR_RSS_FIELD_ATOM_MODIFIED, RAPTOR_RSS_FIELD_UNKNOWN },
#endif

  { RAPTOR_RSS_FIELD_UNKNOWN,       RAPTOR_RSS_FIELD_UNKNOWN }
};
  

const unsigned char * const raptor_atom_namespace_uri=(const unsigned char *)"http://www.w3.org/2005/Atom";

static int raptor_rss_common_initialised=0;


void
raptor_rss_common_init(void) {
  int i;
  if(raptor_rss_common_initialised++)
    return;
  
  for(i=0; i<RAPTOR_RSS_NAMESPACES_SIZE;i++) {
    const char *uri_string=raptor_rss_namespaces_info[i].uri_string;
    if(uri_string)
      raptor_rss_namespaces_info[i].uri=raptor_new_uri((const unsigned char*)uri_string);
  }

  for(i=0; i< RAPTOR_RSS_COMMON_SIZE; i++) {
    int n=raptor_rss_types_info[i].nspace;
    raptor_uri *namespace_uri=raptor_rss_namespaces_info[n].uri;
    if(namespace_uri)
      raptor_rss_types_info[i].uri=raptor_new_uri_from_uri_local_name(namespace_uri, (const unsigned char*)raptor_rss_types_info[i].name);
  }

  for(i=0; i< RAPTOR_RSS_FIELDS_SIZE; i++) {
    raptor_uri *namespace_uri=raptor_rss_namespaces_info[raptor_rss_fields_info[i].nspace].uri;
    if(namespace_uri)
      raptor_rss_fields_info[i].uri=raptor_new_uri_from_uri_local_name(namespace_uri,
                                                                       (const unsigned char*)raptor_rss_fields_info[i].name);
  }

}


void
raptor_rss_common_terminate(void) {
  int i;
  if(--raptor_rss_common_initialised)
    return;

  for(i=0; i< RAPTOR_RSS_COMMON_SIZE; i++) {
    if(raptor_rss_types_info[i].uri)
      raptor_free_uri(raptor_rss_types_info[i].uri);
  }

  for(i=0; i< RAPTOR_RSS_FIELDS_SIZE; i++) {
    if(raptor_rss_fields_info[i].uri)
      raptor_free_uri(raptor_rss_fields_info[i].uri);
  }

  for(i=0; i<RAPTOR_RSS_NAMESPACES_SIZE;i++) {
    if(raptor_rss_namespaces_info[i].uri)
      raptor_free_uri(raptor_rss_namespaces_info[i].uri);
  }

}


void
raptor_rss_model_init(raptor_rss_model* rss_model)
{
  memset(rss_model->common, 0, sizeof(void*)*RAPTOR_RSS_COMMON_SIZE);

  rss_model->last=rss_model->items=NULL;
  rss_model->items_count=0;

  RAPTOR_RSS_RDF_type_URI(rss_model)=raptor_new_uri_for_rdf_concept("type");
  RAPTOR_RSS_RDF_Seq_URI(rss_model)=raptor_new_uri_for_rdf_concept("Seq");
  RAPTOR_RSS_RSS_items_URI(rss_model)=raptor_new_uri_relative_to_base(raptor_rss_namespaces_info[RSS1_0_NS].uri, (const unsigned char*)"items");
}
  

void
raptor_rss_model_clear(raptor_rss_model* rss_model)
{
  int i;
  raptor_rss_item* item;
  
  for(i=0; i< RAPTOR_RSS_COMMON_SIZE; i++) {
    item=rss_model->common[i];
    while(item) {
      raptor_rss_item *next=item->next;
      raptor_free_rss_item(item);
      item=next;
    }
  }

  item=rss_model->items;
  while(item) {
    raptor_rss_item *next=item->next;

    raptor_free_rss_item(item);
    item=next;
  }
  rss_model->last=rss_model->items=NULL;

  for(i=0; i< RAPTOR_RSS_N_CONCEPTS; i++) {
    raptor_uri* concept_uri=rss_model->concepts[i];
    if(concept_uri) {
      raptor_free_uri(concept_uri);
      rss_model->concepts[i]=NULL;
    }
  }
}


raptor_rss_item*
raptor_new_rss_item(void)
{
  raptor_rss_item* item;

  item=(raptor_rss_item*)RAPTOR_CALLOC(raptor_rss_item, 1, 
                                       sizeof(raptor_rss_item));
  if(!item)
    return NULL;
  
  item->triples=raptor_new_sequence((raptor_sequence_free_handler*)raptor_free_statement, (raptor_sequence_print_handler*)raptor_print_statement);
  if(!item->triples) {
    RAPTOR_FREE(raptor_rss_item, item);
    return NULL;
  }
  return item;
}


int
raptor_rss_model_add_item(raptor_rss_model* rss_model)
{
  raptor_rss_item* item;

  item=raptor_new_rss_item();
  if(!item)
    return 1;

  /* new list */
  if(!rss_model->items)
    rss_model->items=item;
  
  /* join last item to this one */
  if(rss_model->last)
    rss_model->last->next=item;
  
  /* this is now the last item */
  rss_model->last=item;
  rss_model->items_count++;

  RAPTOR_DEBUG2("Added item %d\n", rss_model->items_count);

  return 0;
}


raptor_rss_item*
raptor_rss_model_add_common(raptor_rss_model* rss_model,
                            raptor_rss_type type)
{
  raptor_rss_item* item;

  item=raptor_new_rss_item();
  if(!item)
    return NULL;

  if(rss_model->common[type]==NULL) {
    RAPTOR_DEBUG3("Adding common type %d - %s\n", type,
                  raptor_rss_types_info[type].name);
    rss_model->common[type]=item; 
  } else {
    raptor_rss_item* next;
    RAPTOR_DEBUG3("Appending common type %d - %s\n", type, 
                  raptor_rss_types_info[type].name);
    for (next=rss_model->common[type]; next->next; next=next->next)
      ;
    next->next=item;
  }
  return item;
}


raptor_rss_item*
raptor_rss_model_get_common(raptor_rss_model* rss_model, raptor_rss_type type)
{
  raptor_rss_item* item;
  for(item=rss_model->common[type]; 
      item && item->next;
      item=item->next) ;
  return item;
}


void
raptor_free_rss_item(raptor_rss_item* item)
{
  int i;
  for(i=0; i< RAPTOR_RSS_FIELDS_SIZE; i++) {
    if(item->fields[i])
      raptor_rss_field_free(item->fields[i]);
  }
  if(item->enclosure) 
    raptor_enclosure_free(item->enclosure);
  if(item->uri)
    raptor_free_uri(item->uri);
  raptor_free_identifier(&item->identifier);
  if(item->triples)
    raptor_free_sequence(item->triples);

  RAPTOR_FREE(raptor_rss_item, item);
}


void
raptor_rss_item_add_enclosure(raptor_rss_item* item, 
                              raptor_rss_enclosure* enclosure)
{
  if (!item->enclosure) {
    RAPTOR_DEBUG1("Adding first enclosure\n");
    item->enclosure=enclosure;
  } else {
    raptor_rss_enclosure* cur;

    RAPTOR_DEBUG1("Adding subsequent enclosure\n");
    for(cur=item->enclosure; cur->next; cur=cur->next) ;
    cur->next=enclosure;
  }
}


void
raptor_rss_item_add_field(raptor_rss_item* item, int type, 
                          raptor_rss_field* field)
{
  if(!item->fields[type]) {
    RAPTOR_DEBUG3("Adding first type %d field %s\n", type, raptor_rss_fields_info[type].name);
    item->fields_count++;	
    item->fields[type]=field;
  } else { 
    raptor_rss_field* cur;

    RAPTOR_DEBUG1("Adding subsequent field\n");
    for(cur=item->fields[type]; cur->next; cur=cur->next) ;
    cur->next=field;
  }
}


raptor_rss_enclosure*
raptor_rss_new_enclosure(void)
{
  raptor_rss_enclosure* enclosure=(raptor_rss_enclosure*)RAPTOR_CALLOC(raptor_rss_enclosure, 1, sizeof(raptor_rss_enclosure));
  return enclosure;
}


void
raptor_enclosure_free(raptor_rss_enclosure* enclosure)
{
  if(enclosure->length)
    RAPTOR_FREE(cstring, enclosure->length);
  if(enclosure->type)
    RAPTOR_FREE(cstring, enclosure->type);
  if(enclosure->url)
    raptor_free_uri(enclosure->url);
  if(enclosure->next)
    raptor_enclosure_free(enclosure->next);
  raptor_free_identifier(&(enclosure->identifier));
  RAPTOR_FREE(raptor_rss_enclosure, enclosure);
}


raptor_rss_field*
raptor_rss_new_field(void)
{
  raptor_rss_field* field=(raptor_rss_field*)RAPTOR_CALLOC(raptor_rss_field, 1, sizeof(raptor_rss_field));
  return field;
}


void
raptor_rss_field_free(raptor_rss_field* field)
{
  if(field->value)
    RAPTOR_FREE(cstring, field->value);
  if(field->uri)
    raptor_free_uri(field->uri);
  if(field->next)
    raptor_rss_field_free(field->next);
  RAPTOR_FREE(raptor_rss_field, field);
}


#define RAPTOR_ISO_DATE_FORMAT "%Y-%m-%dT%H:%M:%SZ"

int
raptor_rss_format_iso_date(char* buffer, size_t len, time_t unix_time) 
{
  struct tm* structured_time;

  if(len < RAPTOR_ISO_DATE_LEN)
    return 1;

  structured_time=gmtime(&unix_time);
  strftime(buffer, len+1, RAPTOR_ISO_DATE_FORMAT, structured_time);

  return 0;
}


int
raptor_rss_set_date_field(raptor_rss_field* field, time_t unix_time)
{
  size_t len=RAPTOR_ISO_DATE_LEN;
  
  if(field->value)
    RAPTOR_FREE(cstring, field->value);
  field->value=(unsigned char*)RAPTOR_MALLOC(cstring, len + 1);
  if(!field->value)
    return 1;
  
  if(raptor_rss_format_iso_date((char*)field->value, len, unix_time)) {
    RAPTOR_FREE(cstring, field->value);
    return 1;
  }

  return 0;
}


int
raptor_rss_date_uplift(raptor_rss_field* to_field, 
                       const unsigned char *date_string)
{
#ifdef RAPTOR_PARSEDATE_FUNCTION
  time_t unix_time;

  unix_time=RAPTOR_PARSEDATE_FUNCTION((const char*)date_string, NULL);
  if(unix_time < 0)
    return 1;

  return raptor_rss_set_date_field(to_field, unix_time);
#else
  return 1;
#endif
}
