/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rasqal_redland.c - Rasqal redland interface
 *
 * Copyright (C) 2004-2008, David Beckett http://www.dajobe.org/
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
 * 
 */

#ifdef HAVE_CONFIG_H
#include <rasqal_config.h>
#endif

#ifdef WIN32
#include <win32_rasqal_config.h>
#endif

#include <stdio.h>
#include <string.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#include <stdarg.h>

#include <redland.h>

#include "rasqal.h"
#include "rasqal_internal.h"


#define LIBRDF_MALLOC(type, size) malloc(size)
#define LIBRDF_CALLOC(type, size, count) calloc(size, count)
#define LIBRDF_FREE(type, ptr)   free((void*)ptr)

#ifdef RASQAL_DEBUG
#define LIBRDF_DEBUG1(msg) do {fprintf(stderr, "%s:%d:%s: " msg, __FILE__, __LINE__, __func__); } while(0)
#define LIBRDF_DEBUG2(msg, arg1) do {fprintf(stderr, "%s:%d:%s: " msg, __FILE__, __LINE__, __func__, arg1);} while(0)
#else
#define LIBRDF_DEBUG1(msg)
#define LIBRDF_DEBUG2(msg, arg1)
#endif



static librdf_node*
rasqal_literal_to_redland_node(librdf_world *world, rasqal_literal* l) {
  if(l->type == RASQAL_LITERAL_URI)
    return librdf_new_node_from_uri(world, (librdf_uri*)l->value.uri);
  else if (l->type == RASQAL_LITERAL_STRING ||
           l->type == RASQAL_LITERAL_INTEGER ||
           l->type == RASQAL_LITERAL_DOUBLE ||
           l->type == RASQAL_LITERAL_BOOLEAN)
    return librdf_new_node_from_typed_literal(world, l->string, 
                                              l->language, 
                                              (librdf_uri*)l->datatype);
  else if (l->type == RASQAL_LITERAL_BLANK)
    return librdf_new_node_from_blank_identifier(world, l->string);
  else
    RASQAL_FATAL1("Literal type %d cannot be converted to a librdf_node", l->type);

  return NULL;
}


static rasqal_literal*
redland_node_to_rasqal_literal(rasqal_world *world, librdf_node *node) {
  rasqal_literal* l;
  
  if(librdf_node_is_resource(node)) {
    raptor_uri* uri=(raptor_uri*)librdf_new_uri_from_uri(librdf_node_get_uri(node));
    l=rasqal_new_uri_literal(world, uri);
  } else if(librdf_node_is_literal(node)) {
    char *string;
    librdf_uri *uri;
    char *new_string;
    char *new_language=NULL;
    raptor_uri *new_datatype=NULL;
    size_t len;
    string=(char*)librdf_node_get_literal_value_as_counted_string(node, &len);
    new_string=LIBRDF_MALLOC(cstring, len+1);
    strcpy(new_string, (const char*)string);
    string=librdf_node_get_literal_value_language(node);
    if(string) {
      new_language=LIBRDF_MALLOC(cstring, strlen(string)+1);
      strcpy(new_language, (const char*)string);
    }
    uri=librdf_node_get_literal_value_datatype_uri(node);
    if(uri)
      new_datatype=(raptor_uri*)librdf_new_uri_from_uri(uri);
    l=rasqal_new_string_literal(world, (unsigned char*)new_string, new_language, new_datatype, NULL);
  } else {
    char *blank=(char*)librdf_node_get_blank_identifier(node);
    char *new_blank=LIBRDF_MALLOC(cstring, strlen(blank)+1);
    strcpy(new_blank, (const char*)blank);
    l=rasqal_new_simple_literal(world, RASQAL_LITERAL_BLANK, (unsigned char*)new_blank);
  }

  return l;
}


typedef struct {
  librdf_world *world;
  librdf_model *model;
  librdf_storage *storage;

  /* index used while reading triples into the array below.
   * This is used to connect a triple to the URI of the source
   */
  int source_index;

  /* size of the two arrays below */
  int sources_count;
  
  /* array of source URIs */
  librdf_uri **source_uris;
} rasqal_redland_triples_source_user_data;

/* prototypes */
static int rasqal_redland_init_triples_match(rasqal_triples_match* rtm, rasqal_triples_source *rts, void *user_data, rasqal_triple_meta *m, rasqal_triple *t);
static int rasqal_redland_triple_present(rasqal_triples_source *rts, void *user_data, rasqal_triple *t);
static void rasqal_redland_free_triples_source(void *user_data);

static int
rasqal_redland_new_triples_source(rasqal_query* rdf_query,
                                  void *factory_user_data,
                                  void *user_data,
                                  rasqal_triples_source *rts) {
  librdf_world *world=(librdf_world*)factory_user_data;
  rasqal_redland_triples_source_user_data* rtsc=(rasqal_redland_triples_source_user_data*)user_data;
  librdf_parser *parser;
  const char *parser_name;
  int i;
  
  if(!rdf_query->data_graphs)
    return -1; /* no data */

  rtsc->sources_count=raptor_sequence_size(rdf_query->data_graphs);
  /* no default triple source possible */
  if(!rtsc->sources_count)
    return -1;  /* no data */

  rtsc->world=world;

  /* FIXME error checking */
  rtsc->storage = librdf_new_storage(world, NULL, NULL, "contexts='yes'");
  rtsc->model = librdf_new_model(world, rtsc->storage, NULL);

  rts->init_triples_match=rasqal_redland_init_triples_match;
  rts->triple_present=rasqal_redland_triple_present;
  rts->free_triples_source=rasqal_redland_free_triples_source;

  rtsc->source_uris=(librdf_uri**)RASQAL_CALLOC(librdf_uri_ptr, rtsc->sources_count, sizeof(librdf_uri*));

  for(i=0; i< rtsc->sources_count; i++) {
    rasqal_data_graph *dg=(rasqal_data_graph*)raptor_sequence_get_at(rdf_query->data_graphs, i);
    librdf_stream *stream;
    librdf_node *node;
    
    rtsc->source_index=i;
    rtsc->source_uris[i]=librdf_new_uri(world, raptor_uri_as_string(dg->uri));

    parser_name=raptor_guess_parser_name(NULL, NULL, NULL, 0,
                                         raptor_uri_as_string(dg->uri));
    parser=librdf_new_parser(world, parser_name, NULL, NULL);
    stream=librdf_parser_parse_as_stream(parser, (librdf_uri*)dg->uri,
                                         (librdf_uri*)dg->name_uri);
    node=librdf_new_node_from_uri(world, (librdf_uri*)rtsc->source_uris[i]);
    librdf_model_context_add_statements(rtsc->model, node, stream);
    librdf_free_stream(stream);
    librdf_free_node(node);
    librdf_free_parser(parser);
  }

  return 0;
}


static int
rasqal_redland_triple_present(rasqal_triples_source *rts, void *user_data, 
                              rasqal_triple *t) 
{
  rasqal_redland_triples_source_user_data* rtsc=(rasqal_redland_triples_source_user_data*)user_data;
  librdf_node* nodes[3];
  librdf_statement *s;
  int rc;
  
  /* ASSUMPTION: all the parts of the triple are not variables */
  /* FIXME: and no error checks */
  nodes[0]=rasqal_literal_to_redland_node(rtsc->world, t->subject);
  nodes[1]=rasqal_literal_to_redland_node(rtsc->world, t->predicate);
  nodes[2]=rasqal_literal_to_redland_node(rtsc->world, t->object);

  s=librdf_new_statement_from_nodes(rtsc->world, nodes[0], nodes[1], nodes[2]);
  
  rc=librdf_model_contains_statement(rtsc->model, s);
  librdf_free_statement(s);
  return rc;
}



static void
rasqal_redland_free_triples_source(void *user_data) {
  rasqal_redland_triples_source_user_data* rtsc=(rasqal_redland_triples_source_user_data*)user_data;
  int i;
  
  for(i=0; i< rtsc->sources_count; i++) {
    librdf_free_uri(rtsc->source_uris[i]);
  }
  RASQAL_FREE(librdf_uri_ptr, rtsc->source_uris);
  
  librdf_free_model(rtsc->model);
  librdf_free_storage(rtsc->storage);
}


static void
rasqal_redland_register_triples_source_factory(rasqal_triples_source_factory *factory) 
{
  factory->user_data_size=sizeof(rasqal_redland_triples_source_user_data);
  factory->new_triples_source=rasqal_redland_new_triples_source;
}


typedef struct {
  librdf_node* nodes[3];
  librdf_node* origin;
  /* query statement, made from the nodes above (even when exact) */
  librdf_statement *qstatement;
  librdf_stream *stream;
} rasqal_redland_triples_match_context;


static rasqal_triple_parts
rasqal_redland_bind_match(struct rasqal_triples_match_s* rtm,
                          void *user_data,
                          rasqal_variable* bindings[4],
                          rasqal_triple_parts parts) {
  rasqal_redland_triples_match_context* rtmc=(rasqal_redland_triples_match_context*)rtm->user_data;
  rasqal_literal* l;
  rasqal_triple_parts result=(rasqal_triple_parts)0;
  librdf_statement* statement;

  statement= librdf_stream_get_object(rtmc->stream);
  if(!statement)
    return result;
  
#ifdef RASQAL_DEBUG
  LIBRDF_DEBUG1("  matched statement ");
  librdf_statement_print(statement, stderr);
  fputc('\n', stderr);
#endif

  /* set 1 or 2 variable values from the fields of statement */

  if(bindings[0] && (parts & RASQAL_TRIPLE_SUBJECT)) {
    LIBRDF_DEBUG1("binding subject to variable\n");
    l=redland_node_to_rasqal_literal(rtm->world, librdf_statement_get_subject(statement));
    rasqal_variable_set_value(bindings[0], rasqal_literal_as_node(l));
    rasqal_free_literal(l);
    result= RASQAL_TRIPLE_SUBJECT;
  }

  if(bindings[1] && (parts & RASQAL_TRIPLE_PREDICATE)) {
    if(bindings[0] == bindings[1]) {
      if(!librdf_node_equals(librdf_statement_get_subject(statement),
                             librdf_statement_get_predicate(statement)))
        return (rasqal_triple_parts)0;
      LIBRDF_DEBUG1("subject and predicate values match\n");
    } else {
      LIBRDF_DEBUG1("binding predicate to variable\n");
      l=redland_node_to_rasqal_literal(rtm->world, librdf_statement_get_predicate(statement));
      rasqal_variable_set_value(bindings[1], rasqal_literal_as_node(l));
      rasqal_free_literal(l);
      result= (rasqal_triple_parts)(result | RASQAL_TRIPLE_PREDICATE);
    }
  }

  if(bindings[2] && (parts & RASQAL_TRIPLE_OBJECT)) {
    int bind=1;
    
    if(bindings[0] == bindings[2]) {
      if(!librdf_node_equals(librdf_statement_get_subject(statement),
                             librdf_statement_get_object(statement)))
        return (rasqal_triple_parts)0;
      bind=0;
      LIBRDF_DEBUG1("subject and object values match\n");
    }
    if(bindings[1] == bindings[2] &&
       !(bindings[0] == bindings[1]) /* don't do this check if ?x ?x ?x */
       ) {
      if(!librdf_node_equals(librdf_statement_get_predicate(statement),
                             librdf_statement_get_object(statement)))
        return (rasqal_triple_parts)0;
      bind=0;
      LIBRDF_DEBUG1("predicate and object values match\n");
    }
    
    if(bind) {
      LIBRDF_DEBUG1("binding object to variable\n");
      l=redland_node_to_rasqal_literal(rtm->world, librdf_statement_get_object(statement));
      rasqal_variable_set_value(bindings[2], rasqal_literal_as_node(l));
      rasqal_free_literal(l);
      result= (rasqal_triple_parts)(result | RASQAL_TRIPLE_OBJECT);
    }
  }

  if(bindings[3] && (parts & RASQAL_TRIPLE_ORIGIN)) {
    l=redland_node_to_rasqal_literal(rtm->world, (librdf_node*)librdf_stream_get_context(rtmc->stream));
    RASQAL_DEBUG1("binding origin to variable\n");
    rasqal_variable_set_value(bindings[3], rasqal_literal_as_node(l));
    rasqal_free_literal(l);
    result= (rasqal_triple_parts)(result | RASQAL_TRIPLE_ORIGIN);
  }

  return result;
}


static void
rasqal_redland_next_match(struct rasqal_triples_match_s* rtm,
                          void *user_data)
{
  rasqal_redland_triples_match_context* rtmc=(rasqal_redland_triples_match_context*)rtm->user_data;

  librdf_stream_next(rtmc->stream);
}

static int
rasqal_redland_is_end(struct rasqal_triples_match_s* rtm,
                      void *user_data)
{
  rasqal_redland_triples_match_context* rtmc=(rasqal_redland_triples_match_context*)rtm->user_data;

  return librdf_stream_end(rtmc->stream);
}


static void
rasqal_redland_finish_triples_match(struct rasqal_triples_match_s* rtm,
                                    void *user_data) {
  rasqal_redland_triples_match_context* rtmc=(rasqal_redland_triples_match_context*)rtm->user_data;

  if(rtmc->stream) {
    librdf_free_stream(rtmc->stream);
    rtmc->stream=NULL;
  }
  librdf_free_statement(rtmc->qstatement);
  LIBRDF_FREE(rasqal_redland_triples_match_context, rtmc);
}


static int
rasqal_redland_init_triples_match(rasqal_triples_match* rtm,
                                  rasqal_triples_source *rts, void *user_data,
                                  rasqal_triple_meta *m, rasqal_triple *t) {
  rasqal_redland_triples_source_user_data* rtsc=(rasqal_redland_triples_source_user_data*)user_data;
  rasqal_redland_triples_match_context* rtmc;
  rasqal_variable* var;

  rtm->bind_match=rasqal_redland_bind_match;
  rtm->next_match=rasqal_redland_next_match;
  rtm->is_end=rasqal_redland_is_end;
  rtm->finish=rasqal_redland_finish_triples_match;

  rtmc=(rasqal_redland_triples_match_context*)LIBRDF_CALLOC(rasqal_redland_triples_match_context, 1, sizeof(rasqal_redland_triples_match_context));

  rtm->user_data=rtmc;


  /* at least one of the triple terms is a variable and we need to
   * do a triplesMatching() aka librdf_model_find_statements
   *
   * redland find_statements will do the right thing and internally
   * pick the most efficient, indexed way to get the answer.
   */

  if((var=rasqal_literal_as_variable(t->subject))) {
    if(var->value)
      rtmc->nodes[0]=rasqal_literal_to_redland_node(rtsc->world, var->value);
    else
      rtmc->nodes[0]=NULL;
  } else
    rtmc->nodes[0]=rasqal_literal_to_redland_node(rtsc->world, t->subject);

  m->bindings[0]=var;
  

  if((var=rasqal_literal_as_variable(t->predicate))) {
    if(var->value)
      rtmc->nodes[1]=rasqal_literal_to_redland_node(rtsc->world, var->value);
    else
      rtmc->nodes[1]=NULL;
  } else
    rtmc->nodes[1]=rasqal_literal_to_redland_node(rtsc->world, t->predicate);

  m->bindings[1]=var;
  

  if((var=rasqal_literal_as_variable(t->object))) {
    if(var->value)
      rtmc->nodes[2]=rasqal_literal_to_redland_node(rtsc->world, var->value);
    else
      rtmc->nodes[2]=NULL;
  } else
    rtmc->nodes[2]=rasqal_literal_to_redland_node(rtsc->world, t->object);

  m->bindings[2]=var;
  

  if(t->origin) {
    if((var=rasqal_literal_as_variable(t->origin))) {
      if(var->value)
        rtmc->origin=rasqal_literal_to_redland_node(rtsc->world, var->value);
    } else
      rtmc->origin=rasqal_literal_to_redland_node(rtsc->world, t->origin);
    m->bindings[3]=var;
  }


  rtmc->qstatement=librdf_new_statement_from_nodes(rtsc->world, 
                                                   rtmc->nodes[0],
                                                   rtmc->nodes[1], 
                                                   rtmc->nodes[2]);
  if(!rtmc->qstatement)
    return 1;

#ifdef RASQAL_DEBUG
  LIBRDF_DEBUG1("query statement: ");
  librdf_statement_print(rtmc->qstatement, stderr);
  fputc('\n', stderr);
#endif
  
  rtmc->stream=librdf_model_find_statements(rtsc->model, rtmc->qstatement);

  LIBRDF_DEBUG1("rasqal_init_triples_match done\n");

  return 0;
}

static librdf_world* Rasqal_Redland_World=NULL;

int
rasqal_redland_init(rasqal_world* world) {
  Rasqal_Redland_World=librdf_new_world();
  if(!Rasqal_Redland_World)
    return 1;
  librdf_world_open(Rasqal_Redland_World);
  rasqal_set_triples_source_factory(world, rasqal_redland_register_triples_source_factory, Rasqal_Redland_World);
  return 0;
}

void
rasqal_redland_finish() {
  librdf_free_world(Rasqal_Redland_World);
  Rasqal_Redland_World=NULL;
}

