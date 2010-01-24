/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rasqal_result_formats.c - Rasqal RDF Query Result Formats
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

#include "rasqal.h"
#include "rasqal_internal.h"


static int rasqal_query_results_write_json1(raptor_iostream *iostr, rasqal_query_results* results, raptor_uri *base_uri);


int
rasqal_query_results_format_register_factory(rasqal_world* world,
                                             const char *name,
                                             const char *label,
                                             const unsigned char* uri_string,
                                             rasqal_query_results_formatter_func writer,
                                             rasqal_query_results_formatter_func reader,
                                             rasqal_query_results_get_rowsource_func get_rowsource,
                                             const char *mime_type)
{
  rasqal_query_results_format_factory* factory;

  factory=(rasqal_query_results_format_factory*)RASQAL_CALLOC(query_results_format_factory, 
                                                              1, sizeof(rasqal_query_results_format_factory));

  if(!factory) {
    rasqal_log_error_simple(world, RAPTOR_LOG_LEVEL_FATAL, NULL,
                            "Out of memory in rasqal_query_results_format_register_factory()");
    return 1;
  }
  factory->name=name;
  factory->label=label;
  factory->uri_string=uri_string;
  factory->writer=writer;
  factory->reader=reader;
  factory->get_rowsource=get_rowsource;
  factory->mime_type=mime_type;

  return raptor_sequence_push(world->query_results_formats, factory);
}



static
void rasqal_free_query_results_format_factory(rasqal_query_results_format_factory* factory) 
{
  RASQAL_FREE(query_results_format_factory, factory);
}


int
rasqal_init_result_formats(rasqal_world* world)
{
  rasqal_query_results_formatter_func writer_fn=NULL;
  rasqal_query_results_formatter_func reader_fn=NULL;
  rasqal_query_results_get_rowsource_func get_rowsource_fn=NULL;
  int rc=0;
  
  world->query_results_formats=raptor_new_sequence((raptor_sequence_free_handler*)rasqal_free_query_results_format_factory, NULL);
  if(!world->query_results_formats)
    return 1;

  rc+= rasqal_init_result_format_sparql_xml(world) != 0;

  /*
   * SPARQL Query Results in JSON (http://json.org/) draft
   * Defined in http://www.w3.org/2001/sw/DataAccess/json-sparql/
   * Version: 1.6 $ of $Date: 2006/04/05 15:55:17
   */
  writer_fn=&rasqal_query_results_write_json1;
  reader_fn=NULL;
  get_rowsource_fn=NULL;
  rc+= rasqal_query_results_format_register_factory(world,
                                                    "json",
                                                    "JSON",
                                                    (unsigned char*)"http://www.w3.org/2001/sw/DataAccess/json-sparql/",
                                                    writer_fn, reader_fn, get_rowsource_fn,
                                                    "text/json")
                                                    != 0;
  rc+= rasqal_query_results_format_register_factory(world,
                                                    NULL,
                                                    NULL,
                                                    (unsigned char*)"http://www.mindswap.org/%7Ekendall/sparql-results-json/",
                                                    writer_fn, reader_fn, get_rowsource_fn,
                                                    "text/json")
                                                    != 0;

  return rc;
}


void
rasqal_finish_result_formats(rasqal_world* world)
{
  if(world->query_results_formats) {
    raptor_free_sequence(world->query_results_formats);
    world->query_results_formats = NULL;
  }
}


/**
 * rasqal_query_results_formats_enumerate:
 * @world: rasqal_world object
 * @counter: index into the list of query result syntaxes
 * @name: pointer to store the name of the query result syntax (or NULL)
 * @label: pointer to store query result syntax readable label (or NULL)
 * @uri_string: pointer to store query result syntax URI string (or NULL)
 * @mime_type: pointer to store query result syntax mime type string (or NULL)
 * @flags: pointer to store query result syntax flags (or NULL)
 *
 * Get information on query result syntaxes.
 * 
 * The current list of format names/URI is given below however
 * the results of this function will always return the latest.
 *
 * SPARQL XML Results 2007-06-14 (default format when @counter is 0)
 * name '<literal>xml</literal>' with
 * URIs http://www.w3.org/TR/2006/WD-rdf-sparql-XMLres-20070614/ or
 * http://www.w3.org/2005/sparql-results#
 *
 * JSON name '<literal>json</literal>' and
 * URI http://www.w3.org/2001/sw/DataAccess/json-sparql/
 *
 * All returned strings are shared and must be copied if needed to be
 * used dynamically.
 * 
 * Return value: non 0 on failure of if counter is out of range
 **/
int
rasqal_query_results_formats_enumerate(rasqal_world* world,
                                       unsigned int counter,
                                       const char **name,
                                       const char **label,
                                       const unsigned char **uri_string,
                                       const char **mime_type,
                                       int *flags)



{
  rasqal_query_results_format_factory *factory;
  int i;
  unsigned int real_counter;
  
  real_counter=0;
  for(i=0; 1; i++) {
    factory=(rasqal_query_results_format_factory*)raptor_sequence_get_at(world->query_results_formats, i);
    if(!factory)
      break;

    if(factory->name) {
      if(real_counter == counter)
        break;
      real_counter++;
    }
  }

  if(!factory)
    return 1;

  if(name)
    *name=factory->name;
  if(label)
    *label=factory->label;
  if(uri_string)
    *uri_string=factory->uri_string;
  if(mime_type)
    *mime_type=factory->mime_type;
  if(flags) {
    *flags=0;
    if(factory->reader)
      *flags |= RASQAL_QUERY_RESULTS_FORMAT_FLAG_READER;
    if(factory->writer)
      *flags |= RASQAL_QUERY_RESULTS_FORMAT_FLAG_WRITER;
  }
  
  return 0;
}


static rasqal_query_results_format_factory*
rasqal_get_query_results_formatter_factory(rasqal_world* world,
                                           const char *name, raptor_uri* uri,
                                           const char *mime_type)
{
  int i;
  rasqal_query_results_format_factory* factory=NULL;
  
  for(i=0; 1; i++) {
    factory=(rasqal_query_results_format_factory*)raptor_sequence_get_at(world->query_results_formats,
                                                                         i);
    if(!factory)
      break;

    if(!name && !uri)
      /* the default is the first registered format */
      break;
    
    if(name && factory->name &&
       !strcmp(factory->name, (const char*)name))
      return factory;


    if(uri && factory->uri_string &&
       !strcmp((const char*)raptor_uri_as_string(uri),
               (const char*)factory->uri_string))
      break;


    if(mime_type && factory->mime_type &&
       !strcmp(factory->mime_type, (const char*)mime_type))
      return factory;
  }
  
  return factory;
}


/**
 * rasqal_query_results_formats_check:
 * @world: rasqal_world object
 * @name: the query results format name (or NULL)
 * @uri: #raptor_uri query results format uri (or NULL)
 * @mime_type: mime type name
 * 
 * Check if a query results formatter exists for the requested format.
 * 
 * Return value: non-0 if a formatter exists.
 **/
int
rasqal_query_results_formats_check(rasqal_world* world,
                                   const char *name, raptor_uri* uri,
                                   const char *mime_type)
{
  return (rasqal_get_query_results_formatter_factory(world, name, uri, mime_type) 
          != NULL);
}


/**
 * rasqal_new_query_results_formatter:
 * @world: rasqal_world object
 * @name: the query results format name (or NULL)
 * @format_uri: #raptor_uri query results format uri (or NULL)
 *
 * Constructor - create a new rasqal_query_results_formatter object by identified format.
 *
 * A query results format can be named or identified by a URI, both
 * of which are optional.  The default query results format will be used
 * if both are NULL.  rasqal_query_results_formats_enumerate() returns
 * information on the known query results names, labels and URIs.
 *
 * Return value: a new #rasqal_query_results_formatter object or NULL on failure
 */
rasqal_query_results_formatter*
rasqal_new_query_results_formatter(rasqal_world* world, const char *name, raptor_uri* format_uri)
{
  rasqal_query_results_format_factory* factory;
  rasqal_query_results_formatter* formatter;

  factory=rasqal_get_query_results_formatter_factory(world, name, format_uri, NULL);
  if(!factory)
    return NULL;

  formatter=(rasqal_query_results_formatter*)RASQAL_CALLOC(rasqal_query_results_formatter, 1, sizeof(rasqal_query_results_formatter));
  if(!formatter)
    return NULL;

  formatter->factory=factory;

  formatter->mime_type=factory->mime_type;
  
  return formatter;
}


/**
 * rasqal_new_query_results_formatter_by_mime_type:
 * @world: rasqal_world object
 * @mime_type: mime type name
 *
 * Constructor - create a new rasqal_query_results_formatter object by mime type.
 *
 * A query results format generates a syntax with a mime type which
 * may be requested with this constructor.
 *
 * Note that there may be several formatters that generate the same
 * MIME Type (such as SPARQL XML results format drafts) and in thot
 * case the rasqal_new_query_results_formatter() constructor allows
 * selecting of a specific one by name or URI.
 *
 * Return value: a new #rasqal_query_results_formatter object or NULL on failure
 */
rasqal_query_results_formatter*
rasqal_new_query_results_formatter_by_mime_type(rasqal_world* world, const char *mime_type)
{
  rasqal_query_results_format_factory* factory;
  rasqal_query_results_formatter* formatter;

  if(!mime_type)
    return NULL;

  factory=rasqal_get_query_results_formatter_factory(world, NULL, NULL, mime_type);
  if(!factory)
    return NULL;

  formatter=(rasqal_query_results_formatter*)RASQAL_CALLOC(rasqal_query_results_formatter, 1, sizeof(rasqal_query_results_formatter));
  if(!formatter)
    return NULL;

  formatter->factory=factory;

  formatter->mime_type=factory->mime_type;
  
  return formatter;
}


/**
 * rasqal_free_query_results_formatter:
 * @formatter: #rasqal_query_results_formatter object
 * 
 * Destructor - destroy a #rasqal_query_results_formatter object.
 **/
void
rasqal_free_query_results_formatter(rasqal_query_results_formatter* formatter) 
{
  RASQAL_ASSERT_OBJECT_POINTER_RETURN(formatter, rasqal_query_results_formatter);

  RASQAL_FREE(rasqal_query_results_formatter, formatter);
}


static void
rasqal_iostream_write_json_boolean(raptor_iostream* iostr, 
                                   const char* name, int json_bool)
{
  raptor_iostream_write_byte(iostr, '\"');
  raptor_iostream_write_string(iostr, name);
  raptor_iostream_write_counted_string(iostr, "\" : ",4);

  if(json_bool)
    raptor_iostream_write_counted_string(iostr, "true", 4);
  else
    raptor_iostream_write_counted_string(iostr, "false", 5);

}


/*
 * rasqal_query_results_write_json1:
 * @iostr: #raptor_iostream to write the query to
 * @results: #rasqal_query_results query results format
 * @base_uri: #raptor_uri base URI of the output format
 *
 * Write a JSON version of the query results format to an
 * iostream in a format - INTERNAL.
 * 
 * If the writing succeeds, the query results will be exhausted.
 * 
 * Return value: non-0 on failure
 **/
static int
rasqal_query_results_write_json1(raptor_iostream *iostr,
                                 rasqal_query_results* results,
                                 raptor_uri *base_uri)
{
  rasqal_query* query=results->query;
  int i;
  int row_comma;
  int column_comma=0;
  
  if(!rasqal_query_results_is_bindings(results) &&
     !rasqal_query_results_is_boolean(results)) {
    query->failed=1;
    rasqal_log_error_simple(query->world, RAPTOR_LOG_LEVEL_ERROR,
                            &query->locator,
                            "Can only write JSON format for variable binding and boolean results");
    return 1;
  }
  
  
  raptor_iostream_write_counted_string(iostr, "{\n", 2);
  
  /* Header */
  raptor_iostream_write_counted_string(iostr, "  \"head\": {\n", 12);
  
  if(rasqal_query_results_is_bindings(results)) {
    raptor_iostream_write_counted_string(iostr, "    \"vars\": [ ", 14);
    for(i=0; 1; i++) {
      const unsigned char *name;
      
      name=rasqal_query_results_get_binding_name(results, i);
      if(!name)
        break;
      
      /*     'x', */
      if(i > 0)
        raptor_iostream_write_counted_string(iostr, ", ", 2);
      raptor_iostream_write_byte(iostr, '\"');
      raptor_iostream_write_string(iostr, name);
      raptor_iostream_write_byte(iostr, '\"');
    }
    raptor_iostream_write_counted_string(iostr, " ]\n", 3);
  }

  /* FIXME - could add link inside 'head': */
    
  /*   End Header */
  raptor_iostream_write_counted_string(iostr, "  },\n", 5);


  /* Boolean Results */
  if(rasqal_query_results_is_boolean(results)) {
    raptor_iostream_write_counted_string(iostr, "  ", 2);
    rasqal_iostream_write_json_boolean(iostr, "boolean", 
                                       rasqal_query_results_get_boolean(results));
    goto results3done;
  }

  /* Variable Binding Results */
  raptor_iostream_write_counted_string(iostr, "  \"results\": {\n", 15);

  raptor_iostream_write_counted_string(iostr, "    \"", 5);
  rasqal_iostream_write_json_boolean(iostr, "ordered", 
                                     (rasqal_query_get_order_condition(query, 0) != NULL));
  raptor_iostream_write_counted_string(iostr, ",\n", 2);

  raptor_iostream_write_counted_string(iostr, "    \"", 5);
  rasqal_iostream_write_json_boolean(iostr, "distinct", 
                                     rasqal_query_get_distinct(query));
  raptor_iostream_write_counted_string(iostr, ",\n", 2);

  raptor_iostream_write_counted_string(iostr, "    \"bindings\" : [\n", 19);

  row_comma=0;
  while(!rasqal_query_results_finished(results)) {
    if(row_comma)
      raptor_iostream_write_counted_string(iostr, ",\n", 2);

    /* Result row */
    raptor_iostream_write_counted_string(iostr, "      {\n", 8);

    column_comma=0;
    for(i=0; i<rasqal_query_results_get_bindings_count(results); i++) {
      const unsigned char *name=rasqal_query_results_get_binding_name(results, i);
      rasqal_literal *l=rasqal_query_results_get_binding_value(results, i);

      if(column_comma)
        raptor_iostream_write_counted_string(iostr, ",\n", 2);

      /*       <binding> */
      raptor_iostream_write_counted_string(iostr, "        \"", 9);
      raptor_iostream_write_string(iostr, name);
      raptor_iostream_write_counted_string(iostr, "\" : { ", 6);

      if(!l) {
        raptor_iostream_write_string(iostr, "\"type\": \"unbound\", \"value\": null");
      } else switch(l->type) {
        const unsigned char* str;
        size_t len;
        
        case RASQAL_LITERAL_URI:
          raptor_iostream_write_string(iostr, "\"type\": \"uri\", \"value\": \"");
          str=(const unsigned char*)raptor_uri_as_counted_string(l->value.uri, &len);
          raptor_iostream_write_string_ntriples(iostr, str, len, '"');
          raptor_iostream_write_byte(iostr, '"');
          break;

        case RASQAL_LITERAL_BLANK:
          raptor_iostream_write_string(iostr, "\"type\": \"bnode\", \"value\": \"");
          raptor_iostream_write_string_ntriples(iostr, (const unsigned char*)l->string, 
                                                l->string_len, '"');
          raptor_iostream_write_byte(iostr, '"');
          break;

        case RASQAL_LITERAL_STRING:
          raptor_iostream_write_string(iostr, "\"type\": \"literal\", \"value\": \"");
          raptor_iostream_write_string_ntriples(iostr, (const unsigned char*)l->string,
                                                l->string_len, '"');
          raptor_iostream_write_byte(iostr, '"');

          if(l->language) {
            raptor_iostream_write_string(iostr, ",\n      \"xml:lang\" : \"");
            raptor_iostream_write_string(iostr, (const unsigned char*)l->language);
            raptor_iostream_write_byte(iostr, '"');
          }
          
          if(l->datatype) {
            raptor_iostream_write_string(iostr, ",\n      \"datatype\" : \"");
            str=(const unsigned char*)raptor_uri_as_counted_string(l->datatype, &len);
            raptor_iostream_write_string_ntriples(iostr, str, len, '"');
            raptor_iostream_write_byte(iostr, '"');
          }
          
          break;

        case RASQAL_LITERAL_PATTERN:
        case RASQAL_LITERAL_QNAME:
        case RASQAL_LITERAL_INTEGER:
        case RASQAL_LITERAL_BOOLEAN:
        case RASQAL_LITERAL_DOUBLE:
        case RASQAL_LITERAL_FLOAT:
        case RASQAL_LITERAL_VARIABLE:
        case RASQAL_LITERAL_DECIMAL:
        case RASQAL_LITERAL_DATETIME:

        case RASQAL_LITERAL_UNKNOWN:
        default:
          query->failed=1;
          rasqal_log_error_simple(query->world, RAPTOR_LOG_LEVEL_ERROR,
                                  &query->locator,
                                  "Cannot turn literal type %d into XML", 
                                  l->type);
      }

      /* End Binding */
      raptor_iostream_write_counted_string(iostr, " }", 2);
      column_comma=1;
    }

    /* End Result Row */
    raptor_iostream_write_counted_string(iostr, "\n      }", 8);
    row_comma=1;
    
    rasqal_query_results_next(results);
  }

  raptor_iostream_write_counted_string(iostr, "\n    ]\n  }", 10);

  results3done:
  
  /* end sparql */
  raptor_iostream_write_counted_string(iostr, "\n}\n", 3);

  return 0;
}


/**
 * rasqal_query_results_formatter_get_mime_type:
 * @formatter: #rasqal_query_results_formatter object
 * 
 * Get the mime type of the syntax being formatted.
 * 
 * Return value: a shared mime type string
 **/
const char*
rasqal_query_results_formatter_get_mime_type(rasqal_query_results_formatter *formatter)
{
  return formatter->mime_type;
}


/**
 * rasqal_query_results_formatter_write:
 * @iostr: #raptor_iostream to write the query to
 * @formatter: #rasqal_query_results_formatter object
 * @results: #rasqal_query_results query results format
 * @base_uri: #raptor_uri base URI of the output format
 *
 * Write the query results using the given formatter to an iostream
 * 
 * See rasqal_query_results_formats_enumerate() to get the
 * list of syntax URIs and their description. 
 *
 * Return value: non-0 on failure
 **/
int
rasqal_query_results_formatter_write(raptor_iostream *iostr,
                                     rasqal_query_results_formatter* formatter,
                                     rasqal_query_results* results,
                                     raptor_uri *base_uri)
{
  if(!formatter->factory->writer)
     return 1;
  return formatter->factory->writer(iostr, results, base_uri);
}


/**
 * rasqal_query_results_formatter_read:
 * @world: rasqal world object
 * @iostr: #raptor_iostream to read the query from
 * @formatter: #rasqal_query_results_formatter object
 * @results: #rasqal_query_results query results format
 * @base_uri: #raptor_uri base URI of the input format
 *
 * Read the query results using the given formatter from an iostream
 * 
 * See rasqal_query_results_formats_enumerate() to get the
 * list of syntax URIs and their description. 
 *
 * Return value: non-0 on failure
 **/
int
rasqal_query_results_formatter_read(rasqal_world *world,
                                    raptor_iostream *iostr,
                                    rasqal_query_results_formatter* formatter,
                                    rasqal_query_results* results,
                                    raptor_uri *base_uri)
{
  rasqal_rowsource* rowsource=NULL;
  
  if(formatter->factory->reader)
    return formatter->factory->reader(iostr, results, base_uri);

  if(!formatter->factory->get_rowsource)
    return 1;
  
  rowsource=formatter->factory->get_rowsource(world, iostr, base_uri);
  if(!rowsource)
    return 1;

  if(rasqal_rowsource_update_variables(rowsource, results)) {
    rasqal_free_rowsource(rowsource);
    return 1;
  }

  while(1) {
    rasqal_query_result_row* row=rasqal_rowsource_read_row(rowsource);
    if(!row)
      break;
    rasqal_query_results_add_row(results, row);
  }

  if(rowsource)
    rasqal_free_rowsource(rowsource);
  
  return 0;
}
