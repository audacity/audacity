/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rasqal_query_results.c - Rasqal RDF Query Results
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


int
rasqal_init_query_results(void)
{
  return 0;
}


void
rasqal_finish_query_results(void)
{
}


/*
 * rasqal_new_query_results:
 * @query: query object
 * 
 * Internal -  create a query result for a query
 * 
 * Return value: a new query result object or NULL on failure
 **/
rasqal_query_results*  
rasqal_new_query_results(rasqal_query* query)
{
  rasqal_query_results* query_results;
    
  query_results=(rasqal_query_results*)RASQAL_CALLOC(rasqal_query_results, 1, sizeof(rasqal_query_results));
  if(query_results) {
    query_results->query=query;

    if(query) {
      if(query->query_results_formatter_name)
        query_results->type=RASQAL_QUERY_RESULTS_SYNTAX;
      else
        switch(query->verb) {
          case RASQAL_QUERY_VERB_SELECT:
            query_results->type=RASQAL_QUERY_RESULTS_BINDINGS;
            rasqal_query_results_set_variables(query_results,
                                               query->variables_sequence,
                                               query->select_variables_count,
                                               0);
            break;
          case RASQAL_QUERY_VERB_ASK:
            query_results->type=RASQAL_QUERY_RESULTS_BOOLEAN;
            break;
          case RASQAL_QUERY_VERB_CONSTRUCT:
          case RASQAL_QUERY_VERB_DESCRIBE:
            query_results->type=RASQAL_QUERY_RESULTS_GRAPH;
            break;
            
          case RASQAL_QUERY_VERB_UNKNOWN:
          case RASQAL_QUERY_VERB_DELETE:
          case RASQAL_QUERY_VERB_INSERT:
          default:
            break;
        }
    }
    rasqal_query_results_reset(query_results);
  }
  
  return query_results;
}


void
rasqal_query_results_reset(rasqal_query_results* query_results)
{
  query_results->result_count=0;
  query_results->abort=0;
  query_results->finished=0;
  query_results->failed=0;
  query_results->ask_result= -1;
  query_results->current_triple_result= -1;
  query_results->results_sequence=NULL;
}


/**
 * rasqal_free_query_results:
 * @query_results: #rasqal_query_results object
 *
 * Destructor - destroy a rasqal_query_results.
 *
 **/
void
rasqal_free_query_results(rasqal_query_results* query_results)
{
  rasqal_query* query;

  RASQAL_ASSERT_OBJECT_POINTER_RETURN(query_results, rasqal_query_result);

  query=query_results->query;
  
  if(query_results->executed)
    rasqal_engine_execute_finish(query_results);

  if(query_results->row)
    rasqal_free_query_result_row(query_results->row);

  if(query && query_results->execution_data && 
     query_results->free_execution_data)
    query_results->free_execution_data(query, query_results, query_results->execution_data);
  
  if(query_results->results_sequence)
    raptor_free_sequence(query_results->results_sequence);

  if(query_results->triple)
    rasqal_free_triple(query_results->triple);

  if(query_results->variables)
    RASQAL_FREE(varray, query_results->variables);

  if(query_results->variable_names)
    RASQAL_FREE(cstrings, query_results->variable_names);

  if(query_results->variables_sequence)
    raptor_free_sequence(query_results->variables_sequence);
  
  if(query_results->rowsource)
    rasqal_free_rowsource(query_results->rowsource);
  
  if(query)
    rasqal_query_remove_query_result(query, query_results);

  RASQAL_FREE(rasqal_query_results, query_results);
}


/**
 * rasqal_query_results_is_bindings:
 * @query_results: #rasqal_query_results object
 *
 * Test if rasqal_query_results is variable bindings format.
 * 
 * Return value: non-0 if true
 **/
int
rasqal_query_results_is_bindings(rasqal_query_results* query_results)
{
  return (query_results->type == RASQAL_QUERY_RESULTS_BINDINGS);
}


/**
 * rasqal_query_results_is_boolean:
 * @query_results: #rasqal_query_results object
 *
 * Test if rasqal_query_results is boolean format.
 * 
 * Return value: non-0 if true
 **/
int
rasqal_query_results_is_boolean(rasqal_query_results* query_results)
{
  return (query_results->type == RASQAL_QUERY_RESULTS_BOOLEAN);
}
 

/**
 * rasqal_query_results_is_graph:
 * @query_results: #rasqal_query_results object
 *
 * Test if rasqal_query_results is RDF graph format.
 * 
 * Return value: non-0 if true
 **/
int
rasqal_query_results_is_graph(rasqal_query_results* query_results)
{
  return (query_results->type == RASQAL_QUERY_RESULTS_GRAPH);
}


/**
 * rasqal_query_results_is_syntax:
 * @query_results: #rasqal_query_results object
 *
 * Test if the rasqal_query_results is a syntax.
 *
 * Many of the query results may be formatted as a syntax using the
 * #rasqal_query_formatter class however this function returns true
 * if a syntax result was specifically requested.
 * 
 * Return value: non-0 if true
 **/
int
rasqal_query_results_is_syntax(rasqal_query_results* query_results)
{
  return (query_results->type == RASQAL_QUERY_RESULTS_SYNTAX);
}


/**
 * rasqal_query_results_get_count:
 * @query_results: #rasqal_query_results query_results
 *
 * Get number of bindings so far.
 * 
 * Return value: number of bindings found so far or < 0 on failure
 **/
int
rasqal_query_results_get_count(rasqal_query_results* query_results)
{
  rasqal_query* query;

  if(!query_results || query_results->failed)
    return -1;

  if(!rasqal_query_results_is_bindings(query_results))
    return -1;
  
  query=query_results->query;
  if(query && query->offset > 0)
    return query_results->result_count - query->offset;
  return query_results->result_count;
}


/**
 * rasqal_query_results_next:
 * @query_results: #rasqal_query_results query_results
 *
 * Move to the next result.
 * 
 * Return value: non-0 if failed or results exhausted
 **/
int
rasqal_query_results_next(rasqal_query_results* query_results)
{
  if(!query_results || query_results->failed || query_results->finished)
    return 1;
  
  if(!rasqal_query_results_is_bindings(query_results))
    return 1;

  return rasqal_engine_execute_next(query_results);
}


/**
 * rasqal_query_results_finished:
 * @query_results: #rasqal_query_results query_results
 *
 * Find out if binding results are exhausted.
 * 
 * Return value: non-0 if results are finished or query failed
 **/
int
rasqal_query_results_finished(rasqal_query_results* query_results)
{
  if(!query_results)
    return 1;
  
  if(!rasqal_query_results_is_bindings(query_results))
    return 1;
  
  return (query_results->failed || query_results->finished);
}


/**
 * rasqal_query_results_get_bindings:
 * @query_results: #rasqal_query_results query_results
 * @names: pointer to an array of binding names (or NULL)
 * @values: pointer to an array of binding value #rasqal_literal (or NULL)
 *
 * Get all binding names, values for current result.
 * 
 * If names is not NULL, it is set to the address of a shared array
 * of names of the bindings (an output parameter).  These names
 * are shared and must not be freed by the caller
 *
 * If values is not NULL, it is set to the address of a shared array
 * of #rasqal_literal* binding values.  These values are shaerd
 * and must not be freed by the caller.
 * 
 * Return value: non-0 if the assignment failed
 **/
int
rasqal_query_results_get_bindings(rasqal_query_results* query_results,
                                  const unsigned char ***names, 
                                  rasqal_literal ***values)
{
  if(!query_results)
    return 1;
  
  if(!rasqal_query_results_is_bindings(query_results))
    return 1;
  
  if(names)
    *names=query_results->variable_names;
  
  if(values)
    *values=rasqal_engine_get_result_values(query_results);
    
  return 0;
}


/**
 * rasqal_query_results_get_binding_value:
 * @query_results: #rasqal_query_results query_results
 * @offset: offset of binding name into array of known names
 *
 * Get one binding value for the current result.
 * 
 * Return value: a pointer to a shared #rasqal_literal binding value or NULL on failure
 **/
rasqal_literal*
rasqal_query_results_get_binding_value(rasqal_query_results* query_results, 
                                       int offset)
{
  if(!query_results)
    return NULL;
  
  if(!rasqal_query_results_is_bindings(query_results))
    return NULL;
  
  if(offset < 0 || offset > query_results->size-1)
    return NULL;

  return rasqal_engine_get_result_value(query_results, offset);
}


/**
 * rasqal_query_results_get_binding_name:
 * @query_results: #rasqal_query_results query_results
 * @offset: offset of binding name into array of known names
 *
 * Get binding name for the current result.
 * 
 * Return value: a pointer to a shared copy of the binding name or NULL on failure
 **/
const unsigned char*
rasqal_query_results_get_binding_name(rasqal_query_results* query_results, 
                                      int offset)
{
  if(!query_results || !query_results->variables)
    return NULL;
  
  if(!rasqal_query_results_is_bindings(query_results)) 
    return NULL;
  
  if(offset < 0 || offset > query_results->size-1)
    return NULL;
  
  return query_results->variables[offset]->name;
}


/**
 * rasqal_query_results_get_binding_value_by_name:
 * @query_results: #rasqal_query_results query_results
 * @name: variable name
 *
 * Get one binding value for a given name in the current result.
 * 
 * Return value: a pointer to a shared #rasqal_literal binding value or NULL on failure
 **/
rasqal_literal*
rasqal_query_results_get_binding_value_by_name(rasqal_query_results* query_results,
                                               const unsigned char *name)
{
  int offset= -1;
  int i;
  rasqal_literal* value=NULL;

  if(!query_results)
    return NULL;
  
  if(!rasqal_query_results_is_bindings(query_results))
    return NULL;
  
  for(i=0; i< query_results->size; i++) {
    if(!strcmp((const char*)name, (const char*)query_results->variables[i]->name)) {
      offset=i;
      break;
    }
  }
  
  if(offset < 0)
    return NULL;

  value=rasqal_engine_get_result_value(query_results, offset);

  return value;
}


/**
 * rasqal_query_results_get_bindings_count:
 * @query_results: #rasqal_query_results query_results
 *
 * Get the number of bound variables in the result.
 * 
 * Return value: <0 if failed or results exhausted
 **/
int
rasqal_query_results_get_bindings_count(rasqal_query_results* query_results)
{
  if(!query_results || query_results->failed)
    return -1;
  
  if(!rasqal_query_results_is_bindings(query_results))
    return -1;
  
  return query_results->size;
}


static unsigned char*
rasqal_prefix_id(int prefix_id, unsigned char *string)
{
  int tmpid=prefix_id;
  unsigned char* buffer;
  size_t length=strlen((const char*)string)+4;  /* "r" +... + "_" +... \0 */

  while(tmpid/=10)
    length++;
  
  buffer=(unsigned char*)RASQAL_MALLOC(cstring, length);
  if(!buffer)
    return NULL;
  
  sprintf((char*)buffer, "r%d_%s", prefix_id, string);
  
  return buffer;
}


/**
 * rasqal_query_results_get_triple:
 * @query_results: #rasqal_query_results query_results
 *
 * Get the current triple in the result.
 *
 * The return value is a shared #raptor_statement.
 * 
 * Return value: #raptor_statement or NULL if failed or results exhausted
 **/
raptor_statement*
rasqal_query_results_get_triple(rasqal_query_results* query_results)
{
  rasqal_query* query;
  int rc;
  rasqal_triple *t;
  rasqal_literal *s, *p, *o;
  raptor_statement *rs;
  unsigned char *nodeid;
  int skipped;
  
  if(!query_results || query_results->failed || query_results->finished)
    return NULL;
  
  if(!rasqal_query_results_is_graph(query_results))
    return NULL;
  
  query=query_results->query;
  if(query->verb == RASQAL_QUERY_VERB_DESCRIBE)
    return NULL;

  skipped=0;
  while(1) {
    if(skipped) {
      rc=rasqal_engine_execute_next(query_results);
      if(rc) {
        rs=NULL;
        break;
      }
      query_results->current_triple_result= -1;
    }
    
    if(query_results->current_triple_result < 0)
      query_results->current_triple_result=0;

    t=(rasqal_triple*)raptor_sequence_get_at(query->constructs,
                                             query_results->current_triple_result);

    rs=&query_results->result_triple;

    s=rasqal_literal_as_node(t->subject);
    if(!s) {
      rasqal_log_error_simple(query->world, RAPTOR_LOG_LEVEL_WARNING,
                              &query->locator,
                              "Triple with unbound subject skipped");
      skipped=1;
      continue;
    }
    switch(s->type) {
      case RASQAL_LITERAL_URI:
        rs->subject=s->value.uri;
        rs->subject_type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
        break;

      case RASQAL_LITERAL_BLANK:
        nodeid=rasqal_prefix_id(query_results->result_count, (unsigned char*)s->string);
        rasqal_free_literal(s);
        if(!nodeid) {
          rasqal_log_error_simple(query->world, RAPTOR_LOG_LEVEL_FATAL,
                                  &query->locator,
                                  "Could not prefix subject blank identifier");
          return NULL;
        }
        s=rasqal_new_simple_literal(query->world, RASQAL_LITERAL_BLANK, nodeid);
        if(!s) {
          rasqal_log_error_simple(query->world, RAPTOR_LOG_LEVEL_FATAL,
                                  &query->locator,
                                  "Could not create a new subject blank literal");
          return NULL;
        }
        rs->subject=nodeid;
        rs->subject_type=RAPTOR_IDENTIFIER_TYPE_ANONYMOUS;
        break;

      case RASQAL_LITERAL_QNAME:
      case RASQAL_LITERAL_PATTERN:
      case RASQAL_LITERAL_BOOLEAN:
      case RASQAL_LITERAL_INTEGER:
      case RASQAL_LITERAL_DOUBLE:
      case RASQAL_LITERAL_FLOAT:
      case RASQAL_LITERAL_VARIABLE:
      case RASQAL_LITERAL_DECIMAL:
      case RASQAL_LITERAL_DATETIME:
        /* QNames should be gone by the time expression eval happens
         * Everything else is removed by rasqal_literal_as_node() above. 
         */

      case RASQAL_LITERAL_STRING:
        /* string [literal] subjects are not RDF */

      case RASQAL_LITERAL_UNKNOWN:
      default:
        /* case RASQAL_LITERAL_STRING: */
        rasqal_log_error_simple(query->world, RAPTOR_LOG_LEVEL_WARNING,
                                &query->locator,
                                "Triple with non-URI/blank node subject skipped");
        skipped=1;
        break;
    }
    if(skipped) {
      if(s)
        rasqal_free_literal(s);
      continue;
    }
    

    p=rasqal_literal_as_node(t->predicate);
    if(!p) {
      rasqal_log_error_simple(query->world, RAPTOR_LOG_LEVEL_WARNING,
                              &query->locator,
                              "Triple with unbound predicate skipped");
      rasqal_free_literal(s);
      skipped=1;
      continue;
    }
    switch(p->type) {
      case RASQAL_LITERAL_URI:
        rs->predicate=p->value.uri;
        rs->predicate_type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
        break;

      case RASQAL_LITERAL_QNAME:
      case RASQAL_LITERAL_PATTERN:
      case RASQAL_LITERAL_BOOLEAN:
      case RASQAL_LITERAL_INTEGER:
      case RASQAL_LITERAL_DOUBLE:
      case RASQAL_LITERAL_FLOAT:
      case RASQAL_LITERAL_VARIABLE:
      case RASQAL_LITERAL_DECIMAL:
      case RASQAL_LITERAL_DATETIME:
        /* QNames should be gone by the time expression eval happens
         * Everything else is removed by rasqal_literal_as_node() above. 
         */

      case RASQAL_LITERAL_BLANK:
      case RASQAL_LITERAL_STRING:
        /* blank node or string [literal] predicates are not RDF */

      case RASQAL_LITERAL_UNKNOWN:
      default:
        rasqal_log_error_simple(query->world, RAPTOR_LOG_LEVEL_WARNING,
                                &query->locator,
                                "Triple with non-URI predicate skipped");
        skipped=1;
        break;
    }
    if(skipped) {
      rasqal_free_literal(s);
      if(p)
        rasqal_free_literal(p);
      continue;
    }

    o=rasqal_literal_as_node(t->object);
    if(!o) {
      rasqal_log_error_simple(query->world, RAPTOR_LOG_LEVEL_WARNING,
                              &query->locator,
                              "Triple with unbound object skipped");
      rasqal_free_literal(s);
      rasqal_free_literal(p);
      skipped=1;
      continue;
    }
    switch(o->type) {
      case RASQAL_LITERAL_URI:
        rs->object=o->value.uri;
        rs->object_type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
        break;

      case RASQAL_LITERAL_BLANK:
        nodeid=rasqal_prefix_id(query_results->result_count, (unsigned char*)o->string);
        rasqal_free_literal(o);
        if(!nodeid) {
          rasqal_log_error_simple(query->world, RAPTOR_LOG_LEVEL_FATAL,
                                  &query->locator,
                                  "Could not prefix blank identifier");
          rasqal_free_literal(s);
          rasqal_free_literal(p);
          return NULL;
        }
        o=rasqal_new_simple_literal(query->world, RASQAL_LITERAL_BLANK, nodeid);
        if(!o) {
          rasqal_log_error_simple(query->world, RAPTOR_LOG_LEVEL_FATAL,
                                  &query->locator,
                                  "Could not create a new subject blank literal");
          rasqal_free_literal(s);
          rasqal_free_literal(p);
          return NULL;
        }
        rs->object=nodeid;
        rs->object_type=RAPTOR_IDENTIFIER_TYPE_ANONYMOUS;
        break;

      case RASQAL_LITERAL_STRING:
        rs->object=o->string;
        rs->object_literal_language=(const unsigned char*)o->language;
        rs->object_literal_datatype=o->datatype;
        rs->object_type=RAPTOR_IDENTIFIER_TYPE_LITERAL;
        break;

      case RASQAL_LITERAL_QNAME:
      case RASQAL_LITERAL_PATTERN:
      case RASQAL_LITERAL_BOOLEAN:
      case RASQAL_LITERAL_INTEGER:
      case RASQAL_LITERAL_DOUBLE:
      case RASQAL_LITERAL_FLOAT:
      case RASQAL_LITERAL_VARIABLE:
      case RASQAL_LITERAL_DECIMAL:
      case RASQAL_LITERAL_DATETIME:
        /* QNames should be gone by the time expression eval happens
         * Everything else is removed by rasqal_literal_as_node() above. 
         */

      case RASQAL_LITERAL_UNKNOWN:
      default:
        rasqal_log_error_simple(query->world, RAPTOR_LOG_LEVEL_WARNING,
                                &query->locator,
                                "Triple with unknown object skipped");
        skipped=1;
        break;
    }
    if(skipped) {
      rasqal_free_literal(s);
      rasqal_free_literal(p);
      if(o)
        rasqal_free_literal(o);
      continue;
    }
    
    /* dispose previous triple if any */
    if(query_results->triple) {
      rasqal_free_triple(query_results->triple);
      query_results->triple=NULL;
    }

    /* for saving s, p, o for later disposal */
    query_results->triple=rasqal_new_triple(s, p, o);

    /* got triple, return it */
    break;
  }
  
  return rs;
}


/**
 * rasqal_query_results_next_triple:
 * @query_results: #rasqal_query_results query_results
 *
 * Move to the next triple result.
 * 
 * Return value: non-0 if failed or results exhausted
 **/
int
rasqal_query_results_next_triple(rasqal_query_results* query_results)
{
  rasqal_query* query;
  int rc=0;
  
  if(!query_results || query_results->failed || query_results->finished)
    return 1;
  
  if(!rasqal_query_results_is_graph(query_results))
    return 1;
  
  query=query_results->query;
  if(query->verb == RASQAL_QUERY_VERB_DESCRIBE)
    return 1;
  
  if(query_results->triple) {
    rasqal_free_triple(query_results->triple);
    query_results->triple=NULL;
  }

  if(++query_results->current_triple_result >= raptor_sequence_size(query->constructs)) {
    rc=rasqal_engine_execute_next(query_results);
    if(rc)
      return 1;
    
    query_results->current_triple_result= -1;
  }
  
  return rc;
}


/**
 * rasqal_query_results_get_boolean:
 * @query_results: #rasqal_query_results query_results
 *
 * Get boolean query result.
 *
 * The return value is only meaningful if this is a boolean
 * query result - see rasqal_query_results_is_boolean()
 *
 * Return value: boolean query result - >0 is true, 0 is false, <0 on error
 */
int
rasqal_query_results_get_boolean(rasqal_query_results* query_results)
{
  if(!query_results || query_results->failed)
    return -1;
  
  if(!rasqal_query_results_is_boolean(query_results))
    return -1;
  
  if(query_results->ask_result >= 0)
    return query_results->ask_result;

  query_results->ask_result= (query_results->result_count > 0) ? 1 : 0;
  query_results->finished= 1;
  
  return query_results->ask_result;
}

/**
 * rasqal_query_results_write:
 * @iostr: #raptor_iostream to write the query to
 * @results: #rasqal_query_results query results format
 * @format_uri: #raptor_uri describing the format to write (or NULL for default)
 * @base_uri: #raptor_uri base URI of the output format
 *
 * Write the query results to an iostream in a format.
 * 
 * This uses the #rasqal_query_results_formatter class
 * and the rasqal_query_results_formatter_write() method
 * to perform the formatting. See
 * rasqal_query_results_formats_enumerate() 
 * for obtaining the supported format URIs at run time.
 *
 * Return value: non-0 on failure
 **/
int
rasqal_query_results_write(raptor_iostream *iostr,
                           rasqal_query_results* results,
                           raptor_uri *format_uri,
                           raptor_uri *base_uri)
{
  rasqal_query_results_formatter *formatter;
  int status;
  
  if(!results || results->failed)
    return 1;

  formatter=rasqal_new_query_results_formatter(results->query->world, NULL, format_uri);
  if(!formatter)
    return 1;

  status=rasqal_query_results_formatter_write(iostr, formatter,
                                              results, base_uri);

  rasqal_free_query_results_formatter(formatter);
  return status;
}


/**
 * rasqal_new_query_result_row:
 * @rowsource: rowsource
 *
 * INTERNAL - Create a new query result row at an offset into the result sequence.
 *
 * Return value: a new query result row or NULL on failure
 */
rasqal_query_result_row*
rasqal_new_query_result_row(rasqal_rowsource* rowsource)
{
  rasqal_query_result_row* row;
  
  row=(rasqal_query_result_row*)RASQAL_CALLOC(rasqal_query_result_row, 1,
                                              sizeof(rasqal_query_result_row));
  if(!row)
    return NULL;

  row->usage=1;
  row->rowsource=rowsource;

  rasqal_rowsource_get_sizes(rowsource, &row->size, &row->order_size);
  
  row->values=(rasqal_literal**)RASQAL_CALLOC(array, row->size,
					      sizeof(rasqal_literal*));
  if(!row->values) {
    rasqal_free_query_result_row(row);
    return NULL;
  }

  if(row->order_size > 0) {
    row->order_values=(rasqal_literal**)RASQAL_CALLOC(array,  row->order_size,
                                                      sizeof(rasqal_literal*));
    if(!row->order_values) {
      rasqal_free_query_result_row(row);
      return NULL;
    }
  }
  
  return row;
}


/**
 * rasqal_new_query_result_row_from_query_result_row:
 * @row: query result row
 * 
 * INTERNAL - Copy a query result row.
 *
 * Return value: a copy of the query result row or NULL
 */
rasqal_query_result_row*
rasqal_new_query_result_row_from_query_result_row(rasqal_query_result_row* row)
{
  row->usage++;
  return row;
}


/**
 * rasqal_free_query_result_row:
 * @row: query result row
 * 
 * INTERNAL - Free a query result row object.
 */
void 
rasqal_free_query_result_row(rasqal_query_result_row* row)
{
  RASQAL_ASSERT_OBJECT_POINTER_RETURN(row, rasqal_query_result_row);

  if(--row->usage)
    return;
  
  if(row->values) {
    int i; 
    for(i=0; i < row->size; i++) {
      if(row->values[i])
        rasqal_free_literal(row->values[i]);
    }
    RASQAL_FREE(array, row->values);
  }
  if(row->order_values) {
    int i; 
    for(i=0; i < row->order_size; i++) {
      if(row->order_values[i])
        rasqal_free_literal(row->order_values[i]);
    }
    RASQAL_FREE(array, row->order_values);
  }

  RASQAL_FREE(rasqal_query_result_row, row);
}


int
rasqal_query_results_set_variables(rasqal_query_results* query_results,
                                   raptor_sequence* variables_sequence,
                                   int size, int order_size)
{
  int i;

  /* Set query_results size and order size initially to zero
   * until all initialization that can fail has been done.
   * Ensure size is never larger than the number of valid pointers in
   * variables or variable_names arrays.
   */
  query_results->size=0;
  query_results->order_size=0;
  
  if(query_results->variables_sequence) {
    raptor_free_sequence(query_results->variables_sequence);
    query_results->variables_sequence=NULL;
  }
  query_results->variables_sequence=raptor_new_sequence((raptor_sequence_free_handler*)rasqal_free_variable, (raptor_sequence_print_handler*)rasqal_variable_print);
  if(!query_results->variables_sequence)
    return 1;

  if(query_results->variable_names) {
    RASQAL_FREE(cstrings, query_results->variable_names);
    query_results->variable_names=NULL;
  }
  query_results->variable_names=(const unsigned char**)RASQAL_CALLOC(cstrings, sizeof(unsigned char*), (size+1));
  if(!query_results->variable_names)
    return 1;

  if(query_results->variables) {
    RASQAL_FREE(varray, query_results->variables);
    query_results->variables=NULL;
  }  
  query_results->variables=(rasqal_variable**)RASQAL_CALLOC(varray, sizeof(rasqal_variable*), size);
  if(!query_results->variables)
    return 1;

  for(i=0; i < size; i++) {
    rasqal_variable* v=(rasqal_variable*)raptor_sequence_get_at(variables_sequence, i);
    rasqal_variable* new_v=rasqal_new_variable_from_variable(v);
    if(new_v) {
      raptor_sequence_push(query_results->variables_sequence, new_v);
      query_results->variables[i]=new_v;
      query_results->variable_names[i]=new_v->name;
    } else
      return 1;
  }

  /* Initialization ok - now set size and order_size for real */
  query_results->size=size;
  query_results->order_size=order_size;

  return 0;
}


void
rasqal_query_results_set_order_conditions(rasqal_query_results* query_results,
                                          int size)
{
  query_results->order_size=size;
}


/**
 * rasqal_query_result_row_print:
 * @row: query result row
 * @fp: FILE* handle
 *
 * INTERNAL - Print a query result row.
 */
void 
rasqal_query_result_row_print(rasqal_query_result_row* row, FILE* fh)
{
  rasqal_rowsource* rowsource=row->rowsource;
  int i;
  
  fputs("result[", fh);
  for(i=0; i < row->size; i++) {
    /* Do not use rasqal_query_results_get_binding_name(row->results, i); 
     * as it does not work for a construct result
     */
    const unsigned char *name=NULL;
    rasqal_literal *value;

    if(rowsource) {
      rasqal_variable* v=rasqal_rowsource_get_variable_by_offset(rowsource, i);
      if(v)
        name=v->name;
    }
    
    value=row->values[i];
    if(i > 0)
      fputs(", ", fh);
    if(name)
      fprintf(fh, "%s=", name);

    if(value)
      rasqal_literal_print(value, fh);
    else
      fputs("NULL", fh);
  }

  if(row->order_size > 0) {
    fputs(" with ordering values [", fh);

    for(i=0; i < row->order_size; i++) {
      rasqal_literal *value=row->order_values[i];
      
      if(i > 0)
        fputs(", ", fh);
      if(value)
        rasqal_literal_print(value, fh);
      else
        fputs("NULL", fh);
    }
    fputs("]", fh);

  }

  fprintf(fh, " offset %d]", row->offset);
}


/**
 * rasqal_query_results_read:
 * @iostr: #raptor_iostream to read the query from
 * @results: #rasqal_query_results query results format
 * @format_uri: #raptor_uri describing the format to read (or NULL for default)
 * @base_uri: #raptor_uri base URI of the input format
 *
 * Read the query results from an iostream in a format.
 * 
 * This uses the #rasqal_query_results_formatter class
 * and the rasqal_query_results_formatter_read() method
 * to perform the formatting. See
 * rasqal_query_results_formats_enumerate() 
 * for obtaining the supported format URIs at run time.
 *
 * Return value: non-0 on failure
 **/
int
rasqal_query_results_read(raptor_iostream *iostr,
                          rasqal_query_results* results,
                          raptor_uri *format_uri,
                          raptor_uri *base_uri)
{
  rasqal_query_results_formatter *formatter;
  int status;
  
  if(!results || results->failed)
    return 1;

  formatter=rasqal_new_query_results_formatter(results->query->world, NULL, format_uri);
  if(!formatter)
    return 1;

  status=rasqal_query_results_formatter_read(results->query->world, iostr, formatter,
                                             results, base_uri);

  rasqal_free_query_results_formatter(formatter);
  return status;
}


/**
 * rasqal_query_results_add_row:
 * @query_results: query results object
 * @row: query result row
 *
 * INTERNAL - Add a query result row to a sequence
 */
void
rasqal_query_results_add_row(rasqal_query_results* query_results,
                             rasqal_query_result_row* row)
{
  if(!query_results->results_sequence) {
    query_results->results_sequence=raptor_new_sequence((raptor_sequence_free_handler*)rasqal_free_query_result_row, (raptor_sequence_print_handler*)rasqal_query_result_row_print);
    query_results->result_count= 1;
  }
  row->offset=query_results->result_count-1;
  raptor_sequence_push(query_results->results_sequence, row);
}


/**
 * rasqal_query_result_row_set_value_at:
 * @row: query result row
 * @offset: offset into row (column number)
 * @value: literal value to set
 *
 * INTERNAL - Set the value of a variable in a query result row
 */
void
rasqal_query_result_row_set_value_at(rasqal_query_result_row* row, int offset,
                                     rasqal_literal* value)
{
  row->values[offset]=value;
}
