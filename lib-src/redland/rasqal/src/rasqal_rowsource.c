/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rasqal_rowsource.c - Rasqal query row generator class
 *
 * Copyright (C) 2008, David Beckett http://www.dajobe.org/
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

#include <raptor.h>

#include "rasqal.h"
#include "rasqal_internal.h"


#ifndef STANDALONE


/**
 * rasqal_new_rowsource_from_handler:
 * @user_data: pointer to context information to pass in to calls
 * @handler: pointer to handler methods
 *
 * Create a new rowsource over a user-defined handler.
 *
 * Return value: new #rasqal_rowsource object or NULL on failure
 **/
rasqal_rowsource*
rasqal_new_rowsource_from_handler(void *user_data,
                                  const rasqal_rowsource_handler *handler,
                                  int flags)
{
  rasqal_rowsource* rowsource;

  if(!handler)
    return NULL;

  if(handler->version < 1 || handler->version > 1)
    return NULL;

  rowsource=(rasqal_rowsource*)RASQAL_CALLOC(rasqal_rowsource, 1,
                                             sizeof(rasqal_rowsource));
  if(!rowsource) {
    if(handler->finish)
      handler->finish(NULL, user_data);
    return NULL;
  }

  rowsource->user_data=(void*)user_data;
  rowsource->handler=handler;
  rowsource->flags=flags;

  rowsource->size= -1;
  rowsource->order_size= -1;

  rowsource->variables_sequence=raptor_new_sequence((raptor_sequence_free_handler*)rasqal_free_variable, (raptor_sequence_print_handler*)rasqal_variable_print);
  if(!rowsource->variables_sequence) {
    rasqal_free_rowsource(rowsource);
    return NULL;
  }
  
  if(rowsource->handler->init && 
     rowsource->handler->init(rowsource, rowsource->user_data)) {
    rasqal_free_rowsource(rowsource);
    return NULL;
  }
  return rowsource;
}


/**
 * rasqal_free_rowsource:
 * @rowsource: rowsource object
 *
 * Destructor - destroy an rowsource.
 **/
void
rasqal_free_rowsource(rasqal_rowsource *rowsource)
{
  RASQAL_ASSERT_OBJECT_POINTER_RETURN(rowsource, rasqal_rowsource);

  if(rowsource->handler->finish)
    rowsource->handler->finish(rowsource, rowsource->user_data);

  if(rowsource->variables_sequence)
    raptor_free_sequence(rowsource->variables_sequence);

  RASQAL_FREE(rasqal_rowsource, rowsource);
}



/**
 * rasqal_rowsource_add_variable:
 * @rowsource: rasqal rowsource
 * @v: variable
 *
 * Add a variable column to the rowsource
 **/
void
rasqal_rowsource_add_variable(rasqal_rowsource *rowsource, rasqal_variable* v)
{
  raptor_sequence_push(rowsource->variables_sequence, v);

  if(rowsource->size < 0)
    rowsource->size=0;
  
  rowsource->size++;
  if(rowsource->flags & RASQAL_ROWSOURCE_FLAGS_ORDERING) {
    if(rowsource->order_size < 0)
      rowsource->order_size=0;
    rowsource->order_size++;
  }
}



/**
 * rasqal_rowsource_update_variables:
 * @rowsource: rasqal rowsource
 *
 * Set the rowsource metadata
 *
 * Return value: non-0 on failure
 **/
int
rasqal_rowsource_update_variables(rasqal_rowsource *rowsource,
                                  rasqal_query_results* results)
{
  if(rowsource->finished)
    return 1;

  if(rowsource->handler->ensure_variables)
     rowsource->handler->ensure_variables(rowsource, rowsource->user_data);

  RASQAL_DEBUG2("Setting results to hold %d variables\n", rowsource->size);
  rasqal_query_results_set_variables(results, rowsource->variables_sequence,
                                     rowsource->size, 0);

  return 0;
}


/**
 * rasqal_rowsource_read_row:
 * @rowsource: rasqal rowsource
 *
 * Read a query result row from the rowsource.
 *
 * If a row is returned, it is owned by the caller.
 *
 * Return value: row or NULL when no more rows are available
 **/
rasqal_query_result_row*
rasqal_rowsource_read_row(rasqal_rowsource *rowsource)
{
  rasqal_query_result_row* row=NULL;
  
  if(rowsource->finished)
    return NULL;

  if(rowsource->handler->read_row)
    row=rowsource->handler->read_row(rowsource, rowsource->user_data);

  if(!row)
    rowsource->finished=1;
  else
    rowsource->count++;

  return row;
}


/**
 * rasqal_rowsource_get_row_count:
 * @rowsource: rasqal rowsource
 *
 * Get number of rows seen from a rowsource.
 *
 * Return value: row count
 **/
int
rasqal_rowsource_get_rows_count(rasqal_rowsource *rowsource)
{
  return rowsource->count;
}


/**
 * rasqal_rowsource_read_all_rows:
 * @rowsource: rasqal rowsource
 *
 * Read all rows from a rowsource
 *
 * After calling this, the rowsource will be empty of rows and finished
 * and if a sequence is returned, it is owned by the caller.
 *
 * Return value: new sequence of all rows
 **/
raptor_sequence*
rasqal_rowsource_read_all_rows(rasqal_rowsource *rowsource)
{
  raptor_sequence* seq;

  if(rowsource->handler->read_all_rows)
    return rowsource->handler->read_all_rows(rowsource, rowsource->user_data);
  
  seq=raptor_new_sequence((raptor_sequence_free_handler*)rasqal_free_query_result_row, (raptor_sequence_print_handler*)rasqal_query_result_row_print);
  if(!seq)
    return NULL;
  
  while(1) {
    rasqal_query_result_row* row=rasqal_rowsource_read_row(rowsource);
    if(!row)
      break;
    raptor_sequence_push(seq, row);
  }
  
  return seq;
}


/**
 * rasqal_rowsource_get_query:
 * @rowsource: rasqal rowsource
 *
 * Get a query associated with a rowsource
 *
 * Return value: query or NULL
 **/
rasqal_query*
rasqal_rowsource_get_query(rasqal_rowsource *rowsource)
{
  if(rowsource->handler->get_query)
    return rowsource->handler->get_query(rowsource, rowsource->user_data);
  return NULL;
}


/**
 * rasqal_rowsource_get_sizes:
 * @rowsource: rasqal rowsource
 * @size_p: pointer to size (or NULL)
 * @order_size_p: pointer to order size (or NULL)
 *
 * Get rowsource row width and row ordering width sizes
 **/
void
rasqal_rowsource_get_sizes(rasqal_rowsource *rowsource,
                           int* size_p, int* order_size_p)
{
  if(rowsource->handler->ensure_variables)
     rowsource->handler->ensure_variables(rowsource, rowsource->user_data);

  if(size_p)
    *size_p=rowsource->size;
  if(order_size_p)
    *order_size_p=rowsource->order_size;
}


/**
 * rasqal_rowsource_get_variable_by_offset:
 * @rowsource: rasqal rowsource
 * @offset: integer offset into array of variables
 *
 * Get the variable associated with the given offset
 *
 * Return value: pointer to shared #rasqal_variable or NULL if out of range
 **/
rasqal_variable*
rasqal_rowsource_get_variable_by_offset(rasqal_rowsource *rowsource, int offset)
{
  if(!rowsource->variables_sequence)
    return NULL;
  
  return (rasqal_variable*)raptor_sequence_get_at(rowsource->variables_sequence,
                                                  offset);
}


/**
 * rasqal_rowsource_get_variable_offset_by_name:
 * @rowsource: rasqal rowsource
 * @name: variable name
 *
 * Get the offset of a variable into the list of variables
 *
 * Return value: offset or <0 if not present
 **/
int
rasqal_rowsource_get_variable_offset_by_name(rasqal_rowsource *rowsource,
                                             const char* name)
{
  int offset= -1;
  int i;
  
  if(!rowsource->variables_sequence)
    return -1;
  
  for(i=0; i < raptor_sequence_size(rowsource->variables_sequence); i++) {
    rasqal_variable* v;
    v=(rasqal_variable*)raptor_sequence_get_at(rowsource->variables_sequence, i);
    if(!strcmp((const char*)v->name, name)) {
      offset=i;
      break;
    }
  }

  return offset;
}


#endif



#ifdef STANDALONE

/* one more prototype */
int main(int argc, char *argv[]);


#define IN_FILENAME "in.bin"
#define OUT_BYTES_COUNT 14


int
main(int argc, char *argv[]) 
{
  const char *program=rasqal_basename(argv[0]);
#define TEST_ITEMS_COUNT 9
  int i;

  for(i=0; i<4; i++) {
    rasqal_rowsource *rowsource;
    size_t count;

    /* for _from_file */
    FILE *handle=NULL;
    /* for _from_string */
    void *string;
    size_t string_len;

    switch(i) {
      case 0:
#ifdef RASQAL_DEBUG
        fprintf(stderr, "%s: Creating rowsource from afilename '%s'\n", program, OUT_FILENAME);
#endif
        rowsource=rasqal_new_rowsource_from_filename((const char*)IN_FILENAME);
        if(!rowsource) {
          fprintf(stderr, "%s: Failed to create rowsource to filename '%s'\n",
                  program, OUT_FILENAME);
          exit(1);
        }
        break;

      case 1:
#ifdef RASQAL_DEBUG
        fprintf(stderr, "%s: Creating rowsource from file handle\n", program);
#endif
        handle=fopen((const char*)OUT_FILENAME, "wb");
        rowsource=rasqal_new_rowsource_from_file_handle(handle);
        if(!rowsource) {
          fprintf(stderr, "%s: Failed to create rowsource from a file handle\n", program);
          exit(1);
        }
        break;

      case 2:
#ifdef RASQAL_DEBUG
        fprintf(stderr, "%s: Creating rowsource from a string\n", program);
#endif
        rowsource=rasqal_new_rowsource_from_string(&string, &string_len, NULL);
        if(!rowsource) {
          fprintf(stderr, "%s: Failed to create rowsource from a string\n", program);
          exit(1);
        }
        break;

      case 3:
#ifdef RASQAL_DEBUG
        fprintf(stderr, "%s: Creating rowsource from a sink\n", program);
#endif
        rowsource=rasqal_new_rowsource_from_sink();
        if(!rowsource) {
          fprintf(stderr, "%s: Failed to create rowsource from a sink\n", program);
          exit(1);
        }
        break;

      default:
        fprintf(stderr, "%s: Unknown test case %d init\n", program, i);
        exit(1);
    }
    

    count=rasqal_rowsource_get_rows_count(rowsource);
    if(count != OUT_BYTES_COUNT) {
      fprintf(stderr, "%s: I/O stream wrote %d bytes, expected %d\n", program,
              (int)count, (int)OUT_BYTES_COUNT);
      return 1;
    }
    
#ifdef RASQAL_DEBUG
    fprintf(stderr, "%s: Freeing rowsource\n", program);
#endif
    rasqal_free_rowsource(rowsource);

    switch(i) {
      case 0:
        remove(OUT_FILENAME);
        break;

      case 1:
        fclose(handle);
        remove(OUT_FILENAME);
        break;

      case 2:
        if(!string) {
          fprintf(stderr, "%s: I/O stream failed to create a string\n", program);
          return 1;
        }
        if(string_len != count) {
          fprintf(stderr, "%s: I/O stream created a string length %d, expected %d\n", program, (int)string_len, (int)count);
          return 1;
        }
        rasqal_free_memory(string);
        break;

      case 3:
        break;

      default:
        fprintf(stderr, "%s: Unknown test case %d tidy\n", program, i);
        exit(1);
    }
    
  }
  
  /* keep gcc -Wall happy */
  return(0);
}

#endif
