/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_sequence.c - Raptor sequence support
 *
 * Copyright (C) 2003-2008, David Beckett http://www.dajobe.org/
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


#include "raptor.h"
#include "raptor_internal.h"


#ifndef STANDALONE

/*
 * Sequence of maximum capacity C containing N data items
 *
 * array:
 *    0            <-- N consecutive items -->         C - 1
 * -----------------------------------------------------------
 * |      |      | data1 |  .....     data N |  ...  |       |
 * -----------------------------------------------------------
 * ------ O -----> offset of first data item
 *
 * start    = O
 * size     = N
 * capacity = C
 *
 */
struct raptor_sequence_s {
  /* how many items are in the sequence 0..capacity */
  int size;

  /* length of the 'sequence' array below */
  int capacity;

  /* offset of the first data item in the sequence: 0..capacity-1 */
  int start;

  /* array of size 'capacity' pointing to the data */
  void **sequence;

  /* handler to call to free a data item (or NULL) */
  raptor_sequence_free_handler *free_handler;

  /* handler to call to print a data item (or NULL) */
  raptor_sequence_print_handler *print_handler;
};


static int raptor_sequence_ensure(raptor_sequence *seq, int capacity, int grow_at_front);

/**
 * raptor_new_sequence:
 * @free_handler: handler to free a sequence item
 * @print_handler: handler to print a sequence item to a FILE*
 * 
 * Constructor - create a new sequence with the given handlers.
 * 
 * Return value: a new #raptor_sequence or NULL on failure 
 **/
raptor_sequence*
raptor_new_sequence(raptor_sequence_free_handler *free_handler,
                    raptor_sequence_print_handler *print_handler)
{
  raptor_sequence* seq=(raptor_sequence*)RAPTOR_CALLOC(raptor_sequence, 1, sizeof(raptor_sequence));
  if(!seq)
    return NULL;
  seq->free_handler=free_handler;
  seq->print_handler=print_handler;
  
  return seq;
}


/**
 * raptor_free_sequence:
 * @seq: sequence to destroy
 * 
 * Destructor - free a #raptor_sequence
 **/
void
raptor_free_sequence(raptor_sequence* seq)
{
  int i;
  int j;

  RAPTOR_ASSERT_OBJECT_POINTER_RETURN(seq, raptor_sequence);

  if(seq->free_handler)
    for(i=seq->start, j=seq->start+seq->size; i<j; i++)
      if(seq->sequence[i])
        seq->free_handler(seq->sequence[i]);

  if(seq->sequence)
    RAPTOR_FREE(ptrarray, seq->sequence);

  RAPTOR_FREE(raptor_sequence, seq);
}


static int
raptor_sequence_ensure(raptor_sequence *seq, int capacity, int grow_at_front)
{
  void **new_sequence;
  int offset;

  RAPTOR_ASSERT_OBJECT_POINTER_RETURN_VALUE(seq, raptor_sequence, 1);

  if(capacity && seq->capacity >= capacity)
    return 0;

  /* POLICY - minimum size */
  if(capacity < 8)
    capacity=8;

  new_sequence=(void**)RAPTOR_CALLOC(ptrarray, capacity, sizeof(void*));
  if(!new_sequence)
    return 1;

  offset=(grow_at_front ? (capacity-seq->capacity) : 0)+seq->start;
  if(seq->size) {
    memcpy(&new_sequence[offset], &seq->sequence[seq->start], sizeof(void*)*seq->size);
    RAPTOR_FREE(ptrarray, seq->sequence);
  }
  seq->start=offset;

  seq->sequence=new_sequence;
  seq->capacity=capacity;
  return 0;
}


/**
 * raptor_sequence_size:
 * @seq: sequence object
 * 
 * Get the number of items in a sequence.
 * 
 * Return value: the sequence size (>=0)
 **/
int
raptor_sequence_size(raptor_sequence* seq)
{
  RAPTOR_ASSERT_OBJECT_POINTER_RETURN_VALUE(seq, raptor_sequence, -1);

  return seq->size;
}


/* Store methods */

/**
 * raptor_sequence_set_at:
 * @seq: sequence object
 * @idx: index into sequence to operate at
 * @data: new data item.
 * 
 * Replace/set an item in a sequence.
 * 
 * The item at the offset @idx in the sequence is replaced with the
 * new item @data (which may be NULL). Any existing item is freed
 * with the sequence's free_handler.  If necessary the sequence
 * is extended (with NULLs) to handle a larger offset.
 *
 * The sequence takes ownership of the new data item.  On failure, the
 * item is freed immediately.
 *
 * Return value: non-0 on failure
 **/
int
raptor_sequence_set_at(raptor_sequence* seq, int idx, void *data)
{
  int need_capacity;
  
  RAPTOR_ASSERT_OBJECT_POINTER_RETURN_VALUE(seq, raptor_sequence, 1);

  /* Cannot provide a negative index */
  if(idx < 0) {
    if(seq->free_handler && data)
      seq->free_handler(data);
    return 1;
  }
  
  need_capacity=seq->start+idx+1;
  if(need_capacity > seq->capacity) {
    if(seq->capacity*2 > need_capacity)
      need_capacity = seq->capacity*2;
    if(raptor_sequence_ensure(seq, need_capacity, 0)) {
      if(seq->free_handler && data)
        seq->free_handler(data);
      return 1;
    }
  }

  if(seq->sequence[seq->start+idx]) {
    /* if there is old data, delete it if there is a free handler */
    if(seq->free_handler)
      seq->free_handler(seq->sequence[seq->start+idx]);
    /*  size remains the same */
  } else
    /* if there is no old data, size is increasing */
    seq->size++;
  
  seq->sequence[seq->start+idx]=data;
  return 0;
}



/**
 * raptor_sequence_push:
 * @seq: sequence to add to
 * @data: item to add
 * 
 * Add an item to the end of the sequence.
 *
 * The sequence takes ownership of the pushed item and frees it with the
 * free_handler. On failure, the item is freed immediately.
 *
 * Return value: non-0 on failure
 **/
int
raptor_sequence_push(raptor_sequence* seq, void *data)
{
  RAPTOR_ASSERT_OBJECT_POINTER_RETURN_VALUE(seq, raptor_sequence, 1);

  if(seq->start+seq->size == seq->capacity) {
    if(raptor_sequence_ensure(seq, seq->capacity*2, 0)) {
      if(seq->free_handler && data)
        seq->free_handler(data);
      return 1;
    }
  }

  seq->sequence[seq->start+seq->size]=data;
  seq->size++;
  return 0;
}


/**
 * raptor_sequence_shift:
 * @seq: sequence to add to
 * @data: item to add
 * 
 * Add an item to the start of the sequence.
 *
 * The sequence takes ownership of the shifted item and frees it with the
 * free_handler. On failure, the item is freed immediately.
 *
 * Return value: non-0 on failure
 **/
int
raptor_sequence_shift(raptor_sequence* seq, void *data)
{
  RAPTOR_ASSERT_OBJECT_POINTER_RETURN_VALUE(seq, raptor_sequence, 1);

  if(!seq->start) {
    if(raptor_sequence_ensure(seq, seq->capacity*2, 1)) {
      if(seq->free_handler && data)
        seq->free_handler(data);
      return 1;
    }
  }
  
  seq->sequence[--seq->start]=data;
  seq->size++;
  return 0;
}


/**
 * raptor_sequence_get_at:
 * @seq: sequence to use
 * @idx: index of item to get
 * 
 * Retrieve an item at offset @index in the sequence.
 *
 * This is efficient to perform. #raptor_sequence is optimised
 * to append/remove from the end of the sequence.
 *
 * After this call the item is still owned by the sequence.
 *
 * Return value: the object or NULL if @index is out of range (0... sequence size-1)
 **/
void*
raptor_sequence_get_at(raptor_sequence* seq, int idx)
{
  RAPTOR_ASSERT_OBJECT_POINTER_RETURN_VALUE(seq, raptor_sequence, NULL);

  if(idx < 0 || idx > seq->size-1)
    return NULL;
  return seq->sequence[seq->start+idx];
}


/**
 * raptor_sequence_delete_at:
 * @seq: sequence object
 * @idx: index into sequence to operate at
 * 
 * Remove an item from a position a sequence, returning it
 * 
 * The item at the offset @idx in the sequence is replaced with a
 * NULL pointer and any existing item is returned.  The caller
 * owns the resulting item.
 *
 * Return value: NULL on failure
 **/
void*
raptor_sequence_delete_at(raptor_sequence* seq, int idx)
{
  void* data;
  
  RAPTOR_ASSERT_OBJECT_POINTER_RETURN_VALUE(seq, raptor_sequence, NULL);

  if(idx < 0 || idx > seq->size-1)
    return NULL;

  data=seq->sequence[seq->start+idx];
  seq->sequence[seq->start+idx]=NULL;
  
  return data;
}



/**
 * raptor_sequence_pop:
 * @seq: sequence to use
 * 
 * Retrieve the item at the end of the sequence.
 * 
 * Ownership of the item is transferred to the caller,
 * i.e. caller is responsible of freeing the item.
 *
 * Return value: the object or NULL if the sequence is empty
 **/
void*
raptor_sequence_pop(raptor_sequence* seq)
{
  void *data;
  int i;

  RAPTOR_ASSERT_OBJECT_POINTER_RETURN_VALUE(seq, raptor_sequence, NULL);

  if(!seq->size)
    return NULL;

  seq->size--;
  i=seq->start+seq->size;
  data=seq->sequence[i];
  seq->sequence[i]=NULL;

  return data;
}


/**
 * raptor_sequence_unshift:
 * @seq: sequence to use
 * 
 * Retrieve the item at the start of the sequence.
 *
 * Ownership of the item is transferred to the caller,
 * i.e. caller is responsible of freeing the item.
 *
 * Return value: the object or NULL if the sequence is empty
 **/
void*
raptor_sequence_unshift(raptor_sequence* seq)
{
  void *data;
  int i;

  RAPTOR_ASSERT_OBJECT_POINTER_RETURN_VALUE(seq, raptor_sequence, NULL);

  if(!seq->size)
    return NULL;
  
  i=seq->start++;
  data=seq->sequence[i];
  seq->size--;
  seq->sequence[i]=NULL;
  
  return data;
}


/**
 * raptor_compare_strings:
 * @a: pointer first string
 * @b: pointer to second string
 * 
 * Utility function for raptor_sequence_sort() to compare a sequence of strings.
 *
 * Return value: comparison of @a to @b as strings
 **/
int
raptor_compare_strings(const void *a, const void *b) 
{
  return strcmp(*(char**)a, *(char**)b);
}



/**
 * raptor_sequence_sort:
 * @seq: sequence to sort
 * @compare: comparison function
 * 
 * The comparison function is compatible with that used for qsort()
 * and provides the addresses of pointers to the data that
 * must be dereferenced to get to the stored sequence data.
 * 
 **/
void
raptor_sequence_sort(raptor_sequence* seq, 
                     int(*compare)(const void *, const void *))
{
  RAPTOR_ASSERT_OBJECT_POINTER_RETURN(seq, raptor_sequence);

  if(seq->size > 1)
    qsort(&seq->sequence[seq->start], seq->size, sizeof(void*), compare);
}



/**
 * raptor_sequence_print_string:
 * @data: data item (a char*)
 * @fh: file handle to print to
 *
 * Helper function for printing a sequence of strings.
 *
 * Intended for use as a #raptor_sequence_print_handler passed into
 * raptor_new_sequence().
 */
void
raptor_sequence_print_string(char *data, FILE *fh) 
{
  fputs(data, fh);
}


/**
 * raptor_sequence_print_uri:
 * @data: data item (a #raptor_uri)
 * @fh: file handle to print to
 *
 * Helper function for printing a sequence of URIs.
 *
 * Intended for use as a #raptor_sequence_print_handler passed into
 * raptor_new_sequence().
 */
void
raptor_sequence_print_uri(char *data, FILE *fh) 
{
  raptor_uri* uri=(raptor_uri*)data;
  fputs((const char*)raptor_uri_as_string(uri), fh);
}


/**
 * raptor_sequence_set_print_handler:
 * @seq: sequence
 * @print_handler: print handler
 *
 * Set the print handler for the sequence.
 *
 * This is set in the raptor_new_sequence() constructor and can be
 * overridden here.
 */
void
raptor_sequence_set_print_handler(raptor_sequence *seq,
                                  raptor_sequence_print_handler *print_handler) {
  if(!seq)
    return;
  seq->print_handler=print_handler;
}


/**
 * raptor_sequence_print:
 * @seq: sequence to sort
 * @fh: file handle
 *
 * Print the sequence contents using the print_handler to print the data items.
 */
void
raptor_sequence_print(raptor_sequence* seq, FILE* fh)
{
  int i;

  RAPTOR_ASSERT_OBJECT_POINTER_RETURN(seq, raptor_sequence);

  fputc('[', fh);
  for(i=0; i<seq->size; i++) {
    if(i)
      fputs(", ", fh);
    if(seq->sequence[seq->start+i])
      seq->print_handler(seq->sequence[seq->start+i], fh);
    else
      fputs("(empty)", fh);
  }
  fputc(']', fh);
}


/**
 * raptor_sequence_join:
 * @dest: #raptor_sequence destination sequence
 * @src: #raptor_sequence source sequence
 *
 * Join two sequences moving all items from one sequence to the end of another.
 *
 * After this operation, sequence src will be empty (zero size) but
 * will have the same item capacity as before.
 *
 * Return value: non-0 on failure
 */
int
raptor_sequence_join(raptor_sequence* dest, raptor_sequence *src)
{
  RAPTOR_ASSERT_OBJECT_POINTER_RETURN_VALUE(dest, raptor_sequence, 1);
  RAPTOR_ASSERT_OBJECT_POINTER_RETURN_VALUE(src, raptor_sequence, 1);

  if(raptor_sequence_ensure(dest, dest->size + src->size, 0))
    return 1;
  memcpy(&dest->sequence[dest->start+dest->size], &src->sequence[src->start], sizeof(void*)*src->size);
  dest->size += src->size;

  src->size=0;

  return 0;
}


#endif



#ifdef STANDALONE
#include <stdio.h>

int main(int argc, char *argv[]);


#define assert_match_string(function, expr, string) do { char *result=expr; if(strcmp(result, string)) { fprintf(stderr, "%s:" #function " failed - returned %s, expected %s\n", program, result, string); exit(1); } } while(0)
#define assert_match_int(function, expr, value) do { int result=expr; if(result != value) { fprintf(stderr, "%s:" #function " failed - returned %d, expected %d\n", program, result, value); exit(1); } } while(0)

int
main(int argc, char *argv[]) 
{
  const char *program=raptor_basename(argv[0]);
  raptor_sequence* seq1=raptor_new_sequence(NULL, (raptor_sequence_print_handler*)raptor_sequence_print_string);
  raptor_sequence* seq2=raptor_new_sequence(NULL, (raptor_sequence_print_handler*)raptor_sequence_print_string);
  char *s;
  int i;

  if(raptor_sequence_pop(seq1) || raptor_sequence_unshift(seq1)) {
    fprintf(stderr, "%s: should not be able to pop/unshift from an empty sequence\n", program);
    exit(1);
  }

  raptor_sequence_set_at(seq1, 0, (void*)"first");

  raptor_sequence_push(seq1, (void*)"third");

  raptor_sequence_shift(seq1, (void*)"second");

  s=(char*)raptor_sequence_get_at(seq1, 0);
  assert_match_string(raptor_sequence_get_at, s, "second");

  s=(char*)raptor_sequence_get_at(seq1, 1);
  assert_match_string(raptor_sequence_get_at, s, "first");
  
  s=(char*)raptor_sequence_get_at(seq1, 2);
  assert_match_string(raptor_sequence_get_at, s, "third");
  
  assert_match_int(raptor_sequence_size, raptor_sequence_size(seq1), 3);

  fprintf(stderr, "%s: sequence after additions: ", program);
  raptor_sequence_print(seq1, stderr);
  fputc('\n', stderr);

  /* now made alphabetical i.e. first, second, third */
  raptor_sequence_sort(seq1, raptor_compare_strings);

  fprintf(stderr, "%s: sequence after sort: ", program);
  raptor_sequence_print(seq1, stderr);
  fputc('\n', stderr);

  s=(char*)raptor_sequence_pop(seq1);
  assert_match_string(raptor_sequence_get_at, s, "third");

  assert_match_int(raptor_sequence_size, raptor_sequence_size(seq1), 2);

  fprintf(stderr, "%s: sequence after pop: ", program);
  raptor_sequence_print(seq1, stderr);
  fputc('\n', stderr);

  s=(char*)raptor_sequence_unshift(seq1);
  assert_match_string(raptor_sequence_get_at, s, "first");

  assert_match_int(raptor_sequence_size, raptor_sequence_size(seq1), 1);

  fprintf(stderr, "%s: sequence after unshift: ", program);
  raptor_sequence_print(seq1, stderr);
  fputc('\n', stderr);

  s=(char*)raptor_sequence_get_at(seq1, 0);
  assert_match_string(raptor_sequence_get_at, s, "second");
  
  raptor_sequence_push(seq2, (void*)"first.2");
  if(raptor_sequence_join(seq2, seq1)) {
    fprintf(stderr, "%s: raptor_sequence_join failed\n", program);
    exit(1);
  }

  assert_match_int(raptor_sequence_size, raptor_sequence_size(seq1), 0);
  assert_match_int(raptor_sequence_size, raptor_sequence_size(seq2), 2);

  raptor_free_sequence(seq1);
  raptor_free_sequence(seq2);

  /* test sequence growing */
  
  seq1=raptor_new_sequence(NULL, (raptor_sequence_print_handler*)raptor_sequence_print_string);
  for(i=0; i<100; i++)
    if(raptor_sequence_shift(seq1, (void*)"foo")) {
      fprintf(stderr, "%s: raptor_sequence_shift failed\n", program);
      exit(1);
    }
  assert_match_int(raptor_sequence_size, raptor_sequence_size(seq1), 100);
  for(i=0; i<100; i++)
    raptor_sequence_unshift(seq1);
  assert_match_int(raptor_sequence_size, raptor_sequence_size(seq1), 0);
  raptor_free_sequence(seq1);

  seq1=raptor_new_sequence(NULL, (raptor_sequence_print_handler*)raptor_sequence_print_string);
  for(i=0; i<100; i++)
    if(raptor_sequence_push(seq1, (void*)"foo")) {
      fprintf(stderr, "%s: raptor_sequence_push failed\n", program);
      exit(1);
    }
  assert_match_int(raptor_sequence_size, raptor_sequence_size(seq1), 100);
  for(i=0; i<100; i++)
    raptor_sequence_pop(seq1);
  assert_match_int(raptor_sequence_size, raptor_sequence_size(seq1), 0);
  raptor_free_sequence(seq1);

  return (0);
}
#endif
