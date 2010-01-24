/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_stringbuffer.c - Stringbuffer class for growing strings
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
#include <stdarg.h>
#include <sys/types.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h> /* for abort() as used in errors */
#endif

/* Raptor includes */
#include "raptor.h"
#include "raptor_internal.h"


#ifndef STANDALONE

struct raptor_stringbuffer_node_s
{
  struct raptor_stringbuffer_node_s* next;
  unsigned char *string;
  size_t length;
};
typedef struct raptor_stringbuffer_node_s raptor_stringbuffer_node;


struct raptor_stringbuffer_s
{
  /* Pointing to the first item in the list of nodes */
  raptor_stringbuffer_node* head;
  /* and the last */
  raptor_stringbuffer_node* tail;
  
  /* total length of the string */
  size_t length;

  /* frozen string if already calculated, or NULL if not present */
  unsigned char *string;
};


/* prototypes for local functions */
static int raptor_stringbuffer_append_string_common(raptor_stringbuffer* stringbuffer, const unsigned char *string, size_t length, int do_copy);


/* functions implementing the stringbuffer api */

/**
 * raptor_new_stringbuffer:
 *
 * Create a new stringbuffer.
 * 
 * Return value: pointer to a raptor_stringbuffer object or NULL on failure
 **/
raptor_stringbuffer*
raptor_new_stringbuffer(void) 
{
  return (raptor_stringbuffer*)RAPTOR_CALLOC(raptor_stringbuffer, 1, sizeof(raptor_stringbuffer));
}


/**
 * raptor_free_stringbuffer:
 * @stringbuffer: stringbuffer object to destroy.
 *
 * Destroy a stringbuffer.
 * 
 **/
void
raptor_free_stringbuffer(raptor_stringbuffer *stringbuffer) 
{
  RAPTOR_ASSERT_OBJECT_POINTER_RETURN(stringbuffer, raptor_stringbuffer);

  if(stringbuffer->head) {
    raptor_stringbuffer_node *node=stringbuffer->head;
  
    while(node) {
      raptor_stringbuffer_node *next=node->next;
      
      if(node->string)
        RAPTOR_FREE(cstring, node->string);
      RAPTOR_FREE(raptor_stringbuffer_node, node);
      node=next;
    }
  }

  if(stringbuffer->string)
    RAPTOR_FREE(cstring, stringbuffer->string);

  RAPTOR_FREE(raptor_stringbuffer, stringbuffer);
}



/**
 * raptor_stringbuffer_append_string_common:
 * @stringbuffer: raptor stringbuffer
 * @string: string
 * @length: length of string
 * @do_copy: non-0 to copy the string
 *
 * Add a string to the stringbuffer.
 *
 * INTERNAL
 *
 * If @string is NULL or @length is 0, no work is performed.
 *
 * If @do_copy is non-0, the passed-in string is copied into new memory
 * otherwise the stringbuffer becomes the owner of the string pointer
 * and will free it when the stringbuffer is destroyed.
 *
 * Return value: non-0 on failure
 **/
static int
raptor_stringbuffer_append_string_common(raptor_stringbuffer* stringbuffer, 
                                         const unsigned char *string,
                                         size_t length,
                                         int do_copy)
{
  raptor_stringbuffer_node *node;

  if(!string || !length)
    return 0;
  
  node=(raptor_stringbuffer_node*)RAPTOR_MALLOC(raptor_stringbuffer_node, sizeof(raptor_stringbuffer_node));
  if(!node) {
    if(!do_copy)
      RAPTOR_FREE(cstring, string);
    return 1;
  }

  if(do_copy) {
    /* Note this copy does not include the \0 character - not needed  */
    node->string=(unsigned char*)RAPTOR_MALLOC(bytes, length);
    if(!node->string) {
      RAPTOR_FREE(raptor_stringbuffer_node, node);
      return 1;
    }
    strncpy((char*)node->string, (const char*)string, length);
  } else
    node->string=(unsigned char*)string;
  node->length=length;


  if(stringbuffer->tail) {
    stringbuffer->tail->next=node;
    stringbuffer->tail=node;
  } else
    stringbuffer->head=stringbuffer->tail=node;
  node->next=NULL;

  if(stringbuffer->string) {
    RAPTOR_FREE(cstring, stringbuffer->string);
    stringbuffer->string=NULL;
  }
  stringbuffer->length += length;

  return 0;
}




/**
 * raptor_stringbuffer_append_counted_string:
 * @stringbuffer: raptor stringbuffer
 * @string: string
 * @length: length of string
 * @do_copy: non-0 to copy the string
 *
 * If @string is NULL or @length is 0, no work is performed.
 *
 * If @do_copy is non-0, the passed-in string is copied into new memory
 * otherwise the stringbuffer becomes the owner of the string pointer
 * and will free it when the stringbuffer is destroyed.
 *
 * Add a string to the stringbuffer.
 *
 * Return value: non-0 on failure
 **/
int
raptor_stringbuffer_append_counted_string(raptor_stringbuffer* stringbuffer, 
                                          const unsigned char *string, size_t length,
                                          int do_copy)
{
  if(!string || !length)
    return 0;
  
  return raptor_stringbuffer_append_string_common(stringbuffer, string, length, do_copy);
}
  

/**
 * raptor_stringbuffer_append_string:
 * @stringbuffer: raptor stringbuffer
 * @string: string
 * @do_copy: non-0 to copy the string
 *
 * Add a string to the stringbuffer.
 * 
 * If @string is NULL, no work is performed.
 *
 * If @do_copy is non-0, the passed-in string is copied into new memory
 * otherwise the stringbuffer becomes the owner of the string pointer
 * and will free it when the stringbuffer is destroyed.
 *
 * Return value: non-0 on failure
 **/
int
raptor_stringbuffer_append_string(raptor_stringbuffer* stringbuffer, 
                                  const unsigned char *string, int do_copy)
{
  if(!string)
    return 0;
  
  return raptor_stringbuffer_append_string_common(stringbuffer, string, strlen((const char*)string), do_copy);
}


/**
 * raptor_stringbuffer_append_decimal:
 * @stringbuffer: raptor stringbuffer
 * @integer: integer to format as decimal and add
 *
 * Add an integer in decimal to the stringbuffer.
 * 
 * Return value: non-0 on failure
 **/
int
raptor_stringbuffer_append_decimal(raptor_stringbuffer* stringbuffer, 
                                   int integer)
{
  /* enough for 64 bit signed integer
   * INT64_MAX is  9223372036854775807 (19 digits) + 1 for sign 
   */
  unsigned char buf[20];
  unsigned char *p;
  int i=integer;
  size_t length=1;
  if(integer<0) {
    length++;
    i= -integer;
  }
  while(i/=10)
    length++;

  p=buf+length-1;
  i=integer;
  if(i<0)
    i= -i;
  do {
    *p-- ='0'+(i %10);
    i /= 10;
  } while(i);
  if(integer<0)
    *p= '-';
  
  return raptor_stringbuffer_append_counted_string(stringbuffer, buf, length, 1);
}


/**
 * raptor_stringbuffer_append_stringbuffer:
 * @stringbuffer: #raptor_stringbuffer
 * @append: #raptor_stringbuffer to append
 *
 * Add a stringbuffer to the stringbuffer.
 *
 * This function removes the content from the appending stringbuffer,
 * making it empty and appends it to the supplied stringbuffer.
 *
 * Return value: non-0 on failure
 **/
int
raptor_stringbuffer_append_stringbuffer(raptor_stringbuffer* stringbuffer, 
                                        raptor_stringbuffer* append)
{
  raptor_stringbuffer_node *node=append->head;

  if(!node)
    return 0;

  /* move all append nodes to stringbuffer */
  if(stringbuffer->tail) {
    stringbuffer->tail->next=node;
  } else
    stringbuffer->head=node;

  stringbuffer->tail=append->tail;

  /* adjust our length */
  stringbuffer->length += append->length;
  if(stringbuffer->string) {
    RAPTOR_FREE(cstring, stringbuffer->string);
    stringbuffer->string=NULL;
  }

  /* zap append content */
  append->head=append->tail=NULL;
  append->length=0;
  if(append->string) {
    RAPTOR_FREE(cstring, append->string);
    append->string=NULL;
  }
  
  return 0;
}




/**
 * raptor_stringbuffer_prepend_string_common:
 * @stringbuffer: raptor stringbuffer
 * @string: string
 * @length: length of string
 * @do_copy: non-0 to copy the string
 *
 * Add a string to the start of a stringbuffer.
 *
 * INTERNAL
 *
 * If do_copy is non-0, the passed-in string is copied into new memory
 * otherwise the stringbuffer becomes the owner of the string pointer
 * and will free it when the stringbuffer is destroyed.
 *
 * Return value: non-0 on failure
 **/
static int
raptor_stringbuffer_prepend_string_common(raptor_stringbuffer* stringbuffer, 
                                          const unsigned char *string, size_t length,
                                          int do_copy)
{
  raptor_stringbuffer_node *node;

  node=(raptor_stringbuffer_node*)RAPTOR_MALLOC(raptor_stringbuffer_node, sizeof(raptor_stringbuffer_node));
  if(!node)
    return 1;

  if(do_copy) {
    /* Note this copy does not include the \0 character - not needed  */
    node->string=(unsigned char*)RAPTOR_MALLOC(bytes, length);
    if(!node->string) {
      RAPTOR_FREE(raptor_stringbuffer_node, node);
      return 1;
    }
    strncpy((char*)node->string, (const char*)string, length);
  } else
    node->string=(unsigned char*)string;
  node->length=length;


  node->next=stringbuffer->head;
  if(stringbuffer->head)
    stringbuffer->head=node;
  else
    stringbuffer->head=stringbuffer->tail=node;

  if(stringbuffer->string) {
    RAPTOR_FREE(cstring, stringbuffer->string);
    stringbuffer->string=NULL;
  }
  stringbuffer->length += length;

  return 0;
}




/**
 * raptor_stringbuffer_prepend_counted_string:
 * @stringbuffer: raptor stringbuffer
 * @string: string
 * @length: length of string
 * @do_copy: non-0 to copy the string

 * If do_copy is non-0, the passed-in string is copied into new memory
 * otherwise the stringbuffer becomes the owner of the string pointer
 * and will free it when the stringbuffer is destroyed.
 *
 * Add a string to the start of the stringbuffer.
 *
 * Return value: non-0 on failure
 **/
int
raptor_stringbuffer_prepend_counted_string(raptor_stringbuffer* stringbuffer, 
                                           const unsigned char *string, size_t length,
                                           int do_copy)
{
  return raptor_stringbuffer_prepend_string_common(stringbuffer, string, length, do_copy);
}
  

/**
 * raptor_stringbuffer_prepend_string:
 * @stringbuffer: raptor stringbuffer
 * @string: string
 * @do_copy: non-0 to copy the string
 *
 * Add a string to the start of the stringbuffer.
 * 
 * If do_copy is non-0, the passed-in string is copied into new memory
 * otherwise the stringbuffer becomes the owner of the string pointer
 * and will free it when the stringbuffer is destroyed.
 *
 * Return value: non-0 on failure
 **/
int
raptor_stringbuffer_prepend_string(raptor_stringbuffer* stringbuffer, 
                                   const unsigned char *string, int do_copy)
{
  return raptor_stringbuffer_prepend_string_common(stringbuffer, string, strlen((const char*)string), do_copy);
}


/**
 * raptor_stringbuffer_length:
 * @stringbuffer: raptor stringbuffer
 *
 * Return the stringbuffer length.
 * 
 * Return value: size of stringbuffer
 **/
size_t
raptor_stringbuffer_length(raptor_stringbuffer* stringbuffer)
{
  return stringbuffer->length;
}



/**
 * raptor_stringbuffer_as_string:
 * @stringbuffer: raptor stringbuffer
 *
 * Return the stringbuffer as a C string.
 * 
 * Note: the return value is a to a shared string that the stringbuffer
 * allocates and manages.
 *
 * Return value: NULL on failure or stringbuffer is empty, otherwise
 *   a pointer to a shared copy of the string.
 **/
unsigned char *
raptor_stringbuffer_as_string(raptor_stringbuffer* stringbuffer)
{
  raptor_stringbuffer_node *node;
  unsigned char *p;
  
  if(!stringbuffer->length)
    return NULL;
  if(stringbuffer->string)
    return stringbuffer->string;

  stringbuffer->string=(unsigned char*)RAPTOR_MALLOC(cstring, stringbuffer->length+1);
  if(!stringbuffer->string)
    return NULL;

  node=stringbuffer->head;
  p=stringbuffer->string;
  while(node) {
    strncpy((char*)p, (const char*)node->string, node->length);
    p+= node->length;
    node=node->next;
  }
  *p='\0';
  return stringbuffer->string;
}


/**
 * raptor_stringbuffer_copy_to_string:
 * @stringbuffer: raptor stringbuffer
 * @string: output string
 * @length: size of output string
 *
 * Copy the stringbuffer into a string.
 * 
 * Copies the underlying string to a pre-allocated buffer.  The
 * output string is always '\0' terminated.
 *
 * Return value: non-0 on failure such as stringbuffer is empty, buffer is too small
 **/
int
raptor_stringbuffer_copy_to_string(raptor_stringbuffer* stringbuffer,
                                   unsigned char *string, size_t length)
{
  raptor_stringbuffer_node *node;
  unsigned char *p;
  
  if(!string || length < 1)
    return 1;

  if(!stringbuffer->length)
    return 0;

  p=string;
  for(node=stringbuffer->head; node; node=node->next) {
    if(node->length > length) {
      p[-1]='\0';
      return 1;
    }
    strncpy((char*)p, (const char*)node->string, node->length);
    p+= node->length;
    length-= node->length;
  }
  *p='\0';
  return 0;
}

#endif



#ifdef STANDALONE

/* one more prototype */
int main(int argc, char *argv[]);


int
main(int argc, char *argv[]) 
{
  const char *program=raptor_basename(argv[0]);
#define TEST_ITEMS_COUNT 9
  const char *items[TEST_ITEMS_COUNT] = { "the", "quick" ,"brown", "fox", "jumps", "over", "the", "lazy", "dog" };
  const char *items_string = "thequickbrownfoxjumpsoverthelazydog";  
  const size_t items_len=35;
  const char *test_integer_string = "abcd";
#define TEST_INTEGERS_COUNT 7
  const int test_integers[TEST_INTEGERS_COUNT]={ 0, 1, -1, 11, 1234, 12345, -12345 };
  const char *test_integer_results[TEST_INTEGERS_COUNT]={ "abcd0", "abcd1", "abcd-1", "abcd11", "abcd1234", "abcd12345", "abcd-12345" };
  raptor_stringbuffer *sb;
  unsigned char *str;
  size_t len;
  int i=0;
  raptor_stringbuffer *sb1, *sb2;
#define TEST_APPEND_COUNT 2
  const char *test_append_results[TEST_APPEND_COUNT]={ "thebrownjumpsthedog", "quickfoxoverlazy" };
  const char *test_append_results_total="thebrownjumpsthedogquickfoxoverlazy";
#define COPY_STRING_BUFFER_SIZE 100
  unsigned char *copy_string;
  
#ifdef RAPTOR_DEBUG
  fprintf(stderr, "%s: Creating string buffer\n", program);
#endif

  /* test appending */

  sb=raptor_new_stringbuffer();
  if(!sb) {
    fprintf(stderr, "%s: Failed to create string buffer\n", program);
    exit(1);
  }

  for(i=0; i<TEST_ITEMS_COUNT; i++) {
    int rc;
    len=strlen(items[i]);

#ifdef RAPTOR_DEBUG
    fprintf(stderr, "%s: Adding string buffer item '%s'\n", program, items[i]);
#endif
  
    rc=raptor_stringbuffer_append_counted_string(sb, (unsigned char*)items[i], len, 1);
    if(rc) {
      fprintf(stderr, "%s: Adding string buffer item %d '%s' failed, returning error %d\n",
              program, i, items[i], rc);
      exit(1);
    }
  }

  len=raptor_stringbuffer_length(sb);
  if(len != items_len) {
    fprintf(stderr, "%s: string buffer len is %d, expected %d\n", program,
            (int)len, (int)items_len);
    exit(1);
  }

  str=raptor_stringbuffer_as_string(sb);
  if(strcmp((const char*)str, items_string)) {
    fprintf(stderr, "%s: string buffer contains '%s', expected '%s'\n",
            program, str, items_string);
    exit(1);
  }

  raptor_free_stringbuffer(sb);

  
  /* test prepending */

#ifdef RAPTOR_DEBUG
  fprintf(stderr, "%s: Creating string buffer\n", program);
#endif

  sb=raptor_new_stringbuffer();
  if(!sb) {
    fprintf(stderr, "%s: Failed to create string buffer\n", program);
    exit(1);
  }

  for(i=TEST_ITEMS_COUNT-1; i>=0 ; i--) {
    int rc;
    len=strlen(items[i]);

#ifdef RAPTOR_DEBUG
    fprintf(stderr, "%s: Prepending string buffer item '%s'\n", program, items[i]);
#endif
  
    rc=raptor_stringbuffer_prepend_counted_string(sb, (unsigned char*)items[i], len, 1);
    if(rc) {
      fprintf(stderr, "%s: Prepending string buffer item %d '%s' failed, returning error %d\n",
              program, i, items[i], rc);
      exit(1);
    }
  }

  len=raptor_stringbuffer_length(sb);
  if(len != items_len) {
    fprintf(stderr, "%s: string buffer len is %d, expected %d\n", program,
            (int)len, (int)items_len);
    exit(1);
  }

  str=raptor_stringbuffer_as_string(sb);
  if(strcmp((const char*)str, items_string)) {
    fprintf(stderr, "%s: string buffer contains '%s', expected '%s'\n",
            program, str, items_string);
    exit(1);
  }


  /* test adding integers */

  for(i=0; i<TEST_INTEGERS_COUNT; i++) {
    raptor_stringbuffer *isb=raptor_new_stringbuffer();
    if(!isb) {
      fprintf(stderr, "%s: Failed to create string buffer\n", program);
      exit(1);
    }
    
    raptor_stringbuffer_append_string(isb, 
                                      (const unsigned char*)test_integer_string, 1);

#ifdef RAPTOR_DEBUG
    fprintf(stderr, "%s: Adding decimal integer %d to buffer\n", program, test_integers[i]);
#endif

    raptor_stringbuffer_append_decimal(isb, test_integers[i]);

    str=raptor_stringbuffer_as_string(isb);
    if(strcmp((const char*)str, test_integer_results[i])) {
      fprintf(stderr, "%s: string buffer contains '%s', expected '%s'\n",
              program, str, test_integer_results[i]);
      exit(1);
    }
#ifdef RAPTOR_DEBUG
    fprintf(stderr, "%s: Freeing string buffer\n", program);
#endif
    raptor_free_stringbuffer(isb);
  }


#ifdef RAPTOR_DEBUG
    fprintf(stderr, "%s: Creating two stringbuffers to join\n", program);
#endif

  sb1=raptor_new_stringbuffer();
  if(!sb1) {
    fprintf(stderr, "%s: Failed to create string buffer\n", program);
    exit(1);
  }
  sb2=raptor_new_stringbuffer();
  if(!sb2) {
    fprintf(stderr, "%s: Failed to create string buffer\n", program);
    exit(1);
  }

  for(i=0; i<TEST_ITEMS_COUNT; i++) {
    raptor_stringbuffer *sbx;
    int rc;
    len=strlen(items[i]);

    sbx=(i % 2) ? sb2 : sb1;
    rc=raptor_stringbuffer_append_counted_string(sbx, (unsigned char*)items[i], len, 1);
    if(rc) {
      fprintf(stderr, "%s: Adding string buffer item %d '%s' failed, returning error %d\n",
              program, i, items[i], rc);
      exit(1);
    }
  }

  if(1) {
    int rc;

    rc=raptor_stringbuffer_append_counted_string(sb1, (unsigned char*)"X", 0, 1);
    if(rc) {
      fprintf(stderr, "%s: Adding 0-length counted string failed, returning error %d\n",
              program, rc);
      exit(1);
    }
    rc=raptor_stringbuffer_append_string(sb1, NULL, 1);
    if(rc) {
      fprintf(stderr, "%s: Adding NULL string failed, returning error %d\n",
              program, rc);
      exit(1);
    }
  }
  
  str=raptor_stringbuffer_as_string(sb1);
  if(strcmp((const char*)str, test_append_results[0])) {
    fprintf(stderr, "%s: string buffer sb1 contains '%s', expected '%s'\n",
            program, str, test_append_results[0]);
    exit(1);
  }
  str=raptor_stringbuffer_as_string(sb2);
  if(strcmp((const char*)str, test_append_results[1])) {
    fprintf(stderr, "%s: string buffer sb2 contains '%s', expected '%s'\n",
            program, str, test_append_results[1]);
    exit(1);
  }

#ifdef RAPTOR_DEBUG
    fprintf(stderr, "%s: Appended two stringbuffers\n", program);
#endif

  if(raptor_stringbuffer_append_stringbuffer(sb1, sb2)) {
    fprintf(stderr, "%s: Failed to append string buffer\n", program);
    exit(1);
  }

  str=raptor_stringbuffer_as_string(sb1);
  if(strcmp((const char*)str, test_append_results_total)) {
    fprintf(stderr, "%s: appended string buffer contains '%s', expected '%s'\n",
            program, str, test_append_results_total);
    exit(1);
  }
  
  len=raptor_stringbuffer_length(sb2);
  if(len) {
    fprintf(stderr, "%s: appended string buffer is length %d, not empty'\n",
            program, (int)len);
    exit(1);
  }


#ifdef RAPTOR_DEBUG
  fprintf(stderr, "%s: Copying string buffer to string\n", program);
#endif

  copy_string=(unsigned char*)malloc(COPY_STRING_BUFFER_SIZE);
  if(raptor_stringbuffer_copy_to_string(sb1, copy_string, COPY_STRING_BUFFER_SIZE)) {
    fprintf(stderr, "%s: copying string buffer to string failed\n",
            program);
    exit(1);
  }
  if(strcmp((const char*)copy_string, test_append_results_total)) {
    fprintf(stderr, "%s: copied string buffer contains '%s', expected '%s'\n",
            program, copy_string, test_append_results_total);
    exit(1);
  }
  free(copy_string);
  
  
#ifdef RAPTOR_DEBUG
  fprintf(stderr, "%s: Freeing string buffers\n", program);
#endif
  raptor_free_stringbuffer(sb1);
  raptor_free_stringbuffer(sb2);
  raptor_free_stringbuffer(sb);

  /* keep gcc -Wall happy */
  return(0);
}

#endif
