/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_uri.c - RDF URI interface
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


#ifdef HAVE_CONFIG_H
#include <rdf_config.h>
#endif

#ifdef WIN32
#include <win32_rdf_config.h>
#endif

#include <stdio.h>
#include <string.h>
#include <ctype.h> /* for isalnum */
#ifdef WITH_THREADS
#include <pthread.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h> /* for abort() as used in errors */
#endif

#include <redland.h>


#ifndef STANDALONE

/* class methods */


/**
 * librdf_init_uri:
 * @world: redland world object
 *
 * INTERNAL - Initialise the uri module.
 *
 **/
void
librdf_init_uri(librdf_world *world)
{
  /* If no default given, create an in memory hash */
  if(!world->uris_hash) {
    world->uris_hash=librdf_new_hash(world, NULL);
    if(!world->uris_hash)
      LIBRDF_FATAL1(world, LIBRDF_FROM_URI, "Failed to create URI hash from factory");
    
    /* remember to free it later */
    world->uris_hash_allocated_here=1;

    if(librdf_hash_open(world->uris_hash, NULL, 0, 1, 1, NULL))
      LIBRDF_FATAL1(world, LIBRDF_FROM_URI, "Failed to open URI hash");
  }
}



/**
 * librdf_finish_uri:
 * @world: redland world object
 *
 * INTERNAL - Terminate the uri module.
 *
 **/
void
librdf_finish_uri(librdf_world *world)
{
  if (world->uris_hash) {
    librdf_hash_close(world->uris_hash);

    if(world->uris_hash_allocated_here)
      librdf_free_hash(world->uris_hash);
  }
}



/**
 * librdf_new_uri:
 * @world: redland world object
 * @uri_string: URI in string form
 *
 * Constructor - create a new #librdf_uri object from a URI string.
 * 
 * A new URI is constructed from a copy of the string.  If the
 * string is a NULL pointer or empty (0 length) then the result is NULL.
 *
 * Return value: a new #librdf_uri object or NULL on failure
 **/
librdf_uri*
librdf_new_uri (librdf_world *world, 
                const unsigned char *uri_string)
{
  librdf_uri* new_uri;
  unsigned char *new_string;
  int length;
  librdf_hash_datum key, value; /* on stack - not allocated */
  librdf_hash_datum *old_value;

  librdf_world_open(world);

  if(!uri_string || !*uri_string)
    return NULL;

#ifdef WITH_THREADS
  pthread_mutex_lock(world->mutex);
#endif
  
  length=strlen((const char*)uri_string);

  key.data=(char*)uri_string;
  key.size=length;

  /* if existing URI found in hash, return it */
  if((old_value=librdf_hash_get_one(world->uris_hash, &key))) {
    new_uri=*(librdf_uri**)old_value->data;

#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
    LIBRDF_DEBUG3("Found existing URI %s in hash with current usage %d\n", uri_string, new_uri->usage);
#endif

    librdf_free_hash_datum(old_value);
    new_uri->usage++;

#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
    if(new_uri->usage > new_uri->max_usage)
      new_uri->max_usage=new_uri->usage;
#endif    

    goto unlock;
  }
  

  /* otherwise create a new one */

#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
  LIBRDF_DEBUG2("Creating new URI %s in hash\n", uri_string);
#endif

  new_uri = (librdf_uri*)LIBRDF_CALLOC(librdf_uri, 1, sizeof(librdf_uri));
  if(!new_uri)
    goto unlock;

  new_uri->world=world;
  new_uri->string_length=length;

  new_string=(unsigned char*)LIBRDF_MALLOC(cstring, length+1);
  if(!new_string) {
    LIBRDF_FREE(librdf_uri, new_uri);
    new_uri=NULL;
    goto unlock;
  }
  
  strcpy((char*)new_string, (const char*)uri_string);
  new_uri->string=new_string;

  new_uri->usage=1;
#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
  new_uri->max_usage=1;
#endif

  value.data=&new_uri; value.size=sizeof(librdf_uri*);
  
  /* store in hash: URI-string => (librdf_uri*) */
  if(librdf_hash_put(world->uris_hash, &key, &value)) {
    LIBRDF_FREE(cstring, new_string);
    LIBRDF_FREE(librdf_uri, new_uri);
    new_uri=NULL;
  }

 unlock:
#ifdef WITH_THREADS
  pthread_mutex_unlock(world->mutex);
#endif

  return new_uri;
}


/**
 * librdf_new_uri_from_uri:
 * @old_uri: #librdf_uri object
 *
 * Copy constructor - create a new librdf_uri object from an existing librdf_uri object.
 * 
 * Return value: a new #librdf_uri object or NULL on failure
 **/
librdf_uri*
librdf_new_uri_from_uri (librdf_uri* old_uri) {

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(old_uri, librdf_uri, NULL);
  
  old_uri->usage++;
  return old_uri;
}


/**
 * librdf_new_uri_from_uri_local_name:
 * @old_uri: #librdf_uri object
 * @local_name: local name to append to URI
 *
 * Copy constructor - create a new librdf_uri object from an existing librdf_uri object and a local name.
 * 
 * Return value: a new #librdf_uri object or NULL on failure
 **/
librdf_uri*
librdf_new_uri_from_uri_local_name (librdf_uri* old_uri, 
                                    const unsigned char *local_name) {
  int len;
  unsigned char *new_string;
  librdf_uri* new_uri;

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(old_uri, librdf_uri, NULL);
  
  if(!old_uri)
    return NULL;
  
  len=old_uri->string_length + strlen((const char*)local_name) +1 ; /* +1 for \0 */

  new_string=(unsigned char*)LIBRDF_MALLOC(cstring, len);
  if(!new_string)
    return NULL;

  strcpy((char*)new_string, (const char*)old_uri->string);
  strcat((char*)new_string, (const char*)local_name);

  new_uri=librdf_new_uri (old_uri->world, new_string);
  LIBRDF_FREE(cstring, new_string);

  return new_uri;
}


/**
 * librdf_new_uri_normalised_to_base:
 * @uri_string: URI in string form
 * @source_uri: source URI to remove
 * @base_uri: base URI to add
 *
 * Constructor - create a new #librdf_uri object from a URI string stripped of the source URI, made relative to the base URI.
 * 
 * Return value: a new #librdf_uri object or NULL on failure
 **/
librdf_uri*
librdf_new_uri_normalised_to_base(const unsigned char *uri_string,
                                  librdf_uri* source_uri,
                                  librdf_uri* base_uri) 
{
  int uri_string_len;
  int len;
  unsigned char *new_uri_string;
  librdf_uri *new_uri;
  librdf_world *world=source_uri->world;
                                    
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(source_uri, librdf_uri, NULL);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(base_uri, librdf_uri, NULL);
  
  if(!uri_string)
    return NULL;

  /* empty URI - easy, just make from base_uri */
  if(!*uri_string && base_uri)
    return librdf_new_uri_from_uri(base_uri);

  /* not a fragment, and no match - easy */
  if(*uri_string != '#' &&
     strncmp((const char*)uri_string, (const char*)source_uri->string, source_uri->string_length))
    return librdf_new_uri(world, uri_string);

  /* darn - is a fragment or matches, is a prefix of the source URI */

  /* move uri_string pointer to first non-matching char 
   * unless a fragment, when all of the uri_string will 
   * be appended
   */
  if(*uri_string != '#')
    uri_string += source_uri->string_length;

  /* size of remaining bytes to copy from uri_string */
  uri_string_len=strlen((const char*)uri_string);

  /* total bytes */
  len=uri_string_len + 1 + base_uri->string_length;

  new_uri_string=(unsigned char*)LIBRDF_MALLOC(cstring, len);
  if(!new_uri_string)
    return NULL;
  strncpy((char*)new_uri_string, (const char*)base_uri->string, base_uri->string_length);
  /* strcpy not strncpy since I want a \0 on the end */
  strcpy((char*)new_uri_string + base_uri->string_length, (const char*)uri_string);
  
  new_uri=librdf_new_uri(world, new_uri_string);
  LIBRDF_FREE(cstring, new_uri_string); /* always free this even on failure */

  return new_uri; /* new URI or NULL from librdf_new_uri failure */
}



/**
 * librdf_new_uri_relative_to_base:
 * @base_uri: absolute base URI
 * @uri_string: relative URI string
 *
 * Constructor - create a new #librdf_uri object from a URI string relative to a base URI.
 *
 * An empty uri_string or NULL is equivalent to 
 * librdf_new_uri_from_uri(base_uri)
 * 
 * Return value: a new #librdf_uri object or NULL on failure
 **/
librdf_uri*
librdf_new_uri_relative_to_base(librdf_uri* base_uri,
                                const unsigned char *uri_string) {
  unsigned char *buffer;
  int buffer_length;
  librdf_uri* new_uri;
  librdf_world *world=base_uri->world;
                                  
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(base_uri, librdf_uri, NULL);

  if(!uri_string)
    return NULL;
  
  /* If URI string is empty, just copy base URI */
  if(!*uri_string)
    return librdf_new_uri_from_uri(base_uri);
  
  /* +2 is for \0 plus an extra 1 for adding any missing URI path '/' */
  buffer_length=base_uri->string_length + strlen((const char*)uri_string) +2;
  buffer=(unsigned char*)LIBRDF_MALLOC(cstring, buffer_length);
  if(!buffer)
    return NULL;
  
  raptor_uri_resolve_uri_reference(base_uri->string, uri_string,
                                   buffer, buffer_length);

  new_uri=librdf_new_uri(world, buffer);
  LIBRDF_FREE(cstring, buffer);
  return new_uri;
}


/**
 * librdf_new_uri_from_filename:
 * @world: Redland #librdf_world object
 * @filename: filename
 *
 * Constructor - create a new #librdf_uri object from a filename.
 *
 * Return value: a new #librdf_uri object or NULL on failure
 **/
librdf_uri*
librdf_new_uri_from_filename(librdf_world* world, const char *filename) {
  librdf_uri* new_uri;
  unsigned char *uri_string;

  librdf_world_open(world);

  if(!filename)
    return NULL;
  
  uri_string=raptor_uri_filename_to_uri_string(filename);
  if(!uri_string)
    return NULL;
  
  new_uri=librdf_new_uri(world, uri_string);
  raptor_free_memory(uri_string);
  return new_uri;
}



/**
 * librdf_free_uri:
 * @uri: #librdf_uri object
 *
 * Destructor - destroy a #librdf_uri object.
 * 
 **/
void
librdf_free_uri (librdf_uri* uri) 
{
  librdf_hash_datum key; /* on stack */
#ifdef WITH_THREADS
  librdf_world *world;
#endif

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN(uri, librdf_uri);

#ifdef WITH_THREADS
  world = uri->world;
  pthread_mutex_lock(world->mutex);
#endif

  uri->usage--;
  
#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
  LIBRDF_DEBUG3("URI %s usage count now %d\n", uri->string, uri->usage);
#endif

  /* decrement usage, don't free if not 0 yet*/
  if(uri->usage) {
#ifdef WITH_THREADS
    pthread_mutex_unlock(world->mutex);
#endif
    return;
  }

#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
  LIBRDF_DEBUG3("Deleting URI %s from hash, max usage was %d\n", uri->string, uri->max_usage);
#endif

  key.data=uri->string;
  key.size=uri->string_length;
  /* Hash deletion fails only if the key is not found.
     This is not a fatal error so do not check for return value. */
  librdf_hash_delete_all(uri->world->uris_hash, &key);

  if(uri->string)
    LIBRDF_FREE(cstring, uri->string);
  LIBRDF_FREE(librdf_uri, uri);

#ifdef WITH_THREADS
  pthread_mutex_unlock(world->mutex);
#endif
}


/**
 * librdf_uri_as_string:
 * @uri: #librdf_uri object
 *
 * Get a pointer to the string representation of the URI.
 * 
 * Returns a shared pointer to the URI string representation. 
 * Note: does not allocate a new string so the caller must not free it.
 * 
 * Return value: string representation of URI
 **/
unsigned char*
librdf_uri_as_string (librdf_uri *uri) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(uri, librdf_uri, NULL);

  return uri->string;
}


/**
 * librdf_uri_as_counted_string:
 * @uri: #librdf_uri object
 * @len_p: pointer to location to store length
 *
 * Get a pointer to the string representation of the URI with length.
 * 
 * Returns a shared pointer to the URI string representation. 
 * Note: does not allocate a new string so the caller must not free it.
 * 
 * Return value: string representation of URI
 **/
unsigned char*
librdf_uri_as_counted_string(librdf_uri *uri, size_t* len_p) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(uri, librdf_uri, NULL);

  if(len_p)
    *len_p=uri->string_length;
  return uri->string;
}


/**
 * librdf_uri_get_digest:
 * @uri: #librdf_uri object
 *
 * Get a digest for the URI.
 * 
 * Generates a digest object for the URI.  The digest factory used is
 * determined at class initialisation time by librdf_init_uri().
 * 
 * Return value: new #librdf_digest object or NULL on failure.
 **/
librdf_digest*
librdf_uri_get_digest(librdf_uri* uri) 
{
  librdf_world *world=uri->world;
  librdf_digest* d;
  
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(uri, librdf_uri, NULL);

  d=librdf_new_digest_from_factory(world, world->digest_factory);
  if(!d)
    return NULL;
  
  librdf_digest_update(d, (unsigned char*)uri->string, uri->string_length);
  librdf_digest_final(d);
  
  return d;
}


/**
 * librdf_uri_print:
 * @uri: #librdf_uri object
 * @fh: file handle
 *
 * Print the URI to the given file handle.
 *
 **/
void
librdf_uri_print (librdf_uri* uri, FILE *fh) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN(uri, librdf_uri);

  fputs((const char*)uri->string, fh);
}


/**
 * librdf_uri_to_string:
 * @uri: #librdf_uri object
 *
 * Format the URI as a string.
 * 
 * Note: this method allocates a new string since this is a _to_ method
 * and the caller must free the resulting memory.
 *
 * Return value: string representation of the URI or NULL on failure
 **/
unsigned char*
librdf_uri_to_string (librdf_uri* uri)
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(uri, librdf_uri, NULL);

  return librdf_uri_to_counted_string(uri, NULL);
}


/**
 * librdf_uri_to_counted_string:
 * @uri: #librdf_uri object
 * @len_p: pointer to location to store length
 *
 * Format the URI as a counted string.
 * 
 * Note: this method allocates a new string since this is a _to_ method
 * and the caller must free the resulting memory.
 *
 * Return value: string representation of the URI or NULL on failure
 **/
unsigned char*
librdf_uri_to_counted_string (librdf_uri* uri, size_t* len_p)
{
  unsigned char *s;

  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(uri, librdf_uri, NULL);

  if(!uri)
    return NULL;
  
  if(len_p)
    *len_p=uri->string_length;

  s=(unsigned char*)LIBRDF_MALLOC(cstring, uri->string_length+1);
  if(!s)
    return NULL;

  strcpy((char*)s, (const char*)uri->string);
  return s;
}


/**
 * librdf_uri_equals:
 * @first_uri: #librdf_uri object 1
 * @second_uri: #librdf_uri object 2
 *
 * Compare two librdf_uri objects for equality.
 * 
 * Return value: non 0 if the objects are equal
 **/
int
librdf_uri_equals(librdf_uri* first_uri, librdf_uri* second_uri) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(first_uri, librdf_uri, 0);
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(second_uri, librdf_uri, 0);

  if(!first_uri || !second_uri)
    return 0;
  return (first_uri == second_uri);
}


/**
 * librdf_uri_is_file_uri:
 * @uri: #librdf_uri object
 *
 * Test if a URI points to a filename.
 * 
 * Return value: Non zero if the URI points to a file
 **/
int
librdf_uri_is_file_uri(librdf_uri* uri) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(uri, librdf_uri, 1);

  return raptor_uri_uri_string_is_file_uri(uri->string);
}


/**
 * librdf_uri_to_filename:
 * @uri: #librdf_uri object
 *
 * Return pointer to filename of URI.
 * 
 * Returns a pointer to a newly allocated buffer that
 * the caller must free.  This will fail if the URI
 * is not a file: URI.  This can be checked with #librdf_uri_is_file_uri
 *
 * Return value: pointer to filename or NULL on failure
 **/
const char*
librdf_uri_to_filename(librdf_uri* uri) 
{
  LIBRDF_ASSERT_OBJECT_POINTER_RETURN_VALUE(uri, librdf_uri, NULL);

  return raptor_uri_uri_string_to_filename(uri->string);
  
}


/**
 * librdf_uri_compare:
 * @uri1: #librdf_uri object 1 or NULL
 * @uri2: #librdf_uri object 2 or NULL
 * 
 * Compare two librdf_uri objects lexicographically.
 * 
 * A NULL URI is always less than (never equal to) a non-NULL URI.
 *
 * Return value: <0 if @uri1 is less than @uri2, 0 if equal, >0 if @uri1 is greater than @uri2
 **/
int
librdf_uri_compare(librdf_uri* uri1, librdf_uri* uri2)
{
  if(uri1 == uri2)
    return 0;
  else if(!uri1)
    return -1;
  else if(!uri2)
    return 1;
  else
    return strcmp((const char*)uri1->string, (const char*)uri2->string);
}


#endif


/* TEST CODE */


#ifdef STANDALONE

/* one more prototype */
int main(int argc, char *argv[]);


int
main(int argc, char *argv[]) 
{
  const unsigned char *hp_string=(const unsigned char*)"http://purl.org/net/dajobe/";
  librdf_uri *uri1, *uri2, *uri3, *uri4, *uri5, *uri6, *uri7, *uri8, *uri9;
  librdf_digest *d;
  const char *program=librdf_basename((const char*)argv[0]);
  const char *file_string="/big/long/directory/file";
  const unsigned char *file_uri_string=(const unsigned char*)"file:///big/long/directory/file";
  const unsigned char *uri_string=(const unsigned char*)"http://example.com/big/long/directory/blah#frag";
  const unsigned char *relative_uri_string1=(const unsigned char*)"#foo";
  const unsigned char *relative_uri_string2=(const unsigned char*)"bar";
  librdf_world *world;
  
  world=librdf_new_world();
  librdf_world_open(world);

  fprintf(stderr, "%s: Creating new URI from string\n", program);
  uri1=librdf_new_uri(world, hp_string);
  if(!uri1) {
    fprintf(stderr, "%s: Failed to create URI from string '%s'\n", program, 
	    hp_string);
    return(1);
  }
  
  fprintf(stderr, "%s: Home page URI is ", program);
  librdf_uri_print(uri1, stderr);
  fputs("\n", stderr);
  
  fprintf(stderr, "%s: Creating URI from URI\n", program);
  uri2=librdf_new_uri_from_uri(uri1);
  if(!uri2) {
    fprintf(stderr, "%s: Failed to create new URI from old one\n", program);
    return(1);
  }

  fprintf(stderr, "%s: New URI is ", program);
  librdf_uri_print(uri2, stderr);
  fputs("\n", stderr);

  
  fprintf(stderr, "%s: Getting digest for URI\n", program);
  d=librdf_uri_get_digest(uri2);
  if(!d) {
    fprintf(stderr, "%s: Failed to get digest for URI %s\n", program, 
	    librdf_uri_as_string(uri2));
    return(1);
  }
  fprintf(stderr, "%s: Digest is: ", program);
  librdf_digest_print(d, stderr);
  fputs("\n", stderr);
  librdf_free_digest(d);

  uri3=librdf_new_uri(world, (const unsigned char*)"file:/big/long/directory/");
  uri4=librdf_new_uri(world, (const unsigned char*)"http://somewhere/dir/");
  fprintf(stderr, "%s: Source URI is ", program);
  librdf_uri_print(uri3, stderr);
  fputs("\n", stderr);
  fprintf(stderr, "%s: Base URI is ", program);
  librdf_uri_print(uri4, stderr);
  fputs("\n", stderr);
  fprintf(stderr, "%s: URI string is '%s'\n", program, uri_string);

  uri5=librdf_new_uri_normalised_to_base(uri_string, uri3, uri4);
  fprintf(stderr, "%s: Normalised URI is ", program);
  librdf_uri_print(uri5, stderr);
  fputs("\n", stderr);


  uri6=librdf_new_uri_relative_to_base(uri5, relative_uri_string1);
  fprintf(stderr, "%s: URI + Relative URI %s gives ", program, 
          relative_uri_string1);
  librdf_uri_print(uri6, stderr);
  fputs("\n", stderr);

  uri7=librdf_new_uri_relative_to_base(uri5, relative_uri_string2);
  fprintf(stderr, "%s: URI + Relative URI %s gives ", program, 
          relative_uri_string2);
  librdf_uri_print(uri7, stderr);
  fputs("\n", stderr);

  uri8=librdf_new_uri_from_filename(world, file_string);
  uri9=librdf_new_uri(world, file_uri_string);
  if(!librdf_uri_equals(uri8, uri9)) {
    fprintf(stderr, "%s: URI string from filename %s returned %s, expected %s\n", program, file_string, librdf_uri_as_string(uri8), file_uri_string);
    return(1);
  }

  fprintf(stderr, "%s: Freeing URIs\n", program);
  librdf_free_uri(uri1);
  librdf_free_uri(uri2);
  librdf_free_uri(uri3);
  librdf_free_uri(uri4);
  librdf_free_uri(uri5);
  librdf_free_uri(uri6);
  librdf_free_uri(uri7);
  librdf_free_uri(uri8);
  librdf_free_uri(uri9);
  
  librdf_free_world(world);

  /* keep gcc -Wall happy */
  return(0);
}

#endif
