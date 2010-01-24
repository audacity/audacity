/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_hash.c - RDF Hash Cursor Implementation
 *
 * Copyright (C) 2000-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2000-2004, University of Bristol, UK http://www.bristol.ac.uk/
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
#include <stdarg.h>
#include <ctype.h>
#include <sys/types.h>

#include <redland.h>


/* private structure */
struct librdf_hash_cursor_s {
  librdf_hash *hash;
  void *context;
};



/**
 * librdf_new_hash_cursor:
 * @hash: the hash object
 *
 * Constructor - Create a new #librdf_hash_cursor over a #librdf_hash.
 *
 * Return value: a new #librdf_hash_cursor or NULL on failure
 **/
librdf_hash_cursor*
librdf_new_hash_cursor (librdf_hash* hash) 
{
  librdf_hash_cursor* cursor;
  void *cursor_context;

  cursor=(librdf_hash_cursor*)LIBRDF_CALLOC(librdf_hash_cursor, 1, 
                                            sizeof(librdf_hash_cursor));
  if(!cursor)
    return NULL;

  cursor_context=(char*)LIBRDF_CALLOC(librdf_hash_cursor_context, 1,
                                      hash->factory->cursor_context_length);
  if(!cursor_context) {
    LIBRDF_FREE(librdf_hash_cursor, cursor);
    return NULL;
  }

  cursor->hash=hash;
  cursor->context=cursor_context;

  if(hash->factory->cursor_init(cursor->context, hash->context)) {
    librdf_free_hash_cursor(cursor);
    cursor=NULL;
  }

  return cursor;
}


/**
 * librdf_free_hash_cursor:
 *
 * Destructor - destroy a #librdf_hash_cursor object.
 *
 * @cursor: hash cursor object
 **/
void
librdf_free_hash_cursor (librdf_hash_cursor* cursor) 
{
  if(cursor->context) {
    cursor->hash->factory->cursor_finish(cursor->context);
    LIBRDF_FREE(librdf_hash_cursor_context, cursor->context);
  }

  LIBRDF_FREE(librdf_hash_cursor, cursor);
}


int
librdf_hash_cursor_set(librdf_hash_cursor *cursor,
                       librdf_hash_datum *key,
                       librdf_hash_datum *value)
{
  return cursor->hash->factory->cursor_get(cursor->context, key, value, 
                                           LIBRDF_HASH_CURSOR_SET);
}


int
librdf_hash_cursor_get_next_value(librdf_hash_cursor *cursor, 
                                  librdf_hash_datum *key,
                                  librdf_hash_datum *value)
{
  return cursor->hash->factory->cursor_get(cursor->context, key, value, 
                                           LIBRDF_HASH_CURSOR_NEXT_VALUE);
}


int
librdf_hash_cursor_get_first(librdf_hash_cursor *cursor,
                             librdf_hash_datum *key, librdf_hash_datum *value)
{
  return cursor->hash->factory->cursor_get(cursor->context, key, value, 
                                           LIBRDF_HASH_CURSOR_FIRST);
}


int
librdf_hash_cursor_get_next(librdf_hash_cursor *cursor, librdf_hash_datum *key,
                            librdf_hash_datum *value)
{
  return cursor->hash->factory->cursor_get(cursor->context, key, value, 
                                           LIBRDF_HASH_CURSOR_NEXT);
}
