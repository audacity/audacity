/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_files.c - RDF File and directory handling utilities
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
#include <string.h>
#include <stdarg.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h> /* for mktemp(), mkstemp(), getenv() */
#endif
#ifdef HAVE_MKSTEMP
#ifdef HAVE_UNISTD_H
#include <unistd.h> /* for close(), unlink() */
#endif
#endif

#ifdef STANDALONE
#define LIBRDF_DEBUG 1
#endif

#include <redland.h>

#ifndef STANDALONE



/**
 * librdf_files_temporary_file_name:
 * 
 * Create a temporary file name.
 * 
 * @deprecated: Do not use this, it is unsafe.
 *
 * Return value: a new filename or NULL on failure.
 **/
char *
librdf_files_temporary_file_name(void) 
{
#if defined(HAVE_MKSTEMP) || defined(HAVE_MKTEMP)
  const char *tmp_dir;
  size_t length;
  char *name;
  static const char * const file_template="librdf_tmp_XXXXXX"; /* FIXME */
#ifdef HAVE_MKSTEMP
  int fd;
#endif

  /* FIXME: unix dependencies */
  tmp_dir=getenv("TMPDIR");
  if(!tmp_dir)
    tmp_dir="/tmp";

  length=strlen(tmp_dir) + strlen(file_template) + 2; /* 2: / sep and \/0 */
  
  name=(char*)LIBRDF_MALLOC(cstring, length);
  if(!name)
    return NULL;

  /* FIXME: unix dependency - file/dir separator */
  sprintf(name, "%s/%s", tmp_dir, file_template);
  
#ifdef HAVE_MKSTEMP
  /* Proritise mkstemp() since GNU libc says: Never use mktemp(). */
  fd=mkstemp(name);
  if(fd<0) {
    LIBRDF_FREE(cstring, name);
    return NULL;
  }
  close(fd);
  unlink(name);

  return name;  
#else
  return mktemp(name);
#endif

#else
#ifdef HAVE_TMPNAM
  /* GNU libc says: Never use this function. Use mkstemp(3) instead. */
  char *name;
  char *new_name;

  name=tmpnam(NULL); /* NULL ensures statically allocated */
  new_name=(char*)LIBRDF_MALLOC(cstring, strlen(name)+1);
  if(!new_name)
    return NULL;
  strcpy(new_name, name);

  return name;
#else /* not tmpnam(), mkstemp() or mktemp() */
HELP
#endif
#endif
}

#endif


/* TEST CODE */


#ifdef STANDALONE

/* one more prototype */
int main(int argc, char *argv[]);


int
main(int argc, char *argv[]) 
{
  /* keep gcc -Wall happy */
  return(0);
}

#endif
