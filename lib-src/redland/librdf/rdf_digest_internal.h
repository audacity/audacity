/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_digest_internal.h - Internal RDF Digest Factory / Digest definitions
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



#ifndef LIBRDF_DIGEST_INTERNAL_H
#define LIBRDF_DIGEST_INTERNAL_H

#ifdef __cplusplus
extern "C" {
#endif


/* based on the GNUPG cipher/digest registration stuff */
struct librdf_digest_factory_s 
{
  struct librdf_digest_factory_s* next;
  char *   name;

  /* the rest of this structure is populated by the
     digest-specific register function */
  size_t  context_length;
  size_t  digest_length;

  /* functions (over context) */
  void (*init)( void *_context );
  void (*update)( void *_context, const unsigned char *buf, size_t nbytes );
  void (*final)( void *_context );
  unsigned char *(*get_digest)( void *_context );
};


struct librdf_digest_s {
  librdf_world *world;
  char *context;
  unsigned char *digest;
  librdf_digest_factory* factory;
};

/* module init */
void librdf_init_digest(librdf_world *world);
/* module finish */
void librdf_finish_digest(librdf_world *world);

/* in librdf_digest_openssl.c */
#ifdef HAVE_OPENSSL_DIGESTS
void librdf_digest_openssl_constructor(librdf_world *world);
#endif

/* in librdf_digest_md5.c */
#ifdef HAVE_LOCAL_MD5_DIGEST
void librdf_digest_md5_constructor(librdf_world *world);
#endif

/* in librdf_digest_sha1.c */
#ifdef HAVE_LOCAL_SHA1_DIGEST
void librdf_digest_sha1_constructor(librdf_world *world);
#endif

/* in librdf_digest_ripemd160.c */
#ifdef HAVE_LOCAL_RIPEMD160_DIGEST
void librdf_digest_rmd160_constructor(librdf_world *world);
#endif

/* factory static methods */
void librdf_digest_register_factory(librdf_world *world, const char *name, void (*factory) (librdf_digest_factory*));

librdf_digest_factory* librdf_get_digest_factory(librdf_world *world, const char *name);

/* constructor */
librdf_digest* librdf_new_digest_from_factory(librdf_world *world, librdf_digest_factory *factory);

#ifdef __cplusplus
}
#endif

#endif
