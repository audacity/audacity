/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_types.h - RDF data types used by some bit-twiddling routines
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



#ifndef LIBRDF_TYPES_H
#define LIBRDF_TYPES_H

#ifdef __cplusplus
extern "C" {
#endif

#include <sys/types.h>

#ifndef HAVE_U64
  #if SIZEOF_UNSIGNED_INT == 8
    typedef unsigned int u64;
  #elif SIZEOF_UNSIGNED_LONG == 8
    typedef unsigned long u64;
  #elif SIZEOF_UNSIGNED_LONG_LONG == 8
    #ifdef WIN32
      typedef __int64 u64;
    #else
       typedef unsigned long long u64;
    #endif
  #else
    #error u64 type not defined
  #endif
#endif

#ifndef HAVE_U32
  #if SIZEOF_UNSIGNED_INT == 4
    typedef unsigned int u32;
  #elif SIZEOF_UNSIGNED_LONG == 4
    typedef unsigned long u32;
  #else
    #error u32 type not defined
  #endif
#endif


#ifndef HAVE_BYTE
  #if SIZEOF_UNSIGNED_CHAR == 1
    typedef unsigned char byte;
  #else
    #error byte type not defined
  #endif
#endif


#ifdef WIN32
#define UINT64_T_FMT "%I64u"
#else
#define UINT64_T_FMT "%llu"
#endif


#ifdef __cplusplus
}
#endif

#endif
