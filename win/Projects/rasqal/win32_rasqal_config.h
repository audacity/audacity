/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * win32_config.h - Rasqal WIN32 hard-coded config
 *
 * Copyright (C) 2004-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2004-2005, University of Bristol, UK http://www.bristol.ac.uk/
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


#ifndef WIN32_CONFIG_H
#define WIN32_CONFIG_H


#ifdef __cplusplus
extern "C" {
#endif

#define WIN32_LEAN_AND_MEAN 1

/* getopt is not in standard win32 C library - define if we have it */
/* #define HAVE_GETOPT_H 1 */

#define HAVE_STDLIB_H 1

#define HAVE_STRICMP 1

/* MS names for these functions */
#if !defined _MSC_VER || _MSC_VER < 1500
   #define vsnprintf _vsnprintf
#endif
#define snprintf _snprintf
#define access _access
#define stricmp _stricmp
#define strnicmp _strnicmp

#include <float.h>
#define isnan _isnan

/* This is a SPARQL token define */
#undef OPTIONAL

#define HAVE_C99_VSNPRINTF 1

/* for access() which is POSIX but doesn't seem to have the defines in VC */
#ifndef R_OK
#define R_OK 4
#endif

/* __func__ doesn't exist in Visual Studio 6 */
#define __func__ ""

/* 
 * Defines that come from config.h
 */

/* Release version as a decimal */
#define RASQAL_VERSION_DECIMAL 916

/* Major version number */
#define RASQAL_VERSION_MAJOR 0

/* Minor version number */
#define RASQAL_VERSION_MINOR 9

/* Release version number */
#define RASQAL_VERSION_RELEASE 16

/* Version number of package */
#define VERSION "0.9.16"

#include <windows.h>
#include <io.h>
#include <memory.h>

/* This is a SPARQL token define */
#ifdef OPTIONAL
#undef OPTIONAL
#endif

/* bison: output uses ERROR in an enum which breaks if this is defined */
#ifdef ERROR
#undef ERROR
#endif

/* flex: const is available */
#define YY_USE_CONST
/* looks like the .c files define this anyway */
/* #define YY_NO_UNISTD_H */

#undef RASQAL_INLINE
#define RASQAL_INLINE __inline

/* Decimal without a library */
#define RASQAL_DECIMAL_NONE 1

/* Building RDQL query */
//#define RASQAL_QUERY_RDQL 1

/* Building SPARQL query */
#define RASQAL_QUERY_SPARQL 1

/* Use raptor to provide triples */
#define RAPTOR_TRIPLES_SOURCE_RAPTOR 1

/* Use redland to provide triples */
/* #define RAPTOR_TRIPLES_SOURCE_REDLAND 1 */

/* Use PCRE regex library */
// #define RASQAL_REGEX_PCRE 1

/* Use posix regex library */
//#define RASQAL_REGEX_POSIX 1

#ifdef _DEBUG
#define RASQAL_DEBUG 1
#endif

#undef DELETE

#ifdef __cplusplus
}
#endif

#endif
