/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * win32_config.h - Raptor WIN32 hard-coded config
 *
 * Copyright (C) 2002-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2002-2005, University of Bristol, UK http://www.bristol.ac.uk/
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


#ifndef WIN32_CONFIG_H
#define WIN32_CONFIG_H


#ifdef __cplusplus
extern "C" {
#endif

#define WIN32_LEAN_AND_MEAN 1

/* getopt is not in standard win32 C library - define if we have it */
/* #define HAVE_GETOPT_H 1 */

#define HAVE_STDLIB_H 1

#if 0
/* For using expat on win32 */
#define RAPTOR_XML_EXPAT 1
#define HAVE_EXPAT_H 1

#else
/* For using libxml2 on win32 */
#define RAPTOR_XML_LIBXML
#define HAVE_LIBXML_XMLREADER_H
/* does libxml struct xmlEntity have a field etype */
/*#define RAPTOR_LIBXML_ENTITY_ETYPE*/

/* does libxml struct xmlEntity have a field name_length */
/*#define RAPTOR_LIBXML_ENTITY_NAME_LENGTH*/

/* Define to 1 if you have the `xmlCtxtUseOptions' function. */
#define HAVE_XMLCTXTUSEOPTIONS 1

/* Define to 1 if you have the `xmlSAX2InternalSubset' function. */
#define HAVE_XMLSAX2INTERNALSUBSET 1

/* does libxml xmlSAXHandler have externalSubset field */
/*#define RAPTOR_LIBXML_XMLSAXHANDLER_EXTERNALSUBSET*/

/* does libxml xmlSAXHandler have initialized field */
/*#define RAPTOR_LIBXML_XMLSAXHANDLER_INITIALIZED*/
#endif

#define HAVE_STRICMP 1

/* MS names for these functions */
#define vsnprintf _vsnprintf
#define snprintf _snprintf
#define access _access
#define stricmp _stricmp
#define strnicmp _strnicmp

/*#define HAVE_C99_VSNPRINTF */

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
#define RAPTOR_VERSION_DECIMAL 10418

/* Major version number */
#define RAPTOR_VERSION_MAJOR 1

/* Minor version number */
#define RAPTOR_VERSION_MINOR 4

/* Release version number */
#define RAPTOR_VERSION_RELEASE 18

/* Version number of package */
#define VERSION "1.4.18"


#ifdef RAPTOR_XML_LIBXML
/* RSS parser needs libxml 2.5.x+ */
#define RAPTOR_PARSER_RSS 1
#else
#undef RAPTOR_PARSER_RSS
#endif

#define RAPTOR_PARSER_GUESS 1
#define RAPTOR_PARSER_GRDDL 1
#define RAPTOR_PARSER_N3 1
#define RAPTOR_PARSER_TURTLE 1
#define RAPTOR_PARSER_NTRIPLES 1
#define RAPTOR_PARSER_RDFXML 1

#define RAPTOR_SERIALIZER_ATOM 1
#define RAPTOR_SERIALIZER_RSS_1_0 1
#define RAPTOR_SERIALIZER_RDFXML 1
#define RAPTOR_SERIALIZER_RDFXML_ABBREV 1
#define RAPTOR_SERIALIZER_NTRIPLES 1

#define RAPTOR_WWW_LIBCURL 1


#include <windows.h>

#include <io.h>
#include <memory.h>

/* bison: output uses ERROR in an enum which breaks if this is defined */
#ifdef ERROR
#undef ERROR
#endif

/* flex: const is available */
#define YY_USE_CONST

#undef RAPTOR_INLINE
#define RAPTOR_INLINE __inline

/* The size of a `unsigned char', as computed by sizeof. */
#define SIZEOF_UNSIGNED_CHAR 1

/* The size of a `unsigned short', as computed by sizeof. */
#define SIZEOF_UNSIGNED_SHORT 2

/* The size of a `unsigned int', as computed by sizeof. */
#define SIZEOF_UNSIGNED_INT 4

/* The size of a `unsigned long', as computed by sizeof. */
#define SIZEOF_UNSIGNED_LONG 4

/* The size of a `unsigned long long', as computed by sizeof. */
#define SIZEOF_UNSIGNED_LONG_LONG 8


#ifdef __cplusplus
}
#endif

#endif
