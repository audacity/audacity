/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * win32_rdf_config.h - redland WIN32 hard-coded config
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

#ifndef WIN32_RDF_CONFIG_H
#define WIN32_RDF_CONFIG_H


#ifdef __cplusplus
extern "C" {
#endif

#define WIN32_LEAN_AND_MEAN 1

/* getopt is not in standard win32 C library - define if we have it */
/* #define HAVE_GETOPT_H 1 */

#define HAVE_STDLIB_H 1

/* For using expat on win32 */
/*#define RAPTOR_XML_EXPAT 1
#define HAVE_EXPAT_H 1*/

#define HAVE_STRICMP 1

/* MS names for these functions */
#if !defined _MSC_VER || _MSC_VER < 1500
   #define vsnprintf _vsnprintf
#endif
#define snprintf _snprintf
#define access _access
#define stricmp _stricmp
#define strnicmp _strnicmp
#define close _close
#define unlink _unlink
#define mktemp _mktemp

#define HAVE_C99_VSNPRINTF 1

/* for access() which is POSIX but doesn't seem to have the defines in VC */
#ifndef R_OK
#define F_OK 0
#define R_OK 4
#endif

/* __func__ doesn't exist in Visual Studio 6 */
#define __func__ ""

#undef REDLAND_INLINE
#define REDLAND_INLINE __inline


/* 
 * Defines that come from rdf_config.h
 */

/* BDB has close method with 2 args */
#define HAVE_BDB_CLOSE_2_ARGS 1

/* BDB defines DBC */
#define HAVE_BDB_CURSOR 1

/* BDB cursor method has 4 arguments */
#define HAVE_BDB_CURSOR_4_ARGS 1

/* BDB defines DB_TXN */
#define HAVE_BDB_DB_TXN 1

/* BDB has fd method with 2 args */
#define HAVE_BDB_FD_2_ARGS 1

/* Have BDB hash support */
#define HAVE_BDB_HASH 1

/* BDB has open method with 6 args */
/* #undef HAVE_BDB_OPEN_6_ARGS */

/* BDB has open method with 7 args */
#define HAVE_BDB_OPEN_7_ARGS 1

/* BDB has set_flags method */
#define HAVE_BDB_SET_FLAGS 1

/* BDB has dbopen method */
/* #undef HAVE_DBOPEN */

/* BDB has db_create method */
#define HAVE_DB_CREATE 1

/* Define to 1 if you have the <db.h> header file. */
#define HAVE_DB_H 1

/* Define to 1 if you have the <dlfcn.h> header file. */
/* undef HAVE_DLFCN_H */

/* Define to 1 if you have the <dmalloc.h> header file. */
/* #undef HAVE_DMALLOC_H */

/* Define to 1 if you have the <errno.h> header file. */
#define HAVE_ERRNO_H 1

/* Define to 1 if you have the <fcntl.h> header file. */
#define HAVE_FCNTL_H 1

/* Define to 1 if you have the `getenv' function. */
#define HAVE_GETENV 1

/* Define to 1 if you have the `getopt' function. */
#define HAVE_GETOPT 1

/* Define to 1 if you have the <getopt.h> header file. */
#define HAVE_GETOPT_H 1

/* Define to 1 if you have the `getopt_long' function. */
#define HAVE_GETOPT_LONG 1

/* Define to 1 if you have the `gettimeofday' function. */
/* #undef HAVE_GETTIMEOFDAY */

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* W3C libwww is present */
/* #undef HAVE_LIBWWW */

/* Have local MD5 digest */
#define HAVE_LOCAL_MD5_DIGEST 1

/* Have local RIPEMD160 digest */
/* #undef HAVE_LOCAL_RIPEMD160_DIGEST */

/* Have local SHA1 digest */
#define HAVE_LOCAL_SHA1_DIGEST 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have the `mkstemp' function. */
/* #undef HAVE_MKSTEMP */

/* Define to 1 if you have the `mktemp' function. */
#define HAVE_MKTEMP 1

/* MySQL libraries are present */
#define HAVE_MYSQL 1

/* Define to 1 if you have the <openssl/crypto.h> header file. */
/* #undef HAVE_OPENSSL_CRYPTO_H */

/* Have openssl MD5 digest */
/* #undef HAVE_OPENSSL_CRYPTO_MD5_DIGEST */

/* Have openssl RIPEMD160 digest */
/* #undef HAVE_OPENSSL_CRYPTO_RIPEMD160_DIGEST */

/* Have openssl SHA1 digest */
/* #undef HAVE_OPENSSL_CRYPTO_SHA1_DIGEST */

/* Have openssl digests */
/* #undef HAVE_OPENSSL_DIGESTS */

/* Define to 1 if you have the <pthread.h> header file. */
/* #undef HAVE_PTHREAD_H */

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/time.h> header file. */
/*#define HAVE_SYS_TIME_H*/

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <time.h> header file. */
#define HAVE_TIME_H 1

/* Define to 1 if you have the `tmpnam' function. */
#define HAVE_TMPNAM 1

/* Have 3store available */
/* #undef HAVE_TSTORE */

/* Define to 1 if you have the <unistd.h> header file. */
/* #define HAVE_UNISTD_H */

/* Disable run time assertion checks. */
/* #undef LIBRDF_DISABLE_ASSERT */

/* Disable run time assertion check messages. */
/* #undef LIBRDF_DISABLE_ASSERT_MESSAGES */

/* Release version as a decimal */
#define LIBRDF_VERSION_DECIMAL 10008

/* Major version number */
#define LIBRDF_VERSION_MAJOR 1

/* Minor version number */
#define LIBRDF_VERSION_MINOR 0

/* Release version number */
#define LIBRDF_VERSION_RELEASE 8

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

/* SQLite API version */
#define SQLITE_API 3

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Building file storage */
#define STORAGE_FILE 1

/* Building MySQL storage */
#define STORAGE_MYSQL 1

/* Building PostgreSQL storage */
/*#undef STORAGE_POSTGRESQL*/

/* Building SQLite storage */
#define STORAGE_SQLITE 1

/* Building 3store storage */
/*#undef STORAGE_TSTORE*/

/* Define to 1 if you can safely include both <sys/time.h> and <time.h>. */
/* #define TIME_WITH_SYS_TIME*/

/* Version number of package */
#define VERSION "1.0.8"

/* Use POSIX threads */
/* #undef WITH_THREADS */

/* Define to 1 if your processor stores words with the most significant byte
   first (like Motorola and SPARC, unlike Intel and VAX). */
/* #undef WORDS_BIGENDIAN */

/* Define to empty if `const' does not conform to ANSI C. */
/* #undef const */


#include <windows.h>
#include <io.h>
#include <memory.h>

/* get _isnan() since it is not in math.h */
#include <float.h>
#ifndef isnan
#define isnan(d) (_isnan(d))
#endif

#ifdef __cplusplus
}
#endif

#endif
