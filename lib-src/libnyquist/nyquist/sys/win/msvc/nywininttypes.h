/*
 * Copyright (c) 2000,2001 Apple Computer, Inc. All rights reserved.
 *
 * @APPLE_LICENSE_HEADER_START@
 *
 * The contents of this file constitute Original Code as defined in and
 * are subject to the Apple Public Source License Version 1.1 (the
 * "License").  You may not use this file except in compliance with the
 * License.  Please obtain a copy of the License at
 * http://www.apple.com/publicsource and read it before using this file.
 *
 * This Original Code and all software distributed under the License are
 * distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, AND APPLE HEREBY DISCLAIMS ALL SUCH WARRANTIES,
 * INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE OR NON-INFRINGEMENT.  Please see the
 * License for the specific language governing rights and limitations
 * under the License.
 *
 * @APPLE_LICENSE_HEADER_END@
 */

/*
 * <inttypes.h> -- Standard C header, defined in ISO/IEC 9899:1999
 * (aka "C99"), section 7.8.   This defines format string conversion
 * specifiers suitable for use within arguments to fprintf and fscanf
 * and their ilk.
 */

#if !defined(_INTTYPES_H_)
#define _INTTYPES_H_

#include <stdint.h>

#if !defined(__STDC_VERSION__) || (__STDC_VERSION__ < 199901L)
  /* Translator is not ISO/IEC 9899:1999-compliant. */
  #if !defined(restrict)
    #define restrict
    #define __RESTRICT_KEYWORD_DEFINED__
  #endif
#endif

/* "C++ implementations should define these macros only when
 *  __STDC_FORMAT_MACROS is defined before <inttypes.h> is included."
 */
#if (! defined(__cplusplus)) || defined(__STDC_FORMAT_MACROS)

  #undef __PRI_8_LENGTH_MODIFIER__
  #undef __PRI_64_LENGTH_MODIFIER__
  #undef __SCN_8_LENGTH_MODIFIER__
  #undef __SCN_64_LENGTH_MODIFIER__

  #if defined(__STDC_LIBRARY_SUPPORTED__)
    #define __PRI_8_LENGTH_MODIFIER__ "%hh"
    #define __PRI_64_LENGTH_MODIFIER__ "%ll"
    #define __SCN_8_LENGTH_MODIFIER__ "%hh"
    #define __SCN_64_LENGTH_MODIFIER__ "%ll"
  #else
    #define __PRI_8_LENGTH_MODIFIER__ "%"  /* none */
    #define __PRI_64_LENGTH_MODIFIER__ "%q"
  #endif

  #define PRId8         __PRI_8_LENGTH_MODIFIER__ "d"
  #define PRIi8         __PRI_8_LENGTH_MODIFIER__ "i"
  #define PRIo8         __PRI_8_LENGTH_MODIFIER__ "o"
  #define PRIu8         __PRI_8_LENGTH_MODIFIER__ "u"
  #define PRIx8         __PRI_8_LENGTH_MODIFIER__ "x"
  #define PRIX8         __PRI_8_LENGTH_MODIFIER__ "X"

  #define PRId16        "%hd"
  #define PRIi16        "%hi"
  #define PRIo16        "%ho"
  #define PRIu16        "%hu"
  #define PRIx16        "%hx"
  #define PRIX16        "%hX"

  #define PRId32        "%ld"
  #define PRIi32        "%li"
  #define PRIo32        "%lo"
  #define PRIu32        "%lu"
  #define PRIx32        "%lx"
  #define PRIX32        "%lX"

  #define PRId64        __PRI_64_LENGTH_MODIFIER__ "d"
  #define PRIi64        __PRI_64_LENGTH_MODIFIER__ "i"
  #define PRIo64        __PRI_64_LENGTH_MODIFIER__ "o"
  #define PRIu64        __PRI_64_LENGTH_MODIFIER__ "u"
  #define PRIx64        __PRI_64_LENGTH_MODIFIER__ "x"
  #define PRIX64        __PRI_64_LENGTH_MODIFIER__ "X"

  #define PRIdLEAST8    PRId8
  #define PRIiLEAST8    PRIi8
  #define PRIoLEAST8    PRIo8
  #define PRIuLEAST8    PRIu8
  #define PRIxLEAST8    PRIx8
  #define PRIXLEAST8    PRIX8

  #define PRIdLEAST16   PRId16
  #define PRIiLEAST16   PRIi16
  #define PRIoLEAST16   PRIo16
  #define PRIuLEAST16   PRIu16
  #define PRIxLEAST16   PRIx16
  #define PRIXLEAST16   PRIX16

  #define PRIdLEAST32   PRId32
  #define PRIiLEAST32   PRIi32
  #define PRIoLEAST32   PRIo32
  #define PRIuLEAST32   PRIu32
  #define PRIxLEAST32   PRIx32
  #define PRIXLEAST32   PRIX32

  #define PRIdLEAST64   PRId64
  #define PRIiLEAST64   PRIi64
  #define PRIoLEAST64   PRIo64
  #define PRIuLEAST64   PRIu64
  #define PRIxLEAST64   PRIx64
  #define PRIXLEAST64   PRIX64

  #define PRIdFAST8     PRId32
  #define PRIiFAST8     PRIi32
  #define PRIoFAST8     PRIo32
  #define PRIuFAST8     PRIu32
  #define PRIxFAST8     PRIx32
  #define PRIXFAST8     PRIX32

  #define PRIdFAST16    PRId32
  #define PRIiFAST16    PRIi32
  #define PRIoFAST16    PRIo32
  #define PRIuFAST16    PRIu32
  #define PRIxFAST16    PRIx32
  #define PRIXFAST16    PRIX32

  #define PRIdFAST32    PRId32
  #define PRIiFAST32    PRIi32
  #define PRIoFAST32    PRIo32
  #define PRIuFAST32    PRIu32
  #define PRIxFAST32    PRIx32
  #define PRIXFAST32    PRIX32

  #define PRIdFAST64    PRId64
  #define PRIiFAST64    PRIi64
  #define PRIoFAST64    PRIo64
  #define PRIuFAST64    PRIu64
  #define PRIxFAST64    PRIx64
  #define PRIXFAST64    PRIX64

  #define PRIdPTR       PRId32
  #define PRIiPTR       PRIi32
  #define PRIoPTR       PRIo32
  #define PRIuPTR       PRIu32
  #define PRIxPTR       PRIx32
  #define PRIXPTR       PRIX32

  #define PRIdMAX       PRId64
  #define PRIiMAX       PRIi64
  #define PRIoMAX       PRIo64
  #define PRIuMAX       PRIu64
  #define PRIxMAX       PRIx64
  #define PRIXMAX       PRIX64

  #if defined(__SCN_8_LENGTH_MODIFIER__)
    #define SCNd8       __SCN_8_LENGTH_MODIFIER__ "d"
    #define SCNi8       __SCN_8_LENGTH_MODIFIER__ "i"
    #define SCNo8       __SCN_8_LENGTH_MODIFIER__ "o"
    #define SCNu8       __SCN_8_LENGTH_MODIFIER__ "u"
    #define SCNx8       __SCN_8_LENGTH_MODIFIER__ "x"
  #endif

  #define SCNd16        "%hd"
  #define SCNi16        "%hi"
  #define SCNo16        "%ho"
  #define SCNu16        "%hu"
  #define SCNx16        "%hx"

  #define SCNd32        "%ld"
  #define SCNi32        "%li"
  #define SCNo32        "%lo"
  #define SCNu32        "%lu"
  #define SCNx32        "%lx"

  #if defined(__SCN_64_LENGTH_MODIFIER__)
    #define SCNd64      __SCN_64_LENGTH_MODIFIER__ "d"
    #define SCNi64      __SCN_64_LENGTH_MODIFIER__ "i"
    #define SCNo64      __SCN_64_LENGTH_MODIFIER__ "o"
    #define SCNu64      __SCN_64_LENGTH_MODIFIER__ "u"
    #define SCNx64      __SCN_64_LENGTH_MODIFIER__ "x"
  #endif

  #if defined(__SCN_8_LENGTH_MODIFIER__)
    #define SCNdLEAST8  SCNd8
    #define SCNiLEAST8  SCNi8
    #define SCNoLEAST8  SCNo8
    #define SCNuLEAST8  SCNu8
    #define SCNxLEAST8  SCNx8
  #endif

  #define SCNdLEAST16   SCNd16
  #define SCNiLEAST16   SCNi16
  #define SCNoLEAST16   SCNo16
  #define SCNuLEAST16   SCNu16
  #define SCNxLEAST16   SCNx16

  #define SCNdLEAST32   SCNd32
  #define SCNiLEAST32   SCNi32
  #define SCNoLEAST32   SCNo32
  #define SCNuLEAST32   SCNu32
  #define SCNxLEAST32   SCNx32

  #if defined(__SCN_64_LENGTH_MODIFIER__)
    #define SCNdLEAST64 SCNd64
    #define SCNiLEAST64 SCNi64
    #define SCNoLEAST64 SCNo64
    #define SCNuLEAST64 SCNu64
    #define SCNxLEAST64 SCNx64
  #endif

  #define SCNdFAST8     SCNd32
  #define SCNiFAST8     SCNi32
  #define SCNoFAST8     SCNo32
  #define SCNuFAST8     SCNu32
  #define SCNxFAST8     SCNx32

  #define SCNdFAST16    SCNd32
  #define SCNiFAST16    SCNi32
  #define SCNoFAST16    SCNo32
  #define SCNuFAST16    SCNu32
  #define SCNxFAST16    SCNx32

  #define SCNdFAST32    SCNd32
  #define SCNiFAST32    SCNi32
  #define SCNoFAST32    SCNo32
  #define SCNuFAST32    SCNu32
  #define SCNxFAST32    SCNx32

  #if defined(__SCN_64_LENGTH_MODIFIER__)
    #define SCNdFAST64  SCNd64
    #define SCNiFAST64  SCNi64
    #define SCNoFAST64  SCNo64
    #define SCNuFAST64  SCNu64
    #define SCNxFAST64  SCNx64
  #endif

  #define SCNdPTR       SCNd32
  #define SCNiPTR       SCNi32
  #define SCNoPTR       SCNo32
  #define SCNuPTR       SCNu32
  #define SCNxPTR       SCNx32

  #if defined(__SCN_64_LENGTH_MODIFIER__)
    #define SCNdMAX     SCNd64
    #define SCNiMAX     SCNi64
    #define SCNoMAX     SCNo64
    #define SCNuMAX     SCNu64
    #define SCNxMAX     SCNx64
  #endif

#endif /* if C++, then __STDC_FORMAT_MACROS enables the above macros */

#if defined(__STDC_LIBRARY_SUPPORTED__)

  /* 7.8.2.1 */
  extern intmax_t imaxabs(intmax_t j);

  /* 7.8.2.2 */
  typedef struct {
        intmax_t quot;
        intmax_t rem;
  } imaxdiv_t;

  extern imaxdiv_t imaxdiv(intmax_t numer, intmax_t denom);

  /* 7.8.2.3 */
  extern intmax_t strtoimax(const char * restrict nptr, char ** restrict endptr, int base);
  extern uintmax_t strtoumax(const char * restrict nptr, char ** restrict endptr, int base);

  /* 7.8.2.4 */
  extern intmax_t wcstoimax(const wchar_t * restrict nptr, wchar_t ** restrict endptr, int base);
  extern uintmax_t wcstoumax(const wchar_t * restrict nptr, wchar_t ** restrict endptr, int base);

#endif

/*
   No need to #undef the __*_{8,64}_LENGTH_MODIFIER__ macros;
   in fact, you can't #undef them, because later uses of any of
   their dependents will *not* then do the intended substitution.
   Expansion of a #define like this one:

        #define x IDENT y

   uses the cpp value of IDENT at the location where x is *expanded*,
   not where it is #defined.
*/

#if defined(__RESTRICT_KEYWORD_DEFINED__)
  #undef restrict
  #undef __RESTRICT_KEYWORD_DEFINED__
#endif

#endif /* !_INTTYPES_H_ */