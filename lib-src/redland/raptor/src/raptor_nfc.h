/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_nfc.h - Raptor Unicode NFC headers
 *
 * Copyright (C) 2004-2006, David Beckett http://www.dajobe.org/
 * Copyright (C) 2004-2004, University of Bristol, UK http://www.bristol.ac.uk/
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


#include <sys/types.h>

#ifndef HAVE_U16
  #if SIZEOF_UNSIGNED_SHORT == 2
    typedef unsigned int u16;
  #elif SIZEOF_UNSIGNED_INT == 2
    typedef unsigned long u16;
  #else
    #error u16 type not defined
  #endif
#endif


#ifndef HAVE_U8
  #if SIZEOF_UNSIGNED_CHAR == 1
    typedef unsigned char u8;
  #else
    #error u8 type not defined
  #endif
#endif

/*
 * Definitions for Unicode NFC data tables
 *
 * See Unicode Normalization http://unicode.org/unicode/reports/tr15/
 * for the definition of Unicode Normal Form C (NFC)
 */


/* Unicode combining classes
 *
 * The combining class is taken from the 4th field of UnicodeData.txt
 * and are mostly class 0 - nothing special.  This structure
 * is used to make a sparse sequence of (key, class) pairs
 * ordered by key, of the non-0 class entries.
 *
 */
typedef struct 
{
  /* the code (0.. 0x10FFD inclusive - 24 bits) */
  unsigned int key:24;
 /* the combining class (0.. 255 - 8 bits is enough, there are ~50-60 used) */
  unsigned int combining_class:8;
} raptor_nfc_key_class;


/* Unicode combining characters
 *
 * Pairs of characters (base, follow) that must be in that order
 * They are all 0..0xFFFF inclusive
 *
 * This structure is used to make a sparse sequence of (base, follow)
 * pairs of valid combinations. 'base' may have several valid 'follow's in
 * the sequence.
 */
typedef struct
{
  u16 base;
  u16 follow;
}  raptor_nfc_base_follow;


/*
 * Flags for codes U+0 to U+108FF, U+1D000 to U+1D7FF
 */

typedef enum {
  HIGH,  /* U+D800 to U+DBFF High Surrogates */
  loww,  /* U+DC00 to U+DFFF Low Surrogates */
  NoNo,  /* code that does not exist */
  NOFC,  /* forbidden or excluded in NFC */
  ReCo,  /* class > 0 recombining */
  NoRe,  /* class > 0 not recombining */
  COM0,  /* class 0 and composing */
  Hang,  /* U+1100 to U+1112 - Hangul Jamo (Korean) initial consonants */
  hAng,  /* U+1161 to U+1175 - Hangul Jamo (Korean) medial vowels */
  haNG,  /* U+11A8 to U+11C2 - Hangul Jamo (Korean) trailing consonants */
  HAng,  /* U+AC00 to U+D7A3 (except for every 28) - Hangul syllables */
  Base,  /* base that combines */
  simp   /* class 0 nothing special */
} raptor_nfc_code_flag;


#define RAPTOR_NFC_CLASSES_COUNT 352
extern const raptor_nfc_key_class raptor_nfc_classes[RAPTOR_NFC_CLASSES_COUNT];

#define RAPTOR_NFC_RECOMBINERS_COUNT 2177
extern const raptor_nfc_base_follow raptor_nfc_recombiners[RAPTOR_NFC_RECOMBINERS_COUNT];

#define RAPTOR_NFC_CODE_FLAGS_COUNT 34944
extern const u8 raptor_nfc_flags[RAPTOR_NFC_CODE_FLAGS_COUNT];
