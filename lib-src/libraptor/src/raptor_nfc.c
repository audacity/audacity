/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_utf8.c - Raptor Unicode Normal Form C (NFC) support
 *
 * Copyright (C) 2004-2006, David Beckett http://purl.org/net/dajobe/
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
 * See Unicode Normalization http://unicode.org/unicode/reports/tr15/
 * for the definition of Unicode Normal Form C (NFC)
 * 
 */


#ifdef HAVE_CONFIG_H
#include <raptor_config.h>
#endif

#ifdef WIN32
#include <win32_raptor_config.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include <stdio.h>
#include <stdarg.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

/* Raptor includes */
#include "raptor.h"
#include "raptor_internal.h"
#include "raptor_nfc.h"


#undef RAPTOR_DEBUG_NFC_CHECK


/*
 * raptor_nfc_check_combiners - Check for allowed combining characters
 * @base: first character (U+0...U+FFFF)
 * @follow: second character (U+0...U+FFFF)
 *
 * Return value: non-0 if combination is allowed
 */
static int
raptor_nfc_check_combiners(u16 base, u16 follow)
{
  int low=0;
  int high=RAPTOR_NFC_RECOMBINERS_COUNT;

  while(low < high) {
    int middle=(low+high)/2;
    u16 middle_base=raptor_nfc_recombiners[middle].base;
    
    if(base == middle_base) {
      /* found base */
      u16 middle_follow=raptor_nfc_recombiners[middle].follow;
      if(middle_follow == follow) 
        /* success */
        return 1;

      /* otherwise need to binary search for further 'follow'
       * values for this base 
       */
      if(follow < middle_follow)
        high=middle;
      else
        low=middle+1;
    } else if(base < middle_base)
      high=middle;
    else
      low=middle+1;
  }

  return raptor_nfc_recombiners[low].base == base &&
         raptor_nfc_recombiners[low].follow == follow;
}



static int
raptor_nfc_get_class(unsigned long key)
{
  int low=0;
  int high=RAPTOR_NFC_CLASSES_COUNT;

  while (low < high) {
    int middle=(low+high)/2;
    unsigned int middle_key=raptor_nfc_classes[middle].key;

    /* found class */
    if(key == middle_key)
      return raptor_nfc_classes[middle].combining_class;

    /* otherwise need to binary search further */
    if(key < middle_key)
      high=middle;
    else
      low=middle+1;
  }

  return raptor_nfc_classes[low].combining_class;
}


static RAPTOR_INLINE raptor_nfc_code_flag
raptor_nfc_get_code_flag (unsigned long c)
{
  if(c < 0x10900)  {
    /* U+0 to U+108FF - from flags table (first 0x10900 entries) */
    if(c & 1)
      return (raptor_nfc_code_flag)(raptor_nfc_flags[c>>1] & 0xF);
    else
      return (raptor_nfc_code_flag)(raptor_nfc_flags[c>>1] >>4);
  } else if(c < 0x1D000)
    /* U+10900 to U+1CFFF - codes do not exist */
    return NoNo;
  else if(c < 0x1D800) {
    /* U+1D000 to U+1D7FF - from flags table (after first 0x10900) */
    c -= (0x1D000-0x10900);
    if(c & 1)
      return (raptor_nfc_code_flag)(raptor_nfc_flags[c>>1] & 0xF);
    else
      return (raptor_nfc_code_flag)(raptor_nfc_flags[c>>1] >>4);
  } else if(c < 0x20000)
    /* U+1D800 to U+1FFFF - codes do not exist */
    return NoNo;
  else if(c < 0x2A6D7)
    /* U+20000 to U+2A6D6 - CJK Ideograph Extension B  - simple */
    return simp;
  else if(c < 0x2F800)
    /* U+2A6D8 to U+2F7FF - codes do not exist */
    return NoNo;
  else if(c < 0x2FA1E)
    /* U+2F800 to U+2FA1D - CJK Compatibility Ideographs Supplement - forbidden/excluded in NFC */
    /* FIXME Unicode 4 says to 2FA1F */
    return NOFC;
  else if(c == 0xE0001)
    /* U+E0001 - "Language Tag" - simple */
    return simp;
  else if(c < 0xE0020)
    /* U+E0002 to U+E001F - codes do not exist */
    return NoNo;
  else if(c < 0xE0080)
    /* U+E0020 to U+E007F - Tag components - simple */
    return simp;
  else if(c < 0xE0100)
    /* U+E0080 to U+E00FF - codes do not exist */
    return NoNo;
  else if(c < 0xE01F0)
    /* U+E0100 to U+E01EF - Variation Selectors Supplement - simple */
    return simp;
  else
    /* otherwise does not exist/forbidden */
    return NoNo;
}



#ifdef RAPTOR_DEBUG_NFC_CHECK
#define RAPTOR_NFC_CHECK_FAIL(char, reason) do { fprintf(stderr, "%s:%d:%s: NFC check failed on U+%04lX: " reason "\n", __FILE__, __LINE__, __func__, char); } while(0)
#else
#define RAPTOR_NFC_CHECK_FAIL(char, reason)
#endif


/**
 * raptor_nfc_check:
 * @input: UTF-8 string
 * @length: length of string
 * @errorp: pointer to store offset of character in error (or NULL)
 *
 * Unicode Normal Form C (NFC) check function.
 *
 * If errorp is not NULL, it is set to the offset of the character
 * in error in the buffer, or <0 if there is no error.
 * 
 * Return value: Non 0 if the string is NFC
 **/
int
raptor_nfc_check (const unsigned char* string, size_t len, int *error)
{
  const unsigned char* start;
  int is_start;
  size_t offset;
  
  raptor_nfc_code_flag prev_char_flag=(raptor_nfc_code_flag)0;
  unsigned long prev_char=0L;
  int prev_class;
  
  start=string;
  is_start=1;
  offset=0;
  prev_class=0;

  while(offset < len) {
    raptor_unichar unichar;
    int unichar_len;
    int combining_class=0;
    raptor_nfc_code_flag flag;
    
    unichar_len=raptor_utf8_to_unicode_char(&unichar, string, len);
    if(unichar_len < 0 || unichar_len > (int)len) {
      /* UTF-8 encoding had an error or ended in the middle of a string */
      if(error)
        *error=offset;
      RAPTOR_NFC_CHECK_FAIL(unichar, "UTF-8 decoding error");
      return 0;
    }
    string += unichar_len; 
    offset += unichar_len;
    
    len -= unichar_len;

    flag = raptor_nfc_get_code_flag(unichar);

    switch (flag) {
      case HIGH:
      case loww:
      case NOFC:
        /* Forbidden combinations:
         * HIGH: high surrogate
         *
         * loww: low surrogate
         *
         * NOFC: Either singleton or excluded.  Exclusions are given in:
         *   http://www.unicode.org/unicode/reports/tr15/#Primary_Exclusion_List_Table
         *   http://www.unicode.org/Public/UNIDATA/CompositionExclusions.txt
         */
        if(error)
          *error=offset;
        RAPTOR_NFC_CHECK_FAIL(unichar, "forbidden combinations - HIGH, loww, NOFC");
        return 0;
        

      case NoNo:
        /* character does not exist */
        if(error)
          *error=offset;
        RAPTOR_NFC_CHECK_FAIL(unichar, "NoNo - character does not exist");
        return 0;


      case ReCo:
        /* class > 0 and recombining */

        /* class > 0 are forbidden at start */
        if(is_start) {
          if(error)
            *error=offset;
          RAPTOR_NFC_CHECK_FAIL(unichar, "ReCo at start");
          return 0;
        }
        combining_class=raptor_nfc_get_class(unichar);

        /* check 1 - previous class later than current, always an error */
        if(prev_class > combining_class) {
          if(error)
            *error=offset;
          RAPTOR_NFC_CHECK_FAIL(unichar, "ReCo and prev class > current");
          return 0;
        }
        
        /* check 2 - previous class same as current - always OK */

        /* check 3 - previous class earlier than current class.
         * Only perform combining check when both are in range
         */
        if(prev_class < combining_class && 
           prev_char < 0x10000 && unichar < 0x10000 &&
           raptor_nfc_check_combiners(prev_char, unichar)) {
          if(error)
            *error=offset;
          RAPTOR_NFC_CHECK_FAIL(unichar, "ReCo and combiners check failed");
          return 0;
        }

        break;


      case NoRe:
        /* class >0, not recombining */

        /* class > 0 are forbidden at start */
        if(is_start)  {
          if(error)
            *error=offset;
          RAPTOR_NFC_CHECK_FAIL(unichar, "NoRe at start");
          return 0;
        }

        combining_class=raptor_nfc_get_class(unichar);
        if(prev_class > combining_class) {
          if(error)
            *error=offset;
          RAPTOR_NFC_CHECK_FAIL(unichar, "NoRe and prev class > current");
          return 0;
        }

        break;


      case COM0:
        /* class is 0 and composing */

        /* Composing characters forbidden at start */
        if(is_start)  {
          if(error)
            *error=offset;
          RAPTOR_NFC_CHECK_FAIL(unichar, "COMB at start");
          return 0;
        }
        
        /* Only perform combining check when both are in range */
        if(prev_char < 0x10000 && unichar < 0x10000 && 
           raptor_nfc_check_combiners(prev_char, unichar)) {
          if(error)
            *error=offset;
          RAPTOR_NFC_CHECK_FAIL(unichar, "COMB and combiners check failed");
          return 0;
        }

        combining_class=0;
        break;


      case Hang:
        /* hangul Jamo (Korean) initial consonants */

        combining_class=0;
        break;


      case hAng:
        /* Hangul Jamo (Korean) medial vowels 
         * Must not be at the start and must not follow
         * a Hangul initial consonant
         */
        if(is_start || prev_char_flag == Hang) {
          if(error)
            *error=offset;
          RAPTOR_NFC_CHECK_FAIL(unichar, "hAng at start");
          return 0;
        }

        combining_class=0;
        break;


      case haNG:  
        /* hangul trailing consonants 
         * Must not be at the start and must not follow a
         * hangul initial/medial syllable
         */
        if(is_start || prev_char_flag == HAng) {
          if(error)
            *error=offset;
          RAPTOR_NFC_CHECK_FAIL(unichar, "haNG at start");
          return 0;
        }

        combining_class=0;
        break;


      case HAng:
        /* Hangul Jamo (Korean) initial/medial syllables */

        combining_class=0;
        break;


      case Base:
        /* base that combines */
      case simp:
        /* simple characters */

        combining_class=0;
        break;
    }

    prev_char=unichar;
    prev_char_flag=flag;
    prev_class=combining_class;

    is_start=0;
  }

  return 1;
}
