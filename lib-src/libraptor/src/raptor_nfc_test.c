/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_nfc_test.c - Raptor Unicode NFC validation check
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
 */


#ifdef HAVE_CONFIG_H
#include <raptor_config.h>
#endif

#ifdef WIN32
#include <win32_raptor_config.h>
#endif

#include <stdio.h>
#include <string.h>
#include <ctype.h> /* for isprint() */
#include <stdarg.h>
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

/* Raptor includes */
#include "raptor.h"
#include "raptor_internal.h"
#include "raptor_nfc.h"


#undef RAPTOR_NFC_DECODE_DEBUG


/*
 * decode_to_utf8:
 * @utf8_string: destination utf8 buffer (FIXME big enough!)
 * @unicode_string: first char of string
 * @end: last char of unicode_string
 */
static int
decode_to_utf8(unsigned char *utf8_string, size_t utf8_string_length,
               const char *unicode_string, const char *end)
{
  unsigned char *u=utf8_string;
  const char *p=unicode_string;
  
#ifdef RAPTOR_NFC_DECODE_DEBUG
  fputs("decode_to_utf8: string '", stderr);
  (void)fwrite(unicode_string, sizeof(char), (end-unicode_string)+1, stderr);
  fputs("' converts to:\n  ", stderr);
#endif

  while(p < end) {
    unsigned long c=0;
    char *endptr;

    if(*p == ' ') {
      p++;
      continue;
    }
    
    c=(unsigned long)strtol(p, &endptr, 16);

#ifdef RAPTOR_NFC_DECODE_DEBUG
    fprintf(stderr, "U+%04lX ", c);
#endif

    p=(unsigned char*)endptr;
    
    u+= raptor_unicode_char_to_utf8(c, u);
    
    if((u-utf8_string) > utf8_string_length) {
      fprintf(stderr,
              "decode_to_utf8 overwote utf8_string buffer at byte %d\n",
              (u-utf8_string));
      abort();
    }
  }

#ifdef RAPTOR_NFC_DECODE_DEBUG
  fputs("\n", stderr);
#endif

  return u-utf8_string;
}



static void
utf8_print(const unsigned char *input, int length, FILE *stream)
{
  int i=0;
  
  while(i<length && *input) {
    unsigned long c;
    int size=raptor_utf8_to_unicode_char(&c, input, length-i);
    if(size <= 0)
      return;
    if(i)
      fputc(' ', stream);
    fprintf(stream, "U+%04X", (int)c);
    input += size;
    i += size;
  }
}


int
main (int argc, char *argv[]) 
{
  const char *program=raptor_basename(argv[0]);
  static const char *filename="NormalizationTest.txt";
  FILE *fh;
  int rc=0;
  unsigned int line=1;
  size_t max_c2_len=0;
  size_t max_c4_len=0;
  int passes=0;
  int fails=0;

  fh=fopen(filename, "r");
  if(!fh) {
    fprintf(stderr, "%s: file '%s' open failed - %s\n",
            program, filename, strerror(errno));
    return 1;
  }

#define LINE_BUFFER_SIZE 1024

/* FIXME big enough for Unicode 4 (c2 max 16; c4 max 33) */
#define UNISTR_SIZE 40

  for(;!feof(fh); line++) {
    char buffer[LINE_BUFFER_SIZE];
    char *p, *start;
    unsigned char nfc1[UNISTR_SIZE];
    unsigned char nfc2[UNISTR_SIZE];
    size_t nfc1_len, nfc2_len;
    int nfc_rc;
    int error;
    
    p=fgets(buffer, LINE_BUFFER_SIZE, fh);
    if(!p) {
      if(ferror(fh)) {
        fprintf(stderr, "%s: file '%s' read failed - %s\n",
                program, filename, strerror(errno));
        rc=1;
        break;
      }
      /* assume feof */
      break;
    };

#if 0
    fprintf(stderr, "%s:%d: line '%s'\n", program, line, buffer);
#endif

    /* skip lines */
    if(*p == '@' || *p == '#')
      continue;
    
    /* skip column 1 */
    while(*p++ != ';')
      ;
      
    start=p;
    /* find end column 2 */
    while(*p++ != ';')
      ;

    nfc1_len=decode_to_utf8(nfc1, UNISTR_SIZE, start, p-1);
    if(nfc1_len > max_c2_len)
      max_c2_len=nfc1_len;
    
    /* skip column 3 */
    while(*p++ != ';')
      ;

    start=p;
    /* find end column 4 */
    while(*p++ != ';')
      ;

    nfc2_len=decode_to_utf8(nfc2, UNISTR_SIZE, start, p-1);
    if(nfc2_len > max_c4_len)
      max_c4_len=nfc2_len;

    if(!raptor_utf8_check(nfc1, nfc1_len)) {
      fprintf(stderr, "%s:%d: UTF8 check 1 failed on: '", filename, line);
      utf8_print(nfc1, nfc1_len, stderr);
      fputs("'\n", stderr);
      fails++;
    } else
      passes++;

    nfc_rc=raptor_nfc_check(nfc1, nfc1_len, &error);
    if(!nfc_rc) {
      fprintf(stderr, "%s:%d: NFC check 1 failed on: '", filename, line);
      utf8_print(nfc1, nfc1_len, stderr);
      fprintf(stderr, "' at byte %d of %d\n", error, (int)nfc1_len);
      fails++;
    } else
      passes++;

    if(nfc1_len == nfc2_len && !memcmp(nfc1, nfc2, nfc1_len))
      continue;

    if(!raptor_utf8_check(nfc2, nfc2_len)) {
      fprintf(stderr, "%s:%d: UTF8 check 2 failed on: '", filename, line);
      utf8_print(nfc2, nfc2_len, stderr);
      fputs("'\n", stderr);
      fails++;
    } else
      passes++;

    nfc_rc=raptor_nfc_check(nfc2, nfc2_len, &error);
    if(!nfc_rc) {
      fprintf(stderr, "%s:%d: NFC check 2 failed on: '", filename, line);
      utf8_print(nfc2, nfc2_len, stderr);
      fprintf(stderr, "' at byte %d of %d\n", error, (int)nfc2_len);
      fails++;
    } else
      passes++;
    
  }

  fclose(fh);

  fprintf(stderr, "%s: max c2 len: %d,  max c4 len: %d\n", program,
          (int)max_c2_len, (int)max_c4_len);
  fprintf(stderr, "%s: passes: %d fails: %d\n", program,
          passes, fails);

  return rc;
}
