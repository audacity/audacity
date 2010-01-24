/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_utf8.c - RDF UTF8 / Unicode chars helper routines Implementation
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
#include <ctype.h> /* for isprint() */

#include <redland.h>
#include <rdf_utf8.h>


#ifndef STANDALONE

/* UTF-8 encoding of 32 bit Unicode chars
 *
 * Characters  0x00000000 to 0x0000007f are US-ASCII
 * Characters  0x00000080 to 0x000000ff are ISO Latin 1 (ISO 8859-1)
 *
 * incoming char| outgoing
 * bytes | bits | representation
 * ==================================================
 *     1 |    7 | 0xxxxxxx
 *     2 |   11 | 110xxxxx 10xxxxxx
 *     3 |   16 | 1110xxxx 10xxxxxx 10xxxxxx
 *     4 |   21 | 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
 *     5 |   26 | 111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
 *     6 |   31 | 1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
 *
 * The first byte is always in the range 0xC0-0xFD
 * Further bytes are all in the range 0x80-0xBF
 * No byte is ever 0xFE or 0xFF
 *
*/

/*
 * Unicode 3.0 Corrigendum #1: UTF-8 Shortest Form
 * http://www.unicode.org/versions/corrigendum1.html
 *
 * C12 
 *
 * (a) When a process generates data in a Unicode Transformation
 * Format, it shall not emit ill-formed code unit sequences.
 *
 * (b) When a process interprets data in a Unicode Transformation
 * Format, it shall treat illegal code unit sequences as an error
 * condition.
 *
 * (c) A conformant process shall not interpret illegal UTF code unit
 * sequences as characters.
 *
 * (d) Irregular UTF code unit sequences shall not be used for
 * encoding any other information.
 *
 *
 * My Summary: never encode non-shortest form UTF-8 sequences - they are
 * are illegal sequences.  Do not accept them on decoding.
 *
 *       Table 3.1B. Legal UTF-8 Byte Sequences
 *   Code Points         1st Byte  2nd Byte  3rd Byte  4th Byte
 *   U+0000..U+007F      00..7F
 *   U+0080..U+07FF      C2..DF    80..BF
 *   U+0800..U+0FFF      E0        A0..BF    80..BF
 *   U+1000..U+FFFF      E1..EF    80..BF    80..BF
 *   U+10000..U+3FFFF    F0        90..BF    80..BF    80..BF
 *   U+40000..U+FFFFF    F1..F3    80..BF    80..BF    80..BF
 *   U+100000..U+10FFFF  F4        80..8F    80..BF    80..BF
 *
 */


/**
 * librdf_unicode_char_to_utf8:
 * @c: Unicode character
 * @output: UTF-8 string buffer or NULL
 * @length: buffer size
 *
 * Convert a Unicode character to UTF-8 encoding.
 * 
 * If buffer is NULL, then will calculate the length rather than
 * perform it.  This can be used by the caller to allocate space
 * and then re-call this function with the new buffer.
 * 
 * Return value: bytes written to output buffer or <0 on failure
 **/
int
librdf_unicode_char_to_utf8(librdf_unichar c, byte *output, int length)
{
  int size=0;

  /* check for illegal code positions:
   * U+D800 to U+DFFF (UTF-16 surrogates)
   * U+FFFE and U+FFFF
   */
  if((c > 0xD7FF && c < 0xE000) || c == 0xFFFE || c == 0xFFFF)
    return -1;

  /* Unicode 3.2 only defines U+0000 to U+10FFFF and UTF-8 encodings of it */
  if(c > 0x10ffff)
    return -1;
  
  if      (c < 0x00000080)
    size=1;
  else if (c < 0x00000800)
    size=2;
  else if (c < 0x00010000)
    size=3;
  else
    size=4;

  /* when no buffer given, return size */
  if(!output)
    return size;

  if(size > length)
    return -1;
  
  switch(size) {
    case 4:
      output[3]=0x80 | (c & 0x3F);
      c= c >> 6;
       /* set bit 4 (bits 7,6,5,4 less 7,6,5 set below) on last byte */
      c |= 0x10000; /* 0x10000 = 0x10 << 12 */
      /* FALLTHROUGH */
    case 3:
      output[2]=0x80 | (c & 0x3F);
      c= c >> 6;
      /* set bit 5 (bits 7,6,5 less 7,6 set below) on last byte */
      c |= 0x800; /* 0x800 = 0x20 << 6 */
      /* FALLTHROUGH */
    case 2:
      output[1]=0x80 | (c & 0x3F);
      c= c >> 6;
      /* set bits 7,6 on last byte */
      c |= 0xc0; 
      /* FALLTHROUGH */
    case 1:
      output[0]=c;
  }

  return size;
}



/**
 * librdf_utf8_to_unicode_char:
 * @output: Pointer to the Unicode character or NULL
 * @input: UTF-8 string buffer
 * @length: buffer size
 *
 * Convert an UTF-8 encoded buffer to a Unicode character.
 * 
 * If output is NULL, then will calculate the number of bytes that
 * will be used from the input buffer and not perform the conversion.
 * 
 * Return value: bytes used from input buffer or <0 on failure
 **/
int
librdf_utf8_to_unicode_char(librdf_unichar *output, const byte *input, int length)
{
  byte in;
  int size;
  librdf_unichar c=0;
  
  if(length < 1)
    return -1;

  in=*input++;
  if((in & 0x80) == 0) { /* First byte 00..7F */
    size=1;
    c= in & 0x7f;
  } else if((in & 0xe0) == 0xc0) { /* First byte C0..DF */
    size=2;
    c= in & 0x1f;
  } else if((in & 0xf0) == 0xe0) { /* First byte E0..EF */
    size=3;
    c= in & 0x0f;
  } else if((in & 0xf8) == 0xf0) { /* First byte F0..F7 */
    size=4;
    c = in & 0x07;
  } else /* First byte anything else: 80..BF F8..FF - illegal */
    return -1;


  if(!output)
    return size;

  if(length < size)
    return -1;

  switch(size) {
    case 4:
      in=*input++ & 0x3f;
      c= c << 6;
      c |= in;
      /* FALLTHROUGH */
    case 3:
      in=*input++ & 0x3f;
      c= c << 6;
      c |= in;
      /* FALLTHROUGH */
    case 2:
      in=*input++ & 0x3f;
      c= c << 6;
      c |= in;
      /* FALLTHROUGH */
    default:
      break;
  }


  /* check for overlong UTF-8 sequences */
  switch(size) {
    case 2:
      if(c < 0x00000080)
        return -2;
      break;
    case 3:
      if(c < 0x00000800)
        return -2;
      break;
    case 4:
      if(c < 0x00010000)
        return -2;
      break;

    default: /* 1 */
      break;
  }


  /* check for illegal code positions:
   * U+D800 to U+DFFF (UTF-16 surrogates)
   * U+FFFE and U+FFFF
   */
  if((c > 0xD7FF && c < 0xE000) || c == 0xFFFE || c == 0xFFFF)
    return -1;

  /* Unicode 3.2 only defines U+0000 to U+10FFFF and UTF-8 encodings of it */
  /* of course this makes some 4 byte forms illegal */
  if(c > 0x10ffff)
    return -1;

  *output=c;

  return size;
}


/**
 * librdf_utf8_to_latin1:
 * @input: UTF-8 string buffer
 * @length: buffer size
 * @output_length: Pointer to variable to store resulting string length or NULL
 *
 * Convert a UTF-8 string to ISO Latin-1.
 * 
 * Converts the given UTF-8 string to the ISO Latin-1 subset of
 * Unicode (characters 0x00-0xff), discarding any out of range
 * characters.
 *
 * If the output_length pointer is not NULL, the returned string
 * length will be stored there.
 *
 * Return value: pointer to new ISO Latin-1 string or NULL on failure
 **/
byte*
librdf_utf8_to_latin1(const byte *input, int length, int *output_length)
{
  int utf8_char_length=0;
  int utf8_byte_length=0;
  int i;
  int j;
  byte *output;

  i=0;
  while(input[i]) {
    int size=librdf_utf8_to_unicode_char(NULL, &input[i], length-i);
    if(size <= 0)
      return NULL;
    utf8_char_length++;
    i+= size;
  }

  /* This is a maximal length; since chars may be discarded, the
   * actual length of the resulting can be shorter
   */
  utf8_byte_length=i;


  output=(byte*)LIBRDF_MALLOC(byte_string, utf8_byte_length+1);
  if(!output)
    return NULL;
  

  i=0; j=0;
  while(i < utf8_byte_length) {
    librdf_unichar c;
    int size=librdf_utf8_to_unicode_char(&c, &input[i], length-i);
    if(size <= 0)
      return NULL;
    if(c < 0x100) /* Discards characters! */
      output[j++]=c;
    i+= size;
  } 
  output[j]='\0';

  if(output_length)
    *output_length=j;
  
  return output;
}


/**
 * librdf_latin1_to_utf8:
 * @input: ISO Latin-1 string buffer
 * @length: buffer size
 * @output_length: Pointer to variable to store resulting string length or NULL
 *
 * Convert an ISO Latin-1 encoded string to UTF-8.
 * 
 * Converts the given ISO Latin-1 string to an UTF-8 encoded string
 * representing the same content.  This is lossless.
 * 
 * If the output_length pointer is not NULL, the returned string
 * length will be stored there.
 *
 * Return value: pointer to new UTF-8 string or NULL on failure
 **/
byte*
librdf_latin1_to_utf8(const byte *input, int length, int *output_length)
{
  int utf8_length=0;
  int i;
  int j;
  byte *output;

  for(i=0; input[i]; i++) {
    int size=librdf_unicode_char_to_utf8(input[i], NULL, length-i);
    if(size <= 0)
      return NULL;
    utf8_length += size;
  }

  output=(byte*)LIBRDF_MALLOC(byte_string, utf8_length+1);
  if(!output)
    return NULL;
  

  j=0;
  for(i=0; input[i]; i++) {
    int size=librdf_unicode_char_to_utf8(input[i], &output[j], length-i);
    if(size <= 0)
      return NULL;
    j+= size;
  } 
  output[j]='\0';

  if(output_length)
    *output_length=j;
  
  return output;
}


/**
 * librdf_utf8_print:
 * @input: UTF-8 string buffer
 * @length: buffer size
 * @stream: FILE* stream
 *
 * Print a UTF-8 string to a stream.
 * 
 * Pretty prints the UTF-8 string in a pseudo-C character
 * format like \u<emphasis>hex digits</emphasis> when the characters fail
 * the isprint() test.
 **/
void
librdf_utf8_print(const byte *input, int length, FILE *stream)
{
  int i=0;
  
  while(i<length && *input) {
    librdf_unichar c;
    int size=librdf_utf8_to_unicode_char(&c, input, length-i);
    if(size <= 0)
      return;
    if(c < 0x100) {
      if(isprint(c))
        fputc(c, stream);
      else
        fprintf(stream, "\\u%02X", c);
    } else if (c < 0x10000)
      fprintf(stream, "\\u%04X", c);
    else
      fprintf(stream, "\\U%08X", c);
    input += size;
    i += size;
  }
}

#endif


/* TEST CODE */


#ifdef STANDALONE

/* static prototypes */
void librdf_bad_string_print(const byte *input, int length, FILE *stream);
int main(int argc, char *argv[]);

void
librdf_bad_string_print(const byte *input, int length, FILE *stream)
{
  while(*input && length>0) {
    char c=*input;
    if(isprint(c))
      fputc(c, stream);
    else
      fprintf(stream, "\\x%02X", (c & 0xff));
    input++;
    length--;
  }
}


int
main(int argc, char *argv[]) 
{
  const char *program=librdf_basename((const char*)argv[0]);
  librdf_unichar c;
  struct tv {
    const byte *string;
    const int length;
    const librdf_unichar result;
  };
  struct tv *t;
  struct tv test_values[]={
    /* what is the capital of England? 'E' */
    {(const byte*)"E", 1, 'E'},
    /* latin small letter e with acute, U+00E9 ISOlat1 */
    {(const byte*)"\xc3\xa9", 2, 0xE9},
    /*  euro sign, U+20AC NEW */
    {(const byte*)"\xe2\x82\xac", 3, 0x20AC}, 
    /* unknown char - U+1FFFFF (21 bits) */

    /* First possible sequence of a certain length */
    {(const byte*)"\x00",                     1, 0x00000000},
    {(const byte*)"\xc2\x80",                 2, 0x00000080},
    {(const byte*)"\xe0\xa0\x80",             3, 0x00000800}, 
    {(const byte*)"\xf0\x90\x80\x80",         4, 0x00010000},

    /* Last possible sequence of a certain length */
    {(const byte*)"\x7f",                     1, 0x0000007F},
    {(const byte*)"\xdf\xbf",                 2, 0x000007FF},
    {(const byte*)"\xef\xbf\xbd",             3, 0x0000FFFD}, /*no FFFE-FFFF */
    {(const byte*)"\xf4\x8f\xbf\xbf",         4, 0x0010FFFF},

    /* Boundary conditions */
    {(const byte*)"\xed\x9f\xbf",     3, 0x0000D7FF},
    {(const byte*)"\xee\x80\x80",     3, 0x0000E000},
    {(const byte*)"\xef\xbf\xbd",     3, 0x0000FFFD}, 
    {(const byte*)"\xf4\x8f\xbf\xbf", 4, 0x0010FFFF},

    {NULL, 0, 0}
  };
  struct tv bad_test_values[]={
    /* Sequences that cannot appear in UTF-8 */
    {(const byte*)"\xfe",                     1, 0x000000FE},
    {(const byte*)"\xff",                     1, 0x000000FF},
    {(const byte*)"\xef\xbf\xbe",             3, 0x0000FFFE},
    {(const byte*)"\xef\xbf\xbf",             3, 0x0000FFFF},

    /* Minumum (ASCII NUL) overlong sequences */
    {(const byte*)"\xc0\x80",                 2, 0x00000000},
    {(const byte*)"\xe0\x80\x80",             3, 0x00000000},
    {(const byte*)"\xf0\x80\x80\x80",         4, 0x00000000},

    /* Maximum overlong sequences */
    {(const byte*)"\xc1\xbf",                 2, 0x0000007F},
    {(const byte*)"\xe0\x9f\xbf",             3, 0x000007FF},
    {(const byte*)"\xf0\x8f\xbf\xbf",         4, 0x0000FFFF},

    /* Beyond U+10FFFF */
    {(const byte*)"\xf4\x90\x80\x80",         4, 0x00110000},

    {NULL, 0, 0}
  };

  const byte test_utf8_string[]="Lib" "\xc3\xa9" "ration costs " "\xe2\x82\xac" "3.50";
  int test_utf8_string_length=strlen((const char*)test_utf8_string);
  const byte result_latin1_string[]="Lib" "\xe9" "ration costs 3.50";
  int result_latin1_string_length=strlen((const char*)result_latin1_string);
  const byte result_utf8_string[]="Lib" "\xc3\xa9" "ration costs 3.50";
  int result_utf8_string_length=strlen((const char*)result_utf8_string);
  
  int i;
  byte *latin1_string;
  int latin1_string_length;
  byte *utf8_string;
  int utf8_string_length;
  int failures=0;
  int verbose=0;

  for(i=0; (t=&test_values[i]) && t->string; i++) {
    int size;
    const byte *buffer=t->string;
    int length=t->length;
#define OUT_BUFFER_SIZE 6
    byte out_buffer[OUT_BUFFER_SIZE];
    
    size=librdf_utf8_to_unicode_char(&c, buffer, length);
    if(size < 0) {
      fprintf(stderr, "%s: librdf_utf8_to_unicode_char FAILED to convert UTF-8 string '", program);
      librdf_bad_string_print(buffer, length, stderr);
      fprintf(stderr, "' (length %d) to Unicode\n", length);
      failures++;
      continue;
    }
    if(c != t->result) {
      fprintf(stderr, "%s: librdf_utf8_to_unicode_char FAILED conversion of UTF-8 string '", program);
      librdf_bad_string_print(buffer, size, stderr);
      fprintf(stderr, "' to Unicode char U+%04X, expected U+%04X\n",
              (u32)c, (u32)t->result);
      failures++;
      continue;
    }

    if(verbose) {
      fprintf(stderr, "%s: librdf_utf8_to_unicode_char converted UTF-8 string '", program);
      librdf_utf8_print(buffer, size, stderr);
      fprintf(stderr, "' to Unicode char U+%04X correctly\n", (u32)c);
    }

    size=librdf_unicode_char_to_utf8(t->result, out_buffer, OUT_BUFFER_SIZE);
    if(size <= 0) {
      fprintf(stderr, "%s: librdf_unicode_char_to_utf8 FAILED to convert U+%04X to UTF-8 string\n", program, (u32)t->result);
      failures++;
      continue;
    }

    if(memcmp(out_buffer, buffer, length)) {
      fprintf(stderr, "%s: librdf_unicode_char_to_utf8 FAILED conversion U+%04X to UTF-8 - returned '", program, (u32)t->result);
      librdf_utf8_print(buffer, size, stderr);
      fputs("', expected '", stderr);
      librdf_utf8_print(out_buffer, t->length, stderr);
      fputs("'\n", stderr);
      failures++;
      continue;
    }
    
    if(verbose) {
      fprintf(stderr, "%s: librdf_unicode_char_to_utf8 converted U+%04X to UTF-8 string '", program, (u32)t->result);
      librdf_utf8_print(out_buffer, size, stderr);
      fputs("' correctly\n", stderr);
    }
  }


  /* Check for failures */
  for(i=0; (t=&bad_test_values[i]) && t->string; i++) {
    int size;
    const byte *buffer=t->string;
    int length=t->length;
    
    size=librdf_utf8_to_unicode_char(&c, buffer, length);
    if(size >= 0) {
      fprintf(stderr, "%s: librdf_utf8_to_unicode_char SUCCEEDED when it should have failed to convert UTF-8 string '", program);
      librdf_bad_string_print(buffer, length, stderr);
      fprintf(stderr, "' (length %d) to Unicode\n", length);
      failures++;
      continue;
    }
    if(verbose) {
      fprintf(stderr, "%s: librdf_utf8_to_unicode_char failed as expected converting bad UTF-8 string '", program);
      librdf_bad_string_print(buffer, length, stderr);
      fprintf(stderr, "' (length %d) to Unicode\n", length);
    }
  }
  


  latin1_string=librdf_utf8_to_latin1(test_utf8_string, 
                                      test_utf8_string_length,
                                      &latin1_string_length);
  if(!latin1_string) {
    fprintf(stderr, "%s: librdf_utf8_to_latin1 FAILED to convert UTF-8 string '", program);
    librdf_bad_string_print(test_utf8_string, test_utf8_string_length, stderr);
    fputs("' to Latin-1\n", stderr);
    failures++;
  }

  if(memcmp(latin1_string, result_latin1_string, result_latin1_string_length)) {
    fprintf(stderr, "%s: librdf_utf8_to_latin1 FAILED to convert UTF-8 string '", program);
    librdf_utf8_print(test_utf8_string, test_utf8_string_length, stderr);
    fprintf(stderr, "' to Latin-1 - returned '%s' but expected '%s'\n",
            latin1_string, result_latin1_string);
    failures++;
  }

  if(verbose) {
    fprintf(stderr, "%s: librdf_utf8_to_latin1 converted UTF-8 string '",
            program);
    librdf_utf8_print(test_utf8_string, test_utf8_string_length, stderr);
    fprintf(stderr, "' to Latin-1 string '%s' OK\n", latin1_string);
  }
  

  utf8_string=librdf_latin1_to_utf8(latin1_string, latin1_string_length,
                                    &utf8_string_length);
  if(!utf8_string) {
    fprintf(stderr, "%s: librdf_latin1_to_utf8 FAILED to convert Latin-1 string '%s' to UTF-8\n", program, latin1_string);
    failures++;
  }

  if(memcmp(utf8_string, result_utf8_string, result_utf8_string_length)) {
    fprintf(stderr, "%s: librdf_latin1_to_utf8 FAILED to convert Latin-1 string '%s' to UTF-8 - returned '", program, latin1_string);
    librdf_utf8_print(utf8_string, utf8_string_length, stderr);
    fputs("' but expected '", stderr);
    librdf_utf8_print(result_utf8_string, result_utf8_string_length, stderr);
    fputs("'\n", stderr);
    failures++;
  }

  if(verbose) {
    fprintf(stderr, "%s: librdf_latin1_to_utf8 converted Latin-1 string '%s' to UTF-8 string '", program, latin1_string);
    librdf_utf8_print(utf8_string, utf8_string_length, stderr);
    fputs("' OK\n", stderr);
  }

  LIBRDF_FREE(cstring, latin1_string);
  LIBRDF_FREE(cstring, utf8_string);

#ifdef LIBRDF_MEMORY_DEBUG 
  librdf_memory_report(stderr);
#endif
 
  return failures;
}

#endif
