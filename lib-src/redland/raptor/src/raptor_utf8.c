/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_utf8.c - Raptor UTF-8 and Unicode support
 *
 * Copyright (C) 2002-2007, David Beckett http://www.dajobe.org/
 * Copyright (C) 2002-2004, University of Bristol, UK http://www.bristol.ac.uk/
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
#include <stdarg.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

/* Raptor includes */
#include "raptor.h"
#include "raptor_internal.h"
#ifdef RAPTOR_NFC_CHECK
#include "raptor_nfc.h"
#endif


/**
 * raptor_unicode_char_to_utf8:
 * @c: Unicode character
 * @output: UTF-8 string buffer or NULL
 *
 * Convert a Unicode character to UTF-8 encoding.
 * 
 * Based on librdf_unicode_char_to_utf8() with no need to calculate
 * length since the encoded character is always copied into a buffer
 * with sufficient size.
 * 
 * Return value: bytes encoded to output buffer or <0 on failure
 **/
int
raptor_unicode_char_to_utf8(raptor_unichar c, unsigned char *output)
{
  int size=0;
  
  if      (c < 0x00000080)
    size=1;
  else if (c < 0x00000800)
    size=2;
  else if (c < 0x00010000)
    size=3;
  else if (c < 0x00200000)
    size=4;
  else if (c < 0x04000000)
    size=5;
  else if (c < 0x80000000)
    size=6;
  else
    return -1;

  switch(size) {
    case 6:
      output[5]=0x80 | (unsigned char)(c & 0x3F);
      c= c >> 6;
       /* set bit 2 (bits 7,6,5,4,3,2 less 7,6,5,4,3 set below) on last byte */
      c |= 0x4000000; /* 0x10000 = 0x04 << 24 */
      /* FALLTHROUGH */
    case 5:
      output[4]=0x80 | (unsigned char)(c & 0x3F);
      c= c >> 6;
       /* set bit 3 (bits 7,6,5,4,3 less 7,6,5,4 set below) on last byte */
      c |= 0x200000; /* 0x10000 = 0x08 << 18 */
      /* FALLTHROUGH */
    case 4:
      output[3]=0x80 | (unsigned char)(c & 0x3F);
      c= c >> 6;
       /* set bit 4 (bits 7,6,5,4 less 7,6,5 set below) on last byte */
      c |= 0x10000; /* 0x10000 = 0x10 << 12 */
      /* FALLTHROUGH */
    case 3:
      output[2]=0x80 | (unsigned char)(c & 0x3F);
      c= c >> 6;
      /* set bit 5 (bits 7,6,5 less 7,6 set below) on last byte */
      c |= 0x800; /* 0x800 = 0x20 << 6 */
      /* FALLTHROUGH */
    case 2:
      output[1]=0x80 | (unsigned char)(c & 0x3F);
      c= c >> 6;
      /* set bits 7,6 on last byte */
      c |= 0xc0; 
      /* FALLTHROUGH */
    case 1:
      output[0]=(unsigned char)c;
  }

  return size;
}


/**
 * raptor_utf8_to_unicode_char:
 * @output: Pointer to the Unicode character or NULL
 * @input: UTF-8 string buffer
 * @length: buffer size
 *
 * Convert an UTF-8 encoded buffer to a Unicode character.
 * 
 * If output is NULL, then will calculate the number of bytes that
 * will be used from the input buffer and not perform the conversion.
 * 
 * Return value: bytes used from input buffer or <0 on failure: -1 input buffer too short or length error, -2 overlong UTF-8 sequence, -3 illegal code positions, -4 code out of range U+0000 to U+10FFFF.  In cases -2, -3 and -4 the coded character is stored in the output.
 */
int
raptor_utf8_to_unicode_char(raptor_unichar *output,
                            const unsigned char *input, int length)
{
  unsigned char in;
  int size;
  raptor_unichar c=0;
  
  if(length < 1)
    return -1;

  in=*input++;
  if((in & 0x80) == 0) {
    size=1;
    c= in & 0x7f;
  } else if((in & 0xe0) == 0xc0) {
    size=2;
    c= in & 0x1f;
  } else if((in & 0xf0) == 0xe0) {
    size=3;
    c= in & 0x0f;
  } else if((in & 0xf8) == 0xf0) {
    size=4;
    c = in & 0x07;
  } else if((in & 0xfc) == 0xf8) {
    size=5;
    c = in & 0x03;
  } else if((in & 0xfe) == 0xfc) {
    size=6;
    c = in & 0x01;
  } else
    return -1;


  if(!output)
    return size;

  if(length < size)
    return -1;

  switch(size) {
    case 6:
      in=*input++ & 0x3f;
      c= c << 6;
      c |= in;
      /* FALLTHROUGH */
    case 5:
      in=*input++ & 0x3f;
      c= c << 6;
      c |= in;
      /* FALLTHROUGH */
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
  
  *output=c;

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
    return -3;

  /* Unicode 3.2 only defines U+0000 to U+10FFFF and UTF-8 encodings of it */
  /* of course this makes some 4 byte forms illegal */
  if(c > 0x10ffff)
    return -4;

  return size;
}


static int raptor_unicode_is_letter(long c);
static int raptor_unicode_is_basechar(long c);
static int raptor_unicode_is_ideographic(long c);
static int raptor_unicode_is_combiningchar(long c);
static int raptor_unicode_is_digit(long c);
static int raptor_unicode_is_extender(long c);


/**
 * raptor_unicode_is_xml11_namestartchar:
 * @c: Unicode character to check
 * 
 * Check if Unicode character is legal to start an XML 1.1 Name
 * 
 * Namespaces in XML 1.1 REC 2004-02-04
 *   http://www.w3.org/TR/2004/REC-xml11-20040204/#NT-NameStartChar
 * updating
 *   Extensible Markup Language (XML) 1.1 REC 2004-02-04
 *   http://www.w3.org/TR/2004/REC-xml11-20040204/ sec 2.3, [4a]
 * excluding the ':'
 *
 * Return value: non-0 if legal
 **/
int
raptor_unicode_is_xml11_namestartchar(raptor_unichar c)
{
  return (((c >= 0x0041)  && (c <= 0x005A)) || /* [A-Z] */
          (c == 0x005F) ||                     /* '_' */
          ((c >= 0x0061)  && (c <= 0x007A)) || /* [a-z] */
          ((c >= 0x00C0)  && (c <= 0x00D6)) ||
          ((c >= 0x00D8)  && (c <= 0x00F6)) ||
          ((c >= 0x00F8)  && (c <= 0x02FF)) ||
          ((c >= 0x0370)  && (c <= 0x037D)) ||
          ((c >= 0x037F)  && (c <= 0x1FFF)) ||
          ((c >= 0x200C)  && (c <= 0x200D)) ||
          ((c >= 0x2070)  && (c <= 0x218F)) ||
          ((c >= 0x2C00)  && (c <= 0x2FEF)) ||
          ((c >= 0x3001)  && (c <= 0xD7FF)) ||
          ((c >= 0xF900)  && (c <= 0xFDCF)) ||
          ((c >= 0xFDF0)  && (c <= 0xFFFD)) ||
          ((c >= 0x10000) && (c <= 0xEFFFF)));
}


/**
 * raptor_unicode_is_xml10_namestartchar:
 * @c: Unicode character to check
 *
 * Check if Unicode character is legal to start an XML 1.0 Name
 * 
 * Namespaces in XML REC 1999-01-14
 *   http://www.w3.org/TR/1999/REC-xml-names-19990114/#NT-NCName
 * updating
 *   Extensible Markup Language (XML) 1.0 (Third Edition) REC 2004-02-04
 *   http://www.w3.org/TR/2004/REC-xml-20040204/
 * excluding the ':'
 *
 * Return value: non-0 if legal
 **/
int
raptor_unicode_is_xml10_namestartchar(raptor_unichar c)
{
  return (raptor_unicode_is_letter(c) ||
          (c == '_'));
}


/**
 * raptor_unicode_is_namestartchar:
 * @c: Unicode character to check
 *
 * Check if Unicode character is legal to start an XML Name
 * 
 * Return value: non-0 if the character is legal
 **/
int
raptor_unicode_is_namestartchar(raptor_unichar c) {
#ifdef RAPTOR_XML_1_1
   return raptor_unicode_is_xml11_namestartchar(c);
#else
   return raptor_unicode_is_xml10_namestartchar(c);
#endif
}


/**
 * raptor_unicode_is_xml11_namechar:
 * @c: Unicode character
 * 
 * Check if a Unicode codepoint is a legal to continue an XML 1.1 Name
 *
 * Namespaces in XML 1.1 REC 2004-02-04
 *   http://www.w3.org/TR/2004/REC-xml11-20040204/
 * updating
 *   Extensible Markup Language (XML) 1.1 REC 2004-02-04
 *   http://www.w3.org/TR/2004/REC-xml11-20040204/ sec 2.3, [4a]
 * excluding the ':'
 * 
 * Return value: non-0 if legal
 **/
int
raptor_unicode_is_xml11_namechar(raptor_unichar c)
{
  return (raptor_unicode_is_xml11_namestartchar(c) ||
          (c == 0x002D) || /* '-' */
          (c == 0x002E) || /* '.' */
          (c >= 0x0030 && c <= 0x0039) || /* 0-9 */
          (c == 0x00B7) ||
          (c >= 0x0300 && c <=0x036F) ||
          (c >= 0x203F && c <=0x2040));
}


/**
 * raptor_unicode_is_xml10_namechar:
 * @c: Unicode character
 * 
 * Check if a Unicode codepoint is a legal to continue an XML 1.0 Name
 * 
 * Namespaces in XML REC 1999-01-14
 *   http://www.w3.org/TR/1999/REC-xml-names-19990114/#NT-NCNameChar
 * updating
 *   Extensible Markup Language (XML) 1.0 (Third Edition) REC 2004-02-04
 *   http://www.w3.org/TR/2004/REC-xml-20040204/
 * excluding the ':'
 *
 * Return value: non-0 if legal
 **/
int
raptor_unicode_is_xml10_namechar(raptor_unichar c)
{
  return (raptor_unicode_is_letter(c) ||
          raptor_unicode_is_digit(c) ||
          (c == 0x002E) || /* '.' */
          (c == 0x002D) || /* '-' */
          (c == 0x005F) || /* '_' */
          raptor_unicode_is_combiningchar(c) ||
          raptor_unicode_is_extender(c));
}
 

/**
 * raptor_unicode_is_namechar:
 * @c: Unicode character to check
 *
 * Check if Unicode character is legal to continue an XML Name .
 * 
 * Return value: non-0 if the character is legal
 **/
int
raptor_unicode_is_namechar(raptor_unichar c) 
{
#ifdef RAPTOR_XML_1_1
   return raptor_unicode_is_xml11_namechar(c);
#else
   return raptor_unicode_is_xml10_namechar(c);
#endif
}


/*
 * All this below was derived by machine-transforming the classes in Appendix B
 * of http://www.w3.org/TR/2000/REC-xml-20001006
 */

static int
raptor_unicode_is_letter(long c)
{
  return(raptor_unicode_is_basechar(c) ||
         raptor_unicode_is_ideographic(c));
}


static int
raptor_unicode_is_basechar(long c)
{
  /* http://www.w3.org/TR/2000/REC-xml-20001006#NT-BaseChar */
  return((c >= 0x0041 && c <= 0x005A ) ||
         (c >= 0x0061 && c <= 0x007A ) ||
         (c >= 0x00C0 && c <= 0x00D6 ) ||
         (c >= 0x00D8 && c <= 0x00F6 ) ||
         (c >= 0x00F8 && c <= 0x00FF ) ||
         (c >= 0x0100 && c <= 0x0131 ) ||
         (c >= 0x0134 && c <= 0x013E ) ||
         (c >= 0x0141 && c <= 0x0148 ) ||
         (c >= 0x014A && c <= 0x017E ) ||
         (c >= 0x0180 && c <= 0x01C3 ) ||
         (c >= 0x01CD && c <= 0x01F0 ) ||
         (c >= 0x01F4 && c <= 0x01F5 ) || 
         (c >= 0x01FA && c <= 0x0217 ) ||
         (c >= 0x0250 && c <= 0x02A8 ) ||
         (c >= 0x02BB && c <= 0x02C1 ) ||
         (c == 0x0386) || 
         (c >= 0x0388 && c <= 0x038A ) ||
         (c == 0x038C) ||
         (c >= 0x038E && c <= 0x03A1 ) ||
         (c >= 0x03A3 && c <= 0x03CE ) ||
         (c >= 0x03D0 && c <= 0x03D6 ) ||
         (c == 0x03DA) ||
         (c == 0x03DC) ||
         (c == 0x03DE) ||
         (c == 0x03E0) ||
         (c >= 0x03E2 && c <= 0x03F3 ) ||
         (c >= 0x0401 && c <= 0x040C ) ||
         (c >= 0x040E && c <= 0x044F ) ||
         (c >= 0x0451 && c <= 0x045C ) ||
         (c >= 0x045E && c <= 0x0481 ) ||
         (c >= 0x0490 && c <= 0x04C4 ) ||
         (c >= 0x04C7 && c <= 0x04C8 ) ||
         (c >= 0x04CB && c <= 0x04CC ) ||
         (c >= 0x04D0 && c <= 0x04EB ) ||
         (c >= 0x04EE && c <= 0x04F5 ) ||
         (c >= 0x04F8 && c <= 0x04F9 ) ||
         (c >= 0x0531 && c <= 0x0556 ) ||
         (c == 0x0559) ||
         (c >= 0x0561 && c <= 0x0586 ) ||
         (c >= 0x05D0 && c <= 0x05EA ) ||
         (c >= 0x05F0 && c <= 0x05F2 ) ||
         (c >= 0x0621 && c <= 0x063A ) ||
         (c >= 0x0641 && c <= 0x064A ) ||
         (c >= 0x0671 && c <= 0x06B7 ) ||
         (c >= 0x06BA && c <= 0x06BE ) ||
         (c >= 0x06C0 && c <= 0x06CE ) ||
         (c >= 0x06D0 && c <= 0x06D3 ) ||
         (c == 0x06D5) ||
         (c >= 0x06E5 && c <= 0x06E6 ) ||
         (c >= 0x0905 && c <= 0x0939 ) ||
         (c == 0x093D) ||
         (c >= 0x0958 && c <= 0x0961 ) ||
         (c >= 0x0985 && c <= 0x098C ) ||
         (c >= 0x098F && c <= 0x0990 ) ||
         (c >= 0x0993 && c <= 0x09A8 ) ||
         (c >= 0x09AA && c <= 0x09B0 ) ||
         (c == 0x09B2) ||
         (c >= 0x09B6 && c <= 0x09B9 ) ||
         (c >= 0x09DC && c <= 0x09DD ) ||
         (c >= 0x09DF && c <= 0x09E1 ) ||
         (c >= 0x09F0 && c <= 0x09F1 ) ||
         (c >= 0x0A05 && c <= 0x0A0A ) ||
         (c >= 0x0A0F && c <= 0x0A10 ) ||
         (c >= 0x0A13 && c <= 0x0A28 ) ||
         (c >= 0x0A2A && c <= 0x0A30 ) ||
         (c >= 0x0A32 && c <= 0x0A33 ) ||
         (c >= 0x0A35 && c <= 0x0A36 ) ||
         (c >= 0x0A38 && c <= 0x0A39 ) ||
         (c >= 0x0A59 && c <= 0x0A5C ) || 
         (c == 0x0A5E) ||
         (c >= 0x0A72 && c <= 0x0A74 ) ||
         (c >= 0x0A85 && c <= 0x0A8B ) ||
         (c == 0x0A8D) ||
	 (c >= 0x0A8F && c <= 0x0A91 ) ||
         (c >= 0x0A93 && c <= 0x0AA8 ) ||
         (c >= 0x0AAA && c <= 0x0AB0 ) ||
         (c >= 0x0AB2 && c <= 0x0AB3 ) ||
	 (c >= 0x0AB5 && c <= 0x0AB9 ) ||
         (c == 0x0ABD) ||
         (c == 0x0AE0) ||
         (c >= 0x0B05 && c <= 0x0B0C ) ||
	 (c >= 0x0B0F && c <= 0x0B10 ) ||
         (c >= 0x0B13 && c <= 0x0B28 ) ||
         (c >= 0x0B2A && c <= 0x0B30 ) ||
         (c >= 0x0B32 && c <= 0x0B33 ) ||
	 (c >= 0x0B36 && c <= 0x0B39 ) ||
         (c == 0x0B3D) ||
         (c >= 0x0B5C && c <= 0x0B5D ) ||
         (c >= 0x0B5F && c <= 0x0B61 ) ||
	 (c >= 0x0B85 && c <= 0x0B8A ) ||
         (c >= 0x0B8E && c <= 0x0B90 ) ||
         (c >= 0x0B92 && c <= 0x0B95 ) ||
         (c >= 0x0B99 && c <= 0x0B9A ) ||
	 (c == 0x0B9C) ||
         (c >= 0x0B9E && c <= 0x0B9F ) ||
         (c >= 0x0BA3 && c <= 0x0BA4 ) ||
         (c >= 0x0BA8 && c <= 0x0BAA ) ||
	 (c >= 0x0BAE && c <= 0x0BB5 ) ||
         (c >= 0x0BB7 && c <= 0x0BB9 ) ||
         (c >= 0x0C05 && c <= 0x0C0C ) ||
         (c >= 0x0C0E && c <= 0x0C10 ) ||
	 (c >= 0x0C12 && c <= 0x0C28 ) ||
         (c >= 0x0C2A && c <= 0x0C33 ) ||
         (c >= 0x0C35 && c <= 0x0C39 ) ||
         (c >= 0x0C60 && c <= 0x0C61 ) ||
	 (c >= 0x0C85 && c <= 0x0C8C ) ||
         (c >= 0x0C8E && c <= 0x0C90 ) ||
         (c >= 0x0C92 && c <= 0x0CA8 ) ||
         (c >= 0x0CAA && c <= 0x0CB3 ) ||
	 (c >= 0x0CB5 && c <= 0x0CB9 ) ||
         (c == 0x0CDE) ||
         (c >= 0x0CE0 && c <= 0x0CE1 ) ||
         (c >= 0x0D05 && c <= 0x0D0C ) ||
	 (c >= 0x0D0E && c <= 0x0D10 ) ||
         (c >= 0x0D12 && c <= 0x0D28 ) ||
         (c >= 0x0D2A && c <= 0x0D39 ) ||
         (c >= 0x0D60 && c <= 0x0D61 ) ||
	 (c >= 0x0E01 && c <= 0x0E2E ) ||
         (c == 0x0E30) ||
         (c >= 0x0E32 && c <= 0x0E33 ) ||
         (c >= 0x0E40 && c <= 0x0E45 ) ||
	 (c >= 0x0E81 && c <= 0x0E82 ) ||
         (c == 0x0E84) ||
         (c >= 0x0E87 && c <= 0x0E88 ) ||
         (c == 0x0E8A) ||
	 (c == 0x0E8D) ||
         (c >= 0x0E94 && c <= 0x0E97 ) ||
         (c >= 0x0E99 && c <= 0x0E9F ) ||
         (c >= 0x0EA1 && c <= 0x0EA3 ) ||
	 (c == 0x0EA5) ||
         (c == 0x0EA7) ||
         (c >= 0x0EAA && c <= 0x0EAB ) ||
         (c >= 0x0EAD && c <= 0x0EAE ) ||
	 (c == 0x0EB0) ||
         (c >= 0x0EB2 && c <= 0x0EB3 ) ||
         (c == 0x0EBD) ||
         (c >= 0x0EC0 && c <= 0x0EC4 ) ||
	 (c >= 0x0F40 && c <= 0x0F47 ) ||
         (c >= 0x0F49 && c <= 0x0F69 ) ||
         (c >= 0x10A0 && c <= 0x10C5 ) ||
         (c >= 0x10D0 && c <= 0x10F6 ) ||
	 (c == 0x1100) ||
         (c >= 0x1102 && c <= 0x1103 ) ||
         (c >= 0x1105 && c <= 0x1107 ) ||
         (c == 0x1109) ||
         (c >= 0x110B && c <= 0x110C ) ||
         (c >= 0x110E && c <= 0x1112 ) ||
         (c == 0x113C) ||
         (c == 0x113E) ||
         (c == 0x1140) ||
         (c == 0x114C) ||
         (c == 0x114E) ||
         (c == 0x1150) ||
         (c >= 0x1154 && c <= 0x1155 ) ||
         (c == 0x1159) ||
         (c >= 0x115F && c <= 0x1161 ) ||
         (c == 0x1163) ||
         (c == 0x1165) ||
         (c == 0x1167) ||
	 (c == 0x1169) ||
         (c >= 0x116D && c <= 0x116E ) ||
         (c >= 0x1172 && c <= 0x1173 ) ||
         (c == 0x1175) ||
	 (c == 0x119E) ||
         (c == 0x11A8) ||
         (c == 0x11AB) ||
         (c >= 0x11AE && c <= 0x11AF ) ||
         (c >= 0x11B7 && c <= 0x11B8 ) ||
	 (c == 0x11BA) ||
         (c >= 0x11BC && c <= 0x11C2 ) ||
         (c == 0x11EB) ||
         (c == 0x11F0) ||
         (c == 0x11F9) ||
	 (c >= 0x1E00 && c <= 0x1E9B ) ||
         (c >= 0x1EA0 && c <= 0x1EF9 ) ||
         (c >= 0x1F00 && c <= 0x1F15 ) ||
         (c >= 0x1F18 && c <= 0x1F1D ) ||
	 (c >= 0x1F20 && c <= 0x1F45 ) ||
         (c >= 0x1F48 && c <= 0x1F4D ) ||
         (c >= 0x1F50 && c <= 0x1F57 ) ||
         (c == 0x1F59) ||
	 (c == 0x1F5B) ||
         (c == 0x1F5D) ||
         (c >= 0x1F5F && c <= 0x1F7D ) ||
         (c >= 0x1F80 && c <= 0x1FB4 ) ||
	 (c >= 0x1FB6 && c <= 0x1FBC ) ||
         (c == 0x1FBE) ||
         (c >= 0x1FC2 && c <= 0x1FC4 ) ||
         (c >= 0x1FC6 && c <= 0x1FCC ) ||
	 (c >= 0x1FD0 && c <= 0x1FD3 ) ||
         (c >= 0x1FD6 && c <= 0x1FDB ) ||
         (c >= 0x1FE0 && c <= 0x1FEC ) ||
         (c >= 0x1FF2 && c <= 0x1FF4 ) ||
	 (c >= 0x1FF6 && c <= 0x1FFC ) ||
         (c == 0x2126) ||
         (c >= 0x212A && c <= 0x212B ) ||
         (c == 0x212E) ||
	 (c >= 0x2180 && c <= 0x2182 ) ||
         (c >= 0x3041 && c <= 0x3094 ) ||
         (c >= 0x30A1 && c <= 0x30FA ) ||
         (c >= 0x3105 && c <= 0x312C ) ||
         (c >= 0xAC00 && c <= 0xD7A3 ) 
         );
}

   
static int
raptor_unicode_is_ideographic(long c)
{
  /* http://www.w3.org/TR/2000/REC-xml-20001006#NT-Ideographic */
  return((c >= 0x4E00 && c <= 0x9FA5 ) ||
         (c == 0x3007) ||
         (c >= 0x3021 && c <= 0x3029 ));
}


static int
raptor_unicode_is_combiningchar(long c)
{
  /* http://www.w3.org/TR/2000/REC-xml-20001006#NT-CombiningChar */
  return((c >= 0x0300 && c <= 0x0345 ) ||
         (c >= 0x0360 && c <= 0x0361 ) ||
         (c >= 0x0483 && c <= 0x0486 ) ||
         (c >= 0x0591 && c <= 0x05A1 ) ||
         (c >= 0x05A3 && c <= 0x05B9 ) ||
         (c >= 0x05BB && c <= 0x05BD ) ||
         (c == 0x05BF) ||
         (c >= 0x05C1 && c <= 0x05C2 ) ||
         (c == 0x05C4) ||
         (c >= 0x064B && c <= 0x0652 ) ||
         (c == 0x0670) ||
         (c >= 0x06D6 && c <= 0x06DC ) ||
	 (c >= 0x06DD && c <= 0x06DF ) ||
         (c >= 0x06E0 && c <= 0x06E4 ) ||
         (c >= 0x06E7 && c <= 0x06E8 ) ||
         (c >= 0x06EA && c <= 0x06ED ) ||
	 (c >= 0x0901 && c <= 0x0903 ) ||
         (c == 0x093C) ||
         (c >= 0x093E && c <= 0x094C ) ||
         (c == 0x094D) ||
	 (c >= 0x0951 && c <= 0x0954 ) ||
         (c >= 0x0962 && c <= 0x0963 ) ||
         (c >= 0x0981 && c <= 0x0983 ) ||
         (c == 0x09BC) ||
	 (c == 0x09BE) ||
         (c == 0x09BF) ||
         (c >= 0x09C0 && c <= 0x09C4 ) ||
         (c >= 0x09C7 && c <= 0x09C8 ) ||
	 (c >= 0x09CB && c <= 0x09CD ) ||
         (c == 0x09D7) ||
         (c >= 0x09E2 && c <= 0x09E3 ) ||
         (c == 0x0A02) ||
	 (c == 0x0A3C) ||
         (c == 0x0A3E) ||
         (c == 0x0A3F) ||
         (c >= 0x0A40 && c <= 0x0A42 ) ||
         (c >= 0x0A47 && c <= 0x0A48 ) ||
	 (c >= 0x0A4B && c <= 0x0A4D ) ||
         (c >= 0x0A70 && c <= 0x0A71 ) ||
         (c >= 0x0A81 && c <= 0x0A83 ) ||
         (c == 0x0ABC) ||
	 (c >= 0x0ABE && c <= 0x0AC5 ) ||
         (c >= 0x0AC7 && c <= 0x0AC9 ) ||
         (c >= 0x0ACB && c <= 0x0ACD ) ||
         (c >= 0x0B01 && c <= 0x0B03 ) ||
	 (c == 0x0B3C) ||
         (c >= 0x0B3E && c <= 0x0B43 ) ||
         (c >= 0x0B47 && c <= 0x0B48 ) ||
         (c >= 0x0B4B && c <= 0x0B4D ) ||
	 (c >= 0x0B56 && c <= 0x0B57 ) ||
         (c >= 0x0B82 && c <= 0x0B83 ) ||
         (c >= 0x0BBE && c <= 0x0BC2 ) ||
         (c >= 0x0BC6 && c <= 0x0BC8 ) ||
	 (c >= 0x0BCA && c <= 0x0BCD ) ||
         (c == 0x0BD7) ||
         (c >= 0x0C01 && c <= 0x0C03 ) ||
         (c >= 0x0C3E && c <= 0x0C44 ) ||
	 (c >= 0x0C46 && c <= 0x0C48 ) ||
         (c >= 0x0C4A && c <= 0x0C4D ) ||
         (c >= 0x0C55 && c <= 0x0C56 ) ||
         (c >= 0x0C82 && c <= 0x0C83 ) ||
	 (c >= 0x0CBE && c <= 0x0CC4 ) ||
         (c >= 0x0CC6 && c <= 0x0CC8 ) ||
         (c >= 0x0CCA && c <= 0x0CCD ) ||
         (c >= 0x0CD5 && c <= 0x0CD6 ) ||
	 (c >= 0x0D02 && c <= 0x0D03 ) ||
         (c >= 0x0D3E && c <= 0x0D43 ) ||
         (c >= 0x0D46 && c <= 0x0D48 ) ||
         (c >= 0x0D4A && c <= 0x0D4D ) ||
	 (c == 0x0D57) ||
         (c == 0x0E31) ||
         (c >= 0x0E34 && c <= 0x0E3A ) ||
         (c >= 0x0E47 && c <= 0x0E4E ) ||
	 (c == 0x0EB1) ||
         (c >= 0x0EB4 && c <= 0x0EB9 ) ||
         (c >= 0x0EBB && c <= 0x0EBC ) ||
         (c >= 0x0EC8 && c <= 0x0ECD ) ||
	 (c >= 0x0F18 && c <= 0x0F19 ) ||
         (c == 0x0F35) ||
         (c == 0x0F37) ||
         (c == 0x0F39) ||
         (c == 0x0F3E) ||
	 (c == 0x0F3F) ||
         (c >= 0x0F71 && c <= 0x0F84 ) ||
         (c >= 0x0F86 && c <= 0x0F8B ) ||
         (c >= 0x0F90 && c <= 0x0F95 ) ||
	 (c == 0x0F97) ||
         (c >= 0x0F99 && c <= 0x0FAD ) ||
         (c >= 0x0FB1 && c <= 0x0FB7 ) ||
         (c == 0x0FB9) ||
	 (c >= 0x20D0 && c <= 0x20DC ) ||
         (c == 0x20E1) ||
         (c >= 0x302A && c <= 0x302F ) ||
         (c == 0x3099) ||
	 (c == 0x309A));
}


static int
raptor_unicode_is_digit(long c)
{
  /* http://www.w3.org/TR/2000/REC-xml-20001006#NT-Digit */
  return((c >= 0x0030 && c <= 0x0039 ) ||
         (c >= 0x0660 && c <= 0x0669 ) ||
         (c >= 0x06F0 && c <= 0x06F9 ) ||
         (c >= 0x0966 && c <= 0x096F ) ||
         (c >= 0x09E6 && c <= 0x09EF ) ||
         (c >= 0x0A66 && c <= 0x0A6F ) ||
         (c >= 0x0AE6 && c <= 0x0AEF ) ||
         (c >= 0x0B66 && c <= 0x0B6F ) ||
         (c >= 0x0BE7 && c <= 0x0BEF ) ||
         (c >= 0x0C66 && c <= 0x0C6F ) ||
         (c >= 0x0CE6 && c <= 0x0CEF ) ||
         (c >= 0x0D66 && c <= 0x0D6F ) ||
         (c >= 0x0E50 && c <= 0x0E59 ) ||
         (c >= 0x0ED0 && c <= 0x0ED9 ) ||
         (c >= 0x0F20 && c <= 0x0F29 ));
}


static int
raptor_unicode_is_extender(long c)
{
  /* http://www.w3.org/TR/2000/REC-xml-20001006#NT-Extender */
  return((c == 0x00B7) ||
         (c == 0x02D0) ||
         (c == 0x02D1) ||
         (c == 0x0387) ||
         (c == 0x0640) ||
         (c == 0x0E46) || 
         (c == 0x0EC6) ||
         (c == 0x3005) ||
         (c >= 0x3031 && c <= 0x3035 ) ||
         (c >= 0x309D && c <= 0x309E ) ||
         (c >= 0x30FC && c <= 0x30FE ));
}


/**
 * raptor_utf8_is_nfc:
 * @input: UTF-8 string
 * @length: length of string
 *
 * Check a string is in Unicode Normal Form C.
 * 
 * Return value: Non 0 if the string is NFC
 **/
int
raptor_utf8_is_nfc(const unsigned char *input, size_t length) 
{
  unsigned int i;
  int plain=1;
  
  for(i=0; i<length; i++)
    if(input[i]>0x7f) {
      plain=0;
      break;
    }
    
  if(plain)
    return 1;

#ifdef RAPTOR_NFC_CHECK  
  return raptor_nfc_check(input, length, NULL);
#else
  return 1;
#endif
}


/**
 * raptor_utf8_check:
 * @string: UTF-8 string
 * @length: length of string
 *
 * Check a string is UTF-8.
 * 
 * Return value: Non 0 if the string is UTF-8
 **/
int
raptor_utf8_check(const unsigned char *string, size_t length)
{
  while(length > 0) {
    raptor_unichar unichar=0;

    int unichar_len=raptor_utf8_to_unicode_char(&unichar, string, length);
    if(unichar_len < 0 || unichar_len > (int)length)
      return 0;

    if(unichar > 0x10ffff)
      return 0;
  
    string += unichar_len;
    length -= unichar_len;
  }
  return 1;
}
