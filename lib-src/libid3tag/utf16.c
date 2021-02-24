/*
 * libid3tag - ID3 tag manipulation library
 * Copyright (C) 2000-2004 Underbit Technologies, Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * $Id: utf16.c,v 1.4 2009-03-30 10:00:58 llucius Exp $
 */

# ifdef HAVE_CONFIG_H
#  include "config.h"
# endif

# include "global.h"

# include <stdlib.h>

# include "id3tag.h"
# include "utf16.h"
# include "ucs4.h"

/*
 * NAME:	utf16->length()
 * DESCRIPTION:	return the number of ucs4 chars represented by a utf16 string
 */
id3_length_t id3_utf16_length(id3_utf16_t const *utf16)
{
  id3_length_t length = 0;

  while (*utf16) {
    if (utf16[0] < 0xd800 || utf16[0] > 0xdfff)
      ++length;
    else if (utf16[0] >= 0xd800 && utf16[0] <= 0xdbff &&
	     utf16[1] >= 0xdc00 && utf16[1] <= 0xdfff) {
      ++length;
      ++utf16;
    }

    ++utf16;
  }

  return length;
}

/*
 * NAME:	utf16->size()
 * DESCRIPTION:	return the encoding size of a utf16 string
 */
id3_length_t id3_utf16_size(id3_utf16_t const *utf16)
{
  id3_utf16_t const *ptr = utf16;

  while (*ptr)
    ++ptr;

  return ptr - utf16 + 1;
}

/*
 * NAME:	utf16->ucs4duplicate()
 * DESCRIPTION:	duplicate and decode a utf16 string into ucs4
 */
id3_ucs4_t *id3_utf16_ucs4duplicate(id3_utf16_t const *utf16)
{
  id3_ucs4_t *ucs4;

  ucs4 = malloc((id3_utf16_length(utf16) + 1) * sizeof(*ucs4));
  if (ucs4)
    id3_utf16_decode(utf16, ucs4);

  return release(ucs4);
}

/*
 * NAME:	utf16->decodechar()
 * DESCRIPTION:	decode a series of utf16 chars into a single ucs4 char
 */
id3_length_t id3_utf16_decodechar(id3_utf16_t const *utf16, id3_ucs4_t *ucs4)
{
  id3_utf16_t const *start = utf16;

  while (1) {
    if (utf16[0] < 0xd800 || utf16[0] > 0xdfff) {
      *ucs4 = utf16[0];
      return utf16 - start + 1;
    }
    else if (utf16[0] >= 0xd800 && utf16[0] <= 0xdbff &&
	     utf16[1] >= 0xdc00 && utf16[1] <= 0xdfff) {
      *ucs4 = (((utf16[0] & 0x03ffL) << 10) |
	       ((utf16[1] & 0x03ffL) <<  0)) + 0x00010000L;
      return utf16 - start + 2;
    }

    ++utf16;
  }
}

/*
 * NAME:	utf16->encodechar()
 * DESCRIPTION:	encode a single ucs4 char into a series of up to 2 utf16 chars
 */
id3_length_t id3_utf16_encodechar(id3_utf16_t *utf16, id3_ucs4_t ucs4)
{
  if (ucs4 < 0x00010000L) {
    utf16[0] = ucs4;

    return 1;
  }
  else if (ucs4 < 0x00110000L) {
    ucs4 -= 0x00010000L;

    utf16[0] = ((ucs4 >> 10) & 0x3ff) | 0xd800;
    utf16[1] = ((ucs4 >>  0) & 0x3ff) | 0xdc00;

    return 2;
  }

  /* default */

  return id3_utf16_encodechar(utf16, ID3_UCS4_REPLACEMENTCHAR);
}

/*
 * NAME:	utf16->decode()
 * DESCRIPTION:	decode a complete utf16 string into a ucs4 string
 */
void id3_utf16_decode(id3_utf16_t const *utf16, id3_ucs4_t *ucs4)
{
  do
    utf16 += id3_utf16_decodechar(utf16, ucs4);
  while (*ucs4++);
}

/*
 * NAME:	utf16->encode()
 * DESCRIPTION:	encode a complete ucs4 string into a utf16 string
 */
void id3_utf16_encode(id3_utf16_t *utf16, id3_ucs4_t const *ucs4)
{
  do
    utf16 += id3_utf16_encodechar(utf16, *ucs4);
  while (*ucs4++);
}

/*
 * NAME:	utf16->put()
 * DESCRIPTION:	serialize a single utf16 character
 */
id3_length_t id3_utf16_put(id3_byte_t **ptr, id3_utf16_t utf16,
			   enum id3_utf16_byteorder byteorder)
{
  if (ptr) {
    switch (byteorder) {
    case ID3_UTF16_BYTEORDER_BE:
      (*ptr)[0] = (utf16 >> 8) & 0xff;
      (*ptr)[1] = (utf16 >> 0) & 0xff;
      break;

    default:
    case ID3_UTF16_BYTEORDER_LE:
      (*ptr)[0] = (utf16 >> 0) & 0xff;
      (*ptr)[1] = (utf16 >> 8) & 0xff;
      break;
    }

    *ptr += 2;
  }

  return 2;
}

/*
 * NAME:	utf16->get()
 * DESCRIPTION:	deserialize a single utf16 character
 */
id3_utf16_t id3_utf16_get(id3_byte_t const **ptr,
			  enum id3_utf16_byteorder byteorder)
{
  id3_utf16_t utf16;

  switch (byteorder) {
  default:
  case ID3_UTF16_BYTEORDER_BE:
    utf16 =
      ((*ptr)[0] << 8) |
      ((*ptr)[1] << 0);
    break;

  case ID3_UTF16_BYTEORDER_LE:
    utf16 =
      ((*ptr)[0] << 0) |
      ((*ptr)[1] << 8);
    break;
  }

  *ptr += 2;

  return utf16;
}

/*
 * NAME:	utf16->serialize()
 * DESCRIPTION:	serialize a ucs4 string using utf16 encoding
 */
id3_length_t id3_utf16_serialize(id3_byte_t **ptr, id3_ucs4_t const *ucs4,
				 enum id3_utf16_byteorder byteorder,
				 int terminate)
{
  id3_length_t size = 0;
  id3_utf16_t utf16[2], *out;

  if (byteorder == ID3_UTF16_BYTEORDER_ANY)
    size += id3_utf16_put(ptr, 0xfeff, byteorder);

  while (*ucs4) {
    switch (id3_utf16_encodechar(out = utf16, *ucs4++)) {
    case 2: size += id3_utf16_put(ptr, *out++, byteorder);
    case 1: size += id3_utf16_put(ptr, *out++, byteorder);
    case 0: break;
    }
  }

  if (terminate)
    size += id3_utf16_put(ptr, 0, byteorder);

  return size;
}

/*
 * NAME:	utf16->deserialize()
 * DESCRIPTION:	deserialize a ucs4 string using utf16 encoding
 */
id3_ucs4_t *id3_utf16_deserialize(id3_byte_t const **ptr, id3_length_t length,
				  enum id3_utf16_byteorder byteorder)
{
  id3_byte_t const *end;
  id3_utf16_t *utf16ptr, *utf16;
  id3_ucs4_t *ucs4;

  end = *ptr + (length & ~1);

  utf16 = malloc((length / 2 + 1) * sizeof(*utf16));
  if (utf16 == 0)
    return 0;

  if (byteorder == ID3_UTF16_BYTEORDER_ANY && end - *ptr > 0) {
    switch (((*ptr)[0] << 8) |
	    ((*ptr)[1] << 0)) {
    case 0xfeff:
      byteorder = ID3_UTF16_BYTEORDER_BE;
      *ptr += 2;
      break;

    case 0xfffe:
      byteorder = ID3_UTF16_BYTEORDER_LE;
      *ptr += 2;
      break;
    default:
       // The text is missing the BOM, so attempt to detect the byte order.
       // This is using code from ReactOS and, while it's not foolproof, it
       // does provide some hope of getting the endianness correct.
       {
         unsigned char last_lo_byte = 0;
         unsigned char last_hi_byte = 0;
         unsigned int hi_byte_diff = 0;
         unsigned int lo_byte_diff = 0;

         int i;
         for (i = 0; i < length; i += 2)
         {
           unsigned char lo_byte = (*ptr)[i + 1];
           unsigned char hi_byte = (*ptr)[i];

#if !defined(MIN)
#define MIN(x, y) ((x) < (y) ? (x) : (y))
#endif
#if !defined(MAX)
#define MAX(x, y) ((x) > (y) ? (x) : (y))
#endif

           lo_byte_diff += MAX(lo_byte, last_lo_byte) - MIN(lo_byte, last_lo_byte);
           hi_byte_diff += MAX(hi_byte, last_hi_byte) - MIN(hi_byte, last_hi_byte);

           last_lo_byte = lo_byte;
           last_hi_byte = hi_byte;
         }

          if (lo_byte_diff < 127 && !hi_byte_diff)
          {
            byteorder = ID3_UTF16_BYTEORDER_BE;
          }

          if (hi_byte_diff && !lo_byte_diff)
          {
            byteorder = ID3_UTF16_BYTEORDER_LE;
          }
       }
       break;
    }
  }

  utf16ptr = utf16;
  while (end - *ptr > 0 && (*utf16ptr = id3_utf16_get(ptr, byteorder)))
    ++utf16ptr;

  *utf16ptr = 0;

  ucs4 = malloc((id3_utf16_length(utf16) + 1) * sizeof(*ucs4));
  if (ucs4)
    id3_utf16_decode(utf16, ucs4);

  free(utf16);

  return ucs4;
}
