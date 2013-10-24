/*
 * Copyright (C) 2001 Edmund Grimley Evans <edmundo@rano.org>
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
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#ifdef HAVE_ICONV

#include <assert.h>
#include <errno.h>
#include <iconv.h>
#include <stdlib.h>
#include <string.h>

#include "iconvert.h"
#include "share/alloc.h"
#include "share/safe_str.h"

/*
 * Convert data from one encoding to another. Return:
 *
 *  -2 : memory allocation failed
 *  -1 : unknown encoding
 *   0 : data was converted exactly
 *   1 : data was converted inexactly
 *   2 : data was invalid (but still converted)
 *
 * We convert in two steps, via UTF-8, as this is the only
 * reliable way of distinguishing between invalid input
 * and valid input which iconv refuses to transliterate.
 * We convert from UTF-8 twice, because we have no way of
 * knowing whether the conversion was exact if iconv returns
 * E2BIG (due to a bug in the specification of iconv).
 * An alternative approach is to assume that the output of
 * iconv is never more than 4 times as long as the input,
 * but I prefer to avoid that assumption if possible.
 */

int iconvert(const char *fromcode, const char *tocode,
	     const char *from, size_t fromlen,
	     char **to, size_t *tolen)
{
  int ret = 0;
  iconv_t cd1, cd2;
  char *ib;
  char *ob;
  char *utfbuf = 0, *outbuf, *newbuf;
  size_t utflen, outlen, ibl, obl, k;
  char tbuf[2048];

  cd1 = iconv_open("UTF-8", fromcode);
  if (cd1 == (iconv_t)(-1))
    return -1;

  cd2 = (iconv_t)(-1);
  /* Don't use strcasecmp() as it's locale-dependent. */
  if (!strchr("Uu", tocode[0]) ||
      !strchr("Tt", tocode[1]) ||
      !strchr("Ff", tocode[2]) ||
      tocode[3] != '-' ||
      tocode[4] != '8' ||
      tocode[5] != '\0') {
    char *tocode1;
	size_t dest_len = strlen(tocode) + 11;
    /*
     * Try using this non-standard feature of glibc and libiconv.
     * This is deliberately not a config option as people often
     * change their iconv library without rebuilding applications.
     */
    tocode1 = safe_malloc_(dest_len);
    if (!tocode1)
      goto fail;

    safe_strncpy(tocode1, tocode, dest_len);
    safe_strncat(tocode1, "//TRANSLIT", dest_len);
    cd2 = iconv_open(tocode1, "UTF-8");
    free(tocode1);

    if (cd2 == (iconv_t)(-1))
      cd2 = iconv_open(tocode, fromcode);

    if (cd2 == (iconv_t)(-1)) {
      iconv_close(cd1);
      return -1;
    }
  }

  utflen = 1; /*fromlen * 2 + 1; XXX */
  utfbuf = malloc(utflen);
  if (!utfbuf)
    goto fail;

  /* Convert to UTF-8 */
  ib = (char *)from;
  ibl = fromlen;
  ob = utfbuf;
  obl = utflen;
  for (;;) {
    k = iconv(cd1, &ib, &ibl, &ob, &obl);
    assert((!k && !ibl) ||
	   (k == (size_t)(-1) && errno == E2BIG && ibl && obl < 6) ||
	   (k == (size_t)(-1) &&
	    (errno == EILSEQ || errno == EINVAL) && ibl));
    if (!ibl)
      break;
    if (obl < 6) {
      /* Enlarge the buffer */
      if(utflen*2 < utflen) /* overflow check */
	goto fail;
      utflen *= 2;
      newbuf = realloc(utfbuf, utflen);
      if (!newbuf)
	goto fail;
      ob = (ob - utfbuf) + newbuf;
      obl = utflen - (ob - newbuf);
      utfbuf = newbuf;
    }
    else {
      /* Invalid input */
      ib++, ibl--;
      *ob++ = '#', obl--;
      ret = 2;
      iconv(cd1, 0, 0, 0, 0);
    }
  }

  if (cd2 == (iconv_t)(-1)) {
    /* The target encoding was UTF-8 */
    if (tolen)
      *tolen = ob - utfbuf;
    if (!to) {
      free(utfbuf);
      iconv_close(cd1);
      return ret;
    }
    newbuf = safe_realloc_add_2op_(utfbuf, (ob - utfbuf), /*+*/1);
    if (!newbuf)
      goto fail;
    ob = (ob - utfbuf) + newbuf;
    *ob = '\0';
    *to = newbuf;
    iconv_close(cd1);
    return ret;
  }

  /* Truncate the buffer to be tidy */
  utflen = ob - utfbuf;
  newbuf = realloc(utfbuf, utflen);
  if (!newbuf)
    goto fail;
  utfbuf = newbuf;

  /* Convert from UTF-8 to discover how long the output is */
  outlen = 0;
  ib = utfbuf;
  ibl = utflen;
  while (ibl) {
    ob = tbuf;
    obl = sizeof(tbuf);
    k = iconv(cd2, &ib, &ibl, &ob, &obl);
    assert((k != (size_t)(-1) && !ibl) ||
	   (k == (size_t)(-1) && errno == E2BIG && ibl) ||
	   (k == (size_t)(-1) && errno == EILSEQ && ibl));
    if (ibl && !(k == (size_t)(-1) && errno == E2BIG)) {
      /* Replace one character */
      char *tb = "?";
      size_t tbl = 1;

      outlen += ob - tbuf;
      ob = tbuf;
      obl = sizeof(tbuf);
      k = iconv(cd2, &tb, &tbl, &ob, &obl);
      assert((!k && !tbl) ||
	     (k == (size_t)(-1) && errno == EILSEQ && tbl));
      for (++ib, --ibl; ibl && (*ib & 0x80); ib++, ibl--)
	;
    }
    outlen += ob - tbuf;
  }
  ob = tbuf;
  obl = sizeof(tbuf);
  k = iconv(cd2, 0, 0, &ob, &obl);
  assert(!k);
  outlen += ob - tbuf;

  /* Convert from UTF-8 for real */
  outbuf = safe_malloc_add_2op_(outlen, /*+*/1);
  if (!outbuf)
    goto fail;
  ib = utfbuf;
  ibl = utflen;
  ob = outbuf;
  obl = outlen;
  while (ibl) {
    k = iconv(cd2, &ib, &ibl, &ob, &obl);
    assert((k != (size_t)(-1) && !ibl) ||
	   (k == (size_t)(-1) && errno == EILSEQ && ibl));
    if (k && !ret)
      ret = 1;
    if (ibl && !(k == (size_t)(-1) && errno == E2BIG)) {
      /* Replace one character */
      char *tb = "?";
      size_t tbl = 1;

      k = iconv(cd2, &tb, &tbl, &ob, &obl);
      assert((!k && !tbl) ||
	     (k == (size_t)(-1) && errno == EILSEQ && tbl));
      for (++ib, --ibl; ibl && (*ib & 0x80); ib++, ibl--)
	;
    }
  }
  k = iconv(cd2, 0, 0, &ob, &obl);
  assert(!k);
  assert(!obl);
  *ob = '\0';

  free(utfbuf);
  iconv_close(cd1);
  iconv_close(cd2);
  if (tolen)
    *tolen = outlen;
  if (!to) {
    free(outbuf);
    return ret;
  }
  *to = outbuf;
  return ret;

 fail:
  if(0 != utfbuf)
    free(utfbuf);
  iconv_close(cd1);
  if (cd2 != (iconv_t)(-1))
    iconv_close(cd2);
  return -2;
}

#endif /* HAVE_ICONV */
