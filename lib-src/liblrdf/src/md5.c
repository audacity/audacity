/*
 * Functions to compute MD5 message digest of files or memory blocks
 * according to the definition of MD5 in RFC 1321 from April 1992.
 * Copyright (C) 1995, 1996 Free Software Foundation, Inc.  NOTE: The
 * canonical source of this file is maintained with the GNU C Library.
 * Bugs can be reported to bug-glibc@prep.ai.mit.edu.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 2, or (at your
 * option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *
 * Written by Ulrich Drepper <drepper@gnu.ai.mit.edu>, 1995.
 * Modified by Gray Watson <http://256.com/gray/>, 1997.
 *
 * $Id: md5.c,v 1.2 2008-06-26 15:14:26 larsl Exp $
 */

/*
 * NOTE: during quick performance tests on a Sun Sparc Ultra 1 and an
 * Alpha 255 300, these functions performed upwards of 3mb/sec
 * including disk I/O time.
 */

/*
 * MD5 Test Suite from RFC1321: http://ds.internic.net:/rfc/rfc1321.txt
 *
 * MD5 ("") = d41d8cd98f00b204e9800998ecf8427e
 * MD5 ("a") = 0cc175b9c0f1b6a831c399e269772661
 * MD5 ("abc") = 900150983cd24fb0d6963f7d28e17f72
 * MD5 ("message digest") = f96b697d7cb7938d525a2f31aaf161d0
 * MD5 ("abcdefghijklmnopqrstuvwxyz") = c3fcd3d76192e4007dfb496cca67e13b
 * MD5 ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789") =
 * d174ab98d277d9f5a5611c2c9f419d9f
 * MD5 ("123456789012345678901234567890123456789012345678901234567890123456
 * 78901234567890") = 57edf4a22be3c955ac49da2e2107b67a
 */

#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#include "lrdf_md5.h"
#include "md5_loc.h"

/* static	char	*rcs_id =
   "$Id: md5.c,v 1.2 2008-06-26 15:14:26 larsl Exp $"; */

/* version id for the library */
/* static char *version_id = "$MD5Version: 1.0.0 November-19-1997 $"; */

/****************************** local routines *******************************/

/*
 * process_block
 *
 * DESCRIPTION:
 *
 * Process a block of bytes into a MD5 state structure.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * md5_p - Pointer to MD5 structure from which we are getting the result.
 *
 * buffer - A buffer of bytes whose MD5 signature we are calculating.
 *
 * buf_len - The length of the buffer.
 */
static	void	process_block(md5_t *md5_p, const void *buffer,
			      const unsigned int buf_len)
{
  md5_uint32	correct[16];
  const void	*buf_p = buffer, *end_p;
  unsigned int	words_n;
  md5_uint32	A, B, C, D;

  words_n = buf_len / sizeof(md5_uint32);
  end_p = (char *)buf_p + words_n * sizeof(md5_uint32);

  A = md5_p->md_A;
  B = md5_p->md_B;
  C = md5_p->md_C;
  D = md5_p->md_D;

  /*
   * First increment the byte count.  RFC 1321 specifies the possible
   * length of the file up to 2^64 bits.  Here we only compute the
   * number of bytes with a double word increment.  Modified to do
   * this to better avoid overflows in the lower word -- Gray 10/97.
   */
  if (md5_p->md_total[0] > MAX_MD5_UINT32 - buf_len) {
    md5_p->md_total[1]++;
    md5_p->md_total[0] -= (MAX_MD5_UINT32 - buf_len);
  }
  else {
    md5_p->md_total[0] += buf_len;
  }

  /*
   * Process all bytes in the buffer with MD5_BLOCK bytes in each
   * round of the loop.
   */
  while (buf_p < end_p) {
    md5_uint32	A_save, B_save, C_save, D_save;
    md5_uint32	*corr_p = correct;

    A_save = A;
    B_save = B;
    C_save = C;
    D_save = D;

    /*
     * Before we start, one word to the strange constants.  They are
     * defined in RFC 1321 as
     *
     * T[i] = (int) (4294967296.0 * fabs (sin (i))), i=1..MD5_BLOCK
     */

    /* Round 1. */
    OP1 (A, B, C, D, buf_p, corr_p,  7, 0xd76aa478);
    OP1 (D, A, B, C, buf_p, corr_p, 12, 0xe8c7b756);
    OP1 (C, D, A, B, buf_p, corr_p, 17, 0x242070db);
    OP1 (B, C, D, A, buf_p, corr_p, 22, 0xc1bdceee);
    OP1 (A, B, C, D, buf_p, corr_p,  7, 0xf57c0faf);
    OP1 (D, A, B, C, buf_p, corr_p, 12, 0x4787c62a);
    OP1 (C, D, A, B, buf_p, corr_p, 17, 0xa8304613);
    OP1 (B, C, D, A, buf_p, corr_p, 22, 0xfd469501);
    OP1 (A, B, C, D, buf_p, corr_p,  7, 0x698098d8);
    OP1 (D, A, B, C, buf_p, corr_p, 12, 0x8b44f7af);
    OP1 (C, D, A, B, buf_p, corr_p, 17, 0xffff5bb1);
    OP1 (B, C, D, A, buf_p, corr_p, 22, 0x895cd7be);
    OP1 (A, B, C, D, buf_p, corr_p,  7, 0x6b901122);
    OP1 (D, A, B, C, buf_p, corr_p, 12, 0xfd987193);
    OP1 (C, D, A, B, buf_p, corr_p, 17, 0xa679438e);
    OP1 (B, C, D, A, buf_p, corr_p, 22, 0x49b40821);

    /* Round 2. */
    OP234 (FG, A, B, C, D, correct[  1],  5, 0xf61e2562);
    OP234 (FG, D, A, B, C, correct[  6],  9, 0xc040b340);
    OP234 (FG, C, D, A, B, correct[ 11], 14, 0x265e5a51);
    OP234 (FG, B, C, D, A, correct[  0], 20, 0xe9b6c7aa);
    OP234 (FG, A, B, C, D, correct[  5],  5, 0xd62f105d);
    OP234 (FG, D, A, B, C, correct[ 10],  9, 0x02441453);
    OP234 (FG, C, D, A, B, correct[ 15], 14, 0xd8a1e681);
    OP234 (FG, B, C, D, A, correct[  4], 20, 0xe7d3fbc8);
    OP234 (FG, A, B, C, D, correct[  9],  5, 0x21e1cde6);
    OP234 (FG, D, A, B, C, correct[ 14],  9, 0xc33707d6);
    OP234 (FG, C, D, A, B, correct[  3], 14, 0xf4d50d87);
    OP234 (FG, B, C, D, A, correct[  8], 20, 0x455a14ed);
    OP234 (FG, A, B, C, D, correct[ 13],  5, 0xa9e3e905);
    OP234 (FG, D, A, B, C, correct[  2],  9, 0xfcefa3f8);
    OP234 (FG, C, D, A, B, correct[  7], 14, 0x676f02d9);
    OP234 (FG, B, C, D, A, correct[ 12], 20, 0x8d2a4c8a);

    /* Round 3. */
    OP234 (FH, A, B, C, D, correct[  5],  4, 0xfffa3942);
    OP234 (FH, D, A, B, C, correct[  8], 11, 0x8771f681);
    OP234 (FH, C, D, A, B, correct[ 11], 16, 0x6d9d6122);
    OP234 (FH, B, C, D, A, correct[ 14], 23, 0xfde5380c);
    OP234 (FH, A, B, C, D, correct[  1],  4, 0xa4beea44);
    OP234 (FH, D, A, B, C, correct[  4], 11, 0x4bdecfa9);
    OP234 (FH, C, D, A, B, correct[  7], 16, 0xf6bb4b60);
    OP234 (FH, B, C, D, A, correct[ 10], 23, 0xbebfbc70);
    OP234 (FH, A, B, C, D, correct[ 13],  4, 0x289b7ec6);
    OP234 (FH, D, A, B, C, correct[  0], 11, 0xeaa127fa);
    OP234 (FH, C, D, A, B, correct[  3], 16, 0xd4ef3085);
    OP234 (FH, B, C, D, A, correct[  6], 23, 0x04881d05);
    OP234 (FH, A, B, C, D, correct[  9],  4, 0xd9d4d039);
    OP234 (FH, D, A, B, C, correct[ 12], 11, 0xe6db99e5);
    OP234 (FH, C, D, A, B, correct[ 15], 16, 0x1fa27cf8);
    OP234 (FH, B, C, D, A, correct[  2], 23, 0xc4ac5665);

    /* Round 4. */
    OP234 (FI, A, B, C, D, correct[  0],  6, 0xf4292244);
    OP234 (FI, D, A, B, C, correct[  7], 10, 0x432aff97);
    OP234 (FI, C, D, A, B, correct[ 14], 15, 0xab9423a7);
    OP234 (FI, B, C, D, A, correct[  5], 21, 0xfc93a039);
    OP234 (FI, A, B, C, D, correct[ 12],  6, 0x655b59c3);
    OP234 (FI, D, A, B, C, correct[  3], 10, 0x8f0ccc92);
    OP234 (FI, C, D, A, B, correct[ 10], 15, 0xffeff47d);
    OP234 (FI, B, C, D, A, correct[  1], 21, 0x85845dd1);
    OP234 (FI, A, B, C, D, correct[  8],  6, 0x6fa87e4f);
    OP234 (FI, D, A, B, C, correct[ 15], 10, 0xfe2ce6e0);
    OP234 (FI, C, D, A, B, correct[  6], 15, 0xa3014314);
    OP234 (FI, B, C, D, A, correct[ 13], 21, 0x4e0811a1);
    OP234 (FI, A, B, C, D, correct[  4],  6, 0xf7537e82);
    OP234 (FI, D, A, B, C, correct[ 11], 10, 0xbd3af235);
    OP234 (FI, C, D, A, B, correct[  2], 15, 0x2ad7d2bb);
    OP234 (FI, B, C, D, A, correct[  9], 21, 0xeb86d391);

    /* Add the starting values of the context. */
    A += A_save;
    B += B_save;
    C += C_save;
    D += D_save;
  }

  /* Put checksum in context given as argument. */
  md5_p->md_A = A;
  md5_p->md_B = B;
  md5_p->md_C = C;
  md5_p->md_D = D;
}

/*
 * md5_get_result
 *
 * DESCRIPTION:
 *
 * Copy the resulting MD5 signature from MD5_P into the first 16 bytes
 * (MD5_SIZE) of the result buffer.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * md5_p - Pointer to MD5 structure from which we are getting the result.
 *
 * result - A 16 byte buffer that will contain the MD5 signature.
 */
static	void	md5_get_result(const md5_t *md5_p, void *result)
{
  md5_uint32	hold;
  void		*res_p = result;

  hold = SWAP(md5_p->md_A);
  memcpy(res_p, &hold, sizeof(md5_uint32));
  res_p = (char *)res_p + sizeof(md5_uint32);

  hold = SWAP(md5_p->md_B);
  memcpy(res_p, &hold, sizeof(md5_uint32));
  res_p = (char *)res_p + sizeof(md5_uint32);

  hold = SWAP(md5_p->md_C);
  memcpy(res_p, &hold, sizeof(md5_uint32));
  res_p = (char *)res_p + sizeof(md5_uint32);

  hold = SWAP(md5_p->md_D);
  memcpy(res_p, &hold, sizeof(md5_uint32));
}

/***************************** exported routines *****************************/

/*
 * md5_init
 *
 * DESCRIPTION:
 *
 * Initialize structure containing state of MD5 computation. (RFC 1321,
 * 3.3: Step 3).  This is for progressive MD5 calculations only.  If
 * you have the complete string available, md5_buffer should be used.
 * md5_process should be called for each bunch of bytes and after the
 * last process call, md5_finish should be called to get the
 * signature.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * md5_p - Pointer to md5 structure that we are initializing.
 */
void	md5_init(md5_t *md5_p)
{
  md5_p->md_A = 0x67452301;
  md5_p->md_B = 0xefcdab89;
  md5_p->md_C = 0x98badcfe;
  md5_p->md_D = 0x10325476;

  md5_p->md_total[0] = 0;
  md5_p->md_total[1] = 0;
  md5_p->md_buf_len = 0;
}

/*
 * md5_process
 *
 * DESCRIPTION:
 *
 * This function is used to progressively calculate a MD5 signature some
 * number of bytes at a time.  If you have the complete string
 * available, md5_buffer should be used.  The MD5 structure should
 * have been initialized with md5_init and after the last process
 * call, md5_finish should be called to get the results.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * md5_p - Pointer to MD5 structure which we are progressively updating.
 *
 * buffer - A buffer of bytes whose MD5 signature we are calculating.
 *
 * buf_len - The length of the buffer.
 */
void	md5_process(md5_t *md5_p, const void *buffer,
		    const unsigned int buf_len)
{
  unsigned int	len = buf_len;
  unsigned int	in_block, add;

  /*
   * When we already have some bytes in our internal buffer, copy some
   * from the user to fill the block.
   */
  if (md5_p->md_buf_len > 0) {
    
    in_block = md5_p->md_buf_len;
    if (in_block + len > sizeof(md5_p->md_buffer)) {
      add = sizeof(md5_p->md_buffer) - in_block;
    }
    else {
      add = len;
    }

    memcpy (md5_p->md_buffer + in_block, buffer, add);
    md5_p->md_buf_len += add;
    in_block += add;

    if (in_block > MD5_BLOCK_SIZE) {
      process_block (md5_p, md5_p->md_buffer, in_block & ~BLOCK_SIZE_MASK);
      /* the regions in the following copy operation will not overlap. */
      memcpy (md5_p->md_buffer,
	      md5_p->md_buffer + (in_block & ~BLOCK_SIZE_MASK),
	      in_block & BLOCK_SIZE_MASK);
      md5_p->md_buf_len = in_block & BLOCK_SIZE_MASK;
    }

    buffer = (const char *)buffer + add;
    len -= add;
  }

  /* process available complete blocks right from the user buffer */
  if (len > MD5_BLOCK_SIZE) {
    process_block (md5_p, buffer, len & ~BLOCK_SIZE_MASK);
    buffer = (const char *) buffer + (len & ~BLOCK_SIZE_MASK);
    len &= BLOCK_SIZE_MASK;
  }

  /* copy remaining bytes into the internal buffer */
  if (len > 0) {
    memcpy (md5_p->md_buffer, buffer, len);
    md5_p->md_buf_len = len;
  }
}

/*
 * md5_finish
 *
 * DESCRIPTION:
 *
 * Finish a progressing MD5 calculation and copy the resulting MD5
 * signature into the result buffer which should be 16 bytes
 * (MD5_SIZE).  After this call, the MD5 structure is invalid.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * md5_p - Pointer to MD5 structure which we are finishing.
 *
 * signature - A 16 byte buffer that will contain the MD5 signature.
 */
void	md5_finish(md5_t *md5_p, void *signature)
{
  md5_uint32	bytes, hold;
  int		pad;

  /* take yet unprocessed bytes into account */
  bytes = md5_p->md_buf_len;

  /*
   * Count remaining bytes.  Modified to do this to better avoid
   * overflows in the lower word -- Gray 10/97.
   */
  if (md5_p->md_total[0] > MAX_MD5_UINT32 - bytes) {
    md5_p->md_total[1]++;
    md5_p->md_total[0] -= (MAX_MD5_UINT32 - bytes);
  }
  else {
    md5_p->md_total[0] += bytes;
  }

  /*
   * Pad the buffer to the next MD5_BLOCK-byte boundary.  (RFC 1321,
   * 3.1: Step 1).  We need enough room for two size words and the
   * bytes left in the buffer.  For some reason even if we are equal
   * to the block-size, we add an addition block of pad bytes.
   */
  pad = MD5_BLOCK_SIZE - (sizeof(md5_uint32) * 2) - bytes;
  if (pad <= 0) {
    pad += MD5_BLOCK_SIZE;
  }

  /*
   * Modified from a fixed array to this assignment and memset to be
   * more flexible with block-sizes -- Gray 10/97.
   */
  if (pad > 0) {
    /* some sort of padding start byte */
    md5_p->md_buffer[bytes] = (unsigned char)0x80;
    if (pad > 1) {
      memset (md5_p->md_buffer + bytes + 1, 0, pad - 1);
    }
    bytes += pad;
  }

  /* put the 64-bit file length in _bits_ (i.e. *8) at the end of the buffer */
  hold = SWAP(md5_p->md_total[0] << 3);
  memcpy(md5_p->md_buffer + bytes, &hold, sizeof(md5_uint32));
  bytes += sizeof(md5_uint32);

  /* shift the high word over by 3 and add in the top 3 bits from the low */
  hold = SWAP((md5_p->md_total[1] << 3) | (md5_p->md_total[0] >> 29));
  memcpy(md5_p->md_buffer + bytes, &hold, sizeof(md5_uint32));
  bytes += sizeof(md5_uint32);

  /* process last bytes, the padding chars, and size words */
  process_block(md5_p, md5_p->md_buffer, bytes);
  md5_get_result(md5_p, signature);
}

/*
 * md5_buffer
 *
 * DESCRIPTION:
 *
 * This function is used to calculate a MD5 signature for a buffer of
 * bytes.  If you only have part of a buffer that you want to process
 * then md5_init, md5_process, and md5_finish should be used.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * buffer - A buffer of bytes whose MD5 signature we are calculating.
 *
 * buf_len - The length of the buffer.
 *
 * signature - A 16 byte buffer that will contain the MD5 signature.
 */
void	md5_buffer(const char *buffer, const unsigned int buf_len,
		   void *signature)
{
  md5_t		md5;

  /* initialize the computation context */
  md5_init(&md5);

  /* process whole buffer but last buf_len % MD5_BLOCK bytes */
  md5_process(&md5, buffer, buf_len);

  /* put result in desired memory area */
  md5_finish(&md5, signature);
}

/*
 * md5_sig_to_string
 *
 * DESCRIPTION:
 *
 * Convert a MD5 signature in a 16 byte buffer into a hexadecimal string
 * representation.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * signature - a 16 byte buffer that contains the MD5 signature.
 *
 * str - a string of charactes which should be at least 33 bytes long (2
 * characters per MD5 byte and 1 for the \0).
 *
 * str_len - the length of the string.
 */
void	md5_sig_to_string(void *signature, char *str, const int str_len)
{
  unsigned char	*sig_p;
  char		*str_p, *max_p;
  unsigned int	high, low;
  
  str_p = str;
  max_p = str + str_len;
  
  for (sig_p = (unsigned char *)signature;
       sig_p < (unsigned char *)signature + MD5_SIZE;
       sig_p++) {
    high = *sig_p / 16;
    low = *sig_p % 16;
    /* account for 2 chars */
    if (str_p + 1 >= max_p) {
      break;
    }
    *str_p++ = HEX_STRING[high];
    *str_p++ = HEX_STRING[low];
  }
  /* account for 2 chars */
  if (str_p < max_p) {
    *str_p++ = '\0';
  }
}

/*
 * md5_sig_from_string
 *
 * DESCRIPTION:
 *
 * Convert a MD5 signature from a hexadecimal string representation into
 * a 16 byte buffer.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * signature - A 16 byte buffer that will contain the MD5 signature.
 *
 * str - A string of charactes which _must_ be at least 32 bytes long (2
 * characters per MD5 byte).
 */
void	md5_sig_from_string(void *signature, const char *str)
{
  unsigned char	*sig_p;
  const char	*str_p;
  char		*hex;
  unsigned int	high, low, val;
  
  hex = HEX_STRING;
  sig_p = signature;
  
  for (str_p = str; str_p < str + MD5_SIZE * 2; str_p += 2) {
    high = strchr(hex, *str_p) - hex;
    low = strchr(hex, *(str_p + 1)) - hex;
    val = high * 16 + low;
    *sig_p++ = val;
  }
}
