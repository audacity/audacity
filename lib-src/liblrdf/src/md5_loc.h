/*
 * Local defines for the md5 functions.
 *
 * $Id: md5_loc.h,v 1.2 2008-06-26 15:14:26 larsl Exp $
 */

/*
 * Copyright (C) 1991-2, RSA Data Security, Inc. Created 1991. All
 * rights reserved.
 *
 * License to copy and use this software is granted provided that it is
 * identified as the "RSA Data Security, Inc. MD5 Message-Digest
 * Algorithm" in all material mentioning or referencing this software
 * or this function.
 *
 * License is also granted to make and use derivative works provided that
 * such works are identified as "derived from the RSA Data Security,
 * Inc. MD5 Message-Digest Algorithm" in all material mentioning or
 * referencing the derived work.
 *
 * RSA Data Security, Inc. makes no representations concerning either the
 * merchantability of this software or the suitability of this
 * software for any particular purpose. It is provided "as is" without
 * express or implied warranty of any kind.
 *
 * These notices must be retained in any copies of any part of this
 * documentation and/or software.
 */

#ifndef __MD5_LOC_H__
#define __MD5_LOC_H__

#define HEX_STRING	"0123456789abcdef"	/* to convert to hex */
#define BLOCK_SIZE_MASK	(MD5_BLOCK_SIZE - 1)


/*
 * Define my endian-ness.  Could not do in a portable manner using the
 * include files -- grumble.
 */
#ifdef WORDS_BIGENDIAN
/*
 * big endian - big is better
 */
#define SWAP(n)	\
    (((n) << 24) | (((n) & 0xff00) << 8) | (((n) >> 8) & 0xff00) | ((n) >> 24))
#else
/*
	+ * little endian
	+ */
#define SWAP(n)    (n)
#endif

/*
 * These are the four functions used in the four steps of the MD5
 * algorithm and defined in the RFC 1321.  The first function is a
 * little bit optimized (as found in Colin Plumbs public domain
 * implementation).
 */
/* #define FF(b, c, d) ((b & c) | (~b & d)) */
#define FF(b, c, d)	(d ^ (b & (c ^ d)))
#define FG(b, c, d)	FF(d, b, c)
#define FH(b, c, d)	(b ^ c ^ d)
#define FI(b, c, d)	(c ^ (b | ~d))

/*
 * It is unfortunate that C does not provide an operator for cyclic
 * rotation.  Hope the C compiler is smart enough.  -- Modified to
 * remove the w = at the front - Gray 2/97
 */
#define CYCLIC(w, s)	((w << s) | (w >> (32 - s)))

/*
 * First Round: using the given function, the context and a constant
 * the next context is computed.  Because the algorithms processing
 * unit is a 32-bit word and it is determined to work on words in
 * little endian byte order we perhaps have to change the byte order
 * before the computation.  To reduce the work for the next steps we
 * store the swapped words in the array CORRECT_WORDS. -- Modified to
 * fix the handling of unaligned buffer spaces - Gray 7/97
 */
#define OP1(a, b, c, d, b_p, c_p, s, T)				\
     do {							\
       memcpy(c_p, b_p, sizeof(md5_uint32));       		\
       *c_p = SWAP(*c_p);					\
       a += FF (b, c, d) + *c_p + T;				\
       a = CYCLIC (a, s);					\
       a += b;							\
       b_p = (char *)b_p + sizeof(md5_uint32);			\
       c_p++;							\
    } while (0)

/*
 * Second to Fourth Round: we have the possibly swapped words in
 * CORRECT_WORDS.  Redefine the macro to take an additional first
 * argument specifying the function to use.
 */
#define OP234(FUNC, a, b, c, d, k, s, T)		\
    do { 						\
      a += FUNC (b, c, d) + k + T;			\
      a = CYCLIC (a, s);				\
      a += b;						\
    } while (0)

#endif /* ! __MD5_LOC_H__ */
