/*
 * Copyright (c) 2011 Apple Inc. All rights reserved.
 *
 * @APPLE_APACHE_LICENSE_HEADER_START@
 *
 * Licensed under the Apache License, Version 2.0 (the "License") ;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *	 http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * @APPLE_APACHE_LICENSE_HEADER_END@
 */

/*
	File:		dp_enc.c

	Contains:	Dynamic Predictor encode routines

	Copyright:	(c) 2001-2011 Apple, Inc.
*/

#include <string.h>

#include "dplib.h"
#include "shift.h"

#if __GNUC__
#define ALWAYS_INLINE		__attribute__ ((always_inline))
#else
#define ALWAYS_INLINE
#endif

#define LOOP_ALIGN

void
init_coefs (int16_t * coefs, uint32_t denshift, int32_t numPairs)
{
	int32_t		k ;
	int32_t		den = 1 << denshift ;

	coefs [0] = (AINIT * den) >> 4 ;
	coefs [1] = (BINIT * den) >> 4 ;
	coefs [2] = (CINIT * den) >> 4 ;
	for (k = 3 ; k < numPairs ; k++)
		coefs [k] = 0 ;
}

void
copy_coefs (const int16_t * srcCoefs, int16_t * dstCoefs, int32_t numPairs)
{
	int32_t k ;

	for (k = 0 ; k < numPairs ; k++)
		dstCoefs [k] = srcCoefs [k] ;
}

static inline int32_t ALWAYS_INLINE sign_of_int (int32_t i)
{
	int32_t negishift ;

	negishift = ((uint32_t) - i) >> 31 ;
	return negishift | (i >> 31) ;
}

void
pc_block (int32_t * in, int32_t * pc1, int32_t num, int16_t * coefs, int32_t numactive, uint32_t chanbits, uint32_t denshift)
{
	register int16_t	a0, a1, a2, a3 ;
	register int32_t	b0, b1, b2, b3 ;
	int32_t					j, k, lim ;
	int32_t *			pin ;
	int32_t				sum1, dd ;
	int32_t				sg, sgn ;
	int32_t				top ;
	int32_t				del, del0 ;
	uint32_t			chanshift = 32 - chanbits ;
	int32_t				denhalf = 1 << (denshift - 1) ;

	pc1 [0] = in [0] ;
	if (numactive == 0)
	{
		// just copy if numactive == 0 (but don't bother if in/out pointers the same)
		if ((num > 1) && (in != pc1))
			memcpy (&pc1 [1], &in [1], (num - 1) * sizeof (int32_t)) ;
		return ;
	}
	if (numactive == 31)
	{
		// short-circuit if numactive == 31
		for (j = 1 ; j < num ; j++)
		{
			del = in [j] - in [j-1] ;
			pc1 [j] = (del << chanshift) >> chanshift ;
		}
		return ;
	}

	for (j = 1 ; j <= numactive ; j++)
	{
		del = in [j] - in [j-1] ;
		pc1 [j] = arith_shift_left (del, chanshift) >> chanshift ;
	}

	lim = numactive + 1 ;

	if (numactive == 4)
	{
		// optimization for numactive == 4
		a0 = coefs [0] ;
		a1 = coefs [1] ;
		a2 = coefs [2] ;
		a3 = coefs [3] ;

		for (j = lim ; j < num ; j++)
		{
			LOOP_ALIGN

			top = in [j - lim] ;
			pin = in + j - 1 ;

			b0 = top - pin [0] ;
			b1 = top - pin [-1] ;
			b2 = top - pin [-2] ;
			b3 = top - pin [-3] ;

			sum1 = (denhalf - a0 * b0 - a1 * b1 - a2 * b2 - a3 * b3) >> denshift ;

			del = in [j] - top - sum1 ;
			del = arith_shift_left (del, chanshift) >> chanshift ;
			pc1 [j] = del ;
			del0 = del ;

			sg = sign_of_int (del) ;
			if (sg > 0)
			{
				sgn = sign_of_int (b3) ;
				a3 -= sgn ;
				del0 -= (4 - 3) * ((sgn * b3) >> denshift) ;
				if (del0 <= 0)
					continue ;

				sgn = sign_of_int (b2) ;
				a2 -= sgn ;
				del0 -= (4 - 2) * ((sgn * b2) >> denshift) ;
				if (del0 <= 0)
					continue ;

				sgn = sign_of_int (b1) ;
				a1 -= sgn ;
				del0 -= (4 - 1) * ((sgn * b1) >> denshift) ;
				if (del0 <= 0)
					continue ;

				a0 -= sign_of_int (b0) ;
			}
			else if (sg < 0)
			{
				// note: to avoid unnecessary negations, we flip the value of "sgn"
				sgn = -sign_of_int (b3) ;
				a3 -= sgn ;
				del0 -= (4 - 3) * ((sgn * b3) >> denshift) ;
				if (del0 >= 0)
					continue ;

				sgn = -sign_of_int (b2) ;
				a2 -= sgn ;
				del0 -= (4 - 2) * ((sgn * b2) >> denshift) ;
				if (del0 >= 0)
					continue ;

				sgn = -sign_of_int (b1) ;
				a1 -= sgn ;
				del0 -= (4 - 1) * ((sgn * b1) >> denshift) ;
				if (del0 >= 0)
					continue ;

				a0 += sign_of_int (b0) ;
			}
		}

		coefs [0] = a0 ;
		coefs [1] = a1 ;
		coefs [2] = a2 ;
		coefs [3] = a3 ;
	}
	else if (numactive == 8)
	{
		// optimization for numactive == 8
		register int16_t	a4, a5, a6, a7 ;
		register int32_t	b4, b5, b6, b7 ;

		a0 = coefs [0] ;
		a1 = coefs [1] ;
		a2 = coefs [2] ;
		a3 = coefs [3] ;
		a4 = coefs [4] ;
		a5 = coefs [5] ;
		a6 = coefs [6] ;
		a7 = coefs [7] ;

		for (j = lim ; j < num ; j++)
		{
			LOOP_ALIGN

			top = in [j - lim] ;
			pin = in + j - 1 ;

			b0 = top - (*pin--) ;
			b1 = top - (*pin--) ;
			b2 = top - (*pin--) ;
			b3 = top - (*pin--) ;
			b4 = top - (*pin--) ;
			b5 = top - (*pin--) ;
			b6 = top - (*pin--) ;
			b7 = top - (*pin) ;
			pin += 8 ;

			sum1 = (denhalf - a0 * b0 - a1 * b1 - a2 * b2 - a3 * b3
					- a4 * b4 - a5 * b5 - a6 * b6 - a7 * b7) >> denshift ;

			del = in [j] - top - sum1 ;
			del = arith_shift_left (del, chanshift) >> chanshift ;
			pc1 [j] = del ;
			del0 = del ;

			sg = sign_of_int (del) ;
			if (sg > 0)
			{
				sgn = sign_of_int (b7) ;
				a7 -= sgn ;
				del0 -= 1 * ((sgn * b7) >> denshift) ;
				if (del0 <= 0)
					continue ;

				sgn = sign_of_int (b6) ;
				a6 -= sgn ;
				del0 -= 2 * ((sgn * b6) >> denshift) ;
				if (del0 <= 0)
					continue ;

				sgn = sign_of_int (b5) ;
				a5 -= sgn ;
				del0 -= 3 * ((sgn * b5) >> denshift) ;
				if (del0 <= 0)
					continue ;

				sgn = sign_of_int (b4) ;
				a4 -= sgn ;
				del0 -= 4 * ((sgn * b4) >> denshift) ;
				if (del0 <= 0)
					continue ;

				sgn = sign_of_int (b3) ;
				a3 -= sgn ;
				del0 -= 5 * ((sgn * b3) >> denshift) ;
				if (del0 <= 0)
					continue ;

				sgn = sign_of_int (b2) ;
				a2 -= sgn ;
				del0 -= 6 * ((sgn * b2) >> denshift) ;
				if (del0 <= 0)
					continue ;

				sgn = sign_of_int (b1) ;
				a1 -= sgn ;
				del0 -= 7 * ((sgn * b1) >> denshift) ;
				if (del0 <= 0)
					continue ;

				a0 -= sign_of_int (b0) ;
			}
			else if (sg < 0)
			{
				// note: to avoid unnecessary negations, we flip the value of "sgn"
				sgn = -sign_of_int (b7) ;
				a7 -= sgn ;
				del0 -= 1 * ((sgn * b7) >> denshift) ;
				if (del0 >= 0)
					continue ;

				sgn = -sign_of_int (b6) ;
				a6 -= sgn ;
				del0 -= 2 * ((sgn * b6) >> denshift) ;
				if (del0 >= 0)
					continue ;

				sgn = -sign_of_int (b5) ;
				a5 -= sgn ;
				del0 -= 3 * ((sgn * b5) >> denshift) ;
				if (del0 >= 0)
					continue ;

				sgn = -sign_of_int (b4) ;
				a4 -= sgn ;
				del0 -= 4 * ((sgn * b4) >> denshift) ;
				if (del0 >= 0)
					continue ;

				sgn = -sign_of_int (b3) ;
				a3 -= sgn ;
				del0 -= 5 * ((sgn * b3) >> denshift) ;
				if (del0 >= 0)
					continue ;

				sgn = -sign_of_int (b2) ;
				a2 -= sgn ;
				del0 -= 6 * ((sgn * b2) >> denshift) ;
				if (del0 >= 0)
					continue ;

				sgn = -sign_of_int (b1) ;
				a1 -= sgn ;
				del0 -= 7 * ((sgn * b1) >> denshift) ;
				if (del0 >= 0)
					continue ;

				a0 += sign_of_int (b0) ;
			}
		}

		coefs [0] = a0 ;
		coefs [1] = a1 ;
		coefs [2] = a2 ;
		coefs [3] = a3 ;
		coefs [4] = a4 ;
		coefs [5] = a5 ;
		coefs [6] = a6 ;
		coefs [7] = a7 ;
	}
	else
	{
//pc_block_general:
		// general case
		for (j = lim ; j < num ; j++)
		{
			LOOP_ALIGN

			top = in [j - lim] ;
			pin = in + j - 1 ;

			sum1 = 0 ;
			for (k = 0 ; k < numactive ; k++)
				sum1 -= coefs [k] * (top - pin [-k]) ;

			del = in [j] - top - ((sum1 + denhalf) >> denshift) ;
			del = (del << chanshift) >> chanshift ;
			pc1 [j] = del ;
			del0 = del ;

			sg = sign_of_int (del) ;
			if (sg > 0)
			{
				for (k = (numactive - 1) ; k >= 0 ; k--)
				{
					dd = top - pin [-k] ;
					sgn = sign_of_int (dd) ;
					coefs [k] -= sgn ;
					del0 -= (numactive - k) * ((sgn * dd) >> denshift) ;
					if (del0 <= 0)
						break ;
				}
			}
			else if (sg < 0)
			{
				for (k = (numactive - 1) ; k >= 0 ; k--)
				{
					dd = top - pin [-k] ;
					sgn = sign_of_int (dd) ;
					coefs [k] += sgn ;
					del0 -= (numactive - k) * ((-sgn * dd) >> denshift) ;
					if (del0 >= 0)
						break ;
				}
			}
		}
	}
}
