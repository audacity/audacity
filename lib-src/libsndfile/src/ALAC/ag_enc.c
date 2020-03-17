/*
 * Copyright (c) 2011 Apple Inc. All rights reserved.
 * Copyright (C) 2013-2014 Erik de Castro Lopo <erikd@mega-nerd.com>
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
	File:		ag_enc.c

	Contains:   Adaptive Golomb encode routines.

	Copyright:	(c) 2001-2011 Apple, Inc.
*/

#include "aglib.h"
#include "ALACBitUtilities.h"
#include "EndianPortable.h"
#include "ALACAudioTypes.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define CODE_TO_LONG_MAXBITS	32
#define N_MAX_MEAN_CLAMP		0xffff
#define N_MEAN_CLAMP_VAL		0xffff
#define REPORT_VAL				40

#if __GNUC__
#define ALWAYS_INLINE		__attribute__ ((always_inline))
#else
#define ALWAYS_INLINE
#endif


/*	And on the subject of the CodeWarrior x86 compiler and inlining, I reworked a lot of this
	to help the compiler out.   In many cases this required manual inlining or a macro.  Sorry
	if it is ugly but the performance gains are well worth it.
	- WSK 5/19/04
*/

// note: implementing this with some kind of "count leading zeros" assembly is a big performance win
static inline int32_t lead (int32_t m)
{
	long j ;
	unsigned long c = (1ul << 31) ;

	for (j = 0 ; j < 32 ; j++)
	{
		if ((c & m) != 0)
			break ;
		c >>= 1 ;
	}
	return j ;
}

#define arithmin (a, b) ((a) < (b) ? (a) : (b))

static inline int32_t ALWAYS_INLINE lg3a (int32_t x)
{
	int32_t result ;

	x += 3 ;
	result = lead (x) ;

	return 31 - result ;
}

static inline int32_t ALWAYS_INLINE abs_func (int32_t a)
{
	// note: the CW PPC intrinsic __abs () turns into these instructions so no need to try and use it
	int32_t isneg = a >> 31 ;
	int32_t xorval = a ^ isneg ;
	int32_t result = xorval-isneg ;

	return result ;
}

#if PRAGMA_MARK
#pragma mark -
#endif

static inline int32_t dyn_code (int32_t m, int32_t k, int32_t n, uint32_t *outNumBits)
{
	uint32_t 	divx, mod, de ;
	uint32_t	numBits ;
	uint32_t	value ;

	// Assert (n >= 0) ;

	divx = n / m ;

	if (divx >= MAX_PREFIX_16)
	{
		numBits = MAX_PREFIX_16 + MAX_DATATYPE_BITS_16 ;
		value = (((1 << MAX_PREFIX_16) - 1) << MAX_DATATYPE_BITS_16) + n ;
	}
	else
	{
		mod = n%m ;
		de = (mod == 0) ;
		numBits = divx + k + 1 - de ;
		value = (((1 << divx) - 1) << (numBits - divx)) + mod + 1 - de ;

		// if coding this way is bigger than doing escape, then do escape
		if (numBits > MAX_PREFIX_16 + MAX_DATATYPE_BITS_16)
		{
			numBits = MAX_PREFIX_16 + MAX_DATATYPE_BITS_16 ;
			value = (((1 << MAX_PREFIX_16) - 1) << MAX_DATATYPE_BITS_16) + n ;
		}
	}

	*outNumBits = numBits ;

	return (int32_t) value ;
}


static inline int32_t dyn_code_32bit (int32_t maxbits, uint32_t m, uint32_t k, uint32_t n, uint32_t *outNumBits, uint32_t *outValue, uint32_t *overflow, uint32_t *overflowbits)
{
	uint32_t 	divx, mod, de ;
	uint32_t	numBits ;
	uint32_t	value ;
	int32_t			didOverflow = 0 ;

	divx = n / m ;

	if (divx < MAX_PREFIX_32)
	{
		mod = n - (m * divx) ;

		de = (mod == 0) ;
		numBits = divx + k + 1 - de ;
		value = (((1 << divx) - 1) << (numBits - divx)) + mod + 1 - de ;
		if (numBits > 25)
			goto codeasescape ;
	}
	else
	{
codeasescape:
		numBits = MAX_PREFIX_32 ;
		value = (((1 << MAX_PREFIX_32) - 1)) ;
		*overflow = n ;
		*overflowbits = maxbits ;
		didOverflow = 1 ;
	}

	*outNumBits = numBits ;
	*outValue = value ;

	return didOverflow ;
}


static inline void ALWAYS_INLINE dyn_jam_noDeref (unsigned char *out, uint32_t bitPos, uint32_t numBits, uint32_t value)
{
	uint32_t	mask ;
	uint32_t	curr ;
	uint32_t	shift ;

	//Assert (numBits <= 32) ;

	curr = psf_get_be32 (out, bitPos >> 3) ;

	shift = 32 - (bitPos & 7) - numBits ;

	mask = ~0u >> (32 - numBits) ;		// mask must be created in two steps to avoid compiler sequencing ambiguity
	mask <<= shift ;

	value = (value << shift) & mask ;
	value |= curr & ~mask ;

	psf_put_be32 (out, bitPos >> 3, value) ;
}


static inline void ALWAYS_INLINE dyn_jam_noDeref_large (unsigned char *out, uint32_t bitPos, uint32_t numBits, uint32_t value)
{
	uint32_t	w ;
	uint32_t	curr ;
	uint32_t	mask ;
	int32_t		shiftvalue = (32 - (bitPos & 7) - numBits) ;

	//Assert (numBits <= 32) ;

	curr = psf_get_be32 (out, bitPos >> 3) ;

	if (shiftvalue < 0)
	{
		uint8_t 	tailbyte ;
		uint8_t 	*tailptr ;

		w = value >> -shiftvalue ;
		mask = ~0u >> -shiftvalue ;
		w |= (curr & ~mask) ;

		tailptr = out + (bitPos >> 3) + 4 ;
		tailbyte = (value << ((8+shiftvalue))) & 0xff ;
		*tailptr = (uint8_t) tailbyte ;
	}
	else
	{
		mask = ~0u >> (32 - numBits) ;
		mask <<= shiftvalue ;			// mask must be created in two steps to avoid compiler sequencing ambiguity

		w = (value << shiftvalue) & mask ;
		w |= curr & ~mask ;
	}

	psf_put_be32 (out, bitPos >> 3, w) ;
}


int32_t dyn_comp (AGParamRecPtr params, int32_t * pc, BitBuffer * bitstream, int32_t numSamples, int32_t bitSize, uint32_t * outNumBits)
{
	unsigned char *		out ;
	uint32_t		bitPos, startPos ;
	uint32_t			m, k, n, c, mz, nz ;
	uint32_t		numBits ;
	uint32_t			value ;
	int32_t				del, zmode ;
	uint32_t		overflow, overflowbits ;
	int32_t					status ;

	// shadow the variables in params so there's not the dereferencing overhead
	uint32_t		mb, pb, kb, wb ;
	int32_t					rowPos = 0 ;
	int32_t					rowSize = params->sw ;
	int32_t					rowJump = (params->fw) - rowSize ;
	int32_t *			inPtr = pc ;

	*outNumBits = 0 ;
	RequireAction ((bitSize >= 1) && (bitSize <= 32), return kALAC_ParamError ;) ;

	out = bitstream->cur ;
	startPos = bitstream->bitIndex ;
	bitPos = startPos ;

	mb = params->mb = params->mb0 ;
	pb = params->pb ;
	kb = params->kb ;
	wb = params->wb ;
	zmode = 0 ;

	c = 0 ;
	status = ALAC_noErr ;

	while (c < (uint32_t) numSamples)
	{
		m = mb >> QBSHIFT ;
		k = lg3a (m) ;
		if (k > kb)
		{
			k = kb ;
		}
		m = (1 << k) - 1 ;

		del = *inPtr++ ;
		rowPos++ ;

		n = (abs_func (del) << 1) - ((del >> 31) & 1) - zmode ;
		//Assert (32-lead (n) <= bitSize) ;

		if (dyn_code_32bit (bitSize, m, k, n, &numBits, &value, &overflow, &overflowbits))
		{
			dyn_jam_noDeref (out, bitPos, numBits, value) ;
			bitPos += numBits ;
			dyn_jam_noDeref_large (out, bitPos, overflowbits, overflow) ;
			bitPos += overflowbits ;
		}
		else
		{
			dyn_jam_noDeref (out, bitPos, numBits, value) ;
			bitPos += numBits ;
		}

		c++ ;
		if (rowPos >= rowSize)
		{
			rowPos = 0 ;
			inPtr += rowJump ;
		}

		mb = pb * (n + zmode) + mb - ((pb * mb) >> QBSHIFT) ;

		// update mean tracking if it's overflowed
		if (n > N_MAX_MEAN_CLAMP)
			mb = N_MEAN_CLAMP_VAL ;

		zmode = 0 ;

		RequireAction (c <= (uint32_t) numSamples, status = kALAC_ParamError ; goto Exit ;) ;

		if (((mb << MMULSHIFT) < QB) && (c < (uint32_t) numSamples))
		{
			zmode = 1 ;
			nz = 0 ;

			while (c < (uint32_t) numSamples && *inPtr == 0)
			{
				/* Take care of wrap-around globals. */
				++inPtr ;
				++nz ;
				++c ;
				if (++rowPos >= rowSize)
				{
					rowPos = 0 ;
					inPtr += rowJump ;
				}

				if (nz >= 65535)
				{
					zmode = 0 ;
					break ;
				}
			}

			k = lead (mb) - BITOFF + ((mb + MOFF) >> MDENSHIFT) ;
			mz = ((1 << k) - 1) & wb ;

			value = dyn_code (mz, k, nz, &numBits) ;
			dyn_jam_noDeref (out, bitPos, numBits, value) ;
			bitPos += numBits ;

			mb = 0 ;
		}
	}

	*outNumBits = (bitPos - startPos) ;
	BitBufferAdvance (bitstream, *outNumBits) ;

Exit:
	return status ;
}
