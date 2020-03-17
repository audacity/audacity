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
	File:		ag_dec.c

	Contains:   Adaptive Golomb decode routines.

	Copyright:	(c) 2001-2011 Apple, Inc.
*/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "aglib.h"
#include "ALACBitUtilities.h"
#include "ALACAudioTypes.h"

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

void set_standard_ag_params (AGParamRecPtr params, uint32_t fullwidth, uint32_t sectorwidth)
{
	/* Use
		fullwidth = sectorwidth = numOfSamples, for analog 1-dimensional type-short data,
		but use
		fullwidth = full image width, sectorwidth = sector (patch) width
		for such as image (2-dim.) data.
	*/
	set_ag_params (params, MB0, PB0, KB0, fullwidth, sectorwidth, MAX_RUN_DEFAULT) ;
}

void set_ag_params (AGParamRecPtr params, uint32_t m, uint32_t p, uint32_t k, uint32_t f, uint32_t s, uint32_t maxrun)
{
	params->mb = params->mb0 = m ;
	params->pb = p ;
	params->kb = k ;
	params->wb = (1u << params->kb) - 1 ;
	params->qb = QB-params->pb ;
	params->fw = f ;
	params->sw = s ;
	params->maxrun = maxrun ;
}

#if PRAGMA_MARK
#pragma mark -
#endif


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

#define arithmin(a, b) ((a) < (b) ? (a) : (b))

static inline int32_t ALWAYS_INLINE lg3a (int32_t x)
{
	int32_t result ;

	x += 3 ;
	result = lead (x) ;

	return 31 - result ;
}

static inline uint32_t ALWAYS_INLINE read32bit (uint8_t * buffer)
{
	// embedded CPUs typically can't read unaligned 32-bit words so just read the bytes
	uint32_t		value ;

	value = ((uint32_t) buffer [0] << 24) | ((uint32_t) buffer [1] << 16) |
				((uint32_t) buffer [2] << 8) | (uint32_t) buffer [3] ;
	return value ;

}

#if PRAGMA_MARK
#pragma mark -
#endif

#define get_next_fromlong(inlong, suff)		((inlong) >> (32 - (suff)))


static inline uint32_t ALWAYS_INLINE
getstreambits (uint8_t *in, int32_t bitoffset, int32_t numbits)
{
	uint32_t	load1, load2 ;
	uint32_t	byteoffset = bitoffset / 8 ;
	uint32_t	result ;

	//Assert (numbits <= 32) ;

	load1 = read32bit (in + byteoffset) ;

	if ((numbits + (bitoffset & 0x7)) > 32)
	{
		int32_t load2shift ;

		result = load1 << (bitoffset & 0x7) ;
		load2 = (uint32_t) in [byteoffset + 4] ;
		load2shift = (8 - (numbits + (bitoffset & 0x7) - 32)) ;
		load2 >>= load2shift ;
		result >>= (32 - numbits) ;
		result |= load2 ;
	}
	else
	{
		result = load1 >> (32 - numbits - (bitoffset & 7)) ;
	}

	// a shift of >= "the number of bits in the type of the value being shifted" results in undefined
	// behavior so don't try to shift by 32
	if (numbits != (sizeof (result) * 8))
		result &= ~ (0xfffffffful << numbits) ;

	return result ;
}


static inline int32_t dyn_get (unsigned char *in, uint32_t *bitPos, uint32_t m, uint32_t k)
{
	uint32_t	tempbits = *bitPos ;
	uint32_t		result ;
	uint32_t		pre = 0, v ;
	uint32_t		streamlong ;

	streamlong = read32bit (in + (tempbits >> 3)) ;
	streamlong <<= (tempbits & 7) ;

	/* find the number of bits in the prefix */
	{
		uint32_t	notI = ~streamlong ;
		pre = lead (notI) ;
	}

	if (pre >= MAX_PREFIX_16)
	{
		pre = MAX_PREFIX_16 ;
		tempbits += pre ;
		streamlong <<= pre ;
		result = get_next_fromlong (streamlong, MAX_DATATYPE_BITS_16) ;
		tempbits += MAX_DATATYPE_BITS_16 ;

	}
	else
	{
		// all of the bits must fit within the long we have loaded
		//Assert (pre+1+k <= 32) ;

		tempbits += pre ;
		tempbits += 1 ;
		streamlong <<= pre + 1 ;
		v = get_next_fromlong (streamlong, k) ;
		tempbits += k ;

		result = pre*m + v-1 ;

		if (v < 2)
		{
			result -= (v-1) ;
			tempbits -= 1 ;
		}
	}

	*bitPos = tempbits ;
	return result ;
}


static inline int32_t dyn_get_32bit (uint8_t * in, uint32_t * bitPos, int32_t m, int32_t k, int32_t maxbits)
{
	uint32_t	tempbits = *bitPos ;
	uint32_t		v ;
	uint32_t		streamlong ;
	uint32_t		result ;

	streamlong = read32bit (in + (tempbits >> 3)) ;
	streamlong <<= (tempbits & 7) ;

	/* find the number of bits in the prefix */
	{
		uint32_t notI = ~streamlong ;
		result = lead (notI) ;
	}

	if (result >= MAX_PREFIX_32)
	{
		result = getstreambits (in, tempbits+MAX_PREFIX_32, maxbits) ;
		tempbits += MAX_PREFIX_32 + maxbits ;
	}
	else
	{
		/* all of the bits must fit within the long we have loaded*/
		//Assert (k<=14) ;
		//Assert (result<MAX_PREFIX_32) ;
		//Assert (result+1+k <= 32) ;

		tempbits += result ;
		tempbits += 1 ;

		if (k != 1)
		{
			streamlong <<= result + 1 ;
			v = get_next_fromlong (streamlong, k) ;
			tempbits += k ;
			tempbits -= 1 ;
			result = result*m ;

			if (v >= 2)
			{
				result += (v-1) ;
				tempbits += 1 ;
			}
		}
	}

	*bitPos = tempbits ;

	return result ;
}

int32_t dyn_decomp (AGParamRecPtr params, BitBuffer * bitstream, int32_t * pc, int32_t numSamples, int32_t maxSize, uint32_t * outNumBits)
{
	uint8_t 		*in ;
	int32_t			*outPtr = pc ;
	uint32_t 	bitPos, startPos, maxPos ;
	uint32_t		j, m, k, n, c, mz ;
	int32_t			del, zmode ;
	uint32_t 	mb ;
	uint32_t	pb_local = params->pb ;
	uint32_t	kb_local = params->kb ;
	uint32_t	wb_local = params->wb ;
	int32_t				status ;

	RequireAction ((bitstream != NULL) && (pc != NULL) && (outNumBits != NULL), return kALAC_ParamError ;) ;
	*outNumBits = 0 ;

	in = bitstream->cur ;
	startPos = bitstream->bitIndex ;
	maxPos = bitstream->byteSize * 8 ;
	bitPos = startPos ;

	mb = params->mb0 ;
	zmode = 0 ;

	c = 0 ;
	status = ALAC_noErr ;

	while (c < (uint32_t) numSamples)
	{
		// bail if we've run off the end of the buffer
		RequireAction (bitPos < maxPos, status = kALAC_ParamError ; goto Exit ;) ;

		m = (mb) >> QBSHIFT ;
		k = lg3a (m) ;

		k = arithmin (k, kb_local) ;
		m = (1 << k) - 1 ;

		n = dyn_get_32bit (in, &bitPos, m, k, maxSize) ;

		// least significant bit is sign bit
		{
			uint32_t	ndecode = n + zmode ;
			int32_t		multiplier = - (int) (ndecode & 1) ;

			multiplier |= 1 ;
			del = ((ndecode+1) >> 1) * (multiplier) ;
		}

		*outPtr++ = del ;

		c++ ;

		mb = pb_local * (n + zmode) + mb - ((pb_local * mb) >> QBSHIFT) ;

		// update mean tracking
		if (n > N_MAX_MEAN_CLAMP)
			mb = N_MEAN_CLAMP_VAL ;

		zmode = 0 ;

		if (((mb << MMULSHIFT) < QB) && (c < (uint32_t) numSamples))
		{
			zmode = 1 ;
			k = lead (mb) - BITOFF + ((mb + MOFF) >> MDENSHIFT) ;
			mz = ((1 << k) - 1) & wb_local ;

			n = dyn_get (in, &bitPos, mz, k) ;

			RequireAction (c+n <= (uint32_t) numSamples, status = kALAC_ParamError ; goto Exit ;) ;

			for (j = 0 ; j < n ; j++)
			{
				*outPtr++ = 0 ;
				++c ;
			}

			if (n >= 65535)
				zmode = 0 ;

			mb = 0 ;
		}
	}

Exit:
	*outNumBits = (bitPos - startPos) ;
	BitBufferAdvance (bitstream, *outNumBits) ;
	RequireAction (bitstream->cur <= bitstream->end, status = kALAC_ParamError ;) ;

	return status ;
}
