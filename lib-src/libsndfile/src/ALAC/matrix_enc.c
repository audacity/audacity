/*
 * Copyright (c) 2011 Apple Inc. All rights reserved.
 * Copyright (C) 2012-2014 Erik de Castro Lopo <erikd@mega-nerd.com>
 *
 * @APPLE_APACHE_LICENSE_HEADER_START@
 *
 * Licensed under the Apache License, Version 2.0 (the "License") ;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
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
	File:		matrix_enc.c

	Contains:	ALAC mixing/matrixing encode routines.

	Copyright:	(c) 2004-2011 Apple, Inc.
*/

#include "matrixlib.h"
#include "ALACAudioTypes.h"

/*
    There is no plain middle-side option ; instead there are various mixing
    modes including middle-side, each lossless, as embodied in the mix ()
    and unmix () functions.  These functions exploit a generalized middle-side
    transformation:

    u := [(rL + (m-r)R)/m] ;
    v := L - R ;

    where [ ] denotes integer floor.  The (lossless) inverse is

    L = u + v - [rV/m] ;
    R = L - v ;
*/

// 16-bit routines

void
mix16 (const int32_t * in, uint32_t stride, int32_t * u, int32_t * v, int32_t numSamples, int32_t mixbits, int32_t mixres)
{
	int32_t		j ;

	if (mixres != 0)
	{
		int32_t		mod = 1 << mixbits ;
		int32_t		m2 ;

		/* matrixed stereo */
		m2 = mod - mixres ;
		for (j = 0 ; j < numSamples ; j++)
		{
			int32_t		l, r ;

			l = in [0] >> 16 ;
			r = in [1] >> 16 ;
			in += stride ;
			u [j] = (mixres * l + m2 * r) >> mixbits ;
			v [j] = l - r ;
		}
	}
	else
	{
		/* Conventional separated stereo. */
		for (j = 0 ; j < numSamples ; j++)
		{
			u [j] = in [0] >> 16 ;
			v [j] = in [1] >> 16 ;
			in += stride ;
		}
	}
}

// 20-bit routines
// - the 20 bits of data are left-justified in 3 bytes of storage but right-aligned for input/output predictor buffers

void
mix20 (const int32_t * in, uint32_t stride, int32_t * u, int32_t * v, int32_t numSamples, int32_t mixbits, int32_t mixres)
{
	int32_t		l, r ;
	int32_t		j ;

	if (mixres != 0)
	{
		/* matrixed stereo */
		int32_t		mod = 1 << mixbits ;
		int32_t		m2 = mod - mixres ;

		for (j = 0 ; j < numSamples ; j++)
		{
			l = in [0] >> 12 ;
			r = in [1] >> 12 ;
			in += stride ;

			u [j] = (mixres * l + m2 * r) >> mixbits ;
			v [j] = l - r ;
		}
	}
	else
	{
		/* Conventional separated stereo. */
		for (j = 0 ; j < numSamples ; j++)
		{
			u [j] = in [0] >> 12 ;
			v [j] = in [1] >> 12 ;
			in += stride ;
		}
	}
}

// 24-bit routines
// - the 24 bits of data are right-justified in the input/output predictor buffers

void
mix24 (const int32_t * in, uint32_t stride, int32_t * u, int32_t * v, int32_t numSamples,
			int32_t mixbits, int32_t mixres, uint16_t * shiftUV, int32_t bytesShifted)
{
	int32_t		l, r ;
	int32_t		shift = bytesShifted * 8 ;
	uint32_t	mask = (1ul << shift) - 1 ;
	int32_t		j, k ;

	if (mixres != 0)
	{
		/* matrixed stereo */
		int32_t		mod = 1 << mixbits ;
		int32_t		m2 = mod - mixres ;

		if (bytesShifted != 0)
		{
			for (j = 0, k = 0 ; j < numSamples ; j++, k += 2)
			{
				l = in [0] >> 8 ;
				r = in [1] >> 8 ;
				in += stride ;

				shiftUV [k + 0] = (uint16_t) (l & mask) ;
				shiftUV [k + 1] = (uint16_t) (r & mask) ;

				l >>= shift ;
				r >>= shift ;

				u [j] = (mixres * l + m2 * r) >> mixbits ;
				v [j] = l - r ;
			}
		}
		else
		{
			for (j = 0 ; j < numSamples ; j++)
			{
				l = in [0] >> 8 ;
				r = in [1] >> 8 ;
				in += stride ;

				u [j] = (mixres * l + m2 * r) >> mixbits ;
				v [j] = l - r ;
			}
		}
	}
	else
	{
		/* Conventional separated stereo. */
		if (bytesShifted != 0)
		{
			for (j = 0, k = 0 ; j < numSamples ; j++, k += 2)
			{
				l = in [0] >> 8 ;
				r = in [1] >> 8 ;
				in += stride ;

				shiftUV [k + 0] = (uint16_t) (l & mask) ;
				shiftUV [k + 1] = (uint16_t) (r & mask) ;

				l >>= shift ;
				r >>= shift ;

				u [j] = l ;
				v [j] = r ;
			}
		}
		else
		{
			for (j = 0 ; j < numSamples ; j++)
			{
				l = in [0] >> 8 ;
				r = in [1] >> 8 ;
				in += stride ;
			}
		}
	}
}

// 32-bit routines
// - note that these really expect the internal data width to be < 32 but the arrays are 32-bit
// - otherwise, the calculations might overflow into the 33rd bit and be lost
// - therefore, these routines deal with the specified "unused lower" bytes in the "shift" buffers

void
mix32 (const int32_t * in, uint32_t stride, int32_t * u, int32_t * v, int32_t numSamples,
			int32_t mixbits, int32_t mixres, uint16_t * shiftUV, int32_t bytesShifted)
{
	int32_t		shift = bytesShifted * 8 ;
	uint32_t	mask = (1ul << shift) - 1 ;
	int32_t		l, r ;
	int32_t		j, k ;

	if (mixres != 0)
	{
		int32_t		mod = 1 << mixbits ;
		int32_t		m2 ;

		//Assert (bytesShifted != 0) ;

		/* matrixed stereo with shift */
		m2 = mod - mixres ;
		for (j = 0, k = 0 ; j < numSamples ; j++, k += 2)
		{
			l = in [0] ;
			r = in [1] ;
			in += stride ;

			shiftUV [k + 0] = (uint16_t) (l & mask) ;
			shiftUV [k + 1] = (uint16_t) (r & mask) ;

			l >>= shift ;
			r >>= shift ;

			u [j] = (mixres * l + m2 * r) >> mixbits ;
			v [j] = l - r ;
		}
	}
	else
	{
		if (bytesShifted == 0)
		{
			/* de-interleaving w/o shift */
			for (j = 0 ; j < numSamples ; j++)
			{
				u [j] = in [0] ;
				v [j] = in [1] ;
				in += stride ;
			}
		}
		else
		{
			/* de-interleaving with shift */
			for (j = 0, k = 0 ; j < numSamples ; j++, k += 2)
			{
				l = in [0] ;
				r = in [1] ;
				in += stride ;

				shiftUV [k + 0] = (uint16_t) (l & mask) ;
				shiftUV [k + 1] = (uint16_t) (r & mask) ;

				l >>= shift ;
				r >>= shift ;

				u [j] = l ;
				v [j] = r ;
			}
		}
	}
}
