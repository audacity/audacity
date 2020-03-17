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
	File:		aglib.h

	Copyright:	(C) 2001-2011 Apple, Inc.
*/

#ifndef AGLIB_H
#define AGLIB_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define QBSHIFT		9
#define QB			(1 << QBSHIFT)
#define PB0			40
#define MB0			10
#define KB0			14
#define MAX_RUN_DEFAULT 255

#define MMULSHIFT	2
#define MDENSHIFT	(QBSHIFT - MMULSHIFT - 1)
#define MOFF		((1 << (MDENSHIFT - 2)))

#define BITOFF 24

/* Max. prefix of 1's. */
#define MAX_PREFIX_16			9
#define MAX_PREFIX_TOLONG_16	15
#define MAX_PREFIX_32			9

/* Max. bits in 16-bit data type */
#define MAX_DATATYPE_BITS_16	16

typedef struct AGParamRec
{
	uint32_t mb, mb0, pb, kb, wb, qb ;
	uint32_t fw, sw ;

	uint32_t maxrun ;

	// fw = 1, sw = 1 ;

} AGParamRec, *AGParamRecPtr ;

struct BitBuffer ;

void	set_standard_ag_params (AGParamRecPtr params, uint32_t fullwidth, uint32_t sectorwidth) ;
void	set_ag_params (AGParamRecPtr params, uint32_t m, uint32_t p, uint32_t k, uint32_t f, uint32_t s, uint32_t maxrun) ;

int32_t	dyn_comp (AGParamRecPtr params, int32_t * pc, struct BitBuffer * bitstream, int32_t numSamples, int32_t bitSize, uint32_t * outNumBits) ;
int32_t	dyn_decomp (AGParamRecPtr params, struct BitBuffer * bitstream, int32_t * pc, int32_t numSamples, int32_t maxSize, uint32_t * outNumBits) ;


#ifdef __cplusplus
}
#endif

#endif //#ifndef AGLIB_H
