/*
 * Copyright (c) 2011 Apple Inc. All rights reserved.
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
	File:		dplib.h

	Contains:	Dynamic Predictor routines

	Copyright:	Copyright (C) 2001-2011 Apple, Inc.
*/

#ifndef __DPLIB_H__
#define __DPLIB_H__

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// defines

#define DENSHIFT_MAX	15
#define DENSHIFT_DEFAULT 9
#define AINIT 38
#define BINIT (-29)
#define CINIT (-2)
#define NUMCOEPAIRS 16

// prototypes

void init_coefs (int16_t * coefs, uint32_t denshift, int32_t numPairs) ;
void copy_coefs (const int16_t * srcCoefs, int16_t * dstCoefs, int32_t numPairs) ;

// NOTE: these routines read at least "numactive" samples so the i/o buffers must be at least that big

void pc_block (int32_t * in, int32_t * pc, int32_t num, int16_t * coefs, int32_t numactive, uint32_t chanbits, uint32_t denshift) ;
void unpc_block (const int32_t * pc, int32_t * out, int32_t num, int16_t * coefs, int32_t numactive, uint32_t chanbits, uint32_t denshift) ;

#ifdef __cplusplus
}
#endif

#endif	/* __DPLIB_H__ */
