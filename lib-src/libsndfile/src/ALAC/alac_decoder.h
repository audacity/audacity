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
	File:		alac_decoder.h
*/

#ifndef ALAC_DECODER_H
#define ALAC_DECODER_H

#include <stdint.h>

#include "ALACAudioTypes.h"

typedef enum
{
	false = 0,
	ALAC_TRUE = 1
} bool ;

struct BitBuffer ;

typedef struct alac_decoder
{
		// decoding parameters (public for use in the analyzer)
		ALACSpecificConfig		mConfig ;

		uint16_t					mActiveElements ;

		// decoding buffers
		int32_t *				mMixBufferU ;
		int32_t *				mMixBufferV ;
		int32_t *				mPredictor ;
		uint16_t *				mShiftBuffer ;	// note: this points to mPredictor's memory but different
												//		 variable for clarity and type difference
} alac_decoder ;

alac_decoder * alac_decoder_new (void) ;
void alac_decoder_delete (alac_decoder *) ;

int32_t	alac_init (alac_decoder *p, void * inMagicCookie, uint32_t inMagicCookieSize) ;
int32_t	alac_decode (alac_decoder *, struct BitBuffer * bits, uint8_t * sampleBuffer, uint32_t numSamples, uint32_t numChannels, uint32_t * outNumSamples) ;

#endif	/* ALAC_DECODER_H */
