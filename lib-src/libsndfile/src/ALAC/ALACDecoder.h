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
	File:		ALACDecoder.h
*/

#ifndef _ALACDECODER_H
#define _ALACDECODER_H

#include <stdint.h>

#include "ALACAudioTypes.h"

struct BitBuffer ;

class ALACDecoder
{
	public:
		ALACDecoder () ;
		~ALACDecoder () ;

		int32_t	Init (void * inMagicCookie, uint32_t inMagicCookieSize) ;
		int32_t	Decode (struct BitBuffer * bits, uint8_t * sampleBuffer, uint32_t numSamples, uint32_t numChannels, uint32_t * outNumSamples) ;

	public:
		// decoding parameters (public for use in the analyzer)
		ALACSpecificConfig		mConfig ;

	protected:
		int32_t	FillElement (struct BitBuffer * bits) ;
		int32_t	DataStreamElement (struct BitBuffer * bits) ;

		uint16_t			mActiveElements ;

		// decoding buffers
		int32_t *			mMixBufferU ;
		int32_t *			mMixBufferV ;
		int32_t *			mPredictor ;
		uint16_t *			mShiftBuffer ;	// note: this points to mPredictor's memory but different
											//		 variable for clarity and type difference
} ;

#endif	/* _ALACDECODER_H */
