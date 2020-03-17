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
	File:		alac_codec.h
*/

#ifndef ALAC_CODEC_H
#define ALAC_CODEC_H

#include <stdint.h>

#include "ALACAudioTypes.h"

#define		ALAC_FRAME_LENGTH	4096

struct BitBuffer ;

typedef struct alac_decoder_s
{
	// decoding parameters (public for use in the analyzer)
	ALACSpecificConfig		mConfig ;

	uint16_t				mActiveElements ;

	// decoding buffers
	int32_t				mMixBufferU [ALAC_FRAME_LENGTH] ;
	int32_t				mMixBufferV [ALAC_FRAME_LENGTH] ;
	union
	{
		int32_t			mPredictor [ALAC_FRAME_LENGTH] ;
		uint16_t		mShiftBuffer [ALAC_FRAME_LENGTH] ;
	} ;
	uint32_t			mNumChannels ;
} ALAC_DECODER ;

typedef struct alac_encoder_s
{
	// ALAC encoder parameters
	int16_t			mBitDepth ;

	// encoding state
	int16_t			mLastMixRes [kALACMaxChannels] ;

	int32_t			mFastMode ;

	// encoding buffers
	int32_t			mMixBufferU [ALAC_FRAME_LENGTH] ;
	int32_t			mMixBufferV [ALAC_FRAME_LENGTH] ;
	int32_t			mPredictorU [ALAC_FRAME_LENGTH] ;
	int32_t			mPredictorV [ALAC_FRAME_LENGTH] ;
	uint16_t		mShiftBufferUV [2 * ALAC_FRAME_LENGTH] ;
	uint8_t			mWorkBuffer [4 * ALAC_FRAME_LENGTH] ;

	// per-channel coefficients buffers
	int16_t			mCoefsU [kALACMaxChannels][kALACMaxSearches][kALACMaxCoefs] ;
	int16_t			mCoefsV [kALACMaxChannels][kALACMaxSearches][kALACMaxCoefs] ;

	// encoding statistics
	uint32_t		mTotalBytesGenerated ;
	uint32_t		mAvgBitRate ;
	uint32_t		mMaxFrameBytes ;
	uint32_t		mFrameSize ;
	uint32_t		mMaxOutputBytes ;
	uint32_t		mNumChannels ;
	uint32_t		mOutputSampleRate ;
} ALAC_ENCODER ;


int32_t	alac_decoder_init (ALAC_DECODER *p, void * inMagicCookie, uint32_t inMagicCookieSize) ;
int32_t alac_encoder_init (ALAC_ENCODER *p, uint32_t samplerate, uint32_t channels, uint32_t format_flags, uint32_t frameSize) ;

int32_t	alac_decode (ALAC_DECODER *, struct BitBuffer * bits, int32_t * sampleBuffer,
					uint32_t numSamples, uint32_t * outNumSamples) ;

int32_t	alac_encode (ALAC_ENCODER *p, uint32_t numSamples,
					const int32_t * theReadBuffer, unsigned char * theWriteBuffer,
					uint32_t * ioNumBytes) ;

void alac_set_fastmode (ALAC_ENCODER * p, int32_t fast) ;

uint32_t alac_get_magic_cookie_size (uint32_t inNumChannels) ;
void	alac_get_magic_cookie (ALAC_ENCODER *p, void * config, uint32_t * ioSize) ;
void	alac_get_source_format (ALAC_ENCODER *p, const AudioFormatDescription * source, AudioFormatDescription * output) ;

#endif
