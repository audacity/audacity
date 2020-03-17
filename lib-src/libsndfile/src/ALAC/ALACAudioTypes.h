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
	File:		ALACAudioTypes.h
*/

#ifndef ALACAUDIOTYPES_H
#define ALACAUDIOTYPES_H

/* Force these Mac OS specific things to zero. */
#define PRAGMA_STRUCT_ALIGN 0
#define PRAGMA_STRUCT_PACKPUSH 0
#define PRAGMA_STRUCT_PACK 0
#define PRAGMA_ONCE 0
#define PRAGMA_MARK 0


#if PRAGMA_ONCE
#pragma once
#endif

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#include "sfendian.h"

#if CPU_IS_BIG_ENDIAN == 1
#define TARGET_RT_BIG_ENDIAN 1
#else
#define TARGET_RT_BIG_ENDIAN 0
#endif

#define kChannelAtomSize 12

enum
{
	kALAC_UnimplementedError	= -4,
	kALAC_FileNotFoundError		= -43,
	kALAC_ParamError			= -50,
	kALAC_MemFullError			= -108,
	fALAC_FrameLengthError		= -666,

	/* Add for libsndfile */
	kALAC_BadBitWidth			= -0x100000,
	kALAC_IncompatibleVersion	= -0x100001,
	kALAC_BadSpecificConfigSize	= -0x100002,
	kALAC_ZeroChannelCount		= -0x100003,
	kALAC_NumSamplesTooBig		= -0x100004,
	kALAC_UnsupportedElement	= -0x100005,
} ;

enum
{
	kALACFormatAppleLossless = MAKE_MARKER ('a', 'l', 'a', 'c'),
	kALACFormatLinearPCM = MAKE_MARKER ('l', 'p', 'c', 'm')
} ;

enum
{
	kALACMaxChannels			= 8,
	kALACMaxEscapeHeaderBytes	= 8,
	kALACMaxSearches			= 16,
	kALACMaxCoefs				= 16,
	kALACDefaultFramesPerPacket = 4096
} ;

typedef uint32_t ALACChannelLayoutTag ;

enum
{
	kALACFormatFlagIsFloat					= (1 << 0),	// 0x1
	kALACFormatFlagIsBigEndian				= (1 << 1),	// 0x2
	kALACFormatFlagIsSignedInteger			= (1 << 2),	// 0x4
	kALACFormatFlagIsPacked					= (1 << 3),	// 0x8
	kALACFormatFlagIsAlignedHigh			= (1 << 4),	// 0x10
} ;

enum
{
#if TARGET_RT_BIG_ENDIAN
	kALACFormatFlagsNativeEndian	= kALACFormatFlagIsBigEndian
#else
	kALACFormatFlagsNativeEndian	= 0
#endif
} ;

// this is required to be an IEEE 64bit float
typedef double alac_float64_t ;

// These are the Channel Layout Tags used in the Channel Layout Info portion of the ALAC magic cookie
enum
{	kALACChannelLayoutTag_Mono			= (100 << 16) | 1,	// C
	kALACChannelLayoutTag_Stereo		= (101 << 16) | 2,	// L R
	kALACChannelLayoutTag_MPEG_3_0_B	= (113 << 16) | 3,	// C L R
	kALACChannelLayoutTag_MPEG_4_0_B	= (116 << 16) | 4,	// C L R Cs
	kALACChannelLayoutTag_MPEG_5_0_D	= (120 << 16) | 5,	// C L R Ls Rs
	kALACChannelLayoutTag_MPEG_5_1_D	= (124 << 16) | 6,	// C L R Ls Rs LFE
	kALACChannelLayoutTag_AAC_6_1		= (142 << 16) | 7,	// C L R Ls Rs Cs LFE
	kALACChannelLayoutTag_MPEG_7_1_B	= (127 << 16) | 8	// C Lc Rc L R Ls Rs LFE	(doc: IS-13818-7 MPEG2-AAC)
} ;

// ALAC currently only utilizes these channels layouts. There is a one for one correspondance between a
// given number of channels and one of these layout tags
static const ALACChannelLayoutTag	ALACChannelLayoutTags [kALACMaxChannels] =
{
	kALACChannelLayoutTag_Mono,			// C
	kALACChannelLayoutTag_Stereo,		// L R
	kALACChannelLayoutTag_MPEG_3_0_B,	// C L R
	kALACChannelLayoutTag_MPEG_4_0_B,	// C L R Cs
	kALACChannelLayoutTag_MPEG_5_0_D,	// C L R Ls Rs
	kALACChannelLayoutTag_MPEG_5_1_D,	// C L R Ls Rs LFE
	kALACChannelLayoutTag_AAC_6_1,		// C L R Ls Rs Cs LFE
	kALACChannelLayoutTag_MPEG_7_1_B	// C Lc Rc L R Ls Rs LFE	(doc: IS-13818-7 MPEG2-AAC)
} ;

// AudioChannelLayout from CoreAudioTypes.h. We never need the AudioChannelDescription so we remove it
struct ALACAudioChannelLayout
{	ALACChannelLayoutTag		mChannelLayoutTag ;
	uint32_t					mChannelBitmap ;
	uint32_t					mNumberChannelDescriptions ;
} ;
typedef struct ALACAudioChannelLayout ALACAudioChannelLayout ;

struct AudioFormatDescription
{
	alac_float64_t mSampleRate ;
	uint32_t mFormatID ;
	uint32_t mFormatFlags ;
	uint32_t mBytesPerPacket ;
	uint32_t mFramesPerPacket ;
	uint32_t mBytesPerFrame ;
	uint32_t mChannelsPerFrame ;
	uint32_t mBitsPerChannel ;
	uint32_t mReserved ;
} ;
typedef struct AudioFormatDescription AudioFormatDescription ;

/* Lossless Definitions */

enum
{
	kALACCodecFormat		= MAKE_MARKER ('a', 'l', 'a', 'c'),
	kALACVersion			= 0,
	kALACCompatibleVersion	= kALACVersion,
	kALACDefaultFrameSize	= 4096
} ;

// note: this struct is wrapped in an 'alac' atom in the sample description extension area
// note: in QT movies, it will be further wrapped in a 'wave' atom surrounded by 'frma' and 'term' atoms
typedef struct ALACSpecificConfig
{
	uint32_t				frameLength ;
	uint8_t					compatibleVersion ;
	uint8_t					bitDepth ;							// max 32
	uint8_t					pb ;									// 0 <= pb <= 255
	uint8_t					mb ;
	uint8_t					kb ;
	uint8_t					numChannels ;
	uint16_t				maxRun ;
	uint32_t				maxFrameBytes ;
	uint32_t				avgBitRate ;
	uint32_t				sampleRate ;

} ALACSpecificConfig ;


// The AudioChannelLayout atom type is not exposed yet so define it here
enum
{
	AudioChannelLayoutAID = MAKE_MARKER ('c', 'h', 'a', 'n')
} ;

#if PRAGMA_STRUCT_ALIGN
	#pragma options align = reset
#elif PRAGMA_STRUCT_PACKPUSH
	#pragma pack (pop)
#elif PRAGMA_STRUCT_PACK
	#pragma pack ()
#endif

#ifdef __cplusplus
}
#endif

#endif	/* ALACAUDIOTYPES_H */
