/*
 * Copyright (c) 2011 Apple Inc. All rights reserved.
 * Copyright (C) 2012-2015 Erik de Castro Lopo <erikd@mega-nerd.com>
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
	File:		ALACDecoder.cpp
*/

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>

#include "alac_codec.h"

#include "dplib.h"
#include "aglib.h"
#include "matrixlib.h"
#include "shift.h"

#include "ALACBitUtilities.h"
#include "EndianPortable.h"

typedef enum
{	false = 0,
	true = 1
} bool ;

// constants/data
const uint32_t kMaxBitDepth = 32 ;			// max allowed bit depth is 32


// prototypes
static int32_t	alac_fill_element (struct BitBuffer * bits) ;
static int32_t	alac_data_stream_element (struct BitBuffer * bits) ;

static void Zero32 (int32_t * buffer, uint32_t numItems, uint32_t stride) ;


/*
	Init ()
	- initialize the decoder with the given configuration
*/
int32_t
alac_decoder_init (ALAC_DECODER *p, void * inMagicCookie, uint32_t inMagicCookieSize)
{
	int32_t		status = ALAC_noErr ;
	ALACSpecificConfig theConfig ;
	uint8_t * theActualCookie = (uint8_t *) inMagicCookie ;
	uint32_t theCookieBytesRemaining = inMagicCookieSize ;

	// For historical reasons the decoder needs to be resilient to magic cookies vended by older encoders.
	// As specified in the ALACMagicCookieDescription.txt document, there may be additional data encapsulating
	// the ALACSpecificConfig. This would consist of format ('frma') and 'alac' atoms which precede the
	// ALACSpecificConfig.
	// See ALACMagicCookieDescription.txt for additional documentation concerning the 'magic cookie'

	// skip format ('frma') atom if present
	if (theActualCookie [4] == 'f' && theActualCookie [5] == 'r' && theActualCookie [6] == 'm' && theActualCookie [7] == 'a')
	{
		theActualCookie += 12 ;
		theCookieBytesRemaining -= 12 ;
	}

	// skip 'alac' atom header if present
	if (theActualCookie [4] == 'a' && theActualCookie [5] == 'l' && theActualCookie [6] == 'a' && theActualCookie [7] == 'c')
	{
		theActualCookie += 12 ;
		theCookieBytesRemaining -= 12 ;
	}

	// read the ALACSpecificConfig
	if (theCookieBytesRemaining >= sizeof (ALACSpecificConfig))
	{
		theConfig.frameLength = psf_get_be32 (theActualCookie, offsetof (ALACSpecificConfig, frameLength)) ;

		if (theConfig.frameLength > ALAC_FRAME_LENGTH)
			return fALAC_FrameLengthError ;

		theConfig.compatibleVersion = theActualCookie [offsetof (ALACSpecificConfig, compatibleVersion)] ;
		theConfig.bitDepth = theActualCookie [offsetof (ALACSpecificConfig, bitDepth)] ;
		theConfig.pb = theActualCookie [offsetof (ALACSpecificConfig, pb)] ;
		theConfig.mb = theActualCookie [offsetof (ALACSpecificConfig, mb)] ;
		theConfig.kb = theActualCookie [offsetof (ALACSpecificConfig, kb)] ;
		theConfig.numChannels = theActualCookie [offsetof (ALACSpecificConfig, numChannels)] ;
		theConfig.maxRun = psf_get_be16 (theActualCookie, offsetof (ALACSpecificConfig, maxRun)) ;
		theConfig.maxFrameBytes = psf_get_be32 (theActualCookie, offsetof (ALACSpecificConfig, maxFrameBytes)) ;
		theConfig.avgBitRate = psf_get_be32 (theActualCookie, offsetof (ALACSpecificConfig, avgBitRate)) ;
		theConfig.sampleRate = psf_get_be32 (theActualCookie, offsetof (ALACSpecificConfig, sampleRate)) ;

		p->mConfig = theConfig ;
		p->mNumChannels = theConfig.numChannels ;

		RequireAction (p->mConfig.compatibleVersion <= kALACVersion, return kALAC_IncompatibleVersion ;) ;
		RequireAction ((p->mConfig.bitDepth >= 8 && p->mConfig.bitDepth <= 32), return kALAC_BadBitWidth ;) ;
		RequireAction ((p->mMixBufferU != NULL) && (p->mMixBufferV != NULL) && (p->mPredictor != NULL),
						status = kALAC_MemFullError ; goto Exit ;) ;
	}
	else
	{
		status = kALAC_BadSpecificConfigSize ;
	}

	// skip to Channel Layout Info
	// theActualCookie += sizeof (ALACSpecificConfig) ;

	// Currently, the Channel Layout Info portion of the magic cookie (as defined in the
	// ALACMagicCookieDescription.txt document) is unused by the decoder.

Exit:
	return status ;
}

/*
	Decode ()
	- the decoded samples are interleaved into the output buffer in the order they arrive in
	  the bitstream
*/
int32_t
alac_decode (ALAC_DECODER *p, struct BitBuffer * bits, int32_t * sampleBuffer, uint32_t numSamples, uint32_t * outNumSamples)
{
	BitBuffer		shiftBits ;
	uint32_t		bits1, bits2 ;
	uint8_t			tag ;
	uint8_t			elementInstanceTag ;
	AGParamRec		agParams ;
	uint32_t		channelIndex ;
	int16_t			coefsU [32] ;		// max possible size is 32 although NUMCOEPAIRS is the current limit
	int16_t			coefsV [32] ;
	uint8_t			numU, numV ;
	uint8_t			mixBits ;
	int8_t			mixRes ;
	uint16_t		unusedHeader ;
	uint8_t			escapeFlag ;
	uint32_t		chanBits ;
	uint8_t			bytesShifted ;
	uint32_t		shift ;
	uint8_t			modeU, modeV ;
	uint32_t		denShiftU, denShiftV ;
	uint16_t		pbFactorU, pbFactorV ;
	uint16_t		pb ;
	int32_t *		out32 ;
	uint8_t			headerByte ;
	uint8_t			partialFrame ;
	uint32_t		extraBits ;
	int32_t			val ;
	uint32_t		i, j ;
	int32_t			status ;
	uint32_t		numChannels = p->mNumChannels ;

	RequireAction ((bits != NULL) && (sampleBuffer != NULL) && (outNumSamples != NULL), return kALAC_ParamError ;) ;
	RequireAction (p->mNumChannels > 0, return kALAC_ZeroChannelCount ;) ;

	p->mActiveElements = 0 ;
	channelIndex	= 0 ;

	status = ALAC_noErr ;
	*outNumSamples = numSamples ;

	while (status == ALAC_noErr)
	{
		// bail if we ran off the end of the buffer
		RequireAction (bits->cur < bits->end, status = kALAC_ParamError ; goto Exit ;) ;

		// copy global decode params for this element
		pb = p->mConfig.pb ;

		// read element tag
		tag = BitBufferReadSmall (bits, 3) ;
		switch (tag)
		{
			case ID_SCE:
			case ID_LFE:
			{
				// mono/LFE channel
				elementInstanceTag = BitBufferReadSmall (bits, 4) ;
				p->mActiveElements |= (1u << elementInstanceTag) ;

				// read the 12 unused header bits
				unusedHeader = (uint16_t) BitBufferRead (bits, 12) ;
				RequireAction (unusedHeader == 0, status = kALAC_ParamError ; goto Exit ;) ;

				// read the 1-bit "partial frame" flag, 2-bit "shift-off" flag & 1-bit "escape" flag
				headerByte = (uint8_t) BitBufferRead (bits, 4) ;

				partialFrame = headerByte >> 3 ;

				bytesShifted = (headerByte >> 1) & 0x3u ;
				RequireAction (bytesShifted != 3, status = kALAC_ParamError ; goto Exit ;) ;

				shift = bytesShifted * 8 ;

				escapeFlag = headerByte & 0x1 ;

				chanBits = p->mConfig.bitDepth - (bytesShifted * 8) ;

				// check for partial frame to override requested numSamples
				if (partialFrame != 0)
				{
					numSamples = BitBufferRead (bits, 16) << 16 ;
					numSamples |= BitBufferRead (bits, 16) ;

					RequireAction (numSamples < kALACDefaultFramesPerPacket, return kALAC_NumSamplesTooBig ;) ;
				}

				if (escapeFlag == 0)
				{
					// compressed frame, read rest of parameters
					mixBits	= (uint8_t) BitBufferRead (bits, 8) ;
					mixRes	= (int8_t) BitBufferRead (bits, 8) ;
					//Assert ((mixBits == 0) && (mixRes == 0)) ;		// no mixing for mono

					headerByte	= (uint8_t) BitBufferRead (bits, 8) ;
					modeU		= headerByte >> 4 ;
					denShiftU	= headerByte & 0xfu ;

					headerByte	= (uint8_t) BitBufferRead (bits, 8) ;
					pbFactorU	= headerByte >> 5 ;
					numU		= headerByte & 0x1fu ;

					for (i = 0 ; i < numU ; i++)
						coefsU [i] = (int16_t) BitBufferRead (bits, 16) ;

					// if shift active, skip the shift buffer but remember where it starts
					if (bytesShifted != 0)
					{
						shiftBits = *bits ;
						BitBufferAdvance (bits, (bytesShifted * 8) * numSamples) ;
					}

					// decompress
					set_ag_params (&agParams, p->mConfig.mb, (pb * pbFactorU) / 4, p->mConfig.kb, numSamples, numSamples, p->mConfig.maxRun) ;
					status = dyn_decomp (&agParams, bits, p->mPredictor, numSamples, chanBits, &bits1) ;
					RequireNoErr (status, goto Exit ;) ;

					if (modeU == 0)
					{
						unpc_block (p->mPredictor, p->mMixBufferU, numSamples, &coefsU [0], numU, chanBits, denShiftU) ;
					}
					else
					{
						// the special "numActive == 31" mode can be done in-place
						unpc_block (p->mPredictor, p->mPredictor, numSamples, NULL, 31, chanBits, 0) ;
						unpc_block (p->mPredictor, p->mMixBufferU, numSamples, &coefsU [0], numU, chanBits, denShiftU) ;
					}
				}
				else
				{
					//Assert (bytesShifted == 0) ;

					// uncompressed frame, copy data into the mix buffer to use common output code
					shift = 32 - chanBits ;
					if (chanBits <= 16)
					{
						for (i = 0 ; i < numSamples ; i++)
						{
							val = (int32_t) BitBufferRead (bits, (uint8_t) chanBits) ;
							val = (val << shift) >> shift ;
							p->mMixBufferU [i] = val ;
						}
					}
					else
					{
						// BitBufferRead () can't read more than 16 bits at a time so break up the reads
						extraBits = chanBits - 16 ;
						for (i = 0 ; i < numSamples ; i++)
						{
							val = (int32_t) BitBufferRead (bits, 16) ;
							val = arith_shift_left (val, 16) >> shift ;
							p->mMixBufferU [i] = val | BitBufferRead (bits, (uint8_t) extraBits) ;
						}
					}

					mixBits = mixRes = 0 ;
					bits1 = chanBits * numSamples ;
					bytesShifted = 0 ;
				}

				// now read the shifted values into the shift buffer
				if (bytesShifted != 0)
				{
					shift = bytesShifted * 8 ;
					//Assert (shift <= 16) ;

					for (i = 0 ; i < numSamples ; i++)
						p->mShiftBuffer [i] = (uint16_t) BitBufferRead (&shiftBits, (uint8_t) shift) ;
				}

				// convert 32-bit integers into output buffer
				switch (p->mConfig.bitDepth)
				{
					case 16:
						out32 = sampleBuffer + channelIndex ;
						for (i = 0, j = 0 ; i < numSamples ; i++, j += numChannels)
							out32 [j] = arith_shift_left (p->mMixBufferU [i], 16) ;
						break ;
					case 20:
						out32 = sampleBuffer + channelIndex ;
						copyPredictorTo20 (p->mMixBufferU, out32, numChannels, numSamples) ;
						break ;
					case 24:
						out32 = sampleBuffer + channelIndex ;
						if (bytesShifted != 0)
							copyPredictorTo24Shift (p->mMixBufferU, p->mShiftBuffer, out32, numChannels, numSamples, bytesShifted) ;
						else
							copyPredictorTo24 (p->mMixBufferU, out32, numChannels, numSamples) ;
						break ;
					case 32:
						out32 = sampleBuffer + channelIndex ;
						if (bytesShifted != 0)
							copyPredictorTo32Shift (p->mMixBufferU, p->mShiftBuffer, out32, numChannels, numSamples, bytesShifted) ;
						else
							copyPredictorTo32 (p->mMixBufferU, out32, numChannels, numSamples) ;
						break ;
				}

				channelIndex += 1 ;
				*outNumSamples = numSamples ;
				break ;
			}

			case ID_CPE:
			{
				// if decoding this pair would take us over the max channels limit, bail
				if ((channelIndex + 2) > numChannels)
					goto NoMoreChannels ;

				// stereo channel pair
				elementInstanceTag = BitBufferReadSmall (bits, 4) ;
				p->mActiveElements |= (1u << elementInstanceTag) ;

				// read the 12 unused header bits
				unusedHeader = (uint16_t) BitBufferRead (bits, 12) ;
				RequireAction (unusedHeader == 0, status = kALAC_ParamError ; goto Exit ;) ;

				// read the 1-bit "partial frame" flag, 2-bit "shift-off" flag & 1-bit "escape" flag
				headerByte = (uint8_t) BitBufferRead (bits, 4) ;

				partialFrame = headerByte >> 3 ;

				bytesShifted = (headerByte >> 1) & 0x3u ;
				RequireAction (bytesShifted != 3, status = kALAC_ParamError ; goto Exit ;) ;

				shift = bytesShifted * 8 ;

				escapeFlag = headerByte & 0x1 ;

				chanBits = p->mConfig.bitDepth - (bytesShifted * 8) + 1 ;

				// check for partial frame length to override requested numSamples
				if (partialFrame != 0)
				{
					numSamples = BitBufferRead (bits, 16) << 16 ;
					numSamples |= BitBufferRead (bits, 16) ;

					RequireAction (numSamples < kALACDefaultFramesPerPacket, return kALAC_NumSamplesTooBig ;) ;
				}

				if (escapeFlag == 0)
				{
					// compressed frame, read rest of parameters
					mixBits		= (uint8_t) BitBufferRead (bits, 8) ;
					mixRes		= (int8_t) BitBufferRead (bits, 8) ;

					headerByte	= (uint8_t) BitBufferRead (bits, 8) ;
					modeU		= headerByte >> 4 ;
					denShiftU	= headerByte & 0xfu ;

					headerByte	= (uint8_t) BitBufferRead (bits, 8) ;
					pbFactorU	= headerByte >> 5 ;
					numU		= headerByte & 0x1fu ;
					for (i = 0 ; i < numU ; i++)
						coefsU [i] = (int16_t) BitBufferRead (bits, 16) ;

					headerByte	= (uint8_t) BitBufferRead (bits, 8) ;
					modeV		= headerByte >> 4 ;
					denShiftV	= headerByte & 0xfu ;

					headerByte	= (uint8_t) BitBufferRead (bits, 8) ;
					pbFactorV	= headerByte >> 5 ;
					numV		= headerByte & 0x1fu ;
					for (i = 0 ; i < numV ; i++)
						coefsV [i] = (int16_t) BitBufferRead (bits, 16) ;

					// if shift active, skip the interleaved shifted values but remember where they start
					if (bytesShifted != 0)
					{
						shiftBits = *bits ;
						BitBufferAdvance (bits, (bytesShifted * 8) * 2 * numSamples) ;
					}

					// decompress and run predictor for "left" channel
					set_ag_params (&agParams, p->mConfig.mb, (pb * pbFactorU) / 4, p->mConfig.kb, numSamples, numSamples, p->mConfig.maxRun) ;
					status = dyn_decomp (&agParams, bits, p->mPredictor, numSamples, chanBits, &bits1) ;
					RequireNoErr (status, goto Exit ;) ;

					if (modeU == 0)
					{
						unpc_block (p->mPredictor, p->mMixBufferU, numSamples, &coefsU [0], numU, chanBits, denShiftU) ;
					}
					else
					{
						// the special "numActive == 31" mode can be done in-place
						unpc_block (p->mPredictor, p->mPredictor, numSamples, NULL, 31, chanBits, 0) ;
						unpc_block (p->mPredictor, p->mMixBufferU, numSamples, &coefsU [0], numU, chanBits, denShiftU) ;
					}

					// decompress and run predictor for "right" channel
					set_ag_params (&agParams, p->mConfig.mb, (pb * pbFactorV) / 4, p->mConfig.kb, numSamples, numSamples, p->mConfig.maxRun) ;
					status = dyn_decomp (&agParams, bits, p->mPredictor, numSamples, chanBits, &bits2) ;
					RequireNoErr (status, goto Exit ;) ;

					if (modeV == 0)
					{
						unpc_block (p->mPredictor, p->mMixBufferV, numSamples, &coefsV [0], numV, chanBits, denShiftV) ;
					}
					else
					{
						// the special "numActive == 31" mode can be done in-place
						unpc_block (p->mPredictor, p->mPredictor, numSamples, NULL, 31, chanBits, 0) ;
						unpc_block (p->mPredictor, p->mMixBufferV, numSamples, &coefsV [0], numV, chanBits, denShiftV) ;
					}
				}
				else
				{
					//Assert (bytesShifted == 0) ;

					// uncompressed frame, copy data into the mix buffers to use common output code
					chanBits = p->mConfig.bitDepth ;
					shift = 32 - chanBits ;
					if (chanBits <= 16)
					{
						for (i = 0 ; i < numSamples ; i++)
						{
							val = (int32_t) BitBufferRead (bits, (uint8_t) chanBits) ;
							val = (val << shift) >> shift ;
							p->mMixBufferU [i] = val ;

							val = (int32_t) BitBufferRead (bits, (uint8_t) chanBits) ;
							val = (val << shift) >> shift ;
							p->mMixBufferV [i] = val ;
						}
					}
					else
					{
						// BitBufferRead () can't read more than 16 bits at a time so break up the reads
						extraBits = chanBits - 16 ;
						for (i = 0 ; i < numSamples ; i++)
						{
							val = (int32_t) BitBufferRead (bits, 16) ;
							val = (((uint32_t) val) << 16) >> shift ;
							p->mMixBufferU [i] = val | BitBufferRead (bits, (uint8_t) extraBits) ;

							val = (int32_t) BitBufferRead (bits, 16) ;
							val = ((uint32_t) val) >> shift ;
							p->mMixBufferV [i] = val | BitBufferRead (bits, (uint8_t) extraBits) ;
						}
					}

					bits1 = chanBits * numSamples ;
					bits2 = chanBits * numSamples ;
					mixBits = mixRes = 0 ;
					bytesShifted = 0 ;
				}

				// now read the shifted values into the shift buffer
				if (bytesShifted != 0)
				{
					shift = bytesShifted * 8 ;
					//Assert (shift <= 16) ;

					for (i = 0 ; i < (numSamples * 2) ; i += 2)
					{
						p->mShiftBuffer [i + 0] = (uint16_t) BitBufferRead (&shiftBits, (uint8_t) shift) ;
						p->mShiftBuffer [i + 1] = (uint16_t) BitBufferRead (&shiftBits, (uint8_t) shift) ;
					}
				}

				// un-mix the data and convert to output format
				// - note that mixRes = 0 means just interleave so we use that path for uncompressed frames
				switch (p->mConfig.bitDepth)
				{
					case 16:
						out32 = sampleBuffer + channelIndex ;
						unmix16 (p->mMixBufferU, p->mMixBufferV, out32, numChannels, numSamples, mixBits, mixRes) ;
						break ;
					case 20:
						out32 = sampleBuffer + channelIndex ;
						unmix20 (p->mMixBufferU, p->mMixBufferV, out32, numChannels, numSamples, mixBits, mixRes) ;
						break ;
					case 24:
						out32 = sampleBuffer + channelIndex ;
						unmix24 (p->mMixBufferU, p->mMixBufferV, out32, numChannels, numSamples,
									mixBits, mixRes, p->mShiftBuffer, bytesShifted) ;
						break ;
					case 32:
						out32 = sampleBuffer + channelIndex ;
						unmix32 (p->mMixBufferU, p->mMixBufferV, out32, numChannels, numSamples,
									mixBits, mixRes, p->mShiftBuffer, bytesShifted) ;
						break ;
				}

				channelIndex += 2 ;
				*outNumSamples = numSamples ;
				break ;
			}

			case ID_CCE:
			case ID_PCE:
			{
				// unsupported element, bail
				//AssertNoErr (tag) ;
				status = kALAC_UnsupportedElement ;
				break ;
			}

			case ID_DSE:
			{
				// data stream element -- parse but ignore
				status = alac_data_stream_element (bits) ;
				break ;
			}

			case ID_FIL:
			{
				// fill element -- parse but ignore
				status = alac_fill_element (bits) ;
				break ;
			}

			case ID_END:
			{
				// frame end, all done so byte align the frame and check for overruns
				BitBufferByteAlign (bits, false) ;
				//Assert (bits->cur == bits->end) ;
				goto Exit ;
			}
		}

#if 0 // ! DEBUG
		// if we've decoded all of our channels, bail (but not in debug b/c we want to know if we're seeing bad bits)
		// - this also protects us if the config does not match the bitstream or crap data bits follow the audio bits
		if (channelIndex >= numChannels)
			break ;
#endif
	}

NoMoreChannels:

	// if we get here and haven't decoded all of the requested channels, fill the remaining channels with zeros
	for ( ; channelIndex < numChannels ; channelIndex++)
	{
		int32_t *	fill32 = sampleBuffer + channelIndex ;
		Zero32 (fill32, numSamples, numChannels) ;
	}

Exit:
	return status ;
}

#if PRAGMA_MARK
#pragma mark -
#endif

/*
	FillElement ()
	- they're just filler so we don't need 'em
*/
static int32_t
alac_fill_element (struct BitBuffer * bits)
{
	int16_t		count ;

	// 4-bit count or (4-bit + 8-bit count) if 4-bit count == 15
	// - plus this weird -1 thing I still don't fully understand
	count = BitBufferReadSmall (bits, 4) ;
	if (count == 15)
		count += (int16_t) BitBufferReadSmall (bits, 8) - 1 ;

	BitBufferAdvance (bits, count * 8) ;

	RequireAction (bits->cur <= bits->end, return kALAC_ParamError ;) ;

	return ALAC_noErr ;
}

/*
	DataStreamElement ()
	- we don't care about data stream elements so just skip them
*/
static int32_t
alac_data_stream_element (struct BitBuffer * bits)
{
	int32_t		data_byte_align_flag ;
	uint16_t		count ;

	// the tag associates this data stream element with a given audio element

	/* element_instance_tag = */ BitBufferReadSmall (bits, 4) ;

	data_byte_align_flag = BitBufferReadOne (bits) ;

	// 8-bit count or (8-bit + 8-bit count) if 8-bit count == 255
	count = BitBufferReadSmall (bits, 8) ;
	if (count == 255)
		count += BitBufferReadSmall (bits, 8) ;

	// the align flag means the bitstream should be byte-aligned before reading the following data bytes
	if (data_byte_align_flag)
		BitBufferByteAlign (bits, false) ;

	// skip the data bytes
	BitBufferAdvance (bits, count * 8) ;

	RequireAction (bits->cur <= bits->end, return kALAC_ParamError ;) ;

	return ALAC_noErr ;
}

/*
	ZeroN ()
	- helper routines to clear out output channel buffers when decoding fewer channels than requested
*/
static void Zero32 (int32_t * buffer, uint32_t numItems, uint32_t stride)
{
	if (stride == 1)
	{
		memset (buffer, 0, numItems * sizeof (int32_t)) ;
	}
	else
	{
		for (uint32_t indx = 0 ; indx < (numItems * stride) ; indx += stride)
			buffer [indx] = 0 ;
	}
}
