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

/*=============================================================================
	File:		ALACBitUtilities.h

	$NoKeywords: $
=============================================================================*/

#ifndef __ALACBITUTILITIES_H
#define __ALACBITUTILITIES_H

#include <stdint.h>

#ifndef MIN
#define MIN(x, y) 			((x) < (y) ? (x) : (y))
#endif // MIN
#ifndef MAX
#define MAX(x, y) 			((x) > (y) ? (x) : (y))
#endif // MAX

#define RequireAction(condition, action)		if (! (condition)) { action }
#define RequireActionSilent(condition, action)	if (! (condition)) { action }
#define RequireNoErr(condition, action)			if (condition) { action }

enum
{
	ALAC_noErr = 0
} ;


typedef enum
{	ID_SCE = 0,						/* Single Channel Element   */
	ID_CPE = 1,						/* Channel Pair Element	 */
	ID_CCE = 2,						/* Coupling Channel Element */
	ID_LFE = 3,						/* LFE Channel Element	  */
	ID_DSE = 4,						/* not yet supported		*/
	ID_PCE = 5,
	ID_FIL = 6,
	ID_END = 7
} ELEMENT_TYPE ;

// types
typedef struct BitBuffer
{
	uint8_t *		cur ;
	uint8_t *		end ;
	uint32_t		bitIndex ;
	uint32_t		byteSize ;

} BitBuffer ;

/*
	BitBuffer routines
	- these routines take a fixed size buffer and read/write to it
	- bounds checking must be done by the client
*/
void		BitBufferInit (BitBuffer * bits, uint8_t * buffer, uint32_t byteSize) ;
uint32_t	BitBufferRead (BitBuffer * bits, uint8_t numBits) ;	// note: cannot read more than 16 bits at a time
uint8_t		BitBufferReadSmall (BitBuffer * bits, uint8_t numBits) ;
uint8_t		BitBufferReadOne (BitBuffer * bits) ;
uint32_t	BitBufferPeek (BitBuffer * bits, uint8_t numBits) ;		// note: cannot read more than 16 bits at a time
uint32_t	BitBufferPeekOne (BitBuffer * bits) ;
uint32_t	BitBufferUnpackBERSize (BitBuffer * bits) ;
uint32_t	BitBufferGetPosition (BitBuffer * bits) ;
void		BitBufferByteAlign (BitBuffer * bits, int32_t addZeros) ;
void		BitBufferAdvance (BitBuffer * bits, uint32_t numBits) ;
void		BitBufferRewind (BitBuffer * bits, uint32_t numBits) ;
void		BitBufferWrite (BitBuffer * bits, uint32_t value, uint32_t numBits) ;
void		BitBufferReset (BitBuffer * bits) ;

#endif	/* __BITUTILITIES_H */
