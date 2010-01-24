#ifndef _RINGBUFFER_H
#define _RINGBUFFER_H
#ifdef __cplusplus
extern "C"
{
#endif /* __cplusplus */

/*
 * $Id: ringbuffer.h,v 1.3 2003-03-02 08:01:47 dmazzoni Exp $
 * ringbuffer.h
 * Ring Buffer utility..
 *
 * Author: Phil Burk, http://www.softsynth.com
 *
 * This program is distributed with the PortAudio Portable Audio Library.
 * For more information see: http://www.audiomulch.com/portaudio/
 * Copyright (c) 1999-2000 Ross Bencina and Phil Burk
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files
 * (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge,
 * publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * Any person wishing to distribute modifications to the Software is
 * requested to send the modifications to the original developer so that
 * they can be incorporated into the canonical version.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "ringbuffer.h"
#include <string.h>

typedef struct
{
    long   bufferSize; /* Number of bytes in FIFO. Power of 2. Set by RingBuffer_Init. */
    long   writeIndex; /* Index of next writable byte. Set by RingBuffer_AdvanceWriteIndex. */
    long   readIndex;  /* Index of next readable byte. Set by RingBuffer_AdvanceReadIndex. */
    long   bigMask;    /* Used for wrapping indices with extra bit to distinguish full/empty. */
    long   smallMask;  /* Used for fitting indices to buffer. */
    char *buffer;
}
RingBuffer;
/*
 * Initialize Ring Buffer.
 * numBytes must be power of 2, returns -1 if not.
 */
long RingBuffer_Init( RingBuffer *rbuf, long numBytes, void *dataPtr );

/* Clear buffer. Should only be called when buffer is NOT being read. */
void RingBuffer_Flush( RingBuffer *rbuf );

/* Return number of bytes available for writing. */
long RingBuffer_GetWriteAvailable( RingBuffer *rbuf );
/* Return number of bytes available for read. */
long RingBuffer_GetReadAvailable( RingBuffer *rbuf );
/* Return bytes written. */
long RingBuffer_Write( RingBuffer *rbuf, void *data, long numBytes );
/* Return bytes read. */
long RingBuffer_Read( RingBuffer *rbuf, void *data, long numBytes );

/* Get address of region(s) to which we can write data.
** If the region is contiguous, size2 will be zero.
** If non-contiguous, size2 will be the size of second region.
** Returns room available to be written or numBytes, whichever is smaller.
*/
long RingBuffer_GetWriteRegions( RingBuffer *rbuf, long numBytes,
                                 void **dataPtr1, long *sizePtr1,
                                 void **dataPtr2, long *sizePtr2 );
long RingBuffer_AdvanceWriteIndex( RingBuffer *rbuf, long numBytes );

/* Get address of region(s) from which we can read data.
** If the region is contiguous, size2 will be zero.
** If non-contiguous, size2 will be the size of second region.
** Returns room available to be written or numBytes, whichever is smaller.
*/
long RingBuffer_GetReadRegions( RingBuffer *rbuf, long numBytes,
                                void **dataPtr1, long *sizePtr1,
                                void **dataPtr2, long *sizePtr2 );

long RingBuffer_AdvanceReadIndex( RingBuffer *rbuf, long numBytes );

#ifdef __cplusplus
}
#endif /* __cplusplus */
#endif /* _RINGBUFFER_H */
