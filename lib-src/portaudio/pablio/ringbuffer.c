/*
 * $Id: ringbuffer.c,v 1.3 2003-03-02 08:01:47 dmazzoni Exp $
 * ringbuffer.c
 * Ring Buffer utility..
 *
 * Author: Phil Burk, http://www.softsynth.com
 *
 * This program uses the PortAudio Portable Audio Library.
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

/***************************************************************************
 * Initialize FIFO.
 * numBytes must be power of 2, returns -1 if not.
 */
long RingBuffer_Init( RingBuffer *rbuf, long numBytes, void *dataPtr )
{
    if( ((numBytes-1) & numBytes) != 0) return -1; /* Not Power of two. */
    rbuf->bufferSize = numBytes;
    rbuf->buffer = (char *)dataPtr;
    RingBuffer_Flush( rbuf );
    rbuf->bigMask = (numBytes*2)-1;
    rbuf->smallMask = (numBytes)-1;
    return 0;
}
/***************************************************************************
** Return number of bytes available for reading. */
long RingBuffer_GetReadAvailable( RingBuffer *rbuf )
{
    return ( (rbuf->writeIndex - rbuf->readIndex) & rbuf->bigMask );
}
/***************************************************************************
** Return number of bytes available for writing. */
long RingBuffer_GetWriteAvailable( RingBuffer *rbuf )
{
    return ( rbuf->bufferSize - RingBuffer_GetReadAvailable(rbuf));
}

/***************************************************************************
** Clear buffer. Should only be called when buffer is NOT being read. */
void RingBuffer_Flush( RingBuffer *rbuf )
{
    rbuf->writeIndex = rbuf->readIndex = 0;
}

/***************************************************************************
** Get address of region(s) to which we can write data.
** If the region is contiguous, size2 will be zero.
** If non-contiguous, size2 will be the size of second region.
** Returns room available to be written or numBytes, whichever is smaller.
*/
long RingBuffer_GetWriteRegions( RingBuffer *rbuf, long numBytes,
                                 void **dataPtr1, long *sizePtr1,
                                 void **dataPtr2, long *sizePtr2 )
{
    long   index;
    long   available = RingBuffer_GetWriteAvailable( rbuf );
    if( numBytes > available ) numBytes = available;
    /* Check to see if write is not contiguous. */
    index = rbuf->writeIndex & rbuf->smallMask;
    if( (index + numBytes) > rbuf->bufferSize )
    {
        /* Write data in two blocks that wrap the buffer. */
        long   firstHalf = rbuf->bufferSize - index;
        *dataPtr1 = &rbuf->buffer[index];
        *sizePtr1 = firstHalf;
        *dataPtr2 = &rbuf->buffer[0];
        *sizePtr2 = numBytes - firstHalf;
    }
    else
    {
        *dataPtr1 = &rbuf->buffer[index];
        *sizePtr1 = numBytes;
        *dataPtr2 = NULL;
        *sizePtr2 = 0;
    }
    return numBytes;
}


/***************************************************************************
*/
long RingBuffer_AdvanceWriteIndex( RingBuffer *rbuf, long numBytes )
{
    return rbuf->writeIndex = (rbuf->writeIndex + numBytes) & rbuf->bigMask;
}

/***************************************************************************
** Get address of region(s) from which we can read data.
** If the region is contiguous, size2 will be zero.
** If non-contiguous, size2 will be the size of second region.
** Returns room available to be written or numBytes, whichever is smaller.
*/
long RingBuffer_GetReadRegions( RingBuffer *rbuf, long numBytes,
                                void **dataPtr1, long *sizePtr1,
                                void **dataPtr2, long *sizePtr2 )
{
    long   index;
    long   available = RingBuffer_GetReadAvailable( rbuf );
    if( numBytes > available ) numBytes = available;
    /* Check to see if read is not contiguous. */
    index = rbuf->readIndex & rbuf->smallMask;
    if( (index + numBytes) > rbuf->bufferSize )
    {
        /* Write data in two blocks that wrap the buffer. */
        long firstHalf = rbuf->bufferSize - index;
        *dataPtr1 = &rbuf->buffer[index];
        *sizePtr1 = firstHalf;
        *dataPtr2 = &rbuf->buffer[0];
        *sizePtr2 = numBytes - firstHalf;
    }
    else
    {
        *dataPtr1 = &rbuf->buffer[index];
        *sizePtr1 = numBytes;
        *dataPtr2 = NULL;
        *sizePtr2 = 0;
    }
    return numBytes;
}
/***************************************************************************
*/
long RingBuffer_AdvanceReadIndex( RingBuffer *rbuf, long numBytes )
{
    return rbuf->readIndex = (rbuf->readIndex + numBytes) & rbuf->bigMask;
}

/***************************************************************************
** Return bytes written. */
long RingBuffer_Write( RingBuffer *rbuf, void *data, long numBytes )
{
    long size1, size2, numWritten;
    void *data1, *data2;
    numWritten = RingBuffer_GetWriteRegions( rbuf, numBytes, &data1, &size1, &data2, &size2 );
    if( size2 > 0 )
    {

        memcpy( data1, data, size1 );
        data = ((char *)data) + size1;
        memcpy( data2, data, size2 );
    }
    else
    {
        memcpy( data1, data, size1 );
    }
    RingBuffer_AdvanceWriteIndex( rbuf, numWritten );
    return numWritten;
}

/***************************************************************************
** Return bytes read. */
long RingBuffer_Read( RingBuffer *rbuf, void *data, long numBytes )
{
    long size1, size2, numRead;
    void *data1, *data2;
    numRead = RingBuffer_GetReadRegions( rbuf, numBytes, &data1, &size1, &data2, &size2 );
    if( size2 > 0 )
    {
        memcpy( data, data1, size1 );
        data = ((char *)data) + size1;
        memcpy( data, data2, size2 );
    }
    else
    {
        memcpy( data, data1, size1 );
    }
    RingBuffer_AdvanceReadIndex( rbuf, numRead );
    return numRead;
}
