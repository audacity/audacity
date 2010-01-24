/*
 * $Id: pablio.c,v 1.3 2003-03-02 08:01:46 dmazzoni Exp $
 * pablio.c
 * Portable Audio Blocking Input/Output utility.
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

/* History:
 * PLB021214 - check for valid stream in CloseAudioStream() to prevent hang.
 *             add timeOutMSec to CloseAudioStream() to prevent hang.
 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "portaudio.h"
#include "ringbuffer.h"
#include "pablio.h"
#include <string.h>

/************************************************************************/
/******** Constants *****************************************************/
/************************************************************************/

#define FRAMES_PER_BUFFER    (256)

/************************************************************************/
/******** Prototypes ****************************************************/
/************************************************************************/

static int blockingIOCallback( void *inputBuffer, void *outputBuffer,
                               unsigned long framesPerBuffer,
                               PaTimestamp outTime, void *userData );
static PaError PABLIO_InitFIFO( RingBuffer *rbuf, long numFrames, long bytesPerFrame );
static PaError PABLIO_TermFIFO( RingBuffer *rbuf );

/************************************************************************/
/******** Functions *****************************************************/
/************************************************************************/

/* Called from PortAudio.
 * Read and write data only if there is room in FIFOs.
 */
static int blockingIOCallback( void *inputBuffer, void *outputBuffer,
                               unsigned long framesPerBuffer,
                               PaTimestamp outTime, void *userData )
{
    PABLIO_Stream *data = (PABLIO_Stream*)userData;
    long numBytes = data->bytesPerFrame * framesPerBuffer;
    (void) outTime;

    /* This may get called with NULL inputBuffer during initial setup. */
    if( inputBuffer != NULL )
    {
        RingBuffer_Write( &data->inFIFO, inputBuffer, numBytes );
    }
    if( outputBuffer != NULL )
    {
        int i;
        int numRead = RingBuffer_Read( &data->outFIFO, outputBuffer, numBytes );
        /* Zero out remainder of buffer if we run out of data. */
        for( i=numRead; i<numBytes; i++ )
        {
            ((char *)outputBuffer)[i] = 0;
        }
    }

    return 0;
}

/* Allocate buffer. */
static PaError PABLIO_InitFIFO( RingBuffer *rbuf, long numFrames, long bytesPerFrame )
{
    long numBytes = numFrames * bytesPerFrame;
    char *buffer = (char *) malloc( numBytes );
    if( buffer == NULL ) return paInsufficientMemory;
    memset( buffer, 0, numBytes );
    return (PaError) RingBuffer_Init( rbuf, numBytes, buffer );
}

/* Free buffer. */
static PaError PABLIO_TermFIFO( RingBuffer *rbuf )
{
    if( rbuf->buffer ) free( rbuf->buffer );
    rbuf->buffer = NULL;
    return paNoError;
}

/************************************************************
 * Write data to ring buffer.
 * Will not return until all the data has been written.
 */
long WriteAudioStream( PABLIO_Stream *aStream, void *data, long numFrames )
{
    long bytesWritten;
    char *p = (char *) data;
    long numBytes = aStream->bytesPerFrame * numFrames;
    while( numBytes > 0)
    {
        bytesWritten = RingBuffer_Write( &aStream->outFIFO, p, numBytes );
        numBytes -= bytesWritten;
        p += bytesWritten;
        if( numBytes > 0) Pa_Sleep(10);
    }
    return numFrames;
}

/************************************************************
 * Read data from ring buffer.
 * Will not return until all the data has been read.
 */
long ReadAudioStream( PABLIO_Stream *aStream, void *data, long numFrames )
{
    long bytesRead;
    char *p = (char *) data;
    long numBytes = aStream->bytesPerFrame * numFrames;
    while( numBytes > 0)
    {
        bytesRead = RingBuffer_Read( &aStream->inFIFO, p, numBytes );
        numBytes -= bytesRead;
        p += bytesRead;
        if( numBytes > 0) Pa_Sleep(10);
    }
    return numFrames;
}

/************************************************************
 * Return the number of frames that could be written to the stream without
 * having to wait.
 */
long GetAudioStreamWriteable( PABLIO_Stream *aStream )
{
    int bytesEmpty = RingBuffer_GetWriteAvailable( &aStream->outFIFO );
    return bytesEmpty / aStream->bytesPerFrame;
}

/************************************************************
 * Return the number of frames that are available to be read from the
 * stream without having to wait.
 */
long GetAudioStreamReadable( PABLIO_Stream *aStream )
{
    int bytesFull = RingBuffer_GetReadAvailable( &aStream->inFIFO );
    return bytesFull / aStream->bytesPerFrame;
}

/************************************************************/
static unsigned long RoundUpToNextPowerOf2( unsigned long n )
{
    long numBits = 0;
    if( ((n-1) & n) == 0) return n; /* Already Power of two. */
    while( n > 0 )
    {
        n= n>>1;
        numBits++;
    }
    return (1<<numBits);
}

/************************************************************
 * Opens a PortAudio stream with default characteristics.
 * Allocates PABLIO_Stream structure.
 *
 * flags parameter can be an ORed combination of:
 *    PABLIO_READ, PABLIO_WRITE, or PABLIO_READ_WRITE,
 *    and either PABLIO_MONO or PABLIO_STEREO
 */
PaError OpenAudioStream( PABLIO_Stream **rwblPtr, double sampleRate,
                         PaSampleFormat format, long flags )
{
    long   bytesPerSample;
    long   doRead = 0;
    long   doWrite = 0;
    PaError err;
    PABLIO_Stream *aStream;
    long   minNumBuffers;
    long   numFrames;

    /* Allocate PABLIO_Stream structure for caller. */
    aStream = (PABLIO_Stream *) malloc( sizeof(PABLIO_Stream) );
    if( aStream == NULL ) return paInsufficientMemory;
    memset( aStream, 0, sizeof(PABLIO_Stream) );

    /* Determine size of a sample. */
    bytesPerSample = Pa_GetSampleSize( format );
    if( bytesPerSample < 0 )
    {
        err = (PaError) bytesPerSample;
        goto error;
    }
    aStream->samplesPerFrame = ((flags&PABLIO_MONO) != 0) ? 1 : 2;
    aStream->bytesPerFrame = bytesPerSample * aStream->samplesPerFrame;

    /* Initialize PortAudio  */
    err = Pa_Initialize();
    if( err != paNoError ) goto error;

    /* Warning: numFrames must be larger than amount of data processed per interrupt
     *    inside PA to prevent glitches. Just to be safe, adjust size upwards.
     */
    minNumBuffers = 2 * Pa_GetMinNumBuffers( FRAMES_PER_BUFFER, sampleRate );
    numFrames = minNumBuffers * FRAMES_PER_BUFFER;
    /* The PortAudio callback runs in a high priority thread. But PABLIO
     * runs in a normal foreground thread. So we may have much worse
     * latency in PABLIO. So adjust latency to a safe level.
     */
    {
        const int safeLatencyMSec = 200;
        int minLatencyMSec = (1000 * numFrames) / sampleRate;
        if( minLatencyMSec < safeLatencyMSec )
        {
            numFrames = (int) ((safeLatencyMSec * sampleRate) / 1000);
        }
    }
    numFrames = RoundUpToNextPowerOf2( numFrames );

    /* Initialize Ring Buffers */
    doRead = ((flags & PABLIO_READ) != 0);
    doWrite = ((flags & PABLIO_WRITE) != 0);
    if(doRead)
    {
        err = PABLIO_InitFIFO( &aStream->inFIFO, numFrames, aStream->bytesPerFrame );
        if( err != paNoError ) goto error;
    }
    if(doWrite)
    {
        long numBytes;
        err = PABLIO_InitFIFO( &aStream->outFIFO, numFrames, aStream->bytesPerFrame );
        if( err != paNoError ) goto error;
        /* Make Write FIFO appear full initially. */
        numBytes = RingBuffer_GetWriteAvailable( &aStream->outFIFO );
        RingBuffer_AdvanceWriteIndex( &aStream->outFIFO, numBytes );
    }

    /* Open a PortAudio stream that we will use to communicate with the underlying
     * audio drivers. */
    err = Pa_OpenStream(
              &aStream->stream,
              (doRead ? Pa_GetDefaultInputDeviceID() : paNoDevice),
              (doRead ? aStream->samplesPerFrame : 0 ),
              format,
              NULL,
              (doWrite ? Pa_GetDefaultOutputDeviceID() : paNoDevice),
              (doWrite ? aStream->samplesPerFrame : 0 ),
              format,
              NULL,
              sampleRate,
              FRAMES_PER_BUFFER,
              minNumBuffers,
              paClipOff,       /* we won't output out of range samples so don't bother clipping them */
              blockingIOCallback,
              aStream );
    if( err != paNoError ) goto error;

    err = Pa_StartStream( aStream->stream );
    if( err != paNoError ) goto error;

    *rwblPtr = aStream;
    return paNoError;

error:
    CloseAudioStream( aStream );
    *rwblPtr = NULL;
    return err;
}

/************************************************************/
PaError CloseAudioStream( PABLIO_Stream *aStream )
{
    PaError err = paNoError;
    int bytesEmpty;
    int byteSize = aStream->outFIFO.bufferSize;

    if( aStream->stream != NULL ) /* Make sure stream was opened. PLB021214 */
    {
        /* If we are writing data, make sure we play everything written. */
        if( byteSize > 0 )
        {
            int timeOutMSec = 2000;
            bytesEmpty = RingBuffer_GetWriteAvailable( &aStream->outFIFO );
            while( (bytesEmpty < byteSize) && (timeOutMSec > 0) )
            {
                Pa_Sleep( 20 );
                timeOutMSec -= 20;
                bytesEmpty = RingBuffer_GetWriteAvailable( &aStream->outFIFO );
            }
        }
        err = Pa_StopStream( aStream->stream );
        if( err != paNoError ) goto error;
        err = Pa_CloseStream( aStream->stream );
    }

error:
    Pa_Terminate();
    PABLIO_TermFIFO( &aStream->inFIFO );
    PABLIO_TermFIFO( &aStream->outFIFO );
    free( aStream );
    return err;
}
