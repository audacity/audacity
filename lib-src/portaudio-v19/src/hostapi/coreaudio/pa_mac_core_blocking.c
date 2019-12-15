/*
 * Implementation of the PortAudio API for Apple AUHAL
 *
 * PortAudio Portable Real-Time Audio Library
 * Latest Version at: http://www.portaudio.com
 *
 * Written by Bjorn Roche of XO Audio LLC, from PA skeleton code.
 * Portions copied from code by Dominic Mazzoni (who wrote a HAL implementation)
 *
 * Dominic's code was based on code by Phil Burk, Darren Gibbs,
 * Gord Peters, Stephane Letz, and Greg Pfiel.
 *
 * The following people also deserve acknowledgements:
 *
 * Olivier Tristan for feedback and testing
 * Glenn Zelniker and Z-Systems engineering for sponsoring the Blocking I/O
 * interface.
 * 
 *
 * Based on the Open Source API proposed by Ross Bencina
 * Copyright (c) 1999-2002 Ross Bencina, Phil Burk
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
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

/*
 * The text above constitutes the entire PortAudio license; however, 
 * the PortAudio community also makes the following non-binding requests:
 *
 * Any person wishing to distribute modifications to the Software is
 * requested to send the modifications to the original developer so that
 * they can be incorporated into the canonical version. It is also 
 * requested that these non-binding requests be included along with the 
 * license above.
 */

/**
 @file
 @ingroup hostapi_src

 This file contains the implementation
 required for blocking I/O. It is separated from pa_mac_core.c simply to ease
 development.
*/

#include "pa_mac_core_blocking.h"
#include "pa_mac_core_internal.h"
#include <assert.h>
#ifdef MOSX_USE_NON_ATOMIC_FLAG_BITS
# define OSAtomicOr32( a, b ) ( (*(b)) |= (a) )
# define OSAtomicAnd32( a, b ) ( (*(b)) &= (a) )
#else
# include <libkern/OSAtomic.h>
#endif

/*
 * This function determines the size of a particular sample format.
 * if the format is not recognized, this returns zero.
 */
static size_t computeSampleSizeFromFormat( PaSampleFormat format )
{
   switch( format & (~paNonInterleaved) ) {
   case paFloat32: return 4;
   case paInt32: return 4;
   case paInt24: return 3;
   case paInt16: return 2;
   case paInt8: case paUInt8: return 1;
   default: return 0;
   }
}
/*
 * Same as computeSampleSizeFromFormat, except that if
 * the size is not a power of two, it returns the next power of two up
 */
static size_t computeSampleSizeFromFormatPow2( PaSampleFormat format )
{
   switch( format & (~paNonInterleaved) ) {
   case paFloat32: return 4;
   case paInt32: return 4;
   case paInt24: return 4;
   case paInt16: return 2;
   case paInt8: case paUInt8: return 1;
   default: return 0;
   }
}



/*
 * Functions for initializing, resetting, and destroying BLIO structures.
 *
 */

/**
 * This should be called with the relevant info when initializing a stream for callback.
 *
 * @param ringBufferSizeInFrames must be a power of 2
 */
PaError initializeBlioRingBuffers(
                                       PaMacBlio *blio,
                                       PaSampleFormat inputSampleFormat,
                                       PaSampleFormat outputSampleFormat,
                                       long ringBufferSizeInFrames,
                                       int inChan,
                                       int outChan )
{
   void *data;
   int result;
   OSStatus err;

   /* zeroify things */
   bzero( blio, sizeof( PaMacBlio ) );
   /* this is redundant, but the buffers are used to check
      if the buffers have been initialized, so we do it explicitly. */
   blio->inputRingBuffer.buffer = NULL;
   blio->outputRingBuffer.buffer = NULL;

   /* initialize simple data */
   blio->ringBufferFrames = ringBufferSizeInFrames;
   blio->inputSampleFormat = inputSampleFormat;
   blio->inputSampleSizeActual = computeSampleSizeFromFormat(inputSampleFormat);
   blio->inputSampleSizePow2 = computeSampleSizeFromFormatPow2(inputSampleFormat); // FIXME: WHY?
   blio->outputSampleFormat = outputSampleFormat;
   blio->outputSampleSizeActual = computeSampleSizeFromFormat(outputSampleFormat);
   blio->outputSampleSizePow2 = computeSampleSizeFromFormatPow2(outputSampleFormat);

   blio->inChan = inChan;
   blio->outChan = outChan;
   blio->statusFlags = 0;
   blio->errors = paNoError;
#ifdef PA_MAC_BLIO_MUTEX
   blio->isInputEmpty = false;
   blio->isOutputFull = false;
#endif

   /* setup ring buffers */
#ifdef PA_MAC_BLIO_MUTEX
   result = PaMacCore_SetUnixError( pthread_mutex_init(&(blio->inputMutex),NULL), 0 );
   if( result )
      goto error;
   result = UNIX_ERR( pthread_cond_init( &(blio->inputCond), NULL ) );
   if( result )
      goto error;
   result = UNIX_ERR( pthread_mutex_init(&(blio->outputMutex),NULL) );
   if( result )
      goto error;
   result = UNIX_ERR( pthread_cond_init( &(blio->outputCond), NULL ) );
#endif
   if( inChan ) {
      data = calloc( ringBufferSizeInFrames, blio->inputSampleSizePow2 * inChan );
      if( !data )
      {
         result = paInsufficientMemory;
         goto error;
      }

      err = PaUtil_InitializeRingBuffer(
            &blio->inputRingBuffer,
            blio->inputSampleSizePow2 * inChan,
            ringBufferSizeInFrames,
            data );
      assert( !err );
   }
   if( outChan ) {
      data = calloc( ringBufferSizeInFrames, blio->outputSampleSizePow2 * outChan );
      if( !data )
      {
         result = paInsufficientMemory;
         goto error;
      }

      err = PaUtil_InitializeRingBuffer(
            &blio->outputRingBuffer,
            blio->outputSampleSizePow2 * outChan,
            ringBufferSizeInFrames,
            data );
      assert( !err );
   }

   result = resetBlioRingBuffers( blio );
   if( result )
      goto error;

   return 0;

 error:
   destroyBlioRingBuffers( blio );
   return result;
}

#ifdef PA_MAC_BLIO_MUTEX
PaError blioSetIsInputEmpty( PaMacBlio *blio, bool isEmpty )
{
   PaError result = paNoError;
   if( isEmpty == blio->isInputEmpty )
      goto done;

   /* we need to update the value. Here's what we do:
    * - Lock the mutex, so noone else can write.
    * - update the value.
    * - unlock.
    * - broadcast to all listeners.
    */
   result = UNIX_ERR( pthread_mutex_lock( &blio->inputMutex ) );
   if( result )
      goto done;
   blio->isInputEmpty = isEmpty;
   result = UNIX_ERR( pthread_mutex_unlock( &blio->inputMutex ) );
   if( result )
      goto done;
   result = UNIX_ERR( pthread_cond_broadcast( &blio->inputCond ) );
   if( result )
      goto done;

 done:
   return result;
}
PaError blioSetIsOutputFull( PaMacBlio *blio, bool isFull )
{
   PaError result = paNoError;
   if( isFull == blio->isOutputFull )
      goto done;

   /* we need to update the value. Here's what we do:
    * - Lock the mutex, so noone else can write.
    * - update the value.
    * - unlock.
    * - broadcast to all listeners.
    */
   result = UNIX_ERR( pthread_mutex_lock( &blio->outputMutex ) );
   if( result )
      goto done;
   blio->isOutputFull = isFull;
   result = UNIX_ERR( pthread_mutex_unlock( &blio->outputMutex ) );
   if( result )
      goto done;
   result = UNIX_ERR( pthread_cond_broadcast( &blio->outputCond ) );
   if( result )
      goto done;

 done:
   return result;
}
#endif

/* This should be called after stopping or aborting the stream, so that on next
   start, the buffers will be ready. */
PaError resetBlioRingBuffers( PaMacBlio *blio )
{
#ifdef PA_MAC__BLIO_MUTEX
   int result;
#endif
   blio->statusFlags = 0;
   if( blio->outputRingBuffer.buffer ) {
       PaUtil_FlushRingBuffer( &blio->outputRingBuffer );
       /* Fill the buffer with zeros. */
       bzero( blio->outputRingBuffer.buffer,
             blio->outputRingBuffer.bufferSize * blio->outputRingBuffer.elementSizeBytes );
       PaUtil_AdvanceRingBufferWriteIndex( &blio->outputRingBuffer, blio->ringBufferFrames );

      /* Update isOutputFull. */
#ifdef PA_MAC__BLIO_MUTEX
      result = blioSetIsOutputFull( blio, toAdvance == blio->outputRingBuffer.bufferSize );
      if( result )
         goto error;
#endif
/*
      printf( "------%d\n" ,  blio->outChan );
      printf( "------%d\n" ,  blio->outputSampleSize );
*/
   }
   if( blio->inputRingBuffer.buffer ) {
      PaUtil_FlushRingBuffer( &blio->inputRingBuffer );
      bzero( blio->inputRingBuffer.buffer,
             blio->inputRingBuffer.bufferSize * blio->inputRingBuffer.elementSizeBytes );
      /* Update isInputEmpty. */
#ifdef PA_MAC__BLIO_MUTEX
      result = blioSetIsInputEmpty( blio, true );
      if( result )
         goto error;
#endif
   }
   return paNoError;
#ifdef PA_MAC__BLIO_MUTEX
 error:
   return result;
#endif
}

/*This should be called when you are done with the blio. It can safely be called
  multiple times if there are no exceptions. */
PaError destroyBlioRingBuffers( PaMacBlio *blio )
{
   PaError result = paNoError;
   if( blio->inputRingBuffer.buffer ) {
      free( blio->inputRingBuffer.buffer );
#ifdef PA_MAC__BLIO_MUTEX
      result = UNIX_ERR( pthread_mutex_destroy( & blio->inputMutex ) );
      if( result ) return result;
      result = UNIX_ERR( pthread_cond_destroy( & blio->inputCond ) );
      if( result ) return result;
#endif
   }
   blio->inputRingBuffer.buffer = NULL;
   if( blio->outputRingBuffer.buffer ) {
      free( blio->outputRingBuffer.buffer );
#ifdef PA_MAC__BLIO_MUTEX
      result = UNIX_ERR( pthread_mutex_destroy( & blio->outputMutex ) );
      if( result ) return result;
      result = UNIX_ERR( pthread_cond_destroy( & blio->outputCond ) );
      if( result ) return result;
#endif
   }
   blio->outputRingBuffer.buffer = NULL;

   return result;
}

/*
 * this is the BlioCallback function. It expects to recieve a PaMacBlio Object
 * pointer as userData.
 *
 */
int BlioCallback( const void *input, void *output, unsigned long frameCount,
	const PaStreamCallbackTimeInfo* timeInfo,
        PaStreamCallbackFlags statusFlags,
        void *userData )
{
   PaMacBlio *blio = (PaMacBlio*)userData;
   ring_buffer_size_t framesAvailable;
   ring_buffer_size_t framesToTransfer;
   ring_buffer_size_t framesTransferred;

   /* set flags returned by OS: */
   OSAtomicOr32( statusFlags, &blio->statusFlags ) ;

   /* --- Handle Input Buffer --- */
   if( blio->inChan ) {
      framesAvailable = PaUtil_GetRingBufferWriteAvailable( &blio->inputRingBuffer );

      /* check for underflow */
      if( framesAvailable < frameCount )
      {
          OSAtomicOr32( paInputOverflow, &blio->statusFlags );
          framesToTransfer = framesAvailable;
      }
      else
      {
          framesToTransfer = (ring_buffer_size_t)frameCount;
      }

      /* Copy the data from the audio input to the application ring buffer. */
      /*printf( "reading %d\n", toRead );*/
      framesTransferred = PaUtil_WriteRingBuffer( &blio->inputRingBuffer, input, framesToTransfer );
      assert( framesToTransfer == framesTransferred );
#ifdef PA_MAC__BLIO_MUTEX
      /* Priority inversion. See notes below. */
      blioSetIsInputEmpty( blio, false );
#endif
   }


   /* --- Handle Output Buffer --- */
   if( blio->outChan ) {
      framesAvailable = PaUtil_GetRingBufferReadAvailable( &blio->outputRingBuffer );

      /* check for underflow */
      if( framesAvailable < frameCount )
      {
          /* zero out the end of the output buffer that we do not have data for */
          framesToTransfer = framesAvailable;

          size_t bytesPerFrame = blio->outputSampleSizeActual * blio->outChan;
          size_t offsetInBytes = framesToTransfer * bytesPerFrame;
          size_t countInBytes = (frameCount - framesToTransfer) * bytesPerFrame;
          bzero( ((char *)output) + offsetInBytes, countInBytes );

          OSAtomicOr32( paOutputUnderflow, &blio->statusFlags );
          framesToTransfer = framesAvailable;
      }
      else
      {
          framesToTransfer = (ring_buffer_size_t)frameCount;
      }

      /* copy the data */
      /*printf( "writing %d\n", toWrite );*/
      framesTransferred = PaUtil_ReadRingBuffer( &blio->outputRingBuffer, output, framesToTransfer );
      assert( framesToTransfer == framesTransferred );
#ifdef PA_MAC__BLIO_MUTEX
      /* We have a priority inversion here. However, we will only have to
         wait if this was true and is now false, which means we've got
         some room in the buffer.
         Hopefully problems will be minimized. */
      blioSetIsOutputFull( blio, false );
#endif
   }

   return paContinue;
}

PaError ReadStream( PaStream* stream,
                           void *buffer,
                           unsigned long framesRequested )
{
    PaMacBlio *blio = & ((PaMacCoreStream*)stream) -> blio;
    char *cbuf = (char *) buffer;
    PaError ret = paNoError;
    VVDBUG(("ReadStream()\n"));

    while( framesRequested > 0 ) {
       ring_buffer_size_t framesAvailable;
       ring_buffer_size_t framesToTransfer;
       ring_buffer_size_t framesTransferred;
       do {
          framesAvailable = PaUtil_GetRingBufferReadAvailable( &blio->inputRingBuffer );
/*
          printf( "Read Buffer is %%%g full: %ld of %ld.\n",
                  100 * (float)avail / (float) blio->inputRingBuffer.bufferSize,
                  framesAvailable, blio->inputRingBuffer.bufferSize );
*/
          if( framesAvailable == 0 ) {
#ifdef PA_MAC_BLIO_MUTEX
             /**block when empty*/
             ret = UNIX_ERR( pthread_mutex_lock( &blio->inputMutex ) );
             if( ret )
                return ret;
             while( blio->isInputEmpty ) {
                ret = UNIX_ERR( pthread_cond_wait( &blio->inputCond, &blio->inputMutex ) );
                if( ret )
                   return ret;
             }
             ret = UNIX_ERR( pthread_mutex_unlock( &blio->inputMutex ) );
             if( ret )
                return ret;
#else
             Pa_Sleep( PA_MAC_BLIO_BUSY_WAIT_SLEEP_INTERVAL );
#endif
          }
       } while( framesAvailable == 0 );
       framesToTransfer = (ring_buffer_size_t) MIN( framesAvailable, framesRequested );
       framesTransferred = PaUtil_ReadRingBuffer( &blio->inputRingBuffer, (void *)cbuf, framesToTransfer );
       cbuf += framesTransferred * blio->inputSampleSizeActual * blio->inChan;
       framesRequested -= framesTransferred;

       if( framesToTransfer == framesAvailable ) {
#ifdef PA_MAC_BLIO_MUTEX
          /* we just emptied the buffer, so we need to mark it as empty. */
          ret = blioSetIsInputEmpty( blio, true );
          if( ret )
             return ret;
          /* of course, in the meantime, the callback may have put some sats
             in, so
             so check for that, too, to avoid a race condition. */
          /* FIXME - this does not seem to fix any race condition. */
          if( PaUtil_GetRingBufferReadAvailable( &blio->inputRingBuffer ) ) {
             blioSetIsInputEmpty( blio, false );
             /* FIXME - why check? ret has not been set? */
             if( ret )
                return ret;
          }
#endif
       }
    }

    /*   Report either paNoError or paInputOverflowed. */
    /*   may also want to report other errors, but this is non-standard. */
    /* FIXME should not clobber ret, use if(blio->statusFlags & paInputOverflow) */
    ret = blio->statusFlags & paInputOverflow;

    /* report underflow only once: */
    if( ret ) {
       OSAtomicAnd32( (uint32_t)(~paInputOverflow), &blio->statusFlags );
       ret = paInputOverflowed;
    }

    return ret;
}


PaError WriteStream( PaStream* stream,
                            const void *buffer,
                            unsigned long framesRequested )
{
    PaMacCoreStream *macStream = (PaMacCoreStream*)stream;
    PaMacBlio *blio = &macStream->blio;
    char *cbuf = (char *) buffer;
    PaError ret = paNoError;
    VVDBUG(("WriteStream()\n"));

    while( framesRequested > 0 && macStream->state != STOPPING ) {
        ring_buffer_size_t framesAvailable;
        ring_buffer_size_t framesToTransfer;
        ring_buffer_size_t framesTransferred;

       do {
          framesAvailable = PaUtil_GetRingBufferWriteAvailable( &blio->outputRingBuffer );
/*
          printf( "Write Buffer is %%%g full: %ld of %ld.\n",
                  100 - 100 * (float)avail / (float) blio->outputRingBuffer.bufferSize,
                  framesAvailable, blio->outputRingBuffer.bufferSize );
*/
          if( framesAvailable == 0 ) {
#ifdef PA_MAC_BLIO_MUTEX
             /*block while full*/
             ret = UNIX_ERR( pthread_mutex_lock( &blio->outputMutex ) );
             if( ret )
                return ret;
             while( blio->isOutputFull ) {
                ret = UNIX_ERR( pthread_cond_wait( &blio->outputCond, &blio->outputMutex ) );
                if( ret )
                   return ret;
             }
             ret = UNIX_ERR( pthread_mutex_unlock( &blio->outputMutex ) );
             if( ret )
                return ret;
#else
             Pa_Sleep( PA_MAC_BLIO_BUSY_WAIT_SLEEP_INTERVAL );
#endif
          }
       } while( framesAvailable == 0 && macStream->state != STOPPING );

       if( macStream->state == STOPPING )
       {
           break;
       }

       framesToTransfer = MIN( framesAvailable, framesRequested );
       framesTransferred = PaUtil_WriteRingBuffer( &blio->outputRingBuffer, (void *)cbuf, framesToTransfer );
       cbuf += framesTransferred * blio->outputSampleSizeActual * blio->outChan;
       framesRequested -= framesTransferred;

#ifdef PA_MAC_BLIO_MUTEX
       if( framesToTransfer == framesAvailable ) {
          /* we just filled up the buffer, so we need to mark it as filled. */
          ret = blioSetIsOutputFull( blio, true );
          if( ret )
             return ret;
          /* of course, in the meantime, we may have emptied the buffer, so
             so check for that, too, to avoid a race condition. */
          if( PaUtil_GetRingBufferWriteAvailable( &blio->outputRingBuffer ) ) {
             blioSetIsOutputFull( blio, false );
              /* FIXME remove or review this code, does not fix race, ret not set! */
             if( ret )
                return ret;
          }
       }
#endif
    }

    if ( macStream->state == STOPPING )
    {
        ret = paInternalError;
    }
    else if (ret == paNoError )
    {
        /*   Test for underflow. */
        ret = blio->statusFlags & paOutputUnderflow;

        /* report underflow only once: */
        if( ret )
        {
            OSAtomicAnd32( (uint32_t)(~paOutputUnderflow), &blio->statusFlags );
            ret = paOutputUnderflowed;
        }
    }

    return ret;
}

/*
 * Wait until the data in the buffer has finished playing.
 */
PaError waitUntilBlioWriteBufferIsEmpty( PaMacBlio *blio, double sampleRate,
                                        size_t framesPerBuffer )
{
    PaError result = paNoError;
    if( blio->outputRingBuffer.buffer ) {
        ring_buffer_size_t framesLeft = PaUtil_GetRingBufferReadAvailable( &blio->outputRingBuffer );

        /* Calculate when we should give up waiting. To be safe wait for two extra periods. */
        PaTime now = PaUtil_GetTime();
        PaTime startTime = now;
        PaTime timeoutTime = startTime + (framesLeft + (2 * framesPerBuffer)) / sampleRate;

        long msecPerBuffer = 1 + (long)( 1000.0 * framesPerBuffer / sampleRate);
        while( framesLeft > 0 && now < timeoutTime ) {
            VDBUG(( "waitUntilBlioWriteBufferIsFlushed: framesLeft = %d, framesPerBuffer = %ld\n",
                  framesLeft, framesPerBuffer ));
            Pa_Sleep( msecPerBuffer );
            framesLeft = PaUtil_GetRingBufferReadAvailable( &blio->outputRingBuffer );
            now = PaUtil_GetTime();
        }

        if( framesLeft > 0 )
        {
            VDBUG(( "waitUntilBlioWriteBufferIsFlushed: TIMED OUT - framesLeft = %d\n", framesLeft ));
            result = paTimedOut;
        }
    }
    return result;
}

signed long GetStreamReadAvailable( PaStream* stream )
{
    PaMacBlio *blio = & ((PaMacCoreStream*)stream) -> blio;
    VVDBUG(("GetStreamReadAvailable()\n"));

    return PaUtil_GetRingBufferReadAvailable( &blio->inputRingBuffer );
}


signed long GetStreamWriteAvailable( PaStream* stream )
{
    PaMacBlio *blio = & ((PaMacCoreStream*)stream) -> blio;
    VVDBUG(("GetStreamWriteAvailable()\n"));

    return PaUtil_GetRingBufferWriteAvailable( &blio->outputRingBuffer );
}

