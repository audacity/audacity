/*
 * $Id: pa_lib.c,v 1.6 2003-03-02 08:01:34 dmazzoni Exp $
 * Portable Audio I/O Library
 * Host Independant Layer
 *
 * Based on the Open Source API proposed by Ross Bencina
 * Copyright (c) 1999-2000 Phil Burk
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

/* Modification History:
 PLB20010422 - apply Mike Berry's changes for CodeWarrior on PC
 PLB20010820 - fix dither and shift for recording PaUInt8 format 
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* PLB20010422 - "memory.h" doesn't work on CodeWarrior for PC. Thanks Mike Berry for the mod. */
#ifdef _WIN32
#ifndef __MWERKS__
#include <memory.h>
#endif  /* __MWERKS__ */
#else   /* !_WIN32 */
#include <memory.h>
#endif  /* _WIN32 */

#include "portaudio.h"
#include "pa_host.h"
#include "pa_trace.h"

/* The reason we might NOT want to validate the rate before opening the stream
 * is because many DirectSound drivers lie about the rates they actually support.
 */
#define PA_VALIDATE_RATE    (0)   /* If true validate sample rate against driver info. */

/*
O- maybe not allocate past_InputBuffer and past_OutputBuffer if not needed for conversion
*/

#ifndef FALSE
 #define FALSE  (0)
 #define TRUE   (!FALSE)
#endif

#define PRINT(x) { printf x; fflush(stdout); }
#define ERR_RPT(x) PRINT(x)
#define DBUG(x)  /* PRINT(x) */
#define DBUGX(x) /* PRINT(x) */

static int gInitCount = 0; /* Count number of times Pa_Initialize() called to allow nesting and overlapping. */

static PaError Pa_KillStream(  PortAudioStream *stream, int abort );

/***********************************************************************/
int PaHost_FindClosestTableEntry( double allowableError,  const double *rateTable, int numRates, double frameRate )
{
    double err, minErr = allowableError;
    int i, bestFit = -1;

    for( i=0; i<numRates; i++ )
    {
        err = fabs( frameRate - rateTable[i] );
        if( err < minErr )
        {
            minErr = err;
            bestFit = i;
        }
    }
    return bestFit;
}

/**************************************************************************
** Make sure sample rate is legal and also convert to enumeration for driver.
*/
PaError PaHost_ValidateSampleRate( PaDeviceID id, double requestedFrameRate,
                                   double *closestFrameRatePtr )
{
    long bestRateIndex;
    const PaDeviceInfo *pdi;
    pdi = Pa_GetDeviceInfo( id );
    if( pdi == NULL )
    {
        return paInvalidDeviceId;
    }

    if( pdi->numSampleRates == -1 )
    {
        /* Is it out of range? */
        if( (requestedFrameRate < pdi->sampleRates[0]) ||
                (requestedFrameRate > pdi->sampleRates[1]) )
        {
            return paInvalidSampleRate;
        }

        *closestFrameRatePtr = requestedFrameRate;
    }
    else
    {
        bestRateIndex = PaHost_FindClosestTableEntry( 1.0, pdi->sampleRates, pdi->numSampleRates, requestedFrameRate );
        if( bestRateIndex < 0 ) return paInvalidSampleRate;
        *closestFrameRatePtr = pdi->sampleRates[bestRateIndex];
    }
    return paNoError;
}

/*************************************************************************/
PaError Pa_OpenStream(
    PortAudioStream** streamPtrPtr,
    PaDeviceID inputDeviceID,
    int numInputChannels,
    PaSampleFormat inputSampleFormat,
    void *inputDriverInfo,
    PaDeviceID outputDeviceID,
    int numOutputChannels,
    PaSampleFormat outputSampleFormat,
    void *outputDriverInfo,
    double sampleRate,
    unsigned long framesPerBuffer,
    unsigned long numberOfBuffers,
    unsigned long streamFlags,
    PortAudioCallback *callback,
    void *userData )
{
    internalPortAudioStream   *past = NULL;
    PaError                    result = paNoError;
    int                        bitsPerInputSample;
    int                        bitsPerOutputSample;
    /* Print passed parameters. */
    DBUG(("Pa_OpenStream( %p, %d, %d, %d, %p, /* input */ \n",
          streamPtrPtr, inputDeviceID, numInputChannels,
          inputSampleFormat, inputDriverInfo ));
    DBUG(("               %d, %d, %d, %p, /* output */\n",
          outputDeviceID, numOutputChannels,
          outputSampleFormat, outputDriverInfo ));
    DBUG(("               %g, %d, %d, 0x%x, , %p )\n",
          sampleRate, framesPerBuffer, numberOfBuffers,
          streamFlags, userData ));

    /* Check for parameter errors. */
    if( (streamFlags & ~(paClipOff | paDitherOff)) != 0 ) return paInvalidFlag;
    if( streamPtrPtr == NULL ) return paBadStreamPtr;
    if( inputDriverInfo != NULL ) return paHostError; /* REVIEW */
    if( outputDriverInfo != NULL ) return paHostError; /* REVIEW */
    if( (inputDeviceID < 0) && ( outputDeviceID < 0) ) return paInvalidDeviceId;
    if( (outputDeviceID >= Pa_CountDevices()) || (inputDeviceID >= Pa_CountDevices()) )
    {
        return paInvalidDeviceId;
    }
    if( (numInputChannels <= 0) && ( numOutputChannels <= 0) ) return paInvalidChannelCount;

#if SUPPORT_AUDIO_CAPTURE
    if( inputDeviceID >= 0 )
    {
        PaError size = Pa_GetSampleSize( inputSampleFormat );
        if( size < 0 ) return size;
        bitsPerInputSample = 8 * size;
        if( (numInputChannels <= 0) ) return paInvalidChannelCount;
    }
#else
    if( inputDeviceID >= 0 )
    {
        return paInvalidChannelCount;
    }
#endif /* SUPPORT_AUDIO_CAPTURE */
    else
    {
        if( numInputChannels > 0 ) return paInvalidChannelCount;
        bitsPerInputSample = 0;
    }

    if( outputDeviceID >= 0 )
    {
        PaError size = Pa_GetSampleSize( outputSampleFormat );
        if( size < 0 ) return size;
        bitsPerOutputSample = 8 * size;
        if( (numOutputChannels <= 0) ) return paInvalidChannelCount;
    }
    else
    {
        if( numOutputChannels > 0 ) return paInvalidChannelCount;
        bitsPerOutputSample = 0;
    }

    if( callback == NULL ) return paNullCallback;

    /* Allocate and clear stream structure. */
    past = (internalPortAudioStream *) PaHost_AllocateFastMemory( sizeof(internalPortAudioStream) );
    if( past == NULL ) return paInsufficientMemory;
    memset( past, 0, sizeof(internalPortAudioStream) );
    AddTraceMessage("Pa_OpenStream: past", (long) past );

    past->past_Magic = PA_MAGIC;  /* Set ID to catch bugs. */
    past->past_FramesPerUserBuffer = framesPerBuffer;
    past->past_NumUserBuffers = numberOfBuffers; /* NOTE - PaHost_OpenStream() MUST CHECK FOR ZERO! */
    past->past_Callback = callback;
    past->past_UserData = userData;
    past->past_OutputSampleFormat = outputSampleFormat;
    past->past_InputSampleFormat = inputSampleFormat;
    past->past_OutputDeviceID = outputDeviceID;
    past->past_InputDeviceID = inputDeviceID;
    past->past_NumInputChannels = numInputChannels;
    past->past_NumOutputChannels = numOutputChannels;
    past->past_Flags = streamFlags;

    /* Check for absurd sample rates. */
    if( (sampleRate < 1000.0) || (sampleRate > 200000.0) )
    {
        result = paInvalidSampleRate;
        goto cleanup;
    }

    /* Allocate buffers that may be used for format conversion from user to native buffers. */
    if( numInputChannels > 0 )
    {

#if PA_VALIDATE_RATE
        result = PaHost_ValidateSampleRate( inputDeviceID, sampleRate, &past->past_SampleRate );
        if( result < 0 )
        {
            goto cleanup;
        }
#else
        past->past_SampleRate = sampleRate;
#endif
        /* Allocate single Input buffer for passing formatted samples to user callback. */
        past->past_InputBufferSize = framesPerBuffer * numInputChannels * ((bitsPerInputSample+7) / 8);
        past->past_InputBuffer = PaHost_AllocateFastMemory(past->past_InputBufferSize);
        if( past->past_InputBuffer == NULL )
        {
            result = paInsufficientMemory;
            goto cleanup;
        }
    }
    else
    {
        past->past_InputBuffer = NULL;
    }

    /* Allocate single Output buffer. */
    if( numOutputChannels > 0 )
    {
#if PA_VALIDATE_RATE
        result = PaHost_ValidateSampleRate( outputDeviceID, sampleRate, &past->past_SampleRate );
        if( result < 0 )
        {
            goto cleanup;
        }
#else
        past->past_SampleRate = sampleRate;
#endif
        past->past_OutputBufferSize = framesPerBuffer * numOutputChannels * ((bitsPerOutputSample+7) / 8);
        past->past_OutputBuffer = PaHost_AllocateFastMemory(past->past_OutputBufferSize);
        if( past->past_OutputBuffer == NULL )
        {
            result = paInsufficientMemory;
            goto cleanup;
        }
    }
    else
    {
        past->past_OutputBuffer = NULL;
    }

    result = PaHost_OpenStream( past );
    if( result < 0 ) goto cleanup;

    *streamPtrPtr = (void *) past;

    return result;

cleanup:
    if( past != NULL ) Pa_CloseStream( past );
    *streamPtrPtr = NULL;
    return result;
}


/*************************************************************************/
PaError Pa_OpenDefaultStream( PortAudioStream** stream,
                              int numInputChannels,
                              int numOutputChannels,
                              PaSampleFormat sampleFormat,
                              double sampleRate,
                              unsigned long framesPerBuffer,
                              unsigned long numberOfBuffers,
                              PortAudioCallback *callback,
                              void *userData )
{
    return Pa_OpenStream(
               stream,
               ((numInputChannels > 0) ? Pa_GetDefaultInputDeviceID() : paNoDevice),
               numInputChannels, sampleFormat, NULL,
               ((numOutputChannels > 0) ? Pa_GetDefaultOutputDeviceID() : paNoDevice),
               numOutputChannels, sampleFormat, NULL,
               sampleRate, framesPerBuffer, numberOfBuffers, paNoFlag, callback, userData );
}

/*************************************************************************/
PaError Pa_CloseStream( PortAudioStream* stream)
{
    PaError   result;
    internalPortAudioStream   *past;

    DBUG(("Pa_CloseStream()\n"));
    if( stream == NULL ) return paBadStreamPtr;
    past = (internalPortAudioStream *) stream;

    Pa_AbortStream( past );
    result = PaHost_CloseStream( past );

    if( past->past_InputBuffer ) PaHost_FreeFastMemory( past->past_InputBuffer, past->past_InputBufferSize );
    if( past->past_OutputBuffer ) PaHost_FreeFastMemory( past->past_OutputBuffer, past->past_OutputBufferSize );
    PaHost_FreeFastMemory( past, sizeof(internalPortAudioStream) );

    return result;
}

/*************************************************************************/
PaError Pa_StartStream( PortAudioStream *stream )
{
    PaError result = paHostError;
    internalPortAudioStream   *past;

    if( stream == NULL ) return paBadStreamPtr;
    past = (internalPortAudioStream *) stream;

    past->past_FrameCount = 0.0;

    if( past->past_NumInputChannels > 0 )
    {
        result = PaHost_StartInput( past );
        DBUG(("Pa_StartStream: PaHost_StartInput returned = 0x%X.\n", result));
        if( result < 0 ) goto error;
    }

    if( past->past_NumOutputChannels > 0 )
    {
        result = PaHost_StartOutput( past );
        DBUG(("Pa_StartStream: PaHost_StartOutput returned = 0x%X.\n", result));
        if( result < 0 ) goto error;
    }

    result = PaHost_StartEngine( past );
    DBUG(("Pa_StartStream: PaHost_StartEngine returned = 0x%X.\n", result));
    if( result < 0 ) goto error;

    return paNoError;

error:
    return result;
}

/*************************************************************************/
PaError Pa_StopStream(  PortAudioStream *stream )
{
    return Pa_KillStream( stream, 0 );
}

/*************************************************************************/
PaError Pa_AbortStream(  PortAudioStream *stream )
{
    return Pa_KillStream( stream, 1 );
}

/*************************************************************************/
static PaError Pa_KillStream(  PortAudioStream *stream, int abort )
{
    PaError result = paNoError;
    internalPortAudioStream   *past;

    DBUG(("Pa_StopStream().\n"));
    if( stream == NULL ) return paBadStreamPtr;
    past = (internalPortAudioStream *) stream;

    if( (past->past_NumInputChannels > 0) || (past->past_NumOutputChannels > 0) )
    {
        result = PaHost_StopEngine( past, abort );
        DBUG(("Pa_StopStream: PaHost_StopEngine returned = 0x%X.\n", result));
        if( result < 0 ) goto error;
    }

    if( past->past_NumInputChannels > 0 )
    {
        result = PaHost_StopInput( past, abort );
        DBUG(("Pa_StopStream: PaHost_StopInput returned = 0x%X.\n", result));
        if( result != paNoError ) goto error;
    }

    if( past->past_NumOutputChannels > 0 )
    {
        result = PaHost_StopOutput( past, abort );
        DBUG(("Pa_StopStream: PaHost_StopOutput returned = 0x%X.\n", result));
        if( result != paNoError ) goto error;
    }

error:
    past->past_Usage = 0;
    past->past_IfLastExitValid = 0;

    return result;
}

/*************************************************************************/
PaError Pa_StreamActive( PortAudioStream *stream )
{
    internalPortAudioStream   *past;
    if( stream == NULL ) return paBadStreamPtr;
    past = (internalPortAudioStream *) stream;
    return PaHost_StreamActive( past );
}

/*************************************************************************/
const char *Pa_GetErrorText( PaError errnum )
{
    const char *msg;

    switch(errnum)
    {
    case paNoError:                  msg = "Success"; break;
    case paHostError:                msg = "Host error."; break;
    case paInvalidChannelCount:      msg = "Invalid number of channels."; break;
    case paInvalidSampleRate:        msg = "Invalid sample rate."; break;
    case paInvalidDeviceId:          msg = "Invalid device ID."; break;
    case paInvalidFlag:              msg = "Invalid flag."; break;
    case paSampleFormatNotSupported: msg = "Sample format not supported"; break;
    case paBadIODeviceCombination:   msg = "Illegal combination of I/O devices."; break;
    case paInsufficientMemory:       msg = "Insufficient memory."; break;
    case paBufferTooBig:             msg = "Buffer too big."; break;
    case paBufferTooSmall:           msg = "Buffer too small."; break;
    case paNullCallback:             msg = "No callback routine specified."; break;
    case paBadStreamPtr:             msg = "Invalid stream pointer."; break;
    case paTimedOut    :             msg = "Wait Timed Out."; break;
    case paInternalError:            msg = "Internal PortAudio Error."; break;
    case paDeviceUnavailable:        msg = "Device Unavailable."; break;
    default:                         msg = "Illegal error number."; break;
    }
    return msg;
}

/*
 Get CPU Load as a fraction of total CPU time.
 A value of 0.5 would imply that PortAudio and the sound generating
 callback was consuming roughly 50% of the available CPU time.
 The amount may vary depending on CPU load.
 This function may be called from the callback function.
*/
double Pa_GetCPULoad(  PortAudioStream* stream)
{
    internalPortAudioStream   *past;
    if( stream == NULL ) return (double) paBadStreamPtr;
    past = (internalPortAudioStream *) stream;
    return past->past_Usage;
}

/*************************************************************************/
internalPortAudioStream* PaHost_GetStreamRepresentation( PortAudioStream *stream )
{
    internalPortAudioStream* result = (internalPortAudioStream*) stream;

    if( result == NULL || result->past_Magic != PA_MAGIC )
        return NULL;
    else
        return result;
}

/*************************************************************
** Calculate 2 LSB dither signal with a triangular distribution.
** Ranged properly for adding to a 32 bit integer prior to >>15.
** Range of output is +/- 32767
*/
#define PA_DITHER_BITS   (15)
#define PA_DITHER_SCALE  (1.0f / ((1<<PA_DITHER_BITS)-1))
long PaConvert_TriangularDither( void )
{
    static unsigned long previous = 0;
    static unsigned long randSeed1 = 22222;
    static unsigned long randSeed2 = 5555555;
    long current, highPass;
    /* Generate two random numbers. */
    randSeed1 = (randSeed1 * 196314165) + 907633515;
    randSeed2 = (randSeed2 * 196314165) + 907633515;
    /* Generate triangular distribution about 0.
     * Shift before adding to prevent overflow which would skew the distribution.
     * Also shift an extra bit for the high pass filter. 
     */
#define DITHER_SHIFT  ((32 - PA_DITHER_BITS) + 1)
    current = (((long)randSeed1)>>DITHER_SHIFT) + (((long)randSeed2)>>DITHER_SHIFT);
    /* High pass filter to reduce audibility. */
    highPass = current - previous;
    previous = current;
    return highPass;
}

/*************************************************************************
** Called by host code.
** Convert input from Int16, call user code, then convert output
** to Int16 format for native use.
** Assumes host native format is paInt16.
** Returns result from user callback.
*/
long Pa_CallConvertInt16( internalPortAudioStream   *past,
                          short *nativeInputBuffer,
                          short *nativeOutputBuffer )
{
    long              temp;
    int               userResult;
    unsigned int      i;
    void             *inputBuffer = NULL;
    void             *outputBuffer = NULL;

#if SUPPORT_AUDIO_CAPTURE
    /* Get native data from DirectSound. */
    if( (past->past_NumInputChannels > 0) && (nativeInputBuffer != NULL) )
    {
        /* Convert from native format to PA format. */
        unsigned int samplesPerBuffer = past->past_FramesPerUserBuffer * past->past_NumInputChannels;
        switch(past->past_InputSampleFormat)
        {

        case paFloat32:
            {
                float *inBufPtr = (float *) past->past_InputBuffer;
                inputBuffer = past->past_InputBuffer;
                for( i=0; i<samplesPerBuffer; i++ )
                {
                    inBufPtr[i] = nativeInputBuffer[i] * (1.0f / 32767.0f);
                }
                break;
            }

        case paInt32:
            {
                /* Convert 16 bit data to 32 bit integers */
                int *inBufPtr = (int *) past->past_InputBuffer;
                inputBuffer = past->past_InputBuffer;
                for( i=0; i<samplesPerBuffer; i++ )
                {
                    inBufPtr[i] = nativeInputBuffer[i] << 16;
                }
                break;
            }

        case paInt16:
            {
                /* Already in correct format so don't copy. */
                inputBuffer = nativeInputBuffer;
                break;
            }

        case paInt8:
            {
                /* Convert 16 bit data to 8 bit chars */
                char *inBufPtr = (char *) past->past_InputBuffer;
                inputBuffer = past->past_InputBuffer;
                if( past->past_Flags & paDitherOff )
                {
                    for( i=0; i<samplesPerBuffer; i++ )
                    {
                        inBufPtr[i] = (char)(nativeInputBuffer[i] >> 8);
                    }
                }
                else
                {
                    for( i=0; i<samplesPerBuffer; i++ )
                    {
                        temp = nativeInputBuffer[i];
                        temp += PaConvert_TriangularDither() >> 8; /* PLB20010820 */
                        temp = ((temp < -0x8000) ? -0x8000 : ((temp > 0x7FFF) ? 0x7FFF : temp));
                        inBufPtr[i] = (char)(temp >> 8);
                    }
                }
                break;
            }

        case paUInt8:
            {
                /* Convert 16 bit data to 8 bit unsigned chars */
                unsigned char *inBufPtr = (unsigned char *) past->past_InputBuffer;
                inputBuffer = past->past_InputBuffer;
                if( past->past_Flags & paDitherOff )
                {
                    for( i=0; i<samplesPerBuffer; i++ )
                    {
                        inBufPtr[i] = (unsigned char)((nativeInputBuffer[i] >> 8) + 0x80);
                    }
                }
                else
                {
                    /* If you dither then you have to clip because dithering could push the signal out of range! */
                    for( i=0; i<samplesPerBuffer; i++ )
                    {
                        temp = nativeInputBuffer[i];
                        temp += PaConvert_TriangularDither() >> 8; /* PLB20010820 */
                        temp = ((temp < -0x8000) ? -0x8000 : ((temp > 0x7FFF) ? 0x7FFF : temp));
                        inBufPtr[i] = (unsigned char)((temp>>8) + 0x80); /* PLB20010820 */
                    }
                }
                break;
            }

        default:
            break;
        }
    }
#endif /* SUPPORT_AUDIO_CAPTURE */

    /* Are we doing output time? */
    if( (past->past_NumOutputChannels > 0) && (nativeOutputBuffer != NULL) )
    {
        /* May already be in native format so just write directly to native buffer. */
        outputBuffer = (past->past_OutputSampleFormat == paInt16) ?
                       nativeOutputBuffer : past->past_OutputBuffer;
    }
    /*
     AddTraceMessage("Pa_CallConvertInt16: inputBuffer = ", (int) inputBuffer );
     AddTraceMessage("Pa_CallConvertInt16: outputBuffer = ", (int) outputBuffer );
    */
    /* Call user callback routine. */
    userResult = past->past_Callback(
                     inputBuffer,
                     outputBuffer,
                     past->past_FramesPerUserBuffer,
                     past->past_FrameCount,
                     past->past_UserData );

    past->past_FrameCount += (PaTimestamp) past->past_FramesPerUserBuffer;

    /* Convert to native format if necessary. */
    if( outputBuffer != NULL )
    {
        unsigned int samplesPerBuffer = past->past_FramesPerUserBuffer * past->past_NumOutputChannels;
        switch(past->past_OutputSampleFormat)
        {
        case paFloat32:
            {
                float *outBufPtr = (float *) past->past_OutputBuffer;
                if( past->past_Flags & paDitherOff )
                {
                    if( past->past_Flags & paClipOff ) /* NOTHING */
                    {
                        for( i=0; i<samplesPerBuffer; i++ )
                        {
                            *nativeOutputBuffer++ = (short) (outBufPtr[i] * (32767.0f));
                        }
                    }
                    else /* CLIP */
                    {
                        for( i=0; i<samplesPerBuffer; i++ )
                        {
                            temp = (long)(outBufPtr[i] * 32767.0f);
                            *nativeOutputBuffer++ = (short)((temp < -0x8000) ? -0x8000 : ((temp > 0x7FFF) ? 0x7FFF : temp));
                        }
                    }
                }
                else
                {
                    /* If you dither then you have to clip because dithering could push the signal out of range! */
                    for( i=0; i<samplesPerBuffer; i++ )
                    {
                        float dither  = PaConvert_TriangularDither()*PA_DITHER_SCALE;
                        float dithered = (outBufPtr[i] * (32767.0f)) + dither;
                        temp = (long) (dithered);
                        *nativeOutputBuffer++ = (short)((temp < -0x8000) ? -0x8000 : ((temp > 0x7FFF) ? 0x7FFF : temp));
                    }
                }
                break;
            }

        case paInt32:
            {
                int *outBufPtr = (int *) past->past_OutputBuffer;
                if( past->past_Flags & paDitherOff )
                {
                    for( i=0; i<samplesPerBuffer; i++ )
                    {
                        *nativeOutputBuffer++ = (short) (outBufPtr[i] >> 16 );
                    }
                }
                else
                {
                    for( i=0; i<samplesPerBuffer; i++ )
                    {
                        /* Shift one bit down before dithering so that we have room for overflow from add. */
                        temp = (outBufPtr[i] >> 1) + PaConvert_TriangularDither();
                        temp = temp >> 15;
                        *nativeOutputBuffer++ = (short)((temp < -0x8000) ? -0x8000 : ((temp > 0x7FFF) ? 0x7FFF : temp));
                    }
                }
                break;
            }

        case paInt8:
            {
                char *outBufPtr = (char *) past->past_OutputBuffer;
                for( i=0; i<samplesPerBuffer; i++ )
                {
                    *nativeOutputBuffer++ = (short) (((int)outBufPtr[i]) << 8);
                }
                break;
            }

        case paUInt8:
            {
                unsigned char *outBufPtr = (unsigned char *) past->past_OutputBuffer;
                for( i=0; i<samplesPerBuffer; i++ )
                {
                    *nativeOutputBuffer++ = (short) (((int)(outBufPtr[i] - 0x80)) << 8);
                }
                break;
            }

        default:
            break;
        }

    }

    return userResult;
}

/*************************************************************************/
PaError Pa_Initialize( void )
{
    if( gInitCount++ > 0 ) return paNoError;
    ResetTraceMessages();
    return PaHost_Init();
}

PaError Pa_Terminate( void )
{
    PaError result = paNoError;

    if( gInitCount == 0 ) return paNoError;
    else if( --gInitCount == 0 )
    {
        result = PaHost_Term();
        DumpTraceMessages();
    }
    return result;
}

int PaHost_IsInitialized()
{
    return gInitCount;
}

/*************************************************************************/
PaError Pa_GetSampleSize( PaSampleFormat format )
{
    int size;
    switch(format )
    {

    case paUInt8:
    case paInt8:
        size = 1;
        break;

    case paInt16:
        size = 2;
        break;

    case paPackedInt24:
        size = 3;
        break;

    case paFloat32:
    case paInt32:
    case paInt24:
        size = 4;
        break;

    default:
        size = paSampleFormatNotSupported;
        break;
    }
    return (PaError) size;
}


