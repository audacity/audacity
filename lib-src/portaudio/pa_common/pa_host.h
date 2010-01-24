#ifndef PA_HOST_H
#define PA_HOST_H

/*
 * $Id: pa_host.h,v 1.5 2003-03-02 08:01:34 dmazzoni Exp $
 * Host dependant internal API for PortAudio
 *
 * Author: Phil Burk  <philburk@softsynth.com>
 *
 * PortAudio Portable Real-Time Audio Library
 * Latest Version at: http://www.softsynth.com/portaudio/
 * DirectSound and Macintosh Implementation
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
 
#include "portaudio.h"

#ifdef __cplusplus
extern "C"
{
#endif /* __cplusplus */

#ifndef SUPPORT_AUDIO_CAPTURE
#define SUPPORT_AUDIO_CAPTURE  (1)
#endif

#ifndef int32
    typedef long int32;
#endif
#ifndef uint32
    typedef unsigned long uint32;
#endif
#ifndef int16
    typedef short int16;
#endif
#ifndef uint16
    typedef unsigned short uint16;
#endif

/* Used to convert between various sample formats. */
typedef void (PortAudioConverter)(
    void *inputBuffer, int inputStride,
    void *outputBuffer, int outputStride,
    int numSamples );

#define PA_MAGIC    (0x18273645)

/************************************************************************************/
/****************** Structures ******************************************************/
/************************************************************************************/

typedef struct internalPortAudioStream
{
    uint32                    past_Magic;  /* ID for struct to catch bugs. */
    
    /* Begin user specified information. */
    uint32                    past_FramesPerUserBuffer;
    uint32                    past_NumUserBuffers;
    double                    past_SampleRate;     /* Closest supported sample rate. */
    int                       past_NumInputChannels;
    int                       past_NumOutputChannels;
    PaDeviceID                past_InputDeviceID;
    PaDeviceID                past_OutputDeviceID;
    PaSampleFormat            past_InputSampleFormat;
    PaSampleFormat            past_OutputSampleFormat;
    PortAudioCallback        *past_Callback;
    void                     *past_UserData;
    uint32                    past_Flags;
    /* End user specified information. */
    
    void                     *past_DeviceData;
    PaSampleFormat            past_NativeOutputSampleFormat;
    PaSampleFormat            past_NativeInputSampleFormat;

    /* Flags for communicating between foreground and background. */
    volatile int              past_IsActive;      /* Background is still playing. */
    volatile int              past_StopSoon;      /* Background should keep playing when buffers empty. */
    volatile int              past_StopNow;       /* Background should stop playing now. */
    /* These buffers are used when the native format does not match the user format. */
    void                     *past_InputBuffer;
    uint32                    past_InputBufferSize; /* Size in bytes of the input buffer. */
    void                     *past_OutputBuffer;
    uint32                    past_OutputBufferSize;
    /* Measurements */
    uint32                    past_NumCallbacks;
    PaTimestamp               past_FrameCount;    /* Frames output to buffer. */
    /* For measuring CPU utilization. */
    double                    past_AverageInsideCount;
    double                    past_AverageTotalCount;
    double                    past_Usage;
    int                       past_IfLastExitValid;
    /* Format Conversion */
    /* These are setup by PaConversion_Setup() */
    PortAudioConverter       *past_InputConversionProc;
    int                       past_InputConversionSourceStride;
    int                       past_InputConversionTargetStride;
    PortAudioConverter       *past_OutputConversionProc;
    int                       past_OutputConversionSourceStride;
    int                       past_OutputConversionTargetStride;
}
internalPortAudioStream;

/************************************************************************************/
/******** These functions must be provided by a platform implementation. ************/
/************************************************************************************/

PaError PaHost_Init( void );
PaError PaHost_Term( void );

PaError PaHost_OpenStream( internalPortAudioStream   *past );
PaError PaHost_CloseStream( internalPortAudioStream   *past );

PaError PaHost_StartOutput( internalPortAudioStream   *past );
PaError PaHost_StopOutput( internalPortAudioStream   *past, int abort );
PaError PaHost_StartInput( internalPortAudioStream   *past );
PaError PaHost_StopInput( internalPortAudioStream   *past, int abort );
PaError PaHost_StartEngine( internalPortAudioStream   *past );
PaError PaHost_StopEngine( internalPortAudioStream *past, int abort );
PaError PaHost_StreamActive( internalPortAudioStream   *past );

void   *PaHost_AllocateFastMemory( long numBytes );
void    PaHost_FreeFastMemory( void *addr, long numBytes );

/* This only called if PA_VALIDATE_RATE IS CALLED. */
PaError PaHost_ValidateSampleRate( PaDeviceID id, double requestedFrameRate,
                                   double *closestFrameRatePtr );

/**********************************************************************/
/************ Common Utility Routines provided by PA ******************/
/**********************************************************************/

/* PaHost_IsInitialized() returns non-zero if PA is initialized, 0 otherwise */
int PaHost_IsInitialized( void );

internalPortAudioStream* PaHost_GetStreamRepresentation( PortAudioStream *stream );

int PaHost_FindClosestTableEntry( double allowableError,  const double *rateTable,
                                  int numRates, double frameRate );

long Pa_CallConvertInt16( internalPortAudioStream   *past,
                          short *nativeInputBuffer,
                          short *nativeOutputBuffer );
                          
/* Calculate 2 LSB dither signal with a triangular distribution.
** Ranged properly for adding to a 32 bit 1.31 fixed point value prior to >>15.
** Range of output is +/- 65535
** Multiply by PA_DITHER_SCALE to get a float between -2.0 and 2.0. */
#define PA_DITHER_BITS   (15)
#define PA_DITHER_SCALE  (1.0f / ((1<<PA_DITHER_BITS)-1))
long PaConvert_TriangularDither( void );

PaError PaConvert_SetupInput( internalPortAudioStream   *past,
    PaSampleFormat   nativeInputSampleFormat );

PaError PaConvert_SetupOutput( internalPortAudioStream   *past,
    PaSampleFormat   nativeOutputSampleFormat );

long PaConvert_Process( internalPortAudioStream   *past,
            void *nativeInputBuffer,
            void *nativeOutputBuffer );

#ifdef __cplusplus
}
#endif /* __cplusplus */
#endif /* PA_HOST_H */
