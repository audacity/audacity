/** @file patest_two_rates.c
	@ingroup test_src
	@brief Play two streams at different rates to make sure they don't interfere.
	@author Phil Burk <philburk@softsynth.com>
*/
/*
 * $Id$
 *
 * Author: Phil Burk  http://www.softsynth.com
 *
 * This program uses the PortAudio Portable Audio Library.
 * For more information see: http://www.portaudio.com
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

#include <stdio.h>
#include <math.h>
#include "portaudio.h"

#define OUTPUT_DEVICE       (Pa_GetDefaultOutputDeviceID())
#define SAMPLE_RATE_1       (44100)
#define SAMPLE_RATE_2       (48000)
#define FRAMES_PER_BUFFER   (256)
#define FREQ_INCR           (0.1)

#ifndef M_PI
#define M_PI  (3.14159265)
#endif

typedef struct
{
    double phase;
    int    numFrames;
} paTestData;

/* This routine will be called by the PortAudio engine when audio is needed.
 ** It may called at interrupt level on some machines so don't do anything
 ** that could mess up the system like calling malloc() or free().
 */
static int patestCallback( const void *inputBuffer, void *outputBuffer,
						  unsigned long framesPerBuffer,
						  const PaStreamCallbackTimeInfo* timeInfo,
						  PaStreamCallbackFlags statusFlags,
						  void *userData )
{
    paTestData *data = (paTestData*)userData;
    float *out = (float*)outputBuffer;
    int frameIndex;
    (void) timeInfo; /* Prevent unused variable warnings. */
    (void) inputBuffer;

    for( frameIndex=0; frameIndex<(int)framesPerBuffer; frameIndex++ )
    {
        /* Generate sine wave. */
        float value = (float) 0.3 * sin(data->phase);
        /* Stereo - two channels. */
        *out++ = value;
        *out++ = value;

        data->phase += FREQ_INCR;
        if( data->phase >= (2.0 * M_PI) ) data->phase -= (2.0 * M_PI);
    }
	data->numFrames += 1;
    return 0;
}

/*******************************************************************/
int main(void);
int main(void)
{
    PaError err;
    PaStreamParameters outputParameters;
    PaStream *stream1;
    PaStream *stream2;
    paTestData data1 = {0};
    paTestData data2 = {0};
    printf("PortAudio Test: two rates.\n" );

    err = Pa_Initialize();
    if( err != paNoError ) goto error;
    
    outputParameters.device = Pa_GetDefaultOutputDevice(); /* default output device */
    if (outputParameters.device == paNoDevice) {
		fprintf(stderr,"Error: No default output device.\n");
		goto error;
    }
    outputParameters.channelCount = 2;       /* stereo output */
    outputParameters.sampleFormat = paFloat32; /* 32 bit floating point output */
    outputParameters.suggestedLatency = Pa_GetDeviceInfo( outputParameters.device )->defaultLowOutputLatency;
    outputParameters.hostApiSpecificStreamInfo = NULL;

    /* Start first stream. **********************/	
    err = Pa_OpenStream(
						&stream1,
						NULL, /* no input */
						&outputParameters,
						SAMPLE_RATE_1,
						FRAMES_PER_BUFFER,
						paClipOff,      /* we won't output out of range samples so don't bother clipping them */
						patestCallback,
						&data1 );
	if( err != paNoError ) goto error;

    err = Pa_StartStream( stream1 );
    if( err != paNoError ) goto error;

    Pa_Sleep( 3 * 1000 );

    /* Start second stream. **********************/
    err = Pa_OpenStream(
						&stream2,
						NULL, /* no input */
						&outputParameters,
						SAMPLE_RATE_2,
						FRAMES_PER_BUFFER,
						paClipOff,      /* we won't output out of range samples so don't bother clipping them */
						patestCallback,
						&data2 );
    if( err != paNoError ) goto error;

    err = Pa_StartStream( stream2 );
    if( err != paNoError ) goto error;

    Pa_Sleep( 3 * 1000 );
    
    err = Pa_StopStream( stream2 );
    if( err != paNoError ) goto error;

    Pa_Sleep( 3 * 1000 );
    
    err = Pa_StopStream( stream1 );
    if( err != paNoError ) goto error;

    Pa_CloseStream( stream2 );
    Pa_CloseStream( stream1 );
    
    Pa_Terminate();
	
    printf("NumFrames = %d on stream1, %d on stream2.\n", data1.numFrames, data2.numFrames );
    printf("Test finished.\n");
    return err;
error:
    Pa_Terminate();
    fprintf( stderr, "An error occured while using the portaudio stream\n" );
    fprintf( stderr, "Error number: %d\n", err );
    fprintf( stderr, "Error message: %s\n", Pa_GetErrorText( err ) );
    return err;
}
