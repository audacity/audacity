/** @file patest_leftright.c
	@ingroup test_src
	@brief Play different tone sine waves that 
		alternate between left and right channel.

	The low tone should be on the left channel.

	@author Ross Bencina <rossb@audiomulch.com>
	@author Phil Burk <philburk@softsynth.com>
*/
/*
 * $Id$
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

#define NUM_SECONDS   (8)
#define SAMPLE_RATE   (44100)
#define FRAMES_PER_BUFFER  (512)
#ifndef M_PI
#define M_PI  (3.14159265)
#endif
#define TABLE_SIZE   (200)
#define BALANCE_DELTA  (0.001)

typedef struct
{
    float sine[TABLE_SIZE];
    int left_phase;
    int right_phase;
    float targetBalance; // 0.0 = left, 1.0 = right
    float currentBalance;
} paTestData;

/* This routine will be called by the PortAudio engine when audio is needed.
** It may called at interrupt level on some machines so don't do anything
** that could mess up the system like calling malloc() or free().
*/
static int patestCallback( const void *inputBuffer,
                           void *outputBuffer,
                           unsigned long framesPerBuffer,
                           const PaStreamCallbackTimeInfo* timeInfo,
                           PaStreamCallbackFlags statusFlags,
                           void *userData )
{
    paTestData *data = (paTestData*)userData;
    float *out = (float*)outputBuffer;
    unsigned long i;
    int finished = 0;
    /* Prevent unused variable warnings. */
    (void) inputBuffer;

    for( i=0; i<framesPerBuffer; i++ )
    {
		// Smoothly pan between left and right.
		if( data->currentBalance < data->targetBalance )
		{
			data->currentBalance += BALANCE_DELTA;
		}
		else if( data->currentBalance > data->targetBalance )
		{
			data->currentBalance -= BALANCE_DELTA;
		}
		// Apply left/right balance.
        *out++ = data->sine[data->left_phase] * (1.0f - data->currentBalance);  /* left */
		*out++ = data->sine[data->right_phase] * data->currentBalance;  /* right */
		
        data->left_phase += 1;
        if( data->left_phase >= TABLE_SIZE ) data->left_phase -= TABLE_SIZE;
        data->right_phase += 3; /* higher pitch so we can distinguish left and right. */
        if( data->right_phase >= TABLE_SIZE ) data->right_phase -= TABLE_SIZE;
    }

    return finished;
}

/*******************************************************************/
int main(void);
int main(void)
{
    PaStream *stream;
    PaStreamParameters outputParameters;
    PaError err;
    paTestData data;
    int i;    
    printf("Play different tone sine waves that alternate between left and right channel.\n");
    printf("The low tone should be on the left channel.\n");
    
    /* initialise sinusoidal wavetable */
    for( i=0; i<TABLE_SIZE; i++ )
    {
        data.sine[i] = (float) sin( ((double)i/(double)TABLE_SIZE) * M_PI * 2. );
    }
    data.left_phase = data.right_phase = 0;
    data.currentBalance = 0.0;
    data.targetBalance = 0.0;

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

    err = Pa_OpenStream( &stream,
                         NULL,                  /* No input. */
                         &outputParameters,     /* As above. */
                         SAMPLE_RATE,
                         FRAMES_PER_BUFFER,
                         paClipOff,      /* we won't output out of range samples so don't bother clipping them */
                         patestCallback,
                         &data );
    if( err != paNoError ) goto error;
    
    err = Pa_StartStream( stream );
    if( err != paNoError ) goto error;
    
    printf("Play for several seconds.\n");
    for( i=0; i<4; i++ )
	{
		printf("Hear low sound on left side.\n");
		data.targetBalance = 0.01;
        Pa_Sleep( 1000 );
		
		printf("Hear high sound on right side.\n");
		data.targetBalance = 0.99;
        Pa_Sleep( 1000 ); 
	}

    err = Pa_StopStream( stream );
    if( err != paNoError ) goto error;
    err = Pa_CloseStream( stream );
    if( err != paNoError ) goto error;
    Pa_Terminate();
    printf("Test finished.\n");
    return err;
error:
    Pa_Terminate();
    fprintf( stderr, "An error occured while using the portaudio stream\n" );
    fprintf( stderr, "Error number: %d\n", err );
    fprintf( stderr, "Error message: %s\n", Pa_GetErrorText( err ) );
    return err;
}
