/*
 * $Id: patest_dither.c,v 1.2 2003-03-02 08:01:42 dmazzoni Exp $
 * patest_dither.c
 * Attempt to hear difference between dithered and non-dithered signal.
 * This only has an effect if the native format is 16 bit.
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
#include <math.h>
#include "portaudio.h"
#define NUM_SECONDS   (4)
#define SAMPLE_RATE   (44100)
#ifndef M_PI
#define M_PI  (3.14159265)
#endif
#define TABLE_SIZE   (200)
typedef struct paTestData
{
    float sine[TABLE_SIZE];
    float amplitude;
    int left_phase;
    int right_phase;
}
paTestData;
PaError PlaySine( paTestData *data, PaStreamFlags flags, float amplitude );
static int sineCallback( void *inputBuffer, void *outputBuffer,
                         unsigned long framesPerBuffer,
                         PaTimestamp outTime, void *userData );
/* This routine will be called by the PortAudio engine when audio is needed.
** It may called at interrupt level on some machines so don't do anything
** that could mess up the system like calling malloc() or free().
*/
static int sineCallback( void *inputBuffer, void *outputBuffer,
                         unsigned long framesPerBuffer,
                         PaTimestamp outTime, void *userData )
{
    paTestData *data = (paTestData*)userData;
    float *out = (float*)outputBuffer;
    float amplitude = data->amplitude;
    unsigned int i;
    (void) outTime;
    (void) inputBuffer;
    for( i=0; i<framesPerBuffer; i++ )
    {
        *out++ = amplitude * data->sine[data->left_phase];  /* left */
        *out++ = amplitude * data->sine[data->right_phase];  /* right */
        data->left_phase += 1;
        if( data->left_phase >= TABLE_SIZE ) data->left_phase -= TABLE_SIZE;
        data->right_phase += 3; /* higher pitch so we can distinguish left and right. */
        if( data->right_phase >= TABLE_SIZE ) data->right_phase -= TABLE_SIZE;
    }
    return 0;
}
/*******************************************************************/
int main(void);
int main(void)
{
    PaError err;
    paTestData DATA;
    int i;
    float amplitude = 32.0 / (1<<15);
    printf("PortAudio Test: output EXTREMELY QUIET sine wave with and without dithering.\n");
    /* initialise sinusoidal wavetable */
    for( i=0; i<TABLE_SIZE; i++ )
    {
        DATA.sine[i] = (float) sin( ((double)i/(double)TABLE_SIZE) * M_PI * 2. );
    }
    printf("\nNo treatment..\n"); fflush(stdout);
    err = PlaySine( &DATA, paClipOff | paDitherOff, amplitude );
    if( err < 0 ) goto error;
    printf("\nClip.\n");
    fflush(stdout);
    err = PlaySine( &DATA, paDitherOff, amplitude );
    if( err < 0 ) goto error;
    printf("\nClip and Dither.\n");
    fflush(stdout);
    err = PlaySine( &DATA, paNoFlag, amplitude );
    if( err < 0 ) goto error;
    return 0;
error:
    fprintf( stderr, "An error occured while using the portaudio stream\n" );
    fprintf( stderr, "Error number: %d\n", err );
    fprintf( stderr, "Error message: %s\n", Pa_GetErrorText( err ) );
    return -1;
}
/*****************************************************************************/
PaError PlaySine( paTestData *data, PaStreamFlags flags, float amplitude )
{
    PortAudioStream *stream;
    PaError err;
    data->left_phase = data->right_phase = 0;
    data->amplitude = amplitude;
    err = Pa_Initialize();
    if( err != paNoError ) goto error;
    err = Pa_OpenStream(
              &stream,
              paNoDevice,/* default input device */
              0,              /* no input */
              paFloat32,  /* 32 bit floating point input */
              NULL,
              Pa_GetDefaultOutputDeviceID(), /* default output device */
              2,              /* stereo output */
              paFloat32,      /* 32 bit floating point output */
              NULL,
              SAMPLE_RATE,
              1024,
              0,              /* number of buffers, if zero then use default minimum */
              flags,      /* we won't output out of range samples so don't bother clipping them */
              sineCallback,
              (void *)data );
    if( err != paNoError ) goto error;

    err = Pa_StartStream( stream );
    if( err != paNoError ) goto error;
    Pa_Sleep( NUM_SECONDS * 1000 );
    printf("CPULoad = %8.6f\n", Pa_GetCPULoad( stream ) );
    err = Pa_CloseStream( stream );
    if( err != paNoError ) goto error;
    Pa_Terminate();
    return paNoError;
error:
    return err;
}
