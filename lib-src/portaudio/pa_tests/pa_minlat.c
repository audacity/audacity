/*
 * $Id: pa_minlat.c,v 1.2 2003-03-02 08:01:40 dmazzoni Exp $
 * paminlat.c
 * Experiment with different numbers of buffers to determine the
 * minimum latency for a computer.
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
#include <stdlib.h>
#include <math.h>
#include "portaudio.h"

#ifndef M_PI
#define M_PI  (3.14159265)
#endif
#define TWOPI (M_PI * 2.0)

#define DEFAULT_BUFFER_SIZE   (64)

typedef struct
{
    double left_phase;
    double right_phase;
}
paTestData;

/* Very simple synthesis routine to generate two sine waves. */
static int paminlatCallback( void *inputBuffer, void *outputBuffer,
                             unsigned long framesPerBuffer,
                             PaTimestamp outTime, void *userData )
{
    paTestData *data = (paTestData*)userData;
    float *out = (float*)outputBuffer;
    unsigned int i;
    double left_phaseInc = 0.02;
    double right_phaseInc = 0.06;

    double left_phase = data->left_phase;
    double right_phase = data->right_phase;

    for( i=0; i<framesPerBuffer; i++ )
    {
        left_phase += left_phaseInc;
        if( left_phase > TWOPI ) left_phase -= TWOPI;
        *out++ = (float) sin( left_phase );

        right_phase += right_phaseInc;
        if( right_phase > TWOPI ) right_phase -= TWOPI;
        *out++ = (float) sin( right_phase );
    }

    data->left_phase = left_phase;
    data->right_phase = right_phase;
    return 0;
}
void main( int argc, char **argv );
void main( int argc, char **argv )
{
    PortAudioStream *stream;
    PaError err;
    paTestData data;
    int    go;
    int    numBuffers = 0;
    int    minBuffers = 0;
    int    framesPerBuffer;
    double sampleRate = 44100.0;
    char   str[256];
    printf("paminlat - Determine minimum latency for your computer.\n");
    printf("  usage:         paminlat {framesPerBuffer}\n");
    printf("  for example:   paminlat 256\n");
    printf("Adjust your stereo until you hear a smooth tone in each speaker.\n");
    printf("Then try to find the smallest number of buffers that still sounds smooth.\n");
    printf("Note that the sound will stop momentarily when you change the number of buffers.\n");
    /* Get bufferSize from command line. */
    framesPerBuffer = ( argc > 1 ) ? atol( argv[1] ) : DEFAULT_BUFFER_SIZE;
    printf("Frames per buffer = %d\n", framesPerBuffer );

    data.left_phase = data.right_phase = 0.0;
    err = Pa_Initialize();
    if( err != paNoError ) goto error;
    /* Ask PortAudio for the recommended minimum number of buffers. */
    numBuffers = minBuffers = Pa_GetMinNumBuffers( framesPerBuffer, sampleRate );
    printf("NumBuffers set to %d based on a call to Pa_GetMinNumBuffers()\n", numBuffers );
    /* Try different numBuffers in a loop. */
    go = 1;
    while( go )
    {

        printf("Latency = framesPerBuffer * numBuffers = %d * %d = %d frames = %d msecs.\n",
               framesPerBuffer, numBuffers, framesPerBuffer*numBuffers,
               (int)((1000 * framesPerBuffer * numBuffers) / sampleRate) );
        err = Pa_OpenStream(
                  &stream,
                  paNoDevice,
                  0,              /* no input */
                  paFloat32,  /* 32 bit floating point input */
                  NULL,
                  Pa_GetDefaultOutputDeviceID(), /* default output device */
                  2,              /* stereo output */
                  paFloat32,      /* 32 bit floating point output */
                  NULL,
                  sampleRate,
                  framesPerBuffer,
                  numBuffers,     /* number of buffers */
                  paClipOff,      /* we won't output out of range samples so don't bother clipping them */
                  paminlatCallback,
                  &data );
        if( err != paNoError ) goto error;
        if( stream == NULL ) goto error;
        /* Start audio. */
        err = Pa_StartStream( stream );
        if( err != paNoError ) goto error;
        /* Ask user for a new number of buffers. */
        printf("\nMove windows around to see if the sound glitches.\n");
        printf("NumBuffers currently %d, enter new number, or 'q' to quit: ", numBuffers );
        gets( str );
        if( str[0] == 'q' ) go = 0;
        else
        {
            numBuffers = atol( str );
            if( numBuffers < minBuffers )
            {
                printf( "numBuffers below minimum of %d! Set to minimum!!!\n", minBuffers );
                numBuffers = minBuffers;
            }
        }
        /* Stop sound until ENTER hit. */
        err = Pa_StopStream( stream );
        if( err != paNoError ) goto error;
        err = Pa_CloseStream( stream );
        if( err != paNoError ) goto error;
    }
    printf("A good setting for latency would be somewhat higher than\n");
    printf("the minimum latency that worked.\n");
    printf("PortAudio: Test finished.\n");
    Pa_Terminate();
    return;
error:
    Pa_Terminate();
    fprintf( stderr, "An error occured while using the portaudio stream\n" );
    fprintf( stderr, "Error number: %d\n", err );
    fprintf( stderr, "Error message: %s\n", Pa_GetErrorText( err ) );
}
