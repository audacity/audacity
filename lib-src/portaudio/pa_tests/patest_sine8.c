/*
 * $Id: patest_sine8.c,v 1.2 2003-03-02 08:01:43 dmazzoni Exp $
 * patest_sine8.c
 * Play a sine wave using the Portable Audio api for several seconds.
 * Test 8 bit data.
 *
 * Author: Ross Bencina <rossb@audiomulch.com>
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
#include <math.h>
#include "portaudio.h"
#define NUM_SECONDS   (8)
#define SAMPLE_RATE   (44100)
#define TEST_UNSIGNED    (1)
#if TEST_UNSIGNED
#define TEST_FORMAT     paUInt8
#else
#define TEST_FORMAT     paInt8
#endif
#ifndef M_PI
#define M_PI  (3.14159265)
#endif
#define TABLE_SIZE   (200)
typedef struct
{
#if TEST_UNSIGNED
    unsigned char sine[TABLE_SIZE];
#else
    char sine[TABLE_SIZE];
#endif
    int left_phase;
    int right_phase;
    unsigned int framesToGo;
}
paTestData;
/* This routine will be called by the PortAudio engine when audio is needed.
** It may called at interrupt level on some machines so don't do anything
** that could mess up the system like calling malloc() or free().
*/
static int patestCallback( void *inputBuffer, void *outputBuffer,
                           unsigned long framesPerBuffer,
                           PaTimestamp outTime, void *userData )
{
    paTestData *data = (paTestData*)userData;
    char *out = (char*)outputBuffer;
    int i;
    int framesToCalc;
    int finished = 0;
    (void) outTime; /* Prevent unused variable warnings. */
    (void) inputBuffer;

    if( data->framesToGo < framesPerBuffer )
    {
        framesToCalc = data->framesToGo;
        data->framesToGo = 0;
        finished = 1;
    }
    else
    {
        framesToCalc = framesPerBuffer;
        data->framesToGo -= framesPerBuffer;
    }

    for( i=0; i<framesToCalc; i++ )
    {
        *out++ = data->sine[data->left_phase];  /* left */
        *out++ = data->sine[data->right_phase];  /* right */
        data->left_phase += 1;
        if( data->left_phase >= TABLE_SIZE ) data->left_phase -= TABLE_SIZE;
        data->right_phase += 3; /* higher pitch so we can distinguish left and right. */
        if( data->right_phase >= TABLE_SIZE ) data->right_phase -= TABLE_SIZE;
    }
    /* zero remainder of final buffer */
    for( ; i<(int)framesPerBuffer; i++ )
    {
#if TEST_UNSIGNED
        *out++ = (unsigned char) 0x80; /* left */
        *out++ = (unsigned char) 0x80; /* right */
#else
        *out++ = 0; /* left */
        *out++ = 0; /* right */
#endif

    }
    return finished;
}
/*******************************************************************/
int main(void);
int main(void)
{
    PortAudioStream *stream;
    PaError err;
    paTestData data;
    int i;
    int totalSamps;
#if TEST_UNSIGNED
    printf("PortAudio Test: output UNsigned 8 bit sine wave.\n");
#else
    printf("PortAudio Test: output signed 8 bit sine wave.\n");
#endif
    /* initialise sinusoidal wavetable */
    for( i=0; i<TABLE_SIZE; i++ )
    {
        data.sine[i] = (char) (127.0 * sin( ((double)i/(double)TABLE_SIZE) * M_PI * 2. ));
#if TEST_UNSIGNED
        data.sine[i] += (unsigned char) 0x80;
#endif

    }
    data.left_phase = data.right_phase = 0;
    data.framesToGo = totalSamps =  NUM_SECONDS * SAMPLE_RATE; /* Play for a few seconds. */
    err = Pa_Initialize();
    if( err != paNoError ) goto error;
    err = Pa_OpenStream(
              &stream,
              paNoDevice,/* default input device */
              0,              /* no input */
              TEST_FORMAT,
              NULL,
              Pa_GetDefaultOutputDeviceID(), /* default output device */
              2,          /* stereo output */
              TEST_FORMAT,
              NULL,
              SAMPLE_RATE,
              256,            /* frames per buffer */
              0,              /* number of buffers, if zero then use default minimum */
              paClipOff,      /* we won't output out of range samples so don't bother clipping them */
              patestCallback,
              &data );
    if( err != paNoError ) goto error;
    err = Pa_StartStream( stream );
    if( err != paNoError ) goto error;
    /* Watch until sound is halfway finished. */
    while( Pa_StreamTime( stream ) < (totalSamps/2) ) Pa_Sleep(10);
    /* Stop sound until ENTER hit. */
    err = Pa_StopStream( stream );
    if( err != paNoError ) goto error;
    printf("Pause for 2 seconds.\n");
    Pa_Sleep( 2000 );

    err = Pa_StartStream( stream );
    if( err != paNoError ) goto error;
    printf("Waiting for sound to finish.\n");
    while( Pa_StreamActive( stream ) ) Pa_Sleep(10);
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
