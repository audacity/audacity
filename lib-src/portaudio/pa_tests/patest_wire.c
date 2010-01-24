/*
 * $Id: patest_wire.c,v 1.3 2003-03-02 08:01:43 dmazzoni Exp $
 * patest_wire.c
 *
 * Pass input directly to output.
 * Note that some HW devices, for example many ISA audio cards
 * on PCs, do NOT support full duplex! For a PC, you normally need
 * a PCI based audio card such as the SBLive.
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

#define INPUT_DEVICE  Pa_GetDefaultInputDeviceID()
#define OUTPUT_DEVICE Pa_GetDefaultOutputDeviceID()

/*
** Note that many of the older ISA sound cards on PCs do NOT support
** full duplex audio (simultaneous record and playback).
** And some only support full duplex at lower sample rates.
*/
#define SAMPLE_RATE  (22050)
#define FRAMES_PER_BUFFER  (64)

#if 1
#define PA_SAMPLE_TYPE  paFloat32
typedef float SAMPLE;
#else
#define PA_SAMPLE_TYPE  paInt16
typedef short SAMPLE;
#endif
static int wireCallback( void *inputBuffer, void *outputBuffer,
                         unsigned long framesPerBuffer,
                         PaTimestamp outTime, void *userData );

/* This routine will be called by the PortAudio engine when audio is needed.
** It may be called at interrupt level on some machines so don't do anything
** that could mess up the system like calling malloc() or free().
*/
static int wireCallback( void *inputBuffer, void *outputBuffer,
                         unsigned long framesPerBuffer,
                         PaTimestamp outTime, void *userData )
{
    SAMPLE *out = (SAMPLE*)outputBuffer;
    SAMPLE *in = (SAMPLE*)inputBuffer;
    unsigned int i;
    (void) outTime;

    /* This may get called with NULL inputBuffer during initial setup. */
    if( inputBuffer == NULL )
    {
        for( i=0; i<framesPerBuffer; i++ )
        {
            *out++ = 0;  /* left */
            *out++ = 0;  /* right */
        }
    }
    else
    {
        for( i=0; i<framesPerBuffer; i++ )
        {
            *out++ = *in++;  /* left */
            *out++ = *in++;  /* right */
        }
    }

    return 0;
}


/*******************************************************************/
int main(void);
int main(void)
{
    PortAudioStream *stream;
    PaError err;
    
    err = Pa_Initialize();
    if( err != paNoError ) goto error;
    
    printf("PortAudio Test: input device ID  = %d\n", INPUT_DEVICE );
    printf("PortAudio Test: output device ID = %d\n", OUTPUT_DEVICE );
    err = Pa_OpenStream(
              &stream,
              INPUT_DEVICE,
              2,               /* stereo input */
              PA_SAMPLE_TYPE,
              NULL,
              OUTPUT_DEVICE,
              2,               /* stereo output */
              PA_SAMPLE_TYPE,
              NULL,
              SAMPLE_RATE,
              FRAMES_PER_BUFFER,            /* frames per buffer */
              0,               /* number of buffers, if zero then use default minimum */
              paClipOff,       /* we won't output out of range samples so don't bother clipping them */
              wireCallback,
              NULL );          /* no data */
    if( err != paNoError ) goto error;
    
    err = Pa_StartStream( stream );
    if( err != paNoError ) goto error;
    
    printf("Full duplex sound test in progress.\n");
    printf("Hit ENTER to exit test.\n");  fflush(stdout);
    getchar();
    
    printf("Closing stream.\n");
    err = Pa_CloseStream( stream );
    if( err != paNoError ) goto error;
    Pa_Terminate();
    
    printf("Full duplex sound test complete.\n"); fflush(stdout);
    return 0;
error:
    Pa_Terminate();
    fprintf( stderr, "An error occured while using the portaudio stream\n" );
    fprintf( stderr, "Error number: %d\n", err );
    fprintf( stderr, "Error message: %s\n", Pa_GetErrorText( err ) );
    return -1;
}
