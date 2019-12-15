/** @file patest_write_stop_threads.c
	@brief Call Pa_StopStream() from another thread to see if PortAudio hangs.
	@author Bjorn Roche of XO Audio (www.xoaudio.com)
	@author Ross Bencina
	@author Phil Burk
*/
/*
 * $Id$
 *
 * This program uses the PortAudio Portable Audio Library.
 * For more information see: http://www.portaudio.com/
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
#include <unistd.h>
#include <math.h>
#include <memory.h>
/* pthread may only be available on Mac and Linux. */
#include <pthread.h>
#include "portaudio.h"

#define SAMPLE_RATE         (44100)
#define FRAMES_PER_BUFFER   (2048)

static float s_buffer[FRAMES_PER_BUFFER][2]; /* stereo output buffer */

/**
 * WARNING: PortAudio is NOT thread safe. DO NOT call PortAudio
 * from multiple threads without synchronization. This test uses
 * PA in an ILLEGAL WAY in order to try to flush out potential hang bugs.
 * The test calls Pa_WriteStream() and Pa_StopStream() simultaneously
 * from separate threads in order to try to cause Pa_StopStream() to hang.
 * In the main thread we write to the stream in a loop.
 * Then try stopping PA from another thread to see if it hangs.
 *
 * @note: Do not expect this test to pass. The test is only here
 * as a debugging aid for hang bugs. Since this test uses PA in an
 * illegal way, it may fail for reasons that are not PA bugs.
 */

/* Wait for awhile then abort the stream. */
void *stop_thread_proc(void *arg)
{
    PaStream *stream = (PaStream *)arg;
    PaTime time;
    for (int i = 0; i < 20; i++)
    {
        /* ILLEGAL unsynchronised call to PA, see comment above */
        time = Pa_GetStreamTime( stream );
        printf("Stream time = %f\n", time);
        fflush(stdout);
        usleep(100 * 1000);
    }
    printf("Call Pa_StopStream()\n");
    fflush(stdout);
    /* ILLEGAL unsynchronised call to PA, see comment above */
    PaError err = Pa_StopStream( stream );
    printf("Pa_StopStream() returned %d\n", err);
    fflush(stdout);

    return stream;
}

int main(void);
int main(void)
{
    PaStreamParameters outputParameters;
    PaStream *stream;
    PaError err;
    int result;
    pthread_t thread;

    printf( "PortAudio Test: output silence and stop from another thread. SR = %d, BufSize = %d\n",
            SAMPLE_RATE, FRAMES_PER_BUFFER);

    err = Pa_Initialize();
    if( err != paNoError ) goto error;

    outputParameters.device = Pa_GetDefaultOutputDevice(); /* default output device */
    outputParameters.channelCount = 2;       /* stereo output */
    outputParameters.sampleFormat = paFloat32; /* 32 bit floating point output */
    outputParameters.suggestedLatency = Pa_GetDeviceInfo( outputParameters.device )->defaultHighOutputLatency * 5;
    outputParameters.hostApiSpecificStreamInfo = NULL;

    /* open the stream */
    err = Pa_OpenStream(
              &stream,
              NULL, /* no input */
              &outputParameters,
              SAMPLE_RATE,
              FRAMES_PER_BUFFER,
              paClipOff,      /* we won't output out of range samples so don't bother clipping them */
              NULL, /* no callback, use blocking API */
              NULL ); /* no callback, so no callback userData */
    if( err != paNoError ) goto error;

    result = pthread_create(&thread, NULL /* attributes */, stop_thread_proc, stream);

    /* start the stream */
    err = Pa_StartStream( stream );
    if( err != paNoError ) goto error;

    /* clear buffer */
    memset( s_buffer, 0, sizeof(s_buffer) );

    /* play the silent buffer many times */
    while( Pa_IsStreamActive(stream) > 0 )
    {
        err = Pa_WriteStream( stream, s_buffer, FRAMES_PER_BUFFER );
        printf("Pa_WriteStream returns %d = %s\n", err, Pa_GetErrorText( err ));
        if( err != paNoError )
        {
            err = paNoError;
            break;
        };
    }

    printf("Try to join the thread that called Pa_StopStream().\n");
    result = pthread_join( thread, NULL );
    printf("pthread_join returned %d\n", result);

    /* close, and terminate */
    printf("Call Pa_CloseStream\n");
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
