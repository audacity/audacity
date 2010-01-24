/*
 * $Id: patest_stop.c,v 1.2 2003-03-02 08:01:43 dmazzoni Exp $
 * patest_stop.c
 *
 * Test the three ways of stopping audio:
 *    calling Pa_StopStream(),
 *    calling Pa_AbortStream(),
 *    and returning a 1 from the callback function.
 *
 * A long latency is set up so that you can hear the difference.
 * Then a simple 8 note sequence is repeated twice.
 * The program will print what you should hear.
 *
 * Author: Phil Burk <philburk@softsynth.com>
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
#define OUTPUT_DEVICE       (Pa_GetDefaultOutputDeviceID())
#define SLEEP_DUR           (200)
#define SAMPLE_RATE         (44100)
#define FRAMES_PER_BUFFER   (256)
#define LATENCY_MSEC    (3000)
#define NUM_BUFFERS         ((LATENCY_MSEC * SAMPLE_RATE) / (FRAMES_PER_BUFFER * 1000))
#define FRAMES_PER_NOTE     (SAMPLE_RATE/2)
#define MAX_REPEATS         (2)
#define FUNDAMENTAL         (400.0f / SAMPLE_RATE)
#define NOTE_0              (FUNDAMENTAL * 1.0f / 1.0f)
#define NOTE_1              (FUNDAMENTAL * 5.0f / 4.0f)
#define NOTE_2              (FUNDAMENTAL * 4.0f / 3.0f)
#define NOTE_3              (FUNDAMENTAL * 3.0f / 2.0f)
#define NOTE_4              (FUNDAMENTAL * 2.0f / 1.0f)
#define MODE_FINISH    (0)
#define MODE_STOP      (1)
#define MODE_ABORT     (2)
#ifndef M_PI
#define M_PI  (3.14159265)
#endif
#define TABLE_SIZE   (400)
typedef struct
{
    float  waveform[TABLE_SIZE + 1]; // add one for guard point for interpolation
    float  phase_increment;
    float  phase;
    float *tune;
    int    notesPerTune;
    int    frameCounter;
    int    noteCounter;
    int    repeatCounter;
    PaTimestamp    outTime;
    int    stopMode;
    int    done;
}
paTestData;
/************* Prototypes *****************************/
int TestStopMode( paTestData *data );
float LookupWaveform( paTestData *data, float phase );
/******************************************************
 * Convert phase between 0.0 and 1.0 to waveform value 
 * using linear interpolation.
 */
float LookupWaveform( paTestData *data, float phase )
{
    float fIndex = phase*TABLE_SIZE;
    int   index = (int) fIndex;
    float fract = fIndex - index;
    float lo = data->waveform[index];
    float hi = data->waveform[index+1];
    float val = lo + fract*(hi-lo);
    return val;
}
/* This routine will be called by the PortAudio engine when audio is needed.
** It may called at interrupt level on some machines so don't do anything
** that could mess up the system like calling malloc() or free().
*/
static int patestCallback( void *inputBuffer, void *outputBuffer,
                           unsigned long framesPerBuffer,
                           PaTimestamp outTime, void *userData )
{
    paTestData *data = (paTestData*)userData;
    float *out = (float*)outputBuffer;
    float value;
    unsigned int i = 0;
    int finished = 0;
    (void) outTime; /* Prevent unused variable warnings. */
    (void) inputBuffer;

    data->outTime = outTime;
    if( !data->done )
    {
        for( i=0; i<framesPerBuffer; i++ )
        {
            /* Are we done with this note? */
            if( data->frameCounter >= FRAMES_PER_NOTE )
            {
                data->noteCounter += 1;
                data->frameCounter = 0;
                /* Are we done with this tune? */
                if( data->noteCounter >= data->notesPerTune )
                {
                    data->noteCounter = 0;
                    data->repeatCounter += 1;
                    /* Are we totally done? */
                    if( data->repeatCounter >= MAX_REPEATS )
                    {
                        data->done = 1;
                        if( data->stopMode == MODE_FINISH )
                        {
                            finished = 1;
                            break;
                        }
                    }
                }
                data->phase_increment = data->tune[data->noteCounter];
            }
            value = LookupWaveform(data, data->phase);
            *out++ = value;  /* left */
            *out++ = value;  /* right */
            data->phase += data->phase_increment;
            if( data->phase >= 1.0f ) data->phase -= 1.0f;

            data->frameCounter += 1;
        }
    }
    /* zero remainder of final buffer */
    for( ; i<framesPerBuffer; i++ )
    {
        *out++ = 0; /* left */
        *out++ = 0; /* right */
    }
    return finished;
}
/*******************************************************************/
int main(void);
int main(void)
{
    paTestData DATA;
    int i;
    float simpleTune[] = { NOTE_0, NOTE_1, NOTE_2, NOTE_3, NOTE_4, NOTE_3, NOTE_2, NOTE_1 };
    printf("PortAudio Test: play song and test stopping. ask for %d buffers\n", NUM_BUFFERS );
    /* initialise sinusoidal wavetable */
    for( i=0; i<TABLE_SIZE; i++ )
    {
        DATA.waveform[i] = (float) (
                               (0.2 * sin( ((double)i/(double)TABLE_SIZE) * M_PI * 2. )) +
                               (0.2 * sin( ((double)(3*i)/(double)TABLE_SIZE) * M_PI * 2. )) +
                               (0.1 * sin( ((double)(5*i)/(double)TABLE_SIZE) * M_PI * 2. ))
                           );
    }
    DATA.waveform[TABLE_SIZE] = DATA.waveform[0]; // set guard point
    DATA.tune = &simpleTune[0];
    DATA.notesPerTune = sizeof(simpleTune) / sizeof(float);
    printf("Test MODE_FINISH - callback returns 1.\n");
    printf("Should hear entire %d note tune repeated twice.\n", DATA.notesPerTune);
    DATA.stopMode = MODE_FINISH;
    if( TestStopMode( &DATA ) != paNoError )
    {
        printf("Test of MODE_FINISH failed!\n");
        goto error;
    }
    printf("Test MODE_STOP - stop when song is done.\n");
    printf("Should hear entire %d note tune repeated twice.\n", DATA.notesPerTune);
    DATA.stopMode = MODE_STOP;
    if( TestStopMode( &DATA ) != paNoError )
    {
        printf("Test of MODE_STOP failed!\n");
        goto error;
    }

    printf("Test MODE_ABORT - abort immediately.\n");
    printf("Should hear last repetition cut short by %d msec.\n", LATENCY_MSEC);
    DATA.stopMode = MODE_ABORT;
    if( TestStopMode( &DATA ) != paNoError )
    {
        printf("Test of MODE_ABORT failed!\n");
        goto error;
    }
    return 0;
error:
    return 1;
}

int TestStopMode( paTestData *data )
{
    PortAudioStream *stream;
    PaError err;
    data->done = 0;
    data->phase = 0.0;
    data->frameCounter = 0;
    data->noteCounter = 0;
    data->repeatCounter = 0;
    data->phase_increment = data->tune[data->noteCounter];
    err = Pa_Initialize();
    if( err != paNoError ) goto error;
    err = Pa_OpenStream(
              &stream,
              paNoDevice,/* default input device */
              0,              /* no input */
              paFloat32,  /* 32 bit floating point input */
              NULL,
              OUTPUT_DEVICE,
              2,          /* stereo output */
              paFloat32,      /* 32 bit floating point output */
              NULL,
              SAMPLE_RATE,
              FRAMES_PER_BUFFER,            /* frames per buffer */
              NUM_BUFFERS,    /* number of buffers, if zero then use default minimum */
              paClipOff,      /* we won't output out of range samples so don't bother clipping them */
              patestCallback,
              data );
    if( err != paNoError ) goto error;
    err = Pa_StartStream( stream );
    if( err != paNoError ) goto error;
    if( data->stopMode == MODE_FINISH )
    {
        while( Pa_StreamActive( stream ) )
        {
            /*printf("outTime = %g, note# = %d, repeat# = %d\n", data->outTime,
             data->noteCounter, data->repeatCounter  );
            fflush(stdout); /**/
            Pa_Sleep( SLEEP_DUR );
        }
    }
    else
    {
        while( data->repeatCounter < MAX_REPEATS )
        {
            /*printf("outTime = %g, note# = %d, repeat# = %d\n", data->outTime,
             data->noteCounter, data->repeatCounter  );
            fflush(stdout); /**/
            Pa_Sleep( SLEEP_DUR );
        }
    }
    if( data->stopMode == MODE_ABORT )
    {
        printf("Call Pa_AbortStream()\n");
        err = Pa_AbortStream( stream );
    }
    else
    {
        printf("Call Pa_StopStream()\n");
        err = Pa_StopStream( stream );
    }
    if( err != paNoError ) goto error;
    printf("Call Pa_CloseStream()\n"); fflush(stdout);
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
