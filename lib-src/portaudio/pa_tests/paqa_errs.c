/*
 * $Id: paqa_errs.c,v 1.2 2003-03-02 08:01:41 dmazzoni Exp $
 * paqa_devs.c
 * Self Testing Quality Assurance app for PortAudio
 * Do lots of bad things to test error reporting.
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
/****************************************** Definitions ***********/
#define MODE_INPUT      (0)
#define MODE_OUTPUT     (1)
#define FRAMES_PER_BUFFER       (64)
#define SAMPLE_RATE        (44100.0)
#define NUM_BUFFERS              (0)
typedef struct PaQaData
{
    unsigned long  framesLeft;
    int            numChannels;
    int            bytesPerSample;
    int            mode;
}
PaQaData;
/****************************************** Prototypes ***********/
static void TestDevices( int mode );
static void TestFormats( int mode, PaDeviceID deviceID, double sampleRate,
                         int numChannels );
static int TestBadOpens( void );
static int TestBadActions( void );
static int QaCallback( void *inputBuffer, void *outputBuffer,
                       unsigned long framesPerBuffer,
                       PaTimestamp outTime, void *userData );
/****************************************** Globals ***********/
static int gNumPassed = 0;
static int gNumFailed = 0;
/****************************************** Macros ***********/
/* Print ERROR if it fails. Tally success or failure. */
/* Odd do-while wrapper seems to be needed for some compilers. */
#define EXPECT( msg, _exp) \
    do \
    { \
        if ((_exp)) {\
            gNumPassed++; \
        } \
        else { \
            printf("\nERROR %s\n    - 0x%x - %s for %s\n", (msg), result, Pa_GetErrorText(result), #_exp ); \
            gNumFailed++; \
            goto error; \
        } \
    } while(0)
#define HOPEFOR( msg, _exp) \
    do \
    { \
        if ((_exp)) {\
            gNumPassed++; \
        } \
        else { \
            printf("\nERROR %s\n    - 0x%x - %s for %s\n", (msg), result, Pa_GetErrorText(result), #_exp ); \
            gNumFailed++; \
        } \
    } while(0)
/*******************************************************************/
/* This routine will be called by the PortAudio engine when audio is needed.
** It may be called at interrupt level on some machines so don't do anything
** that could mess up the system like calling malloc() or free().
*/
static int QaCallback( void *inputBuffer, void *outputBuffer,
                       unsigned long framesPerBuffer,
                       PaTimestamp outTime, void *userData )
{
    unsigned long i;
    unsigned char *out = (unsigned char *) outputBuffer;
    PaQaData *data = (PaQaData *) userData;
    (void) inputBuffer; /* Prevent "unused variable" warnings. */
    (void) outTime;

    /* Zero out buffer so we don't hear terrible noise. */
    if( data->mode == MODE_OUTPUT )
    {
        unsigned long numBytes = framesPerBuffer * data->numChannels * data->bytesPerSample;
        for( i=0; i<numBytes; i++ )
        {
            *out++ = 0;
        }
    }
    /* Are we through yet? */
    if( data->framesLeft > framesPerBuffer )
    {
        data->framesLeft -= framesPerBuffer;
        return 0;
    }
    else
    {
        data->framesLeft = 0;
        return 1;
    }
}
/*******************************************************************/
int main(void);
int main(void)
{
    PaError result;
    EXPECT( "init", ((result=Pa_Initialize()) == 0) );
    TestBadActions();
    TestBadOpens();
error:
    Pa_Terminate();
    printf("QA Report: %d passed, %d failed.\n", gNumPassed, gNumFailed );
    return 0;
}
/*******************************************************************/
static int TestBadOpens( void )
{
    PortAudioStream *stream = NULL;
    PaError result;
    PaQaData myData;
    /* Setup data for synthesis thread. */
    myData.framesLeft = (unsigned long) (SAMPLE_RATE * 100); /* 100 seconds */
    myData.numChannels = 1;
    myData.mode = MODE_OUTPUT;
    HOPEFOR( "No devices specified.",(
                 (result = Pa_OpenStream(
                               &stream,
                               paNoDevice, 0, paFloat32, NULL,
                               paNoDevice, 0, paFloat32, NULL,
                               SAMPLE_RATE, FRAMES_PER_BUFFER, NUM_BUFFERS,
                               paClipOff,
                               QaCallback,
                               &myData )
                 ) == paInvalidDeviceId) );
    HOPEFOR( "Out of range input device specified.",(
                 (result = Pa_OpenStream(
                               &stream,
                               Pa_CountDevices(), 0, paFloat32, NULL,
                               paNoDevice, 0, paFloat32, NULL,
                               SAMPLE_RATE, FRAMES_PER_BUFFER, NUM_BUFFERS,
                               paClipOff,
                               QaCallback,
                               &myData )
                 ) == paInvalidDeviceId) );

    HOPEFOR( "Out of range output device specified.",(
                 (result = Pa_OpenStream(
                               &stream,
                               paNoDevice, 0, paFloat32, NULL,
                               Pa_CountDevices(), 0, paFloat32, NULL,
                               SAMPLE_RATE, FRAMES_PER_BUFFER, NUM_BUFFERS,
                               paClipOff,
                               QaCallback,
                               &myData )
                 ) == paInvalidDeviceId) );
    HOPEFOR( "Zero input channels.",(
                 (result = Pa_OpenStream(
                               &stream,
                               Pa_GetDefaultInputDeviceID(), 0, paFloat32, NULL,
                               paNoDevice, 0, paFloat32, NULL,
                               SAMPLE_RATE, FRAMES_PER_BUFFER, NUM_BUFFERS,
                               paClipOff,
                               QaCallback,
                               &myData )
                 ) == paInvalidChannelCount) );
    HOPEFOR( "Zero output channels.",(
                 (result = Pa_OpenStream(
                               &stream,
                               paNoDevice, 0, paFloat32, NULL,
                               Pa_GetDefaultOutputDeviceID(), 0, paFloat32, NULL,
                               SAMPLE_RATE, FRAMES_PER_BUFFER, NUM_BUFFERS,
                               paClipOff,
                               QaCallback,
                               &myData )
                 ) == paInvalidChannelCount) );
    HOPEFOR( "Nonzero input channels but no device.",(
                 (result = Pa_OpenStream(
                               &stream,
                               Pa_GetDefaultInputDeviceID(), 2, paFloat32, NULL,
                               paNoDevice, 2, paFloat32, NULL,
                               SAMPLE_RATE, FRAMES_PER_BUFFER, NUM_BUFFERS,
                               paClipOff,
                               QaCallback,
                               &myData )
                 ) == paInvalidChannelCount) );

    HOPEFOR( "Nonzero output channels but no device.",(
                 (result = Pa_OpenStream(
                               &stream,
                               paNoDevice, 2, paFloat32, NULL,
                               Pa_GetDefaultOutputDeviceID(), 2, paFloat32, NULL,
                               SAMPLE_RATE, FRAMES_PER_BUFFER, NUM_BUFFERS,
                               paClipOff,
                               QaCallback,
                               &myData )
                 ) == paInvalidChannelCount) );
    HOPEFOR( "NULL stream pointer.",(
                 (result = Pa_OpenStream(
                               NULL,
                               paNoDevice, 0, paFloat32, NULL,
                               Pa_GetDefaultOutputDeviceID(), 2, paFloat32, NULL,
                               SAMPLE_RATE, FRAMES_PER_BUFFER, NUM_BUFFERS,
                               paClipOff,
                               QaCallback,
                               &myData )
                 ) == paBadStreamPtr) );
    HOPEFOR( "Low sample rate.",(
                 (result = Pa_OpenStream(
                               &stream,
                               paNoDevice, 0, paFloat32, NULL,
                               Pa_GetDefaultOutputDeviceID(), 2, paFloat32, NULL,
                               1.0, FRAMES_PER_BUFFER, NUM_BUFFERS,
                               paClipOff,
                               QaCallback,
                               &myData )
                 ) == paInvalidSampleRate) );
    HOPEFOR( "High sample rate.",(
                 (result = Pa_OpenStream(
                               &stream,
                               paNoDevice, 0, paFloat32, NULL,
                               Pa_GetDefaultOutputDeviceID(), 2, paFloat32, NULL,
                               10000000.0, FRAMES_PER_BUFFER, NUM_BUFFERS,
                               paClipOff,
                               QaCallback,
                               &myData )
                 ) == paInvalidSampleRate) );
    HOPEFOR( "NULL callback.",(
                 (result = Pa_OpenStream(
                               &stream,
                               paNoDevice, 0, paFloat32, NULL,
                               Pa_GetDefaultOutputDeviceID(), 2, paFloat32, NULL,
                               SAMPLE_RATE, FRAMES_PER_BUFFER, NUM_BUFFERS,
                               paClipOff,
                               NULL,
                               &myData )
                 ) == paNullCallback) );
    HOPEFOR( "Bad flag.",(
                 (result = Pa_OpenStream(
                               &stream,
                               paNoDevice, 0, paFloat32, NULL,
                               Pa_GetDefaultOutputDeviceID(), 2, paFloat32, NULL,
                               SAMPLE_RATE, FRAMES_PER_BUFFER, NUM_BUFFERS,
                               (1<<3),
                               QaCallback,
                               &myData )
                 ) == paInvalidFlag) );

#if 1 /* FIXME - this is legal for some implementations. */
    HOPEFOR( "Use input device as output device.",(
                 (result = Pa_OpenStream(
                               &stream,
                               paNoDevice, 0, paFloat32, NULL,
                               Pa_GetDefaultInputDeviceID(), 2, paFloat32, NULL,
                               SAMPLE_RATE, FRAMES_PER_BUFFER, NUM_BUFFERS,
                               paClipOff,
                               QaCallback,
                               &myData )
                 ) == paInvalidDeviceId) );

    HOPEFOR( "Use output device as input device.",(
                 (result = Pa_OpenStream(
                               &stream,
                               Pa_GetDefaultOutputDeviceID(), 2, paFloat32, NULL,
                               paNoDevice, 0, paFloat32, NULL,
                               SAMPLE_RATE, FRAMES_PER_BUFFER, NUM_BUFFERS,
                               paClipOff,
                               QaCallback,
                               &myData )
                 ) == paInvalidDeviceId) );
#endif

    if( stream != NULL ) Pa_CloseStream( stream );
    return result;
}
/*******************************************************************/
static int TestBadActions( void )
{
    PortAudioStream *stream = NULL;
    PaError result;
    PaQaData myData;
    /* Setup data for synthesis thread. */
    myData.framesLeft = (unsigned long) (SAMPLE_RATE * 100); /* 100 seconds */
    myData.numChannels = 1;
    myData.mode = MODE_OUTPUT;
    /* Default output. */
    EXPECT( "TestBadActions", ((result = Pa_OpenStream(
                           &stream,
                           paNoDevice, 0, paFloat32, NULL,
                           Pa_GetDefaultOutputDeviceID(), 2, paFloat32, NULL,
                           SAMPLE_RATE, FRAMES_PER_BUFFER, NUM_BUFFERS,
                           paClipOff,
                           QaCallback,
                           &myData )
             ) == 0) );
    HOPEFOR( "start", ((result = Pa_StartStream( NULL )) == paBadStreamPtr) );
    HOPEFOR( "stop", ((result = Pa_StopStream( NULL )) == paBadStreamPtr) );
    HOPEFOR( "active?", ((result = Pa_StreamActive( NULL )) == paBadStreamPtr) );
    HOPEFOR( "close", ((result = Pa_CloseStream( NULL )) == paBadStreamPtr) );
    HOPEFOR( "time?", ((result = (PaError)Pa_StreamTime( NULL )) != 0) );
    HOPEFOR( "CPULoad?", ((result = (PaError)Pa_GetCPULoad( NULL )) != 0) );
error:
    if( stream != NULL ) Pa_CloseStream( stream );
    return result;
}
