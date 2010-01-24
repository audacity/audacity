/*
 * $Id: paqa_devs.c,v 1.2 2003-03-02 08:01:40 dmazzoni Exp $
 * paqa_devs.c
 * Self Testing Quality Assurance app for PortAudio
 * Try to open each device and run through all the
 * possible configurations.
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
#include "pa_trace.h"
/****************************************** Definitions ***********/
#define MODE_INPUT      (0)
#define MODE_OUTPUT     (1)
typedef struct PaQaData
{
    unsigned long  framesLeft;
    int            numChannels;
    int            bytesPerSample;
    int            mode;
    short          sawPhase;
    PaSampleFormat format;
} PaQaData;

/****************************************** Prototypes ***********/
static void TestDevices( int mode );
static void TestFormats( int mode, PaDeviceID deviceID, double sampleRate,
                         int numChannels );
static int TestAdvance( int mode, PaDeviceID deviceID, double sampleRate,
                        int numChannels, PaSampleFormat format );
static int QaCallback( void *inputBuffer, void *outputBuffer,
                       unsigned long framesPerBuffer,
                       PaTimestamp outTime, void *userData );
                       
/****************************************** Globals ***********/
static int gNumPassed = 0;
static int gNumFailed = 0;

/****************************************** Macros ***********/
/* Print ERROR if it fails. Tally success or failure. */
/* Odd do-while wrapper seems to be needed for some compilers. */
#define EXPECT(_exp) \
    do \
    { \
        if ((_exp)) {\
            /* printf("SUCCESS for %s\n", #_exp ); */ \
            gNumPassed++; \
        } \
        else { \
            printf("ERROR - 0x%x - %s for %s\n", result, \
                   ((result == 0) ? "-" : Pa_GetErrorText(result)), \
                   #_exp ); \
            gNumFailed++; \
            goto error; \
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
    unsigned long  i;
    short          phase;
    PaQaData *data = (PaQaData *) userData;
    (void) inputBuffer;
    (void) outTime;

    /* Play simple sawtooth wave. */
    if( data->mode == MODE_OUTPUT )
    {
        phase = data->sawPhase;
        switch( data->format )
        {
        case paFloat32:
            {
                float *out =  (float *) outputBuffer;
                for( i=0; i<framesPerBuffer; i++ )
                {
                    phase += 0x123;
                    *out++ = (float) (phase * (1.0 / 32768.0));
                    if( data->numChannels == 2 )
                    {
                        *out++ = (float) (phase * (1.0 / 32768.0));
                    }
                }
            }
            break;

        case paInt32:
            {
                int *out =  (int *) outputBuffer;
                for( i=0; i<framesPerBuffer; i++ )
                {
                    phase += 0x123;
                    *out++ = ((int) phase ) << 16;
                    if( data->numChannels == 2 )
                    {
                        *out++ = ((int) phase ) << 16;
                    }
                }
            }
            break;
        case paInt16:
            {
                short *out =  (short *) outputBuffer;
                for( i=0; i<framesPerBuffer; i++ )
                {
                    phase += 0x123;
                    *out++ = phase;
                    if( data->numChannels == 2 )
                    {
                        *out++ = phase;
                    }
                }
            }
            break;

        default:
            {
                unsigned char *out =  (unsigned char *) outputBuffer;
                unsigned long numBytes = framesPerBuffer * data->numChannels * data->bytesPerSample;
                for( i=0; i<numBytes; i++ )
                {
                    *out++ = 0;
                }
            }
            break;
        }
        data->sawPhase = phase;
    }
    /* Are we through yet? */
    if( data->framesLeft > framesPerBuffer )
    {
        AddTraceMessage("QaCallback: running. framesLeft", data->framesLeft );
        data->framesLeft -= framesPerBuffer;
        return 0;
    }
    else
    {
        AddTraceMessage("QaCallback: DONE! framesLeft", data->framesLeft );
        data->framesLeft = 0;
        return 1;
    }
}
/*******************************************************************/
int main(void);
int main(void)
{
    PaError result;
    EXPECT( ((result=Pa_Initialize()) == 0) );
    printf("Test OUTPUT ---------------\n");
    TestDevices( MODE_OUTPUT );
    printf("Test INPUT ---------------\n");
    TestDevices( MODE_INPUT );
error:
    Pa_Terminate();
    printf("QA Report: %d passed, %d failed.\n", gNumPassed, gNumFailed );
}
/*******************************************************************
* Try each output device, through its full range of capabilities. */
static void TestDevices( int mode )
{
    int id,jc,kr;
    int maxChannels;
    const PaDeviceInfo *pdi;
    int numDevices  = Pa_CountDevices();
    /* Iterate through all devices. */
    for( id=0; id<numDevices; id++ )
    {
        pdi = Pa_GetDeviceInfo( id );
        /* Try 1 to maxChannels on each device. */
        maxChannels = ( mode == MODE_INPUT ) ? pdi->maxInputChannels : pdi->maxOutputChannels;
        for( jc=1; jc<=maxChannels; jc++ )
        {
            printf("Name         = %s\n", pdi->name );
            /* Try each legal sample rate. */
            if( pdi->numSampleRates == -1 )
            {
                double low, high;
                low = pdi->sampleRates[0];
                high = pdi->sampleRates[1];
                if( low < 8000.0 ) low = 8000.0;
                TestFormats( mode, id, low, jc );
#define TESTSR(sr) {if(((sr)>=low) && ((sr)<=high)) TestFormats( mode, id, (sr), jc ); }

                TESTSR(11025.0);
                TESTSR(22050.0);
                TESTSR(34567.0);
                TESTSR(44100.0);
                TestFormats( mode, id, high, jc );
            }
            else
            {
                for( kr=0; kr<pdi->numSampleRates; kr++ )
                {
                    TestFormats( mode, id, pdi->sampleRates[kr], jc );
                }
            }
        }
    }
}
/*******************************************************************/
static void TestFormats( int mode, PaDeviceID deviceID, double sampleRate,
                         int numChannels )
{
    TestAdvance( mode, deviceID, sampleRate, numChannels, paFloat32 ); /* */
    TestAdvance( mode, deviceID, sampleRate, numChannels, paInt16 ); /* */
    TestAdvance( mode, deviceID, sampleRate, numChannels, paInt32 ); /* */
}
/*******************************************************************/
static int TestAdvance( int mode, PaDeviceID deviceID, double sampleRate,
                        int numChannels, PaSampleFormat format )
{
    PortAudioStream *stream = NULL;
    PaError result;
    PaQaData myData;
#define FRAMES_PER_BUFFER  (64)
    printf("------ TestAdvance: %s, device = %d, rate = %g, numChannels = %d, format = %d -------\n",
           ( mode == MODE_INPUT ) ? "INPUT" : "OUTPUT",
           deviceID, sampleRate, numChannels, format);
    fflush(stdout);
    /* Setup data for synthesis thread. */
    myData.framesLeft = (unsigned long) (sampleRate * 100); /* 100 seconds */
    myData.numChannels = numChannels;
    myData.mode = mode;
    myData.format = format;
    switch( format )
    {
    case paFloat32:
    case paInt32:
    case paInt24:
        myData.bytesPerSample = 4;
        break;
    case paPackedInt24:
        myData.bytesPerSample = 3;
        break;
    default:
        myData.bytesPerSample = 2;
        break;
    }
    EXPECT( ((result = Pa_OpenStream(
                           &stream,
                           ( mode == MODE_INPUT ) ? deviceID : paNoDevice,
                           ( mode == MODE_INPUT ) ? numChannels : 0,
                           format,
                           NULL,
                           ( mode == MODE_OUTPUT ) ? deviceID : paNoDevice,
                           ( mode == MODE_OUTPUT ) ? numChannels : 0,
                           format,
                           NULL,
                           sampleRate,
                           FRAMES_PER_BUFFER,     /* frames per buffer */
                           0,             /* number of buffers, if zero then use default minimum */
                           paClipOff,     /* we won't output out of range samples so don't bother clipping them */
                           QaCallback,
                           &myData )
             ) == 0) );
    if( stream )
    {
        PaTimestamp oldStamp, newStamp;
        unsigned long oldFrames;
        int minDelay = ( mode == MODE_INPUT ) ? 1000 : 400;
        int minNumBuffers = Pa_GetMinNumBuffers( FRAMES_PER_BUFFER, sampleRate );
        int msec = (int) ((minNumBuffers * 3 * 1000.0 * FRAMES_PER_BUFFER) / sampleRate);
        if( msec < minDelay ) msec = minDelay;
        printf("msec = %d\n", msec);  /**/
        EXPECT( ((result=Pa_StartStream( stream )) == 0) );
        /* Check to make sure PortAudio is advancing timeStamp. */
        result = paNoError;
        oldStamp = Pa_StreamTime(stream);
        fflush(stdout);
        Pa_Sleep(msec);
        newStamp = Pa_StreamTime(stream);
        printf("oldStamp = %g,newStamp = %g\n", oldStamp, newStamp ); /**/
        EXPECT( (oldStamp < newStamp) );
        /* Check to make sure callback is decrementing framesLeft. */
        oldFrames = myData.framesLeft;
        Pa_Sleep(msec);
        printf("oldFrames = %d, myData.framesLeft = %d\n", oldFrames,  myData.framesLeft ); /**/
        EXPECT( (oldFrames > myData.framesLeft) );
        EXPECT( ((result=Pa_CloseStream( stream )) == 0) );
        stream = NULL;
    }
error:
    if( stream != NULL ) Pa_CloseStream( stream );
    fflush(stdout);
    return result;
}
