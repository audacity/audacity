/** @file paex_record_file.c
	@ingroup examples_src
	@brief Record input into a file, then playback recorded data from file (Windows only at the moment) 
	@author Robert Bielik
*/
/*
 * $Id: paex_record_file.c 1752 2011-09-08 03:21:55Z philburk $
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
#include <stdlib.h>
#include "portaudio.h"
#include "pa_ringbuffer.h"
#include "pa_util.h"

#ifdef _WIN32
#include <windows.h>
#include <process.h>
#endif

/* #define SAMPLE_RATE  (17932) // Test failure to open with this value. */
#define FILE_NAME       "audio_data.raw"
#define SAMPLE_RATE  (44100)
#define FRAMES_PER_BUFFER (512)
#define NUM_SECONDS     (10)
#define NUM_CHANNELS    (2)
#define NUM_WRITES_PER_BUFFER   (4)
/* #define DITHER_FLAG     (paDitherOff) */
#define DITHER_FLAG     (0) /**/


/* Select sample format. */
#if 1
#define PA_SAMPLE_TYPE  paFloat32
typedef float SAMPLE;
#define SAMPLE_SILENCE  (0.0f)
#define PRINTF_S_FORMAT "%.8f"
#elif 1
#define PA_SAMPLE_TYPE  paInt16
typedef short SAMPLE;
#define SAMPLE_SILENCE  (0)
#define PRINTF_S_FORMAT "%d"
#elif 0
#define PA_SAMPLE_TYPE  paInt8
typedef char SAMPLE;
#define SAMPLE_SILENCE  (0)
#define PRINTF_S_FORMAT "%d"
#else
#define PA_SAMPLE_TYPE  paUInt8
typedef unsigned char SAMPLE;
#define SAMPLE_SILENCE  (128)
#define PRINTF_S_FORMAT "%d"
#endif

typedef struct
{
    unsigned            frameIndex;
    int                 threadSyncFlag;
    SAMPLE             *ringBufferData;
    PaUtilRingBuffer    ringBuffer;
    FILE               *file;
    void               *threadHandle;
}
paTestData;

/* This routine is run in a separate thread to write data from the ring buffer into a file (during Recording) */
static int threadFunctionWriteToRawFile(void* ptr)
{
    paTestData* pData = (paTestData*)ptr;

    /* Mark thread started */
    pData->threadSyncFlag = 0;

    while (1)
    {
        ring_buffer_size_t elementsInBuffer = PaUtil_GetRingBufferReadAvailable(&pData->ringBuffer);
        if ( (elementsInBuffer >= pData->ringBuffer.bufferSize / NUM_WRITES_PER_BUFFER) ||
             pData->threadSyncFlag )
        {
            void* ptr[2] = {0};
            ring_buffer_size_t sizes[2] = {0};

            /* By using PaUtil_GetRingBufferReadRegions, we can read directly from the ring buffer */
            ring_buffer_size_t elementsRead = PaUtil_GetRingBufferReadRegions(&pData->ringBuffer, elementsInBuffer, ptr + 0, sizes + 0, ptr + 1, sizes + 1);
            if (elementsRead > 0)
            {
                int i;
                for (i = 0; i < 2 && ptr[i] != NULL; ++i)
                {
                    fwrite(ptr[i], pData->ringBuffer.elementSizeBytes, sizes[i], pData->file);
                }
                PaUtil_AdvanceRingBufferReadIndex(&pData->ringBuffer, elementsRead);
            }

            if (pData->threadSyncFlag)
            {
                break;
            }
        }

        /* Sleep a little while... */
        Pa_Sleep(20);
    }

    pData->threadSyncFlag = 0;

    return 0;
}

/* This routine is run in a separate thread to read data from file into the ring buffer (during Playback). When the file
   has reached EOF, a flag is set so that the play PA callback can return paComplete */
static int threadFunctionReadFromRawFile(void* ptr)
{
    paTestData* pData = (paTestData*)ptr;

    while (1)
    {
        ring_buffer_size_t elementsInBuffer = PaUtil_GetRingBufferWriteAvailable(&pData->ringBuffer);

        if (elementsInBuffer >= pData->ringBuffer.bufferSize / NUM_WRITES_PER_BUFFER)
        {
            void* ptr[2] = {0};
            ring_buffer_size_t sizes[2] = {0};

            /* By using PaUtil_GetRingBufferWriteRegions, we can write directly into the ring buffer */
            PaUtil_GetRingBufferWriteRegions(&pData->ringBuffer, elementsInBuffer, ptr + 0, sizes + 0, ptr + 1, sizes + 1);

            if (!feof(pData->file))
            {
                ring_buffer_size_t itemsReadFromFile = 0;
                int i;
                for (i = 0; i < 2 && ptr[i] != NULL; ++i)
                {
                    itemsReadFromFile += (ring_buffer_size_t)fread(ptr[i], pData->ringBuffer.elementSizeBytes, sizes[i], pData->file);
                }
                PaUtil_AdvanceRingBufferWriteIndex(&pData->ringBuffer, itemsReadFromFile);

                /* Mark thread started here, that way we "prime" the ring buffer before playback */
                pData->threadSyncFlag = 0;
            }
            else
            {
                /* No more data to read */
                pData->threadSyncFlag = 1;
                break;
            }
        }

        /* Sleep a little while... */
        Pa_Sleep(20);
    }

    return 0;
}

typedef int (*ThreadFunctionType)(void*);

/* Start up a new thread in the given function, at the moment only Windows, but should be very easy to extend
   to posix type OSs (Linux/Mac) */
static PaError startThread( paTestData* pData, ThreadFunctionType fn )
{
#ifdef _WIN32
    typedef unsigned (__stdcall* WinThreadFunctionType)(void*);
    pData->threadHandle = (void*)_beginthreadex(NULL, 0, (WinThreadFunctionType)fn, pData, CREATE_SUSPENDED, NULL);
    if (pData->threadHandle == NULL) return paUnanticipatedHostError;

    /* Set file thread to a little higher prio than normal */
    SetThreadPriority(pData->threadHandle, THREAD_PRIORITY_ABOVE_NORMAL);

    /* Start it up */
    pData->threadSyncFlag = 1;
    ResumeThread(pData->threadHandle);

#endif

    /* Wait for thread to startup */
    while (pData->threadSyncFlag) {
        Pa_Sleep(10);
    }

    return paNoError;
}

static int stopThread( paTestData* pData )
{
    pData->threadSyncFlag = 1;
    /* Wait for thread to stop */
    while (pData->threadSyncFlag) {
        Pa_Sleep(10);
    }
#ifdef _WIN32
    CloseHandle(pData->threadHandle);
    pData->threadHandle = 0;
#endif

    return paNoError;
}


/* This routine will be called by the PortAudio engine when audio is needed.
** It may be called at interrupt level on some machines so don't do anything
** that could mess up the system like calling malloc() or free().
*/
static int recordCallback( const void *inputBuffer, void *outputBuffer,
                           unsigned long framesPerBuffer,
                           const PaStreamCallbackTimeInfo* timeInfo,
                           PaStreamCallbackFlags statusFlags,
                           void *userData )
{
    paTestData *data = (paTestData*)userData;
    ring_buffer_size_t elementsWriteable = PaUtil_GetRingBufferWriteAvailable(&data->ringBuffer);
    ring_buffer_size_t elementsToWrite = min(elementsWriteable, (ring_buffer_size_t)(framesPerBuffer * NUM_CHANNELS));
    const SAMPLE *rptr = (const SAMPLE*)inputBuffer;

    (void) outputBuffer; /* Prevent unused variable warnings. */
    (void) timeInfo;
    (void) statusFlags;
    (void) userData;

    data->frameIndex += PaUtil_WriteRingBuffer(&data->ringBuffer, rptr, elementsToWrite);

    return paContinue;
}

/* This routine will be called by the PortAudio engine when audio is needed.
** It may be called at interrupt level on some machines so don't do anything
** that could mess up the system like calling malloc() or free().
*/
static int playCallback( const void *inputBuffer, void *outputBuffer,
                         unsigned long framesPerBuffer,
                         const PaStreamCallbackTimeInfo* timeInfo,
                         PaStreamCallbackFlags statusFlags,
                         void *userData )
{
    paTestData *data = (paTestData*)userData;
    ring_buffer_size_t elementsToPlay = PaUtil_GetRingBufferReadAvailable(&data->ringBuffer);
    ring_buffer_size_t elementsToRead = min(elementsToPlay, (ring_buffer_size_t)(framesPerBuffer * NUM_CHANNELS));
    SAMPLE* wptr = (SAMPLE*)outputBuffer;

    (void) inputBuffer; /* Prevent unused variable warnings. */
    (void) timeInfo;
    (void) statusFlags;
    (void) userData;

    data->frameIndex += PaUtil_ReadRingBuffer(&data->ringBuffer, wptr, elementsToRead);

    return data->threadSyncFlag ? paComplete : paContinue;
}

static unsigned NextPowerOf2(unsigned val)
{
    val--;
    val = (val >> 1) | val;
    val = (val >> 2) | val;
    val = (val >> 4) | val;
    val = (val >> 8) | val;
    val = (val >> 16) | val;
    return ++val;
}

/*******************************************************************/
int main(void);
int main(void)
{
    PaStreamParameters  inputParameters,
                        outputParameters;
    PaStream*           stream;
    PaError             err = paNoError;
    paTestData          data = {0};
    unsigned            delayCntr;
    unsigned            numSamples;
    unsigned            numBytes;

    printf("patest_record.c\n"); fflush(stdout);

    /* We set the ring buffer size to about 500 ms */
    numSamples = NextPowerOf2((unsigned)(SAMPLE_RATE * 0.5 * NUM_CHANNELS));
    numBytes = numSamples * sizeof(SAMPLE);
    data.ringBufferData = (SAMPLE *) PaUtil_AllocateMemory( numBytes );
    if( data.ringBufferData == NULL )
    {
        printf("Could not allocate ring buffer data.\n");
        goto done;
    }

    if (PaUtil_InitializeRingBuffer(&data.ringBuffer, sizeof(SAMPLE), numSamples, data.ringBufferData) < 0)
    {
        printf("Failed to initialize ring buffer. Size is not power of 2 ??\n");
        goto done;
    }

    err = Pa_Initialize();
    if( err != paNoError ) goto done;

    inputParameters.device = Pa_GetDefaultInputDevice(); /* default input device */
    if (inputParameters.device == paNoDevice) {
        fprintf(stderr,"Error: No default input device.\n");
        goto done;
    }
    inputParameters.channelCount = 2;                    /* stereo input */
    inputParameters.sampleFormat = PA_SAMPLE_TYPE;
    inputParameters.suggestedLatency = Pa_GetDeviceInfo( inputParameters.device )->defaultLowInputLatency;
    inputParameters.hostApiSpecificStreamInfo = NULL;

    /* Record some audio. -------------------------------------------- */
    err = Pa_OpenStream(
              &stream,
              &inputParameters,
              NULL,                  /* &outputParameters, */
              SAMPLE_RATE,
              FRAMES_PER_BUFFER,
              paClipOff,      /* we won't output out of range samples so don't bother clipping them */
              recordCallback,
              &data );
    if( err != paNoError ) goto done;

    /* Open the raw audio 'cache' file... */
    data.file = fopen(FILE_NAME, "wb");
    if (data.file == 0) goto done;

    /* Start the file writing thread */
    err = startThread(&data, threadFunctionWriteToRawFile);
    if( err != paNoError ) goto done;

    err = Pa_StartStream( stream );
    if( err != paNoError ) goto done;
    printf("\n=== Now recording to '" FILE_NAME "' for %d seconds!! Please speak into the microphone. ===\n", NUM_SECONDS); fflush(stdout);

    /* Note that the RECORDING part is limited with TIME, not size of the file and/or buffer, so you can
       increase NUM_SECONDS until you run out of disk */
    delayCntr = 0;
    while( delayCntr++ < NUM_SECONDS )
    {
        printf("index = %d\n", data.frameIndex ); fflush(stdout);
        Pa_Sleep(1000);
    }
    if( err < 0 ) goto done;

    err = Pa_CloseStream( stream );
    if( err != paNoError ) goto done;

    /* Stop the thread */
    err = stopThread(&data);
    if( err != paNoError ) goto done;

    /* Close file */
    fclose(data.file);
    data.file = 0;

    /* Playback recorded data.  -------------------------------------------- */
    data.frameIndex = 0;

    outputParameters.device = Pa_GetDefaultOutputDevice(); /* default output device */
    if (outputParameters.device == paNoDevice) {
        fprintf(stderr,"Error: No default output device.\n");
        goto done;
    }
    outputParameters.channelCount = 2;                     /* stereo output */
    outputParameters.sampleFormat =  PA_SAMPLE_TYPE;
    outputParameters.suggestedLatency = Pa_GetDeviceInfo( outputParameters.device )->defaultLowOutputLatency;
    outputParameters.hostApiSpecificStreamInfo = NULL;

    printf("\n=== Now playing back from file '" FILE_NAME "' until end-of-file is reached ===\n"); fflush(stdout);
    err = Pa_OpenStream(
              &stream,
              NULL, /* no input */
              &outputParameters,
              SAMPLE_RATE,
              FRAMES_PER_BUFFER,
              paClipOff,      /* we won't output out of range samples so don't bother clipping them */
              playCallback,
              &data );
    if( err != paNoError ) goto done;

    if( stream )
    {
        /* Open file again for reading */
        data.file = fopen(FILE_NAME, "rb");
        if (data.file != 0)
        {
            /* Start the file reading thread */
            err = startThread(&data, threadFunctionReadFromRawFile);
            if( err != paNoError ) goto done;

            err = Pa_StartStream( stream );
            if( err != paNoError ) goto done;

            printf("Waiting for playback to finish.\n"); fflush(stdout);

            /* The playback will end when EOF is reached */
            while( ( err = Pa_IsStreamActive( stream ) ) == 1 ) {
                printf("index = %d\n", data.frameIndex ); fflush(stdout);
                Pa_Sleep(1000);
            }
            if( err < 0 ) goto done;
        }
        
        err = Pa_CloseStream( stream );
        if( err != paNoError ) goto done;

        fclose(data.file);
        
        printf("Done.\n"); fflush(stdout);
    }

done:
    Pa_Terminate();
    if( data.ringBufferData )       /* Sure it is NULL or valid. */
        PaUtil_FreeMemory( data.ringBufferData );
    if( err != paNoError )
    {
        fprintf( stderr, "An error occured while using the portaudio stream\n" );
        fprintf( stderr, "Error number: %d\n", err );
        fprintf( stderr, "Error message: %s\n", Pa_GetErrorText( err ) );
        err = 1;          /* Always return 0 or 1, but no other return codes. */
    }
    return err;
}

