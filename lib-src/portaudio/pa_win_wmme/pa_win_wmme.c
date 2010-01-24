/*
 * $Id: pa_win_wmme.c,v 1.11 2004-09-15 13:55:04 jamescrook Exp $
 * pa_win_wmme.c
 * Implementation of PortAudio for Windows MultiMedia Extensions (WMME)
 *
 * PortAudio Portable Real-Time Audio Library
 * Latest Version at: http://www.portaudio.com
 *
 * Authors: Ross Bencina and Phil Burk
 * Copyright (c) 1999-2000 Ross Bencina and Phil Burk
 *
 * Permission is hereby granted, free of charge, to any person obtainingF
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
/*
  All memory allocations and frees are marked with MEM for quick review.
*/

/* Modification History:
 PLB = Phil Burk
 JM = Julien Maillard
 RDB = Ross Bencina
 PLB20010402 - sDevicePtrs now allocates based on sizeof(pointer)
 PLB20010413 - check for excessive numbers of channels
 PLB20010422 - apply Mike Berry's changes for CodeWarrior on PC
               including condition including of memory.h,
               and explicit typecasting on memory allocation
 PLB20010802 - use GlobalAlloc for sDevicesPtr instead of PaHost_AllocFastMemory
 PLB20010816 - pass process instead of thread to SetPriorityClass()
 PLB20010927 - use number of frames instead of real-time for CPULoad calculation.
 JM20020118 - prevent hung thread when buffers underflow.
 PLB20020321 - detect Win XP versus NT, 9x; fix DBUG typo; removed init of CurrentCount
 RDB20020411 - various renaming cleanups, factored streamData alloc and cpu usage init
 RDB20020417 - stopped counting WAVE_MAPPER when there were no real devices
               refactoring, renaming and fixed a few edge case bugs
 PLB20020612 - added 8000.0 Hz to custom sampling rates array
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <windows.h>
#include <mmsystem.h>
#include <process.h>
/* PLB20010422 - "memory.h" doesn't work on CodeWarrior for PC. Thanks Mike Berry for the mod. */
#ifndef __MWERKS__
#include <malloc.h>
#include <memory.h>
#endif /* __MWERKS__ */
#include "portaudio.h"
#include "pa_host.h"
#include "pa_trace.h"

/************************************************* Constants ********/
#define PA_TRACK_MEMORY          (0)

#define PA_USE_TIMER_CALLBACK    (0)  /* Select between two options for background task. 0=thread, 1=timer */
/* Switches for debugging. */
#define PA_SIMULATE_UNDERFLOW    (0)  /* Set to one to force an underflow of the output buffer. */

/* To trace program, enable TRACE_REALTIME_EVENTS in pa_trace.h */
#define PA_TRACE_RUN             (0)
#define PA_TRACE_START_STOP      (1)

#define PA_USE_HIGH_LATENCY      (0)  /* For debugging glitches. */

#if PA_USE_HIGH_LATENCY
 #define PA_MIN_MSEC_PER_HOST_BUFFER  (100)
 #define PA_MAX_MSEC_PER_HOST_BUFFER  (300) /* Do not exceed unless user buffer exceeds */
 #define PA_MIN_NUM_HOST_BUFFERS      (4)
 #define PA_MAX_NUM_HOST_BUFFERS      (16)  /* OK to exceed if necessary */
 #define PA_WIN_9X_LATENCY            (400)
#else
 #define PA_MIN_MSEC_PER_HOST_BUFFER  (10)
 #define PA_MAX_MSEC_PER_HOST_BUFFER  (100) /* Do not exceed unless user buffer exceeds */
 #define PA_MIN_NUM_HOST_BUFFERS      (3)
 #define PA_MAX_NUM_HOST_BUFFERS      (16)  /* OK to exceed if necessary */
 #define PA_WIN_9X_LATENCY            (200)
#endif
#define MIN_TIMEOUT_MSEC                 (1000)

/*
** Use higher latency for NT because it is even worse at real-time
** operation than Win9x.
*/
#define PA_WIN_NT_LATENCY        (PA_WIN_9X_LATENCY * 2)
#define PA_WIN_WDM_LATENCY       (PA_WIN_9X_LATENCY)

#if PA_SIMULATE_UNDERFLOW
static  gUnderCallbackCounter = 0;
#define UNDER_SLEEP_AT       (40)
#define UNDER_SLEEP_FOR      (500)
#endif

#define PRINT(x) { printf x; fflush(stdout); }
#define ERR_RPT(x) PRINT(x)
#define DBUG(x)  /* PRINT(x) /**/
#define DBUGX(x) /* PRINT(x) */
/************************************************* Definitions ********/
/**************************************************************
 * Structure for internal host specific stream data.
 * This is allocated on a per stream basis.
 */
typedef struct PaWMMEStreamData
{
    /* Input -------------- */
    HWAVEIN            hWaveIn;
    WAVEHDR           *inputBuffers;
    int                currentInputBuffer;
    int                bytesPerHostInputBuffer;
    int                bytesPerUserInputBuffer;    /* native buffer size in bytes */
    /* Output -------------- */
    HWAVEOUT           hWaveOut;
    WAVEHDR           *outputBuffers;
    int                currentOutputBuffer;
    int                bytesPerHostOutputBuffer;
    int                bytesPerUserOutputBuffer;    /* native buffer size in bytes */
    /* Run Time -------------- */
    PaTimestamp        framesPlayed;
    long               lastPosition;                /* used to track frames played. */
    /* For measuring CPU utilization. */
    LARGE_INTEGER      entryCount;
    double             inverseTicksPerHostBuffer;
    /* Init Time -------------- */
    int                numHostBuffers;
    int                framesPerHostBuffer;
    int                userBuffersPerHostBuffer;
    CRITICAL_SECTION   streamLock;                  /* Mutext to prevent threads from colliding. */
    INT                streamLockInited;
#if PA_USE_TIMER_CALLBACK
    BOOL               ifInsideCallback;            /* Test for reentrancy. */
    MMRESULT           timerID;
#else
    HANDLE             abortEvent;
    int                abortEventInited;
    HANDLE             bufferEvent;
    int                bufferEventInited;
    HANDLE             engineThread;
    DWORD              engineThreadID;
#endif
}
PaWMMEStreamData;
/************************************************* Shared Data ********/
/* FIXME - put Mutex around this shared data. */
static int sNumInputDevices = 0;
static int sNumOutputDevices = 0;
static int sNumDevices = 0;
static PaDeviceInfo **sDevicePtrs = NULL;
static int sDefaultInputDeviceID = paNoDevice;
static int sDefaultOutputDeviceID = paNoDevice;
static int sPaHostError = 0;
static const char sMapperSuffixInput[] = " - Input";
static const char sMapperSuffixOutput[] = " - Output";

#if PA_TRACK_MEMORY
static int sNumAllocations = 0;
#endif

/************************************************* Macros ********/
/* Convert external PA ID to an internal ID that includes WAVE_MAPPER */
#define PaDeviceIdToWinId(id) (((id) < sNumInputDevices) ? (id - 1) : (id - sNumInputDevices - 1))
/************************************************* Prototypes **********/

void Pa_InitializeNumDevices( void );
PaError Pa_AllocateDevicePtrs( void );

static void CALLBACK Pa_TimerCallback(UINT uID, UINT uMsg,
                                      DWORD dwUser, DWORD dw1, DWORD dw2);
PaError PaHost_GetTotalBufferFrames( internalPortAudioStream   *past );
static PaError PaHost_UpdateStreamTime( PaWMMEStreamData *wmmeStreamData );
static PaError PaHost_BackgroundManager( internalPortAudioStream   *past );

static void *PaHost_AllocateTrackedMemory( long numBytes );
static void PaHost_FreeTrackedMemory( void *addr );

/*******************************************************************/
static PaError PaHost_AllocateWMMEStreamData( internalPortAudioStream *stream )
{
    PaError             result = paNoError;
    PaWMMEStreamData *wmmeStreamData;
    
    wmmeStreamData = (PaWMMEStreamData *) PaHost_AllocateFastMemory(sizeof(PaWMMEStreamData)); /* MEM */
    if( wmmeStreamData == NULL )
    {
        result = paInsufficientMemory;
        goto error;
    }
    memset( wmmeStreamData, 0, sizeof(PaWMMEStreamData) );
    stream->past_DeviceData = (void *) wmmeStreamData;

    return result;
    
error:
    return result;
}

/*******************************************************************/
static void PaHost_FreeWMMEStreamData( internalPortAudioStream *internalStream )
{
    PaWMMEStreamData *wmmeStreamData = (PaWMMEStreamData *) internalStream->past_DeviceData;

    PaHost_FreeFastMemory( wmmeStreamData, sizeof(PaWMMEStreamData) ); /* MEM */
    internalStream->past_DeviceData = NULL;
}
/*************************************************************************/
static PaWMMEStreamData* PaHost_GetWMMEStreamData( internalPortAudioStream* internalStream )
{
    PaWMMEStreamData *result = NULL;

    if( internalStream != NULL )
    {
        result = (PaWMMEStreamData *) internalStream->past_DeviceData;
    }
    return result;
}
/********************************* BEGIN CPU UTILIZATION MEASUREMENT ****/
/* FIXME: the cpu usage code should be factored out into a common module */
static void Pa_InitializeCpuUsageScalar( internalPortAudioStream   *stream )
{
    PaWMMEStreamData *wmmeStreamData = (PaWMMEStreamData *) stream->past_DeviceData;

    LARGE_INTEGER frequency;
    if( QueryPerformanceFrequency( &frequency ) == 0 )
    {
        wmmeStreamData->inverseTicksPerHostBuffer = 0.0;
    }
    else
    {
        wmmeStreamData->inverseTicksPerHostBuffer = stream->past_SampleRate /
                ( (double)frequency.QuadPart * stream->past_FramesPerUserBuffer * wmmeStreamData->userBuffersPerHostBuffer );
        DBUG(("inverseTicksPerHostBuffer = %g\n", wmmeStreamData->inverseTicksPerHostBuffer ));
    }
}
static void Pa_StartUsageCalculation( internalPortAudioStream   *stream )
{
    PaWMMEStreamData *wmmeStreamData = (PaWMMEStreamData *) stream->past_DeviceData;

    if( wmmeStreamData == NULL ) return;
    /* Query system timer for usage analysis and to prevent overuse of CPU. */
    QueryPerformanceCounter( &wmmeStreamData->entryCount );
}
static void Pa_EndUsageCalculation( internalPortAudioStream   *stream )
{
    LARGE_INTEGER CurrentCount;
    PaWMMEStreamData *wmmeStreamData = (PaWMMEStreamData *) stream->past_DeviceData;

    if( wmmeStreamData == NULL ) return;
    /*
     * Measure CPU utilization during this callback. Note that this calculation
     * assumes that we had the processor the whole time.
     */
#define LOWPASS_COEFFICIENT_0   (0.9)
#define LOWPASS_COEFFICIENT_1   (0.99999 - LOWPASS_COEFFICIENT_0)
    if( QueryPerformanceCounter( &CurrentCount ) )
    {
        LONGLONG InsideCount = CurrentCount.QuadPart - wmmeStreamData->entryCount.QuadPart;
        double newUsage = InsideCount * wmmeStreamData->inverseTicksPerHostBuffer;
        stream->past_Usage = (LOWPASS_COEFFICIENT_0 * stream->past_Usage) +
                             (LOWPASS_COEFFICIENT_1 * newUsage);
    }
}
/****************************************** END CPU UTILIZATION *******/

static void Pa_InitializeNumDevices( void )
{
    sNumInputDevices = waveInGetNumDevs();
    if( sNumInputDevices > 0 )
    {
        sNumInputDevices += 1; /* add one extra for the WAVE_MAPPER */
        sDefaultInputDeviceID = 0;
    }
    else
    {
        sDefaultInputDeviceID = paNoDevice;
    }

    sNumOutputDevices = waveOutGetNumDevs();
    if( sNumOutputDevices > 0 )
    {
        sNumOutputDevices += 1; /* add one extra for the WAVE_MAPPER */
        sDefaultOutputDeviceID = sNumInputDevices;
    }
    else
    {
        sDefaultOutputDeviceID = paNoDevice;
    }

    sNumDevices = sNumInputDevices + sNumOutputDevices;
}

static PaError Pa_AllocateDevicePtrs( void )
{
    int numBytes;
    int i;

    /* Allocate structures to hold device info. */
    /* PLB20010402 - was allocating too much memory. */
    /* numBytes = sNumDevices * sizeof(PaDeviceInfo);  // PLB20010402 */

    if( sNumDevices > 0 )
    {
        numBytes = sNumDevices * sizeof(PaDeviceInfo *); /* PLB20010402 */
        sDevicePtrs = (PaDeviceInfo **) PaHost_AllocateTrackedMemory( numBytes ); /* MEM */
        if( sDevicePtrs == NULL ) return paInsufficientMemory;

        for( i = 0; i < sNumDevices; i++ )
            sDevicePtrs[i] = NULL;  /* RDB20020417 explicitly set each ptr to NULL */
    }
    else
    {
        sDevicePtrs = NULL;
    }
    
    return paNoError;
}
/*************************************************************************/
long Pa_GetHostError()
{
    return sPaHostError;
}
/*************************************************************************/
int Pa_CountDevices()
{
    if( PaHost_IsInitialized() )
        return sNumDevices;
    else
        return 0;
}
/*************************************************************************
 * If a PaDeviceInfo structure has not already been created,
 * then allocate one and fill it in for the selected device.
 *
 * We create one extra input and one extra output device for the WAVE_MAPPER.
 * [Does anyone know how to query the default device and get its name?]
 */
const PaDeviceInfo* Pa_GetDeviceInfo( PaDeviceID id )
{
#define NUM_STANDARDSAMPLINGRATES   3   /* 11025, 22050, 44100 */
    static DWORD customSamplingRates[] = { 8000, 32000, 48000, 64000, 88200, 96000 };
#define NUM_CUSTOMSAMPLINGRATES     (sizeof(customSamplingRates)/sizeof(DWORD))
#define MAX_NUMSAMPLINGRATES        (NUM_STANDARDSAMPLINGRATES+NUM_CUSTOMSAMPLINGRATES)

    PaDeviceInfo *deviceInfo;
    double *sampleRates; /* non-const ptr */
    int i;
    char *s;

    if( id < 0 || id >= sNumDevices )
        return NULL;
    if( sDevicePtrs[ id ] != NULL )
    {
        return sDevicePtrs[ id ];
    }
    deviceInfo = (PaDeviceInfo *)PaHost_AllocateTrackedMemory( sizeof(PaDeviceInfo) ); /* MEM */
    if( deviceInfo == NULL ) return NULL;
    deviceInfo->structVersion = 1;
    deviceInfo->maxInputChannels = 0;
    deviceInfo->maxOutputChannels = 0;
    deviceInfo->numSampleRates = 0;
    sampleRates = (double*)PaHost_AllocateTrackedMemory( MAX_NUMSAMPLINGRATES * sizeof(double) ); /* MEM */
    deviceInfo->sampleRates = sampleRates;
    deviceInfo->nativeSampleFormats = paInt16;       /* should query for higher bit depths below */
    if( id < sNumInputDevices )
    {
        /* input device */
        int inputMmID = PaDeviceIdToWinId(id);
        WAVEINCAPS wic;
        if( waveInGetDevCaps( inputMmID, &wic, sizeof( WAVEINCAPS ) ) != MMSYSERR_NOERROR )
            goto error;

        /* Append I/O suffix to WAVE_MAPPER device. */
        if( inputMmID == WAVE_MAPPER )
        {
            s = (char *) PaHost_AllocateTrackedMemory( strlen( wic.szPname ) + 1 + sizeof(sMapperSuffixInput) ); /* MEM */
            strcpy( s, wic.szPname );
            strcat( s, sMapperSuffixInput );
        }
        else
        {
            s = (char *) PaHost_AllocateTrackedMemory( strlen( wic.szPname ) + 1 ); /* MEM */
            strcpy( s, wic.szPname );
        }
        deviceInfo->name = s;
        deviceInfo->maxInputChannels = wic.wChannels;
        /* Sometimes a device can return a rediculously large number of channels.
         * This happened with an SBLive card on a Windows ME box.
         * If that happens, then force it to 2 channels.  PLB20010413
         */
        if( (deviceInfo->maxInputChannels < 1) || (deviceInfo->maxInputChannels > 256) )
        {
            ERR_RPT(("Pa_GetDeviceInfo: Num input channels reported as %d! Changed to 2.\n", deviceInfo->maxOutputChannels ));
            deviceInfo->maxInputChannels = 2;
        }
        /* Add a sample rate to the list if we can do stereo 16 bit at that rate
         * based on the format flags. */
        if( wic.dwFormats & WAVE_FORMAT_1M16 ||wic.dwFormats & WAVE_FORMAT_1S16 )
            sampleRates[ deviceInfo->numSampleRates++ ] = 11025.;
        if( wic.dwFormats & WAVE_FORMAT_2M16 ||wic.dwFormats & WAVE_FORMAT_2S16 )
            sampleRates[ deviceInfo->numSampleRates++ ] = 22050.;
        if( wic.dwFormats & WAVE_FORMAT_4M16 ||wic.dwFormats & WAVE_FORMAT_4S16 )
            sampleRates[ deviceInfo->numSampleRates++ ] = 44100.;
        /* Add a sample rate to the list if we can do stereo 16 bit at that rate
         * based on opening the device successfully. */
        for( i=0; i < NUM_CUSTOMSAMPLINGRATES; i++ )
        {
            WAVEFORMATEX wfx;
            wfx.wFormatTag = WAVE_FORMAT_PCM;
            wfx.nSamplesPerSec = customSamplingRates[i];
            wfx.wBitsPerSample = 16;
            wfx.cbSize = 0; /* ignored */
            wfx.nChannels = (WORD)deviceInfo->maxInputChannels;
            wfx.nAvgBytesPerSec = wfx.nChannels * wfx.nSamplesPerSec * sizeof(short);
            wfx.nBlockAlign = (WORD)(wfx.nChannels * sizeof(short));
            if( waveInOpen( NULL, inputMmID, &wfx, 0, 0, WAVE_FORMAT_QUERY ) == MMSYSERR_NOERROR )
            {
                sampleRates[ deviceInfo->numSampleRates++ ] = customSamplingRates[i];
            }
        }

    }
    else if( id - sNumInputDevices < sNumOutputDevices )
    {
        /* output device */
        int outputMmID = PaDeviceIdToWinId(id);
        WAVEOUTCAPS woc;
        if( waveOutGetDevCaps( outputMmID, &woc, sizeof( WAVEOUTCAPS ) ) != MMSYSERR_NOERROR )
            goto error;
        /* Append I/O suffix to WAVE_MAPPER device. */
        if( outputMmID == WAVE_MAPPER )
        {
            s = (char *) PaHost_AllocateTrackedMemory( strlen( woc.szPname ) + 1 + sizeof(sMapperSuffixOutput) );  /* MEM */
            strcpy( s, woc.szPname );
            strcat( s, sMapperSuffixOutput );
        }
        else
        {
            s = (char *) PaHost_AllocateTrackedMemory( strlen( woc.szPname ) + 1 );  /* MEM */
            strcpy( s, woc.szPname );
        }
        deviceInfo->name = s;
        deviceInfo->maxOutputChannels = woc.wChannels;
        /* Sometimes a device can return a rediculously large number of channels.
         * This happened with an SBLive card on a Windows ME box.
         * It also happens on Win XP!
         */
        if( (deviceInfo->maxOutputChannels < 1) || (deviceInfo->maxOutputChannels > 256) )
        {
#if 1
            deviceInfo->maxOutputChannels = 2;
#else
        /* If channel max is goofy, then query for max channels. PLB20020228
         * This doesn't seem to help. Disable code for now. Remove it later.
         */
            ERR_RPT(("Pa_GetDeviceInfo: Num output channels reported as %d!", deviceInfo->maxOutputChannels ));
            deviceInfo->maxOutputChannels = 0;
			/* Attempt to find the correct maximum by querying the device. */
			for( i=2; i<16; i += 2 )
			{
				WAVEFORMATEX wfx;
				wfx.wFormatTag = WAVE_FORMAT_PCM;
				wfx.nSamplesPerSec = 44100;
				wfx.wBitsPerSample = 16;
				wfx.cbSize = 0; /* ignored */
				wfx.nChannels = (WORD) i;
				wfx.nAvgBytesPerSec = wfx.nChannels * wfx.nSamplesPerSec * sizeof(short);
				wfx.nBlockAlign = (WORD)(wfx.nChannels * sizeof(short));
				if( waveOutOpen( NULL, outputMmID, &wfx, 0, 0, WAVE_FORMAT_QUERY ) == MMSYSERR_NOERROR )
				{
					deviceInfo->maxOutputChannels = i;
				}
				else
				{
					break;
				}
			}
#endif
            ERR_RPT((" Changed to %d.\n", deviceInfo->maxOutputChannels ));
        }

        /* Add a sample rate to the list if we can do stereo 16 bit at that rate
         * based on the format flags. */
        if( woc.dwFormats & WAVE_FORMAT_1M16 ||woc.dwFormats & WAVE_FORMAT_1S16 )
            sampleRates[ deviceInfo->numSampleRates++ ] = 11025.;
        if( woc.dwFormats & WAVE_FORMAT_2M16 ||woc.dwFormats & WAVE_FORMAT_2S16 )
            sampleRates[ deviceInfo->numSampleRates++ ] = 22050.;
        if( woc.dwFormats & WAVE_FORMAT_4M16 ||woc.dwFormats & WAVE_FORMAT_4S16 )
            sampleRates[ deviceInfo->numSampleRates++ ] = 44100.;

        /* Add a sample rate to the list if we can do stereo 16 bit at that rate
         * based on opening the device successfully. */
        for( i=0; i < NUM_CUSTOMSAMPLINGRATES; i++ )
        {
            WAVEFORMATEX wfx;
            wfx.wFormatTag = WAVE_FORMAT_PCM;
            wfx.nSamplesPerSec = customSamplingRates[i];
            wfx.wBitsPerSample = 16;
            wfx.cbSize = 0; /* ignored */
            wfx.nChannels = (WORD)deviceInfo->maxOutputChannels;
            wfx.nAvgBytesPerSec = wfx.nChannels * wfx.nSamplesPerSec * sizeof(short);
            wfx.nBlockAlign = (WORD)(wfx.nChannels * sizeof(short));
            if( waveOutOpen( NULL, outputMmID, &wfx, 0, 0, WAVE_FORMAT_QUERY ) == MMSYSERR_NOERROR )
            {
                sampleRates[ deviceInfo->numSampleRates++ ] = customSamplingRates[i];
            }
        }
    }
    sDevicePtrs[ id ] = deviceInfo;
    return deviceInfo;

error:
    PaHost_FreeTrackedMemory( sampleRates ); /* MEM */
    PaHost_FreeTrackedMemory( deviceInfo ); /* MEM */

    return NULL;
}
/*************************************************************************
 * Returns recommended device ID.
 * On the PC, the recommended device can be specified by the user by
 * setting an environment variable. For example, to use device #1.
 *
 *    set PA_RECOMMENDED_OUTPUT_DEVICE=1
 *
 * The user should first determine the available device ID by using
 * the supplied application "pa_devs".
 */
#define PA_ENV_BUF_SIZE  (32)
#define PA_REC_IN_DEV_ENV_NAME  ("PA_RECOMMENDED_INPUT_DEVICE")
#define PA_REC_OUT_DEV_ENV_NAME  ("PA_RECOMMENDED_OUTPUT_DEVICE")
static PaDeviceID PaHost_GetEnvDefaultDeviceID( char *envName )
{
    DWORD   hresult;
    char    envbuf[PA_ENV_BUF_SIZE];
    PaDeviceID recommendedID = paNoDevice;

    /* Let user determine default device by setting environment variable. */
    hresult = GetEnvironmentVariable( envName, envbuf, PA_ENV_BUF_SIZE );
    if( (hresult > 0) && (hresult < PA_ENV_BUF_SIZE) )
    {
        recommendedID = atoi( envbuf );
    }
    return recommendedID;
}
/**********************************************************************
 * Check for environment variable, else query devices and use result.
 */
PaDeviceID Pa_GetDefaultInputDeviceID( void )
{
    PaDeviceID result;

    result = PaHost_GetEnvDefaultDeviceID( PA_REC_IN_DEV_ENV_NAME );
    if( result == paNoDevice || result < 0 || result >= sNumInputDevices )
    {
        result = sDefaultInputDeviceID;
    }
    return result;
}
PaDeviceID Pa_GetDefaultOutputDeviceID( void )
{
    PaDeviceID result;

    result = PaHost_GetEnvDefaultDeviceID( PA_REC_OUT_DEV_ENV_NAME );
    if( result == paNoDevice || result < sNumInputDevices || result >= sNumDevices )
    {
        result = sDefaultOutputDeviceID;
    }
    return result;
}
/**********************************************************************
 * Initialize Host dependant part of API.
 */
PaError PaHost_Init( void )
{

#if PA_TRACK_MEMORY
    PRINT(("PaHost_Init: sNumAllocations = %d\n", sNumAllocations ));
#endif

#if PA_SIMULATE_UNDERFLOW
    PRINT(("WARNING - Underflow Simulation Enabled - Expect a Big Glitch!!!\n"));
#endif
   

    Pa_InitializeNumDevices();

    return Pa_AllocateDevicePtrs();
}

/**********************************************************************
 * Check WAVE buffers to see if they are done.
 * Fill any available output buffers and use any available
 * input buffers by calling user callback.
 *
 * This routine will loop until:
 *    user callback returns !=0 OR
 *    all output buffers are filled OR
 *    past->past_StopSoon is set OR
 *    an error occurs when calling WMME.
 *
 * Returns >0 when user requests a stop, <0 on error.
 *
 */
static PaError Pa_TimeSlice( internalPortAudioStream *stream )
{
    PaError           result = paNoError;
    MMRESULT          mmresult;
    char             *inBufPtr;
    char             *outBufPtr;
    int               gotInput = 0;
    int               gotOutput = 0;
    int               i;
    int               buffersProcessed = 0;
    int               done = 0;
    PaWMMEStreamData *wmmeStreamData = (PaWMMEStreamData *) stream->past_DeviceData;

    if( wmmeStreamData == NULL ) return paInternalError;

    stream->past_NumCallbacks += 1;
#if PA_TRACE_RUN
    AddTraceMessage("Pa_TimeSlice: past_NumCallbacks ", stream->past_NumCallbacks );
#endif

    /* JM20020118 - prevent hung thread when buffers underflow. */
    /* while( !done ) /* BAD */
    while( !done && !stream->past_StopSoon ) /* GOOD */
    {
#if PA_SIMULATE_UNDERFLOW
        if(gUnderCallbackCounter++ == UNDER_SLEEP_AT)
        {
            Sleep(UNDER_SLEEP_FOR);
        }
#endif

        /* If we are using output, then we need an empty output buffer. */
        gotOutput = 0;
        outBufPtr = NULL;
        if( stream->past_NumOutputChannels > 0 )
        {
            if((wmmeStreamData->outputBuffers[ wmmeStreamData->currentOutputBuffer ].dwFlags & WHDR_DONE) == 0)
            {
                break;  /* If none empty then bail and try again later. */
            }
            else
            {
                outBufPtr = wmmeStreamData->outputBuffers[ wmmeStreamData->currentOutputBuffer ].lpData;
                gotOutput = 1;
            }
        }
        /* Use an input buffer if one is available. */
        gotInput = 0;
        inBufPtr = NULL;
        if( ( stream->past_NumInputChannels > 0 ) &&
                (wmmeStreamData->inputBuffers[ wmmeStreamData->currentInputBuffer ].dwFlags & WHDR_DONE) )
        {
            inBufPtr = wmmeStreamData->inputBuffers[ wmmeStreamData->currentInputBuffer ].lpData;
            gotInput = 1;
#if PA_TRACE_RUN
            AddTraceMessage("Pa_TimeSlice: got input buffer at ", (int)inBufPtr );
            AddTraceMessage("Pa_TimeSlice: got input buffer # ", wmmeStreamData->currentInputBuffer );
#endif

        }
        /* If we can't do anything then bail out. */
        if( !gotInput && !gotOutput ) break;
        buffersProcessed += 1;
        /* Each Wave buffer contains multiple user buffers so do them all now. */
        /* Base Usage on time it took to process one host buffer. */
        Pa_StartUsageCalculation( stream );
        for( i=0; i<wmmeStreamData->userBuffersPerHostBuffer; i++ )
        {
            if( done )
            {
                if( gotOutput )
                {
                    /* Clear remainder of wave buffer if we are waiting for stop. */
                    AddTraceMessage("Pa_TimeSlice: zero rest of wave buffer ", i );
                    memset( outBufPtr, 0, wmmeStreamData->bytesPerUserOutputBuffer );
                }
            }
            else
            {
                /* Convert 16 bit native data to user data and call user routine. */
                result = Pa_CallConvertInt16( stream, (short *) inBufPtr, (short *) outBufPtr );
                if( result != 0) done = 1;
            }
            if( gotInput ) inBufPtr += wmmeStreamData->bytesPerUserInputBuffer;
            if( gotOutput) outBufPtr += wmmeStreamData->bytesPerUserOutputBuffer;
        }
        Pa_EndUsageCalculation( stream );
        /* Send WAVE buffer to Wave Device to be refilled. */
        if( gotInput )
        {
            mmresult = waveInAddBuffer( wmmeStreamData->hWaveIn,
                                       &wmmeStreamData->inputBuffers[ wmmeStreamData->currentInputBuffer ],
                                       sizeof(WAVEHDR) );
            if( mmresult != MMSYSERR_NOERROR )
            {
                sPaHostError = mmresult;
                result = paHostError;
                break;
            }
            wmmeStreamData->currentInputBuffer = (wmmeStreamData->currentInputBuffer+1 >= wmmeStreamData->numHostBuffers) ?
                                              0 : wmmeStreamData->currentInputBuffer+1;
        }
        /* Write WAVE buffer to Wave Device. */
        if( gotOutput )
        {
#if PA_TRACE_START_STOP
            AddTraceMessage( "Pa_TimeSlice: writing buffer ", wmmeStreamData->currentOutputBuffer );
#endif
            mmresult = waveOutWrite( wmmeStreamData->hWaveOut,
                                    &wmmeStreamData->outputBuffers[ wmmeStreamData->currentOutputBuffer ],
                                    sizeof(WAVEHDR) );
            if( mmresult != MMSYSERR_NOERROR )
            {
                sPaHostError = mmresult;
                result = paHostError;
                break;
            }
            wmmeStreamData->currentOutputBuffer = (wmmeStreamData->currentOutputBuffer+1 >= wmmeStreamData->numHostBuffers) ?
                                               0 : wmmeStreamData->currentOutputBuffer+1;
        }

    }

#if PA_TRACE_RUN
    AddTraceMessage("Pa_TimeSlice: buffersProcessed ", buffersProcessed );
#endif
    return (result != 0) ? result : done;
}

/*******************************************************************/
static PaError PaHost_BackgroundManager( internalPortAudioStream *stream )
{
    PaError      result = paNoError;
    int          i;
    int          numQueuedoutputBuffers = 0;
    PaWMMEStreamData *wmmeStreamData = (PaWMMEStreamData *) stream->past_DeviceData;

    /* Has someone asked us to abort by calling Pa_AbortStream()? */
    if( stream->past_StopNow )
    {
        stream->past_IsActive = 0; /* Will cause thread to return. */
    }
    /* Has someone asked us to stop by calling Pa_StopStream()
     * OR has a user callback returned '1' to indicate finished.
     */
    else if( stream->past_StopSoon )
    {
        /* Poll buffer and when all have played then exit thread. */
        /* Count how many output buffers are queued. */
        numQueuedoutputBuffers = 0;
        if( stream->past_NumOutputChannels > 0 )
        {
            for( i=0; i<wmmeStreamData->numHostBuffers; i++ )
            {
                if( !( wmmeStreamData->outputBuffers[ i ].dwFlags & WHDR_DONE) )
                {
#if PA_TRACE_START_STOP
                    AddTraceMessage( "PaHost_BackgroundManager: waiting for buffer ", i );
#endif
                    numQueuedoutputBuffers++;
                }
            }
        }
#if PA_TRACE_START_STOP
        AddTraceMessage( "PaHost_BackgroundManager: numQueuedoutputBuffers ", numQueuedoutputBuffers );
#endif
        if( numQueuedoutputBuffers == 0 )
        {
            stream->past_IsActive = 0; /* Will cause thread to return. */
        }
    }
    else
    {
        /* Process full input buffer and fill up empty output buffers. */
        if( (result = Pa_TimeSlice( stream )) != 0)
        {
            /* User callback has asked us to stop. */
#if PA_TRACE_START_STOP
            AddTraceMessage( "PaHost_BackgroundManager: TimeSlice() returned ", result );
#endif
            stream->past_StopSoon = 1; /* Request that audio play out then stop. */
            result = paNoError;
        }
    }

    PaHost_UpdateStreamTime( wmmeStreamData );
    return result;
}

#if PA_USE_TIMER_CALLBACK
/*******************************************************************/
static void CALLBACK Pa_TimerCallback(UINT uID, UINT uMsg, DWORD dwUser, DWORD dw1, DWORD dw2)
{
    internalPortAudioStream   *stream;
    PaWMMEStreamData        *wmmeStreamData;
    PaError                    result;

    stream = (internalPortAudioStream *) dwUser;
    if( stream == NULL ) return;
    wmmeStreamData = (PaWMMEStreamData *) stream->past_DeviceData;
    if( wmmeStreamData == NULL ) return;
    if( wmmeStreamData->ifInsideCallback )
    {
        if( wmmeStreamData->timerID != 0 )
        {
            timeKillEvent(wmmeStreamData->timerID);  /* Stop callback timer. */
            wmmeStreamData->timerID = 0;
        }
        return;
    }
    wmmeStreamData->ifInsideCallback = 1;
    /* Manage flags and audio processing. */
    result = PaHost_BackgroundManager( stream );
    if( result != paNoError )
    {
        stream->past_IsActive = 0;
    }
    wmmeStreamData->ifInsideCallback = 0;
}
#else /* PA_USE_TIMER_CALLBACK */
/*******************************************************************/
static DWORD WINAPI WinMMPa_OutputThreadProc( void *pArg )
{
    internalPortAudioStream *stream;
    PaWMMEStreamData      *wmmeStreamData;
    HANDLE       events[2];
    int          numEvents = 0;
    DWORD        result = 0;
    DWORD        waitResult;
    DWORD        numTimeouts = 0;
    DWORD        timeOut;
    stream = (internalPortAudioStream *) pArg;
    wmmeStreamData = (PaWMMEStreamData *) stream->past_DeviceData;
#if PA_TRACE_START_STOP
    AddTraceMessage( "WinMMPa_OutputThreadProc: timeoutPeriod", timeoutPeriod );
    AddTraceMessage( "WinMMPa_OutputThreadProc: past_NumUserBuffers", stream->past_NumUserBuffers );
#endif
    /* Calculate timeOut as half the time it would take to play all buffers. */
    timeOut = (DWORD) (500.0 * PaHost_GetTotalBufferFrames( stream ) / stream->past_SampleRate);
    /* Get event(s) ready for wait. */
    events[numEvents++] = wmmeStreamData->bufferEvent;
    if( wmmeStreamData->abortEventInited ) events[numEvents++] = wmmeStreamData->abortEvent;
    /* Stay in this thread as long as we are "active". */
    while( stream->past_IsActive )
    {
        /*******************************************************************/
        /******** WAIT here for an event from WMME or PA *******************/
        /*******************************************************************/
        waitResult = WaitForMultipleObjects( numEvents, events, FALSE, timeOut );
        /* Error? */
        if( waitResult == WAIT_FAILED )
        {
            sPaHostError = GetLastError();
            result = paHostError;
            stream->past_IsActive = 0;
        }
        /* Timeout? Don't stop. Just keep polling for DONE.*/
        else if( waitResult == WAIT_TIMEOUT )
        {
#if PA_TRACE_START_STOP
            AddTraceMessage( "WinMMPa_OutputThreadProc: timed out ", numQueuedoutputBuffers );
#endif
            numTimeouts += 1;
        }
        /* Manage flags and audio processing. */
        result = PaHost_BackgroundManager( stream );
        if( result != paNoError )
        {
            stream->past_IsActive = 0;
        }
    }
    return result;
}
#endif

/*******************************************************************/
PaError PaHost_OpenInputStream( internalPortAudioStream   *stream )
{
    PaError          result = paNoError;
    MMRESULT         mmresult;
    PaWMMEStreamData *wmmeStreamData;
    int              i;
    int              inputMmId;
    int              bytesPerInputFrame;
    WAVEFORMATEX     wfx;
    const PaDeviceInfo  *deviceInfo;

    wmmeStreamData = (PaWMMEStreamData *) stream->past_DeviceData;
    DBUG(("PaHost_OpenStream: deviceID = 0x%x\n", stream->past_InputDeviceID));
    deviceInfo = Pa_GetDeviceInfo( stream->past_InputDeviceID );
    if( deviceInfo == NULL ) return paInternalError;

    switch( deviceInfo->nativeSampleFormats  )
    {
    case paInt32:
    case paFloat32:
        bytesPerInputFrame = sizeof(float) * stream->past_NumInputChannels;
        break;
    default:
        bytesPerInputFrame = sizeof(short) * stream->past_NumInputChannels;
        break;
    }
    wfx.wFormatTag = WAVE_FORMAT_PCM;
    wfx.nChannels = (WORD) stream->past_NumInputChannels;
    wfx.nSamplesPerSec = (DWORD) stream->past_SampleRate;
    wfx.nAvgBytesPerSec = (DWORD)(bytesPerInputFrame * stream->past_SampleRate);
    wfx.nBlockAlign = (WORD)bytesPerInputFrame;
    wfx.wBitsPerSample = (WORD)((bytesPerInputFrame/stream->past_NumInputChannels) * 8);
    wfx.cbSize = 0;
    inputMmId = PaDeviceIdToWinId( stream->past_InputDeviceID );
#if PA_USE_TIMER_CALLBACK
    mmresult = waveInOpen( &wmmeStreamData->hWaveIn, inputMmId, &wfx,
                     0, 0, CALLBACK_NULL );
#else
    mmresult = waveInOpen( &wmmeStreamData->hWaveIn, inputMmId, &wfx,
                     (DWORD)wmmeStreamData->bufferEvent, (DWORD) stream, CALLBACK_EVENT );
#endif
    if( mmresult != MMSYSERR_NOERROR )
    {
        ERR_RPT(("PortAudio: PaHost_OpenInputStream() failed!\n"));
        result = paHostError;
        sPaHostError = mmresult;
        goto error;
    }
    /* Allocate an array to hold the buffer pointers. */
    wmmeStreamData->inputBuffers = (WAVEHDR *) PaHost_AllocateTrackedMemory( sizeof(WAVEHDR)*wmmeStreamData->numHostBuffers ); /* MEM */
    if( wmmeStreamData->inputBuffers == NULL )
    {
        result = paInsufficientMemory;
        goto error;
    }
    /* Allocate each buffer. */
    for( i=0; i<wmmeStreamData->numHostBuffers; i++ )
    {
        wmmeStreamData->inputBuffers[i].lpData = (char *)PaHost_AllocateTrackedMemory( wmmeStreamData->bytesPerHostInputBuffer ); /* MEM */
        if( wmmeStreamData->inputBuffers[i].lpData == NULL )
        {
            result = paInsufficientMemory;
            goto error;
        }
        wmmeStreamData->inputBuffers[i].dwBufferLength = wmmeStreamData->bytesPerHostInputBuffer;
        wmmeStreamData->inputBuffers[i].dwUser = i;
        if( ( mmresult = waveInPrepareHeader( wmmeStreamData->hWaveIn, &wmmeStreamData->inputBuffers[i], sizeof(WAVEHDR) )) != MMSYSERR_NOERROR )
        {
            result = paHostError;
            sPaHostError = mmresult;
            goto error;
        }
    }
    return result;

error:
    return result;
}
/*******************************************************************/
PaError PaHost_OpenOutputStream( internalPortAudioStream *stream )
{
    PaError          result = paNoError;
    MMRESULT         mmresult;
    PaWMMEStreamData *wmmeStreamData;
    int              i;
    int              outputMmID;
    int              bytesPerOutputFrame;
    WAVEFORMATEX     wfx;
    const PaDeviceInfo *deviceInfo;

    wmmeStreamData = (PaWMMEStreamData *) stream->past_DeviceData;
    DBUG(("PaHost_OpenStream: deviceID = 0x%x\n", stream->past_OutputDeviceID));

    deviceInfo = Pa_GetDeviceInfo( stream->past_OutputDeviceID );
    if( deviceInfo == NULL ) return paInternalError;

    switch( deviceInfo->nativeSampleFormats  )
    {
    case paInt32:
    case paFloat32:
        bytesPerOutputFrame = sizeof(float) * stream->past_NumOutputChannels;
        break;
    default:
        bytesPerOutputFrame = sizeof(short) * stream->past_NumOutputChannels;
        break;
    }
    wfx.wFormatTag = WAVE_FORMAT_PCM;
    wfx.nChannels = (WORD) stream->past_NumOutputChannels;
    wfx.nSamplesPerSec = (DWORD) stream->past_SampleRate;
    wfx.nAvgBytesPerSec = (DWORD)(bytesPerOutputFrame * stream->past_SampleRate);
    wfx.nBlockAlign = (WORD)bytesPerOutputFrame;
    wfx.wBitsPerSample = (WORD)((bytesPerOutputFrame/stream->past_NumOutputChannels) * 8);
    wfx.cbSize = 0;
    outputMmID = PaDeviceIdToWinId( stream->past_OutputDeviceID );
#if PA_USE_TIMER_CALLBACK
    mmresult = waveOutOpen( &wmmeStreamData->hWaveOut, outputMmID, &wfx,
                      0, 0, CALLBACK_NULL );
#else

    wmmeStreamData->abortEvent = CreateEvent( NULL, TRUE, FALSE, NULL );
    if( wmmeStreamData->abortEvent == NULL )
    {
        result = paHostError;
        sPaHostError = GetLastError();
        goto error;
    }
    wmmeStreamData->abortEventInited = 1;
    mmresult = waveOutOpen( &wmmeStreamData->hWaveOut, outputMmID, &wfx,
                      (DWORD)wmmeStreamData->bufferEvent, (DWORD) stream, CALLBACK_EVENT );
#endif
    if( mmresult != MMSYSERR_NOERROR )
    {
        ERR_RPT(("PortAudio: PaHost_OpenOutputStream() failed!\n"));
        result = paHostError;
        sPaHostError = mmresult;
        goto error;
    }
    /* Allocate an array to hold the buffer pointers. */
    wmmeStreamData->outputBuffers = (WAVEHDR *) PaHost_AllocateTrackedMemory( sizeof(WAVEHDR)*wmmeStreamData->numHostBuffers ); /* MEM */
    if( wmmeStreamData->outputBuffers == NULL )
    {
        result = paInsufficientMemory;
        goto error;
    }
    /* Allocate each buffer. */
    for( i=0; i<wmmeStreamData->numHostBuffers; i++ )
    {
        wmmeStreamData->outputBuffers[i].lpData = (char *) PaHost_AllocateTrackedMemory( wmmeStreamData->bytesPerHostOutputBuffer ); /* MEM */
        if( wmmeStreamData->outputBuffers[i].lpData == NULL )
        {
            result = paInsufficientMemory;
            goto error;
        }
        wmmeStreamData->outputBuffers[i].dwBufferLength = wmmeStreamData->bytesPerHostOutputBuffer;
        wmmeStreamData->outputBuffers[i].dwUser = i;
        if( (mmresult = waveOutPrepareHeader( wmmeStreamData->hWaveOut, &wmmeStreamData->outputBuffers[i], sizeof(WAVEHDR) )) != MMSYSERR_NOERROR )
        {
            result = paHostError;
            sPaHostError = mmresult;
            goto error;
        }
    }
    return result;

error:
    return result;
}
/*******************************************************************/
PaError PaHost_GetTotalBufferFrames( internalPortAudioStream *stream )
{
    PaWMMEStreamData *wmmeStreamData = (PaWMMEStreamData *) stream->past_DeviceData;
    return wmmeStreamData->numHostBuffers * wmmeStreamData->framesPerHostBuffer;
}
/*******************************************************************
 * Determine number of WAVE Buffers
 * and how many User Buffers we can put into each WAVE buffer.
 */
static void PaHost_CalcNumHostBuffers( internalPortAudioStream *stream )
{
    PaWMMEStreamData *wmmeStreamData = (PaWMMEStreamData *) stream->past_DeviceData;
    unsigned int  minNumBuffers;
    int           minframesPerHostBuffer;
    int           maxframesPerHostBuffer;
    int           minTotalFrames;
    int           userBuffersPerHostBuffer;
    int           framesPerHostBuffer;
    int           numHostBuffers;

    /* Calculate minimum and maximum sizes based on timing and sample rate. */
    minframesPerHostBuffer = (int) (PA_MIN_MSEC_PER_HOST_BUFFER * stream->past_SampleRate * 0.001);
    minframesPerHostBuffer = (minframesPerHostBuffer + 7) & ~7;
    DBUG(("PaHost_CalcNumHostBuffers: minframesPerHostBuffer = %d\n", minframesPerHostBuffer ));
    maxframesPerHostBuffer = (int) (PA_MAX_MSEC_PER_HOST_BUFFER * stream->past_SampleRate * 0.001);
    maxframesPerHostBuffer = (maxframesPerHostBuffer + 7) & ~7;
    DBUG(("PaHost_CalcNumHostBuffers: maxframesPerHostBuffer = %d\n", maxframesPerHostBuffer ));
    /* Determine number of user buffers based on minimum latency. */
    minNumBuffers = Pa_GetMinNumBuffers( stream->past_FramesPerUserBuffer, stream->past_SampleRate );
    stream->past_NumUserBuffers = ( minNumBuffers > stream->past_NumUserBuffers ) ? minNumBuffers : stream->past_NumUserBuffers;
    DBUG(("PaHost_CalcNumHostBuffers: min past_NumUserBuffers = %d\n", stream->past_NumUserBuffers ));
    minTotalFrames = stream->past_NumUserBuffers * stream->past_FramesPerUserBuffer;
    /* We cannot make the WAVE buffers too small because they may not get serviced quickly enough. */
    if( (int) stream->past_FramesPerUserBuffer < minframesPerHostBuffer )
    {
        userBuffersPerHostBuffer =
            (minframesPerHostBuffer + stream->past_FramesPerUserBuffer - 1) /
            stream->past_FramesPerUserBuffer;
    }
    else
    {
        userBuffersPerHostBuffer = 1;
    }
    framesPerHostBuffer = stream->past_FramesPerUserBuffer * userBuffersPerHostBuffer;
    /* Calculate number of WAVE buffers needed. Round up to cover minTotalFrames. */
    numHostBuffers = (minTotalFrames + framesPerHostBuffer - 1) / framesPerHostBuffer;
    /* Make sure we have anough WAVE buffers. */
    if( numHostBuffers < PA_MIN_NUM_HOST_BUFFERS)
    {
        numHostBuffers = PA_MIN_NUM_HOST_BUFFERS;
    }
    else if( (numHostBuffers > PA_MAX_NUM_HOST_BUFFERS) &&
             ((int) stream->past_FramesPerUserBuffer < (maxframesPerHostBuffer/2) ) )
    {
        /* If we have too many WAVE buffers, try to put more user buffers in a wave buffer. */
        while(numHostBuffers > PA_MAX_NUM_HOST_BUFFERS)
        {
            userBuffersPerHostBuffer += 1;
            framesPerHostBuffer = stream->past_FramesPerUserBuffer * userBuffersPerHostBuffer;
            numHostBuffers = (minTotalFrames + framesPerHostBuffer - 1) / framesPerHostBuffer;
            /* If we have gone too far, back up one. */
            if( (framesPerHostBuffer > maxframesPerHostBuffer) ||
                    (numHostBuffers < PA_MAX_NUM_HOST_BUFFERS) )
            {
                userBuffersPerHostBuffer -= 1;
                framesPerHostBuffer = stream->past_FramesPerUserBuffer * userBuffersPerHostBuffer;
                numHostBuffers = (minTotalFrames + framesPerHostBuffer - 1) / framesPerHostBuffer;
                break;
            }
        }
    }

    wmmeStreamData->userBuffersPerHostBuffer = userBuffersPerHostBuffer;
    wmmeStreamData->framesPerHostBuffer = framesPerHostBuffer;
    wmmeStreamData->numHostBuffers = numHostBuffers;
    DBUG(("PaHost_CalcNumHostBuffers: userBuffersPerHostBuffer = %d\n", wmmeStreamData->userBuffersPerHostBuffer ));
    DBUG(("PaHost_CalcNumHostBuffers: numHostBuffers = %d\n", wmmeStreamData->numHostBuffers ));
    DBUG(("PaHost_CalcNumHostBuffers: framesPerHostBuffer = %d\n", wmmeStreamData->framesPerHostBuffer ));
    DBUG(("PaHost_CalcNumHostBuffers: past_NumUserBuffers = %d\n", stream->past_NumUserBuffers ));
}
/*******************************************************************/
PaError PaHost_OpenStream( internalPortAudioStream *stream )
{
    PaError             result = paNoError;
    PaWMMEStreamData *wmmeStreamData;

    result = PaHost_AllocateWMMEStreamData( stream );
    if( result != paNoError ) return result;

    wmmeStreamData = PaHost_GetWMMEStreamData( stream );

    /* Figure out how user buffers fit into WAVE buffers. */
    PaHost_CalcNumHostBuffers( stream );
    {
        int msecLatency = (int) ((PaHost_GetTotalBufferFrames(stream) * 1000) / stream->past_SampleRate);
        DBUG(("PortAudio on WMME - Latency = %d frames, %d msec\n", PaHost_GetTotalBufferFrames(stream), msecLatency ));
    }
    InitializeCriticalSection( &wmmeStreamData->streamLock );
    wmmeStreamData->streamLockInited = 1;

#if (PA_USE_TIMER_CALLBACK == 0)
    wmmeStreamData->bufferEventInited = 0;
    wmmeStreamData->bufferEvent = CreateEvent( NULL, FALSE, FALSE, NULL );
    if( wmmeStreamData->bufferEvent == NULL )
    {
        result = paHostError;
        sPaHostError = GetLastError();
        goto error;
    }
    wmmeStreamData->bufferEventInited = 1;
#endif /* (PA_USE_TIMER_CALLBACK == 0) */
    /* ------------------ OUTPUT */
    wmmeStreamData->bytesPerUserOutputBuffer = stream->past_FramesPerUserBuffer * stream->past_NumOutputChannels * sizeof(short);
    wmmeStreamData->bytesPerHostOutputBuffer = wmmeStreamData->userBuffersPerHostBuffer * wmmeStreamData->bytesPerUserOutputBuffer;
    if( (stream->past_OutputDeviceID != paNoDevice) && (stream->past_NumOutputChannels > 0) )
    {
        result = PaHost_OpenOutputStream( stream );
        if( result < 0 ) goto error;
    }
    /* ------------------ INPUT */
    wmmeStreamData->bytesPerUserInputBuffer = stream->past_FramesPerUserBuffer * stream->past_NumInputChannels * sizeof(short);
    wmmeStreamData->bytesPerHostInputBuffer = wmmeStreamData->userBuffersPerHostBuffer * wmmeStreamData->bytesPerUserInputBuffer;
    if( (stream->past_InputDeviceID != paNoDevice) && (stream->past_NumInputChannels > 0) )
    {
        result = PaHost_OpenInputStream( stream );
        if( result < 0 ) goto error;
    }

    Pa_InitializeCpuUsageScalar( stream );

    return result;

error:
    PaHost_CloseStream( stream );
    return result;
}
/*************************************************************************/
PaError PaHost_StartOutput( internalPortAudioStream *stream )
{
    PaError          result = paNoError;
    MMRESULT         mmresult;
    int              i;
    PaWMMEStreamData *wmmeStreamData = PaHost_GetWMMEStreamData( stream );

    if( wmmeStreamData == NULL ) return paInternalError;

    if( stream->past_OutputDeviceID != paNoDevice )
    {
        if( (mmresult = waveOutPause( wmmeStreamData->hWaveOut )) != MMSYSERR_NOERROR )
        {
            result = paHostError;
            sPaHostError = mmresult;
            goto error;
        }
        for( i=0; i<wmmeStreamData->numHostBuffers; i++ )
        {
            ZeroMemory( wmmeStreamData->outputBuffers[i].lpData, wmmeStreamData->outputBuffers[i].dwBufferLength );
            mmresult = waveOutWrite( wmmeStreamData->hWaveOut, &wmmeStreamData->outputBuffers[i], sizeof(WAVEHDR) );
            if( mmresult != MMSYSERR_NOERROR )
            {
                result = paHostError;
                sPaHostError = mmresult;
                goto error;
            }
            stream->past_FrameCount += wmmeStreamData->framesPerHostBuffer;
        }
        wmmeStreamData->currentOutputBuffer = 0;
        if( (mmresult = waveOutRestart( wmmeStreamData->hWaveOut )) != MMSYSERR_NOERROR )
        {
            result = paHostError;
            sPaHostError = mmresult;
            goto error;
        }
    }

error:
    DBUG(("PaHost_StartOutput: wave returned mmresult = 0x%X.\n", mmresult));
    return result;
}
/*************************************************************************/
PaError PaHost_StartInput( internalPortAudioStream *internalStream )
{
    PaError          result = paNoError;
    MMRESULT         mmresult;
    int              i;
    PaWMMEStreamData *wmmeStreamData = PaHost_GetWMMEStreamData( internalStream );

    if( wmmeStreamData == NULL ) return paInternalError;

    if( internalStream->past_InputDeviceID != paNoDevice )
    {
        for( i=0; i<wmmeStreamData->numHostBuffers; i++ )
        {
            mmresult = waveInAddBuffer( wmmeStreamData->hWaveIn, &wmmeStreamData->inputBuffers[i], sizeof(WAVEHDR) );
            if( mmresult != MMSYSERR_NOERROR )
            {
                result = paHostError;
                sPaHostError = mmresult;
                goto error;
            }
        }
        wmmeStreamData->currentInputBuffer = 0;
        mmresult = waveInStart( wmmeStreamData->hWaveIn );
        DBUG(("Pa_StartStream: waveInStart returned = 0x%X.\n", mmresult));
        if( mmresult != MMSYSERR_NOERROR )
        {
            result = paHostError;
            sPaHostError = mmresult;
            goto error;
        }
    }

error:
    return result;
}
/*************************************************************************/
PaError PaHost_StartEngine( internalPortAudioStream *stream )
{
    PaError             result = paNoError;
    PaWMMEStreamData *wmmeStreamData = PaHost_GetWMMEStreamData( stream );
#if PA_USE_TIMER_CALLBACK
    int                 resolution;
    int                 bufsPerTimerCallback;
    int                 msecPerBuffer;
#endif /* PA_USE_TIMER_CALLBACK */

    if( wmmeStreamData == NULL ) return paInternalError;

    stream->past_StopSoon = 0;
    stream->past_StopNow = 0;
    stream->past_IsActive = 1;
    wmmeStreamData->framesPlayed = 0.0;
    wmmeStreamData->lastPosition = 0;
#if PA_TRACE_START_STOP
    AddTraceMessage( "PaHost_StartEngine: TimeSlice() returned ", result );
#endif
#if PA_USE_TIMER_CALLBACK
    /* Create timer that will wake us up so we can fill the DSound buffer. */
    bufsPerTimerCallback = wmmeStreamData->numHostBuffers/4;
    if( bufsPerTimerCallback < 1 ) bufsPerTimerCallback = 1;
    if( bufsPerTimerCallback < 1 ) bufsPerTimerCallback = 1;
    msecPerBuffer = (1000 * bufsPerTimerCallback *
                     wmmeStreamData->userBuffersPerHostBuffer *
                     internalStream->past_FramesPerUserBuffer ) / (int) internalStream->past_SampleRate;
    if( msecPerBuffer < 10 ) msecPerBuffer = 10;
    else if( msecPerBuffer > 100 ) msecPerBuffer = 100;
    resolution = msecPerBuffer/4;
    wmmeStreamData->timerID = timeSetEvent( msecPerBuffer, resolution,
                                         (LPTIMECALLBACK) Pa_TimerCallback,
                                         (DWORD) stream, TIME_PERIODIC );
    if( wmmeStreamData->timerID == 0 )
    {
        result = paHostError;
        sPaHostError = GetLastError();;
        goto error;
    }
#else /* PA_USE_TIMER_CALLBACK */
    ResetEvent( wmmeStreamData->abortEvent );
    /* Create thread that waits for audio buffers to be ready for processing. */
    wmmeStreamData->engineThread = CreateThread( 0, 0, WinMMPa_OutputThreadProc, stream, 0, &wmmeStreamData->engineThreadID );
    if( wmmeStreamData->engineThread == NULL )
    {
        result = paHostError;
        sPaHostError = GetLastError();;
        goto error;
    }
#if PA_TRACE_START_STOP
    AddTraceMessage( "PaHost_StartEngine: thread ", (int) wmmeStreamData->engineThread );
#endif
    /* I used to pass the thread which was failing. I now pass GetCurrentProcess().
     * This fix could improve latency for some applications. It could also result in CPU
     * starvation if the callback did too much processing.
     * I also added result checks, so we might see more failures at initialization.
     * Thanks to Alberto di Bene for spotting this.
     */

#if 0  /* dmazzoni: this seems to cause problems */
    if( !SetPriorityClass( GetCurrentProcess(), HIGH_PRIORITY_CLASS ) ) /* PLB20010816 */
    {
        result = paHostError;
        sPaHostError = GetLastError();;
        goto error;
    }
#endif

    if( !SetThreadPriority( wmmeStreamData->engineThread, THREAD_PRIORITY_HIGHEST ) )
    {
        result = paHostError;
        sPaHostError = GetLastError();;
        goto error;
    }
#endif

error:
    return result;
}
/*************************************************************************/
PaError PaHost_StopEngine( internalPortAudioStream *internalStream, int abort )
{
    int timeOut;
    PaWMMEStreamData *wmmeStreamData = PaHost_GetWMMEStreamData( internalStream );

    if( wmmeStreamData == NULL ) return paNoError;

    /* Tell background thread to stop generating more data and to let current data play out. */
    internalStream->past_StopSoon = 1;
    /* If aborting, tell background thread to stop NOW! */
    if( abort ) internalStream->past_StopNow = 1;

    /* Calculate timeOut longer than longest time it could take to play all buffers. */
    timeOut = (DWORD) (1500.0 * PaHost_GetTotalBufferFrames( internalStream ) / internalStream->past_SampleRate);
    if( timeOut < MIN_TIMEOUT_MSEC ) timeOut = MIN_TIMEOUT_MSEC;

#if PA_USE_TIMER_CALLBACK
    if( (internalStream->past_OutputDeviceID != paNoDevice) &&
            internalStream->past_IsActive &&
            (wmmeStreamData->timerID != 0) )
    {
        /* Wait for IsActive to drop. */
        while( (internalStream->past_IsActive) && (timeOut > 0) )
        {
            Sleep(10);
            timeOut -= 10;
        }
        timeKillEvent( wmmeStreamData->timerID );  /* Stop callback timer. */
        wmmeStreamData->timerID = 0;
    }
#else /* PA_USE_TIMER_CALLBACK */
#if PA_TRACE_START_STOP
    AddTraceMessage( "PaHost_StopEngine: thread ", (int) wmmeStreamData->engineThread );
#endif
    if( (internalStream->past_OutputDeviceID != paNoDevice) &&
            (internalStream->past_IsActive) &&
            (wmmeStreamData->engineThread != NULL) )
    {
        DWORD got;
        /* Tell background thread to stop generating more data and to let current data play out. */
        DBUG(("PaHost_StopEngine: waiting for background thread.\n"));
        got = WaitForSingleObject( wmmeStreamData->engineThread, timeOut );
        if( got == WAIT_TIMEOUT )
        {
            ERR_RPT(("PaHost_StopEngine: timed out while waiting for background thread to finish.\n"));
            return paTimedOut;
        }
        CloseHandle( wmmeStreamData->engineThread );
        wmmeStreamData->engineThread = NULL;
    }
#endif /* PA_USE_TIMER_CALLBACK */

    internalStream->past_IsActive = 0;
    return paNoError;
}
/*************************************************************************/
PaError PaHost_StopInput( internalPortAudioStream *stream, int abort )
{
    MMRESULT mmresult;
    PaWMMEStreamData *wmmeStreamData = PaHost_GetWMMEStreamData( stream );

    if( wmmeStreamData == NULL ) return paNoError; /* FIXME: why return paNoError? */
    (void) abort; /* unused parameter */

    if( wmmeStreamData->hWaveIn != NULL )
    {
        mmresult = waveInReset( wmmeStreamData->hWaveIn );
        if( mmresult != MMSYSERR_NOERROR )
        {
            sPaHostError = mmresult;
            return paHostError;
        }
    }
    return paNoError;
}
/*************************************************************************/
PaError PaHost_StopOutput( internalPortAudioStream *internalStream, int abort )
{
    MMRESULT mmresult;
    PaWMMEStreamData *wmmeStreamData = PaHost_GetWMMEStreamData( internalStream );

    if( wmmeStreamData == NULL ) return paNoError;    /* FIXME: why return paNoError? */
    (void) abort;   /* unused parameter */

#if PA_TRACE_START_STOP
    AddTraceMessage( "PaHost_StopOutput: hWaveOut ", (int) wmmeStreamData->hWaveOut );
#endif
    if( wmmeStreamData->hWaveOut != NULL )
    {
        mmresult = waveOutReset( wmmeStreamData->hWaveOut );
        if( mmresult != MMSYSERR_NOERROR )
        {
            sPaHostError = mmresult;
            return paHostError;
        }
    }
    return paNoError;
}
/*******************************************************************/
PaError PaHost_CloseStream( internalPortAudioStream *stream )
{
    int i;
    PaWMMEStreamData *wmmeStreamData = PaHost_GetWMMEStreamData( stream );

    if( stream == NULL ) return paBadStreamPtr;
    if( wmmeStreamData == NULL ) return paNoError;   /* FIXME: why return no error? */

#if PA_TRACE_START_STOP
    AddTraceMessage( "PaHost_CloseStream: hWaveOut ", (int) wmmeStreamData->hWaveOut );
#endif
    /* Free data and device for output. */
    if( wmmeStreamData->hWaveOut )
    {
        if( wmmeStreamData->outputBuffers )
        {
            for( i=0; i<wmmeStreamData->numHostBuffers; i++ )
            {
                waveOutUnprepareHeader( wmmeStreamData->hWaveOut, &wmmeStreamData->outputBuffers[i], sizeof(WAVEHDR) );
                PaHost_FreeTrackedMemory( wmmeStreamData->outputBuffers[i].lpData ); /* MEM */
            }
            PaHost_FreeTrackedMemory( wmmeStreamData->outputBuffers ); /* MEM */
        }
        waveOutClose( wmmeStreamData->hWaveOut );
    }
    /* Free data and device for input. */
    if( wmmeStreamData->hWaveIn )
    {
        if( wmmeStreamData->inputBuffers )
        {
            for( i=0; i<wmmeStreamData->numHostBuffers; i++ )
            {
                waveInUnprepareHeader( wmmeStreamData->hWaveIn, &wmmeStreamData->inputBuffers[i], sizeof(WAVEHDR) );
                PaHost_FreeTrackedMemory( wmmeStreamData->inputBuffers[i].lpData ); /* MEM */
            }
            PaHost_FreeTrackedMemory( wmmeStreamData->inputBuffers ); /* MEM */
        }
        waveInClose( wmmeStreamData->hWaveIn );
    }
#if (PA_USE_TIMER_CALLBACK == 0)
    if( wmmeStreamData->abortEventInited ) CloseHandle( wmmeStreamData->abortEvent );
    if( wmmeStreamData->bufferEventInited ) CloseHandle( wmmeStreamData->bufferEvent );
#endif
    if( wmmeStreamData->streamLockInited )
        DeleteCriticalSection( &wmmeStreamData->streamLock );

    PaHost_FreeWMMEStreamData( stream );

    return paNoError;
}
/*************************************************************************
 * Determine minimum number of buffers required for this host based
 * on minimum latency. Latency can be optionally set by user by setting
 * an environment variable. For example, to set latency to 200 msec, put:
 *
 *    set PA_MIN_LATENCY_MSEC=200
 *
 * in the AUTOEXEC.BAT file and reboot.
 * If the environment variable is not set, then the latency will be determined
 * based on the OS. Windows NT has higher latency than Win95.
 */
#define PA_LATENCY_ENV_NAME  ("PA_MIN_LATENCY_MSEC")
int Pa_GetMinNumBuffers( int framesPerBuffer, double sampleRate )
{
    char      envbuf[PA_ENV_BUF_SIZE];
    DWORD     hresult;
    int       minLatencyMsec = 0;
    double    msecPerBuffer = (1000.0 * framesPerBuffer) / sampleRate;
    int       minBuffers;

    /* Let user determine minimal latency by setting environment variable. */
    hresult = GetEnvironmentVariable( PA_LATENCY_ENV_NAME, envbuf, PA_ENV_BUF_SIZE );
    if( (hresult > 0) && (hresult < PA_ENV_BUF_SIZE) )
    {
        minLatencyMsec = atoi( envbuf );   /* REVIEW: will we crash if the environment variable contains some nasty value? */
    }
    else
    {
        /* Set minimal latency based on whether NT or other OS.
         * NT has higher latency.
         */
        OSVERSIONINFO osvi;
		osvi.dwOSVersionInfoSize = sizeof( osvi );
		GetVersionEx( &osvi );
        DBUG(("PA - PlatformId = 0x%x\n", osvi.dwPlatformId ));
        DBUG(("PA - MajorVersion = 0x%x\n", osvi.dwMajorVersion ));
        DBUG(("PA - MinorVersion = 0x%x\n", osvi.dwMinorVersion ));
        /* Check for NT */
		if( (osvi.dwMajorVersion == 4) && (osvi.dwPlatformId == 2) )
		{
			minLatencyMsec = PA_WIN_NT_LATENCY;
		}
		else if(osvi.dwMajorVersion >= 5)
		{
			minLatencyMsec = PA_WIN_WDM_LATENCY;
		}
		else
		{
			minLatencyMsec = PA_WIN_9X_LATENCY;
		}
#if PA_USE_HIGH_LATENCY
        PRINT(("PA - Minimum Latency set to %d msec!\n", minLatencyMsec ));
#endif

    }
    DBUG(("PA - Minimum Latency set to %d msec!\n", minLatencyMsec ));
    minBuffers = (int) (1.0 + ((double)minLatencyMsec / msecPerBuffer));
    if( minBuffers < 2 ) minBuffers = 2;
    return minBuffers;
}
/*************************************************************************
 * Cleanup device info.
 */
PaError PaHost_Term( void )
{
    int i;

    if( sNumDevices > 0 )
    {
        if( sDevicePtrs != NULL )
        {
            for( i=0; i<sNumDevices; i++ )
            {
                if( sDevicePtrs[i] != NULL )
                {
                    PaHost_FreeTrackedMemory( (char*)sDevicePtrs[i]->name ); /* MEM */
                    PaHost_FreeTrackedMemory( (void*)sDevicePtrs[i]->sampleRates ); /* MEM */
                    PaHost_FreeTrackedMemory( sDevicePtrs[i] ); /* MEM */
                }
            }
            PaHost_FreeTrackedMemory( sDevicePtrs ); /* MEM */
            sDevicePtrs = NULL;
        }
        sNumDevices = 0;
    }

#if PA_TRACK_MEMORY
    PRINT(("PaHost_Term: sNumAllocations = %d\n", sNumAllocations ));
#endif

    return paNoError;
}
/*************************************************************************/
void Pa_Sleep( long msec )
{
    Sleep( msec );
}
/*************************************************************************
FIXME: the following memory allocation routines should not be declared here
 * Allocate memory that can be accessed in real-time.
 * This may need to be held in physical memory so that it is not
 * paged to virtual memory.
 * This call MUST be balanced with a call to PaHost_FreeFastMemory().
 * Memory will be set to zero.
 */
void *PaHost_AllocateFastMemory( long numBytes )
{
    return PaHost_AllocateTrackedMemory( numBytes ); /* FIXME - do we need physical memory? Use VirtualLock() */ /* MEM */
}
/*************************************************************************
 * Free memory that could be accessed in real-time.
 * This call MUST be balanced with a call to PaHost_AllocateFastMemory().
 */
void PaHost_FreeFastMemory( void *addr, long numBytes )
{
    (void) numBytes; /* unused parameter */
    
    PaHost_FreeTrackedMemory( addr ); /* MEM */
}

/*************************************************************************
 * Track memory allocations to avoid leaks.
 */
static void *PaHost_AllocateTrackedMemory( long numBytes )
{
    void *result = GlobalAlloc( GPTR, numBytes ); /* MEM */

#if PA_TRACK_MEMORY
    if( result != NULL ) sNumAllocations += 1;
#endif
    return result;
}

static void PaHost_FreeTrackedMemory( void *addr )
{
    if( addr != NULL )
    {
        GlobalFree( addr ); /* MEM */
#if PA_TRACK_MEMORY
        sNumAllocations -= 1;
#endif
    }
}

/***********************************************************************/
PaError PaHost_StreamActive( internalPortAudioStream *internalStream )
{
    if( internalStream == NULL ) return paBadStreamPtr;

    return (PaError) internalStream->past_IsActive;
}
/*************************************************************************
 * This must be called periodically because mmtime.u.sample
 * is a DWORD and can wrap and lose sync after a few hours.
 */
static PaError PaHost_UpdateStreamTime( PaWMMEStreamData *wmmeStreamData )
{
    MMRESULT  mmresult;
    MMTIME    mmtime;
    const int shift = 6;
    mmtime.wType = TIME_SAMPLES;

    if( wmmeStreamData->hWaveOut != NULL )
    {
        mmresult = waveOutGetPosition( wmmeStreamData->hWaveOut, &mmtime, sizeof(mmtime) );
    }
    else
    {
        mmresult = waveInGetPosition( wmmeStreamData->hWaveIn, &mmtime, sizeof(mmtime) );
    }
    
    if( mmresult != MMSYSERR_NOERROR )
    {
        sPaHostError = mmresult;
        return paHostError;
    }
    
    /* This data has two variables and is shared by foreground and background.
     * So we need to make it thread safe. */
    EnterCriticalSection( &wmmeStreamData->streamLock );

// The casting and shifting in computing frames played is
// to deal with wrap-around.  
//    mmtime.u.sample will 
// wrap at 27 bits, for typical sound cards under windows.
// The shifting and casting is to ensure we get the right sign
// bits. 
   wmmeStreamData->framesPlayed += 
      ((long)((((DWORD)mmtime.u.sample)<<shift) - 
      (((DWORD)(wmmeStreamData->lastPosition))<<shift)))>>shift;
// First attempted fix.
//    wmmeStreamData->framesPlayed += (long)((DWORD)mmtime.u.sample - (DWORD)(wmmeStreamData->lastPosition));
// Original code:
//    wmmeStreamData->framesPlayed += ((long)mmtime.u.sample) - wmmeStreamData->lastPosition;

    wmmeStreamData->lastPosition = (long)mmtime.u.sample;
    LeaveCriticalSection( &wmmeStreamData->streamLock );
    
    return paNoError;
}
/*************************************************************************/
PaTimestamp Pa_StreamTime( PortAudioStream *stream )
{
    internalPortAudioStream *internalStream = PaHost_GetStreamRepresentation( stream );
    PaWMMEStreamData *wmmeStreamData = PaHost_GetWMMEStreamData( internalStream );

    if( internalStream == NULL ) return paBadStreamPtr;
    if( wmmeStreamData == NULL ) return paInternalError;

    PaHost_UpdateStreamTime( wmmeStreamData );
    return wmmeStreamData->framesPlayed;
}
/*************************************************************************/



