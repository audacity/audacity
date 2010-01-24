/*
 * $Id: pa_dsound.c,v 1.3 2003-03-02 08:01:45 dmazzoni Exp $
 * PortAudio Portable Real-Time Audio Library
 * Latest Version at: http://www.softsynth.com/portaudio/
 * DirectSound Implementation
 *
 * Copyright (c) 1999-2000 Phil Burk
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
/* Modifications
 *    7/19/01 Mike Berry - casts for compiling with __MWERKS__ CodeWarrior
 *    9/27/01 Phil Burk - use number of frames instead of real-time for CPULoad calculation.
 *    4/19/02 Phil Burk - Check for Win XP for system latency calculation.
 */
/* Compiler flags:
 SUPPORT_AUDIO_CAPTURE - define this flag if you want to SUPPORT_AUDIO_CAPTURE
 */

#include <stdio.h>
#include <stdlib.h>
#ifndef __MWERKS__
#include <malloc.h>
#include <memory.h>
#endif //__MWERKS__
#include <math.h>
#include "portaudio.h"
#include "pa_host.h"
#include "pa_trace.h"
#include "dsound_wrapper.h"

#define PRINT(x) { printf x; fflush(stdout); }
#define ERR_RPT(x) PRINT(x)
#define DBUG(x)  /* PRINT(x) */
#define DBUGX(x) /* PRINT(x) */

#define PA_USE_HIGH_LATENCY   (0)
#if PA_USE_HIGH_LATENCY
#define PA_WIN_9X_LATENCY     (500)
#define PA_WIN_NT_LATENCY     (600)
#else
#define PA_WIN_9X_LATENCY     (140)
#define PA_WIN_NT_LATENCY     (280)
#endif

#define PA_WIN_WDM_LATENCY       (120)

/* Trigger an underflow for testing purposes. Should normally be (0). */
#define PA_SIMULATE_UNDERFLOW (0)
#if PA_SIMULATE_UNDERFLOW
static  gUnderCallbackCounter = 0;
#define UNDER_START_GAP       (10)
#define UNDER_STOP_GAP        (UNDER_START_GAP + 4)
#endif

/************************************************* Definitions ********/
typedef struct internalPortAudioStream internalPortAudioStream;
typedef struct internalPortAudioDevice
{
    GUID                             pad_GUID;
    GUID                            *pad_lpGUID;
    double                           pad_SampleRates[10]; /* for pointing to from pad_Info FIXME?!*/
    PaDeviceInfo                     pad_Info;
}
internalPortAudioDevice;

/* Define structure to contain all DirectSound and Windows specific data. */
typedef struct PaHostSoundControl
{
    DSoundWrapper    pahsc_DSoundWrapper;
    MMRESULT         pahsc_TimerID;
    BOOL             pahsc_IfInsideCallback;  /* Test for reentrancy. */
    short           *pahsc_NativeBuffer;
    unsigned int     pahsc_BytesPerBuffer;    /* native buffer size in bytes */
    double           pahsc_ValidFramesWritten;
    int              pahsc_FramesPerDSBuffer;
    /* For measuring CPU utilization. */
    LARGE_INTEGER    pahsc_EntryCount;
    double           pahsc_InverseTicksPerUserBuffer;
}
PaHostSoundControl;

/************************************************* Shared Data ********/
/* FIXME - put Mutex around this shared data. */
static int sNumDevices = 0;
static int sDeviceIndex = 0;
static internalPortAudioDevice *sDevices = NULL;
static int sDefaultInputDeviceID = paNoDevice;
static int sDefaultOutputDeviceID = paNoDevice;
static int sEnumerationError;
static int sPaHostError = 0;
/************************************************* Prototypes **********/
static internalPortAudioDevice *Pa_GetInternalDevice( PaDeviceID id );
static BOOL CALLBACK Pa_EnumProc(LPGUID lpGUID,
                                 LPCTSTR lpszDesc,
                                 LPCTSTR lpszDrvName,
                                 LPVOID lpContext );
static BOOL CALLBACK Pa_CountDevProc(LPGUID lpGUID,
                                     LPCTSTR lpszDesc,
                                     LPCTSTR lpszDrvName,
                                     LPVOID lpContext );
static Pa_QueryDevices( void );
static void CALLBACK Pa_TimerCallback(UINT uID, UINT uMsg,
                                      DWORD dwUser, DWORD dw1, DWORD dw2);

/********************************* BEGIN CPU UTILIZATION MEASUREMENT ****/
static void Pa_StartUsageCalculation( internalPortAudioStream   *past )
{
    PaHostSoundControl *pahsc = (PaHostSoundControl *) past->past_DeviceData;
    if( pahsc == NULL ) return;
    /* Query system timer for usage analysis and to prevent overuse of CPU. */
    QueryPerformanceCounter( &pahsc->pahsc_EntryCount );
}

static void Pa_EndUsageCalculation( internalPortAudioStream   *past )
{
    LARGE_INTEGER CurrentCount = { 0, 0 };
    PaHostSoundControl *pahsc = (PaHostSoundControl *) past->past_DeviceData;
    if( pahsc == NULL ) return;
    /*
    ** Measure CPU utilization during this callback. Note that this calculation
    ** assumes that we had the processor the whole time.
    */
#define LOWPASS_COEFFICIENT_0   (0.9)
#define LOWPASS_COEFFICIENT_1   (0.99999 - LOWPASS_COEFFICIENT_0)
    if( QueryPerformanceCounter( &CurrentCount ) )
    {
        LONGLONG InsideCount = CurrentCount.QuadPart - pahsc->pahsc_EntryCount.QuadPart;
        double newUsage = InsideCount * pahsc->pahsc_InverseTicksPerUserBuffer;
        past->past_Usage = (LOWPASS_COEFFICIENT_0 * past->past_Usage) +
                           (LOWPASS_COEFFICIENT_1 * newUsage);
    }
}

/****************************************** END CPU UTILIZATION *******/
static PaError Pa_QueryDevices( void )
{
    int numBytes;
    sDefaultInputDeviceID = paNoDevice;
    sDefaultOutputDeviceID = paNoDevice;
    /* Enumerate once just to count devices. */
    sNumDevices = 0; // for default device
    DirectSoundEnumerate( (LPDSENUMCALLBACK)Pa_CountDevProc, NULL );
#if SUPPORT_AUDIO_CAPTURE
    DirectSoundCaptureEnumerate( (LPDSENUMCALLBACK)Pa_CountDevProc, NULL );
#endif /* SUPPORT_AUDIO_CAPTURE */
    /* Allocate structures to hold device info. */
    numBytes = sNumDevices * sizeof(internalPortAudioDevice);
    sDevices = (internalPortAudioDevice *)PaHost_AllocateFastMemory( numBytes ); /* MEM */
    if( sDevices == NULL ) return paInsufficientMemory;
    /* Enumerate again to fill in structures. */
    sDeviceIndex = 0;
    sEnumerationError = 0;
    DirectSoundEnumerate( (LPDSENUMCALLBACK)Pa_EnumProc, (void *)0 );
#if SUPPORT_AUDIO_CAPTURE
    if( sEnumerationError != paNoError ) return sEnumerationError;
    sEnumerationError = 0;
    DirectSoundCaptureEnumerate( (LPDSENUMCALLBACK)Pa_EnumProc, (void *)1 );
#endif /* SUPPORT_AUDIO_CAPTURE */
    return sEnumerationError;
}
/************************************************************************************/
long Pa_GetHostError()
{
    return sPaHostError;
}
/************************************************************************************
** Just count devices so we know how much memory to allocate.
*/
static BOOL CALLBACK Pa_CountDevProc(LPGUID lpGUID,
                                     LPCTSTR lpszDesc,
                                     LPCTSTR lpszDrvName,
                                     LPVOID lpContext )
{
    sNumDevices++;
    return TRUE;
}
/************************************************************************************
** Extract capabilities info from each device.
*/
static BOOL CALLBACK Pa_EnumProc(LPGUID lpGUID,
                                 LPCTSTR lpszDesc,
                                 LPCTSTR lpszDrvName,
                                 LPVOID lpContext )
{
    HRESULT    hr;
    LPDIRECTSOUND          lpDirectSound;
#if SUPPORT_AUDIO_CAPTURE
    LPDIRECTSOUNDCAPTURE   lpDirectSoundCapture;
#endif /* SUPPORT_AUDIO_CAPTURE */
    int        isInput  = (int) lpContext;  /* Passed from Pa_CountDevices() */
    internalPortAudioDevice *pad;

    if( sDeviceIndex >= sNumDevices )
    {
        sEnumerationError = paInternalError;
        return FALSE;
    }
    pad = &sDevices[sDeviceIndex];
    /* Copy GUID to static array. Set pointer. */
    if( lpGUID == NULL )
    {
        pad->pad_lpGUID = NULL;
    }
    else
    {
        memcpy( &pad->pad_GUID, lpGUID, sizeof(GUID) );
        pad->pad_lpGUID = &pad->pad_GUID;
    }
    pad->pad_Info.sampleRates = pad->pad_SampleRates;  /* Point to array. */
    /* Allocate room for descriptive name. */
    if( lpszDesc != NULL )
    {
        int len = strlen(lpszDesc);
        pad->pad_Info.name = (char *)malloc( len+1 );
        if( pad->pad_Info.name == NULL )
        {
            sEnumerationError = paInsufficientMemory;
            return FALSE;
        }
        memcpy( (void *) pad->pad_Info.name, lpszDesc, len+1 );
    }
#if SUPPORT_AUDIO_CAPTURE
    if( isInput )
    {
        /********** Input ******************************/
        DSCCAPS     caps;
        if( lpGUID == NULL ) sDefaultInputDeviceID = sDeviceIndex;
        hr = DirectSoundCaptureCreate(  lpGUID, &lpDirectSoundCapture,   NULL );
        if( hr != DS_OK )
        {
            pad->pad_Info.maxInputChannels = 0;
            DBUG(("Cannot create Capture for %s. Result = 0x%x\n", lpszDesc, hr ));
        }
        else
        {
            /* Query device characteristics. */
            caps.dwSize = sizeof(caps);
            IDirectSoundCapture_GetCaps( lpDirectSoundCapture, &caps );
            /* printf("caps.dwFormats = 0x%x\n", caps.dwFormats ); */
            pad->pad_Info.maxInputChannels = caps.dwChannels;
            /* Determine sample rates from flags. */
            if( caps.dwChannels == 2 )
            {
                int index = 0;
                if( caps.dwFormats & WAVE_FORMAT_1S16) pad->pad_SampleRates[index++] = 11025.0;
                if( caps.dwFormats & WAVE_FORMAT_2S16) pad->pad_SampleRates[index++] = 22050.0;
                if( caps.dwFormats & WAVE_FORMAT_4S16) pad->pad_SampleRates[index++] = 44100.0;
                pad->pad_Info.numSampleRates = index;
            }
            else if( caps.dwChannels == 1 )
            {
                int index = 0;
                if( caps.dwFormats & WAVE_FORMAT_1M16) pad->pad_SampleRates[index++] = 11025.0;
                if( caps.dwFormats & WAVE_FORMAT_2M16) pad->pad_SampleRates[index++] = 22050.0;
                if( caps.dwFormats & WAVE_FORMAT_4M16) pad->pad_SampleRates[index++] = 44100.0;
                pad->pad_Info.numSampleRates = index;
            }
            else pad->pad_Info.numSampleRates = 0;
            IDirectSoundCapture_Release( lpDirectSoundCapture );
        }
    }
    else
#endif /* SUPPORT_AUDIO_CAPTURE */

    {
        /********** Output ******************************/
        DSCAPS     caps;
        if( lpGUID == NULL ) sDefaultOutputDeviceID = sDeviceIndex;
        /* Create interfaces for each object. */
        hr = DirectSoundCreate(  lpGUID, &lpDirectSound,   NULL );
        if( hr != DS_OK )
        {
            pad->pad_Info.maxOutputChannels = 0;
            DBUG(("Cannot create dsound for %s. Result = 0x%x\n", lpszDesc, hr ));
        }
        else
        {
            /* Query device characteristics. */
            caps.dwSize = sizeof(caps);
            IDirectSound_GetCaps( lpDirectSound, &caps );
            pad->pad_Info.maxOutputChannels = ( caps.dwFlags & DSCAPS_PRIMARYSTEREO ) ? 2 : 1;
            /* Get sample rates. */
            pad->pad_SampleRates[0] = (double) caps.dwMinSecondarySampleRate;
            pad->pad_SampleRates[1] = (double) caps.dwMaxSecondarySampleRate;
            if( caps.dwFlags & DSCAPS_CONTINUOUSRATE ) pad->pad_Info.numSampleRates = -1;
            else if( caps.dwMinSecondarySampleRate == caps.dwMaxSecondarySampleRate )
            {
                if( caps.dwMinSecondarySampleRate == 0 )
                {
                    /*
                    ** On my Thinkpad 380Z, DirectSoundV6 returns min-max=0 !!
                    ** But it supports continuous sampling.
                    ** So fake range of rates, and hope it really supports it.
                    */
                    pad->pad_SampleRates[0] = 11025.0f;
                    pad->pad_SampleRates[1] = 48000.0f;
                    pad->pad_Info.numSampleRates = -1; /* continuous range */

                    DBUG(("PA - Reported rates both zero. Setting to fake values for device #%d\n", sDeviceIndex ));
                }
                else
                {
                    pad->pad_Info.numSampleRates = 1;
                }
            }
            else if( (caps.dwMinSecondarySampleRate < 1000.0) && (caps.dwMaxSecondarySampleRate > 50000.0) )
            {
                /* The EWS88MT drivers lie, lie, lie. The say they only support two rates, 100 & 100000.
                ** But we know that they really support a range of rates!
                ** So when we see a ridiculous set of rates, assume it is a range.
                */
                pad->pad_Info.numSampleRates = -1;
                DBUG(("PA - Sample rate range used instead of two odd values for device #%d\n", sDeviceIndex ));
            }
            else pad->pad_Info.numSampleRates = 2;
            IDirectSound_Release( lpDirectSound );
        }
    }
    pad->pad_Info.nativeSampleFormats = paInt16;
    sDeviceIndex++;
    return( TRUE );
}
/*************************************************************************/
int Pa_CountDevices()
{
    if( sNumDevices <= 0 ) Pa_Initialize();
    return sNumDevices;
}
static internalPortAudioDevice *Pa_GetInternalDevice( PaDeviceID id )
{
    if( (id < 0) || ( id >= Pa_CountDevices()) ) return NULL;
    return &sDevices[id];
}
/*************************************************************************/
const PaDeviceInfo* Pa_GetDeviceInfo( PaDeviceID id )
{
    internalPortAudioDevice *pad;
    if( (id < 0) || ( id >= Pa_CountDevices()) ) return NULL;
    pad = Pa_GetInternalDevice( id );
    return  &pad->pad_Info ;
}
static PaError Pa_MaybeQueryDevices( void )
{
    if( sNumDevices == 0 )
    {
        return Pa_QueryDevices();
    }
    return 0;
}
/*************************************************************************
** Returns recommended device ID.
** On the PC, the recommended device can be specified by the user by
** setting an environment variable. For example, to use device #1.
**
**    set PA_RECOMMENDED_OUTPUT_DEVICE=1
**
** The user should first determine the available device ID by using
** the supplied application "pa_devs".
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
PaDeviceID Pa_GetDefaultInputDeviceID( void )
{
    PaError result;
    result = PaHost_GetEnvDefaultDeviceID( PA_REC_IN_DEV_ENV_NAME );
    if( result < 0 )
    {
        result = Pa_MaybeQueryDevices();
        if( result < 0 ) return result;
        result = sDefaultInputDeviceID;
    }
    return result;
}
PaDeviceID Pa_GetDefaultOutputDeviceID( void )
{
    PaError result;
    result = PaHost_GetEnvDefaultDeviceID( PA_REC_OUT_DEV_ENV_NAME );
    if( result < 0 )
    {
        result = Pa_MaybeQueryDevices();
        if( result < 0 ) return result;
        result = sDefaultOutputDeviceID;
    }
    return result;
}
/**********************************************************************
** Make sure that we have queried the device capabilities.
*/
PaError PaHost_Init( void )
{
#if PA_SIMULATE_UNDERFLOW
    PRINT(("WARNING - Underflow Simulation Enabled - Expect a Big Glitch!!!\n"));
#endif
    return Pa_MaybeQueryDevices();
}
static PaError Pa_TimeSlice( internalPortAudioStream   *past )
{
    PaError           result = 0;
    long              bytesEmpty = 0;
    long              bytesFilled = 0;
    long              bytesToXfer = 0;
    long              numChunks;
    HRESULT           hresult;
    PaHostSoundControl  *pahsc;
    short            *nativeBufPtr;
    past->past_NumCallbacks += 1;
    pahsc = (PaHostSoundControl *) past->past_DeviceData;
    if( pahsc == NULL ) return paInternalError;
    /* How much input data is available? */
#if SUPPORT_AUDIO_CAPTURE
    if( past->past_NumInputChannels > 0 )
    {
        DSW_QueryInputFilled( &pahsc->pahsc_DSoundWrapper, &bytesFilled );
        bytesToXfer = bytesFilled;
    }
#endif /* SUPPORT_AUDIO_CAPTURE */
    /* How much output room is available? */
    if( past->past_NumOutputChannels > 0 )
    {
        DSW_QueryOutputSpace( &pahsc->pahsc_DSoundWrapper, &bytesEmpty );
        bytesToXfer = bytesEmpty;
    }
    AddTraceMessage( "bytesEmpty ", bytesEmpty );
    /* Choose smallest value if both are active. */
    if( (past->past_NumInputChannels > 0) && (past->past_NumOutputChannels > 0) )
    {
        bytesToXfer = ( bytesFilled < bytesEmpty ) ? bytesFilled : bytesEmpty;
    }
    /* printf("bytesFilled = %d, bytesEmpty = %d, bytesToXfer = %d\n",
      bytesFilled, bytesEmpty, bytesToXfer);
    */
    /* Quantize to multiples of a buffer. */
    numChunks = bytesToXfer / pahsc->pahsc_BytesPerBuffer;
    if( numChunks > (long)(past->past_NumUserBuffers/2) )
    {
        numChunks = (long)past->past_NumUserBuffers/2;
    }
    else if( numChunks < 0 )
    {
        numChunks = 0;
    }
    AddTraceMessage( "numChunks ", numChunks );
    nativeBufPtr = pahsc->pahsc_NativeBuffer;
    if( numChunks > 0 )
    {
        while( numChunks-- > 0 )
        {
            /* Measure usage based on time to process one user buffer. */
            Pa_StartUsageCalculation( past );
#if SUPPORT_AUDIO_CAPTURE
            /* Get native data from DirectSound. */
            if( past->past_NumInputChannels > 0 )
            {
                hresult = DSW_ReadBlock( &pahsc->pahsc_DSoundWrapper, (char *) nativeBufPtr, pahsc->pahsc_BytesPerBuffer );
                if( hresult < 0 )
                {
                    ERR_RPT(("DirectSound ReadBlock failed, hresult = 0x%x\n",hresult));
                    sPaHostError = hresult;
                    break;
                }
            }
#endif /* SUPPORT_AUDIO_CAPTURE */
            /* Convert 16 bit native data to user data and call user routine. */
            result = Pa_CallConvertInt16( past, nativeBufPtr, nativeBufPtr );
            if( result != 0) break;
            /* Pass native data to DirectSound. */
            if( past->past_NumOutputChannels > 0 )
            {
                /* static short DEBUGHACK = 0;
                 DEBUGHACK += 0x0049;
                 nativeBufPtr[0] = DEBUGHACK; /* Make buzz to see if DirectSound still running. */
                hresult = DSW_WriteBlock( &pahsc->pahsc_DSoundWrapper, (char *) nativeBufPtr, pahsc->pahsc_BytesPerBuffer );
                if( hresult < 0 )
                {
                    ERR_RPT(("DirectSound WriteBlock failed, result = 0x%x\n",hresult));
                    sPaHostError = hresult;
                    break;
                }
            }
            Pa_EndUsageCalculation( past );
        }
    }
    return result;
}
/*******************************************************************/
static void CALLBACK Pa_TimerCallback(UINT uID, UINT uMsg, DWORD dwUser, DWORD dw1, DWORD dw2)
{
    internalPortAudioStream   *past;
    PaHostSoundControl  *pahsc;
#if PA_SIMULATE_UNDERFLOW
    gUnderCallbackCounter++;
    if( (gUnderCallbackCounter >= UNDER_START_GAP) &&
            (gUnderCallbackCounter <= UNDER_STOP_GAP) )
    {
        if( gUnderCallbackCounter == UNDER_START_GAP)
        {
            AddTraceMessage("Begin stall: gUnderCallbackCounter =======", gUnderCallbackCounter );
        }
        if( gUnderCallbackCounter == UNDER_STOP_GAP)
        {
            AddTraceMessage("End stall: gUnderCallbackCounter =======", gUnderCallbackCounter );
        }
        return;
    }
#endif
    past = (internalPortAudioStream *) dwUser;
    if( past == NULL ) return;
    pahsc = (PaHostSoundControl *) past->past_DeviceData;
    if( pahsc == NULL ) return;
    if( !pahsc->pahsc_IfInsideCallback && past->past_IsActive )
    {
        if( past->past_StopNow )
        {
            past->past_IsActive = 0;
        }
        else if( past->past_StopSoon )
        {
            DSoundWrapper   *dsw = &pahsc->pahsc_DSoundWrapper;
            if( past->past_NumOutputChannels > 0 )
            {
                DSW_ZeroEmptySpace( dsw );
                AddTraceMessage("Pa_TimerCallback: waiting - written ", (int) dsw->dsw_FramesWritten );
                AddTraceMessage("Pa_TimerCallback: waiting - played ", (int) dsw->dsw_FramesPlayed );
                /* clear past_IsActive when all sound played */
                if( dsw->dsw_FramesPlayed >= past->past_FrameCount )
                {
                    past->past_IsActive = 0;
                }
            }
            else
            {
                past->past_IsActive = 0;
            }
        }
        else
        {
            pahsc->pahsc_IfInsideCallback = 1;
            if( Pa_TimeSlice( past ) != 0)  /* Call time slice independant of timing method. */
            {
                past->past_StopSoon = 1;
            }
            pahsc->pahsc_IfInsideCallback = 0;
        }
    }
}
/*******************************************************************/
PaError PaHost_OpenStream( internalPortAudioStream   *past )
{
    HRESULT          hr;
    PaError          result = paNoError;
    PaHostSoundControl *pahsc;
    int              numBytes, maxChannels;
    unsigned int     minNumBuffers;
    internalPortAudioDevice *pad;
    DSoundWrapper   *dsw;
    /* Allocate and initialize host data. */
    pahsc = (PaHostSoundControl *) PaHost_AllocateFastMemory(sizeof(PaHostSoundControl)); /* MEM */
    if( pahsc == NULL )
    {
        result = paInsufficientMemory;
        goto error;
    }
    memset( pahsc, 0, sizeof(PaHostSoundControl) );
    past->past_DeviceData = (void *) pahsc;
    pahsc->pahsc_TimerID = 0;
    dsw = &pahsc->pahsc_DSoundWrapper;
    DSW_Init( dsw );
    /* Allocate native buffer. */
    maxChannels = ( past->past_NumOutputChannels > past->past_NumInputChannels ) ?
                  past->past_NumOutputChannels : past->past_NumInputChannels;
    pahsc->pahsc_BytesPerBuffer = past->past_FramesPerUserBuffer * maxChannels * sizeof(short);
    if( maxChannels > 0 )
    {
        pahsc->pahsc_NativeBuffer = (short *) PaHost_AllocateFastMemory(pahsc->pahsc_BytesPerBuffer); /* MEM */
        if( pahsc->pahsc_NativeBuffer == NULL )
        {
            result = paInsufficientMemory;
            goto error;
        }
    }
    else
    {
        result = paInvalidChannelCount;
        goto error;
    }

    DBUG(("PaHost_OpenStream: pahsc_MinFramesPerHostBuffer = %d\n", pahsc->pahsc_MinFramesPerHostBuffer ));
    minNumBuffers = Pa_GetMinNumBuffers( past->past_FramesPerUserBuffer, past->past_SampleRate );
    past->past_NumUserBuffers = ( minNumBuffers > past->past_NumUserBuffers ) ? minNumBuffers : past->past_NumUserBuffers;
    numBytes = pahsc->pahsc_BytesPerBuffer * past->past_NumUserBuffers;
    if( numBytes < DSBSIZE_MIN )
    {
        result = paBufferTooSmall;
        goto error;
    }
    if( numBytes > DSBSIZE_MAX )
    {
        result = paBufferTooBig;
        goto error;
    }
    pahsc->pahsc_FramesPerDSBuffer = past->past_FramesPerUserBuffer * past->past_NumUserBuffers;
    {
        int msecLatency = (int) ((pahsc->pahsc_FramesPerDSBuffer * 1000) / past->past_SampleRate);
        PRINT(("PortAudio on DirectSound - Latency = %d frames, %d msec\n", pahsc->pahsc_FramesPerDSBuffer, msecLatency ));
    }
    /* ------------------ OUTPUT */
    if( (past->past_OutputDeviceID >= 0) && (past->past_NumOutputChannels > 0) )
    {
        DBUG(("PaHost_OpenStream: deviceID = 0x%x\n", past->past_OutputDeviceID));
        pad = Pa_GetInternalDevice( past->past_OutputDeviceID );
        hr = DirectSoundCreate( pad->pad_lpGUID, &dsw->dsw_pDirectSound,   NULL );
        /* If this fails, then try each output device until we find one that works. */
        if( hr != DS_OK )
        {
            int i;
            ERR_RPT(("Creation of requested Audio Output device '%s' failed.\n",
                     ((pad->pad_lpGUID == NULL) ? "Default" : pad->pad_Info.name) ));
            for( i=0; i<Pa_CountDevices(); i++ )
            {
                pad = Pa_GetInternalDevice( i );
                if( pad->pad_Info.maxOutputChannels >= past->past_NumOutputChannels )
                {
                    DBUG(("Try device '%s' instead.\n", pad->pad_Info.name ));
                    hr = DirectSoundCreate( pad->pad_lpGUID, &dsw->dsw_pDirectSound,   NULL );
                    if( hr == DS_OK )
                    {
                        ERR_RPT(("Using device '%s' instead.\n", pad->pad_Info.name ));
                        break;
                    }
                }
            }
        }
        if( hr != DS_OK )
        {
            ERR_RPT(("PortAudio: DirectSoundCreate() failed!\n"));
            result = paHostError;
            sPaHostError = hr;
            goto error;
        }
        hr = DSW_InitOutputBuffer( dsw,
                                   (unsigned long) (past->past_SampleRate + 0.5),
                                   past->past_NumOutputChannels, numBytes );
        DBUG(("DSW_InitOutputBuffer() returns %x\n", hr));
        if( hr != DS_OK )
        {
            result = paHostError;
            sPaHostError = hr;
            goto error;
        }
        past->past_FrameCount = pahsc->pahsc_DSoundWrapper.dsw_FramesWritten;
    }
#if SUPPORT_AUDIO_CAPTURE
    /* ------------------ INPUT */
    if( (past->past_InputDeviceID >= 0) && (past->past_NumInputChannels > 0) )
    {
        pad = Pa_GetInternalDevice( past->past_InputDeviceID );
        hr = DirectSoundCaptureCreate( pad->pad_lpGUID, &dsw->dsw_pDirectSoundCapture,   NULL );
        /* If this fails, then try each input device until we find one that works. */
        if( hr != DS_OK )
        {
            int i;
            ERR_RPT(("Creation of requested Audio Capture device '%s' failed.\n",
                     ((pad->pad_lpGUID == NULL) ? "Default" : pad->pad_Info.name) ));
            for( i=0; i<Pa_CountDevices(); i++ )
            {
                pad = Pa_GetInternalDevice( i );
                if( pad->pad_Info.maxInputChannels >= past->past_NumInputChannels )
                {
                    PRINT(("Try device '%s' instead.\n", pad->pad_Info.name ));
                    hr = DirectSoundCaptureCreate( pad->pad_lpGUID, &dsw->dsw_pDirectSoundCapture,   NULL );
                    if( hr == DS_OK ) break;
                }
            }
        }
        if( hr != DS_OK )
        {
            ERR_RPT(("PortAudio: DirectSoundCaptureCreate() failed!\n"));
            result = paHostError;
            sPaHostError = hr;
            goto error;
        }
        hr = DSW_InitInputBuffer( dsw,
                                  (unsigned long) (past->past_SampleRate + 0.5),
                                  past->past_NumInputChannels, numBytes );
        DBUG(("DSW_InitInputBuffer() returns %x\n", hr));
        if( hr != DS_OK )
        {
            ERR_RPT(("PortAudio: DSW_InitInputBuffer() returns %x\n", hr));
            result = paHostError;
            sPaHostError = hr;
            goto error;
        }
    }
#endif /* SUPPORT_AUDIO_CAPTURE */
    /* Calculate scalar used in CPULoad calculation. */
    {
        LARGE_INTEGER frequency;
        if( QueryPerformanceFrequency( &frequency ) == 0 )
        {
            pahsc->pahsc_InverseTicksPerUserBuffer = 0.0;
        }
        else
        {
            pahsc->pahsc_InverseTicksPerUserBuffer = past->past_SampleRate /
                    ( (double)frequency.QuadPart * past->past_FramesPerUserBuffer );
            DBUG(("pahsc_InverseTicksPerUserBuffer = %g\n", pahsc->pahsc_InverseTicksPerUserBuffer ));
        }
    }
    return result;
error:
    PaHost_CloseStream( past );
    return result;
}
/*************************************************************************/
PaError PaHost_StartOutput( internalPortAudioStream *past )
{
    HRESULT          hr;
    PaHostSoundControl *pahsc;
    PaError          result = paNoError;
    pahsc = (PaHostSoundControl *) past->past_DeviceData;
    /* Give user callback a chance to pre-fill buffer. */
    result = Pa_TimeSlice( past );
    if( result != paNoError ) return result; // FIXME - what if finished?
    hr = DSW_StartOutput( &pahsc->pahsc_DSoundWrapper );
    DBUG(("PaHost_StartOutput: DSW_StartOutput returned = 0x%X.\n", hr));
    if( hr != DS_OK )
    {
        result = paHostError;
        sPaHostError = hr;
        goto error;
    }
error:
    return result;
}
/*************************************************************************/
PaError PaHost_StartInput( internalPortAudioStream *past )
{
    PaError          result = paNoError;
#if SUPPORT_AUDIO_CAPTURE
    HRESULT          hr;
    PaHostSoundControl *pahsc;
    pahsc = (PaHostSoundControl *) past->past_DeviceData;
    hr = DSW_StartInput( &pahsc->pahsc_DSoundWrapper );
    DBUG(("Pa_StartStream: DSW_StartInput returned = 0x%X.\n", hr));
    if( hr != DS_OK )
    {
        result = paHostError;
        sPaHostError = hr;
        goto error;
    }
error:
#endif /* SUPPORT_AUDIO_CAPTURE */
    return result;
}
/*************************************************************************/
PaError PaHost_StartEngine( internalPortAudioStream *past )
{
    PaHostSoundControl *pahsc;
    PaError          result = paNoError;
    pahsc = (PaHostSoundControl *) past->past_DeviceData;
    past->past_StopNow = 0;
    past->past_StopSoon = 0;
    past->past_IsActive = 1;
    /* Create timer that will wake us up so we can fill the DSound buffer. */
    {
        int msecPerBuffer;
        int resolution;
        int bufsPerInterrupt;
        
        DBUG(("PaHost_StartEngine: past_NumUserBuffers = %d\n", past->past_NumUserBuffers));
        /* Decide how often to wake up and fill the buffers. */
        if( past->past_NumUserBuffers == 2 )
        {
            /* Generate two timer interrupts per user buffer. */
            msecPerBuffer = (500 * past->past_FramesPerUserBuffer) / (int) past->past_SampleRate;
        }
        else
        {
            if ( past->past_NumUserBuffers >= 16 ) bufsPerInterrupt = past->past_NumUserBuffers/8; 
            else if ( past->past_NumUserBuffers >= 8 ) bufsPerInterrupt = 2;
            else bufsPerInterrupt = 1;
                
            msecPerBuffer = 1000 * (bufsPerInterrupt * past->past_FramesPerUserBuffer) / (int) past->past_SampleRate;

            DBUG(("PaHost_StartEngine: bufsPerInterrupt = %d\n", bufsPerInterrupt));
        }

        DBUG(("PaHost_StartEngine: msecPerBuffer = %d\n", msecPerBuffer));

        if( msecPerBuffer < 10 ) msecPerBuffer = 10;
        else if( msecPerBuffer > 100 ) msecPerBuffer = 100;
        DBUG(("PaHost_StartEngine: clipped msecPerBuffer = %d\n", msecPerBuffer));

        resolution = msecPerBuffer/4;
        pahsc->pahsc_TimerID = timeSetEvent( msecPerBuffer, resolution, (LPTIMECALLBACK) Pa_TimerCallback,
                                             (DWORD) past, TIME_PERIODIC );
    }
    if( pahsc->pahsc_TimerID == 0 )
    {
        past->past_IsActive = 0;
        result = paHostError;
        sPaHostError = 0;
        goto error;
    }
error:
    return result;
}
/*************************************************************************/
PaError PaHost_StopEngine( internalPortAudioStream *past, int abort )
{
    int timeoutMsec;
    PaHostSoundControl *pahsc = (PaHostSoundControl *) past->past_DeviceData;
    if( pahsc == NULL ) return paNoError;
    if( abort ) past->past_StopNow = 1;
    past->past_StopSoon = 1;
    /* Set timeout at 20% beyond maximum time we might wait. */
    timeoutMsec = (int) (1200.0 * pahsc->pahsc_FramesPerDSBuffer / past->past_SampleRate);
    while( past->past_IsActive && (timeoutMsec > 0)  )
    {
        Sleep(10);
        timeoutMsec -= 10;
    }
    if( pahsc->pahsc_TimerID != 0 )
    {
        timeKillEvent(pahsc->pahsc_TimerID);  /* Stop callback timer. */
        pahsc->pahsc_TimerID = 0;
    }
    return paNoError;
}
/*************************************************************************/
PaError PaHost_StopInput( internalPortAudioStream *past, int abort )
{
#if SUPPORT_AUDIO_CAPTURE
    HRESULT hr;
    PaHostSoundControl *pahsc = (PaHostSoundControl *) past->past_DeviceData;
    if( pahsc == NULL ) return paNoError;
    (void) abort;
    hr = DSW_StopInput( &pahsc->pahsc_DSoundWrapper );
    DBUG(("DSW_StopInput() result is %x\n", hr));
#endif /* SUPPORT_AUDIO_CAPTURE */
    return paNoError;
}
/*************************************************************************/
PaError PaHost_StopOutput( internalPortAudioStream *past, int abort )
{
    HRESULT hr;
    PaHostSoundControl *pahsc;
    pahsc = (PaHostSoundControl *) past->past_DeviceData;
    if( pahsc == NULL ) return paNoError;
    (void) abort;
    hr = DSW_StopOutput( &pahsc->pahsc_DSoundWrapper );
    DBUG(("DSW_StopOutput() result is %x\n", hr));
    return paNoError;
}
/*******************************************************************/
PaError PaHost_CloseStream( internalPortAudioStream   *past )
{
    PaHostSoundControl *pahsc;
    if( past == NULL ) return paBadStreamPtr;
    pahsc = (PaHostSoundControl *) past->past_DeviceData;
    if( pahsc == NULL ) return paNoError;
    DSW_Term( &pahsc->pahsc_DSoundWrapper );
    if( pahsc->pahsc_NativeBuffer )
    {
        PaHost_FreeFastMemory( pahsc->pahsc_NativeBuffer, pahsc->pahsc_BytesPerBuffer ); /* MEM */
        pahsc->pahsc_NativeBuffer = NULL;
    }
    PaHost_FreeFastMemory( pahsc, sizeof(PaHostSoundControl) ); /* MEM */
    past->past_DeviceData = NULL;
    return paNoError;
}

/* Set minimal latency based on whether NT or Win95.
 * NT has higher latency.
 */
static int PaHost_GetMinSystemLatency( void )
{
    int minLatencyMsec;
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
    return minLatencyMsec;
}

/*************************************************************************
** Determine minimum number of buffers required for this host based
** on minimum latency. Latency can be optionally set by user by setting
** an environment variable. For example, to set latency to 200 msec, put:
**
**    set PA_MIN_LATENCY_MSEC=200
**
** in the AUTOEXEC.BAT file and reboot.
** If the environment variable is not set, then the latency will be determined
** based on the OS. Windows NT has higher latency than Win95.
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
        minLatencyMsec = atoi( envbuf );
    }
    else
    {
        minLatencyMsec = PaHost_GetMinSystemLatency();
#if PA_USE_HIGH_LATENCY
        PRINT(("PA - Minimum Latency set to %d msec!\n", minLatencyMsec ));
#endif

    }
    minBuffers = (int) (1.0 + ((double)minLatencyMsec / msecPerBuffer));
    if( minBuffers < 2 ) minBuffers = 2;
    return minBuffers;
}
/*************************************************************************/
PaError PaHost_Term( void )
{
    int i;
    /* Free names allocated during enumeration. */
    for( i=0; i<sNumDevices; i++ )
    {
        if( sDevices[i].pad_Info.name != NULL )
        {
            free( (void *) sDevices[i].pad_Info.name );
            sDevices[i].pad_Info.name = NULL;
        }
    }
    if( sDevices != NULL )
    {
        PaHost_FreeFastMemory( sDevices, sNumDevices * sizeof(internalPortAudioDevice) ); /* MEM */
        sDevices = NULL;
        sNumDevices = 0;
    }
    return 0;
}
void Pa_Sleep( long msec )
{
    Sleep( msec );
}
/*************************************************************************
 * Allocate memory that can be accessed in real-time.
 * This may need to be held in physical memory so that it is not
 * paged to virtual memory.
 * This call MUST be balanced with a call to PaHost_FreeFastMemory().
 * Memory will be set to zero.
 */
void *PaHost_AllocateFastMemory( long numBytes )
{
    void *addr = GlobalAlloc( GPTR, numBytes ); /* FIXME - do we need physical memory? Use VirtualLock() */ /* MEM */
    return addr;
}
/*************************************************************************
 * Free memory that could be accessed in real-time.
 * This call MUST be balanced with a call to PaHost_AllocateFastMemory().
 */
void PaHost_FreeFastMemory( void *addr, long numBytes )
{
    if( addr != NULL ) GlobalFree( addr ); /* MEM */
}
/***********************************************************************/
PaError PaHost_StreamActive( internalPortAudioStream   *past )
{
    PaHostSoundControl *pahsc;
    if( past == NULL ) return paBadStreamPtr;
    pahsc = (PaHostSoundControl *) past->past_DeviceData;
    if( pahsc == NULL ) return paInternalError;
    return (PaError) (past->past_IsActive);
}
/*************************************************************************/
PaTimestamp Pa_StreamTime( PortAudioStream *stream )
{
    DSoundWrapper   *dsw;
    internalPortAudioStream   *past = (internalPortAudioStream *) stream;
    PaHostSoundControl *pahsc;
    if( past == NULL ) return paBadStreamPtr;
    pahsc = (PaHostSoundControl *) past->past_DeviceData;
    dsw = &pahsc->pahsc_DSoundWrapper;
    return dsw->dsw_FramesPlayed;
}
