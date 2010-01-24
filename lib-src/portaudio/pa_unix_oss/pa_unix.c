/*
 * PortAudio Portable Real-Time Audio Library
 * Latest Version at: http://www.portaudio.com
 * Linux OSS Implementation by douglas repetto and Phil Burk
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

/*
Modification History
  1/2001 - Phil Burk - initial hack for Linux
  2/2001 - Douglas Repetto - many improvements, initial query support
  4/2/2001 - Phil - stop/abort thread control, separate in/out native buffers
  5/28/2001 - Phil - use pthread_create() instead of clone(). Thanks Stephen Brandon!
       use pthread_join() after thread shutdown.
  5/29/2001 - Phil - query for multiple devices, multiple formats,
                     input mode and input+output mode working,
       Pa_GetCPULoad() implemented.
  PLB20010817 - Phil & Janos Haber - don't halt if test of sample rate fails.
  SB20010904 - Stephen Brandon - mods needed for GNUSTEP and SndKit
  JH20010905 - Janos Haber - FreeBSD mods
  2001-09-22 - Heiko - (i.e. Heiko Purnhagen <purnhage@tnt.uni-hannover.de> ;-)
                       added 24k and 16k to ratesToTry[]
         fixed Pa_GetInternalDevice()
         changed DEVICE_NAME_BASE from /dev/audio to /dev/dsp
         handled SNDCTL_DSP_SPEED in Pq_QueryDevice() more graceful
         fixed Pa_StreamTime() for paqa_errs.c
         fixed numCannel=2 oddity and error handling in Pa_SetupDeviceFormat()
         grep also for HP20010922 ...
  PLB20010924 - Phil - merged Heiko's changes
                       removed sNumDevices and potential related bugs,
         use getenv("PA_MIN_LATENCY_MSEC") to set desired latency,
         simplify CPU Load calculation by comparing real-time to framesPerBuffer,
         always close device when querying even if error occurs,
  PLB20010927 - Phil - Improved negotiation for numChannels.
  SG20011005 - Stewart Greenhill - set numChannels back to reasonable value after query.
  DH20010115 - David Herring - fixed uninitialized handle.

  DM20020218 - Dominic Mazzoni - Try to open in nonblocking mode first, in case
                                 the device is already open.  New implementation of
                                 Pa_StreamTime that uses SNDCTL_DSP_GETOPTR but
                                 uses our own counter to avoid wraparound.
  PLB20020222 - Phil Burk - Added WatchDog proc if audio running at high priority.
                      Check error return from read() and write().
                      Check CPU endianness instead of assuming Little Endian.
  20020621 - pa_unix_oss.c split into pa_unix.c, pa_unix.h, pa_unix_oss.c by
         Augustus Saunders. Return values from usleep() ignored by Sam Bayer
         because not cross-platform compatible (at least until we get configure
         going). Pa_SetupDeviceFormat split into input and output sides to
         reflect capabilities of Solaris.

  20030206 - Martin Rohrbach - various mods for Solaris
  
  20030410 - Bjorn Dittmer-Roche - fixed numerous problems associated with pthread_t

TODO
O- put semaphore lock around shared data?
O- handle native formats better
O- handle stereo-only device better ???
O- what if input and output of a device capabilities differ (e.g. es1371) ???
*/


#include "pa_unix.h"

typedef void *(*pthread_function_t)(void *);

/************************************************* Shared Data ********/
/* FIXME - put Mutex around this shared data. */
static internalPortAudioDevice *sDeviceList = NULL;
static int sDefaultInputDeviceID = paNoDevice;
static int sDefaultOutputDeviceID = paNoDevice;
static int sPaHostError = 0;

/********************************* BEGIN CPU UTILIZATION MEASUREMENT ****/
static void Pa_StartUsageCalculation( internalPortAudioStream   *past )
{
    PaHostSoundControl *pahsc = (PaHostSoundControl *) past->past_DeviceData;
    if( pahsc == NULL ) return;
    /* Query system timer for usage analysis and to prevent overuse of CPU. */
    gettimeofday( &pahsc->pahsc_EntryTime, NULL );
}

static long SubtractTime_AminusB( struct timeval *timeA, struct timeval *timeB )
{
    long secs = timeA->tv_sec - timeB->tv_sec;
    long usecs = secs * 1000000;
    usecs += (timeA->tv_usec - timeB->tv_usec);
    return usecs;
}

/******************************************************************************
** Measure fractional CPU load based on real-time it took to calculate
** buffers worth of output.
*/
static void Pa_EndUsageCalculation( internalPortAudioStream   *past )
{
    struct timeval currentTime;
    long  usecsElapsed;
    double newUsage;

#define LOWPASS_COEFFICIENT_0   (0.95)
#define LOWPASS_COEFFICIENT_1   (0.99999 - LOWPASS_COEFFICIENT_0)

    PaHostSoundControl *pahsc = (PaHostSoundControl *) past->past_DeviceData;
    if( pahsc == NULL ) return;

    if( gettimeofday( &currentTime, NULL ) == 0 )
    {
        usecsElapsed = SubtractTime_AminusB( &currentTime, &pahsc->pahsc_EntryTime );
        /* Use inverse because it is faster than the divide. */
        newUsage =  usecsElapsed * pahsc->pahsc_InverseMicrosPerBuffer;

        past->past_Usage = (LOWPASS_COEFFICIENT_0 * past->past_Usage) +
                           (LOWPASS_COEFFICIENT_1 * newUsage);

    }
}
/****************************************** END CPU UTILIZATION *******/

/*********************************************************************
 * Determines the number of available devices by trying to open
 * each "/dev/dsp#" or "/dsp/audio#" in order until it fails.
 * Add each working device to a singly linked list of devices.
 */
PaError Pa_QueryDevices( void )
{
    internalPortAudioDevice *pad, *lastPad;
    int      go = 1;
    int      numDevices = 0;
    PaError  testResult;
    PaError  result = paNoError;
    char     *envdev;

    sDefaultInputDeviceID = paNoDevice;
    sDefaultOutputDeviceID = paNoDevice;

    lastPad = NULL;

    while( go )
    {
        /* Allocate structure to hold device info. */
        pad = (internalPortAudioDevice *)
              PaHost_AllocateFastMemory( sizeof(internalPortAudioDevice) );
        if( pad == NULL ) return paInsufficientMemory;
        memset( pad, 0, sizeof(internalPortAudioDevice) );

        /* Build name for device. */
        if( numDevices == 0 )
        {
            sprintf( pad->pad_DeviceName, DEVICE_NAME_BASE);
        }
        else
        {
            sprintf( pad->pad_DeviceName, DEVICE_NAME_BASE "%d", numDevices );
        }

        DBUG(("Try device %s\n", pad->pad_DeviceName ));
        testResult = Pa_QueryDevice( pad->pad_DeviceName, pad );
        DBUG(("Pa_QueryDevice returned %d\n", testResult ));
        if( testResult != paNoError )
        {
            if( lastPad == NULL )
            {
                result = testResult; /* No good devices! */
            }
            go = 0;
            PaHost_FreeFastMemory( pad, sizeof(internalPortAudioDevice) );
        }
        else
        {
            numDevices += 1;
            /* Add to linked list of devices. */
            if( lastPad )
            {
                lastPad->pad_Next = pad;
            }
            else
            {
                sDeviceList = pad; /* First element in linked list. */
            }
            lastPad = pad;
        }
    }

    /* I'm sitting at a SunRay1 and I neither have /dev/audio# nor /dev/dsp#.
       Instead, the correct audio device is stored in the environment variable
       AUDIODEV and/or UTAUDIODEV, so check these devices as well if we haven't
       checked them yet above  - MR */

    DBUG(("Checking for AUDIODEV and UTAUDIODEV\n"));
    envdev = getenv("AUDIODEV");
    if (envdev != NULL && !strstr(envdev, DEVICE_NAME_BASE)) {
        result = paNoError;

        /* Allocate structure to hold device info. */
        pad = (internalPortAudioDevice *)
              PaHost_AllocateFastMemory( sizeof(internalPortAudioDevice) );
        if( pad == NULL ) return paInsufficientMemory;
        memset( pad, 0, sizeof(internalPortAudioDevice) );

        /* Build name for device. */
        strcpy(pad->pad_DeviceName, envdev);

        DBUG(("Try device %s\n", pad->pad_DeviceName ));
        testResult = Pa_QueryDevice( pad->pad_DeviceName, pad );
        DBUG(("Pa_QueryDevice returned %d\n", testResult ));
        if( testResult != paNoError )
        {
            if( lastPad == NULL )
            {
                result = testResult; /* No good devices! */
            }
            PaHost_FreeFastMemory( pad, sizeof(internalPortAudioDevice) );
        }
        else
        {
            numDevices += 1;
            /* Add to linked list of devices. */
            if( lastPad )
            {
                lastPad->pad_Next = pad;
            }
            else
            {
                sDeviceList = pad; /* First element in linked list. */
            }
            lastPad = pad;
        }
    }

    envdev = getenv("UTAUDIODEV");
    if (envdev != NULL && !strstr(envdev, DEVICE_NAME_BASE) && getenv("AUDIODEV") != NULL && strcmp(envdev, getenv("AUDIODEV"))) {
        result = paNoError;

        /* Allocate structure to hold device info. */
        pad = (internalPortAudioDevice *)
              PaHost_AllocateFastMemory( sizeof(internalPortAudioDevice) );
        if( pad == NULL ) return paInsufficientMemory;
        memset( pad, 0, sizeof(internalPortAudioDevice) );

        /* Build name for device. */
        strcpy(pad->pad_DeviceName, envdev);

        DBUG(("Try device %s\n", pad->pad_DeviceName ));
        testResult = Pa_QueryDevice( pad->pad_DeviceName, pad );
        DBUG(("Pa_QueryDevice returned %d\n", testResult ));
        if( testResult != paNoError )
        {
            if( lastPad == NULL )
            {
                result = testResult; /* No good devices! */
            }
            PaHost_FreeFastMemory( pad, sizeof(internalPortAudioDevice) );
        }
        else
        {
            numDevices += 1;
            /* Add to linked list of devices. */
            if( lastPad )
            {
                lastPad->pad_Next = pad;
            }
            else
            {
                sDeviceList = pad; /* First element in linked list. */
            }
            lastPad = pad;
        }
    }

    return result;
}

/*************************************************************************/
int Pa_CountDevices()
{
    int numDevices = 0;
    internalPortAudioDevice *pad;

    if( sDeviceList == NULL ) Pa_Initialize();
    /* Count devices in list. */
    pad = sDeviceList;
    while( pad != NULL )
    {
        pad = pad->pad_Next;
        numDevices++;
    }

    return numDevices;
}

/*************************************************************************/
internalPortAudioDevice *Pa_GetInternalDevice( PaDeviceID id )
{
    internalPortAudioDevice *pad;
    if( (id < 0) || ( id >= Pa_CountDevices()) ) return NULL;
    pad = sDeviceList;
    while( id > 0 )
    {
        pad = pad->pad_Next;
        id--;
    }
    return pad;
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
    if( sDeviceList == NULL )
    {
        return Pa_QueryDevices();
    }
    return 0;
}

PaDeviceID Pa_GetDefaultInputDeviceID( void )
{
    /* return paNoDevice; */
    return 0;
}

PaDeviceID Pa_GetDefaultOutputDeviceID( void )
{
    return 0;
}

/**********************************************************************
** Make sure that we have queried the device capabilities.
*/

PaError PaHost_Init( void )
{
    return Pa_MaybeQueryDevices();
}

/*******************************************************************************************
 * The ol' Canary in a Coal Mine trick.
 * Just update the time periodically.
 * Runs at low priority so if audio thread runs wild, this thread will get starved
 * and the watchdog will detect it.
 */

#define SCHEDULER_POLICY         SCHED_RR
#define WATCHDOG_MAX_SECONDS    (3)
#define WATCHDOG_INTERVAL_USEC  (1000000)

static int PaHost_CanaryProc( PaHostSoundControl   *pahsc )
{
    int   result = 0;

#ifdef GNUSTEP
    GSRegisterCurrentThread(); /* SB20010904 */
#endif

    while( pahsc->pahsc_CanaryRun) {
      usleep( WATCHDOG_INTERVAL_USEC );
      gettimeofday( &pahsc->pahsc_CanaryTime, NULL );
    }

    DBUG(("PaHost_CanaryProc: exiting.\n"));

#ifdef GNUSTEP
    GSUnregisterCurrentThread();  /* SB20010904 */
#endif

    return result;
}

/*******************************************************************************************
 * Monitor audio thread and lower its it if it hogs the CPU.
 * To prevent getting killed, the audio thread must update a
 * variable with a timer value.
 * If the value is not recent enough, then the
 * thread will get killed.
 */

static PaError PaHost_WatchDogProc( PaHostSoundControl   *pahsc )
{
    struct sched_param    schp = { 0 };
    int                   maxPri;

#ifdef GNUSTEP
    GSRegisterCurrentThread(); /* SB20010904 */
#endif

/* Run at a priority level above audio thread so we can still run if it hangs. */
/* Rise more than 1 because of rumored off-by-one scheduler bugs. */
    schp.sched_priority = pahsc->pahsc_AudioPriority + 4;
    maxPri = sched_get_priority_max(SCHEDULER_POLICY);
    if( schp.sched_priority > maxPri ) schp.sched_priority = maxPri;

    if (sched_setscheduler(0, SCHEDULER_POLICY, &schp) != 0)
    {
        ERR_RPT(("PaHost_WatchDogProc: cannot set watch dog priority!\n"));
        goto killAudio;
    }

    /* Compare watchdog time with audio and canary thread times. */
    /* Sleep for a while or until thread cancelled. */
    while( pahsc->pahsc_WatchDogRun )
    {

        int              delta;
        struct timeval   currentTime;

        usleep( WATCHDOG_INTERVAL_USEC );
        gettimeofday( &currentTime, NULL );

        /* If audio thread is not advancing, then it must be hung so kill it. */
        delta = currentTime.tv_sec - pahsc->pahsc_EntryTime.tv_sec;
        DBUG(("PaHost_WatchDogProc: audio delta = %d\n", delta ));
        if( delta > WATCHDOG_MAX_SECONDS )
        {
            goto killAudio;
        }

        /* If canary died, then lower audio priority and halt canary. */
        delta = currentTime.tv_sec - pahsc->pahsc_CanaryTime.tv_sec;
        if( delta > WATCHDOG_MAX_SECONDS )
        {
            ERR_RPT(("PaHost_WatchDogProc: canary died!\n"));
            goto lowerAudio;
        }
    }

    DBUG(("PaHost_WatchDogProc: exiting.\n"));
#ifdef GNUSTEP
    GSUnregisterCurrentThread();  /* SB20010904 */
#endif
    return 0;

lowerAudio:
    {
        struct sched_param    schat = { 0 };
        if( sched_setscheduler(pahsc->pahsc_AudioThreadPID, SCHED_OTHER, &schat) != 0)
        {
            ERR_RPT(("PaHost_WatchDogProc: failed to lower audio priority. errno = %d\n", errno ));
            /* Fall through into killing audio thread. */
        }
        else
        {
            ERR_RPT(("PaHost_WatchDogProc: lowered audio priority to prevent hogging of CPU.\n"));
            goto cleanup;
        }
    }

killAudio:
    ERR_RPT(("PaHost_WatchDogProc: killing hung audio thread!\n"));
    pthread_kill( pahsc->pahsc_AudioThread, SIGKILL );

cleanup:
    pahsc->pahsc_CanaryRun = 0;
    DBUG(("PaHost_WatchDogProc: cancel Canary\n"));
    pthread_cancel( pahsc->pahsc_CanaryThread );
    DBUG(("PaHost_WatchDogProc: join Canary\n"));
    pthread_join( pahsc->pahsc_CanaryThread, NULL );
    DBUG(("PaHost_WatchDogProc: forget Canary\n"));
    pahsc->pahsc_IsCanaryThreadValid = 0;

#ifdef GNUSTEP
    GSUnregisterCurrentThread();  /* SB20010904 */
#endif
    return 0;
}

/*******************************************************************************************/
static void PaHost_StopWatchDog( PaHostSoundControl   *pahsc )
{
/* Cancel WatchDog thread if there is one. */
    if( pahsc->pahsc_IsWatchDogThreadValid )
    {
        pahsc->pahsc_WatchDogRun = 0;
        DBUG(("PaHost_StopWatchDog: cancel WatchDog\n"));
        pthread_cancel( pahsc->pahsc_WatchDogThread );
        pthread_join( pahsc->pahsc_WatchDogThread, NULL );
        pahsc->pahsc_IsWatchDogThreadValid = 0;
    }
/* Cancel Canary thread if there is one. */
    if( pahsc->pahsc_IsCanaryThreadValid )
    {
        pahsc->pahsc_CanaryRun = 0;
        DBUG(("PaHost_StopWatchDog: cancel Canary\n"));
        pthread_cancel( pahsc->pahsc_CanaryThread );
        DBUG(("PaHost_StopWatchDog: join Canary\n"));
        pthread_join( pahsc->pahsc_CanaryThread, NULL );
        pahsc->pahsc_IsCanaryThreadValid = 0;
    }
}

/*******************************************************************************************/
static PaError PaHost_StartWatchDog( PaHostSoundControl   *pahsc )
{
    int      hres;
    PaError  result = 0;

    /* The watch dog watches for these timer updates */
    gettimeofday( &pahsc->pahsc_EntryTime, NULL );
    gettimeofday( &pahsc->pahsc_CanaryTime, NULL );

    /* Launch a canary thread to detect priority abuse. */
    pahsc->pahsc_CanaryRun = 1;
    hres = pthread_create(&(pahsc->pahsc_CanaryThread),
                      NULL /*pthread_attr_t * attr*/,
                      (pthread_function_t)PaHost_CanaryProc, pahsc);
    if( hres != 0 )
    {
        pahsc->pahsc_IsCanaryThreadValid = 0;
        result = paHostError;
        sPaHostError = hres;
        goto error;
    }
    pahsc->pahsc_IsCanaryThreadValid = 1;

    /* Launch a watchdog thread to prevent runaway audio thread. */
    pahsc->pahsc_WatchDogRun = 1;
    hres = pthread_create(&(pahsc->pahsc_WatchDogThread),
                      NULL /*pthread_attr_t * attr*/,
                      (pthread_function_t)PaHost_WatchDogProc, pahsc);
    if( hres != 0 )
    {
        pahsc->pahsc_IsWatchDogThreadValid = 0;
        result = paHostError;
        sPaHostError = hres;
        goto error;
    }
    pahsc->pahsc_IsWatchDogThreadValid = 1;
    return result;

error:
    PaHost_StopWatchDog( pahsc );
    return result;
}

/*******************************************************************************************
 * Bump priority of audio thread if running with superuser priveledges.
 * if priority bumped then launch a watchdog.
 */
static PaError PaHost_BoostPriority( internalPortAudioStream *past )
{
    PaHostSoundControl  *pahsc;
    PaError              result = paNoError;
    struct sched_param   schp = { 0 };

    pahsc = (PaHostSoundControl *) past->past_DeviceData;
    if( pahsc == NULL ) return paInternalError;

    pahsc->pahsc_AudioThreadPID = getpid();
    DBUG(("PaHost_BoostPriority: audio PID = %d\n", pahsc->pahsc_AudioThreadPID ));

    /* Choose a priority in the middle of the range. */
    pahsc->pahsc_AudioPriority = (sched_get_priority_max(SCHEDULER_POLICY) -
                                  sched_get_priority_min(SCHEDULER_POLICY)) / 2;
    schp.sched_priority = pahsc->pahsc_AudioPriority;

    if (sched_setscheduler(0, SCHEDULER_POLICY, &schp) != 0)
    {
        DBUG(("PortAudio: only superuser can use real-time priority.\n"));
    }
    else
    {
        DBUG(("PortAudio: audio callback priority set to level %d!\n", schp.sched_priority));
        /* We are running at high priority so we should have a watchdog in case audio goes wild. */
        result = PaHost_StartWatchDog( pahsc );
    }

    return result;
}

/*******************************************************************************************/
static PaError Pa_AudioThreadProc( internalPortAudioStream   *past )
{
    PaError      result;
    PaHostSoundControl             *pahsc;
    ssize_t      bytes_read, bytes_written;

    pahsc = (PaHostSoundControl *) past->past_DeviceData;
    if( pahsc == NULL ) return paInternalError;

#ifdef GNUSTEP
    GSRegisterCurrentThread(); /* SB20010904 */
#endif

    result = PaHost_BoostPriority( past );
    if( result < 0 ) goto error;

    past->past_IsActive = 1;
    DBUG(("entering thread.\n"));

    while( (past->past_StopNow == 0) && (past->past_StopSoon == 0) )
    {
        /* Read data from device */
        if(pahsc->pahsc_NativeInputBuffer)
        {
            unsigned int totalread = 0;
            DBUG(("Pa_AudioThreadProc: attempt to read %d bytes\n", pahsc->pahsc_BytesPerInputBuffer));
            do
            {
                bytes_read = read(pahsc->pahsc_InputHandle,
                    (char *)pahsc->pahsc_NativeInputBuffer + totalread,
                    pahsc->pahsc_BytesPerInputBuffer - totalread);

                if (bytes_read < 0)
                {
                    ERR_RPT(("PortAudio: read interrupted!\n"));
                    break;
                }

                totalread += bytes_read;
            } while( totalread < pahsc->pahsc_BytesPerInputBuffer);
        }

        /* Convert 16 bit native data to user data and call user routine. */
        DBUG(("converting...\n"));
        Pa_StartUsageCalculation( past );
        result = Pa_CallConvertInt16( past,
                                      pahsc->pahsc_NativeInputBuffer,
                                      pahsc->pahsc_NativeOutputBuffer );
        Pa_EndUsageCalculation( past );
        if( result != 0)
        {
            DBUG(("hmm, Pa_CallConvertInt16() says: %d. i'm bailing.\n",
                  result));
            break;
        }

        /* Write data to device. */
        if( pahsc->pahsc_NativeOutputBuffer )
        {
            unsigned int totalwritten = 0;
            do
            {
                bytes_written = write(pahsc->pahsc_OutputHandle,
                    (void *)pahsc->pahsc_NativeOutputBuffer,
                    pahsc->pahsc_BytesPerOutputBuffer);
                if( bytes_written < 0 )
                {
                    ERR_RPT(("PortAudio: write interrupted!"));
                    break;
                }

                totalwritten += bytes_written;
            } while( totalwritten < pahsc->pahsc_BytesPerOutputBuffer);
        }

        Pa_UpdateStreamTime(pahsc);
    }
    DBUG(("Pa_AudioThreadProc: left audio loop.\n"));

    past->past_IsActive = 0;
    PaHost_StopWatchDog( pahsc );

error:
    DBUG(("leaving audio thread.\n"));
#ifdef GNUSTEP
    GSUnregisterCurrentThread();  /* SB20010904 */
#endif
    return result;
}

/*************************************************************************
** Determine minimum number of buffers required for this host based
** on minimum latency. Latency can be optionally set by user by setting
** an environment variable. For example, to set latency to 200 msec, put:
**
**    set PA_MIN_LATENCY_MSEC=200
**
** in the cshrc file.
*/
#define PA_LATENCY_ENV_NAME  ("PA_MIN_LATENCY_MSEC")

int Pa_GetMinNumBuffers( int framesPerBuffer, double framesPerSecond )
{
    int minBuffers;
    int minLatencyMsec = MIN_LATENCY_MSEC;
    char *minLatencyText = getenv(PA_LATENCY_ENV_NAME);
    if( minLatencyText != NULL )
    {
        PRINT(("PA_MIN_LATENCY_MSEC = %s\n", minLatencyText ));
        minLatencyMsec = atoi( minLatencyText );
        if( minLatencyMsec < 1 ) minLatencyMsec = 1;
        else if( minLatencyMsec > 5000 ) minLatencyMsec = 5000;
    }

    minBuffers = (int) ((minLatencyMsec * framesPerSecond) / ( 1000.0 * framesPerBuffer ));
    if( minBuffers < 2 ) minBuffers = 2;
    return minBuffers;
}

/*******************************************************************/
PaError PaHost_OpenStream( internalPortAudioStream   *past )
{
    PaError          result = paNoError;
    PaHostSoundControl *pahsc;
    unsigned int     minNumBuffers;
    internalPortAudioDevice *pad;
    DBUG(("PaHost_OpenStream() called.\n" ));

    /* Allocate and initialize host data. */
    pahsc = (PaHostSoundControl *) malloc(sizeof(PaHostSoundControl));
    if( pahsc == NULL )
    {
        result = paInsufficientMemory;
        goto error;
    }
    memset( pahsc, 0, sizeof(PaHostSoundControl) );
    past->past_DeviceData = (void *) pahsc;

    pahsc->pahsc_OutputHandle = BAD_DEVICE_ID; /* No device currently opened. */
    pahsc->pahsc_InputHandle = BAD_DEVICE_ID;
    pahsc->pahsc_IsAudioThreadValid = 0;
    pahsc->pahsc_IsWatchDogThreadValid = 0;

    /* Allocate native buffers. */
    pahsc->pahsc_BytesPerInputBuffer = past->past_FramesPerUserBuffer *
                                       past->past_NumInputChannels * sizeof(short);
    if( past->past_NumInputChannels > 0)
    {
        pahsc->pahsc_NativeInputBuffer = (short *) malloc(pahsc->pahsc_BytesPerInputBuffer);
        if( pahsc->pahsc_NativeInputBuffer == NULL )
        {
            result = paInsufficientMemory;
            goto error;
        }
    }
    pahsc->pahsc_BytesPerOutputBuffer = past->past_FramesPerUserBuffer *
                                        past->past_NumOutputChannels * sizeof(short);
    if( past->past_NumOutputChannels > 0)
    {
        pahsc->pahsc_NativeOutputBuffer = (short *) malloc(pahsc->pahsc_BytesPerOutputBuffer);
        if( pahsc->pahsc_NativeOutputBuffer == NULL )
        {
            result = paInsufficientMemory;
            goto error;
        }
    }

    /* DBUG(("PaHost_OpenStream: pahsc_MinFramesPerHostBuffer = %d\n", pahsc->pahsc_MinFramesPerHostBuffer )); */
    minNumBuffers = Pa_GetMinNumBuffers( past->past_FramesPerUserBuffer, past->past_SampleRate );
    past->past_NumUserBuffers = ( minNumBuffers > past->past_NumUserBuffers ) ? minNumBuffers : past->past_NumUserBuffers;

    pahsc->pahsc_InverseMicrosPerBuffer = past->past_SampleRate / (1000000.0 * past->past_FramesPerUserBuffer);
    DBUG(("past_SampleRate = %g\n", past->past_SampleRate ));
    DBUG(("past_FramesPerUserBuffer = %d\n", past->past_FramesPerUserBuffer ));
    DBUG(("pahsc_InverseMicrosPerBuffer = %g\n", pahsc->pahsc_InverseMicrosPerBuffer ));

    /* ------------------------- OPEN DEVICE -----------------------*/

    /* just output */
    if (past->past_OutputDeviceID == past->past_InputDeviceID)
    {

        if ((past->past_NumOutputChannels > 0) && (past->past_NumInputChannels > 0) )
        {
            pad = Pa_GetInternalDevice( past->past_OutputDeviceID );
            DBUG(("PaHost_OpenStream: attempt to open %s for O_RDWR\n", pad->pad_DeviceName ));

            /* dmazzoni: test it first in nonblocking mode to
               make sure the device is not busy */
            pahsc->pahsc_InputHandle = open(pad->pad_DeviceName,O_RDWR|O_NONBLOCK);
            if(pahsc->pahsc_InputHandle==-1)
            {
                ERR_RPT(("PaHost_OpenStream: could not open %s for O_RDWR\n", pad->pad_DeviceName ));
                result = paHostError;
                goto error;
            }
            close(pahsc->pahsc_InputHandle);

            pahsc->pahsc_OutputHandle = pahsc->pahsc_InputHandle =
                                            open(pad->pad_DeviceName,O_RDWR);
            if(pahsc->pahsc_InputHandle==-1)
            {
                ERR_RPT(("PaHost_OpenStream: could not open %s for O_RDWR\n", pad->pad_DeviceName ));
                result = paHostError;
                goto error;
            }
            Pa_SetLatency( pahsc->pahsc_OutputHandle,
                           past->past_NumUserBuffers, past->past_FramesPerUserBuffer,
                           past->past_NumOutputChannels );
            result = Pa_SetupDeviceFormat( pahsc->pahsc_OutputHandle,
                                           past->past_NumOutputChannels, (int)past->past_SampleRate );
        }
    }
    else
    {
        if (past->past_NumOutputChannels > 0)
        {
            pad = Pa_GetInternalDevice( past->past_OutputDeviceID );
            DBUG(("PaHost_OpenStream: attempt to open %s for O_WRONLY\n", pad->pad_DeviceName ));
            /* dmazzoni: test it first in nonblocking mode to
               make sure the device is not busy */
            pahsc->pahsc_OutputHandle = open(pad->pad_DeviceName,O_WRONLY|O_NONBLOCK);
            if(pahsc->pahsc_OutputHandle==-1)
            {
                ERR_RPT(("PaHost_OpenStream: could not open %s for O_WRONLY\n", pad->pad_DeviceName ));
                result = paHostError;
                goto error;
            }
            close(pahsc->pahsc_OutputHandle);

            pahsc->pahsc_OutputHandle = open(pad->pad_DeviceName,O_WRONLY);
            if(pahsc->pahsc_OutputHandle==-1)
            {
                ERR_RPT(("PaHost_OpenStream: could not open %s for O_WRONLY\n", pad->pad_DeviceName ));
                result = paHostError;
                goto error;
            }
            Pa_SetLatency( pahsc->pahsc_OutputHandle,
                           past->past_NumUserBuffers, past->past_FramesPerUserBuffer,
                           past->past_NumOutputChannels );
            result = Pa_SetupOutputDeviceFormat( pahsc->pahsc_OutputHandle,
                                           past->past_NumOutputChannels, (int)past->past_SampleRate );
        }

        if (past->past_NumInputChannels > 0)
        {
            pad = Pa_GetInternalDevice( past->past_InputDeviceID );
            DBUG(("PaHost_OpenStream: attempt to open %s for O_RDONLY\n", pad->pad_DeviceName ));
            /* dmazzoni: test it first in nonblocking mode to
               make sure the device is not busy */
            pahsc->pahsc_InputHandle = open(pad->pad_DeviceName,O_RDONLY|O_NONBLOCK);
            if(pahsc->pahsc_InputHandle==-1)
            {
                ERR_RPT(("PaHost_OpenStream: could not open %s for O_RDONLY\n", pad->pad_DeviceName ));
                result = paHostError;
                goto error;
            }
            close(pahsc->pahsc_InputHandle);

            pahsc->pahsc_InputHandle = open(pad->pad_DeviceName,O_RDONLY);
            if(pahsc->pahsc_InputHandle==-1)
            {
                ERR_RPT(("PaHost_OpenStream: could not open %s for O_RDONLY\n", pad->pad_DeviceName ));
                result = paHostError;
                goto error;
            }
            Pa_SetLatency( pahsc->pahsc_InputHandle, /* DH20010115 - was OutputHandle! */
                           past->past_NumUserBuffers, past->past_FramesPerUserBuffer,
                           past->past_NumInputChannels );
            result = Pa_SetupInputDeviceFormat( pahsc->pahsc_InputHandle,
                                           past->past_NumInputChannels, (int)past->past_SampleRate );
        }
    }


    DBUG(("PaHost_OpenStream: SUCCESS - result = %d\n", result ));
    return result;

error:
    ERR_RPT(("PaHost_OpenStream: ERROR - result = %d\n", result ));
    PaHost_CloseStream( past );
    return result;
}

/*************************************************************************/
PaError PaHost_StartOutput( internalPortAudioStream *past )
{
    return paNoError;
}

/*************************************************************************/
PaError PaHost_StartInput( internalPortAudioStream *past )
{
    return paNoError;
}

/*************************************************************************/
PaError PaHost_StartEngine( internalPortAudioStream *past )
{
    PaHostSoundControl *pahsc;
    PaError             result = paNoError;
    int                 hres;

    pahsc = (PaHostSoundControl *) past->past_DeviceData;

    past->past_StopSoon = 0;
    past->past_StopNow = 0;
    past->past_IsActive = 1;

    /* Use pthread_create() instead of __clone() because:
     *   - pthread_create also works for other UNIX systems like Solaris,
     *   - the Java HotSpot VM crashes in pthread_setcanceltype() when using __clone()
     */
    hres = pthread_create(&(pahsc->pahsc_AudioThread),
                          NULL /*pthread_attr_t * attr*/,
                          (pthread_function_t)Pa_AudioThreadProc, past);
    if( hres != 0 )
    {
        result = paHostError;
        sPaHostError = hres;
        pahsc->pahsc_IsAudioThreadValid = 0;
        goto error;
    }
    pahsc->pahsc_IsAudioThreadValid = 1;

error:
    return result;
}

/*************************************************************************/
PaError PaHost_StopEngine( internalPortAudioStream *past, int abort )
{
    int                 hres;
    PaError             result = paNoError;
    PaHostSoundControl *pahsc = (PaHostSoundControl *) past->past_DeviceData;

    if( pahsc == NULL ) return paNoError;

    /* Tell background thread to stop generating more data and to let current data play out. */
    past->past_StopSoon = 1;
    /* If aborting, tell background thread to stop NOW! */
    if( abort ) past->past_StopNow = 1;

    /* Join thread to recover memory resources. */
    if( pahsc->pahsc_IsAudioThreadValid )
    {
        /* This check is needed for GNUSTEP - SB20010904 */
        if ( !pthread_equal( pahsc->pahsc_AudioThread, pthread_self() ) )
        {
            hres = pthread_join( pahsc->pahsc_AudioThread, NULL );
        }
        else
        {
            DBUG(("Play thread was stopped from itself - can't do pthread_join()\n"));
            hres = 0;
        }

        if( hres != 0 )
        {
            result = paHostError;
            sPaHostError = hres;
        }
        pahsc->pahsc_IsAudioThreadValid = 0;
    }

    past->past_IsActive = 0;

    return result;
}

/*************************************************************************/
PaError PaHost_StopInput( internalPortAudioStream *past, int abort )
{
    return paNoError;
}

/*************************************************************************/
PaError PaHost_StopOutput( internalPortAudioStream *past, int abort )
{
    return paNoError;
}

/*******************************************************************/
PaError PaHost_CloseStream( internalPortAudioStream   *past )
{
    PaHostSoundControl *pahsc;

    if( past == NULL ) return paBadStreamPtr;
    pahsc = (PaHostSoundControl *) past->past_DeviceData;
    if( pahsc == NULL ) return paNoError;

    if( pahsc->pahsc_OutputHandle != BAD_DEVICE_ID )
    {
        int err = 0;
        DBUG(("PaHost_CloseStream: attempt to close output device handle = %d\n",
              pahsc->pahsc_OutputHandle ));

        Pa_FlushStream(pahsc->pahsc_OutputHandle);

        err = close(pahsc->pahsc_OutputHandle);
        if( err < 0 )
        {
            ERR_RPT(("PaHost_CloseStream: warning, closing output device failed.\n"));
        }
    }

    if( (pahsc->pahsc_InputHandle != BAD_DEVICE_ID) &&
            (pahsc->pahsc_InputHandle != pahsc->pahsc_OutputHandle) )
    {
        int err = 0;
        DBUG(("PaHost_CloseStream: attempt to close input device handle = %d\n",
              pahsc->pahsc_InputHandle ));

        Pa_FlushStream(pahsc->pahsc_InputHandle);

        err = close(pahsc->pahsc_InputHandle);
        if( err < 0 )
        {
            ERR_RPT(("PaHost_CloseStream: warning, closing input device failed.\n"));
        }
    }
    pahsc->pahsc_OutputHandle = BAD_DEVICE_ID;
    pahsc->pahsc_InputHandle = BAD_DEVICE_ID;

    if( pahsc->pahsc_NativeInputBuffer )
    {
        free( pahsc->pahsc_NativeInputBuffer );
        pahsc->pahsc_NativeInputBuffer = NULL;
    }
    if( pahsc->pahsc_NativeOutputBuffer )
    {
        free( pahsc->pahsc_NativeOutputBuffer );
        pahsc->pahsc_NativeOutputBuffer = NULL;
    }

    free( pahsc );
    past->past_DeviceData = NULL;
    return paNoError;
}

/*************************************************************************/
PaError PaHost_Term( void )
{
    /* Free all of the linked devices. */
    internalPortAudioDevice *pad, *nextPad;
    pad = sDeviceList;
    while( pad != NULL )
    {
        nextPad = pad->pad_Next;
        DBUG(("PaHost_Term: freeing %s\n", pad->pad_DeviceName ));
        PaHost_FreeFastMemory( pad, sizeof(internalPortAudioDevice) );
        pad = nextPad;
    }
    sDeviceList = NULL;
    return 0;
}

/*************************************************************************
 * Sleep for the requested number of milliseconds.
 */
void Pa_Sleep( long msec )
{
#if 0
    struct timeval timeout;
    timeout.tv_sec = msec / 1000;
    timeout.tv_usec = (msec % 1000) * 1000;
    select( 0, NULL, NULL, NULL, &timeout );
#else
    long usecs = msec * 1000;
    usleep( usecs );
#endif
}

/*************************************************************************
 * Allocate memory that can be accessed in real-time.
 * This may need to be held in physical memory so that it is not
 * paged to virtual memory.
 * This call MUST be balanced with a call to PaHost_FreeFastMemory().
 */
void *PaHost_AllocateFastMemory( long numBytes )
{
    void *addr = malloc( numBytes ); /* FIXME - do we need physical, wired, non-virtual memory? */
    if( addr != NULL ) memset( addr, 0, numBytes );
    return addr;
}

/*************************************************************************
 * Free memory that could be accessed in real-time.
 * This call MUST be balanced with a call to PaHost_AllocateFastMemory().
 */
void PaHost_FreeFastMemory( void *addr, long numBytes )
{
    if( addr != NULL ) free( addr );
}


/***********************************************************************/
PaError PaHost_StreamActive( internalPortAudioStream   *past )
{
    PaHostSoundControl *pahsc;
    if( past == NULL ) return paBadStreamPtr;
    pahsc = (PaHostSoundControl *) past->past_DeviceData;
    if( pahsc == NULL ) return paInternalError;
    return (PaError) (past->past_IsActive != 0);
}

/***********************************************************************/
long Pa_GetHostError( void )
{
    return (long) sPaHostError;
}
