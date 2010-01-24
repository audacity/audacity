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

/* Modification history:
   20020621: pa_unix_oss.c split into pa_unix.c, pa_unix.h, pa_unix_oss.c by
   Augustus Saunders. See pa_unix.c for previous history. */

/*
 PROPOSED - should we add this to "portaudio.h". Problem with 
 Pa_QueryDevice() not having same driver name os Pa_OpenStream().
 
 A PaDriverInfo structure can be passed to the underlying device
 on the Pa_OpenStream() call. The contents and interpretation of
 the structure is determined by the PA implementation.
*/
typedef struct PaDriverInfo /* PROPOSED */
{
    /* Size of structure. Allows driver to extend the structure without breaking existing applications. */
    int           size;
    /* Can be used to request a specific device name. */
    const char   *name;
    unsigned long data;
}
PaDriverInfo;

#include <stdio.h>
#include <stdlib.h>
/* #include <malloc.h> */
#include <memory.h>
#include <math.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include <fcntl.h>
#include <unistd.h>
#include <signal.h>
#include <sched.h>
#include <pthread.h>
#include <errno.h>

#include "portaudio.h"
#include "pa_host.h"
#include "pa_trace.h"

#define PRINT(x)   { printf x; fflush(stdout); }
#define ERR_RPT(x) PRINT(x)
#define DBUG(x)    /* PRINT(x) */
#define DBUGX(x)   /* PRINT(x) */

#define BAD_DEVICE_ID (-1)

#define MIN_LATENCY_MSEC   (100)
#define MIN_TIMEOUT_MSEC   (100)
#define MAX_TIMEOUT_MSEC   (1000)

/************************************************* Definitions ********/
#ifdef __linux__
 #define DEVICE_NAME_BASE            "/dev/dsp"
#else
 #define DEVICE_NAME_BASE            "/dev/audio"
#endif

#define MAX_CHARS_DEVNAME           (32)
#define MAX_SAMPLE_RATES            (10)
typedef struct internalPortAudioDevice
{
    struct internalPortAudioDevice *pad_Next; /* Singly linked list. */
    double          pad_SampleRates[MAX_SAMPLE_RATES]; /* for pointing to from pad_Info */
    char            pad_DeviceName[MAX_CHARS_DEVNAME];
    PaDeviceInfo    pad_Info;
}
internalPortAudioDevice;

/* Define structure to contain all OSS and Linux specific data. */
typedef struct PaHostSoundControl
{
    int              pahsc_OutputHandle;
    int              pahsc_InputHandle;
    int              pahsc_AudioPriority;          /* priority of background audio thread */
    pthread_t        pahsc_AudioThread;            /* background audio thread */
    int              pahsc_IsAudioThreadValid;     /* Is pahsc_AudioThread valid?*/    pid_t            pahsc_AudioThreadPID;         /* background audio thread */
    pthread_t        pahsc_WatchDogThread;         /* highest priority thread that protects system */
    int              pahsc_IsWatchDogThreadValid;  /* Is pahsc_WatchDogThread valid?*/
    int              pahsc_WatchDogRun;            /* Ask WatchDog to stop. */
    pthread_t        pahsc_CanaryThread;           /* low priority thread that detects abuse by audio */
    int              pahsc_IsCanaryThreadValid;    /* Is pahsc_CanaryThread valid?*/
    struct timeval   pahsc_CanaryTime;
    int              pahsc_CanaryRun;              /* Ask Canary to stop. */
    short           *pahsc_NativeInputBuffer;
    short           *pahsc_NativeOutputBuffer;
    unsigned int     pahsc_BytesPerInputBuffer;    /* native buffer size in bytes */
    unsigned int     pahsc_BytesPerOutputBuffer;   /* native buffer size in bytes */
    /* For measuring CPU utilization. */
    struct timeval   pahsc_EntryTime;
    double           pahsc_InverseMicrosPerBuffer; /* 1/Microseconds of real-time audio per user buffer. */

   /* For calculating stream time */
    int              pahsc_LastPosPtr;
    double           pahsc_LastStreamBytes;
}
PaHostSoundControl;

/************************************************* Prototypes **********/

internalPortAudioDevice *Pa_GetInternalDevice( PaDeviceID id );
PaError Pa_QueryDevices( void );
PaError Pa_QueryDevice( const char *deviceName, internalPortAudioDevice *pad );
PaError Pa_SetupDeviceFormat( int devHandle, int numChannels, int sampleRate );
PaError Pa_SetupInputDeviceFormat( int devHandle, int numChannels, int sampleRate );
PaError Pa_SetupOutputDeviceFormat( int devHandle, int numChannels, int sampleRate );
void Pa_SetLatency( int devHandle, int numBuffers, int framesPerBuffer, int channelsPerFrame  );
void Pa_UpdateStreamTime(PaHostSoundControl *pahsc);
int Pa_FlushStream(int devHandle);
