/*
 * $Id: pa_mac.c,v 1.7 2003-03-02 08:01:36 dmazzoni Exp $
 * Portable Audio I/O Library for Macintosh
 *
 * Based on the Open Source API proposed by Ross Bencina
 * Copyright (c) 1999-2000 Phil Burk
 *
 * Special thanks to Chris Rolfe for his many helpful suggestions, bug fixes,
 * and code contributions.
 * Thanks also to Tue Haste Andersen, Alberto Ricci, Nico Wald,
 * Roelf Toxopeus and Tom Erbe for testing the code and making
 * numerous suggestions.
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
 */
/* Modification History
   PLB20010415 - ScanInputDevices was setting sDefaultOutputDeviceID instead of sDefaultInputDeviceID
   PLB20010415 - Device Scan was crashing for anything other than siBadSoundInDevice, but some Macs may return other errors!
   PLB20010420 - Fix TIMEOUT in record mode.
   PLB20010420 - Change CARBON_COMPATIBLE to TARGET_API_MAC_CARBON
   PLB20010907 - Pass unused event to WaitNextEvent to prevent Mac OSX crash. Thanks Dominic Mazzoni.
   PLB20010908 - Use requested number of input channels. Thanks Dominic Mazzoni.
   PLB20011009 - Use NewSndCallBackUPP() for CARBON
   PLB20020417 - I used to call Pa_GetMinNumBuffers() which doesn't take into account the
                 variable minFramesPerHostBuffer. Now I call PaMac_GetMinNumBuffers() which will
                 give lower latency when virtual memory is turned off.
                 Thanks Kristoffer Jensen and Georgios Marentakis for spotting this bug.
   PLB20020423 - Use new method to calculate CPU load similar to other ports. Based on num frames calculated.
                 Fixed Pa_StreamTime(). Now estimates how many frames have played based on MicroSecond timer.
                 Added PA_MAX_USAGE_ALLOWED to prevent Mac from hanging when CPU load approaches 100%.
   PLB20020424 - Fixed return value in Pa_StreamTime
   PLB20020612 - Fix allocation error on Mac 8600 by casting *nameH as uchar* so that we get a proper Str255 length.
*/

/*
COMPATIBILITY
This Macintosh implementation is designed for use with Mac OS 7, 8 and
9 on PowerMacs, and OS X if compiled with CARBON
 
OUTPUT
A circular array of CmpSoundHeaders is used as a queue. For low latency situations
there will only be two small buffers used. For higher latency, more and larger buffers
may be used.
To play the sound we use SndDoCommand() with bufferCmd. Each buffer is followed
by a callbackCmd which informs us when the buffer has been processsed.
 
INPUT
The SndInput Manager SPBRecord call is used for sound input. If only
input is used, then the PA user callback is called from the Input completion proc.
For full-duplex, or output only operation, the PA callback is called from the
HostBuffer output completion proc. In that case, input sound is passed to the
callback by a simple FIFO.
 
TODO:
O- Add support for native sample data formats other than int16.
O- Review buffer sizing. Should it be based on result of siDeviceBufferInfo query?
O- Determine default devices somehow.
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <memory.h>
#include <math.h>

/* Mac specific includes */
#include "OSUtils.h"
#include <MacTypes.h>
#include <Math64.h>
#include <Errors.h>
#include <Sound.h>
#include <SoundInput.h>
#include <SoundComponents.h>
#include <Devices.h>
#include <DateTimeUtils.h>
#include <Timer.h>
#include <Gestalt.h>

#include "portaudio.h"
#include "pa_host.h"
#include "pa_trace.h"

#ifndef FALSE
 #define FALSE  (0)
 #define TRUE   (!FALSE)
#endif

/* #define TARGET_API_MAC_CARBON (1) */

/*
 * Define maximum CPU load that will be allowed. User callback will
 * be skipped if load exceeds this limit. This is to prevent the Mac
 * from hanging when the CPU is hogged by the sound thread.
 * On my PowerBook G3, the mac hung when I used 94% of CPU ( usage = 0.94 ).
 */
#define PA_MAX_USAGE_ALLOWED    (0.92)

/* Debugging output macros. */
#define PRINT(x) { printf x; fflush(stdout); }
#define ERR_RPT(x) PRINT(x)
#define DBUG(x)   /* PRINT(x) */
#define DBUGX(x)  /* PRINT(x) */

#define MAC_PHYSICAL_FRAMES_PER_BUFFER   (512)  /* Minimum number of stereo frames per SoundManager double buffer. */
#define MAC_VIRTUAL_FRAMES_PER_BUFFER   (4096) /* Need this many when using Virtual Memory for recording. */
#define PA_MIN_NUM_HOST_BUFFERS            (2)
#define PA_MAX_NUM_HOST_BUFFERS           (16)   /* Do not exceed!! */
#define PA_MAX_DEVICE_INFO                (32)

/* Conversions for 16.16 fixed point code. */
#define DoubleToUnsignedFixed(x) ((UnsignedFixed) ((x) * 65536.0))
#define UnsignedFixedToDouble(fx) (((double)(fx)) * (1.0/(1<<16)))

/************************************************************************************/
/****************** Structures ******************************************************/
/************************************************************************************/
/* Use for passing buffers from input callback to output callback for processing. */
typedef struct MultiBuffer
{
    char    *buffers[PA_MAX_NUM_HOST_BUFFERS];
    int      numBuffers;
    int      nextWrite;
    int      nextRead;
}
MultiBuffer;

/* Define structure to contain all Macintosh specific data. */
typedef struct PaHostSoundControl
{
    UInt64                  pahsc_EntryCount;
	double                  pahsc_InverseMicrosPerHostBuffer; /* 1/Microseconds of real-time audio per user buffer. */

    /* Use char instead of Boolean for atomic operation. */
    volatile char           pahsc_IsRecording;   /* Recording in progress. Set by foreground. Cleared by background. */
    volatile char           pahsc_StopRecording; /* Signal sent to background. */
    volatile char           pahsc_IfInsideCallback;
    /* Input */
    SPB                     pahsc_InputParams;
    SICompletionUPP         pahsc_InputCompletionProc;
    MultiBuffer             pahsc_InputMultiBuffer;
    int32                   pahsc_BytesPerInputHostBuffer;
    int32                   pahsc_InputRefNum;
    /* Output */
    CmpSoundHeader          pahsc_SoundHeaders[PA_MAX_NUM_HOST_BUFFERS];
    int32                   pahsc_BytesPerOutputHostBuffer;
    SndChannelPtr           pahsc_Channel;
    SndCallBackUPP          pahsc_OutputCompletionProc;
    int32                   pahsc_NumOutsQueued;
    int32                   pahsc_NumOutsPlayed;
    PaTimestamp             pahsc_NumFramesDone;
    UInt64                  pahsc_WhenFramesDoneIncremented;
    /* Init Time -------------- */
    int32                   pahsc_NumHostBuffers;
    int32                   pahsc_FramesPerHostBuffer;
    int32                   pahsc_UserBuffersPerHostBuffer;
    int32                   pahsc_MinFramesPerHostBuffer; /* Can vary depending on virtual memory usage. */
}
PaHostSoundControl;

/* Mac specific device information. */
typedef struct internalPortAudioDevice
{
    long                    pad_DeviceRefNum;
    long                    pad_DeviceBufferSize;
    Component               pad_Identifier;
    PaDeviceInfo            pad_Info;
}
internalPortAudioDevice;

/************************************************************************************/
/****************** Data ************************************************************/
/************************************************************************************/
static int                 sNumDevices = 0;
static internalPortAudioDevice sDevices[PA_MAX_DEVICE_INFO] = { 0 };
static int32               sPaHostError = 0;
static int                 sDefaultOutputDeviceID;
static int                 sDefaultInputDeviceID;

/************************************************************************************/
/****************** Prototypes ******************************************************/
/************************************************************************************/
static PaError PaMac_TimeSlice( internalPortAudioStream   *past,  int16 *macOutputBufPtr );
static PaError PaMac_CallUserLoop( internalPortAudioStream   *past, int16 *outPtr );
static PaError PaMac_RecordNext( internalPortAudioStream   *past );
static void    PaMac_StartLoadCalculation( internalPortAudioStream   *past );
static int     PaMac_GetMinNumBuffers( int minFramesPerHostBuffer, int framesPerBuffer, double sampleRate );
static double *PaMac_GetSampleRatesFromHandle ( int numRates, Handle h );
static PaError PaMac_ScanInputDevices( void );
static PaError PaMac_ScanOutputDevices( void );
static PaError PaMac_QueryOutputDeviceInfo( Component identifier, internalPortAudioDevice *ipad );
static PaError PaMac_QueryInputDeviceInfo( Str255 deviceName, internalPortAudioDevice *ipad );
static void    PaMac_InitSoundHeader( internalPortAudioStream   *past, CmpSoundHeader *sndHeader );
static void    PaMac_EndLoadCalculation( internalPortAudioStream   *past );
static void    PaMac_PlayNext ( internalPortAudioStream *past, int index );
static long    PaMac_FillNextOutputBuffer( internalPortAudioStream   *past, int index );
static pascal void PaMac_InputCompletionProc(SPBPtr recParams);
static pascal void PaMac_OutputCompletionProc (SndChannelPtr theChannel, SndCommand * theCmd);
static PaError PaMac_BackgroundManager( internalPortAudioStream   *past, int index );
long PaHost_GetTotalBufferFrames( internalPortAudioStream   *past );
static int     Mac_IsVirtualMemoryOn( void );
static void    PToCString(unsigned char* inString, char* outString);
static void    CToPString(char *inString, unsigned char* outString);
char *MultiBuffer_GetNextWriteBuffer( MultiBuffer *mbuf );
char *MultiBuffer_GetNextReadBuffer( MultiBuffer *mbuf );
int   MultiBuffer_GetNextReadIndex( MultiBuffer *mbuf );
int   MultiBuffer_GetNextWriteIndex( MultiBuffer *mbuf );
int   MultiBuffer_IsWriteable(  MultiBuffer *mbuf );
int   MultiBuffer_IsReadable(  MultiBuffer *mbuf );
void  MultiBuffer_AdvanceReadIndex(  MultiBuffer *mbuf );
void  MultiBuffer_AdvanceWriteIndex(  MultiBuffer *mbuf );

/*************************************************************************
** Simple FIFO index control for multiple buffers.
** Read and Write indices range from 0 to 2N-1.
** This allows us to distinguish between full and empty.
*/
char *MultiBuffer_GetNextWriteBuffer( MultiBuffer *mbuf )
{
    return mbuf->buffers[mbuf->nextWrite % mbuf->numBuffers];
}
char *MultiBuffer_GetNextReadBuffer( MultiBuffer *mbuf )
{
    return mbuf->buffers[mbuf->nextRead % mbuf->numBuffers];
}
int MultiBuffer_GetNextReadIndex( MultiBuffer *mbuf )
{
    return mbuf->nextRead % mbuf->numBuffers;
}
int MultiBuffer_GetNextWriteIndex( MultiBuffer *mbuf )
{
    return mbuf->nextWrite % mbuf->numBuffers;
}

int MultiBuffer_IsWriteable(  MultiBuffer *mbuf )
{
    int bufsFull = mbuf->nextWrite - mbuf->nextRead;
    if( bufsFull < 0 ) bufsFull += (2 * mbuf->numBuffers);
    return (bufsFull < mbuf->numBuffers);
}
int MultiBuffer_IsReadable(  MultiBuffer *mbuf )
{
    int bufsFull = mbuf->nextWrite - mbuf->nextRead;
    if( bufsFull < 0 ) bufsFull += (2 * mbuf->numBuffers);
    return (bufsFull > 0);
}
void MultiBuffer_AdvanceReadIndex(  MultiBuffer *mbuf )
{
    int temp = mbuf->nextRead + 1;
    mbuf->nextRead = (temp >= (2 * mbuf->numBuffers)) ? 0 : temp;
}
void MultiBuffer_AdvanceWriteIndex(  MultiBuffer *mbuf )
{
    int temp = mbuf->nextWrite + 1;
    mbuf->nextWrite = (temp >= (2 * mbuf->numBuffers)) ? 0 : temp;
}

/*************************************************************************
** String Utility by Chris Rolfe
*/
static void PToCString(unsigned char* inString, char* outString)
{
    long i;
    for(i=0; i<inString[0]; i++)  /* convert Pascal to C string */
        outString[i] = inString[i+1];
    outString[i]=0;
}

/*************************************************************************
** String Utility by Dominic Mazzoni
*/
static void CToPString(char* inString, unsigned char* outString)
{
    long len = strlen(inString);
    long i;

    if (len > 255)
        len = 255;

    /* Length is stored in first char of Pascal string */
    outString[0] = (unsigned char)len;
    for(i=0; i<len; i++)
        outString[i+1] = inString[i];
}

/*************************************************************************/
PaError PaHost_Term( void )
{
    int           i;
    PaDeviceInfo *dev;
    double       *rates;
    /* Free any allocated sample rate arrays. */
    for( i=0; i<sNumDevices; i++ )
    {
        dev =  &sDevices[i].pad_Info;
        rates = (double *) dev->sampleRates;
        if( (rates != NULL) ) free( rates ); /* MEM_011 */
        dev->sampleRates = NULL;
        if( dev->name != NULL ) free( (void *) dev->name ); /* MEM_010 */
        dev->name = NULL;
    }
    sNumDevices = 0;
    return paNoError;
}

/*************************************************************************
 PaHost_Init() is the library initialization function - call this before
    using the library.
*/
PaError PaHost_Init( void )
{
    PaError err;
    NumVersionVariant version;

    version.parts = SndSoundManagerVersion();
    DBUG(("SndSoundManagerVersion = 0x%x\n", version.whole));

    /* Have we already initialized the device info? */
    err = (PaError) Pa_CountDevices();
    if( err < 0 ) return err;
    else return paNoError;
}

/*************************************************************************
 PaMac_ScanOutputDevices() queries the properties of all output devices.
*/
static PaError PaMac_ScanOutputDevices( void )
{
    PaError       err;
    Component     identifier=0;
    ComponentDescription criteria = { kSoundOutputDeviceType, 0, 0, 0, 0 };
    long       numComponents, i;

    /* Search the system linked list for output components  */
    numComponents = CountComponents (&criteria);
    identifier = 0;
    sDefaultOutputDeviceID = sNumDevices; /* FIXME - query somehow */
    for (i = 0; i < numComponents; i++)
    {
        /* passing nil returns first matching component. */
        identifier = FindNextComponent( identifier, &criteria);
        sDevices[sNumDevices].pad_Identifier = identifier;

        /* Set up for default OUTPUT devices. */
        err = PaMac_QueryOutputDeviceInfo( identifier, &sDevices[sNumDevices] );
        if( err < 0 ) return err;
        else  sNumDevices++;

    }

    return paNoError;
}

/*************************************************************************
 PaMac_ScanInputDevices() queries the properties of all input devices.
*/
static PaError PaMac_ScanInputDevices( void )
{
    Str255     deviceName;
    int        count;
    Handle     iconHandle;
    PaError    err;
    OSErr      oserr;
    count = 1;
    sDefaultInputDeviceID = sNumDevices; /* FIXME - query somehow */ /* PLB20010415 - was setting sDefaultOutputDeviceID */
    while(true)
    {
        /* Thanks Chris Rolfe and Alberto Ricci for this trick. */
        oserr = SPBGetIndexedDevice(count++, deviceName, &iconHandle);
        DBUG(("PaMac_ScanInputDevices: SPBGetIndexedDevice returned %d\n", oserr ));
#if 1
        /* PLB20010415 - was giving error for anything other than siBadSoundInDevice, but some Macs may return other errors! */
        if(oserr != noErr) break; /* Some type of error is expected when count > devices */
#else
        if(oserr == siBadSoundInDevice)
        {  /* it's expected when count > devices */
            oserr = noErr;
            break;
        }
        if(oserr != noErr)
        {
            ERR_RPT(("ERROR: SPBGetIndexedDevice(%d,,) returned %d\n", count-1, oserr ));
            sPaHostError = oserr;
            return paHostError;
        }
#endif
        DisposeHandle(iconHandle);   /* Don't need the icon */

        err = PaMac_QueryInputDeviceInfo( deviceName, &sDevices[sNumDevices] );
        DBUG(("PaMac_ScanInputDevices: PaMac_QueryInputDeviceInfo returned %d\n", err ));
        if( err < 0 ) return err;
        else if( err == 1 ) sNumDevices++;
    }

    return paNoError;
}

/* Sample rate info returned by using siSampleRateAvailable selector in SPBGetDeviceInfo() */
/* Thanks to Chris Rolfe for help with this query. */
#pragma options align=mac68k
typedef  struct
{
    int16                   numRates;
    UnsignedFixed           (**rates)[]; /* Handle created by SPBGetDeviceInfo */
}
SRateInfo;
#pragma options align=reset

/*************************************************************************
** PaMac_QueryOutputDeviceInfo()
** Query information about a named output device.
** Clears contents of ipad and writes info based on queries.
** Return one if OK,
**        zero if device cannot be used,
**        or negative error.
*/
static PaError PaMac_QueryOutputDeviceInfo( Component identifier, internalPortAudioDevice *ipad )
{
    int     len;
    OSErr   err;
    PaDeviceInfo *dev =  &ipad->pad_Info;
    SRateInfo srinfo = {0};
    int     numRates;
    ComponentDescription tempD;
    Handle nameH=nil, infoH=nil, iconH=nil;

    memset( ipad, 0, sizeof(internalPortAudioDevice) );

    dev->structVersion = 1;
    dev->maxInputChannels = 0;
    dev->maxOutputChannels = 2;
    dev->nativeSampleFormats = paInt16; /* FIXME - query to see if 24 or 32 bit data can be handled. */

    /* Get sample rates supported. */
    err = GetSoundOutputInfo(identifier, siSampleRateAvailable, (Ptr) &srinfo);
    if(err != noErr)
    {
        ERR_RPT(("Error in PaMac_QueryOutputDeviceInfo: GetSoundOutputInfo siSampleRateAvailable returned %d\n", err ));
        goto error;
    }
    numRates = srinfo.numRates;
    DBUG(("PaMac_QueryOutputDeviceInfo: srinfo.numRates = %d\n", srinfo.numRates ));
    if( numRates == 0 )
    {
        dev->numSampleRates = -1;
        numRates = 2;
    }
    else
    {
        dev->numSampleRates = numRates;
    }
    dev->sampleRates = PaMac_GetSampleRatesFromHandle( numRates, (Handle) srinfo.rates );
    if(dev->sampleRates == NULL)
    {
    	DBUG(("PaMac_QueryOutputDeviceInfo: PaMac_GetSampleRatesFromHandle alloc failed.\n"));
    	return paInsufficientMemory;
    }
    
    /* SPBGetDeviceInfo created the handle, but it's OUR job to release it. */
    DisposeHandle((Handle) srinfo.rates);

    /* Device name */
    /*  we pass an existing handle for the component name;
     we don't care about the info (type, subtype, etc.) or icon, so set them to nil */
	DBUG(("PaMac_QueryOutputDeviceInfo: get component name.\n"));
    infoH = nil;
    iconH = nil;
    nameH = NewHandle(0);
    if(nameH == nil) return paInsufficientMemory;
    err = GetComponentInfo(identifier, &tempD, nameH, infoH, iconH);
    if (err)
    {
        ERR_RPT(("Error in PaMac_QueryOutputDeviceInfo: GetComponentInfo returned %d\n", err ));
        goto error;
    }
    /* Cast as uchar* so that we get a proper pascal string length. */
	len = ((unsigned char *)(*nameH))[0] + 1;  /* PLB20020612 - fix allocation error on Mac 8600 */
	DBUG(("PaMac_QueryOutputDeviceInfo: new len = %d\n", len ));

    dev->name = (char *) malloc(len);  /* MEM_010 */
    if( dev->name == NULL )
    {
        DisposeHandle(nameH);
        return paInsufficientMemory;
    }
    else
    {
        PToCString((unsigned char *)(*nameH), (char *) dev->name);
        DisposeHandle(nameH);
    }

    DBUG(("PaMac_QueryOutputDeviceInfo: dev->name = %s\n", dev->name ));
    return paNoError;

error:
    sPaHostError = err;
    return paHostError;

}

/*************************************************************************
** PaMac_QueryInputDeviceInfo()
** Query information about a named input device.
** Clears contents of ipad and writes info based on queries.
** Return one if OK,
**        zero if device cannot be used,
**        or negative error.
*/
static PaError PaMac_QueryInputDeviceInfo( Str255 deviceName, internalPortAudioDevice *ipad )
{
    PaError result = paNoError;
    int     len;
    OSErr   err;
    long    mRefNum = 0;
    long    tempL;
    int16   tempS;
    Fixed   tempF;
    PaDeviceInfo *dev =  &ipad->pad_Info;
    SRateInfo srinfo = {0};
    int     numRates;

    memset( ipad, 0, sizeof(internalPortAudioDevice) );
    dev->maxOutputChannels = 0;

    /* Open device based on name. If device is in use, it may not be able to open in write mode. */
    err = SPBOpenDevice( deviceName, siWritePermission, &mRefNum);
    if (err)
    {
        /* If device is in use, it may not be able to open in write mode so try read mode. */
        err = SPBOpenDevice( deviceName, siReadPermission, &mRefNum);
        if (err)
        {
            ERR_RPT(("Error in PaMac_QueryInputDeviceInfo: SPBOpenDevice returned %d\n", err ));
            sPaHostError = err;
            return paHostError;
        }
    }

    /* Define macros for printing out device info. */
#define PrintDeviceInfo(selector,var) \
    err = SPBGetDeviceInfo(mRefNum, selector, (Ptr) &var); \
    if (err) { \
        DBUG(("query %s failed\n", #selector )); \
    }\
    else { \
        DBUG(("query %s = 0x%x\n", #selector, var )); \
    }

    PrintDeviceInfo( siContinuous, tempS );
    PrintDeviceInfo( siAsync, tempS );
    PrintDeviceInfo( siNumberChannels, tempS );
    PrintDeviceInfo( siSampleSize, tempS );
    PrintDeviceInfo( siSampleRate, tempF );
    PrintDeviceInfo( siChannelAvailable, tempS );
    PrintDeviceInfo( siActiveChannels, tempL );
    PrintDeviceInfo( siDeviceBufferInfo, tempL );

    err = SPBGetDeviceInfo(mRefNum, siActiveChannels, (Ptr) &tempL);
    if (err == 0) DBUG(("%s = 0x%x\n", "siActiveChannels", tempL ));
    /* Can we use this device? */
    err = SPBGetDeviceInfo(mRefNum, siAsync, (Ptr) &tempS);
    if (err)
    {
        ERR_RPT(("Error in PaMac_QueryInputDeviceInfo: SPBGetDeviceInfo siAsync returned %d\n", err ));
        goto error;
    }
    if( tempS == 0 ) goto useless; /* Does not support async recording so forget about it. */

    err = SPBGetDeviceInfo(mRefNum, siChannelAvailable, (Ptr) &tempS);
    if (err)
    {
        ERR_RPT(("Error in PaMac_QueryInputDeviceInfo: SPBGetDeviceInfo siChannelAvailable returned %d\n", err ));
        goto error;
    }
    dev->maxInputChannels = tempS;

    /* Get sample rates supported. */
    err = SPBGetDeviceInfo(mRefNum, siSampleRateAvailable, (Ptr) &srinfo);
    if (err)
    {
        ERR_RPT(("Error in PaMac_QueryInputDeviceInfo: SPBGetDeviceInfo siSampleRateAvailable returned %d\n", err ));
        goto error;
    }

    numRates = srinfo.numRates;
    DBUG(("numRates = 0x%x\n", numRates ));
    if( numRates == 0 )
    {
        dev->numSampleRates = -1;
        numRates = 2;
    }
    else
    {
        dev->numSampleRates = numRates;
    }
    dev->sampleRates = PaMac_GetSampleRatesFromHandle( numRates, (Handle) srinfo.rates );
    /* SPBGetDeviceInfo created the handle, but it's OUR job to release it. */
    DisposeHandle((Handle) srinfo.rates);

    /* Get size of device buffer. */
    err = SPBGetDeviceInfo(mRefNum, siDeviceBufferInfo, (Ptr) &tempL);
    if (err)
    {
        ERR_RPT(("Error in PaMac_QueryInputDeviceInfo: SPBGetDeviceInfo siDeviceBufferInfo returned %d\n", err ));
        goto error;
    }
    ipad->pad_DeviceBufferSize = tempL;
    DBUG(("siDeviceBufferInfo = %d\n", tempL ));

    /* Set format based on sample size. */
    err = SPBGetDeviceInfo(mRefNum, siSampleSize, (Ptr) &tempS);
    if (err)
    {
        ERR_RPT(("Error in PaMac_QueryInputDeviceInfo: SPBGetDeviceInfo siSampleSize returned %d\n", err ));
        goto error;
    }
    switch( tempS )
    {
    case 0x0020:
        dev->nativeSampleFormats = paInt32;  /* FIXME - warning, code probably won't support this! */
        break;
    case 0x0010:
    default: /* FIXME - What about other formats? */
        dev->nativeSampleFormats = paInt16;
        break;
    }
    DBUG(("nativeSampleFormats = %d\n", dev->nativeSampleFormats ));

    /* Device name */
    len = deviceName[0] + 1;  /* Get length of Pascal string */
    dev->name = (char *) malloc(len);  /* MEM_010 */
    if( dev->name == NULL )
    {
        result = paInsufficientMemory;
        goto cleanup;
    }
    PToCString(deviceName, (char *) dev->name);
    DBUG(("deviceName = %s\n", dev->name ));
    result = (PaError) 1;
    /* All done so close up device. */
cleanup:
    if( mRefNum )  SPBCloseDevice(mRefNum);
    return result;

error:
    if( mRefNum )  SPBCloseDevice(mRefNum);
    sPaHostError = err;
    return paHostError;

useless:
    if( mRefNum )  SPBCloseDevice(mRefNum);
    return (PaError) 0;
}

/*************************************************************************
** Allocate a double array and fill it with listed sample rates.
*/
static double * PaMac_GetSampleRatesFromHandle ( int numRates, Handle h )
{
    OSErr   err = noErr;
    SInt8   hState;
    int     i;
    UnsignedFixed *fixedRates;
    double *rates = (double *) malloc( numRates * sizeof(double) ); /* MEM_011 */
    if( rates == NULL ) return NULL;
    /* Save and restore handle state as suggested by TechNote at:
            http://developer.apple.com/technotes/tn/tn1122.html
    */
    hState = HGetState (h);
    if (!(err = MemError ()))
    {
        HLock (h);
        if (!(err = MemError ( )))
        {
            fixedRates = (UInt32 *) *h;
            for( i=0; i<numRates; i++ )
            {
                rates[i] = UnsignedFixedToDouble(fixedRates[i]);
            }

            HSetState (h,hState);
            err = MemError ( );
        }
    }
    if( err )
    {
        free( rates );
        ERR_RPT(("Error in PaMac_GetSampleRatesFromHandle = %d\n", err ));
    }
    return rates;
}

/*************************************************************************/
int Pa_CountDevices()
{
    PaError err;
    DBUG(("Pa_CountDevices()\n"));
    /* If no devices, go find some. */
    if( sNumDevices <= 0 )
    {
        err = PaMac_ScanOutputDevices();
        if( err != paNoError ) goto error;
        err = PaMac_ScanInputDevices();
        if( err != paNoError ) goto error;
    }
    return sNumDevices;

error:
    PaHost_Term();
    DBUG(("Pa_CountDevices: returns %d\n", err ));
    return err;

}

/*************************************************************************/
const PaDeviceInfo* Pa_GetDeviceInfo( PaDeviceID id )
{
    if( (id < 0) || ( id >= Pa_CountDevices()) ) return NULL;
    return &sDevices[id].pad_Info;
}
/*************************************************************************/
PaDeviceID Pa_GetDefaultInputDeviceID( void )
{
    return sDefaultInputDeviceID;
}

/*************************************************************************/
PaDeviceID Pa_GetDefaultOutputDeviceID( void )
{
    return sDefaultOutputDeviceID;
}

/********************************* BEGIN CPU UTILIZATION MEASUREMENT ****/
static void PaMac_StartLoadCalculation( internalPortAudioStream   *past )
{
    PaHostSoundControl *pahsc = (PaHostSoundControl *) past->past_DeviceData;
    UnsignedWide widePad;
    if( pahsc == NULL ) return;
    /* Query system timer for usage analysis and to prevent overuse of CPU. */
    Microseconds( &widePad );
    pahsc->pahsc_EntryCount = UnsignedWideToUInt64( widePad );
}

/******************************************************************************
** Measure fractional CPU load based on real-time it took to calculate
** buffers worth of output.
*/
/**************************************************************************/
static void PaMac_EndLoadCalculation( internalPortAudioStream   *past )
{
    UnsignedWide widePad;
    UInt64    currentCount;
    long      usecsElapsed;
    double    newUsage;
    PaHostSoundControl *pahsc = (PaHostSoundControl *) past->past_DeviceData;
    if( pahsc == NULL ) return;
    
    /* Measure CPU utilization during this callback. Note that this calculation
    ** assumes that we had the processor the whole time.
    */
#define LOWPASS_COEFFICIENT_0   (0.95)
#define LOWPASS_COEFFICIENT_1   (0.99999 - LOWPASS_COEFFICIENT_0)
    Microseconds( &widePad );
    currentCount = UnsignedWideToUInt64( widePad );

	usecsElapsed = (long) U64Subtract(currentCount, pahsc->pahsc_EntryCount);
	
        /* Use inverse because it is faster than the divide. */
	newUsage =  usecsElapsed * pahsc->pahsc_InverseMicrosPerHostBuffer;
	
	past->past_Usage = (LOWPASS_COEFFICIENT_0 * past->past_Usage) +
                           (LOWPASS_COEFFICIENT_1 * newUsage);
 
}

/***********************************************************************
** Called by Pa_StartStream()
*/
PaError PaHost_StartInput( internalPortAudioStream   *past )
{
    PaHostSoundControl *pahsc = (PaHostSoundControl *) past->past_DeviceData;
    pahsc->pahsc_IsRecording = 0;
    pahsc->pahsc_StopRecording = 0;
    pahsc->pahsc_InputMultiBuffer.nextWrite = 0;
    pahsc->pahsc_InputMultiBuffer.nextRead = 0;
    return PaMac_RecordNext( past );
}

/***********************************************************************
** Called by Pa_StopStream().
** May be called during error recovery or cleanup code
** so protect against NULL pointers.
*/
PaError PaHost_StopInput( internalPortAudioStream   *past, int abort )
{
    int32   timeOutMsec;
    PaError result = paNoError;
    OSErr   err = 0;
    long    mRefNum;
    PaHostSoundControl *pahsc = (PaHostSoundControl *) past->past_DeviceData;
    if( pahsc == NULL ) return paNoError;

    (void) abort;

    mRefNum = pahsc->pahsc_InputRefNum;

    DBUG(("PaHost_StopInput: mRefNum = %d\n", mRefNum ));
    if( mRefNum )
    {
        DBUG(("PaHost_StopInput: pahsc_IsRecording = %d\n", pahsc->pahsc_IsRecording ));
        if( pahsc->pahsc_IsRecording )
        {
            /* PLB20010420 - Fix TIMEOUT in record mode. */
            pahsc->pahsc_StopRecording = 1; /* Request that we stop recording. */
            err = SPBStopRecording(mRefNum);
            DBUG(("PaHost_StopInput: then pahsc_IsRecording = %d\n", pahsc->pahsc_IsRecording ));

            /* Calculate timeOut longer than longest time it could take to play one buffer. */
            timeOutMsec = (int32) ((1500.0 * pahsc->pahsc_FramesPerHostBuffer) / past->past_SampleRate);
            /* Keep querying sound channel until it is no longer busy playing. */
            while( !err && pahsc->pahsc_IsRecording && (timeOutMsec > 0))
            {
                Pa_Sleep(20);
                timeOutMsec -= 20;
            }
            if( timeOutMsec <= 0 )
            {
                ERR_RPT(("PaHost_StopInput: timed out!\n"));
                return paTimedOut;
            }
        }
    }
    if( err )
    {
        sPaHostError = err;
        result = paHostError;
    }

    DBUG(("PaHost_StopInput: finished.\n", mRefNum ));
    return result;
}

/***********************************************************************/
static void PaMac_InitSoundHeader( internalPortAudioStream   *past, CmpSoundHeader *sndHeader )
{
    sndHeader->numChannels = past->past_NumOutputChannels;
    sndHeader->sampleRate = DoubleToUnsignedFixed(past->past_SampleRate);
    sndHeader->loopStart = 0;
    sndHeader->loopEnd = 0;
    sndHeader->encode = cmpSH;
    sndHeader->baseFrequency = kMiddleC;
    sndHeader->markerChunk = nil;
    sndHeader->futureUse2 = nil;
    sndHeader->stateVars = nil;
    sndHeader->leftOverSamples = nil;
    sndHeader->compressionID = 0;
    sndHeader->packetSize = 0;
    sndHeader->snthID = 0;
    sndHeader->sampleSize = 8 * sizeof(int16); // FIXME - might be 24 or 32 bits some day;
    sndHeader->sampleArea[0] = 0;
    sndHeader->format = kSoundNotCompressed;
}

static void SetFramesDone( PaHostSoundControl *pahsc, PaTimestamp framesDone )
{
	UnsignedWide     now;
 	Microseconds( &now );
 	pahsc->pahsc_NumFramesDone = framesDone;
 	pahsc->pahsc_WhenFramesDoneIncremented = UnsignedWideToUInt64( now );
}

/***********************************************************************/
PaError PaHost_StartOutput( internalPortAudioStream   *past )
{
    SndCommand  pauseCommand;
    SndCommand  resumeCommand;
    int          i;
    OSErr         error;
    PaHostSoundControl *pahsc = (PaHostSoundControl *) past->past_DeviceData;
    if( pahsc == NULL ) return paInternalError;
    if( pahsc->pahsc_Channel == NULL ) return paInternalError;

    past->past_StopSoon = 0;
    past->past_IsActive = 1;
    pahsc->pahsc_NumOutsQueued = 0;
    pahsc->pahsc_NumOutsPlayed = 0;
    
    SetFramesDone( pahsc, 0.0 );

    /* Pause channel so it does not do back ground processing while we are still filling the queue. */
    pauseCommand.cmd = pauseCmd;
    pauseCommand.param1 = pauseCommand.param2 = 0;
    error = SndDoCommand (pahsc->pahsc_Channel, &pauseCommand, true);
    if (noErr != error) goto exit;

    /* Queue all of the buffers so we start off full. */
    for (i = 0; i<pahsc->pahsc_NumHostBuffers; i++)
    {
        PaMac_PlayNext( past, i );
    }
    
    /* Resume channel now that the queue is full. */
    resumeCommand.cmd = resumeCmd;
    resumeCommand.param1 = resumeCommand.param2 = 0;
    error = SndDoImmediate( pahsc->pahsc_Channel, &resumeCommand );
    if (noErr != error) goto exit;

    return paNoError;
exit:
    past->past_IsActive = 0;
    sPaHostError = error;
    ERR_RPT(("Error in PaHost_StartOutput: SndDoCommand returned %d\n", error ));
    return paHostError;
}

/*******************************************************************/
long PaHost_GetTotalBufferFrames( internalPortAudioStream   *past )
{
    PaHostSoundControl *pahsc = (PaHostSoundControl *) past->past_DeviceData;
    return (long) (pahsc->pahsc_NumHostBuffers * pahsc->pahsc_FramesPerHostBuffer);
}

/***********************************************************************
** Called by Pa_StopStream().
** May be called during error recovery or cleanup code
** so protect against NULL pointers.
*/
PaError PaHost_StopOutput( internalPortAudioStream   *past, int abort )
{
    int32 timeOutMsec;
    PaHostSoundControl *pahsc = (PaHostSoundControl *) past->past_DeviceData;
    if( pahsc == NULL ) return paNoError;
    if( pahsc->pahsc_Channel == NULL ) return paNoError;

    DBUG(("PaHost_StopOutput()\n"));
    if( past->past_IsActive == 0 ) return paNoError;

    /* Set flags for callback function to see. */
    if( abort ) past->past_StopNow = 1;
    past->past_StopSoon = 1;
    /* Calculate timeOut longer than longest time it could take to play all buffers. */
    timeOutMsec = (int32) ((1500.0 * PaHost_GetTotalBufferFrames( past )) / past->past_SampleRate);
    /* Keep querying sound channel until it is no longer busy playing. */
    while( past->past_IsActive && (timeOutMsec > 0))
    {
        Pa_Sleep(20);
        timeOutMsec -= 20;
    }
    if( timeOutMsec <= 0 )
    {
        ERR_RPT(("PaHost_StopOutput: timed out!\n"));
        return paTimedOut;
    }
    else return paNoError;
}

/***********************************************************************/
PaError PaHost_StartEngine( internalPortAudioStream   *past )
{
    (void) past; /* Prevent unused variable warnings. */
    return paNoError;
}

/***********************************************************************/
PaError PaHost_StopEngine( internalPortAudioStream   *past, int abort )
{
    (void) past; /* Prevent unused variable warnings. */
    (void) abort; /* Prevent unused variable warnings. */
    return paNoError;
}
/***********************************************************************/
PaError PaHost_StreamActive( internalPortAudioStream   *past )
{
    PaHostSoundControl *pahsc = (PaHostSoundControl *) past->past_DeviceData;
    return (PaError) ( past->past_IsActive + pahsc->pahsc_IsRecording );
}
int Mac_IsVirtualMemoryOn( void )
{
    long  attr;
    OSErr result = Gestalt( gestaltVMAttr, &attr );
    DBUG(("gestaltVMAttr : 0x%x\n", attr ));
    return ((attr >> gestaltVMHasPagingControl ) & 1);
}

/*******************************************************************
* Determine number of host Buffers
* and how many User Buffers we can put into each host buffer.
*/
static void PaHost_CalcNumHostBuffers( internalPortAudioStream *past )
{
    PaHostSoundControl *pahsc = (PaHostSoundControl *) past->past_DeviceData;
    int32  minNumBuffers;
    int32  minFramesPerHostBuffer;
    int32  minTotalFrames;
    int32  userBuffersPerHostBuffer;
    int32  framesPerHostBuffer;
    int32  numHostBuffers;
    
    minFramesPerHostBuffer = pahsc->pahsc_MinFramesPerHostBuffer;
    minFramesPerHostBuffer = (minFramesPerHostBuffer + 7) & ~7;
    DBUG(("PaHost_CalcNumHostBuffers: minFramesPerHostBuffer = %d\n", minFramesPerHostBuffer ));
    
    /* Determine number of user buffers based on minimum latency. */
	/* PLB20020417 I used to call Pa_GetMinNumBuffers() which doesn't take into account the
	**    variable minFramesPerHostBuffer. Now I call PaMac_GetMinNumBuffers() which will
	**    gove lower latency when virtual memory is turned off. */
    /* minNumBuffers = Pa_GetMinNumBuffers( past->past_FramesPerUserBuffer, past->past_SampleRate ); WRONG */
    minNumBuffers = PaMac_GetMinNumBuffers( minFramesPerHostBuffer, past->past_FramesPerUserBuffer, past->past_SampleRate );
    
    past->past_NumUserBuffers = ( minNumBuffers > past->past_NumUserBuffers ) ? minNumBuffers : past->past_NumUserBuffers;
    DBUG(("PaHost_CalcNumHostBuffers: min past_NumUserBuffers = %d\n", past->past_NumUserBuffers ));
    minTotalFrames = past->past_NumUserBuffers * past->past_FramesPerUserBuffer;
    
    /* We cannot make the buffers too small because they may not get serviced quickly enough. */
    if( (int32) past->past_FramesPerUserBuffer < minFramesPerHostBuffer )
    {
        userBuffersPerHostBuffer =
            (minFramesPerHostBuffer + past->past_FramesPerUserBuffer - 1) /
            past->past_FramesPerUserBuffer;
    }
    else
    {
        userBuffersPerHostBuffer = 1;
    }
    framesPerHostBuffer = past->past_FramesPerUserBuffer * userBuffersPerHostBuffer;
    
    /* Calculate number of host buffers needed. Round up to cover minTotalFrames. */
    numHostBuffers = (minTotalFrames + framesPerHostBuffer - 1) / framesPerHostBuffer;
    /* Make sure we have enough host buffers. */
    if( numHostBuffers < PA_MIN_NUM_HOST_BUFFERS)
    {
        numHostBuffers = PA_MIN_NUM_HOST_BUFFERS;
    }
    else
    {
        /* If we have too many host buffers, try to put more user buffers in a host buffer. */
        while(numHostBuffers > PA_MAX_NUM_HOST_BUFFERS)
        {
            userBuffersPerHostBuffer += 1;
            framesPerHostBuffer = past->past_FramesPerUserBuffer * userBuffersPerHostBuffer;
            numHostBuffers = (minTotalFrames + framesPerHostBuffer - 1) / framesPerHostBuffer;
        }
    }

    pahsc->pahsc_UserBuffersPerHostBuffer = userBuffersPerHostBuffer;
    pahsc->pahsc_FramesPerHostBuffer = framesPerHostBuffer;
    pahsc->pahsc_NumHostBuffers = numHostBuffers;
    DBUG(("PaHost_CalcNumHostBuffers: pahsc_UserBuffersPerHostBuffer = %d\n", pahsc->pahsc_UserBuffersPerHostBuffer ));
    DBUG(("PaHost_CalcNumHostBuffers: pahsc_NumHostBuffers = %d\n", pahsc->pahsc_NumHostBuffers ));
    DBUG(("PaHost_CalcNumHostBuffers: pahsc_FramesPerHostBuffer = %d\n", pahsc->pahsc_FramesPerHostBuffer ));
    DBUG(("PaHost_CalcNumHostBuffers: past_NumUserBuffers = %d\n", past->past_NumUserBuffers ));
}

/*******************************************************************/
PaError PaHost_OpenStream( internalPortAudioStream   *past )
{
    OSErr             err;
    PaError             result = paHostError;
    PaHostSoundControl *pahsc;
    int                 i;
    /* Allocate and initialize host data. */
    pahsc = (PaHostSoundControl *) PaHost_AllocateFastMemory(sizeof(PaHostSoundControl));
    if( pahsc == NULL )
    {
        return paInsufficientMemory;
    }
    past->past_DeviceData = (void *) pahsc;

    /* If recording, and virtual memory is turned on, then use bigger buffers to prevent glitches. */
    if( (past->past_NumInputChannels > 0)  && Mac_IsVirtualMemoryOn() )
    {
        pahsc->pahsc_MinFramesPerHostBuffer = MAC_VIRTUAL_FRAMES_PER_BUFFER;
    }
    else
    {
        pahsc->pahsc_MinFramesPerHostBuffer = MAC_PHYSICAL_FRAMES_PER_BUFFER;
    }

    PaHost_CalcNumHostBuffers( past );
    
    /* Setup constants for CPU load measurement. */
    pahsc->pahsc_InverseMicrosPerHostBuffer = past->past_SampleRate / (1000000.0 * 	pahsc->pahsc_FramesPerHostBuffer);

    /* ------------------ OUTPUT */
    if( past->past_NumOutputChannels > 0 )
    {
        /* Create sound channel to which we can send commands. */
        pahsc->pahsc_Channel = 0L;
        err = SndNewChannel(&pahsc->pahsc_Channel, sampledSynth, 0, nil); /* FIXME - use kUseOptionalOutputDevice if not default. */
        if(err != 0)
        {
            ERR_RPT(("Error in PaHost_OpenStream: SndNewChannel returned 0x%x\n", err ));
            goto error;
        }

        /* Install our callback function pointer straight into the sound channel structure */
        /* Use new CARBON name for callback procedure. */
#if TARGET_API_MAC_CARBON
        pahsc->pahsc_OutputCompletionProc = NewSndCallBackUPP(PaMac_OutputCompletionProc);
#else
        pahsc->pahsc_OutputCompletionProc = NewSndCallBackProc(PaMac_OutputCompletionProc);
#endif

        pahsc->pahsc_Channel->callBack = pahsc->pahsc_OutputCompletionProc;

        pahsc->pahsc_BytesPerOutputHostBuffer = pahsc->pahsc_FramesPerHostBuffer * past->past_NumOutputChannels * sizeof(int16);
        for (i = 0; i<pahsc->pahsc_NumHostBuffers; i++)
        {
            char *buf = (char *)PaHost_AllocateFastMemory(pahsc->pahsc_BytesPerOutputHostBuffer);
            if (buf == NULL)
            {
                ERR_RPT(("Error in PaHost_OpenStream: could not allocate output buffer. Size = \n", pahsc->pahsc_BytesPerOutputHostBuffer ));
                goto memerror;
            }

            PaMac_InitSoundHeader( past, &pahsc->pahsc_SoundHeaders[i] );
            pahsc->pahsc_SoundHeaders[i].samplePtr = buf;
            pahsc->pahsc_SoundHeaders[i].numFrames = (unsigned long) pahsc->pahsc_FramesPerHostBuffer;

        }
    }
#ifdef SUPPORT_AUDIO_CAPTURE
    /* ------------------ INPUT */
    /* Use double buffer scheme that matches output. */
    if( past->past_NumInputChannels > 0 )
    {
        int16   tempS;
        long    tempL;
        Fixed   tempF;
        long    mRefNum;
        Str255 namePString;
#if TARGET_API_MAC_CARBON
        pahsc->pahsc_InputCompletionProc = NewSICompletionUPP((SICompletionProcPtr)PaMac_InputCompletionProc);
#else
        pahsc->pahsc_InputCompletionProc = NewSICompletionProc((ProcPtr)PaMac_InputCompletionProc);
#endif
        pahsc->pahsc_BytesPerInputHostBuffer = pahsc->pahsc_FramesPerHostBuffer * past->past_NumInputChannels * sizeof(int16);
        for (i = 0; i<pahsc->pahsc_NumHostBuffers; i++)
        {
            char *buf = (char *) PaHost_AllocateFastMemory(pahsc->pahsc_BytesPerInputHostBuffer);
            if ( buf == NULL )
            {
                ERR_RPT(("PaHost_OpenStream: could not allocate input  buffer. Size = \n", pahsc->pahsc_BytesPerInputHostBuffer ));
                goto memerror;
            }
            pahsc->pahsc_InputMultiBuffer.buffers[i] = buf;
        }
        pahsc->pahsc_InputMultiBuffer.numBuffers = pahsc->pahsc_NumHostBuffers;

        // err = SPBOpenDevice( (const unsigned char *) &noname, siWritePermission, &mRefNum);
        CToPString((char *)sDevices[past->past_InputDeviceID].pad_Info.name, namePString);
        err = SPBOpenDevice(namePString, siWritePermission, &mRefNum);

        if (err) goto error;
        pahsc->pahsc_InputRefNum = mRefNum;
        DBUG(("PaHost_OpenStream: mRefNum = %d\n", mRefNum ));

        /* Set input device characteristics. */
        tempS = 1;
        err = SPBSetDeviceInfo(mRefNum, siContinuous, (Ptr) &tempS);
        if (err)
        {
            ERR_RPT(("Error in PaHost_OpenStream: SPBSetDeviceInfo siContinuous returned %d\n", err ));
            goto error;
        }

        tempL = 0x03;
        err = SPBSetDeviceInfo(mRefNum, siActiveChannels, (Ptr) &tempL);
        if (err)
        {
            DBUG(("PaHost_OpenStream: setting siActiveChannels returned 0x%x. Error ignored.\n", err ));
        }

        /* PLB20010908 - Use requested number of input channels. Thanks Dominic Mazzoni. */
        tempS = past->past_NumInputChannels;
        err = SPBSetDeviceInfo(mRefNum, siNumberChannels, (Ptr) &tempS);
        if (err)
        {
            ERR_RPT(("Error in PaHost_OpenStream: SPBSetDeviceInfo siNumberChannels returned %d\n", err ));
            goto error;
        }

        tempF = ((unsigned long)past->past_SampleRate) << 16;
        err = SPBSetDeviceInfo(mRefNum, siSampleRate, (Ptr) &tempF);
        if (err)
        {
            ERR_RPT(("Error in PaHost_OpenStream: SPBSetDeviceInfo siSampleRate returned %d\n", err ));
            goto error;
        }

        /* Setup record-parameter block */
        pahsc->pahsc_InputParams.inRefNum          = mRefNum;
        pahsc->pahsc_InputParams.milliseconds      = 0;   // not used
        pahsc->pahsc_InputParams.completionRoutine = pahsc->pahsc_InputCompletionProc;
        pahsc->pahsc_InputParams.interruptRoutine  = 0;
        pahsc->pahsc_InputParams.userLong          = (long) past;
        pahsc->pahsc_InputParams.unused1           = 0;
    }
#endif /* SUPPORT_AUDIO_CAPTURE */
    DBUG(("PaHost_OpenStream: complete.\n"));
    return paNoError;

error:
    PaHost_CloseStream( past );
    ERR_RPT(("PaHost_OpenStream: sPaHostError =  0x%x.\n", err ));
    sPaHostError = err;
    return paHostError;

memerror:
    PaHost_CloseStream( past );
    return paInsufficientMemory;
}

/***********************************************************************
** Called by Pa_CloseStream().
** May be called during error recovery or cleanup code
** so protect against NULL pointers.
*/
PaError PaHost_CloseStream( internalPortAudioStream   *past )
{
    PaError result = paNoError;
    OSErr   err = 0;
    int     i;
    PaHostSoundControl *pahsc;

    DBUG(("PaHost_CloseStream( 0x%x )\n", past ));

    if( past == NULL ) return paBadStreamPtr;

    pahsc = (PaHostSoundControl *) past->past_DeviceData;
    if( pahsc == NULL ) return paNoError;

    if( past->past_NumOutputChannels > 0 )
    {
        /* TRUE means flush now instead of waiting for quietCmd to be processed. */
        if( pahsc->pahsc_Channel != NULL ) SndDisposeChannel(pahsc->pahsc_Channel, TRUE);
        {
            for (i = 0; i<pahsc->pahsc_NumHostBuffers; i++)
            {
                Ptr p = (Ptr) pahsc->pahsc_SoundHeaders[i].samplePtr;
                if( p != NULL ) PaHost_FreeFastMemory( p, pahsc->pahsc_BytesPerOutputHostBuffer );
            }
        }
    }

    if( past->past_NumInputChannels > 0 )
    {
        if( pahsc->pahsc_InputRefNum )
        {
            err = SPBCloseDevice(pahsc->pahsc_InputRefNum);
            pahsc->pahsc_InputRefNum = 0;
            if( err )
            {
                sPaHostError = err;
                result = paHostError;
            }
        }
        {
            for (i = 0; i<pahsc->pahsc_InputMultiBuffer.numBuffers; i++)
            {
                Ptr p = (Ptr) pahsc->pahsc_InputMultiBuffer.buffers[i];
                if( p != NULL ) PaHost_FreeFastMemory( p, pahsc->pahsc_BytesPerInputHostBuffer );
            }
        }
    }

    past->past_DeviceData = NULL;
    PaHost_FreeFastMemory( pahsc, sizeof(PaHostSoundControl) );

    DBUG(("PaHost_CloseStream: complete.\n", past ));
    return result;
}
/*************************************************************************/
int Pa_GetMinNumBuffers( int framesPerUserBuffer, double sampleRate )
{
/* We use the MAC_VIRTUAL_FRAMES_PER_BUFFER because we might be recording.
** This routine doesn't have enough information to determine the best value
** and is being depracated. */
    return PaMac_GetMinNumBuffers( MAC_VIRTUAL_FRAMES_PER_BUFFER, framesPerUserBuffer, sampleRate );
}
/*************************************************************************/
static int PaMac_GetMinNumBuffers( int minFramesPerHostBuffer, int framesPerUserBuffer, double sampleRate )
{
    int minUserPerHost = ( minFramesPerHostBuffer + framesPerUserBuffer - 1) / framesPerUserBuffer;
    int numBufs = PA_MIN_NUM_HOST_BUFFERS * minUserPerHost;
    if( numBufs < PA_MIN_NUM_HOST_BUFFERS ) numBufs = PA_MIN_NUM_HOST_BUFFERS;
    (void) sampleRate;
    return numBufs;
}

/*************************************************************************/
void Pa_Sleep( int32 msec )
{
    EventRecord   event;
    int32 sleepTime, endTime;
    /* Convert to ticks. Round up so we sleep a MINIMUM of msec time. */
    sleepTime = ((msec * 60) + 999) / 1000;
    if( sleepTime < 1 ) sleepTime = 1;
    endTime = TickCount() + sleepTime;
    do
    {
        DBUGX(("Sleep for %d ticks.\n", sleepTime ));
        /* Use WaitNextEvent() to sleep without getting events. */
        /* PLB20010907 - Pass unused event to WaitNextEvent instead of NULL to prevent
         * Mac OSX crash. Thanks Dominic Mazzoni. */
        WaitNextEvent( 0, &event, sleepTime, NULL );
        sleepTime = endTime - TickCount();
    }
    while( sleepTime > 0 );
}
/*************************************************************************/
int32 Pa_GetHostError( void )
{
    int32 err = sPaHostError;
    sPaHostError = 0;
    return err;
}

/*************************************************************************
 * Allocate memory that can be accessed in real-time.
 * This may need to be held in physical memory so that it is not
 * paged to virtual memory.
 * This call MUST be balanced with a call to PaHost_FreeFastMemory().
 */
void *PaHost_AllocateFastMemory( long numBytes )
{
    void *addr = NewPtrClear( numBytes );
    if( (addr == NULL) || (MemError () != 0) ) return NULL;

#if (TARGET_API_MAC_CARBON == 0)
    if( HoldMemory( addr, numBytes ) != noErr )
    {
        DisposePtr( (Ptr) addr );
        return NULL;
    }
#endif
    return addr;
}

/*************************************************************************
 * Free memory that could be accessed in real-time.
 * This call MUST be balanced with a call to PaHost_AllocateFastMemory().
 */
void PaHost_FreeFastMemory( void *addr, long numBytes )
{
    if( addr == NULL ) return;
#if TARGET_API_MAC_CARBON
    (void) numBytes;
#else
    UnholdMemory( addr, numBytes );
#endif
    DisposePtr( (Ptr) addr );
}

/*************************************************************************/
PaTimestamp Pa_StreamTime( PortAudioStream *stream )
{
	PaTimestamp   framesDone1;
	PaTimestamp   framesDone2;
	UInt64        whenIncremented;
	UnsignedWide  now;
	UInt64        now64;
	long          microsElapsed;
	long          framesElapsed;
	
    PaHostSoundControl *pahsc;
    internalPortAudioStream   *past = (internalPortAudioStream *) stream;
    if( past == NULL ) return paBadStreamPtr;
    pahsc = (PaHostSoundControl *) past->past_DeviceData;
    
/* Capture information from audio thread.
 * We have to be careful that we don't get interrupted in the middle.
 * So we grab the pahsc_NumFramesDone twice and make sure it didn't change.
 */
 	do
 	{
 		framesDone1 = pahsc->pahsc_NumFramesDone;
 		whenIncremented = pahsc->pahsc_WhenFramesDoneIncremented;
 		framesDone2 = pahsc->pahsc_NumFramesDone;
 	} while( framesDone1 != framesDone2 );
 	
 /* Calculate how many microseconds have elapsed and convert to frames. */
 	Microseconds( &now );
 	now64 = UnsignedWideToUInt64( now );
 	microsElapsed = U64Subtract( now64, whenIncremented );
 	framesElapsed = microsElapsed * past->past_SampleRate * 0.000001;
 	
	return framesDone1 + framesElapsed;
}

/**************************************************************************
** Callback for Input, SPBRecord()
*/
int gRecordCounter = 0;
int gPlayCounter = 0;
pascal void PaMac_InputCompletionProc(SPBPtr recParams)
{
    PaError                    result = paNoError;
    int                        finished = 1;
    internalPortAudioStream   *past;
    PaHostSoundControl        *pahsc;

    gRecordCounter += 1; /* debug hack to see if engine running */

    /* Get our PA data from Mac structure. */
    past = (internalPortAudioStream *) recParams->userLong;
    if( past == NULL ) return;

    if( past->past_Magic != PA_MAGIC )
    {
        AddTraceMessage("PaMac_InputCompletionProc: bad MAGIC, past", (long) past );
        AddTraceMessage("PaMac_InputCompletionProc: bad MAGIC, magic", (long) past->past_Magic );
        goto error;
    }
    pahsc = (PaHostSoundControl *) past->past_DeviceData;
    past->past_NumCallbacks += 1;

    /* Have we been asked to stop recording? */
    if( (recParams->error == abortErr) || pahsc->pahsc_StopRecording ) goto error;

    /* If there are no output channels, then we need to call the user callback function from here.
     * Otherwise we will call the user code during the output completion routine.
     */
    if(past->past_NumOutputChannels == 0)
    {
		SetFramesDone( pahsc,
			pahsc->pahsc_NumFramesDone + pahsc->pahsc_FramesPerHostBuffer );
        result = PaMac_CallUserLoop( past, NULL );
    }

    /* Did user code ask us to stop? If not, issue another recording request. */
    if( (result == paNoError) && (pahsc->pahsc_StopRecording == 0) )
    {
        result = PaMac_RecordNext( past );
        if( result != paNoError ) pahsc->pahsc_IsRecording = 0;
    }
    else goto error;

    return;

error:
    pahsc->pahsc_IsRecording = 0;
    pahsc->pahsc_StopRecording = 0;
    return;
}

/***********************************************************************
** Called by either input or output completion proc.
** Grabs input data if any present, and calls PA conversion code,
** that in turn calls user code.
*/
static PaError PaMac_CallUserLoop( internalPortAudioStream   *past, int16 *outPtr )
{
    PaError             result = paNoError;
    PaHostSoundControl *pahsc = (PaHostSoundControl *) past->past_DeviceData;
    int16              *inPtr = NULL;
    int                 i;
    

    /* Advance read index for sound input FIFO here, independantly of record/write process. */
    if(past->past_NumInputChannels > 0)
    {
        if( MultiBuffer_IsReadable( &pahsc->pahsc_InputMultiBuffer ) )
        {
            inPtr = (int16 *)  MultiBuffer_GetNextReadBuffer( &pahsc->pahsc_InputMultiBuffer );
            MultiBuffer_AdvanceReadIndex( &pahsc->pahsc_InputMultiBuffer );
        }
    }

    /* Call user code enough times to fill buffer. */
    if( (inPtr != NULL) || (outPtr != NULL) )
    {
        PaMac_StartLoadCalculation( past ); /* CPU usage */

#ifdef PA_MAX_USAGE_ALLOWED
	/* If CPU usage exceeds limit, skip user callback to prevent hanging CPU. */
		if( past->past_Usage > PA_MAX_USAGE_ALLOWED )
	    {
	    	past->past_FrameCount += (PaTimestamp) pahsc->pahsc_FramesPerHostBuffer;
		}
		else
#endif
		{
		
	        for( i=0; i<pahsc->pahsc_UserBuffersPerHostBuffer; i++ )
	        {
	            result = (PaError) Pa_CallConvertInt16( past, inPtr, outPtr );
	            if( result != 0)
	            {
	                /* Recording might be in another process, so tell it to stop with a flag. */
	                pahsc->pahsc_StopRecording = pahsc->pahsc_IsRecording;
	                break;
	            }
	            /* Advance sample pointers. */
	            if(inPtr != NULL) inPtr += past->past_FramesPerUserBuffer * past->past_NumInputChannels;
	            if(outPtr != NULL) outPtr += past->past_FramesPerUserBuffer * past->past_NumOutputChannels;
	        }
	    }
		
        PaMac_EndLoadCalculation( past );
    }
    return result;
}

/***********************************************************************
** Setup next recording buffer in FIFO and issue recording request to Snd Input Manager.
*/
static PaError PaMac_RecordNext( internalPortAudioStream   *past )
{
    PaError  result = paNoError;
    OSErr     err;
    PaHostSoundControl *pahsc = (PaHostSoundControl *) past->past_DeviceData;
    /* Get pointer to next buffer to record into. */
    pahsc->pahsc_InputParams.bufferPtr  = MultiBuffer_GetNextWriteBuffer( &pahsc->pahsc_InputMultiBuffer );

    /* Advance write index if there is room. Otherwise keep writing same buffer. */
    if( MultiBuffer_IsWriteable( &pahsc->pahsc_InputMultiBuffer ) )
    {
        MultiBuffer_AdvanceWriteIndex( &pahsc->pahsc_InputMultiBuffer );
    }

    AddTraceMessage("PaMac_RecordNext: bufferPtr", (long) pahsc->pahsc_InputParams.bufferPtr );
    AddTraceMessage("PaMac_RecordNext: nextWrite", pahsc->pahsc_InputMultiBuffer.nextWrite );

    /* Setup parameters and issue an asynchronous recording request. */
    pahsc->pahsc_InputParams.bufferLength      = pahsc->pahsc_BytesPerInputHostBuffer;
    pahsc->pahsc_InputParams.count             = pahsc->pahsc_BytesPerInputHostBuffer;
    err = SPBRecord(&pahsc->pahsc_InputParams, true);
    if( err )
    {
        AddTraceMessage("PaMac_RecordNext: SPBRecord error ", err );
        sPaHostError = err;
        result = paHostError;
    }
    else
    {
        pahsc->pahsc_IsRecording = 1;
    }
    return result;
}

/**************************************************************************
** Callback for Output Playback()
** Return negative error, 0 to continue, 1 to stop.
*/
long    PaMac_FillNextOutputBuffer( internalPortAudioStream   *past, int index )
{
    PaHostSoundControl  *pahsc;
    long                 result = 0;
    int                  finished = 1;
    char                *outPtr;

    gPlayCounter += 1; /* debug hack */

    past->past_NumCallbacks += 1;
    pahsc = (PaHostSoundControl *) past->past_DeviceData;
    if( pahsc == NULL ) return -1;
    /* Are we nested?! */
    if( pahsc->pahsc_IfInsideCallback ) return 0;
    pahsc->pahsc_IfInsideCallback = 1;
    /* Get pointer to buffer to fill. */
    outPtr = pahsc->pahsc_SoundHeaders[index].samplePtr;
    /* Combine with any sound input, and call user callback. */
    result = PaMac_CallUserLoop( past, (int16 *) outPtr );

    pahsc->pahsc_IfInsideCallback = 0;
    return result;
}

/*************************************************************************************
** Called by SoundManager when ready for another buffer.
*/
static pascal void PaMac_OutputCompletionProc (SndChannelPtr theChannel, SndCommand * theCallBackCmd)
{
    internalPortAudioStream *past;
    PaHostSoundControl      *pahsc;
    (void) theChannel;
    (void) theCallBackCmd;

    /* Get our data from Mac structure. */
    past = (internalPortAudioStream *) theCallBackCmd->param2;
    if( past == NULL ) return;

    pahsc = (PaHostSoundControl *) past->past_DeviceData;
    pahsc->pahsc_NumOutsPlayed += 1;

	SetFramesDone( pahsc,
			pahsc->pahsc_NumFramesDone + pahsc->pahsc_FramesPerHostBuffer );
			
    PaMac_BackgroundManager( past, theCallBackCmd->param1 );
}

/*******************************************************************/
static PaError PaMac_BackgroundManager( internalPortAudioStream   *past, int index )
{
    PaError      result = paNoError;
    PaHostSoundControl *pahsc = (PaHostSoundControl *) past->past_DeviceData;
    /* Has someone asked us to abort by calling Pa_AbortStream()? */
    if( past->past_StopNow )
    {
        SndCommand  command;
        /* Clear the queue of any pending commands. */
        command.cmd = flushCmd;
        command.param1 = command.param2 = 0;
        SndDoImmediate( pahsc->pahsc_Channel, &command );
        /* Then stop currently playing buffer, if any. */
        command.cmd = quietCmd;
        SndDoImmediate( pahsc->pahsc_Channel, &command );
        past->past_IsActive = 0;
    }
    /* Has someone asked us to stop by calling Pa_StopStream()
     * OR has a user callback returned '1' to indicate finished.
     */
    else if( past->past_StopSoon )
    {
        if( (pahsc->pahsc_NumOutsQueued - pahsc->pahsc_NumOutsPlayed) <= 0 )
        {
            past->past_IsActive = 0; /* We're finally done. */
        }
    }
    else
    {
        PaMac_PlayNext( past, index );
    }
    return result;
}

/*************************************************************************************
** Fill next buffer with sound and queue it for playback.
*/
static void PaMac_PlayNext ( internalPortAudioStream *past, int index )
{
    OSErr                  error;
    long                     result;
    SndCommand               playCmd;
    SndCommand           callbackCmd;
    PaHostSoundControl      *pahsc = (PaHostSoundControl *) past->past_DeviceData;

    /* If this was the last buffer, or abort requested, then just be done. */
    if ( past->past_StopSoon ) goto done;
    
    /* Load buffer with sound. */
    result = PaMac_FillNextOutputBuffer ( past, index );
    if( result > 0 ) past->past_StopSoon = 1; /* Stop generating audio but wait until buffers play. */
    else if( result < 0 ) goto done;
    
    /* Play the next buffer. */
    playCmd.cmd = bufferCmd;
    playCmd.param1 = 0;
    playCmd.param2 = (long) &pahsc->pahsc_SoundHeaders[ index ];
    error = SndDoCommand (pahsc->pahsc_Channel, &playCmd, true );
    if( error != noErr ) goto gotError;
    
    /* Ask for a callback when it is done. */
    callbackCmd.cmd = callBackCmd;
    callbackCmd.param1 = index;
    callbackCmd.param2 = (long)past;
    error = SndDoCommand (pahsc->pahsc_Channel, &callbackCmd, true );
    if( error != noErr ) goto gotError;
    pahsc->pahsc_NumOutsQueued += 1;

    return;

gotError:
    sPaHostError = error;
done:
    return;
}
