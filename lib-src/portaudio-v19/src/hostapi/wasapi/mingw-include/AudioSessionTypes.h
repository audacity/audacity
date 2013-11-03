//
// AudioSessionTypes.h -- Copyright Microsoft Corporation, All Rights Reserved.
//
// Description: Type definitions used by the audio session manager RPC/COM interfaces
//
#pragma once

#ifndef __AUDIOSESSIONTYPES__
#define __AUDIOSESSIONTYPES__

#if defined(__midl)
#define MIDL_SIZE_IS(x) [size_is(x)]
#define MIDL_STRING [string]
#define MIDL_ANYSIZE_ARRAY
#else   // !defined(__midl)
#define MIDL_SIZE_IS(x)
#define MIDL_STRING
#define MIDL_ANYSIZE_ARRAY ANYSIZE_ARRAY
#endif  // defined(__midl)

//-------------------------------------------------------------------------
// Description: AudioClient share mode
//                                   
//     AUDCLNT_SHAREMODE_SHARED -    The device will be opened in shared mode and use the 
//                                   WAS format.
//     AUDCLNT_SHAREMODE_EXCLUSIVE - The device will be opened in exclusive mode and use the 
//                                   application specified format.
//
typedef enum _AUDCLNT_SHAREMODE
{ 
    AUDCLNT_SHAREMODE_SHARED, 
    AUDCLNT_SHAREMODE_EXCLUSIVE 
} AUDCLNT_SHAREMODE;

//-------------------------------------------------------------------------
// Description: AudioClient stream flags
// 
// Can be a combination of AUDCLNT_STREAMFLAGS and AUDCLNT_SYSFXFLAGS:
// 
// AUDCLNT_STREAMFLAGS (this group of flags uses the high word, w/exception of high-bit which is reserved, 0x7FFF0000):
//                                  
//     AUDCLNT_STREAMFLAGS_CROSSPROCESS - Audio policy control for this stream will be shared with 
//                                        with other process sessions that use the same audio session 
//                                        GUID.
//     AUDCLNT_STREAMFLAGS_LOOPBACK -     Initializes a renderer endpoint for a loopback audio application. 
//                                        In this mode, a capture stream will be opened on the specified 
//                                        renderer endpoint. Shared mode and a renderer endpoint is required.
//                                        Otherwise the IAudioClient::Initialize call will fail. If the 
//                                        initialize is successful, a capture stream will be available 
//                                        from the IAudioClient object.
//
//     AUDCLNT_STREAMFLAGS_EVENTCALLBACK - An exclusive mode client will supply an event handle that will be
//                                         signaled when an IRP completes (or a waveRT buffer completes) telling
//                                         it to fill the next buffer
//
//     AUDCLNT_STREAMFLAGS_NOPERSIST -    Session state will not be persisted
//
// AUDCLNT_SYSFXFLAGS (these flags use low word 0x0000FFFF):
//
//     none defined currently
//
#define AUDCLNT_STREAMFLAGS_CROSSPROCESS  0x00010000
#define AUDCLNT_STREAMFLAGS_LOOPBACK      0x00020000
#define AUDCLNT_STREAMFLAGS_EVENTCALLBACK 0x00040000
#define AUDCLNT_STREAMFLAGS_NOPERSIST     0x00080000

//-------------------------------------------------------------------------
// Description: Device share mode - sharing mode for the audio device.
//
//      DeviceShared - The device can be shared with other processes.
//      DeviceExclusive - The device will only be used by this process.
//
typedef enum _DeviceShareMode
{ 
    DeviceShared, 
    DeviceExclusive 
} DeviceShareMode;


//-------------------------------------------------------------------------
// Description: AudioSession State.
//
//      AudioSessionStateInactive - The session has no active audio streams.
//      AudioSessionStateActive - The session has active audio streams.
//      AudioSessionStateExpired - The session is dormant.
typedef enum _AudioSessionState
{
    AudioSessionStateInactive = 0,
    AudioSessionStateActive = 1,
    AudioSessionStateExpired = 2
} AudioSessionState;

#endif

