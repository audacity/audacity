#ifndef PORT_AUDIO_H
#define PORT_AUDIO_H

#ifdef __cplusplus
extern "C"
{
#endif /* __cplusplus */

/*
 * $Id: portaudio.h,v 1.6 2003-03-02 08:01:35 dmazzoni Exp $
 * PortAudio Portable Real-Time Audio Library
 * PortAudio API Header File
 * Latest version available at: http://www.audiomulch.com/portaudio/
 *
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

typedef int PaError;
typedef enum {
    paNoError = 0,

    paHostError = -10000,
    paInvalidChannelCount,
    paInvalidSampleRate,
    paInvalidDeviceId,
    paInvalidFlag,
    paSampleFormatNotSupported,
    paBadIODeviceCombination,
    paInsufficientMemory,
    paBufferTooBig,
    paBufferTooSmall,
    paNullCallback,
    paBadStreamPtr,
    paTimedOut,
    paInternalError,
    paDeviceUnavailable
} PaErrorNum;

/*
 Pa_Initialize() is the library initialisation function - call this before
 using the library.

*/

PaError Pa_Initialize( void );

/*
 Pa_Terminate() is the library termination function - call this after
 using the library.

*/

PaError Pa_Terminate( void );

/*
 Pa_GetHostError() returns a host specific error code.
 This can be called after receiving a PortAudio error code of paHostError.

*/

long Pa_GetHostError( void );

/*
 Pa_GetErrorText() translates the supplied PortAudio error number
 into a human readable message.
 
*/

const char *Pa_GetErrorText( PaError errnum );

/*
 Sample formats
 
 These are formats used to pass sound data between the callback and the
 stream. Each device has a "native" format which may be used when optimum
 efficiency or control over conversion is required.
 
 Formats marked "always available" are supported (emulated) by all 
 PortAudio implementations.
 
 The floating point representation (paFloat32) uses +1.0 and -1.0 as the 
 maximum and minimum respectively.

 paUInt8 is an unsigned 8 bit format where 128 is considered "ground"

*/

typedef unsigned long PaSampleFormat;
#define paFloat32      ((PaSampleFormat) (1<<0)) /*always available*/
#define paInt16        ((PaSampleFormat) (1<<1)) /*always available*/
#define paInt32        ((PaSampleFormat) (1<<2)) /*always available*/
#define paInt24        ((PaSampleFormat) (1<<3))
#define paPackedInt24  ((PaSampleFormat) (1<<4))
#define paInt8         ((PaSampleFormat) (1<<5))
#define paUInt8        ((PaSampleFormat) (1<<6))
#define paCustomFormat ((PaSampleFormat) (1<<16))

/*
 Device enumeration mechanism.
 
 Device ids range from 0 to Pa_CountDevices()-1.
 
 Devices may support input, output or both.

*/

typedef int PaDeviceID;
#define paNoDevice -1

int Pa_CountDevices( void );

typedef struct
{
    int structVersion;
    const char *name;
    int maxInputChannels;
    int maxOutputChannels;
    /* Number of discrete rates, or -1 if range supported. */
    int numSampleRates;
    /* Array of supported sample rates, or {min,max} if range supported. */
    const double *sampleRates;
    PaSampleFormat nativeSampleFormats;
}
PaDeviceInfo;

/*
 Pa_GetDefaultInputDeviceID(), Pa_GetDefaultOutputDeviceID() return the
 default device ids for input and output respectively, or paNoDevice if
 no device is available.
 The result can be passed to Pa_OpenStream().
 
 On the PC, the user can specify a default device by
 setting an environment variable. For example, to use device #1.
 
  set PA_RECOMMENDED_OUTPUT_DEVICE=1
 
 The user should first determine the available device ids by using
 the supplied application "pa_devs".

*/

PaDeviceID Pa_GetDefaultInputDeviceID( void );
PaDeviceID Pa_GetDefaultOutputDeviceID( void );



/*
 Pa_GetDeviceInfo() returns a pointer to an immutable PaDeviceInfo structure
 for the device specified.
 If the device parameter is out of range the function returns NULL.

 PortAudio manages the memory referenced by the returned pointer, the client
 must not manipulate or free the memory. The pointer is only guaranteed to be
 valid between calls to Pa_Initialize() and Pa_Terminate().

*/

const PaDeviceInfo* Pa_GetDeviceInfo( PaDeviceID device );

/*
 PaTimestamp is used to represent a continuous sample clock with arbitrary
 start time that can be used for syncronization. The type is used for the
 outTime argument to the PortAudioCallback and as the result of Pa_StreamTime()

*/

typedef double PaTimestamp;

/*
 PortAudioCallback is implemented by PortAudio clients.
 
 inputBuffer and outputBuffer are arrays of interleaved samples,
 the format, packing and number of channels used by the buffers are
 determined by parameters to Pa_OpenStream() (see below).
 
 framesPerBuffer is the number of sample frames to be processed by the callback.
 
 outTime is the time in samples when the buffer(s) processed by
 this callback will begin being played at the audio output.
 See also Pa_StreamTime()
 
 userData is the value of a user supplied pointer passed to Pa_OpenStream()
 intended for storing synthesis data etc.
 
 return value:
 The callback can return a non-zero value to stop the stream. This may be
 useful in applications such as soundfile players where a specific duration
 of output is required. However, it is not necessary to utilise this mechanism
 as StopStream() will also terminate the stream. A callback returning a
 non-zero value must fill the entire outputBuffer.
 
 NOTE: None of the other stream functions may be called from within the
 callback function except for Pa_GetCPULoad().

*/

typedef int (PortAudioCallback)(
    void *inputBuffer, void *outputBuffer,
    unsigned long framesPerBuffer,
    PaTimestamp outTime, void *userData );


/*
 Stream flags
 
 These flags may be supplied (ored together) in the streamFlags argument to
 the Pa_OpenStream() function.

*/

#define   paNoFlag      (0)
#define   paClipOff     (1<<0)   /* disable default clipping of out of range samples */
#define   paDitherOff   (1<<1)   /* disable default dithering */
#define   paPlatformSpecificFlags (0x00010000)
typedef   unsigned long PaStreamFlags;

/*
 A single PortAudioStream provides multiple channels of real-time
 input and output audio streaming to a client application.
 Pointers to PortAudioStream objects are passed between PortAudio functions.
*/

typedef void PortAudioStream;
#define PaStream PortAudioStream

/*
 Pa_OpenStream() opens a stream for either input, output or both.
 
 stream is the address of a PortAudioStream pointer which will receive
 a pointer to the newly opened stream.
 
 inputDevice is the id of the device used for input (see PaDeviceID above.)
 inputDevice may be paNoDevice to indicate that an input device is not required.
 
 numInputChannels is the number of channels of sound to be delivered to the
 callback. It can range from 1 to the value of maxInputChannels in the
 PaDeviceInfo record for the device specified by the inputDevice parameter.
 If inputDevice is paNoDevice numInputChannels is ignored.
 
 inputSampleFormat is the sample format of inputBuffer provided to the callback
 function. inputSampleFormat may be any of the formats described by the
 PaSampleFormat enumeration (see above). PortAudio guarantees support for
 the device's native formats (nativeSampleFormats in the device info record)
 and additionally 16 and 32 bit integer and 32 bit floating point formats.
 Support for other formats is implementation defined.
 
 inputDriverInfo is a pointer to an optional driver specific data structure
 containing additional information for device setup or stream processing.
 inputDriverInfo is never required for correct operation. If not used
 inputDriverInfo should be NULL.
 
 outputDevice is the id of the device used for output (see PaDeviceID above.)
 outputDevice may be paNoDevice to indicate that an output device is not required.
 
 numOutputChannels is the number of channels of sound to be supplied by the
 callback. See the definition of numInputChannels above for more details.
 
 outputSampleFormat is the sample format of the outputBuffer filled by the
 callback function. See the definition of inputSampleFormat above for more
 details.
 
 outputDriverInfo is a pointer to an optional driver specific data structure
 containing additional information for device setup or stream processing.
 outputDriverInfo is never required for correct operation. If not used
 outputDriverInfo should be NULL.
 
 sampleRate is the desired sampleRate. For full-duplex streams it is the
 sample rate for both input and output
 
 framesPerBuffer is the length in sample frames of all internal sample buffers
 used for communication with platform specific audio routines. Wherever
 possible this corresponds to the framesPerBuffer parameter passed to the
 callback function.
 
 numberOfBuffers is the number of buffers used for multibuffered communication
 with the platform specific audio routines. If you pass zero, then an optimum
 value will be chosen for you internally. This parameter is provided only
 as a guide - and does not imply that an implementation must use multibuffered
 i/o when reliable double buffering is available (such as SndPlayDoubleBuffer()
 on the Macintosh.)
 
 streamFlags may contain a combination of flags ORed together.
 These flags modify the behaviour of the streaming process. Some flags may only
 be relevant to certain buffer formats.
 
 callback is a pointer to a client supplied function that is responsible
 for processing and filling input and output buffers (see above for details.)
 
 userData is a client supplied pointer which is passed to the callback
 function. It could for example, contain a pointer to instance data necessary
 for processing the audio buffers.
 
 return value:
 Upon success Pa_OpenStream() returns PaNoError and places a pointer to a
 valid PortAudioStream in the stream argument. The stream is inactive (stopped).
 If a call to Pa_OpenStream() fails a non-zero error code is returned (see
 PaError above) and the value of stream is invalid.
 
*/

PaError Pa_OpenStream( PortAudioStream** stream,
                       PaDeviceID inputDevice,
                       int numInputChannels,
                       PaSampleFormat inputSampleFormat,
                       void *inputDriverInfo,
                       PaDeviceID outputDevice,
                       int numOutputChannels,
                       PaSampleFormat outputSampleFormat,
                       void *outputDriverInfo,
                       double sampleRate,
                       unsigned long framesPerBuffer,
                       unsigned long numberOfBuffers,
                       PaStreamFlags streamFlags,
                       PortAudioCallback *callback,
                       void *userData );


/*
 Pa_OpenDefaultStream() is a simplified version of Pa_OpenStream() that opens
 the default input and/or output devices. Most parameters have identical meaning
 to their Pa_OpenStream() counterparts, with the following exceptions:
 
 If either numInputChannels or numOutputChannels is 0 the respective device
 is not opened. This has the same effect as passing paNoDevice in the device
 arguments to Pa_OpenStream().
 
 sampleFormat applies to both the input and output buffers.

*/

PaError Pa_OpenDefaultStream( PortAudioStream** stream,
                              int numInputChannels,
                              int numOutputChannels,
                              PaSampleFormat sampleFormat,
                              double sampleRate,
                              unsigned long framesPerBuffer,
                              unsigned long numberOfBuffers,
                              PortAudioCallback *callback,
                              void *userData );

/*
 Pa_CloseStream() closes an audio stream, flushing any pending buffers.

*/

PaError Pa_CloseStream( PortAudioStream* );

/*
 Pa_StartStream() and Pa_StopStream() begin and terminate audio processing.
 Pa_StopStream() waits until all pending audio buffers have been played.
 Pa_AbortStream() stops playing immediately without waiting for pending
 buffers to complete.
    
*/

PaError Pa_StartStream( PortAudioStream *stream );

PaError Pa_StopStream( PortAudioStream *stream );

PaError Pa_AbortStream( PortAudioStream *stream );

/*
 Pa_StreamActive() returns one (1) when the stream is active (ie playing
 or recording audio), zero (0) when not playing, or a negative error number
 if the stream is invalid.
 The stream is active between calls to Pa_StartStream() and Pa_StopStream(),
 but may also become inactive if the callback returns a non-zero value.
 In the latter case, the stream is considered inactive after the last
 buffer has finished playing.
 
*/

PaError Pa_StreamActive( PortAudioStream *stream );

/*
 Pa_StreamTime() returns the current output time in samples for the stream.
 This time may be used as a time reference (for example synchronizing audio to
 MIDI).
 
*/

PaTimestamp Pa_StreamTime( PortAudioStream *stream );

/*
 Pa_GetCPULoad() returns the CPU Load for the stream.
 The "CPU Load" is a fraction of total CPU time consumed by the stream's
 audio processing routines including, but not limited to the client supplied
 callback.
 A value of 0.5 would imply that PortAudio and the sound generating
 callback was consuming roughly 50% of the available CPU time.
 This function may be called from the callback function or the application.
 
*/

double Pa_GetCPULoad( PortAudioStream* stream );

/*
 Pa_GetMinNumBuffers() returns the minimum number of buffers required by
 the current host based on minimum latency.
 On the PC, for the DirectSound implementation, latency can be optionally set
 by user by setting an environment variable.
 For example, to set latency to 200 msec, put:
 
    set PA_MIN_LATENCY_MSEC=200
 
 in the AUTOEXEC.BAT file and reboot.
 If the environment variable is not set, then the latency will be determined
 based on the OS. Windows NT has higher latency than Win95.
 
*/

int Pa_GetMinNumBuffers( int framesPerBuffer, double sampleRate );

/*
 Pa_Sleep() puts the caller to sleep for at least 'msec' milliseconds.
 You may sleep longer than the requested time so don't rely on this for
 accurate musical timing.
 
 Pa_Sleep() is provided as a convenience for authors of portable code (such as
 the tests and examples in the PortAudio distribution.)
 
*/

void Pa_Sleep( long msec );

/*
 Pa_GetSampleSize() returns the size in bytes of a single sample in the
 supplied PaSampleFormat, or paSampleFormatNotSupported if the format is
 no supported.
  
*/

PaError Pa_GetSampleSize( PaSampleFormat format );


#ifdef __cplusplus
}
#endif /* __cplusplus */
#endif /* PORT_AUDIO_H */
