#ifndef PORT_MIDI_H
#define PORT_MIDI_H
#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/*
 * PortMidi Portable Real-Time MIDI Library
 * PortMidi API Header File
 * Latest version available at: http://sourceforge.net/projects/portmedia
 *
 * Copyright (c) 1999-2000 Ross Bencina and Phil Burk
 * Copyright (c) 2001-2006 Roger B. Dannenberg
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
 * The text above constitutes the entire PortMidi license; however, 
 * the PortMusic community also makes the following non-binding requests:
 *
 * Any person wishing to distribute modifications to the Software is
 * requested to send the modifications to the original developer so that
 * they can be incorporated into the canonical version. It is also
 * requested that these non-binding requests be included along with the 
 * license above.
 */

/* CHANGELOG FOR PORTMIDI
 *     (see ../CHANGELOG.txt)
 *
 * NOTES ON HOST ERROR REPORTING: 
 *
 *    PortMidi errors (of type PmError) are generic, system-independent errors.
 *    When an error does not map to one of the more specific PmErrors, the
 *    catch-all code pmHostError is returned. This means that PortMidi has
 *    retained a more specific system-dependent error code. The caller can
 *    get more information by calling Pm_HasHostError() to test if there is
 *    a pending host error, and Pm_GetHostErrorText() to get a text string
 *    describing the error. Host errors are reported on a per-device basis 
 *    because only after you open a device does PortMidi have a place to 
 *    record the host error code. I.e. only 
 *    those routines that receive a (PortMidiStream *) argument check and 
 *    report errors. One exception to this is that Pm_OpenInput() and 
 *    Pm_OpenOutput() can report errors even though when an error occurs,
 *    there is no PortMidiStream* to hold the error. Fortunately, both
 *    of these functions return any error immediately, so we do not really
 *    need per-device error memory. Instead, any host error code is stored
 *    in a global, pmHostError is returned, and the user can call 
 *    Pm_GetHostErrorText() to get the error message (and the invalid stream
 *    parameter will be ignored.) The functions 
 *    pm_init and pm_term do not fail or raise
 *    errors. The job of pm_init is to locate all available devices so that
 *    the caller can get information via PmDeviceInfo(). If an error occurs,
 *    the device is simply not listed as available.
 *
 *    Host errors come in two flavors:
 *      a) host error 
 *      b) host error during callback
 *    These can occur w/midi input or output devices. (b) can only happen 
 *    asynchronously (during callback routines), whereas (a) only occurs while
 *    synchronously running PortMidi and any resulting system dependent calls.
 *    Both (a) and (b) are reported by the next read or write call. You can
 *    also query for asynchronous errors (b) at any time by calling
 *    Pm_HasHostError().
 *
 * NOTES ON COMPILE-TIME SWITCHES
 *
 *    DEBUG assumes stdio and a console. Use this if you want automatic, simple
 *        error reporting, e.g. for prototyping. If you are using MFC or some 
 *        other graphical interface with no console, DEBUG probably should be
 *        undefined.
 *    PM_CHECK_ERRORS more-or-less takes over error checking for return values,
 *        stopping your program and printing error messages when an error
 *        occurs. This also uses stdio for console text I/O.
 */

#ifndef WIN32
// Linux and OS X have stdint.h
#include <stdint.h>
#else
#ifndef INT32_DEFINED
// rather than having users install a special .h file for windows, 
// just put the required definitions inline here. porttime.h uses
// these too, so the definitions are (unfortunately) duplicated there
typedef int int32_t;
typedef unsigned int uint32_t;
#define INT32_DEFINED
#endif
#endif

#ifdef _WINDLL
#define PMEXPORT __declspec(dllexport)
#else
#define PMEXPORT 
#endif

#ifndef FALSE
    #define FALSE 0
#endif
#ifndef TRUE
    #define TRUE 1
#endif

/* default size of buffers for sysex transmission: */
#define PM_DEFAULT_SYSEX_BUFFER_SIZE 1024

/** List of portmidi errors.*/
typedef enum {
    pmNoError = 0,
    pmNoData = 0, /**< A "no error" return that also indicates no data avail. */
    pmGotData = 1, /**< A "no error" return that also indicates data available */
    pmHostError = -10000,
    pmInvalidDeviceId, /** out of range or 
                        * output device when input is requested or 
                        * input device when output is requested or
                        * device is already opened 
                        */
    pmInsufficientMemory,
    pmBufferTooSmall,
    pmBufferOverflow,
    pmBadPtr, /* PortMidiStream parameter is NULL or
               * stream is not opened or
               * stream is output when input is required or
               * stream is input when output is required */
    pmBadData, /** illegal midi data, e.g. missing EOX */
    pmInternalError,
    pmBufferMaxSize /** buffer is already as large as it can be */
    /* NOTE: If you add a new error type, be sure to update Pm_GetErrorText() */
} PmError;

/**
    Pm_Initialize() is the library initialisation function - call this before
    using the library.
*/
PMEXPORT PmError Pm_Initialize( void );

/**
    Pm_Terminate() is the library termination function - call this after
    using the library.
*/
PMEXPORT PmError Pm_Terminate( void );

/**  A single PortMidiStream is a descriptor for an open MIDI device.
*/
typedef void PortMidiStream;
#define PmStream PortMidiStream

/**
    Test whether stream has a pending host error. Normally, the client finds
    out about errors through returned error codes, but some errors can occur
    asynchronously where the client does not
    explicitly call a function, and therefore cannot receive an error code.
    The client can test for a pending error using Pm_HasHostError(). If true,
    the error can be accessed and cleared by calling Pm_GetErrorText(). 
    Errors are also cleared by calling other functions that can return
    errors, e.g. Pm_OpenInput(), Pm_OpenOutput(), Pm_Read(), Pm_Write(). The
    client does not need to call Pm_HasHostError(). Any pending error will be
    reported the next time the client performs an explicit function call on 
    the stream, e.g. an input or output operation. Until the error is cleared,
    no new error codes will be obtained, even for a different stream.
*/
PMEXPORT int Pm_HasHostError( PortMidiStream * stream );


/**  Translate portmidi error number into human readable message.
    These strings are constants (set at compile time) so client has 
    no need to allocate storage
*/
PMEXPORT const char *Pm_GetErrorText( PmError errnum );

/**  Translate portmidi host error into human readable message.
    These strings are computed at run time, so client has to allocate storage.
    After this routine executes, the host error is cleared. 
*/
PMEXPORT void Pm_GetHostErrorText(char * msg, unsigned int len);

#define HDRLENGTH 50
#define PM_HOST_ERROR_MSG_LEN 256u /* any host error msg will occupy less 
                                      than this number of characters */

/**
    Device enumeration mechanism.

    Device ids range from 0 to Pm_CountDevices()-1.

*/
typedef int PmDeviceID;
#define pmNoDevice -1
typedef struct {
    int structVersion; /**< this internal structure version */ 
    const char *interf; /**< underlying MIDI API, e.g. MMSystem or DirectX */
    const char *name;   /**< device name, e.g. USB MidiSport 1x1 */
    int input; /**< true iff input is available */
    int output; /**< true iff output is available */
    int opened; /**< used by generic PortMidi code to do error checking on arguments */

} PmDeviceInfo;

/**  Get devices count, ids range from 0 to Pm_CountDevices()-1. */
PMEXPORT int Pm_CountDevices( void );
/**
    Pm_GetDefaultInputDeviceID(), Pm_GetDefaultOutputDeviceID()

    Return the default device ID or pmNoDevice if there are no devices.
    The result (but not pmNoDevice) can be passed to Pm_OpenMidi().
    
    The default device can be specified using a small application
    named pmdefaults that is part of the PortMidi distribution. This
    program in turn uses the Java Preferences object created by
    java.util.prefs.Preferences.userRoot().node("/PortMidi"); the
    preference is set by calling 
        prefs.put("PM_RECOMMENDED_OUTPUT_DEVICE", prefName);
    or  prefs.put("PM_RECOMMENDED_INPUT_DEVICE", prefName);
    
    In the statements above, prefName is a string describing the
    MIDI device in the form "interf, name" where interf identifies
    the underlying software system or API used by PortMdi to access
    devices and name is the name of the device. These correspond to 
    the interf and name fields of a PmDeviceInfo. (Currently supported
    interfaces are "MMSystem" for Win32, "ALSA" for Linux, and 
    "CoreMIDI" for OS X, so in fact, there is no choice of interface.)
    In "interf, name", the strings are actually substrings of 
    the full interface and name strings. For example, the preference 
    "Core, Sport" will match a device with interface "CoreMIDI"
    and name "In USB MidiSport 1x1". It will also match "CoreMIDI"
    and "In USB MidiSport 2x2". The devices are enumerated in device
    ID order, so the lowest device ID that matches the pattern becomes
    the default device. Finally, if the comma-space (", ") separator
    between interface and name parts of the preference is not found,
    the entire preference string is interpreted as a name, and the
    interface part is the empty string, which matches anything.

    On the MAC, preferences are stored in 
      /Users/$NAME/Library/Preferences/com.apple.java.util.prefs.plist
    which is a binary file. In addition to the pmdefaults program,
    there are utilities that can read and edit this preference file.

    On the PC, 

    On Linux, 

*/
PMEXPORT PmDeviceID Pm_GetDefaultInputDeviceID( void );
/** see PmDeviceID Pm_GetDefaultInputDeviceID() */
PMEXPORT PmDeviceID Pm_GetDefaultOutputDeviceID( void );

/**
    PmTimestamp is used to represent a millisecond clock with arbitrary
    start time. The type is used for all MIDI timestampes and clocks.
*/
typedef int32_t PmTimestamp;
typedef PmTimestamp (*PmTimeProcPtr)(void *time_info);

/** TRUE if t1 before t2 */
#define PmBefore(t1,t2) ((t1-t2) < 0)
/** 
    \defgroup grp_device Input/Output Devices Handling
    @{
*/
/**
    Pm_GetDeviceInfo() returns a pointer to a PmDeviceInfo structure
    referring to the device specified by id.
    If id is out of range the function returns NULL.

    The returned structure is owned by the PortMidi implementation and must
    not be manipulated or freed. The pointer is guaranteed to be valid
    between calls to Pm_Initialize() and Pm_Terminate().
*/
PMEXPORT const PmDeviceInfo* Pm_GetDeviceInfo( PmDeviceID id );

/**
    Pm_OpenInput() and Pm_OpenOutput() open devices.

    stream is the address of a PortMidiStream pointer which will receive
    a pointer to the newly opened stream.

    inputDevice is the id of the device used for input (see PmDeviceID above).

    inputDriverInfo is a pointer to an optional driver specific data structure
    containing additional information for device setup or handle processing.
    inputDriverInfo is never required for correct operation. If not used
    inputDriverInfo should be NULL.

    outputDevice is the id of the device used for output (see PmDeviceID above.)

    outputDriverInfo is a pointer to an optional driver specific data structure
    containing additional information for device setup or handle processing.
    outputDriverInfo is never required for correct operation. If not used
    outputDriverInfo should be NULL.

    For input, the buffersize specifies the number of input events to be 
    buffered waiting to be read using Pm_Read(). For output, buffersize 
    specifies the number of output events to be buffered waiting for output. 
    (In some cases -- see below -- PortMidi does not buffer output at all
    and merely passes data to a lower-level API, in which case buffersize
    is ignored.)
    
    latency is the delay in milliseconds applied to timestamps to determine 
    when the output should actually occur. (If latency is < 0, 0 is assumed.) 
    If latency is zero, timestamps are ignored and all output is delivered
    immediately. If latency is greater than zero, output is delayed until the
    message timestamp plus the latency. (NOTE: the time is measured relative 
    to the time source indicated by time_proc. Timestamps are absolute,
    not relative delays or offsets.) In some cases, PortMidi can obtain
    better timing than your application by passing timestamps along to the
    device driver or hardware. Latency may also help you to synchronize midi
    data to audio data by matching midi latency to the audio buffer latency.

    time_proc is a pointer to a procedure that returns time in milliseconds. It
    may be NULL, in which case a default millisecond timebase (PortTime) is 
    used. If the application wants to use PortTime, it should start the timer
    (call Pt_Start) before calling Pm_OpenInput or Pm_OpenOutput. If the
    application tries to start the timer *after* Pm_OpenInput or Pm_OpenOutput,
    it may get a ptAlreadyStarted error from Pt_Start, and the application's
    preferred time resolution and callback function will be ignored.
    time_proc result values are appended to incoming MIDI data, and time_proc
    times are used to schedule outgoing MIDI data (when latency is non-zero).

    time_info is a pointer passed to time_proc.

    Example: If I provide a timestamp of 5000, latency is 1, and time_proc
    returns 4990, then the desired output time will be when time_proc returns
    timestamp+latency = 5001. This will be 5001-4990 = 11ms from now.

    return value:
    Upon success Pm_Open() returns PmNoError and places a pointer to a
    valid PortMidiStream in the stream argument.
    If a call to Pm_Open() fails a nonzero error code is returned (see
    PMError above) and the value of port is invalid.

    Any stream that is successfully opened should eventually be closed
    by calling Pm_Close().

*/
PMEXPORT PmError Pm_OpenInput( PortMidiStream** stream,
                PmDeviceID inputDevice,
                void *inputDriverInfo,
                int32_t bufferSize,
                PmTimeProcPtr time_proc,
                void *time_info );

PMEXPORT PmError Pm_OpenOutput( PortMidiStream** stream,
                PmDeviceID outputDevice,
                void *outputDriverInfo,
                int32_t bufferSize,
                PmTimeProcPtr time_proc,
                void *time_info,
                int32_t latency );
  /** @} */

/**
   \defgroup grp_events_filters Events and Filters Handling
   @{
*/

/*  \function PmError Pm_SetFilter( PortMidiStream* stream, int32_t filters )
    Pm_SetFilter() sets filters on an open input stream to drop selected
    input types. By default, only active sensing messages are filtered.
    To prohibit, say, active sensing and sysex messages, call
    Pm_SetFilter(stream, PM_FILT_ACTIVE | PM_FILT_SYSEX);

    Filtering is useful when midi routing or midi thru functionality is being
    provided by the user application.
    For example, you may want to exclude timing messages (clock, MTC, start/stop/continue),
    while allowing note-related messages to pass.
    Or you may be using a sequencer or drum-machine for MIDI clock information but want to
    exclude any notes it may play.
 */
    
/* Filter bit-mask definitions */
/** filter active sensing messages (0xFE): */
#define PM_FILT_ACTIVE (1 << 0x0E)
/** filter system exclusive messages (0xF0): */
#define PM_FILT_SYSEX (1 << 0x00)
/** filter MIDI clock message (0xF8) */
#define PM_FILT_CLOCK (1 << 0x08)
/** filter play messages (start 0xFA, stop 0xFC, continue 0xFB) */
#define PM_FILT_PLAY ((1 << 0x0A) | (1 << 0x0C) | (1 << 0x0B))
/** filter tick messages (0xF9) */
#define PM_FILT_TICK (1 << 0x09)
/** filter undefined FD messages */
#define PM_FILT_FD (1 << 0x0D)
/** filter undefined real-time messages */
#define PM_FILT_UNDEFINED PM_FILT_FD
/** filter reset messages (0xFF) */
#define PM_FILT_RESET (1 << 0x0F)
/** filter all real-time messages */
#define PM_FILT_REALTIME (PM_FILT_ACTIVE | PM_FILT_SYSEX | PM_FILT_CLOCK | \
    PM_FILT_PLAY | PM_FILT_UNDEFINED | PM_FILT_RESET | PM_FILT_TICK)
/** filter note-on and note-off (0x90-0x9F and 0x80-0x8F */
#define PM_FILT_NOTE ((1 << 0x19) | (1 << 0x18))
/** filter channel aftertouch (most midi controllers use this) (0xD0-0xDF)*/
#define PM_FILT_CHANNEL_AFTERTOUCH (1 << 0x1D)
/** per-note aftertouch (0xA0-0xAF) */
#define PM_FILT_POLY_AFTERTOUCH (1 << 0x1A)
/** filter both channel and poly aftertouch */
#define PM_FILT_AFTERTOUCH (PM_FILT_CHANNEL_AFTERTOUCH | PM_FILT_POLY_AFTERTOUCH)
/** Program changes (0xC0-0xCF) */
#define PM_FILT_PROGRAM (1 << 0x1C)
/** Control Changes (CC's) (0xB0-0xBF)*/
#define PM_FILT_CONTROL (1 << 0x1B)
/** Pitch Bender (0xE0-0xEF*/
#define PM_FILT_PITCHBEND (1 << 0x1E)
/** MIDI Time Code (0xF1)*/
#define PM_FILT_MTC (1 << 0x01)
/** Song Position (0xF2) */
#define PM_FILT_SONG_POSITION (1 << 0x02)
/** Song Select (0xF3)*/
#define PM_FILT_SONG_SELECT (1 << 0x03)
/** Tuning request (0xF6)*/
#define PM_FILT_TUNE (1 << 0x06)
/** All System Common messages (mtc, song position, song select, tune request) */
#define PM_FILT_SYSTEMCOMMON (PM_FILT_MTC | PM_FILT_SONG_POSITION | PM_FILT_SONG_SELECT | PM_FILT_TUNE)


PMEXPORT PmError Pm_SetFilter( PortMidiStream* stream, int32_t filters );

#define Pm_Channel(channel) (1<<(channel))
/**
    Pm_SetChannelMask() filters incoming messages based on channel.
    The mask is a 16-bit bitfield corresponding to appropriate channels.
    The Pm_Channel macro can assist in calling this function.
    i.e. to set receive only input on channel 1, call with
    Pm_SetChannelMask(Pm_Channel(1));
    Multiple channels should be OR'd together, like
    Pm_SetChannelMask(Pm_Channel(10) | Pm_Channel(11))

    Note that channels are numbered 0 to 15 (not 1 to 16). Most 
    synthesizer and interfaces number channels starting at 1, but
    PortMidi numbers channels starting at 0.

    All channels are allowed by default
*/
PMEXPORT PmError Pm_SetChannelMask(PortMidiStream *stream, int mask);

/**
    Pm_Abort() terminates outgoing messages immediately
    The caller should immediately close the output port;
    this call may result in transmission of a partial midi message.
    There is no abort for Midi input because the user can simply
    ignore messages in the buffer and close an input device at
    any time.
 */
PMEXPORT PmError Pm_Abort( PortMidiStream* stream );
     
/**
    Pm_Close() closes a midi stream, flushing any pending buffers.
    (PortMidi attempts to close open streams when the application 
    exits -- this is particularly difficult under Windows.)
*/
PMEXPORT PmError Pm_Close( PortMidiStream* stream );

/**
    Pm_Synchronize() instructs PortMidi to (re)synchronize to the
    time_proc passed when the stream was opened. Typically, this
    is used when the stream must be opened before the time_proc
    reference is actually advancing. In this case, message timing
    may be erratic, but since timestamps of zero mean 
    "send immediately," initialization messages with zero timestamps
    can be written without a functioning time reference and without
    problems. Before the first MIDI message with a non-zero
    timestamp is written to the stream, the time reference must
    begin to advance (for example, if the time_proc computes time
    based on audio samples, time might begin to advance when an 
    audio stream becomes active). After time_proc return values
    become valid, and BEFORE writing the first non-zero timestamped 
    MIDI message, call Pm_Synchronize() so that PortMidi can observe
    the difference between the current time_proc value and its
    MIDI stream time. 
    
    In the more normal case where time_proc 
    values advance continuously, there is no need to call 
    Pm_Synchronize. PortMidi will always synchronize at the 
    first output message and periodically thereafter.
*/
PmError Pm_Synchronize( PortMidiStream* stream );


/**
    Pm_Message() encodes a short Midi message into a 32-bit word. If data1
    and/or data2 are not present, use zero.

    Pm_MessageStatus(), Pm_MessageData1(), and 
    Pm_MessageData2() extract fields from a 32-bit midi message.
*/
#define Pm_Message(status, data1, data2) \
         ((((data2) << 16) & 0xFF0000) | \
          (((data1) << 8) & 0xFF00) | \
          ((status) & 0xFF))
#define Pm_MessageStatus(msg) ((msg) & 0xFF)
#define Pm_MessageData1(msg) (((msg) >> 8) & 0xFF)
#define Pm_MessageData2(msg) (((msg) >> 16) & 0xFF)

typedef int32_t PmMessage; /**< see PmEvent */
/**
   All midi data comes in the form of PmEvent structures. A sysex
   message is encoded as a sequence of PmEvent structures, with each
   structure carrying 4 bytes of the message, i.e. only the first
   PmEvent carries the status byte.

   Note that MIDI allows nested messages: the so-called "real-time" MIDI 
   messages can be inserted into the MIDI byte stream at any location, 
   including within a sysex message. MIDI real-time messages are one-byte
   messages used mainly for timing (see the MIDI spec). PortMidi retains 
   the order of non-real-time MIDI messages on both input and output, but 
   it does not specify exactly how real-time messages are processed. This
   is particulary problematic for MIDI input, because the input parser 
   must either prepare to buffer an unlimited number of sysex message 
   bytes or to buffer an unlimited number of real-time messages that 
   arrive embedded in a long sysex message. To simplify things, the input
   parser is allowed to pass real-time MIDI messages embedded within a 
   sysex message, and it is up to the client to detect, process, and 
   remove these messages as they arrive.

   When receiving sysex messages, the sysex message is terminated
   by either an EOX status byte (anywhere in the 4 byte messages) or
   by a non-real-time status byte in the low order byte of the message.
   If you get a non-real-time status byte but there was no EOX byte, it 
   means the sysex message was somehow truncated. This is not
   considered an error; e.g., a missing EOX can result from the user
   disconnecting a MIDI cable during sysex transmission.

   A real-time message can occur within a sysex message. A real-time 
   message will always occupy a full PmEvent with the status byte in 
   the low-order byte of the PmEvent message field. (This implies that
   the byte-order of sysex bytes and real-time message bytes may not
   be preserved -- for example, if a real-time message arrives after
   3 bytes of a sysex message, the real-time message will be delivered
   first. The first word of the sysex message will be delivered only
   after the 4th byte arrives, filling the 4-byte PmEvent message field.
   
   The timestamp field is observed when the output port is opened with
   a non-zero latency. A timestamp of zero means "use the current time",
   which in turn means to deliver the message with a delay of
   latency (the latency parameter used when opening the output port.)
   Do not expect PortMidi to sort data according to timestamps -- 
   messages should be sent in the correct order, and timestamps MUST 
   be non-decreasing. See also "Example" for Pm_OpenOutput() above.

   A sysex message will generally fill many PmEvent structures. On 
   output to a PortMidiStream with non-zero latency, the first timestamp
   on sysex message data will determine the time to begin sending the 
   message. PortMidi implementations may ignore timestamps for the 
   remainder of the sysex message. 
   
   On input, the timestamp ideally denotes the arrival time of the 
   status byte of the message. The first timestamp on sysex message 
   data will be valid. Subsequent timestamps may denote 
   when message bytes were actually received, or they may be simply 
   copies of the first timestamp.

   Timestamps for nested messages: If a real-time message arrives in 
   the middle of some other message, it is enqueued immediately with 
   the timestamp corresponding to its arrival time. The interrupted 
   non-real-time message or 4-byte packet of sysex data will be enqueued 
   later. The timestamp of interrupted data will be equal to that of
   the interrupting real-time message to insure that timestamps are
   non-decreasing.
 */
typedef struct {
    PmMessage      message;
    PmTimestamp    timestamp;
} PmEvent;

/** 
    @}
*/
/** \defgroup grp_io Reading and Writing Midi Messages
    @{
*/
/**
    Pm_Read() retrieves midi data into a buffer, and returns the number
    of events read. Result is a non-negative number unless an error occurs, 
    in which case a PmError value will be returned.

    Buffer Overflow

    The problem: if an input overflow occurs, data will be lost, ultimately 
    because there is no flow control all the way back to the data source. 
    When data is lost, the receiver should be notified and some sort of 
    graceful recovery should take place, e.g. you shouldn't resume receiving 
    in the middle of a long sysex message.

    With a lock-free fifo, which is pretty much what we're stuck with to 
    enable portability to the Mac, it's tricky for the producer and consumer 
    to synchronously reset the buffer and resume normal operation.

    Solution: the buffer managed by PortMidi will be flushed when an overflow
    occurs. The consumer (Pm_Read()) gets an error message (pmBufferOverflow)
    and ordinary processing resumes as soon as a new message arrives. The
    remainder of a partial sysex message is not considered to be a "new
    message" and will be flushed as well.

*/
PMEXPORT int Pm_Read( PortMidiStream *stream, PmEvent *buffer, int32_t length );

/**
    Pm_Poll() tests whether input is available, 
    returning TRUE, FALSE, or an error value.
*/
PMEXPORT PmError Pm_Poll( PortMidiStream *stream);

/** 
    Pm_Write() writes midi data from a buffer. This may contain:
        - short messages 
    or 
        - sysex messages that are converted into a sequence of PmEvent
          structures, e.g. sending data from a file or forwarding them
          from midi input.

    Use Pm_WriteSysEx() to write a sysex message stored as a contiguous 
    array of bytes.

    Sysex data may contain embedded real-time messages.
*/
PMEXPORT PmError Pm_Write( PortMidiStream *stream, PmEvent *buffer, int32_t length );

/**
    Pm_WriteShort() writes a timestamped non-system-exclusive midi message.
    Messages are delivered in order as received, and timestamps must be 
    non-decreasing. (But timestamps are ignored if the stream was opened
    with latency = 0.)
*/
PMEXPORT PmError Pm_WriteShort( PortMidiStream *stream, PmTimestamp when, int32_t msg);

/**
    Pm_WriteSysEx() writes a timestamped system-exclusive midi message.
*/
PMEXPORT PmError Pm_WriteSysEx( PortMidiStream *stream, PmTimestamp when, unsigned char *msg);

/** @} */

#ifdef __cplusplus
}
#endif /* __cplusplus */
#endif /* PORT_MIDI_H */
