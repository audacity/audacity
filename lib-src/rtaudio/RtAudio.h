/************************************************************************/
/*! \class RtAudio
    \brief Realtime audio i/o C++ classes.

    RtAudio provides a common API (Application Programming Interface)
    for realtime audio input/output across Linux (native ALSA, Jack,
    and OSS), SGI, Macintosh OS X (CoreAudio), and Windows
    (DirectSound and ASIO) operating systems.

    RtAudio WWW site: http://music.mcgill.ca/~gary/rtaudio/

    RtAudio: a realtime audio i/o C++ class
    Copyright (c) 2001-2004 Gary P. Scavone

    Permission is hereby granted, free of charge, to any person
    obtaining a copy of this software and associated documentation files
    (the "Software"), to deal in the Software without restriction,
    including without limitation the rights to use, copy, modify, merge,
    publish, distribute, sublicense, and/or sell copies of the Software,
    and to permit persons to whom the Software is furnished to do so,
    subject to the following conditions:

    The above copyright notice and this permission notice shall be
    included in all copies or substantial portions of the Software.

    Any person wishing to distribute modifications to the Software is
    requested to send the modifications to the original developer so that
    they can be incorporated into the canonical version.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
    ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
    CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
    WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
/************************************************************************/

// RtAudio: Version 3.0.1, 22 March 2004

#ifndef __RTAUDIO_H
#define __RTAUDIO_H

#include "RtError.h"
#include <string>
#include <vector>

// Operating system dependent thread functionality.
#if defined(__WINDOWS_DS__) || defined(__WINDOWS_ASIO__)
  #include <windows.h>
  #include <process.h>

  typedef unsigned long ThreadHandle;
  typedef CRITICAL_SECTION StreamMutex;

#else // Various unix flavors with pthread support.
  #include <pthread.h>

  typedef pthread_t ThreadHandle;
  typedef pthread_mutex_t StreamMutex;

#endif

#if defined(LINUX_ALSA) || defined(LINUX_OSS) || defined(LINUX_JACK) || defined(MACOSX_CORE)
#include <sys/time.h>
#define HAVE_GETTIMEOFDAY 1
#endif

// This global structure type is used to pass callback information
// between the private RtAudio stream structure and global callback
// handling functions.
struct CallbackInfo {
  void *object;    // Used as a "this" pointer.
  ThreadHandle thread;
  bool usingCallback;
  void *callback;
  void *userData;
  void *apiInfo;   // void pointer for API specific callback information

  // Default constructor.
  CallbackInfo()
    :object(0), usingCallback(false), callback(0),
     userData(0), apiInfo(0) {}
};

// Support for signed integers and floats.  Audio data fed to/from
// the tickStream() routine is assumed to ALWAYS be in host
// byte order.  The internal routines will automatically take care of
// any necessary byte-swapping between the host format and the
// soundcard.  Thus, endian-ness is not a concern in the following
// format definitions.
typedef unsigned long RtAudioFormat;
static const RtAudioFormat RTAUDIO_SINT8 = 0x1;    /*!< 8-bit signed integer. */
static const RtAudioFormat RTAUDIO_SINT16 = 0x2;   /*!< 16-bit signed integer. */
static const RtAudioFormat RTAUDIO_SINT24 = 0x4;   /*!< Upper 3 bytes of 32-bit signed integer. */
static const RtAudioFormat RTAUDIO_SINT32 = 0x8;   /*!< 32-bit signed integer. */
static const RtAudioFormat RTAUDIO_FLOAT32 = 0x10; /*!< Normalized between plus/minus 1.0. */
static const RtAudioFormat RTAUDIO_FLOAT64 = 0x20; /*!< Normalized between plus/minus 1.0. */

typedef int (*RtAudioCallback)(char *buffer, int bufferSize, void *userData);

//! The public device information structure for returning queried values.
struct RtAudioDeviceInfo {
  std::string name;      /*!< Character string device identifier. */
  bool probed;          /*!< true if the device capabilities were successfully probed. */
  int outputChannels;   /*!< Maximum output channels supported by device. */
  int inputChannels;    /*!< Maximum input channels supported by device. */
  int duplexChannels;   /*!< Maximum simultaneous input/output channels supported by device. */
  bool isDefault;       /*!< true if this is the default output or input device. */
  std::vector<int> sampleRates; /*!< Supported sample rates (queried from list of standard rates). */
  RtAudioFormat nativeFormats;  /*!< Bit mask of supported data formats. */

  // Default constructor.
  RtAudioDeviceInfo()
    :probed(false), outputChannels(0), inputChannels(0),
       duplexChannels(0), isDefault(false), nativeFormats(0) {}
};

// **************************************************************** //
//
// RtApi class declaration.
//
// Note that RtApi is an abstract base class and cannot be
// explicitly instantiated.  The class RtAudio will create an
// instance of an RtApi subclass (RtApiOss, RtApiAlsa,
// RtApiJack, RtApiCore, RtApiAl, RtApiDs, or RtApiAsio).
//
// **************************************************************** //

class RtApi
{
public:

  RtApi();
  virtual ~RtApi();
  void openStream( int outputDevice, int outputChannels,
                   int inputDevice, int inputChannels,
                   RtAudioFormat format, int sampleRate,
                   int *bufferSize, int numberOfBuffers );
  virtual void setStreamCallback( RtAudioCallback callback, void *userData ) = 0;
  virtual void cancelStreamCallback() = 0;
  int getDeviceCount(void);
  RtAudioDeviceInfo getDeviceInfo( int device );
  char * const getStreamBuffer();
  virtual void tickStream();
  virtual void closeStream();
  virtual void startStream() = 0;
  virtual void stopStream() = 0;
  virtual void abortStream() = 0;
  virtual bool isStreamRunning();
  virtual double getStreamTime();

protected:

  static const unsigned int MAX_SAMPLE_RATES;
  static const unsigned int SAMPLE_RATES[];

  enum { FAILURE, SUCCESS };

  enum StreamMode {
    OUTPUT,
    INPUT,
    DUPLEX,
    UNINITIALIZED = -75
  };

  enum StreamState {
    STREAM_STOPPED,
    STREAM_RUNNING
  };

  // A protected structure for audio streams.
  struct RtApiStream {
    int device[2];          // Playback and record, respectively.
    void *apiHandle;        // void pointer for API specific stream handle information
    StreamMode mode;         // OUTPUT, INPUT, or DUPLEX.
    StreamState state;       // STOPPED or RUNNING
    char *userBuffer;
    char *deviceBuffer;
    bool doConvertBuffer[2]; // Playback and record, respectively.
    bool deInterleave[2];    // Playback and record, respectively.
    bool doByteSwap[2];      // Playback and record, respectively.
    int sampleRate;
    int bufferSize;
    int nBuffers;
    int nUserChannels[2];    // Playback and record, respectively.
    int nDeviceChannels[2];  // Playback and record channels, respectively.
    RtAudioFormat userFormat;
    RtAudioFormat deviceFormat[2]; // Playback and record, respectively.
    StreamMutex mutex;
    CallbackInfo callbackInfo;
    double streamTime;      // Number of elapsed seconds since the
               // stream started 

#if defined(HAVE_GETTIMEOFDAY)
    struct timeval lastTickTimestamp;
#endif

    RtApiStream()
      :apiHandle(0), userBuffer(0), deviceBuffer(0) {}
    //      mode(UNINITIALIZED), state(STREAM_STOPPED),
  };

  // A protected device structure for audio devices.
  struct RtApiDevice {
    std::string name;      /*!< Character string device identifier. */
    bool probed;           /*!< true if the device capabilities were successfully probed. */
    void *apiDeviceId;     // void pointer for API specific device information
    int maxOutputChannels; /*!< Maximum output channels supported by device. */
    int maxInputChannels;  /*!< Maximum input channels supported by device. */
    int maxDuplexChannels; /*!< Maximum simultaneous input/output channels supported by device. */
    int minOutputChannels; /*!< Minimum output channels supported by device. */
    int minInputChannels;  /*!< Minimum input channels supported by device. */
    int minDuplexChannels; /*!< Minimum simultaneous input/output channels supported by device. */
    bool hasDuplexSupport; /*!< true if device supports duplex mode. */
    bool isDefault;        /*!< true if this is the default output or input device. */
    std::vector<int> sampleRates; /*!< Supported sample rates. */
    RtAudioFormat nativeFormats;  /*!< Bit mask of supported data formats. */

    // Default constructor.
    RtApiDevice()
      :probed(false), apiDeviceId(0), maxOutputChannels(0), maxInputChannels(0),
       maxDuplexChannels(0), minOutputChannels(0), minInputChannels(0),
       minDuplexChannels(0), isDefault(false), nativeFormats(0) {}
  };

  typedef signed short Int16;
  typedef signed int Int32;
  typedef float Float32;
  typedef double Float64;

  char message_[256];
  int nDevices_;
  std::vector<RtApiDevice> devices_;
  RtApiStream stream_;

  /*!
    Protected, api-specific method to count and identify the system
    audio devices.  This function MUST be implemented by all subclasses.
  */
  virtual void initialize(void) = 0;

  /*!
    Protected, api-specific method which attempts to fill an
    RtAudioDevice structure for a given device.  This function MUST be
    implemented by all subclasses.  If an error is encountered during
    the probe, a "warning" message is reported and the value of
    "probed" remains false (no exception is thrown).  A successful
    probe is indicated by probed = true.
  */
  virtual void probeDeviceInfo( RtApiDevice *info );

  /*!
    Protected, api-specific method which attempts to open a device
    with the given parameters.  This function MUST be implemented by
    all subclasses.  If an error is encountered during the probe, a
    "warning" message is reported and FAILURE is returned (no
    exception is thrown). A successful probe is indicated by a return
    value of SUCCESS.
  */
  virtual bool probeDeviceOpen( int device, StreamMode mode, int channels, 
                                int sampleRate, RtAudioFormat format,
                                int *bufferSize, int numberOfBuffers );

  /*!
    Protected method which returns the index in the devices array to
    the default input device.
  */
  virtual int getDefaultInputDevice(void);

  /*!
    Protected method which returns the index in the devices array to
    the default output device.
  */
  virtual int getDefaultOutputDevice(void);

  //! Protected common method to clear an RtApiDevice structure.
  void clearDeviceInfo( RtApiDevice *info );

  //! Protected common method to clear an RtApiStream structure.
  void clearStreamInfo();

  //! Protected common error method to allow global control over error handling.
  void error( RtError::Type type );

  /*!
    Protected common method used to check whether a stream is open.
    If not, an "invalid identifier" exception is thrown.
  */
  void verifyStream();

  /*!
    Protected method used to perform format, channel number, and/or interleaving
    conversions between the user and device buffers.
  */
  void convertStreamBuffer( StreamMode mode );

  //! Protected common method used to perform byte-swapping on buffers.
  void byteSwapBuffer( char *buffer, int samples, RtAudioFormat format );

  //! Protected common method which returns the number of bytes for a given format.
  int formatBytes( RtAudioFormat format );
};


// **************************************************************** //
//
// RtAudio class declaration.
//
// RtAudio is a "controller" used to select an available audio i/o
// interface.  It presents a common API for the user to call but all
// functionality is implemented by the class RtAudioApi and its
// subclasses.  RtAudio creates an instance of an RtAudioApi subclass
// based on the user's API choice.  If no choice is made, RtAudio
// attempts to make a "logical" API selection.
//
// **************************************************************** //

class RtAudio
{
public:

  //! Audio API specifier arguments.
  enum RtAudioApi {
    UNSPECIFIED,    /*!< Search for a working compiled API. */
    LINUX_ALSA,     /*!< The Advanced Linux Sound Architecture API. */
    LINUX_OSS,      /*!< The Linux Open Sound System API. */
    LINUX_JACK,     /*!< The Linux Jack Low-Latency Audio Server API. */
    MACOSX_CORE,    /*!< Macintosh OS-X Core Audio API. */
    IRIX_AL,        /*!< The Irix Audio Library API. */
    WINDOWS_ASIO,   /*!< The Steinberg Audio Stream I/O API. */
    WINDOWS_DS      /*!< The Microsoft Direct Sound API. */
  };

  //! The default class constructor.
  /*!
    Probes the system to make sure at least one audio input/output
    device is available and determines the api-specific identifier for
    each device found.  An RtError error can be thrown if no devices
    are found or if a memory allocation error occurs.

    If no API argument is specified and multiple API support has been
    compiled, the default order of use is JACK, ALSA, OSS (Linux
    systems) and ASIO, DS (Windows systems).
  */
  RtAudio( RtAudioApi api=UNSPECIFIED );

  //! A constructor which can be used to open a stream during instantiation.
  /*!
    The specified output and/or input device identifiers correspond
    to those enumerated via the getDeviceInfo() method.  If device =
    0, the default or first available devices meeting the given
    parameters is selected.  If an output or input channel value is
    zero, the corresponding device value is ignored.  When a stream is
    successfully opened, its identifier is returned via the "streamId"
    pointer.  An RtError can be thrown if no devices are found
    for the given parameters, if a memory allocation error occurs, or
    if a driver error occurs. \sa openStream()
  */
  RtAudio( int outputDevice, int outputChannels,
           int inputDevice, int inputChannels,
           RtAudioFormat format, int sampleRate,
           int *bufferSize, int numberOfBuffers, RtAudioApi api=UNSPECIFIED );

  //! The destructor.
  /*!
    Stops and closes an open stream and devices and deallocates
    buffer and structure memory.
  */
  ~RtAudio();

  //! A public method for opening a stream with the specified parameters.
  /*!
    An RtError is thrown if a stream cannot be opened.

    \param outputDevice: If equal to 0, the default or first device
           found meeting the given parameters is opened.  Otherwise, the
           device number should correspond to one of those enumerated via
           the getDeviceInfo() method.
    \param outputChannels: The desired number of output channels.  If
           equal to zero, the outputDevice identifier is ignored.
    \param inputDevice: If equal to 0, the default or first device
           found meeting the given parameters is opened.  Otherwise, the
           device number should correspond to one of those enumerated via
           the getDeviceInfo() method.
    \param inputChannels: The desired number of input channels.  If
           equal to zero, the inputDevice identifier is ignored.
    \param format: An RtAudioFormat specifying the desired sample data format.
    \param sampleRate: The desired sample rate (sample frames per second).
    \param *bufferSize: A pointer value indicating the desired internal buffer
           size in sample frames.  The actual value used by the device is
           returned via the same pointer.  A value of zero can be specified,
           in which case the lowest allowable value is determined.
    \param numberOfBuffers: A value which can be used to help control device
           latency.  More buffers typically result in more robust performance,
           though at a cost of greater latency.  A value of zero can be
           specified, in which case the lowest allowable value is used.
  */
  void openStream( int outputDevice, int outputChannels,
                   int inputDevice, int inputChannels,
                   RtAudioFormat format, int sampleRate,
                   int *bufferSize, int numberOfBuffers );

  //! A public method which sets a user-defined callback function for a given stream.
  /*!
    This method assigns a callback function to a previously opened
    stream for non-blocking stream functionality.  A separate process
    is initiated, though the user function is called only when the
    stream is "running" (between calls to the startStream() and
    stopStream() methods, respectively).  The callback process remains
    active for the duration of the stream and is automatically
    shutdown when the stream is closed (via the closeStream() method
    or by object destruction).  The callback process can also be
    shutdown and the user function de-referenced through an explicit
    call to the cancelStreamCallback() method.  Note that the stream
    can use only blocking or callback functionality at a particular
    time, though it is possible to alternate modes on the same stream
    through the use of the setStreamCallback() and
    cancelStreamCallback() methods (the blocking tickStream() method
    can be used before a callback is set and/or after a callback is
    cancelled).  An RtError will be thrown if called when no stream is
    open or a thread errors occurs.
  */
  void setStreamCallback(RtAudioCallback callback, void *userData) { rtapi_->setStreamCallback( callback, userData ); };

  //! A public method which cancels a callback process and function for the stream.
  /*!
    This method shuts down a callback process and de-references the
    user function for the stream.  Callback functionality can
    subsequently be restarted on the stream via the
    setStreamCallback() method.  An RtError will be thrown if called
    when no stream is open.
   */
  void cancelStreamCallback() { rtapi_->cancelStreamCallback(); };

  //! A public method which returns the number of audio devices found.
  int getDeviceCount(void) { return rtapi_->getDeviceCount(); };

  //! Return an RtAudioDeviceInfo structure for a specified device number.
  /*!
    Any device integer between 1 and getDeviceCount() is valid.  If
    a device is busy or otherwise unavailable, the structure member
    "probed" will have a value of "false" and all other members are
    undefined.  If the specified device is the current default input
    or output device, the "isDefault" member will have a value of
    "true".  An RtError will be thrown for an invalid device argument.
  */
  RtAudioDeviceInfo getDeviceInfo(int device) { return rtapi_->getDeviceInfo( device ); };

  //! A public method which returns a pointer to the buffer for an open stream.
  /*!
    The user should fill and/or read the buffer data in interleaved format
    and then call the tickStream() method.  An RtError will be
    thrown if called when no stream is open.
  */
  char * const getStreamBuffer() { return rtapi_->getStreamBuffer(); };

  //! Public method used to trigger processing of input/output data for a stream.
  /*!
    This method blocks until all buffer data is read/written.  An
    RtError will be thrown if a driver error occurs or if called when
    no stream is open.
  */
  void tickStream() { rtapi_->tickStream(); };

  //! Public method which closes a stream and frees any associated buffers.
  /*!
    If a stream is not open, this method issues a warning and
    returns (an RtError is not thrown).
  */
  void closeStream()  { rtapi_->closeStream(); };

  //! Public method which starts a stream.
  /*!
    An RtError will be thrown if a driver error occurs or if called
    when no stream is open.
  */
  void startStream() { rtapi_->startStream(); };

  //! Stop a stream, allowing any samples remaining in the queue to be played out and/or read in.
  /*!
    An RtError will be thrown if a driver error occurs or if called
    when no stream is open.
  */
  void stopStream() { rtapi_->stopStream(); };

  //! Stop a stream, discarding any samples remaining in the input/output queue.
  /*!
    An RtError will be thrown if a driver error occurs or if called
    when no stream is open.
  */
  void abortStream() { rtapi_->abortStream(); };

  //! Returns true is the stream is running, and false if it is stopped.
  
  /*!
    An RtError will be thrown if a driver error occurs or if called
    when no stream is open.
  */
  bool isStreamRunning() { return rtapi_->isStreamRunning(); };

  //! Returns the number of elapsed seconds since the stream was started.

  /*!
    An RtError will be thrown if a driver error occurs or if called
    when no stream is open.
  */
  double getStreamTime() { return rtapi_->getStreamTime(); };

 protected:

  void initialize( RtAudioApi api );

  RtApi *rtapi_;
};


// RtApi Subclass prototypes.

#if defined(__LINUX_ALSA__)

class RtApiAlsa: public RtApi
{
public:

  RtApiAlsa();
  ~RtApiAlsa();
  void tickStream();
  void closeStream();
  void startStream();
  void stopStream();
  void abortStream();
  int streamWillBlock();
  void setStreamCallback( RtAudioCallback callback, void *userData );
  void cancelStreamCallback();

  private:

  void initialize(void);
  void probeDeviceInfo( RtApiDevice *info );
  bool probeDeviceOpen( int device, StreamMode mode, int channels, 
                        int sampleRate, RtAudioFormat format,
                        int *bufferSize, int numberOfBuffers );
};

#endif

#if defined(__LINUX_JACK__)

class RtApiJack: public RtApi
{
public:

  RtApiJack();
  ~RtApiJack();
  void tickStream();
  void closeStream();
  void startStream();
  void stopStream();
  void abortStream();
  void setStreamCallback( RtAudioCallback callback, void *userData );
  void cancelStreamCallback();
  // This function is intended for internal use only.  It must be
  // public because it is called by the internal callback handler,
  // which is not a member of RtAudio.  External use of this function
  // will most likely produce highly undesireable results!
  void callbackEvent( unsigned long nframes );

  private:

  void initialize(void);
  void probeDeviceInfo( RtApiDevice *info );
  bool probeDeviceOpen( int device, StreamMode mode, int channels, 
                        int sampleRate, RtAudioFormat format,
                        int *bufferSize, int numberOfBuffers );
};

#endif

#if defined(__LINUX_OSS__)

class RtApiOss: public RtApi
{
public:

  RtApiOss();
  ~RtApiOss();
  void tickStream();
  void closeStream();
  void startStream();
  void stopStream();
  void abortStream();
  int streamWillBlock();
  void setStreamCallback( RtAudioCallback callback, void *userData );
  void cancelStreamCallback();

  private:

  void initialize(void);
  void probeDeviceInfo( RtApiDevice *info );
  bool probeDeviceOpen( int device, StreamMode mode, int channels, 
                        int sampleRate, RtAudioFormat format,
                        int *bufferSize, int numberOfBuffers );
};

#endif

#if defined(__MACOSX_CORE__)

#include <CoreAudio/AudioHardware.h>

class RtApiCore: public RtApi
{
public:

  RtApiCore();
  ~RtApiCore();
  int getDefaultOutputDevice(void);
  int getDefaultInputDevice(void);
  void tickStream();
  void closeStream();
  void startStream();
  void stopStream();
  void abortStream();
  void setStreamCallback( RtAudioCallback callback, void *userData );
  void cancelStreamCallback();

  // This function is intended for internal use only.  It must be
  // public because it is called by the internal callback handler,
  // which is not a member of RtAudio.  External use of this function
  // will most likely produce highly undesireable results!
  void callbackEvent( AudioDeviceID deviceId, void *inData, void *outData );

  private:

  void initialize(void);
  void probeDeviceInfo( RtApiDevice *info );
  bool probeDeviceOpen( int device, StreamMode mode, int channels, 
                        int sampleRate, RtAudioFormat format,
                        int *bufferSize, int numberOfBuffers );
};

#endif

#if defined(__WINDOWS_DS__)

class RtApiDs: public RtApi
{
public:

  RtApiDs();
  ~RtApiDs();
  int getDefaultOutputDevice(void);
  int getDefaultInputDevice(void);
  void tickStream();
  void closeStream();
  void startStream();
  void stopStream();
  void abortStream();
  int streamWillBlock();
  void setStreamCallback( RtAudioCallback callback, void *userData );
  void cancelStreamCallback();

  private:

  void initialize(void);
  void probeDeviceInfo( RtApiDevice *info );
  bool probeDeviceOpen( int device, StreamMode mode, int channels, 
                        int sampleRate, RtAudioFormat format,
                        int *bufferSize, int numberOfBuffers );
};

#endif

#if defined(__WINDOWS_ASIO__)

class RtApiAsio: public RtApi
{
public:

  RtApiAsio();
  ~RtApiAsio();
  void tickStream();
  void closeStream();
  void startStream();
  void stopStream();
  void abortStream();
  void setStreamCallback( RtAudioCallback callback, void *userData );
  void cancelStreamCallback();

  // This function is intended for internal use only.  It must be
  // public because it is called by the internal callback handler,
  // which is not a member of RtAudio.  External use of this function
  // will most likely produce highly undesireable results!
  void callbackEvent( long bufferIndex );

  private:

  void initialize(void);
  void probeDeviceInfo( RtApiDevice *info );
  bool probeDeviceOpen( int device, StreamMode mode, int channels, 
                        int sampleRate, RtAudioFormat format,
                        int *bufferSize, int numberOfBuffers );
};

#endif

#if defined(__IRIX_AL__)

class RtApiAl: public RtApi
{
public:

  RtApiAl();
  ~RtApiAl();
  int getDefaultOutputDevice(void);
  int getDefaultInputDevice(void);
  void tickStream();
  void closeStream();
  void startStream();
  void stopStream();
  void abortStream();
  int streamWillBlock();
  void setStreamCallback( RtAudioCallback callback, void *userData );
  void cancelStreamCallback();

  private:

  void initialize(void);
  void probeDeviceInfo( RtApiDevice *info );
  bool probeDeviceOpen( int device, StreamMode mode, int channels, 
                        int sampleRate, RtAudioFormat format,
                        int *bufferSize, int numberOfBuffers );
};

#endif

// Define the following flag to have extra information spewed to stderr.
//#define __RTAUDIO_DEBUG__

#endif

// Indentation settings for Vim and Emacs
//
// Local Variables:
// c-basic-offset: 2
// indent-tabs-mode: nil
// End:
//
// vim: et sts=2 sw=2

