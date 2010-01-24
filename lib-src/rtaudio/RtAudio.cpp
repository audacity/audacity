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

#include "RtAudio.h"
#include <iostream>

// Static variable definitions.
const unsigned int RtApi::MAX_SAMPLE_RATES = 14;
const unsigned int RtApi::SAMPLE_RATES[] = {
  4000, 5512, 8000, 9600, 11025, 16000, 22050,
  32000, 44100, 48000, 88200, 96000, 176400, 192000
};

#if defined(__WINDOWS_DS__) || defined(__WINDOWS_ASIO__)
  #define MUTEX_INITIALIZE(A) InitializeCriticalSection(A)
  #define MUTEX_DESTROY(A)    DeleteCriticalSection(A);
  #define MUTEX_LOCK(A)      EnterCriticalSection(A)
  #define MUTEX_UNLOCK(A)     LeaveCriticalSection(A)
#else // pthread API
  #define MUTEX_INITIALIZE(A) pthread_mutex_init(A, NULL)
  #define MUTEX_DESTROY(A)    pthread_mutex_destroy(A);
  #define MUTEX_LOCK(A)       pthread_mutex_lock(A)
  #define MUTEX_UNLOCK(A)     pthread_mutex_unlock(A)
#endif

// *************************************************** //
//
// Public common (OS-independent) methods.
//
// *************************************************** //

RtAudio :: RtAudio( RtAudioApi api )
{
  initialize( api );
}

RtAudio :: RtAudio( int outputDevice, int outputChannels,
                    int inputDevice, int inputChannels,
                    RtAudioFormat format, int sampleRate,
                    int *bufferSize, int numberOfBuffers, RtAudioApi api )
{
  initialize( api );

  try {
    rtapi_->openStream( outputDevice, outputChannels,
                        inputDevice, inputChannels,
                        format, sampleRate,
                        bufferSize, numberOfBuffers );
  }
  catch (RtError &exception) {
    // Deallocate the RtApi instance.
    delete rtapi_;
    throw exception;
  }
}

RtAudio :: ~RtAudio()
{
  delete rtapi_;
}

void RtAudio :: openStream( int outputDevice, int outputChannels,
                            int inputDevice, int inputChannels,
                            RtAudioFormat format, int sampleRate,
                            int *bufferSize, int numberOfBuffers )
{
  rtapi_->openStream( outputDevice, outputChannels, inputDevice,
                      inputChannels, format, sampleRate,
                      bufferSize, numberOfBuffers );
}

void RtAudio::initialize( RtAudioApi api )
{
  rtapi_ = 0;

  // First look for a compiled match to a specified API value. If one
  // of these constructors throws an error, it will be passed up the
  // inheritance chain.
#if defined(__LINUX_JACK__)
  if ( api == LINUX_JACK )
    rtapi_ = new RtApiJack();
#endif
#if defined(__LINUX_ALSA__)
  if ( api == LINUX_ALSA )
    rtapi_ = new RtApiAlsa();
#endif
#if defined(__LINUX_OSS__)
  if ( api == LINUX_OSS )
    rtapi_ = new RtApiOss();
#endif
#if defined(__WINDOWS_ASIO__)
  if ( api == WINDOWS_ASIO )
    rtapi_ = new RtApiAsio();
#endif
#if defined(__WINDOWS_DS__)
  if ( api == WINDOWS_DS )
    rtapi_ = new RtApiDs();
#endif
#if defined(__IRIX_AL__)
  if ( api == IRIX_AL )
    rtapi_ = new RtApiAl();
#endif
#if defined(__MACOSX_CORE__)
  if ( api == MACOSX_CORE )
    rtapi_ = new RtApiCore();
#endif

  if ( rtapi_ ) return;
  if ( api > 0 ) {
    // No compiled support for specified API value.
    throw RtError( "RtAudio: no compiled support for specified API argument!", RtError::INVALID_PARAMETER );
  }

  // No specified API ... search for "best" option.
  try {
#if defined(__LINUX_JACK__)
    rtapi_ = new RtApiJack();
#elif defined(__WINDOWS_ASIO__)
    rtapi_ = new RtApiAsio();
#elif defined(__IRIX_AL__)
    rtapi_ = new RtApiAl();
#elif defined(__MACOSX_CORE__)
    rtapi_ = new RtApiCore();
#else
    ;
#endif
  }
  catch (RtError &) {
#if defined(__RTAUDIO_DEBUG__)
    fprintf(stderr, "\nRtAudio: no devices found for first api option (JACK, ASIO, Al, or CoreAudio).\n\n");
#endif
    rtapi_ = 0;
  }

  if ( rtapi_ ) return;

// Try second API support
  if ( rtapi_ == 0 ) {
    try {
#if defined(__LINUX_ALSA__)
      rtapi_ = new RtApiAlsa();
#elif defined(__WINDOWS_DS__)
      rtapi_ = new RtApiDs();
#else
      ;
#endif
    }
    catch (RtError &) {
#if defined(__RTAUDIO_DEBUG__)
      fprintf(stderr, "\nRtAudio: no devices found for second api option (Alsa or DirectSound).\n\n");
#endif
      rtapi_ = 0;
    }
  }

  if ( rtapi_ ) return;

  // Try third API support
  if ( rtapi_ == 0 ) {
#if defined(__LINUX_OSS__)
    try {
      rtapi_ = new RtApiOss();
    }
    catch (RtError &error) {
      rtapi_ = 0;
    }
#else
    ;
#endif
  }

  if ( rtapi_ == 0 ) {
    // No devices found.
    throw RtError( "RtAudio: no devices found for compiled audio APIs!", RtError::NO_DEVICES_FOUND );
  }
}

RtApi :: RtApi()
{
  stream_.mode = UNINITIALIZED;
  stream_.apiHandle = 0;
  MUTEX_INITIALIZE(&stream_.mutex);
}

RtApi :: ~RtApi()
{
  MUTEX_DESTROY(&stream_.mutex);
}

void RtApi :: openStream( int outputDevice, int outputChannels,
                         int inputDevice, int inputChannels,
                         RtAudioFormat format, int sampleRate,
                         int *bufferSize, int numberOfBuffers )
{
  if ( stream_.mode != UNINITIALIZED ) {
    sprintf(message_, "RtApi: only one open stream allowed per class instance.");
    error(RtError::INVALID_STREAM);
  }

  if (outputChannels < 1 && inputChannels < 1) {
    sprintf(message_,"RtApi: one or both 'channel' parameters must be greater than zero.");
    error(RtError::INVALID_PARAMETER);
  }

  if ( formatBytes(format) == 0 ) {
    sprintf(message_,"RtApi: 'format' parameter value is undefined.");
    error(RtError::INVALID_PARAMETER);
  }

  if ( outputChannels > 0 ) {
    if (outputDevice > nDevices_ || outputDevice < 0) {
      sprintf(message_,"RtApi: 'outputDevice' parameter value (%d) is invalid.", outputDevice);
      error(RtError::INVALID_PARAMETER);
    }
  }

  if ( inputChannels > 0 ) {
    if (inputDevice > nDevices_ || inputDevice < 0) {
      sprintf(message_,"RtApi: 'inputDevice' parameter value (%d) is invalid.", inputDevice);
      error(RtError::INVALID_PARAMETER);
    }
  }

  clearStreamInfo();
  bool result = FAILURE;
  int device, defaultDevice = 0;
  StreamMode mode;
  int channels;
  if ( outputChannels > 0 ) {

    mode = OUTPUT;
    channels = outputChannels;

    if ( outputDevice == 0 ) { // Try default device first.
      defaultDevice = getDefaultOutputDevice();
      device = defaultDevice;
    }
    else
      device = outputDevice - 1;

    for ( int i=-1; i<nDevices_; i++ ) {
      if ( i >= 0 ) { 
        if ( i == defaultDevice ) continue;
        device = i;
      }
      if (devices_[device].probed == false) {
        // If the device wasn't successfully probed before, try it
        // (again) now.
        clearDeviceInfo(&devices_[device]);
        probeDeviceInfo(&devices_[device]);
      }
      if ( devices_[device].probed )
        result = probeDeviceOpen(device, mode, channels, sampleRate,
                                 format, bufferSize, numberOfBuffers);
      if ( result == SUCCESS ) break;
      if ( outputDevice > 0 ) break;
      clearStreamInfo();
    }
  }

  if ( inputChannels > 0 && ( result == SUCCESS || outputChannels <= 0 ) ) {

    mode = INPUT;
    channels = inputChannels;

    if ( inputDevice == 0 ) { // Try default device first.
      defaultDevice = getDefaultInputDevice();
      device = defaultDevice;
    }
    else
      device = inputDevice - 1;

    for (int i=-1; i<nDevices_; i++) {
      if (i >= 0 ) { 
        if ( i == defaultDevice ) continue;
        device = i;
      }
      if (devices_[device].probed == false) {
        // If the device wasn't successfully probed before, try it
        // (again) now.
        clearDeviceInfo(&devices_[device]);
        probeDeviceInfo(&devices_[device]);
      }
      if ( devices_[device].probed )
        result = probeDeviceOpen(device, mode, channels, sampleRate,
                                 format, bufferSize, numberOfBuffers);
      if (result == SUCCESS) break;
      if ( outputDevice > 0 ) break;
    }
  }

  if ( result == SUCCESS )
    return;

  // If we get here, all attempted probes failed.  Close any opened
  // devices and clear the stream structure.
  if ( stream_.mode != UNINITIALIZED ) closeStream();
  clearStreamInfo();
  if ( ( outputDevice == 0 && outputChannels > 0 )
       || ( inputDevice == 0 && inputChannels > 0 ) )
    sprintf(message_,"RtApi: no devices found for given stream parameters.");
  else
    sprintf(message_,"RtApi: unable to open specified device(s) with given stream parameters.");
  error(RtError::INVALID_PARAMETER);

  return;
}

int RtApi :: getDeviceCount(void)
{
  return devices_.size();
}

RtAudioDeviceInfo RtApi :: getDeviceInfo( int device )
{
  if (device > (int) devices_.size() || device < 1) {
    sprintf(message_, "RtApi: invalid device specifier (%d)!", device);
    error(RtError::INVALID_DEVICE);
  }

  RtAudioDeviceInfo info;
  int deviceIndex = device - 1;

  // If the device wasn't successfully probed before, try it now (or again).
  if (devices_[deviceIndex].probed == false) {
    clearDeviceInfo(&devices_[deviceIndex]);
    probeDeviceInfo(&devices_[deviceIndex]);
  }

  info.name.append( devices_[deviceIndex].name );
  info.probed = devices_[deviceIndex].probed;
  if ( info.probed == true ) {
    info.outputChannels = devices_[deviceIndex].maxOutputChannels;
    info.inputChannels = devices_[deviceIndex].maxInputChannels;
    info.duplexChannels = devices_[deviceIndex].maxDuplexChannels;
    for (unsigned int i=0; i<devices_[deviceIndex].sampleRates.size(); i++)
      info.sampleRates.push_back( devices_[deviceIndex].sampleRates[i] );
    info.nativeFormats = devices_[deviceIndex].nativeFormats;
    if ( (deviceIndex == getDefaultOutputDevice()) ||
         (deviceIndex == getDefaultInputDevice()) )
      info.isDefault = true;
  }

  return info;
}

char * const RtApi :: getStreamBuffer(void)
{
  verifyStream();
  return stream_.userBuffer;
}

int RtApi :: getDefaultInputDevice(void)
{
  // Should be implemented in subclasses if appropriate.
  return 0;
}

int RtApi :: getDefaultOutputDevice(void)
{
  // Should be implemented in subclasses if appropriate.
  return 0;
}

void RtApi :: closeStream(void)
{
  // MUST be implemented in subclasses!
}

void RtApi :: probeDeviceInfo( RtApiDevice *info )
{
  // MUST be implemented in subclasses!
}

bool RtApi :: probeDeviceOpen( int device, StreamMode mode, int channels, 
                               int sampleRate, RtAudioFormat format,
                               int *bufferSize, int numberOfBuffers )
{
  // MUST be implemented in subclasses!
  return FAILURE;
}

bool RtApi :: isStreamRunning()
{
  return stream_.state == STREAM_RUNNING;
}

void RtApi :: tickStream()
{
  // Subclasses which do not provide their own implementation of
  // getStreamTime should call the inherited tickStream and a simple
  // version here will be supported.

  stream_.streamTime += (stream_.bufferSize * 1.0 / stream_.sampleRate);

#if defined(HAVE_GETTIMEOFDAY)
  gettimeofday(&stream_.lastTickTimestamp, NULL);
#endif
}

double RtApi :: getStreamTime()
{
#if defined(HAVE_GETTIMEOFDAY)
  // Return a very accurate estimate of the stream time by
  // adding in the elapsed time since the last tick.
  struct timeval then;
  struct timeval now;

  if (stream_.state != RUNNING || stream.streamTime == 0.0)
    return stream.streamTime;
  gettimeofday(&now, NULL);
  then = stream_.lastTickTimestamp;
  return stream_.streamTime +
    ((now.tv_sec + 0.000001 * now.tv_usec) -
     (then.tv_sec + 0.000001 * then.tv_usec));     
#else
  return stream_.streamTime;
#endif
}

// *************************************************** //
//
// OS/API-specific methods.
//
// *************************************************** //

#if defined(__LINUX_OSS__)

#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/soundcard.h>
#include <errno.h>
#include <math.h>

#define DAC_NAME "/dev/dsp"
#define MAX_DEVICES 16
#define MAX_CHANNELS 16

extern "C" void *ossCallbackHandler(void * ptr);

RtApiOss :: RtApiOss()
{
  this->initialize();

  if (nDevices_ <= 0) {
    sprintf(message_, "RtApiOss: no Linux OSS audio devices found!");
    error(RtError::NO_DEVICES_FOUND);
 }
}

RtApiOss :: ~RtApiOss()
{
  if ( stream_.mode != UNINITIALIZED )
    closeStream();
}

void RtApiOss :: initialize(void)
{
  // Count cards and devices
  nDevices_ = 0;

  // We check /dev/dsp before probing devices.  /dev/dsp is supposed to
  // be a link to the "default" audio device, of the form /dev/dsp0,
  // /dev/dsp1, etc...  However, I've seen many cases where /dev/dsp was a
  // real device, so we need to check for that.  Also, sometimes the
  // link is to /dev/dspx and other times just dspx.  I'm not sure how
  // the latter works, but it does.
  char device_name[16];
  struct stat dspstat;
  int dsplink = -1;
  int i = 0;
  if (lstat(DAC_NAME, &dspstat) == 0) {
    if (S_ISLNK(dspstat.st_mode)) {
      i = readlink(DAC_NAME, device_name, sizeof(device_name));
      if (i > 0) {
        device_name[i] = '\0';
        if (i > 8) { // check for "/dev/dspx"
          if (!strncmp(DAC_NAME, device_name, 8))
            dsplink = atoi(&device_name[8]);
        }
        else if (i > 3) { // check for "dspx"
          if (!strncmp("dsp", device_name, 3))
            dsplink = atoi(&device_name[3]);
        }
      }
      else {
        sprintf(message_, "RtApiOss: cannot read value of symbolic link %s.", DAC_NAME);
        error(RtError::SYSTEM_ERROR);
      }
    }
  }
  else {
    sprintf(message_, "RtApiOss: cannot stat %s.", DAC_NAME);
    error(RtError::SYSTEM_ERROR);
  }

  // The OSS API doesn't provide a routine for determining the number
  // of devices.  Thus, we'll just pursue a brute force method.  The
  // idea is to start with /dev/dsp(0) and continue with higher device
  // numbers until we reach MAX_DSP_DEVICES.  This should tell us how
  // many devices we have ... it is not a fullproof scheme, but hopefully
  // it will work most of the time.
  int fd = 0;
  RtApiDevice device;
  for (i=-1; i<MAX_DEVICES; i++) {

    // Probe /dev/dsp first, since it is supposed to be the default device.
    if (i == -1)
      sprintf(device_name, "%s", DAC_NAME);
    else if (i == dsplink)
      continue; // We've aready probed this device via /dev/dsp link ... try next device.
    else
      sprintf(device_name, "%s%d", DAC_NAME, i);

    // First try to open the device for playback, then record mode.
    fd = open(device_name, O_WRONLY | O_NONBLOCK);
    if (fd == -1) {
      // Open device for playback failed ... either busy or doesn't exist.
      if (errno != EBUSY && errno != EAGAIN) {
        // Try to open for capture
        fd = open(device_name, O_RDONLY | O_NONBLOCK);
        if (fd == -1) {
          // Open device for record failed.
          if (errno != EBUSY && errno != EAGAIN)
            continue;
          else {
            sprintf(message_, "RtApiOss: OSS record device (%s) is busy.", device_name);
            error(RtError::WARNING);
            // still count it for now
          }
        }
      }
      else {
        sprintf(message_, "RtApiOss: OSS playback device (%s) is busy.", device_name);
        error(RtError::WARNING);
        // still count it for now
      }
    }

    if (fd >= 0) close(fd);
    device.name.erase();
    device.name.append( (const char *)device_name, strlen(device_name)+1);
    devices_.push_back(device);
    nDevices_++;
  }
}

void RtApiOss :: probeDeviceInfo(RtApiDevice *info)
{
  int i, fd, channels, mask;

  // The OSS API doesn't provide a means for probing the capabilities
  // of devices.  Thus, we'll just pursue a brute force method.

  // First try for playback
  fd = open(info->name.c_str(), O_WRONLY | O_NONBLOCK);
  if (fd == -1) {
    // Open device failed ... either busy or doesn't exist
    if (errno == EBUSY || errno == EAGAIN)
      sprintf(message_, "RtApiOss: OSS playback device (%s) is busy and cannot be probed.",
              info->name.c_str());
    else
      sprintf(message_, "RtApiOss: OSS playback device (%s) open error.", info->name.c_str());
    error(RtError::DEBUG_WARNING);
    goto capture_probe;
  }

  // We have an open device ... see how many channels it can handle
  for (i=MAX_CHANNELS; i>0; i--) {
    channels = i;
    if (ioctl(fd, SNDCTL_DSP_CHANNELS, &channels) == -1) {
      // This would normally indicate some sort of hardware error, but under ALSA's
      // OSS emulation, it sometimes indicates an invalid channel value.  Further,
      // the returned channel value is not changed. So, we'll ignore the possible
      // hardware error.
      continue; // try next channel number
    }
    // Check to see whether the device supports the requested number of channels
    if (channels != i ) continue; // try next channel number
    // If here, we found the largest working channel value
    break;
  }
  info->maxOutputChannels = i;

  // Now find the minimum number of channels it can handle
  for (i=1; i<=info->maxOutputChannels; i++) {
    channels = i;
    if (ioctl(fd, SNDCTL_DSP_CHANNELS, &channels) == -1 || channels != i)
      continue; // try next channel number
    // If here, we found the smallest working channel value
    break;
  }
  info->minOutputChannels = i;
  close(fd);

 capture_probe:
  // Now try for capture
  fd = open(info->name.c_str(), O_RDONLY | O_NONBLOCK);
  if (fd == -1) {
    // Open device for capture failed ... either busy or doesn't exist
    if (errno == EBUSY || errno == EAGAIN)
      sprintf(message_, "RtApiOss: OSS capture device (%s) is busy and cannot be probed.",
              info->name.c_str());
    else
      sprintf(message_, "RtApiOss: OSS capture device (%s) open error.", info->name.c_str());
    error(RtError::DEBUG_WARNING);
    if (info->maxOutputChannels == 0)
      // didn't open for playback either ... device invalid
      return;
    goto probe_parameters;
  }

  // We have the device open for capture ... see how many channels it can handle
  for (i=MAX_CHANNELS; i>0; i--) {
    channels = i;
    if (ioctl(fd, SNDCTL_DSP_CHANNELS, &channels) == -1 || channels != i) {
      continue; // as above
    }
    // If here, we found a working channel value
    break;
  }
  info->maxInputChannels = i;

  // Now find the minimum number of channels it can handle
  for (i=1; i<=info->maxInputChannels; i++) {
    channels = i;
    if (ioctl(fd, SNDCTL_DSP_CHANNELS, &channels) == -1 || channels != i)
      continue; // try next channel number
    // If here, we found the smallest working channel value
    break;
  }
  info->minInputChannels = i;
  close(fd);

  if (info->maxOutputChannels == 0 && info->maxInputChannels == 0) {
    sprintf(message_, "RtApiOss: device (%s) reports zero channels for input and output.",
            info->name.c_str());
    error(RtError::DEBUG_WARNING);
    return;
  }

  // If device opens for both playback and capture, we determine the channels.
  if (info->maxOutputChannels == 0 || info->maxInputChannels == 0)
    goto probe_parameters;

  fd = open(info->name.c_str(), O_RDWR | O_NONBLOCK);
  if (fd == -1)
    goto probe_parameters;

  ioctl(fd, SNDCTL_DSP_SETDUPLEX, 0);
  ioctl(fd, SNDCTL_DSP_GETCAPS, &mask);
  if (mask & DSP_CAP_DUPLEX) {
    info->hasDuplexSupport = true;
    // We have the device open for duplex ... see how many channels it can handle
    for (i=MAX_CHANNELS; i>0; i--) {
      channels = i;
      if (ioctl(fd, SNDCTL_DSP_CHANNELS, &channels) == -1 || channels != i)
        continue; // as above
      // If here, we found a working channel value
      break;
    }
    info->maxDuplexChannels = i;

    // Now find the minimum number of channels it can handle
    for (i=1; i<=info->maxDuplexChannels; i++) {
      channels = i;
      if (ioctl(fd, SNDCTL_DSP_CHANNELS, &channels) == -1 || channels != i)
        continue; // try next channel number
      // If here, we found the smallest working channel value
      break;
    }
    info->minDuplexChannels = i;
  }
  close(fd);

 probe_parameters:
  // At this point, we need to figure out the supported data formats
  // and sample rates.  We'll proceed by openning the device in the
  // direction with the maximum number of channels, or playback if
  // they are equal.  This might limit our sample rate options, but so
  // be it.

  if (info->maxOutputChannels >= info->maxInputChannels) {
    fd = open(info->name.c_str(), O_WRONLY | O_NONBLOCK);
    channels = info->maxOutputChannels;
  }
  else {
    fd = open(info->name.c_str(), O_RDONLY | O_NONBLOCK);
    channels = info->maxInputChannels;
  }

  if (fd == -1) {
    // We've got some sort of conflict ... abort
    sprintf(message_, "RtApiOss: device (%s) won't reopen during probe.",
            info->name.c_str());
    error(RtError::DEBUG_WARNING);
    return;
  }

  // We have an open device ... set to maximum channels.
  i = channels;
  if (ioctl(fd, SNDCTL_DSP_CHANNELS, &channels) == -1 || channels != i) {
    // We've got some sort of conflict ... abort
    close(fd);
    sprintf(message_, "RtApiOss: device (%s) won't revert to previous channel setting.",
            info->name.c_str());
    error(RtError::DEBUG_WARNING);
    return;
  }

  if (ioctl(fd, SNDCTL_DSP_GETFMTS, &mask) == -1) {
    close(fd);
    sprintf(message_, "RtApiOss: device (%s) can't get supported audio formats.",
            info->name.c_str());
    error(RtError::DEBUG_WARNING);
    return;
  }

  // Probe the supported data formats ... we don't care about endian-ness just yet.
  int format;
  info->nativeFormats = 0;
#if defined (AFMT_S32_BE)
  // This format does not seem to be in the 2.4 kernel version of OSS soundcard.h
  if (mask & AFMT_S32_BE) {
    format = AFMT_S32_BE;
    info->nativeFormats |= RTAUDIO_SINT32;
  }
#endif
#if defined (AFMT_S32_LE)
  /* This format is not in the 2.4.4 kernel version of OSS soundcard.h */
  if (mask & AFMT_S32_LE) {
    format = AFMT_S32_LE;
    info->nativeFormats |= RTAUDIO_SINT32;
  }
#endif
  if (mask & AFMT_S8) {
    format = AFMT_S8;
    info->nativeFormats |= RTAUDIO_SINT8;
  }
  if (mask & AFMT_S16_BE) {
    format = AFMT_S16_BE;
    info->nativeFormats |= RTAUDIO_SINT16;
  }
  if (mask & AFMT_S16_LE) {
    format = AFMT_S16_LE;
    info->nativeFormats |= RTAUDIO_SINT16;
  }

  // Check that we have at least one supported format
  if (info->nativeFormats == 0) {
    close(fd);
    sprintf(message_, "RtApiOss: device (%s) data format not supported by RtAudio.",
            info->name.c_str());
    error(RtError::DEBUG_WARNING);
    return;
  }

  // Set the format
  i = format;
  if (ioctl(fd, SNDCTL_DSP_SETFMT, &format) == -1 || format != i) {
    close(fd);
    sprintf(message_, "RtApiOss: device (%s) error setting data format.",
            info->name.c_str());
    error(RtError::DEBUG_WARNING);
    return;
  }

  // Probe the supported sample rates.
  info->sampleRates.clear();
  for (unsigned int k=0; k<MAX_SAMPLE_RATES; k++) {
    int speed = SAMPLE_RATES[k];
    if (ioctl(fd, SNDCTL_DSP_SPEED, &speed) != -1 && speed == (int)SAMPLE_RATES[k])
      info->sampleRates.push_back(speed);
  }

  if (info->sampleRates.size() == 0) {
    close(fd);
    sprintf(message_, "RtApiOss: no supported sample rates found for device (%s).",
            info->name.c_str());
    error(RtError::DEBUG_WARNING);
    return;
  }

  // That's all ... close the device and return
  close(fd);
  info->probed = true;
  return;
}

bool RtApiOss :: probeDeviceOpen(int device, StreamMode mode, int channels, 
                                int sampleRate, RtAudioFormat format,
                                int *bufferSize, int numberOfBuffers)
{
  int buffers, buffer_bytes, device_channels, device_format;
  int srate, temp, fd;
  int *handle = (int *) stream_.apiHandle;

  const char *name = devices_[device].name.c_str();

  if (mode == OUTPUT)
    fd = open(name, O_WRONLY | O_NONBLOCK);
  else { // mode == INPUT
    if (stream_.mode == OUTPUT && stream_.device[0] == device) {
      // We just set the same device for playback ... close and reopen for duplex (OSS only).
      close(handle[0]);
      handle[0] = 0;
      // First check that the number previously set channels is the same.
      if (stream_.nUserChannels[0] != channels) {
        sprintf(message_, "RtApiOss: input/output channels must be equal for OSS duplex device (%s).", name);
        goto error;
      }
      fd = open(name, O_RDWR | O_NONBLOCK);
    }
    else
      fd = open(name, O_RDONLY | O_NONBLOCK);
  }

  if (fd == -1) {
    if (errno == EBUSY || errno == EAGAIN)
      sprintf(message_, "RtApiOss: device (%s) is busy and cannot be opened.",
              name);
    else
      sprintf(message_, "RtApiOss: device (%s) cannot be opened.", name);
    goto error;
  }

  // Now reopen in blocking mode.
  close(fd);
  if (mode == OUTPUT)
    fd = open(name, O_WRONLY | O_SYNC);
  else { // mode == INPUT
    if (stream_.mode == OUTPUT && stream_.device[0] == device)
      fd = open(name, O_RDWR | O_SYNC);
    else
      fd = open(name, O_RDONLY | O_SYNC);
  }

  if (fd == -1) {
    sprintf(message_, "RtApiOss: device (%s) cannot be opened.", name);
    goto error;
  }

  // Get the sample format mask
  int mask;
  if (ioctl(fd, SNDCTL_DSP_GETFMTS, &mask) == -1) {
    close(fd);
    sprintf(message_, "RtApiOss: device (%s) can't get supported audio formats.",
            name);
    goto error;
  }

  // Determine how to set the device format.
  stream_.userFormat = format;
  device_format = -1;
  stream_.doByteSwap[mode] = false;
  if (format == RTAUDIO_SINT8) {
    if (mask & AFMT_S8) {
      device_format = AFMT_S8;
      stream_.deviceFormat[mode] = RTAUDIO_SINT8;
    }
  }
  else if (format == RTAUDIO_SINT16) {
    if (mask & AFMT_S16_NE) {
      device_format = AFMT_S16_NE;
      stream_.deviceFormat[mode] = RTAUDIO_SINT16;
    }
#if BYTE_ORDER == LITTLE_ENDIAN
    else if (mask & AFMT_S16_BE) {
      device_format = AFMT_S16_BE;
      stream_.deviceFormat[mode] = RTAUDIO_SINT16;
      stream_.doByteSwap[mode] = true;
    }
#else
    else if (mask & AFMT_S16_LE) {
      device_format = AFMT_S16_LE;
      stream_.deviceFormat[mode] = RTAUDIO_SINT16;
      stream_.doByteSwap[mode] = true;
    }
#endif
  }
#if defined (AFMT_S32_NE) && defined (AFMT_S32_LE) && defined (AFMT_S32_BE)
  else if (format == RTAUDIO_SINT32) {
    if (mask & AFMT_S32_NE) {
      device_format = AFMT_S32_NE;
      stream_.deviceFormat[mode] = RTAUDIO_SINT32;
    }
#if BYTE_ORDER == LITTLE_ENDIAN
    else if (mask & AFMT_S32_BE) {
      device_format = AFMT_S32_BE;
      stream_.deviceFormat[mode] = RTAUDIO_SINT32;
      stream_.doByteSwap[mode] = true;
    }
#else
    else if (mask & AFMT_S32_LE) {
      device_format = AFMT_S32_LE;
      stream_.deviceFormat[mode] = RTAUDIO_SINT32;
      stream_.doByteSwap[mode] = true;
    }
#endif
  }
#endif

  if (device_format == -1) {
    // The user requested format is not natively supported by the device.
    if (mask & AFMT_S16_NE) {
      device_format = AFMT_S16_NE;
      stream_.deviceFormat[mode] = RTAUDIO_SINT16;
    }
#if BYTE_ORDER == LITTLE_ENDIAN
    else if (mask & AFMT_S16_BE) {
      device_format = AFMT_S16_BE;
      stream_.deviceFormat[mode] = RTAUDIO_SINT16;
      stream_.doByteSwap[mode] = true;
    }
#else
    else if (mask & AFMT_S16_LE) {
      device_format = AFMT_S16_LE;
      stream_.deviceFormat[mode] = RTAUDIO_SINT16;
      stream_.doByteSwap[mode] = true;
    }
#endif
#if defined (AFMT_S32_NE) && defined (AFMT_S32_LE) && defined (AFMT_S32_BE)
    else if (mask & AFMT_S32_NE) {
      device_format = AFMT_S32_NE;
      stream_.deviceFormat[mode] = RTAUDIO_SINT32;
    }
#if BYTE_ORDER == LITTLE_ENDIAN
    else if (mask & AFMT_S32_BE) {
      device_format = AFMT_S32_BE;
      stream_.deviceFormat[mode] = RTAUDIO_SINT32;
      stream_.doByteSwap[mode] = true;
    }
#else
    else if (mask & AFMT_S32_LE) {
      device_format = AFMT_S32_LE;
      stream_.deviceFormat[mode] = RTAUDIO_SINT32;
      stream_.doByteSwap[mode] = true;
    }
#endif
#endif
    else if (mask & AFMT_S8) {
      device_format = AFMT_S8;
      stream_.deviceFormat[mode] = RTAUDIO_SINT8;
    }
  }

  if (stream_.deviceFormat[mode] == 0) {
    // This really shouldn't happen ...
    close(fd);
    sprintf(message_, "RtApiOss: device (%s) data format not supported by RtAudio.",
            name);
    goto error;
  }

  // Determine the number of channels for this device.  Note that the
  // channel value requested by the user might be < min_X_Channels.
  stream_.nUserChannels[mode] = channels;
  device_channels = channels;
  if (mode == OUTPUT) {
    if (channels < devices_[device].minOutputChannels)
      device_channels = devices_[device].minOutputChannels;
  }
  else { // mode == INPUT
    if (stream_.mode == OUTPUT && stream_.device[0] == device) {
      // We're doing duplex setup here.
      if (channels < devices_[device].minDuplexChannels)
        device_channels = devices_[device].minDuplexChannels;
    }
    else {
      if (channels < devices_[device].minInputChannels)
        device_channels = devices_[device].minInputChannels;
    }
  }
  stream_.nDeviceChannels[mode] = device_channels;

  // Attempt to set the buffer size.  According to OSS, the minimum
  // number of buffers is two.  The supposed minimum buffer size is 16
  // bytes, so that will be our lower bound.  The argument to this
  // call is in the form 0xMMMMSSSS (hex), where the buffer size (in
  // bytes) is given as 2^SSSS and the number of buffers as 2^MMMM.
  // We'll check the actual value used near the end of the setup
  // procedure.
  buffer_bytes = *bufferSize * formatBytes(stream_.deviceFormat[mode]) * device_channels;
  if (buffer_bytes < 16) buffer_bytes = 16;
  buffers = numberOfBuffers;
  if (buffers < 2) buffers = 2;
  temp = ((int) buffers << 16) + (int)(log10((double)buffer_bytes)/log10(2.0));
  if (ioctl(fd, SNDCTL_DSP_SETFRAGMENT, &temp)) {
    close(fd);
    sprintf(message_, "RtApiOss: error setting fragment size for device (%s).",
            name);
    goto error;
  }
  stream_.nBuffers = buffers;

  // Set the data format.
  temp = device_format;
  if (ioctl(fd, SNDCTL_DSP_SETFMT, &device_format) == -1 || device_format != temp) {
    close(fd);
    sprintf(message_, "RtApiOss: error setting data format for device (%s).",
            name);
    goto error;
  }

  // Set the number of channels.
  temp = device_channels;
  if (ioctl(fd, SNDCTL_DSP_CHANNELS, &device_channels) == -1 || device_channels != temp) {
    close(fd);
    sprintf(message_, "RtApiOss: error setting %d channels on device (%s).",
            temp, name);
    goto error;
  }

  // Set the sample rate.
  srate = sampleRate;
  temp = srate;
  if (ioctl(fd, SNDCTL_DSP_SPEED, &srate) == -1) {
    close(fd);
    sprintf(message_, "RtApiOss: error setting sample rate = %d on device (%s).",
            temp, name);
    goto error;
  }

  // Verify the sample rate setup worked.
  if (abs(srate - temp) > 100) {
    close(fd);
    sprintf(message_, "RtApiOss: error ... audio device (%s) doesn't support sample rate of %d.",
            name, temp);
    goto error;
  }
  stream_.sampleRate = sampleRate;

  if (ioctl(fd, SNDCTL_DSP_GETBLKSIZE, &buffer_bytes) == -1) {
    close(fd);
    sprintf(message_, "RtApiOss: error getting buffer size for device (%s).",
            name);
    goto error;
  }

  // Save buffer size (in sample frames).
  *bufferSize = buffer_bytes / (formatBytes(stream_.deviceFormat[mode]) * device_channels);
  stream_.bufferSize = *bufferSize;

  if (mode == INPUT && stream_.mode == OUTPUT &&
      stream_.device[0] == device) {
    // We're doing duplex setup here.
    stream_.deviceFormat[0] = stream_.deviceFormat[1];
    stream_.nDeviceChannels[0] = device_channels;
  }

  // Allocate the stream handles if necessary and then save.
  if ( stream_.apiHandle == 0 ) {
    handle = (int *) calloc(2, sizeof(int));
    stream_.apiHandle = (void *) handle;
    handle[0] = 0;
    handle[1] = 0;
  }
  else {
    handle = (int *) stream_.apiHandle;
  }
  handle[mode] = fd;

  // Set flags for buffer conversion
  stream_.doConvertBuffer[mode] = false;
  if (stream_.userFormat != stream_.deviceFormat[mode])
    stream_.doConvertBuffer[mode] = true;
  if (stream_.nUserChannels[mode] < stream_.nDeviceChannels[mode])
    stream_.doConvertBuffer[mode] = true;

  // Allocate necessary internal buffers
  if ( stream_.nUserChannels[0] != stream_.nUserChannels[1] ) {

    long buffer_bytes;
    if (stream_.nUserChannels[0] >= stream_.nUserChannels[1])
      buffer_bytes = stream_.nUserChannels[0];
    else
      buffer_bytes = stream_.nUserChannels[1];

    buffer_bytes *= *bufferSize * formatBytes(stream_.userFormat);
    if (stream_.userBuffer) free(stream_.userBuffer);
    stream_.userBuffer = (char *) calloc(buffer_bytes, 1);
    if (stream_.userBuffer == NULL) {
      close(fd);
      sprintf(message_, "RtApiOss: error allocating user buffer memory (%s).",
              name);
      goto error;
    }
  }

  if ( stream_.doConvertBuffer[mode] ) {

    long buffer_bytes;
    bool makeBuffer = true;
    if ( mode == OUTPUT )
      buffer_bytes = stream_.nDeviceChannels[0] * formatBytes(stream_.deviceFormat[0]);
    else { // mode == INPUT
      buffer_bytes = stream_.nDeviceChannels[1] * formatBytes(stream_.deviceFormat[1]);
      if ( stream_.mode == OUTPUT && stream_.deviceBuffer ) {
        long bytes_out = stream_.nDeviceChannels[0] * formatBytes(stream_.deviceFormat[0]);
        if ( buffer_bytes < bytes_out ) makeBuffer = false;
      }
    }

    if ( makeBuffer ) {
      buffer_bytes *= *bufferSize;
      if (stream_.deviceBuffer) free(stream_.deviceBuffer);
      stream_.deviceBuffer = (char *) calloc(buffer_bytes, 1);
      if (stream_.deviceBuffer == NULL) {
        close(fd);
        sprintf(message_, "RtApiOss: error allocating device buffer memory (%s).",
                name);
        goto error;
      }
    }
  }

  stream_.device[mode] = device;
  stream_.state = STREAM_STOPPED;

  if ( stream_.mode == OUTPUT && mode == INPUT ) {
    stream_.mode = DUPLEX;
    if (stream_.device[0] == device)
      handle[0] = fd;
  }
  else
    stream_.mode = mode;

  return SUCCESS;

 error:
  if (handle) {
    if (handle[0])
      close(handle[0]);
    free(handle);
    stream_.apiHandle = 0;
  }

  if (stream_.userBuffer) {
    free(stream_.userBuffer);
    stream_.userBuffer = 0;
  }

  error(RtError::WARNING);
  return FAILURE;
}

void RtApiOss :: closeStream()
{
  // We don't want an exception to be thrown here because this
  // function is called by our class destructor.  So, do our own
  // stream check.
  if ( stream_.mode == UNINITIALIZED ) {
    sprintf(message_, "RtApiOss::closeStream(): no open stream to close!");
    error(RtError::WARNING);
    return;
  }

  int *handle = (int *) stream_.apiHandle;
  if (stream_.state == STREAM_RUNNING) {
    if (stream_.mode == OUTPUT || stream_.mode == DUPLEX)
      ioctl(handle[0], SNDCTL_DSP_RESET, 0);
    else
      ioctl(handle[1], SNDCTL_DSP_RESET, 0);
    stream_.state = STREAM_STOPPED;
  }

  if (stream_.callbackInfo.usingCallback) {
    stream_.callbackInfo.usingCallback = false;
    pthread_join(stream_.callbackInfo.thread, NULL);
  }

  if (handle) {
    if (handle[0]) close(handle[0]);
    if (handle[1]) close(handle[1]);
    free(handle);
    stream_.apiHandle = 0;
  }

  if (stream_.userBuffer) {
    free(stream_.userBuffer);
    stream_.userBuffer = 0;
  }

  if (stream_.deviceBuffer) {
    free(stream_.deviceBuffer);
    stream_.deviceBuffer = 0;
  }

  stream_.mode = UNINITIALIZED;
}

void RtApiOss :: startStream()
{
  verifyStream();
  if (stream_.state == STREAM_RUNNING) return;

  MUTEX_LOCK(&stream_.mutex);

  stream_.state = STREAM_RUNNING;

  // No need to do anything else here ... OSS automatically starts
  // when fed samples.

  MUTEX_UNLOCK(&stream_.mutex);
}

void RtApiOss :: stopStream()
{
  verifyStream();
  if (stream_.state == STREAM_STOPPED) return;

  // Change the state before the lock to improve shutdown response
  // when using a callback.
  stream_.state = STREAM_STOPPED;
  MUTEX_LOCK(&stream_.mutex);

  int err;
  int *handle = (int *) stream_.apiHandle;
  if (stream_.mode == OUTPUT || stream_.mode == DUPLEX) {
    err = ioctl(handle[0], SNDCTL_DSP_POST, 0);
    //err = ioctl(handle[0], SNDCTL_DSP_SYNC, 0);
    if (err < -1) {
      sprintf(message_, "RtApiOss: error stopping device (%s).",
              devices_[stream_.device[0]].name.c_str());
      error(RtError::DRIVER_ERROR);
    }
  }
  else {
    err = ioctl(handle[1], SNDCTL_DSP_POST, 0);
    //err = ioctl(handle[1], SNDCTL_DSP_SYNC, 0);
    if (err < -1) {
      sprintf(message_, "RtApiOss: error stopping device (%s).",
              devices_[stream_.device[1]].name.c_str());
      error(RtError::DRIVER_ERROR);
    }
  }

  MUTEX_UNLOCK(&stream_.mutex);
}

void RtApiOss :: abortStream()
{
  stopStream();
}

int RtApiOss :: streamWillBlock()
{
  verifyStream();
  if (stream_.state == STREAM_STOPPED) return 0;

  MUTEX_LOCK(&stream_.mutex);

  int bytes = 0, channels = 0, frames = 0;
  audio_buf_info info;
  int *handle = (int *) stream_.apiHandle;
  if (stream_.mode == OUTPUT || stream_.mode == DUPLEX) {
    ioctl(handle[0], SNDCTL_DSP_GETOSPACE, &info);
    bytes = info.bytes;
    channels = stream_.nDeviceChannels[0];
  }

  if (stream_.mode == INPUT || stream_.mode == DUPLEX) {
    ioctl(handle[1], SNDCTL_DSP_GETISPACE, &info);
    if (stream_.mode == DUPLEX ) {
      bytes = (bytes < info.bytes) ? bytes : info.bytes;
      channels = stream_.nDeviceChannels[0];
    }
    else {
      bytes = info.bytes;
      channels = stream_.nDeviceChannels[1];
    }
  }

  frames = (int) (bytes / (channels * formatBytes(stream_.deviceFormat[0])));
  frames -= stream_.bufferSize;
  if (frames < 0) frames = 0;

  MUTEX_UNLOCK(&stream_.mutex);
  return frames;
}

void RtApiOss :: tickStream()
{
  verifyStream();

  int stopStream = 0;
  if (stream_.state == STREAM_STOPPED) {
    if (stream_.callbackInfo.usingCallback) usleep(50000); // sleep 50 milliseconds
    return;
  }
  else if (stream_.callbackInfo.usingCallback) {
    RtAudioCallback callback = (RtAudioCallback) stream_.callbackInfo.callback;
    stopStream = callback(stream_.userBuffer, stream_.bufferSize, stream_.callbackInfo.userData);
  }

  MUTEX_LOCK(&stream_.mutex);

  // The state might change while waiting on a mutex.
  if (stream_.state == STREAM_STOPPED)
    goto unlock;

  int result, *handle;
  char *buffer;
  int samples;
  RtAudioFormat format;
  handle = (int *) stream_.apiHandle;
  if (stream_.mode == OUTPUT || stream_.mode == DUPLEX) {

    // Setup parameters and do buffer conversion if necessary.
    if (stream_.doConvertBuffer[0]) {
      convertStreamBuffer(OUTPUT);
      buffer = stream_.deviceBuffer;
      samples = stream_.bufferSize * stream_.nDeviceChannels[0];
      format = stream_.deviceFormat[0];
    }
    else {
      buffer = stream_.userBuffer;
      samples = stream_.bufferSize * stream_.nUserChannels[0];
      format = stream_.userFormat;
    }

    // Do byte swapping if necessary.
    if (stream_.doByteSwap[0])
      byteSwapBuffer(buffer, samples, format);

    // Write samples to device.
    result = write(handle[0], buffer, samples * formatBytes(format));

    if (result == -1) {
      // This could be an underrun, but the basic OSS API doesn't provide a means for determining that.
      sprintf(message_, "RtApiOss: audio write error for device (%s).",
              devices_[stream_.device[0]].name.c_str());
      error(RtError::DRIVER_ERROR);
    }
  }

  if (stream_.mode == INPUT || stream_.mode == DUPLEX) {

    // Setup parameters.
    if (stream_.doConvertBuffer[1]) {
      buffer = stream_.deviceBuffer;
      samples = stream_.bufferSize * stream_.nDeviceChannels[1];
      format = stream_.deviceFormat[1];
    }
    else {
      buffer = stream_.userBuffer;
      samples = stream_.bufferSize * stream_.nUserChannels[1];
      format = stream_.userFormat;
    }

    // Read samples from device.
    result = read(handle[1], buffer, samples * formatBytes(format));

    if (result == -1) {
      // This could be an overrun, but the basic OSS API doesn't provide a means for determining that.
      sprintf(message_, "RtApiOss: audio read error for device (%s).",
              devices_[stream_.device[1]].name.c_str());
      error(RtError::DRIVER_ERROR);
    }

    // Do byte swapping if necessary.
    if (stream_.doByteSwap[1])
      byteSwapBuffer(buffer, samples, format);

    // Do buffer conversion if necessary.
    if (stream_.doConvertBuffer[1])
      convertStreamBuffer(INPUT);
  }

 unlock:
  MUTEX_UNLOCK(&stream_.mutex);

  if (stream_.callbackInfo.usingCallback && stopStream)
    this->stopStream();

  RtApi::tickStream();
}

void RtApiOss :: setStreamCallback(RtAudioCallback callback, void *userData)
{
  verifyStream();

  CallbackInfo *info = (CallbackInfo *) &stream_.callbackInfo;
  if ( info->usingCallback ) {
    sprintf(message_, "RtApiOss: A callback is already set for this stream!");
    error(RtError::WARNING);
    return;
  }

  info->callback = (void *) callback;
  info->userData = userData;
  info->usingCallback = true;
  info->object = (void *) this;

  // Set the thread attributes for joinable and realtime scheduling
  // priority.  The higher priority will only take affect if the
  // program is run as root or suid.
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
  pthread_attr_setschedpolicy(&attr, SCHED_RR);

  int err = pthread_create(&(info->thread), &attr, ossCallbackHandler, &stream_.callbackInfo);
  pthread_attr_destroy(&attr);
  if (err) {
    info->usingCallback = false;
    sprintf(message_, "RtApiOss: error starting callback thread!");
    error(RtError::THREAD_ERROR);
  }
}

void RtApiOss :: cancelStreamCallback()
{
  verifyStream();

  if (stream_.callbackInfo.usingCallback) {

    if (stream_.state == STREAM_RUNNING)
      stopStream();

    MUTEX_LOCK(&stream_.mutex);

    stream_.callbackInfo.usingCallback = false;
    pthread_join(stream_.callbackInfo.thread, NULL);
    stream_.callbackInfo.thread = 0;
    stream_.callbackInfo.callback = NULL;
    stream_.callbackInfo.userData = NULL;

    MUTEX_UNLOCK(&stream_.mutex);
  }
}

extern "C" void *ossCallbackHandler(void *ptr)
{
  CallbackInfo *info = (CallbackInfo *) ptr;
  RtApiOss *object = (RtApiOss *) info->object;
  bool *usingCallback = &info->usingCallback;

  while ( *usingCallback ) {
    pthread_testcancel();
    try {
      object->tickStream();
    }
    catch (RtError &exception) {
      fprintf(stderr, "\nRtApiOss: callback thread error (%s) ... closing thread.\n\n",
              exception.getMessageString());
      break;
    }
  }

  return 0;
}

//******************** End of __LINUX_OSS__ *********************//
#endif

#if defined(__MACOSX_CORE__)


// The OS X CoreAudio API is designed to use a separate callback
// procedure for each of its audio devices.  A single RtAudio duplex
// stream using two different devices is supported here, though it
// cannot be guaranteed to always behave correctly because we cannot
// synchronize these two callbacks.  This same functionality can be
// achieved with better synchrony by opening two separate streams for
// the devices and using RtAudio blocking calls (i.e. tickStream()).
//
// A property listener is installed for over/underrun information.
// However, no functionality is currently provided to allow property
// listeners to trigger user handlers because it is unclear what could
// be done if a critical stream parameter (buffer size, sample rate,
// device disconnect) notification arrived.  The listeners entail
// quite a bit of extra code and most likely, a user program wouldn't
// be prepared for the result anyway.

// A structure to hold various information related to the CoreAuio API
// implementation.
struct CoreHandle {
  UInt32 index[2];
  bool stopStream;
  bool xrun;
  char *deviceBuffer;
  pthread_cond_t condition;

  CoreHandle()
    :stopStream(false), xrun(false), deviceBuffer(0) {}
};

RtApiCore :: RtApiCore()
{
  this->initialize();

  if (nDevices_ <= 0) {
    sprintf(message_, "RtApiCore: no Macintosh OS-X Core Audio devices found!");
    error(RtError::NO_DEVICES_FOUND);
 }
}

RtApiCore :: ~RtApiCore()
{
  // The subclass destructor gets called before the base class
  // destructor, so close an existing stream before deallocating
  // apiDeviceId memory.
  if ( stream_.mode != UNINITIALIZED ) closeStream();

  // Free our allocated apiDeviceId memory.
  AudioDeviceID *id;
  for ( unsigned int i=0; i<devices_.size(); i++ ) {
    id = (AudioDeviceID *) devices_[i].apiDeviceId;
    if (id) free(id);
  }
}

void RtApiCore :: initialize(void)
{
  OSStatus err = noErr;
  UInt32 dataSize;
  AudioDeviceID	*deviceList = NULL;
  nDevices_ = 0;

  // Find out how many audio devices there are, if any.
  err = AudioHardwareGetPropertyInfo(kAudioHardwarePropertyDevices, &dataSize, NULL);
  if (err != noErr) {
    sprintf(message_, "RtApiCore: OS-X error getting device info!");
    error(RtError::SYSTEM_ERROR);
  }

  nDevices_ = dataSize / sizeof(AudioDeviceID);
  if (nDevices_ == 0) return;

  // Make space for the devices we are about to get.
  deviceList = (AudioDeviceID	*) malloc( dataSize );
  if (deviceList == NULL) {
    sprintf(message_, "RtApiCore: memory allocation error during initialization!");
    error(RtError::MEMORY_ERROR);
  }

  // Get the array of AudioDeviceIDs.
  err = AudioHardwareGetProperty(kAudioHardwarePropertyDevices, &dataSize, (void *) deviceList);
  if (err != noErr) {
    free(deviceList);
    sprintf(message_, "RtApiCore: OS-X error getting device properties!");
    error(RtError::SYSTEM_ERROR);
  }

  // Create list of device structures and write device identifiers.
  RtApiDevice device;
  AudioDeviceID *id;
  for (int i=0; i<nDevices_; i++) {
    devices_.push_back(device);
    id = (AudioDeviceID *) malloc( sizeof(AudioDeviceID) );
    *id = deviceList[i];
    devices_[i].apiDeviceId = (void *) id;
  }

  free(deviceList);
}

int RtApiCore :: getDefaultInputDevice(void)
{
  AudioDeviceID id, *deviceId;
  UInt32 dataSize = sizeof( AudioDeviceID );

  OSStatus result = AudioHardwareGetProperty( kAudioHardwarePropertyDefaultInputDevice,
                                              &dataSize, &id );

  if (result != noErr) {
    sprintf( message_, "RtApiCore: OS-X error getting default input device." );
    error(RtError::WARNING);
    return 0;
  }

  for ( int i=0; i<nDevices_; i++ ) {
    deviceId = (AudioDeviceID *) devices_[i].apiDeviceId;
    if ( id == *deviceId ) return i;
  }

  return 0;
}

int RtApiCore :: getDefaultOutputDevice(void)
{
  AudioDeviceID id, *deviceId;
  UInt32 dataSize = sizeof( AudioDeviceID );

  OSStatus result = AudioHardwareGetProperty( kAudioHardwarePropertyDefaultOutputDevice,
                                              &dataSize, &id );

  if (result != noErr) {
    sprintf( message_, "RtApiCore: OS-X error getting default output device." );
    error(RtError::WARNING);
    return 0;
  }

  for ( int i=0; i<nDevices_; i++ ) {
    deviceId = (AudioDeviceID *) devices_[i].apiDeviceId;
    if ( id == *deviceId ) return i;
  }

  return 0;
}

static bool deviceSupportsFormat( AudioDeviceID id, bool isInput,
                                  AudioStreamBasicDescription	*desc, bool isDuplex )
{
  OSStatus result = noErr;
  UInt32 dataSize = sizeof( AudioStreamBasicDescription );

  result = AudioDeviceGetProperty( id, 0, isInput,
                                   kAudioDevicePropertyStreamFormatSupported,
                                   &dataSize, desc );

  if (result == kAudioHardwareNoError) {
    if ( isDuplex ) {
      result = AudioDeviceGetProperty( id, 0, true,
                                       kAudioDevicePropertyStreamFormatSupported,
                                       &dataSize, desc );


      if (result != kAudioHardwareNoError)
        return false;
    }
    return true;
  }

  return false;
}

void RtApiCore :: probeDeviceInfo( RtApiDevice *info )
{
  OSStatus err = noErr;

  // Get the device manufacturer and name.
  char	name[256];
  char	fullname[512];
  UInt32 dataSize = 256;
  AudioDeviceID *id = (AudioDeviceID *) info->apiDeviceId;
  err = AudioDeviceGetProperty( *id, 0, false,
                                kAudioDevicePropertyDeviceManufacturer,
                                &dataSize, name );
  if (err != noErr) {
    sprintf( message_, "RtApiCore: OS-X error getting device manufacturer." );
    error(RtError::DEBUG_WARNING);
    return;
  }
  strncpy(fullname, name, 256);
  strcat(fullname, ": " );

  dataSize = 256;
  err = AudioDeviceGetProperty( *id, 0, false,
                                kAudioDevicePropertyDeviceName,
                                &dataSize, name );
  if (err != noErr) {
    sprintf( message_, "RtApiCore: OS-X error getting device name." );
    error(RtError::DEBUG_WARNING);
    return;
  }
  strncat(fullname, name, 254);
  info->name.erase();
  info->name.append( (const char *)fullname, strlen(fullname)+1);

  // Get output channel information.
  unsigned int i, minChannels = 0, maxChannels = 0, nStreams = 0;
  AudioBufferList	*bufferList = nil;
  err = AudioDeviceGetPropertyInfo( *id, 0, false,
                                    kAudioDevicePropertyStreamConfiguration,
                                    &dataSize, NULL );
  if (err == noErr && dataSize > 0) {
    bufferList = (AudioBufferList *) malloc( dataSize );
    if (bufferList == NULL) {
      sprintf(message_, "RtApiCore: memory allocation error!");
      error(RtError::DEBUG_WARNING);
      return;
    }

    err = AudioDeviceGetProperty( *id, 0, false,
                                  kAudioDevicePropertyStreamConfiguration,
                                  &dataSize, bufferList );
    if (err == noErr) {
      maxChannels = 0;
      minChannels = 1000;
      nStreams = bufferList->mNumberBuffers;
      for ( i=0; i<nStreams; i++ ) {
        maxChannels += bufferList->mBuffers[i].mNumberChannels;
        if ( bufferList->mBuffers[i].mNumberChannels < minChannels )
          minChannels = bufferList->mBuffers[i].mNumberChannels;
      }
    }
  }
  free (bufferList);

  if (err != noErr || dataSize <= 0) {
    sprintf( message_, "RtApiCore: OS-X error getting output channels for device (%s).",
             info->name.c_str() );
    error(RtError::DEBUG_WARNING);
    return;
  }

  if ( nStreams ) {
    if ( maxChannels > 0 )
      info->maxOutputChannels = maxChannels;
    if ( minChannels > 0 )
      info->minOutputChannels = minChannels;
  }

  // Get input channel information.
  bufferList = nil;
  err = AudioDeviceGetPropertyInfo( *id, 0, true,
                                    kAudioDevicePropertyStreamConfiguration,
                                    &dataSize, NULL );
  if (err == noErr && dataSize > 0) {
    bufferList = (AudioBufferList *) malloc( dataSize );
    if (bufferList == NULL) {
      sprintf(message_, "RtApiCore: memory allocation error!");
      error(RtError::DEBUG_WARNING);
      return;
    }
    err = AudioDeviceGetProperty( *id, 0, true,
                                  kAudioDevicePropertyStreamConfiguration,
                                  &dataSize, bufferList );
    if (err == noErr) {
      maxChannels = 0;
      minChannels = 1000;
      nStreams = bufferList->mNumberBuffers;
      for ( i=0; i<nStreams; i++ ) {
        if ( bufferList->mBuffers[i].mNumberChannels < minChannels )
          minChannels = bufferList->mBuffers[i].mNumberChannels;
        maxChannels += bufferList->mBuffers[i].mNumberChannels;
      }
    }
  }
  free (bufferList);

  if (err != noErr || dataSize <= 0) {
    sprintf( message_, "RtApiCore: OS-X error getting input channels for device (%s).",
             info->name.c_str() );
    error(RtError::DEBUG_WARNING);
    return;
  }

  if ( nStreams ) {
    if ( maxChannels > 0 )
      info->maxInputChannels = maxChannels;
    if ( minChannels > 0 )
      info->minInputChannels = minChannels;
  }

  // If device opens for both playback and capture, we determine the channels.
  if (info->maxOutputChannels > 0 && info->maxInputChannels > 0) {
    info->hasDuplexSupport = true;
    info->maxDuplexChannels = (info->maxOutputChannels > info->maxInputChannels) ?
      info->maxInputChannels : info->maxOutputChannels;
    info->minDuplexChannels = (info->minOutputChannels > info->minInputChannels) ?
      info->minInputChannels : info->minOutputChannels;
  }

  // Probe the device sample rate and data format parameters.  The
  // core audio query mechanism is performed on a "stream"
  // description, which can have a variable number of channels and
  // apply to input or output only.

  // Create a stream description structure.
  AudioStreamBasicDescription	description;
  dataSize = sizeof( AudioStreamBasicDescription );
  memset(&description, 0, sizeof(AudioStreamBasicDescription));
  bool isInput = false;
  if ( info->maxOutputChannels == 0 ) isInput = true;
  bool isDuplex = false;
  if ( info->maxDuplexChannels > 0 ) isDuplex = true;

  // Determine the supported sample rates.
  info->sampleRates.clear();
  for (unsigned int k=0; k<MAX_SAMPLE_RATES; k++) {
    description.mSampleRate = (double) SAMPLE_RATES[k];
    if ( deviceSupportsFormat( *id, isInput, &description, isDuplex ) )
      info->sampleRates.push_back( SAMPLE_RATES[k] );
  }

  if (info->sampleRates.size() == 0) {
    sprintf( message_, "RtApiCore: No supported sample rates found for OS-X device (%s).",
             info->name.c_str() );
    error(RtError::DEBUG_WARNING);
    return;
  }

  // Determine the supported data formats.
  info->nativeFormats = 0;
  description.mFormatID = kAudioFormatLinearPCM;
  description.mBitsPerChannel = 8;
  description.mFormatFlags = kLinearPCMFormatFlagIsSignedInteger | kLinearPCMFormatFlagIsPacked | kLinearPCMFormatFlagIsBigEndian;
  if ( deviceSupportsFormat( *id, isInput, &description, isDuplex ) )
    info->nativeFormats |= RTAUDIO_SINT8;
  else {
    description.mFormatFlags &= ~kLinearPCMFormatFlagIsBigEndian;
    if ( deviceSupportsFormat( *id, isInput, &description, isDuplex ) )
      info->nativeFormats |= RTAUDIO_SINT8;
  }

  description.mBitsPerChannel = 16;
  description.mFormatFlags |= kLinearPCMFormatFlagIsBigEndian;
  if ( deviceSupportsFormat( *id, isInput, &description, isDuplex ) )
    info->nativeFormats |= RTAUDIO_SINT16;
  else {
    description.mFormatFlags &= ~kLinearPCMFormatFlagIsBigEndian;
    if ( deviceSupportsFormat( *id, isInput, &description, isDuplex ) )
      info->nativeFormats |= RTAUDIO_SINT16;
  }

  description.mBitsPerChannel = 32;
  description.mFormatFlags |= kLinearPCMFormatFlagIsBigEndian;
  if ( deviceSupportsFormat( *id, isInput, &description, isDuplex ) )
    info->nativeFormats |= RTAUDIO_SINT32;
  else {
    description.mFormatFlags &= ~kLinearPCMFormatFlagIsBigEndian;
    if ( deviceSupportsFormat( *id, isInput, &description, isDuplex ) )
      info->nativeFormats |= RTAUDIO_SINT32;
  }

  description.mBitsPerChannel = 24;
  description.mFormatFlags = kLinearPCMFormatFlagIsSignedInteger | kLinearPCMFormatFlagIsAlignedHigh | kLinearPCMFormatFlagIsBigEndian;
  if ( deviceSupportsFormat( *id, isInput, &description, isDuplex ) )
    info->nativeFormats |= RTAUDIO_SINT24;
  else {
    description.mFormatFlags &= ~kLinearPCMFormatFlagIsBigEndian;
    if ( deviceSupportsFormat( *id, isInput, &description, isDuplex ) )
      info->nativeFormats |= RTAUDIO_SINT24;
  }

  description.mBitsPerChannel = 32;
  description.mFormatFlags = kLinearPCMFormatFlagIsFloat | kLinearPCMFormatFlagIsPacked | kLinearPCMFormatFlagIsBigEndian;
  if ( deviceSupportsFormat( *id, isInput, &description, isDuplex ) )
    info->nativeFormats |= RTAUDIO_FLOAT32;
  else {
    description.mFormatFlags &= ~kLinearPCMFormatFlagIsBigEndian;
    if ( deviceSupportsFormat( *id, isInput, &description, isDuplex ) )
      info->nativeFormats |= RTAUDIO_FLOAT32;
  }

  description.mBitsPerChannel = 64;
  description.mFormatFlags |= kLinearPCMFormatFlagIsBigEndian;
  if ( deviceSupportsFormat( *id, isInput, &description, isDuplex ) )
    info->nativeFormats |= RTAUDIO_FLOAT64;
  else {
    description.mFormatFlags &= ~kLinearPCMFormatFlagIsBigEndian;
    if ( deviceSupportsFormat( *id, isInput, &description, isDuplex ) )
      info->nativeFormats |= RTAUDIO_FLOAT64;
  }

  // Check that we have at least one supported format.
  if (info->nativeFormats == 0) {
    sprintf(message_, "RtApiCore: OS-X device (%s) data format not supported by RtAudio.",
            info->name.c_str());
    error(RtError::DEBUG_WARNING);
    return;
  }

  info->probed = true;
}

OSStatus callbackHandler(AudioDeviceID inDevice,
                         const AudioTimeStamp* inNow,
                         const AudioBufferList* inInputData,
                         const AudioTimeStamp* inInputTime,
                         AudioBufferList* outOutputData,
                         const AudioTimeStamp* inOutputTime, 
                         void* infoPointer)
{
  CallbackInfo *info = (CallbackInfo *) infoPointer;

  RtApiCore *object = (RtApiCore *) info->object;
  try {
    object->callbackEvent( inDevice, (void *)inInputData, (void *)outOutputData );
  }
  catch (RtError &exception) {
    fprintf(stderr, "\nRtApiCore: callback handler error (%s)!\n\n", exception.getMessageString());
    return kAudioHardwareUnspecifiedError;
  }

  return kAudioHardwareNoError;
}

OSStatus deviceListener(AudioDeviceID inDevice,
                        UInt32 channel,
                        Boolean isInput,
                        AudioDevicePropertyID propertyID,
                        void* handlePointer)
{
  CoreHandle *handle = (CoreHandle *) handlePointer;
  if ( propertyID == kAudioDeviceProcessorOverload ) {
    if ( isInput )
      fprintf(stderr, "\nRtApiCore: OS-X audio input overrun detected!\n");
    else
      fprintf(stderr, "\nRtApiCore: OS-X audio output underrun detected!\n");
    handle->xrun = true;
  }

  return kAudioHardwareNoError;
}

bool RtApiCore :: probeDeviceOpen( int device, StreamMode mode, int channels, 
                                   int sampleRate, RtAudioFormat format,
                                   int *bufferSize, int numberOfBuffers )
{
  // Setup for stream mode.
  bool isInput = false;
  AudioDeviceID id = *((AudioDeviceID *) devices_[device].apiDeviceId);
  if ( mode == INPUT ) isInput = true;

  // Search for a stream which contains the desired number of channels.
  OSStatus err = noErr;
  UInt32 dataSize;
  unsigned int deviceChannels, nStreams = 0;
  UInt32 iChannel = 0, iStream = 0;
  AudioBufferList	*bufferList = nil;
  err = AudioDeviceGetPropertyInfo( id, 0, isInput,
                                    kAudioDevicePropertyStreamConfiguration,
                                    &dataSize, NULL );

  if (err == noErr && dataSize > 0) {
    bufferList = (AudioBufferList *) malloc( dataSize );
    if (bufferList == NULL) {
      sprintf(message_, "RtApiCore: memory allocation error in probeDeviceOpen()!");
      error(RtError::DEBUG_WARNING);
      return FAILURE;
    }
    err = AudioDeviceGetProperty( id, 0, isInput,
                                  kAudioDevicePropertyStreamConfiguration,
                                  &dataSize, bufferList );

    if (err == noErr) {
      stream_.deInterleave[mode] = false;
      nStreams = bufferList->mNumberBuffers;
      for ( iStream=0; iStream<nStreams; iStream++ ) {
        if ( bufferList->mBuffers[iStream].mNumberChannels >= (unsigned int) channels ) break;
        iChannel += bufferList->mBuffers[iStream].mNumberChannels;
      }
      // If we didn't find a single stream above, see if we can meet
      // the channel specification in mono mode (i.e. using separate
      // non-interleaved buffers).  This can only work if there are N
      // consecutive one-channel streams, where N is the number of
      // desired channels.
      iChannel = 0;
      if ( iStream >= nStreams && nStreams >= (unsigned int) channels ) {
        int counter = 0;
        for ( iStream=0; iStream<nStreams; iStream++ ) {
          if ( bufferList->mBuffers[iStream].mNumberChannels == 1 )
            counter++;
          else
            counter = 0;
          if ( counter == channels ) {
            iStream -= channels - 1;
            iChannel -= channels - 1;
            stream_.deInterleave[mode] = true;
            break;
          }
          iChannel += bufferList->mBuffers[iStream].mNumberChannels;
        }
      }
    }
  }
  if (err != noErr || dataSize <= 0) {
    if ( bufferList ) free( bufferList );
    sprintf( message_, "RtApiCore: OS-X error getting channels for device (%s).",
             devices_[device].name.c_str() );
    error(RtError::DEBUG_WARNING);
    return FAILURE;
  }

  if (iStream >= nStreams) {
    free (bufferList);
    sprintf( message_, "RtApiCore: unable to find OS-X audio stream on device (%s) for requested channels (%d).",
             devices_[device].name.c_str(), channels );
    error(RtError::DEBUG_WARNING);
    return FAILURE;
  }

  // This is ok even for mono mode ... it gets updated later.
  deviceChannels = bufferList->mBuffers[iStream].mNumberChannels;
  free (bufferList);

  // Determine the buffer size.
  AudioValueRange	bufferRange;
  dataSize = sizeof(AudioValueRange);
  err = AudioDeviceGetProperty( id, 0, isInput,
                                kAudioDevicePropertyBufferSizeRange,
                                &dataSize, &bufferRange);
  if (err != noErr) {
    sprintf( message_, "RtApiCore: OS-X error getting buffer size range for device (%s).",
             devices_[device].name.c_str() );
    error(RtError::DEBUG_WARNING);
    return FAILURE;
  }

  long bufferBytes = *bufferSize * deviceChannels * formatBytes(RTAUDIO_FLOAT32);
  if (bufferRange.mMinimum > bufferBytes) bufferBytes = (int) bufferRange.mMinimum;
  else if (bufferRange.mMaximum < bufferBytes) bufferBytes = (int) bufferRange.mMaximum;

  // Set the buffer size.  For mono mode, I'm assuming we only need to
  // make this setting for the first channel.
  UInt32 theSize = (UInt32) bufferBytes;
  dataSize = sizeof( UInt32);
  err = AudioDeviceSetProperty(id, NULL, 0, isInput,
                               kAudioDevicePropertyBufferSize,
                               dataSize, &theSize);
  if (err != noErr) {
    sprintf( message_, "RtApiCore: OS-X error setting the buffer size for device (%s).",
             devices_[device].name.c_str() );
    error(RtError::DEBUG_WARNING);
    return FAILURE;
  }

  // If attempting to setup a duplex stream, the bufferSize parameter
  // MUST be the same in both directions!
  *bufferSize = bufferBytes / ( deviceChannels * formatBytes(RTAUDIO_FLOAT32) );
  if ( stream_.mode == OUTPUT && mode == INPUT && *bufferSize != stream_.bufferSize ) {
    sprintf( message_, "RtApiCore: OS-X error setting buffer size for duplex stream on device (%s).",
             devices_[device].name.c_str() );
    error(RtError::DEBUG_WARNING);
    return FAILURE;
  }

  stream_.bufferSize = *bufferSize;
  stream_.nBuffers = 1;

  // Set the stream format description.  Do for each channel in mono mode.
  AudioStreamBasicDescription	description;
  dataSize = sizeof( AudioStreamBasicDescription );
  if ( stream_.deInterleave[mode] ) nStreams = channels;
  else nStreams = 1;
  for ( unsigned int i=0; i<nStreams; i++, iChannel++ ) {

    err = AudioDeviceGetProperty( id, iChannel, isInput,
                                  kAudioDevicePropertyStreamFormat,
                                  &dataSize, &description );
    if (err != noErr) {
      sprintf( message_, "RtApiCore: OS-X error getting stream format for device (%s).",
               devices_[device].name.c_str() );
      error(RtError::DEBUG_WARNING);
      return FAILURE;
    }

    // Only try to change it if the sample rate is not within 1.0 of
    // the rate we want, or if the format is not Linear PCM.
    if (fabs(description.mSampleRate - (double)sampleRate) > 1.0 ||
        description.mFormatID != kAudioFormatLinearPCM) {

      // Set the sample rate and data format id.
      description.mSampleRate = (double) sampleRate;
      description.mFormatID = kAudioFormatLinearPCM;
      
      err = AudioDeviceSetProperty( id, NULL, iChannel, isInput,
                                    kAudioDevicePropertyStreamFormat,
                                    dataSize, &description );
      if (err != noErr) {
        sprintf( message_, "RtApiCore: OS-X error setting sample rate or data format for device (%s).",
                 devices_[device].name.c_str() );
        error(RtError::DEBUG_WARNING);
        return FAILURE;
      }
    }
  }

  // Check whether we need byte-swapping (assuming OS-X host is big-endian).
  iChannel -= nStreams;
  err = AudioDeviceGetProperty( id, iChannel, isInput,
                                kAudioDevicePropertyStreamFormat,
                                &dataSize, &description );
  if (err != noErr) {
    sprintf( message_, "RtApiCore: OS-X error getting stream format for device (%s).", devices_[device].name.c_str() );
    error(RtError::DEBUG_WARNING);
    return FAILURE;
  }

  stream_.doByteSwap[mode] = false;
  if ( !description.mFormatFlags & kLinearPCMFormatFlagIsBigEndian )
    stream_.doByteSwap[mode] = true;

  // From the CoreAudio documentation, PCM data must be supplied as
  // 32-bit floats.
  stream_.userFormat = format;
  stream_.deviceFormat[mode] = RTAUDIO_FLOAT32;

  if ( stream_.deInterleave[mode] ) // mono mode
    stream_.nDeviceChannels[mode] = channels;
  else
    stream_.nDeviceChannels[mode] = description.mChannelsPerFrame;
  stream_.nUserChannels[mode] = channels;

  // Set flags for buffer conversion.
  stream_.doConvertBuffer[mode] = false;
  if (stream_.userFormat != stream_.deviceFormat[mode])
    stream_.doConvertBuffer[mode] = true;
  if (stream_.nUserChannels[mode] < stream_.nDeviceChannels[mode])
    stream_.doConvertBuffer[mode] = true;
  if (stream_.nUserChannels[mode] > 1 && stream_.deInterleave[mode])
    stream_.doConvertBuffer[mode] = true;

  // Allocate our CoreHandle structure for the stream.
  CoreHandle *handle;
  if ( stream_.apiHandle == 0 ) {
    handle = (CoreHandle *) calloc(1, sizeof(CoreHandle));
    if ( handle == NULL ) {
      sprintf(message_, "RtApiCore: OS-X error allocating coreHandle memory (%s).",
              devices_[device].name.c_str());
      goto error;
    }
    handle->index[0] = 0;
    handle->index[1] = 0;
    if ( pthread_cond_init(&handle->condition, NULL) ) {
      sprintf(message_, "RtApiCore: error initializing pthread condition variable (%s).",
              devices_[device].name.c_str());
      goto error;
    }
    stream_.apiHandle = (void *) handle;
  }
  else
     handle = (CoreHandle *) stream_.apiHandle;
  handle->index[mode] = iStream;

  // Allocate necessary internal buffers.
  if ( stream_.nUserChannels[0] != stream_.nUserChannels[1] ) {

    long buffer_bytes;
    if (stream_.nUserChannels[0] >= stream_.nUserChannels[1])
      buffer_bytes = stream_.nUserChannels[0];
    else
      buffer_bytes = stream_.nUserChannels[1];

    buffer_bytes *= *bufferSize * formatBytes(stream_.userFormat);
    if (stream_.userBuffer) free(stream_.userBuffer);
    stream_.userBuffer = (char *) calloc(buffer_bytes, 1);
    if (stream_.userBuffer == NULL) {
      sprintf(message_, "RtApiCore: OS-X error allocating user buffer memory (%s).",
              devices_[device].name.c_str());
      goto error;
    }
  }

  if ( stream_.deInterleave[mode] ) {

    long buffer_bytes;
    bool makeBuffer = true;
    if ( mode == OUTPUT )
      buffer_bytes = stream_.nDeviceChannels[0] * formatBytes(stream_.deviceFormat[0]);
    else { // mode == INPUT
      buffer_bytes = stream_.nDeviceChannels[1] * formatBytes(stream_.deviceFormat[1]);
      if ( stream_.mode == OUTPUT && stream_.deviceBuffer ) {
        long bytes_out = stream_.nDeviceChannels[0] * formatBytes(stream_.deviceFormat[0]);
        if ( buffer_bytes < bytes_out ) makeBuffer = false;
      }
    }

    if ( makeBuffer ) {
      buffer_bytes *= *bufferSize;
      if (stream_.deviceBuffer) free(stream_.deviceBuffer);
      stream_.deviceBuffer = (char *) calloc(buffer_bytes, 1);
      if (stream_.deviceBuffer == NULL) {
        sprintf(message_, "RtApiCore: error allocating device buffer memory (%s).",
                devices_[device].name.c_str());
        goto error;
      }

      // If not de-interleaving, we point stream_.deviceBuffer to the
      // OS X supplied device buffer before doing any necessary data
      // conversions.  This presents a problem if we have a duplex
      // stream using one device which needs de-interleaving and
      // another device which doesn't.  So, save a pointer to our own
      // device buffer in the CallbackInfo structure.
      handle->deviceBuffer = stream_.deviceBuffer;
    }
  }

  stream_.sampleRate = sampleRate;
  stream_.device[mode] = device;
  stream_.state = STREAM_STOPPED;
  stream_.callbackInfo.object = (void *) this;

  if ( stream_.mode == OUTPUT && mode == INPUT && stream_.device[0] == device )
    // Only one callback procedure per device.
    stream_.mode = DUPLEX;
  else {
    err = AudioDeviceAddIOProc( id, callbackHandler, (void *) &stream_.callbackInfo );
    if (err != noErr) {
      sprintf( message_, "RtApiCore: OS-X error setting callback for device (%s).", devices_[device].name.c_str() );
      error(RtError::DEBUG_WARNING);
      return FAILURE;
    }
    if ( stream_.mode == OUTPUT && mode == INPUT )
      stream_.mode = DUPLEX;
    else
      stream_.mode = mode;
  }

  // Setup the device property listener for over/underload.
  err = AudioDeviceAddPropertyListener( id, iChannel, isInput,
                                        kAudioDeviceProcessorOverload,
                                        deviceListener, (void *) handle );

  return SUCCESS;

 error:
  if ( handle ) {
    pthread_cond_destroy(&handle->condition);
    free(handle);
    stream_.apiHandle = 0;
  }

  if (stream_.userBuffer) {
    free(stream_.userBuffer);
    stream_.userBuffer = 0;
  }

  error(RtError::WARNING);
  return FAILURE;
}

void RtApiCore :: closeStream()
{
  // We don't want an exception to be thrown here because this
  // function is called by our class destructor.  So, do our own
  // stream check.
  if ( stream_.mode == UNINITIALIZED ) {
    sprintf(message_, "RtApiCore::closeStream(): no open stream to close!");
    error(RtError::WARNING);
    return;
  }

  AudioDeviceID id = *( (AudioDeviceID *) devices_[stream_.device[0]].apiDeviceId );
  if (stream_.mode == OUTPUT || stream_.mode == DUPLEX) {
    if (stream_.state == STREAM_RUNNING)
      AudioDeviceStop( id, callbackHandler );
    AudioDeviceRemoveIOProc( id, callbackHandler );
  }

  id = *( (AudioDeviceID *) devices_[stream_.device[1]].apiDeviceId );
  if (stream_.mode == INPUT || ( stream_.mode == DUPLEX && stream_.device[0] != stream_.device[1]) ) {
    if (stream_.state == STREAM_RUNNING)
      AudioDeviceStop( id, callbackHandler );
    AudioDeviceRemoveIOProc( id, callbackHandler );
  }

  if (stream_.userBuffer) {
    free(stream_.userBuffer);
    stream_.userBuffer = 0;
  }

  if ( stream_.deInterleave[0] || stream_.deInterleave[1] ) {
    free(stream_.deviceBuffer);
    stream_.deviceBuffer = 0;
  }

  CoreHandle *handle = (CoreHandle *) stream_.apiHandle;

  // Destroy pthread condition variable and free the CoreHandle structure.
  if ( handle ) {
    pthread_cond_destroy(&handle->condition);
    free( handle );
    stream_.apiHandle = 0;
  }

  stream_.mode = UNINITIALIZED;
}

void RtApiCore :: startStream()
{
  verifyStream();
  if (stream_.state == STREAM_RUNNING) return;

  MUTEX_LOCK(&stream_.mutex);

  OSStatus err;
  AudioDeviceID id;
  if (stream_.mode == OUTPUT || stream_.mode == DUPLEX) {

    id = *( (AudioDeviceID *) devices_[stream_.device[0]].apiDeviceId );
    err = AudioDeviceStart(id, callbackHandler);
    if (err != noErr) {
      sprintf(message_, "RtApiCore: OS-X error starting callback procedure on device (%s).",
              devices_[stream_.device[0]].name.c_str());
      MUTEX_UNLOCK(&stream_.mutex);
      error(RtError::DRIVER_ERROR);
    }
  }

  if (stream_.mode == INPUT || ( stream_.mode == DUPLEX && stream_.device[0] != stream_.device[1]) ) {

    id = *( (AudioDeviceID *) devices_[stream_.device[1]].apiDeviceId );
    err = AudioDeviceStart(id, callbackHandler);
    if (err != noErr) {
      sprintf(message_, "RtApiCore: OS-X error starting input callback procedure on device (%s).",
              devices_[stream_.device[0]].name.c_str());
      MUTEX_UNLOCK(&stream_.mutex);
      error(RtError::DRIVER_ERROR);
    }
  }

  CoreHandle *handle = (CoreHandle *) stream_.apiHandle;
  handle->stopStream = false;
  stream_.state = STREAM_RUNNING;

  MUTEX_UNLOCK(&stream_.mutex);
}

void RtApiCore :: stopStream()
{
  verifyStream();
  if (stream_.state == STREAM_STOPPED) return;

  // Change the state before the lock to improve shutdown response
  // when using a callback.
  stream_.state = STREAM_STOPPED;
  MUTEX_LOCK(&stream_.mutex);

  OSStatus err;
  AudioDeviceID id;
  if (stream_.mode == OUTPUT || stream_.mode == DUPLEX) {

    id = *( (AudioDeviceID *) devices_[stream_.device[0]].apiDeviceId );
    err = AudioDeviceStop(id, callbackHandler);
    if (err != noErr) {
      sprintf(message_, "RtApiCore: OS-X error stopping callback procedure on device (%s).",
              devices_[stream_.device[0]].name.c_str());
      MUTEX_UNLOCK(&stream_.mutex);
      error(RtError::DRIVER_ERROR);
    }
  }

  if (stream_.mode == INPUT || ( stream_.mode == DUPLEX && stream_.device[0] != stream_.device[1]) ) {

    id = *( (AudioDeviceID *) devices_[stream_.device[1]].apiDeviceId );
    err = AudioDeviceStop(id, callbackHandler);
    if (err != noErr) {
      sprintf(message_, "RtApiCore: OS-X error stopping input callback procedure on device (%s).",
              devices_[stream_.device[0]].name.c_str());
      MUTEX_UNLOCK(&stream_.mutex);
      error(RtError::DRIVER_ERROR);
    }
  }

  MUTEX_UNLOCK(&stream_.mutex);
}

void RtApiCore :: abortStream()
{
  stopStream();
}

void RtApiCore :: tickStream()
{
  verifyStream();

  if (stream_.state == STREAM_STOPPED) return;

  if (stream_.callbackInfo.usingCallback) {
    sprintf(message_, "RtApiCore: tickStream() should not be used when a callback function is set!");
    error(RtError::WARNING);
    return;
  }

  CoreHandle *handle = (CoreHandle *) stream_.apiHandle;

  MUTEX_LOCK(&stream_.mutex);

  pthread_cond_wait(&handle->condition, &stream_.mutex);

  MUTEX_UNLOCK(&stream_.mutex);
}

void RtApiCore :: callbackEvent( AudioDeviceID deviceId, void *inData, void *outData )
{
  verifyStream();

  if (stream_.state == STREAM_STOPPED) return;

  CallbackInfo *info = (CallbackInfo *) &stream_.callbackInfo;
  CoreHandle *handle = (CoreHandle *) stream_.apiHandle;
  AudioBufferList *inBufferList = (AudioBufferList *) inData;
  AudioBufferList *outBufferList = (AudioBufferList *) outData;

  if ( info->usingCallback && handle->stopStream ) {
    // Check if the stream should be stopped (via the previous user
    // callback return value).  We stop the stream here, rather than
    // after the function call, so that output data can first be
    // processed.
    this->stopStream();
    return;
  }

  MUTEX_LOCK(&stream_.mutex);

  // Invoke user callback first, to get fresh output data.  Don't
  // invoke the user callback if duplex mode AND the input/output devices
  // are different AND this function is called for the input device.
  AudioDeviceID id = *( (AudioDeviceID *) devices_[stream_.device[0]].apiDeviceId );
  if ( info->usingCallback && (stream_.mode != DUPLEX || deviceId == id ) ) {
    RtAudioCallback callback = (RtAudioCallback) info->callback;
    handle->stopStream = callback(stream_.userBuffer, stream_.bufferSize, info->userData);
    if ( handle->xrun == true ) {
      handle->xrun = false;
      MUTEX_UNLOCK(&stream_.mutex);
      return;
    }
  }

  if ( stream_.mode == OUTPUT || ( stream_.mode == DUPLEX && deviceId == id ) ) {

    if (stream_.doConvertBuffer[0]) {

      if ( !stream_.deInterleave[0] )
        stream_.deviceBuffer = (char *) outBufferList->mBuffers[handle->index[0]].mData;
      else
        stream_.deviceBuffer = handle->deviceBuffer;

      convertStreamBuffer(OUTPUT);
      if ( stream_.doByteSwap[0] )
        byteSwapBuffer(stream_.deviceBuffer,
                       stream_.bufferSize * stream_.nDeviceChannels[0],
                       stream_.deviceFormat[0]);

      if ( stream_.deInterleave[0] ) {
        int bufferBytes = outBufferList->mBuffers[handle->index[0]].mDataByteSize;
        for ( int i=0; i<stream_.nDeviceChannels[0]; i++ ) {
          memcpy(outBufferList->mBuffers[handle->index[0]+i].mData,
                 &stream_.deviceBuffer[i*bufferBytes], bufferBytes );
        }
      }

    }
    else {
      if (stream_.doByteSwap[0])
        byteSwapBuffer(stream_.userBuffer,
                       stream_.bufferSize * stream_.nUserChannels[0],
                       stream_.userFormat);

      memcpy(outBufferList->mBuffers[handle->index[0]].mData,
             stream_.userBuffer,
             outBufferList->mBuffers[handle->index[0]].mDataByteSize );
    }
  }

  if ( stream_.mode == INPUT || ( stream_.mode == DUPLEX && deviceId == id ) ) {

    if (stream_.doConvertBuffer[1]) {

      if ( stream_.deInterleave[1] ) {
        stream_.deviceBuffer = (char *) handle->deviceBuffer;
        int bufferBytes = inBufferList->mBuffers[handle->index[1]].mDataByteSize;
        for ( int i=0; i<stream_.nDeviceChannels[1]; i++ ) {
          memcpy(&stream_.deviceBuffer[i*bufferBytes],
                 inBufferList->mBuffers[handle->index[1]+i].mData, bufferBytes );
        }
      }
      else
        stream_.deviceBuffer = (char *) inBufferList->mBuffers[handle->index[1]].mData;

      if ( stream_.doByteSwap[1] )
        byteSwapBuffer(stream_.deviceBuffer,
                       stream_.bufferSize * stream_.nDeviceChannels[1],
                       stream_.deviceFormat[1]);
      convertStreamBuffer(INPUT);

    }
    else {
      memcpy(stream_.userBuffer,
             inBufferList->mBuffers[handle->index[1]].mData,
             inBufferList->mBuffers[handle->index[1]].mDataByteSize );

      if (stream_.doByteSwap[1])
        byteSwapBuffer(stream_.userBuffer,
                       stream_.bufferSize * stream_.nUserChannels[1],
                       stream_.userFormat);
    }
  }

  if ( !info->usingCallback && (stream_.mode != DUPLEX || deviceId == id ) )
    pthread_cond_signal(&handle->condition);

  MUTEX_UNLOCK(&stream_.mutex);

  RtApi::tickStream();
}

void RtApiCore :: setStreamCallback(RtAudioCallback callback, void *userData)
{
  verifyStream();

  if ( stream_.callbackInfo.usingCallback ) {
    sprintf(message_, "RtApiCore: A callback is already set for this stream!");
    error(RtError::WARNING);
    return;
  }

  stream_.callbackInfo.callback = (void *) callback;
  stream_.callbackInfo.userData = userData;
  stream_.callbackInfo.usingCallback = true;
}

void RtApiCore :: cancelStreamCallback()
{
  verifyStream();

  if (stream_.callbackInfo.usingCallback) {

    if (stream_.state == STREAM_RUNNING)
      stopStream();

    MUTEX_LOCK(&stream_.mutex);

    stream_.callbackInfo.usingCallback = false;
    stream_.callbackInfo.userData = NULL;
    stream_.state = STREAM_STOPPED;
    stream_.callbackInfo.callback = NULL;

    MUTEX_UNLOCK(&stream_.mutex);
  }
}


//******************** End of __MACOSX_CORE__ *********************//
#endif

#if defined(__LINUX_JACK__)

// JACK is a low-latency audio server, written primarily for the
// GNU/Linux operating system. It can connect a number of different
// applications to an audio device, as well as allowing them to share
// audio between themselves.
//
// The JACK server must be running before RtApiJack can be instantiated.
// RtAudio will report just a single "device", which is the JACK audio
// server.  The JACK server is typically started in a terminal as follows:
//
// .jackd -d alsa -d hw:0
//
// Many of the parameters normally set for a stream are fixed by the
// JACK server and can be specified when the JACK server is started.
// In particular,
//
// .jackd -d alsa -d hw:0 -r 44100 -p 512 -n 4
//
// specifies a sample rate of 44100 Hz, a buffer size of 512 sample
// frames, and number of buffers = 4.  Once the server is running, it
// is not possible to override these values.  If the values are not
// specified in the command-line, the JACK server uses default values.

#include <jack/jack.h>
#include <unistd.h>

// A structure to hold various information related to the Jack API
// implementation.
struct JackHandle {
  jack_client_t *client;
  jack_port_t **ports[2];
  bool clientOpen;
  bool stopStream;
  pthread_cond_t condition;

  JackHandle()
    :client(0), clientOpen(false), stopStream(false) {}
};

std::string jackmsg;

static void jackerror (const char *desc)
{
  jackmsg.erase();
  jackmsg.append( desc, strlen(desc)+1 );
}

RtApiJack :: RtApiJack()
{
  this->initialize();

  if (nDevices_ <= 0) {
    sprintf(message_, "RtApiJack: no Linux Jack server found or connection error (jack: %s)!",
            jackmsg.c_str());
    error(RtError::NO_DEVICES_FOUND);
  }
}

RtApiJack :: ~RtApiJack()
{
  if ( stream_.mode != UNINITIALIZED ) closeStream();
}

void RtApiJack :: initialize(void)
{
  nDevices_ = 0;

  // Tell the jack server to call jackerror() when it experiences an
  // error.  This function saves the error message for subsequent
  // reporting via the normal RtAudio error function.
	jack_set_error_function( jackerror );

  // Look for jack server and try to become a client.
  jack_client_t *client;
  if ( (client = jack_client_new( "RtApiJack" )) == 0)
    return;

  RtApiDevice device;
  // Determine the name of the device.
  device.name = "Jack Server";
  devices_.push_back(device);
  nDevices_++;

  jack_client_close(client);
}

void RtApiJack :: probeDeviceInfo(RtApiDevice *info)
{
  // Look for jack server and try to become a client.
  jack_client_t *client;
  if ( (client = jack_client_new( "RtApiJack" )) == 0) {
    sprintf(message_, "RtApiJack: error connecting to Linux Jack server in probeDeviceInfo() (jack: %s)!",
            jackmsg.c_str());
    error(RtError::WARNING);
    return;
  }

  // Get the current jack server sample rate.
  info->sampleRates.clear();
  info->sampleRates.push_back( jack_get_sample_rate(client) );

  // Count the available ports as device channels.  Jack "input ports"
  // equal RtAudio output channels.
  const char **ports;
  char *port;
  unsigned int nChannels = 0;
  ports = jack_get_ports( client, NULL, NULL, JackPortIsInput );
  if ( ports ) {
    port = (char *) ports[nChannels];
    while ( port )
      port = (char *) ports[++nChannels];
    free( ports );
    info->maxOutputChannels = nChannels;
    info->minOutputChannels = 1;
  }

  // Jack "output ports" equal RtAudio input channels.
  nChannels = 0;
  ports = jack_get_ports( client, NULL, NULL, JackPortIsOutput );
  if ( ports ) {
    port = (char *) ports[nChannels];
    while ( port )
      port = (char *) ports[++nChannels];
    free( ports );
    info->maxInputChannels = nChannels;
    info->minInputChannels = 1;
  }

  if (info->maxOutputChannels == 0 && info->maxInputChannels == 0) {
    jack_client_close(client);
    sprintf(message_, "RtApiJack: error determining jack input/output channels!");
    error(RtError::WARNING);
    return;
  }

  if (info->maxOutputChannels > 0 && info->maxInputChannels > 0) {
    info->hasDuplexSupport = true;
    info->maxDuplexChannels = (info->maxOutputChannels > info->maxInputChannels) ?
      info->maxInputChannels : info->maxOutputChannels;
    info->minDuplexChannels = (info->minOutputChannels > info->minInputChannels) ?
      info->minInputChannels : info->minOutputChannels;
  }

  // Get the jack data format type.  There isn't much documentation
  // regarding supported data formats in jack.  I'm assuming here that
  // the default type will always be a floating-point type, of length
  // equal to either 4 or 8 bytes.
  int sample_size = sizeof( jack_default_audio_sample_t );
  if ( sample_size == 4 )
    info->nativeFormats = RTAUDIO_FLOAT32;
  else if ( sample_size == 8 )
    info->nativeFormats = RTAUDIO_FLOAT64;

  // Check that we have a supported format
  if (info->nativeFormats == 0) {
    jack_client_close(client);
    sprintf(message_, "RtApiJack: error determining jack server data format!");
    error(RtError::WARNING);
    return;
  }

  jack_client_close(client);
  info->probed = true;
}

int jackCallbackHandler(jack_nframes_t nframes, void *infoPointer)
{
  CallbackInfo *info = (CallbackInfo *) infoPointer;
  RtApiJack *object = (RtApiJack *) info->object;
  try {
    object->callbackEvent( (unsigned long) nframes );
  }
  catch (RtError &exception) {
    fprintf(stderr, "\nRtApiJack: callback handler error (%s)!\n\n", exception.getMessageString());
    return 0;
  }

  return 0;
}

void jackShutdown(void *infoPointer)
{
  CallbackInfo *info = (CallbackInfo *) infoPointer;
  JackHandle *handle = (JackHandle *) info->apiInfo;
  handle->clientOpen = false;
  RtApiJack *object = (RtApiJack *) info->object;
  try {
    object->closeStream();
  }
  catch (RtError &exception) {
    fprintf(stderr, "\nRtApiJack: jackShutdown error (%s)!\n\n", exception.getMessageString());
    return;
  }

  fprintf(stderr, "\nRtApiJack: the Jack server is shutting down ... stream stopped and closed!!!\n\n");
}

int jackXrun( void * )
{
  fprintf(stderr, "\nRtApiJack: audio overrun/underrun reported!\n");
  return 0;
}

bool RtApiJack :: probeDeviceOpen(int device, StreamMode mode, int channels, 
                                int sampleRate, RtAudioFormat format,
                                int *bufferSize, int numberOfBuffers)
{
  // Compare the jack server channels to the requested number of channels.
  if ( (mode == OUTPUT && devices_[device].maxOutputChannels < channels ) || 
       (mode == INPUT && devices_[device].maxInputChannels < channels ) ) {
    sprintf(message_, "RtApiJack: the Jack server does not support requested channels!");
    error(RtError::DEBUG_WARNING);
    return FAILURE;
  }

  JackHandle *handle = (JackHandle *) stream_.apiHandle;

  // Look for jack server and try to become a client (only do once per stream).
  char label[32];
  jack_client_t *client = 0;
  if ( mode == OUTPUT || (mode == INPUT && stream_.mode != OUTPUT) ) {
    snprintf(label, 32, "RtApiJack");
    if ( (client = jack_client_new( (const char *) label )) == 0) {
      sprintf(message_, "RtApiJack: cannot connect to Linux Jack server in probeDeviceOpen() (jack: %s)!",
              jackmsg.c_str());
      error(RtError::DEBUG_WARNING);
      return FAILURE;
    }
  }
  else {
    // The handle must have been created on an earlier pass.
    client = handle->client;
  }

  // First, check the jack server sample rate.
  int jack_rate;
  jack_rate = (int) jack_get_sample_rate(client);
  if ( sampleRate != jack_rate ) {
    jack_client_close(client);
    sprintf( message_, "RtApiJack: the requested sample rate (%d) is different than the JACK server rate (%d).",
             sampleRate, jack_rate );
    error(RtError::DEBUG_WARNING);
    return FAILURE;
  }
  stream_.sampleRate = jack_rate;

  // The jack server seems to support just a single floating-point
  // data type.  Since we already checked it before, just use what we
  // found then.
  stream_.deviceFormat[mode] = devices_[device].nativeFormats;
  stream_.userFormat = format;

  // Jack always uses non-interleaved buffers.  We'll need to
  // de-interleave if we have more than one channel.
  stream_.deInterleave[mode] = false;
  if ( channels > 1 )
    stream_.deInterleave[mode] = true;

  // Jack always provides host byte-ordered data.
  stream_.doByteSwap[mode] = false;

  // Get the buffer size.  The buffer size and number of buffers
  // (periods) is set when the jack server is started.
  stream_.bufferSize = (int) jack_get_buffer_size(client);
  *bufferSize = stream_.bufferSize;

  stream_.nDeviceChannels[mode] = channels;
  stream_.nUserChannels[mode] = channels;

  stream_.doConvertBuffer[mode] = false;
  if (stream_.userFormat != stream_.deviceFormat[mode])
    stream_.doConvertBuffer[mode] = true;
  if (stream_.deInterleave[mode])
    stream_.doConvertBuffer[mode] = true;

  // Allocate our JackHandle structure for the stream.
  if ( handle == 0 ) {
    handle = (JackHandle *) calloc(1, sizeof(JackHandle));
    if ( handle == NULL ) {
      sprintf(message_, "RtApiJack: error allocating JackHandle memory (%s).",
              devices_[device].name.c_str());
      goto error;
    }
    handle->ports[0] = 0;
    handle->ports[1] = 0;
    if ( pthread_cond_init(&handle->condition, NULL) ) {
      sprintf(message_, "RtApiJack: error initializing pthread condition variable!");
      goto error;
    }
    stream_.apiHandle = (void *) handle;
    handle->client = client;
    handle->clientOpen = true;
  }

  // Allocate necessary internal buffers.
  if ( stream_.nUserChannels[0] != stream_.nUserChannels[1] ) {

    long buffer_bytes;
    if (stream_.nUserChannels[0] >= stream_.nUserChannels[1])
      buffer_bytes = stream_.nUserChannels[0];
    else
      buffer_bytes = stream_.nUserChannels[1];

    buffer_bytes *= *bufferSize * formatBytes(stream_.userFormat);
    if (stream_.userBuffer) free(stream_.userBuffer);
    stream_.userBuffer = (char *) calloc(buffer_bytes, 1);
    if (stream_.userBuffer == NULL) {
      sprintf(message_, "RtApiJack: error allocating user buffer memory (%s).",
              devices_[device].name.c_str());
      goto error;
    }
  }

  if ( stream_.doConvertBuffer[mode] ) {

    long buffer_bytes;
    bool makeBuffer = true;
    if ( mode == OUTPUT )
      buffer_bytes = stream_.nDeviceChannels[0] * formatBytes(stream_.deviceFormat[0]);
    else { // mode == INPUT
      buffer_bytes = stream_.nDeviceChannels[1] * formatBytes(stream_.deviceFormat[1]);
      if ( stream_.mode == OUTPUT && stream_.deviceBuffer ) {
        long bytes_out = stream_.nDeviceChannels[0] * formatBytes(stream_.deviceFormat[0]);
        if ( buffer_bytes < bytes_out ) makeBuffer = false;
      }
    }

    if ( makeBuffer ) {
      buffer_bytes *= *bufferSize;
      if (stream_.deviceBuffer) free(stream_.deviceBuffer);
      stream_.deviceBuffer = (char *) calloc(buffer_bytes, 1);
      if (stream_.deviceBuffer == NULL) {
        sprintf(message_, "RtApiJack: error allocating device buffer memory (%s).",
                devices_[device].name.c_str());
        goto error;
      }
    }
  }

  // Allocate memory for the Jack ports (channels) identifiers.
  handle->ports[mode] = (jack_port_t **) malloc (sizeof (jack_port_t *) * channels);
  if ( handle->ports[mode] == NULL )  {
    sprintf(message_, "RtApiJack: error allocating port handle memory (%s).",
            devices_[device].name.c_str());
    goto error;
  }

  stream_.device[mode] = device;
  stream_.state = STREAM_STOPPED;
  stream_.callbackInfo.usingCallback = false;
  stream_.callbackInfo.object = (void *) this;
  stream_.callbackInfo.apiInfo = (void *) handle;

  if ( stream_.mode == OUTPUT && mode == INPUT )
    // We had already set up the stream for output.
    stream_.mode = DUPLEX;
  else {
    stream_.mode = mode;
    jack_set_process_callback( handle->client, jackCallbackHandler, (void *) &stream_.callbackInfo );
    jack_set_xrun_callback( handle->client, jackXrun, NULL );
    jack_on_shutdown( handle->client, jackShutdown, (void *) &stream_.callbackInfo );
  }

  return SUCCESS;

 error:
  if ( handle ) {
    pthread_cond_destroy(&handle->condition);
    if ( handle->clientOpen == true )
      jack_client_close(handle->client);

    if ( handle->ports[0] ) free(handle->ports[0]);
    if ( handle->ports[1] ) free(handle->ports[1]);

    free( handle );
    stream_.apiHandle = 0;
  }

  if (stream_.userBuffer) {
    free(stream_.userBuffer);
    stream_.userBuffer = 0;
  }

  error(RtError::WARNING);
  return FAILURE;
}

void RtApiJack :: closeStream()
{
  // We don't want an exception to be thrown here because this
  // function is called by our class destructor.  So, do our own
  // stream check.
  if ( stream_.mode == UNINITIALIZED ) {
    sprintf(message_, "RtApiJack::closeStream(): no open stream to close!");
    error(RtError::WARNING);
    return;
  }

  JackHandle *handle = (JackHandle *) stream_.apiHandle;
  if ( handle && handle->clientOpen == true ) {
    if (stream_.state == STREAM_RUNNING)
      jack_deactivate(handle->client);

    jack_client_close(handle->client);
  }

  if ( handle ) {
    if ( handle->ports[0] ) free(handle->ports[0]);
    if ( handle->ports[1] ) free(handle->ports[1]);
    pthread_cond_destroy(&handle->condition);
    free( handle );
    stream_.apiHandle = 0;
  }

  if (stream_.userBuffer) {
    free(stream_.userBuffer);
    stream_.userBuffer = 0;
  }

  if (stream_.deviceBuffer) {
    free(stream_.deviceBuffer);
    stream_.deviceBuffer = 0;
  }

  stream_.mode = UNINITIALIZED;
}


void RtApiJack :: startStream()
{
  verifyStream();
  if (stream_.state == STREAM_RUNNING) return;

  MUTEX_LOCK(&stream_.mutex);

  char label[64];
  JackHandle *handle = (JackHandle *) stream_.apiHandle;
  if ( stream_.mode == OUTPUT || stream_.mode == DUPLEX ) {
    for ( int i=0; i<stream_.nUserChannels[0]; i++ ) {
      snprintf(label, 64, "outport %d", i);
      handle->ports[0][i] = jack_port_register(handle->client, (const char *)label,
                                               JACK_DEFAULT_AUDIO_TYPE, JackPortIsOutput, 0);
    }
  }

  if ( stream_.mode == INPUT || stream_.mode == DUPLEX ) {
    for ( int i=0; i<stream_.nUserChannels[1]; i++ ) {
      snprintf(label, 64, "inport %d", i);
      handle->ports[1][i] = jack_port_register(handle->client, (const char *)label,
                                               JACK_DEFAULT_AUDIO_TYPE, JackPortIsInput, 0);
    }
  }

  if (jack_activate(handle->client)) {
    sprintf(message_, "RtApiJack: unable to activate JACK client!");
    error(RtError::SYSTEM_ERROR);
  }

  const char **ports;
  int result;
  // Get the list of available ports.
  if ( stream_.mode == OUTPUT || stream_.mode == DUPLEX ) {
    ports = jack_get_ports(handle->client, NULL, NULL, JackPortIsPhysical|JackPortIsInput);
    if ( ports == NULL) {
      sprintf(message_, "RtApiJack: error determining available jack input ports!");
      error(RtError::SYSTEM_ERROR);
    }

    // Now make the port connections.  Since RtAudio wasn't designed to
    // allow the user to select particular channels of a device, we'll
    // just open the first "nChannels" ports.
    for ( int i=0; i<stream_.nUserChannels[0]; i++ ) {
      result = 1;
      if ( ports[i] )
        result = jack_connect( handle->client, jack_port_name(handle->ports[0][i]), ports[i] );
      if ( result ) {
        free(ports);
        sprintf(message_, "RtApiJack: error connecting output ports!");
        error(RtError::SYSTEM_ERROR);
      }
    }
    free(ports);
  }

  if ( stream_.mode == INPUT || stream_.mode == DUPLEX ) {
    ports = jack_get_ports( handle->client, NULL, NULL, JackPortIsPhysical|JackPortIsOutput );
    if ( ports == NULL) {
      sprintf(message_, "RtApiJack: error determining available jack output ports!");
      error(RtError::SYSTEM_ERROR);
    }

    // Now make the port connections.  See note above.
    for ( int i=0; i<stream_.nUserChannels[1]; i++ ) {
      result = 1;
      if ( ports[i] )
        result = jack_connect( handle->client, ports[i], jack_port_name(handle->ports[1][i]) );
      if ( result ) {
        free(ports);
        sprintf(message_, "RtApiJack: error connecting input ports!");
        error(RtError::SYSTEM_ERROR);
      }
    }
    free(ports);
  }

  handle->stopStream = false;
  stream_.state = STREAM_RUNNING;

  MUTEX_UNLOCK(&stream_.mutex);
}

void RtApiJack :: stopStream()
{
  verifyStream();
  if (stream_.state == STREAM_STOPPED) return;

  // Change the state before the lock to improve shutdown response
  // when using a callback.
  stream_.state = STREAM_STOPPED;
  MUTEX_LOCK(&stream_.mutex);

  JackHandle *handle = (JackHandle *) stream_.apiHandle;
  jack_deactivate(handle->client);

  MUTEX_UNLOCK(&stream_.mutex);
}

void RtApiJack :: abortStream()
{
  stopStream();
}

void RtApiJack :: tickStream()
{
  verifyStream();

  if (stream_.state == STREAM_STOPPED) return;

  if (stream_.callbackInfo.usingCallback) {
    sprintf(message_, "RtApiJack: tickStream() should not be used when a callback function is set!");
    error(RtError::WARNING);
    return;
  }

  JackHandle *handle = (JackHandle *) stream_.apiHandle;

  MUTEX_LOCK(&stream_.mutex);

  pthread_cond_wait(&handle->condition, &stream_.mutex);

  MUTEX_UNLOCK(&stream_.mutex);

  RtApi::tickStream();
}

void RtApiJack :: callbackEvent( unsigned long nframes )
{
  verifyStream();

  if (stream_.state == STREAM_STOPPED) return;

  CallbackInfo *info = (CallbackInfo *) &stream_.callbackInfo;
  JackHandle *handle = (JackHandle *) stream_.apiHandle;
  if ( info->usingCallback && handle->stopStream ) {
    // Check if the stream should be stopped (via the previous user
    // callback return value).  We stop the stream here, rather than
    // after the function call, so that output data can first be
    // processed.
    this->stopStream();
    return;
  }

  MUTEX_LOCK(&stream_.mutex);

  // Invoke user callback first, to get fresh output data.
  if ( info->usingCallback ) {
    RtAudioCallback callback = (RtAudioCallback) info->callback;
    handle->stopStream = callback(stream_.userBuffer, stream_.bufferSize, info->userData);
  }

  jack_default_audio_sample_t *jackbuffer;
  long bufferBytes = nframes * sizeof (jack_default_audio_sample_t);
  if ( stream_.mode == OUTPUT || stream_.mode == DUPLEX ) {

    if (stream_.doConvertBuffer[0]) {
      convertStreamBuffer(OUTPUT);

      for ( int i=0; i<stream_.nDeviceChannels[0]; i++ ) {
        jackbuffer = (jack_default_audio_sample_t *) jack_port_get_buffer(handle->ports[0][i],
                                                                          (jack_nframes_t) nframes);
        memcpy(jackbuffer, &stream_.deviceBuffer[i*bufferBytes], bufferBytes );
      }
    }
    else { // single channel only
      jackbuffer = (jack_default_audio_sample_t *) jack_port_get_buffer(handle->ports[0][0],
                                                                        (jack_nframes_t) nframes);
      memcpy(jackbuffer, stream_.userBuffer, bufferBytes );
    }
  }

  if ( stream_.mode == INPUT || stream_.mode == DUPLEX ) {

    if (stream_.doConvertBuffer[1]) {
    for ( int i=0; i<stream_.nDeviceChannels[1]; i++ ) {
      jackbuffer = (jack_default_audio_sample_t *) jack_port_get_buffer(handle->ports[1][i],
                                                                        (jack_nframes_t) nframes);
      memcpy(&stream_.deviceBuffer[i*bufferBytes], jackbuffer, bufferBytes );
    }
    convertStreamBuffer(INPUT);
    }
    else { // single channel only
      jackbuffer = (jack_default_audio_sample_t *) jack_port_get_buffer(handle->ports[1][0],
                                                                        (jack_nframes_t) nframes);
      memcpy(stream_.userBuffer, jackbuffer, bufferBytes );
    }
  }

  if ( !info->usingCallback )
    pthread_cond_signal(&handle->condition);

  MUTEX_UNLOCK(&stream_.mutex);
}

void RtApiJack :: setStreamCallback(RtAudioCallback callback, void *userData)
{
  verifyStream();

  if ( stream_.callbackInfo.usingCallback ) {
    sprintf(message_, "RtApiJack: A callback is already set for this stream!");
    error(RtError::WARNING);
    return;
  }

  stream_.callbackInfo.callback = (void *) callback;
  stream_.callbackInfo.userData = userData;
  stream_.callbackInfo.usingCallback = true;
}

void RtApiJack :: cancelStreamCallback()
{
  verifyStream();

  if (stream_.callbackInfo.usingCallback) {

    if (stream_.state == STREAM_RUNNING)
      stopStream();

    MUTEX_LOCK(&stream_.mutex);

    stream_.callbackInfo.usingCallback = false;
    stream_.callbackInfo.userData = NULL;
    stream_.state = STREAM_STOPPED;
    stream_.callbackInfo.callback = NULL;

    MUTEX_UNLOCK(&stream_.mutex);
  }
}

#endif

#if defined(__LINUX_ALSA__)

#include <alsa/asoundlib.h>
#include <unistd.h>
#include <ctype.h>

extern "C" void *alsaCallbackHandler(void * ptr);

RtApiAlsa :: RtApiAlsa()
{
  this->initialize();

  if (nDevices_ <= 0) {
    sprintf(message_, "RtApiAlsa: no Linux ALSA audio devices found!");
    error(RtError::NO_DEVICES_FOUND);
  }
}

RtApiAlsa :: ~RtApiAlsa()
{
  if ( stream_.mode != UNINITIALIZED )
    closeStream();
}

void RtApiAlsa :: initialize(void)
{
  int card, subdevice, result;
  char name[64];
  const char *cardId;
  snd_ctl_t *handle;
  snd_ctl_card_info_t *info;
  snd_ctl_card_info_alloca(&info);
  RtApiDevice device;

  // Count cards and devices
  nDevices_ = 0;
  card = -1;
  snd_card_next(&card);
  while ( card >= 0 ) {
    sprintf(name, "hw:%d", card);
    result = snd_ctl_open(&handle, name, 0);
    if (result < 0) {
      sprintf(message_, "RtApiAlsa: control open (%i): %s.", card, snd_strerror(result));
      error(RtError::DEBUG_WARNING);
      goto next_card;
		}
    result = snd_ctl_card_info(handle, info);
		if (result < 0) {
      sprintf(message_, "RtApiAlsa: control hardware info (%i): %s.", card, snd_strerror(result));
      error(RtError::DEBUG_WARNING);
      goto next_card;
		}
    cardId = snd_ctl_card_info_get_id(info);
		subdevice = -1;
		while (1) {
      result = snd_ctl_pcm_next_device(handle, &subdevice);
			if (result < 0) {
        sprintf(message_, "RtApiAlsa: control next device (%i): %s.", card, snd_strerror(result));
        error(RtError::DEBUG_WARNING);
        break;
      }
			if (subdevice < 0)
        break;
      sprintf( name, "hw:%d,%d", card, subdevice );
      // If a cardId exists and it contains at least one non-numeric
      // character, use it to identify the device.  This avoids a bug
      // in ALSA such that a numeric string is interpreted as a device
      // number.
      for ( unsigned int i=0; i<strlen(cardId); i++ ) {
        if ( !isdigit( cardId[i] ) ) {
          sprintf( name, "hw:%s,%d", cardId, subdevice );
          break;
        }
      }
      device.name.erase();
      device.name.append( (const char *)name, strlen(name)+1 );
      devices_.push_back(device);
      nDevices_++;
    }
  next_card:
    snd_ctl_close(handle);
    snd_card_next(&card);
  }
}

void RtApiAlsa :: probeDeviceInfo(RtApiDevice *info)
{
  int err;
  int open_mode = SND_PCM_ASYNC;
  snd_pcm_t *handle;
  snd_ctl_t *chandle;
  snd_pcm_stream_t stream;
	snd_pcm_info_t *pcminfo;
	snd_pcm_info_alloca(&pcminfo);
  snd_pcm_hw_params_t *params;
  snd_pcm_hw_params_alloca(&params);
  char name[64];
  char *card;

  // Open the control interface for this card.
  strncpy( name, info->name.c_str(), 64 );
  card = strtok(name, ",");
  err = snd_ctl_open(&chandle, card, SND_CTL_NONBLOCK);
  if (err < 0) {
    sprintf(message_, "RtApiAlsa: control open (%s): %s.", card, snd_strerror(err));
    error(RtError::DEBUG_WARNING);
    return;
  }
  unsigned int dev = (unsigned int) atoi( strtok(NULL, ",") );

  // First try for playback
  stream = SND_PCM_STREAM_PLAYBACK;
  snd_pcm_info_set_device(pcminfo, dev);
  snd_pcm_info_set_subdevice(pcminfo, 0);
  snd_pcm_info_set_stream(pcminfo, stream);

  if ((err = snd_ctl_pcm_info(chandle, pcminfo)) < 0) {
    if (err == -ENOENT) {
      sprintf(message_, "RtApiAlsa: pcm device (%s) doesn't handle output!", info->name.c_str());
      error(RtError::DEBUG_WARNING);
    }
    else {
      sprintf(message_, "RtApiAlsa: snd_ctl_pcm_info error for device (%s) output: %s",
              info->name.c_str(), snd_strerror(err));
      error(RtError::DEBUG_WARNING);
    }
    goto capture_probe;
  }

  err = snd_pcm_open(&handle, info->name.c_str(), stream, open_mode | SND_PCM_NONBLOCK );
  if (err < 0) {
    if ( err == EBUSY )
      sprintf(message_, "RtApiAlsa: pcm playback device (%s) is busy: %s.",
              info->name.c_str(), snd_strerror(err));
    else
      sprintf(message_, "RtApiAlsa: pcm playback open (%s) error: %s.",
              info->name.c_str(), snd_strerror(err));
    error(RtError::DEBUG_WARNING);
    goto capture_probe;
  }

  // We have an open device ... allocate the parameter structure.
  err = snd_pcm_hw_params_any(handle, params);
  if (err < 0) {
    snd_pcm_close(handle);
    sprintf(message_, "RtApiAlsa: hardware probe error (%s): %s.",
            info->name.c_str(), snd_strerror(err));
    error(RtError::WARNING);
    goto capture_probe;
  }

  // Get output channel information.
  unsigned int value;
  err = snd_pcm_hw_params_get_channels_min(params, &value);
  if (err < 0) {
    snd_pcm_close(handle);
    sprintf(message_, "RtApiAlsa: hardware minimum channel probe error (%s): %s.",
            info->name.c_str(), snd_strerror(err));
    error(RtError::WARNING);
    goto capture_probe;
  }
  info->minOutputChannels = value;

  err = snd_pcm_hw_params_get_channels_max(params, &value);
  if (err < 0) {
    snd_pcm_close(handle);
    sprintf(message_, "RtApiAlsa: hardware maximum channel probe error (%s): %s.",
            info->name.c_str(), snd_strerror(err));
    error(RtError::WARNING);
    goto capture_probe;
  }
  info->maxOutputChannels = value;

  snd_pcm_close(handle);

 capture_probe:
  // Now try for capture
  stream = SND_PCM_STREAM_CAPTURE;
  snd_pcm_info_set_stream(pcminfo, stream);

  err = snd_ctl_pcm_info(chandle, pcminfo);
  snd_ctl_close(chandle);
  if ( err < 0 ) {
    if (err == -ENOENT) {
      sprintf(message_, "RtApiAlsa: pcm device (%s) doesn't handle input!", info->name.c_str());
      error(RtError::DEBUG_WARNING);
    }
    else {
      sprintf(message_, "RtApiAlsa: snd_ctl_pcm_info error for device (%s) input: %s",
              info->name.c_str(), snd_strerror(err));
      error(RtError::DEBUG_WARNING);
    }
    if (info->maxOutputChannels == 0)
      // didn't open for playback either ... device invalid
      return;
    goto probe_parameters;
  }

  err = snd_pcm_open(&handle, info->name.c_str(), stream, open_mode | SND_PCM_NONBLOCK);
  if (err < 0) {
    if ( err == EBUSY )
      sprintf(message_, "RtApiAlsa: pcm capture device (%s) is busy: %s.",
              info->name.c_str(), snd_strerror(err));
    else
      sprintf(message_, "RtApiAlsa: pcm capture open (%s) error: %s.",
              info->name.c_str(), snd_strerror(err));
    error(RtError::DEBUG_WARNING);
    if (info->maxOutputChannels == 0)
      // didn't open for playback either ... device invalid
      return;
    goto probe_parameters;
  }

  // We have an open capture device ... allocate the parameter structure.
  err = snd_pcm_hw_params_any(handle, params);
  if (err < 0) {
    snd_pcm_close(handle);
    sprintf(message_, "RtApiAlsa: hardware probe error (%s): %s.",
            info->name.c_str(), snd_strerror(err));
    error(RtError::WARNING);
    if (info->maxOutputChannels > 0)
      goto probe_parameters;
    else
      return;
  }

  // Get input channel information.
  err = snd_pcm_hw_params_get_channels_min(params, &value);
  if (err < 0) {
    snd_pcm_close(handle);
    sprintf(message_, "RtApiAlsa: hardware minimum in channel probe error (%s): %s.",
            info->name.c_str(), snd_strerror(err));
    error(RtError::WARNING);
    if (info->maxOutputChannels > 0)
      goto probe_parameters;
    else
      return;
  }
  info->minInputChannels = value;

  err = snd_pcm_hw_params_get_channels_max(params, &value);
  if (err < 0) {
    snd_pcm_close(handle);
    sprintf(message_, "RtApiAlsa: hardware maximum in channel probe error (%s): %s.",
            info->name.c_str(), snd_strerror(err));
    error(RtError::WARNING);
    if (info->maxOutputChannels > 0)
      goto probe_parameters;
    else
      return;
  }
  info->maxInputChannels = value;

  snd_pcm_close(handle);

  // If device opens for both playback and capture, we determine the channels.
  if (info->maxOutputChannels == 0 || info->maxInputChannels == 0)
    goto probe_parameters;

  info->hasDuplexSupport = true;
  info->maxDuplexChannels = (info->maxOutputChannels > info->maxInputChannels) ?
    info->maxInputChannels : info->maxOutputChannels;
  info->minDuplexChannels = (info->minOutputChannels > info->minInputChannels) ?
    info->minInputChannels : info->minOutputChannels;

 probe_parameters:
  // At this point, we just need to figure out the supported data
  // formats and sample rates.  We'll proceed by opening the device in
  // the direction with the maximum number of channels, or playback if
  // they are equal.  This might limit our sample rate options, but so
  // be it.

  if (info->maxOutputChannels >= info->maxInputChannels)
    stream = SND_PCM_STREAM_PLAYBACK;
  else
    stream = SND_PCM_STREAM_CAPTURE;

  err = snd_pcm_open(&handle, info->name.c_str(), stream, open_mode);
  if (err < 0) {
    sprintf(message_, "RtApiAlsa: pcm (%s) won't reopen during probe: %s.",
            info->name.c_str(), snd_strerror(err));
    error(RtError::WARNING);
    return;
  }

  // We have an open device ... allocate the parameter structure.
  err = snd_pcm_hw_params_any(handle, params);
  if (err < 0) {
    snd_pcm_close(handle);
    sprintf(message_, "RtApiAlsa: hardware reopen probe error (%s): %s.",
            info->name.c_str(), snd_strerror(err));
    error(RtError::WARNING);
    return;
  }

  // Test our discrete set of sample rate values.
  int dir = 0;
  info->sampleRates.clear();
  for (unsigned int i=0; i<MAX_SAMPLE_RATES; i++) {
    if (snd_pcm_hw_params_test_rate(handle, params, SAMPLE_RATES[i], dir) == 0)
      info->sampleRates.push_back(SAMPLE_RATES[i]);
  }
  if (info->sampleRates.size() == 0) {
    snd_pcm_close(handle);
    sprintf(message_, "RtApiAlsa: no supported sample rates found for device (%s).",
            info->name.c_str());
    error(RtError::DEBUG_WARNING);
    return;
  }

  // Probe the supported data formats ... we don't care about endian-ness just yet
  snd_pcm_format_t format;
  info->nativeFormats = 0;
  format = SND_PCM_FORMAT_S8;
  if (snd_pcm_hw_params_test_format(handle, params, format) == 0)
    info->nativeFormats |= RTAUDIO_SINT8;
  format = SND_PCM_FORMAT_S16;
  if (snd_pcm_hw_params_test_format(handle, params, format) == 0)
    info->nativeFormats |= RTAUDIO_SINT16;
  format = SND_PCM_FORMAT_S24;
  if (snd_pcm_hw_params_test_format(handle, params, format) == 0)
    info->nativeFormats |= RTAUDIO_SINT24;
  format = SND_PCM_FORMAT_S32;
  if (snd_pcm_hw_params_test_format(handle, params, format) == 0)
    info->nativeFormats |= RTAUDIO_SINT32;
  format = SND_PCM_FORMAT_FLOAT;
  if (snd_pcm_hw_params_test_format(handle, params, format) == 0)
    info->nativeFormats |= RTAUDIO_FLOAT32;
  format = SND_PCM_FORMAT_FLOAT64;
  if (snd_pcm_hw_params_test_format(handle, params, format) == 0)
    info->nativeFormats |= RTAUDIO_FLOAT64;

  // Check that we have at least one supported format
  if (info->nativeFormats == 0) {
    snd_pcm_close(handle);
    sprintf(message_, "RtApiAlsa: pcm device (%s) data format not supported by RtAudio.",
            info->name.c_str());
    error(RtError::WARNING);
    return;
  }

  // That's all ... close the device and return
  snd_pcm_close(handle);
  info->probed = true;
  return;
}

bool RtApiAlsa :: probeDeviceOpen( int device, StreamMode mode, int channels, 
                                   int sampleRate, RtAudioFormat format,
                                   int *bufferSize, int numberOfBuffers )
{
#if defined(__RTAUDIO_DEBUG__)
  snd_output_t *out;
  snd_output_stdio_attach(&out, stderr, 0);
#endif

  // I'm not using the "plug" interface ... too much inconsistent behavior.
  const char *name = devices_[device].name.c_str();

  snd_pcm_stream_t alsa_stream;
  if (mode == OUTPUT)
    alsa_stream = SND_PCM_STREAM_PLAYBACK;
  else
    alsa_stream = SND_PCM_STREAM_CAPTURE;

  int err;
  snd_pcm_t *handle;
  int alsa_open_mode = SND_PCM_ASYNC;
  err = snd_pcm_open(&handle, name, alsa_stream, alsa_open_mode);
  if (err < 0) {
    sprintf(message_,"RtApiAlsa: pcm device (%s) won't open: %s.",
            name, snd_strerror(err));
    error(RtError::WARNING);
    return FAILURE;
  }

  // Fill the parameter structure.
  snd_pcm_hw_params_t *hw_params;
  snd_pcm_hw_params_alloca(&hw_params);
  err = snd_pcm_hw_params_any(handle, hw_params);
  if (err < 0) {
    snd_pcm_close(handle);
    sprintf(message_, "RtApiAlsa: error getting parameter handle (%s): %s.",
            name, snd_strerror(err));
    error(RtError::WARNING);
    return FAILURE;
  }

#if defined(__RTAUDIO_DEBUG__)
  fprintf(stderr, "\nRtApiAlsa: dump hardware params just after device open:\n\n");
  snd_pcm_hw_params_dump(hw_params, out);
#endif

  // Set access ... try interleaved access first, then non-interleaved
  if ( !snd_pcm_hw_params_test_access( handle, hw_params, SND_PCM_ACCESS_RW_INTERLEAVED) ) {
    err = snd_pcm_hw_params_set_access(handle, hw_params, SND_PCM_ACCESS_RW_INTERLEAVED);
  }
  else if ( !snd_pcm_hw_params_test_access( handle, hw_params, SND_PCM_ACCESS_RW_NONINTERLEAVED) ) {
		err = snd_pcm_hw_params_set_access(handle, hw_params, SND_PCM_ACCESS_RW_NONINTERLEAVED);
    stream_.deInterleave[mode] = true;
  }
  else {
    snd_pcm_close(handle);
    sprintf(message_, "RtApiAlsa: device (%s) access not supported by RtAudio.", name);
    error(RtError::WARNING);
    return FAILURE;
  }

  if (err < 0) {
    snd_pcm_close(handle);
    sprintf(message_, "RtApiAlsa: error setting access ( (%s): %s.", name, snd_strerror(err));
    error(RtError::WARNING);
    return FAILURE;
  }

  // Determine how to set the device format.
  stream_.userFormat = format;
  snd_pcm_format_t device_format = SND_PCM_FORMAT_UNKNOWN;

  if (format == RTAUDIO_SINT8)
    device_format = SND_PCM_FORMAT_S8;
  else if (format == RTAUDIO_SINT16)
    device_format = SND_PCM_FORMAT_S16;
  else if (format == RTAUDIO_SINT24)
    device_format = SND_PCM_FORMAT_S24;
  else if (format == RTAUDIO_SINT32)
    device_format = SND_PCM_FORMAT_S32;
  else if (format == RTAUDIO_FLOAT32)
    device_format = SND_PCM_FORMAT_FLOAT;
  else if (format == RTAUDIO_FLOAT64)
    device_format = SND_PCM_FORMAT_FLOAT64;

  if (snd_pcm_hw_params_test_format(handle, hw_params, device_format) == 0) {
    stream_.deviceFormat[mode] = format;
    goto set_format;
  }

  // The user requested format is not natively supported by the device.
  device_format = SND_PCM_FORMAT_FLOAT64;
  if (snd_pcm_hw_params_test_format(handle, hw_params, device_format) == 0) {
    stream_.deviceFormat[mode] = RTAUDIO_FLOAT64;
    goto set_format;
  }

  device_format = SND_PCM_FORMAT_FLOAT;
  if (snd_pcm_hw_params_test_format(handle, hw_params, device_format) == 0) {
    stream_.deviceFormat[mode] = RTAUDIO_FLOAT32;
    goto set_format;
  }

  device_format = SND_PCM_FORMAT_S32;
  if (snd_pcm_hw_params_test_format(handle, hw_params, device_format) == 0) {
    stream_.deviceFormat[mode] = RTAUDIO_SINT32;
    goto set_format;
  }

  device_format = SND_PCM_FORMAT_S24;
  if (snd_pcm_hw_params_test_format(handle, hw_params, device_format) == 0) {
    stream_.deviceFormat[mode] = RTAUDIO_SINT24;
    goto set_format;
  }

  device_format = SND_PCM_FORMAT_S16;
  if (snd_pcm_hw_params_test_format(handle, hw_params, device_format) == 0) {
    stream_.deviceFormat[mode] = RTAUDIO_SINT16;
    goto set_format;
  }

  device_format = SND_PCM_FORMAT_S8;
  if (snd_pcm_hw_params_test_format(handle, hw_params, device_format) == 0) {
    stream_.deviceFormat[mode] = RTAUDIO_SINT8;
    goto set_format;
  }

  // If we get here, no supported format was found.
  sprintf(message_,"RtApiAlsa: pcm device (%s) data format not supported by RtAudio.", name);
  snd_pcm_close(handle);
  error(RtError::WARNING);
  return FAILURE;

 set_format:
  err = snd_pcm_hw_params_set_format(handle, hw_params, device_format);
  if (err < 0) {
    snd_pcm_close(handle);
    sprintf(message_, "RtApiAlsa: error setting format (%s): %s.",
            name, snd_strerror(err));
    error(RtError::WARNING);
    return FAILURE;
  }

  // Determine whether byte-swaping is necessary.
  stream_.doByteSwap[mode] = false;
  if (device_format != SND_PCM_FORMAT_S8) {
    err = snd_pcm_format_cpu_endian(device_format);
    if (err == 0)
      stream_.doByteSwap[mode] = true;
    else if (err < 0) {
      snd_pcm_close(handle);
      sprintf(message_, "RtApiAlsa: error getting format endian-ness (%s): %s.",
              name, snd_strerror(err));
      error(RtError::WARNING);
      return FAILURE;
    }
  }

  // Set the sample rate.
  err = snd_pcm_hw_params_set_rate(handle, hw_params, (unsigned int)sampleRate, 0);
  if (err < 0) {
    snd_pcm_close(handle);
    sprintf(message_, "RtApiAlsa: error setting sample rate (%d) on device (%s): %s.",
            sampleRate, name, snd_strerror(err));
    error(RtError::WARNING);
    return FAILURE;
  }

  // Determine the number of channels for this device.  We support a possible
  // minimum device channel number > than the value requested by the user.
  stream_.nUserChannels[mode] = channels;
  unsigned int value;
  err = snd_pcm_hw_params_get_channels_max(hw_params, &value);
  int device_channels = value;
  if (err < 0 || device_channels < channels) {
    snd_pcm_close(handle);
    sprintf(message_, "RtApiAlsa: channels (%d) not supported by device (%s).",
            channels, name);
    error(RtError::WARNING);
    return FAILURE;
  }

  err = snd_pcm_hw_params_get_channels_min(hw_params, &value);
  if (err < 0 ) {
    snd_pcm_close(handle);
    sprintf(message_, "RtApiAlsa: error getting min channels count on device (%s).", name);
    error(RtError::WARNING);
    return FAILURE;
  }
  device_channels = value;
  if (device_channels < channels) device_channels = channels;
  stream_.nDeviceChannels[mode] = device_channels;

  // Set the device channels.
  err = snd_pcm_hw_params_set_channels(handle, hw_params, device_channels);
  if (err < 0) {
    snd_pcm_close(handle);
    sprintf(message_, "RtApiAlsa: error setting channels (%d) on device (%s): %s.",
            device_channels, name, snd_strerror(err));
    error(RtError::WARNING);
    return FAILURE;
  }

  // Set the buffer number, which in ALSA is referred to as the "period".
  int dir;
  unsigned int periods = numberOfBuffers;
  // Even though the hardware might allow 1 buffer, it won't work reliably.
  if (periods < 2) periods = 2;
  err = snd_pcm_hw_params_get_periods_min(hw_params, &value, &dir);
  if (err < 0) {
    snd_pcm_close(handle);
    sprintf(message_, "RtApiAlsa: error getting min periods on device (%s): %s.",
            name, snd_strerror(err));
    error(RtError::WARNING);
    return FAILURE;
  }
  if (value > periods) periods = value;
  err = snd_pcm_hw_params_get_periods_max(hw_params, &value, &dir);
  if (err < 0) {
    snd_pcm_close(handle);
    sprintf(message_, "RtApiAlsa: error getting max periods on device (%s): %s.",
            name, snd_strerror(err));
    error(RtError::WARNING);
    return FAILURE;
  }
  if (value < periods) periods = value;

  err = snd_pcm_hw_params_set_periods(handle, hw_params, periods, 0);
  if (err < 0) {
    snd_pcm_close(handle);
    sprintf(message_, "RtApiAlsa: error setting periods (%s): %s.",
            name, snd_strerror(err));
    error(RtError::WARNING);
    return FAILURE;
  }

  // Set the buffer (or period) size.
  snd_pcm_uframes_t period_size;
  err = snd_pcm_hw_params_get_period_size_min(hw_params, &period_size, &dir);
  if (err < 0) {
    snd_pcm_close(handle);
    sprintf(message_, "RtApiAlsa: error getting period size (%s): %s.",
            name, snd_strerror(err));
    error(RtError::WARNING);
    return FAILURE;
  }
  if (*bufferSize < (int) period_size) *bufferSize = (int) period_size;

  err = snd_pcm_hw_params_set_period_size(handle, hw_params, *bufferSize, 0);
  if (err < 0) {
    snd_pcm_close(handle);
    sprintf(message_, "RtApiAlsa: error setting period size (%s): %s.",
            name, snd_strerror(err));
    error(RtError::WARNING);
    return FAILURE;
  }

  // If attempting to setup a duplex stream, the bufferSize parameter
  // MUST be the same in both directions!
  if ( stream_.mode == OUTPUT && mode == INPUT && *bufferSize != stream_.bufferSize ) {
    sprintf( message_, "RtApiAlsa: error setting buffer size for duplex stream on device (%s).",
             name );
    error(RtError::DEBUG_WARNING);
    return FAILURE;
  }

  stream_.bufferSize = *bufferSize;

  // Install the hardware configuration
  err = snd_pcm_hw_params(handle, hw_params);
  if (err < 0) {
    snd_pcm_close(handle);
    sprintf(message_, "RtApiAlsa: error installing hardware configuration (%s): %s.",
            name, snd_strerror(err));
    error(RtError::WARNING);
    return FAILURE;
  }

#if defined(__RTAUDIO_DEBUG__)
  fprintf(stderr, "\nRtApiAlsa: dump hardware params after installation:\n\n");
  snd_pcm_hw_params_dump(hw_params, out);
#endif

  // Allocate the stream handle if necessary and then save.
  snd_pcm_t **handles;
  if ( stream_.apiHandle == 0 ) {
    handles = (snd_pcm_t **) calloc(2, sizeof(snd_pcm_t *));
    if ( handle == NULL ) {
      sprintf(message_, "RtApiAlsa: error allocating handle memory (%s).",
              devices_[device].name.c_str());
      goto error;
    }
    stream_.apiHandle = (void *) handles;
    handles[0] = 0;
    handles[1] = 0;
  }
  else {
    handles = (snd_pcm_t **) stream_.apiHandle;
  }
  handles[mode] = handle;

  // Set flags for buffer conversion
  stream_.doConvertBuffer[mode] = false;
  if (stream_.userFormat != stream_.deviceFormat[mode])
    stream_.doConvertBuffer[mode] = true;
  if (stream_.nUserChannels[mode] < stream_.nDeviceChannels[mode])
    stream_.doConvertBuffer[mode] = true;
  if (stream_.nUserChannels[mode] > 1 && stream_.deInterleave[mode])
    stream_.doConvertBuffer[mode] = true;

  // Allocate necessary internal buffers
  if ( stream_.nUserChannels[0] != stream_.nUserChannels[1] ) {

    long buffer_bytes;
    if (stream_.nUserChannels[0] >= stream_.nUserChannels[1])
      buffer_bytes = stream_.nUserChannels[0];
    else
      buffer_bytes = stream_.nUserChannels[1];

    buffer_bytes *= *bufferSize * formatBytes(stream_.userFormat);
    if (stream_.userBuffer) free(stream_.userBuffer);
    stream_.userBuffer = (char *) calloc(buffer_bytes, 1);
    if (stream_.userBuffer == NULL) {
      sprintf(message_, "RtApiAlsa: error allocating user buffer memory (%s).",
              devices_[device].name.c_str());
      goto error;
    }
  }

  if ( stream_.doConvertBuffer[mode] ) {

    long buffer_bytes;
    bool makeBuffer = true;
    if ( mode == OUTPUT )
      buffer_bytes = stream_.nDeviceChannels[0] * formatBytes(stream_.deviceFormat[0]);
    else { // mode == INPUT
      buffer_bytes = stream_.nDeviceChannels[1] * formatBytes(stream_.deviceFormat[1]);
      if ( stream_.mode == OUTPUT && stream_.deviceBuffer ) {
        long bytes_out = stream_.nDeviceChannels[0] * formatBytes(stream_.deviceFormat[0]);
        if ( buffer_bytes < bytes_out ) makeBuffer = false;
      }
    }

    if ( makeBuffer ) {
      buffer_bytes *= *bufferSize;
      if (stream_.deviceBuffer) free(stream_.deviceBuffer);
      stream_.deviceBuffer = (char *) calloc(buffer_bytes, 1);
      if (stream_.deviceBuffer == NULL) {
        sprintf(message_, "RtApiAlsa: error allocating device buffer memory (%s).",
                devices_[device].name.c_str());
        goto error;
      }
    }
  }

  stream_.device[mode] = device;
  stream_.state = STREAM_STOPPED;
  if ( stream_.mode == OUTPUT && mode == INPUT )
    // We had already set up an output stream.
    stream_.mode = DUPLEX;
  else
    stream_.mode = mode;
  stream_.nBuffers = periods;
  stream_.sampleRate = sampleRate;

  return SUCCESS;

 error:
  if (handles) {
    if (handles[0])
      snd_pcm_close(handles[0]);
    if (handles[1])
      snd_pcm_close(handles[1]);
    free(handles);
    stream_.apiHandle = 0;
  }

  if (stream_.userBuffer) {
    free(stream_.userBuffer);
    stream_.userBuffer = 0;
  }

  error(RtError::WARNING);
  return FAILURE;
}

void RtApiAlsa :: closeStream()
{
  // We don't want an exception to be thrown here because this
  // function is called by our class destructor.  So, do our own
  // stream check.
  if ( stream_.mode == UNINITIALIZED ) {
    sprintf(message_, "RtApiAlsa::closeStream(): no open stream to close!");
    error(RtError::WARNING);
    return;
  }

  snd_pcm_t **handle = (snd_pcm_t **) stream_.apiHandle;
  if (stream_.state == STREAM_RUNNING) {
    if (stream_.mode == OUTPUT || stream_.mode == DUPLEX)
      snd_pcm_drop(handle[0]);
    if (stream_.mode == INPUT || stream_.mode == DUPLEX)
      snd_pcm_drop(handle[1]);
    stream_.state = STREAM_STOPPED;
  }

  if (stream_.callbackInfo.usingCallback) {
    stream_.callbackInfo.usingCallback = false;
    pthread_join(stream_.callbackInfo.thread, NULL);
  }

  if (handle) {
    if (handle[0]) snd_pcm_close(handle[0]);
    if (handle[1]) snd_pcm_close(handle[1]);
    free(handle);
    handle = 0;
  }

  if (stream_.userBuffer) {
    free(stream_.userBuffer);
    stream_.userBuffer = 0;
  }

  if (stream_.deviceBuffer) {
    free(stream_.deviceBuffer);
    stream_.deviceBuffer = 0;
  }

  stream_.mode = UNINITIALIZED;
}

void RtApiAlsa :: startStream()
{
  // This method calls snd_pcm_prepare if the device isn't already in that state.

  verifyStream();
  if (stream_.state == STREAM_RUNNING) return;

  MUTEX_LOCK(&stream_.mutex);

  int err;
  snd_pcm_state_t state;
  snd_pcm_t **handle = (snd_pcm_t **) stream_.apiHandle;
  if (stream_.mode == OUTPUT || stream_.mode == DUPLEX) {
    state = snd_pcm_state(handle[0]);
    if (state != SND_PCM_STATE_PREPARED) {
      err = snd_pcm_prepare(handle[0]);
      if (err < 0) {
        sprintf(message_, "RtApiAlsa: error preparing pcm device (%s): %s.",
                devices_[stream_.device[0]].name.c_str(), snd_strerror(err));
        MUTEX_UNLOCK(&stream_.mutex);
        error(RtError::DRIVER_ERROR);
      }
    }
  }

  if (stream_.mode == INPUT || stream_.mode == DUPLEX) {
    state = snd_pcm_state(handle[1]);
    if (state != SND_PCM_STATE_PREPARED) {
      err = snd_pcm_prepare(handle[1]);
      if (err < 0) {
        sprintf(message_, "RtApiAlsa: error preparing pcm device (%s): %s.",
                devices_[stream_.device[1]].name.c_str(), snd_strerror(err));
        MUTEX_UNLOCK(&stream_.mutex);
        error(RtError::DRIVER_ERROR);
      }
    }
  }
  stream_.state = STREAM_RUNNING;

  MUTEX_UNLOCK(&stream_.mutex);
}

void RtApiAlsa :: stopStream()
{
  verifyStream();
  if (stream_.state == STREAM_STOPPED) return;

  // Change the state before the lock to improve shutdown response
  // when using a callback.
  stream_.state = STREAM_STOPPED;
  MUTEX_LOCK(&stream_.mutex);

  int err;
  snd_pcm_t **handle = (snd_pcm_t **) stream_.apiHandle;
  if (stream_.mode == OUTPUT || stream_.mode == DUPLEX) {
    err = snd_pcm_drain(handle[0]);
    if (err < 0) {
      sprintf(message_, "RtApiAlsa: error draining pcm device (%s): %s.",
              devices_[stream_.device[0]].name.c_str(), snd_strerror(err));
      MUTEX_UNLOCK(&stream_.mutex);
      error(RtError::DRIVER_ERROR);
    }
  }

  if (stream_.mode == INPUT || stream_.mode == DUPLEX) {
    err = snd_pcm_drain(handle[1]);
    if (err < 0) {
      sprintf(message_, "RtApiAlsa: error draining pcm device (%s): %s.",
              devices_[stream_.device[1]].name.c_str(), snd_strerror(err));
      MUTEX_UNLOCK(&stream_.mutex);
      error(RtError::DRIVER_ERROR);
    }
  }

  MUTEX_UNLOCK(&stream_.mutex);
}

void RtApiAlsa :: abortStream()
{
  verifyStream();
  if (stream_.state == STREAM_STOPPED) return;

  // Change the state before the lock to improve shutdown response
  // when using a callback.
  stream_.state = STREAM_STOPPED;
  MUTEX_LOCK(&stream_.mutex);

  int err;
  snd_pcm_t **handle = (snd_pcm_t **) stream_.apiHandle;
  if (stream_.mode == OUTPUT || stream_.mode == DUPLEX) {
    err = snd_pcm_drop(handle[0]);
    if (err < 0) {
      sprintf(message_, "RtApiAlsa: error draining pcm device (%s): %s.",
              devices_[stream_.device[0]].name.c_str(), snd_strerror(err));
      MUTEX_UNLOCK(&stream_.mutex);
      error(RtError::DRIVER_ERROR);
    }
  }

  if (stream_.mode == INPUT || stream_.mode == DUPLEX) {
    err = snd_pcm_drop(handle[1]);
    if (err < 0) {
      sprintf(message_, "RtApiAlsa: error draining pcm device (%s): %s.",
              devices_[stream_.device[1]].name.c_str(), snd_strerror(err));
      MUTEX_UNLOCK(&stream_.mutex);
      error(RtError::DRIVER_ERROR);
    }
  }

  MUTEX_UNLOCK(&stream_.mutex);
}

int RtApiAlsa :: streamWillBlock()
{
  verifyStream();
  if (stream_.state == STREAM_STOPPED) return 0;

  MUTEX_LOCK(&stream_.mutex);

  int err = 0, frames = 0;
  snd_pcm_t **handle = (snd_pcm_t **) stream_.apiHandle;
  if (stream_.mode == OUTPUT || stream_.mode == DUPLEX) {
    err = snd_pcm_avail_update(handle[0]);
    if (err < 0) {
      sprintf(message_, "RtApiAlsa: error getting available frames for device (%s): %s.",
              devices_[stream_.device[0]].name.c_str(), snd_strerror(err));
      MUTEX_UNLOCK(&stream_.mutex);
      error(RtError::DRIVER_ERROR);
    }
  }

  frames = err;

  if (stream_.mode == INPUT || stream_.mode == DUPLEX) {
    err = snd_pcm_avail_update(handle[1]);
    if (err < 0) {
      sprintf(message_, "RtApiAlsa: error getting available frames for device (%s): %s.",
              devices_[stream_.device[1]].name.c_str(), snd_strerror(err));
      MUTEX_UNLOCK(&stream_.mutex);
      error(RtError::DRIVER_ERROR);
    }
    if (frames > err) frames = err;
  }

  frames = stream_.bufferSize - frames;
  if (frames < 0) frames = 0;

  MUTEX_UNLOCK(&stream_.mutex);
  return frames;
}

void RtApiAlsa :: tickStream()
{
  verifyStream();

  int stopStream = 0;
  if (stream_.state == STREAM_STOPPED) {
    if (stream_.callbackInfo.usingCallback) usleep(50000); // sleep 50 milliseconds
    return;
  }
  else if (stream_.callbackInfo.usingCallback) {
    RtAudioCallback callback = (RtAudioCallback) stream_.callbackInfo.callback;
    stopStream = callback(stream_.userBuffer, stream_.bufferSize, stream_.callbackInfo.userData);
  }

  MUTEX_LOCK(&stream_.mutex);

  // The state might change while waiting on a mutex.
  if (stream_.state == STREAM_STOPPED)
    goto unlock;

  int err;
  char *buffer;
  int channels;
  snd_pcm_t **handle;
  RtAudioFormat format;
  handle = (snd_pcm_t **) stream_.apiHandle;
  if (stream_.mode == OUTPUT || stream_.mode == DUPLEX) {

    // Setup parameters and do buffer conversion if necessary.
    if (stream_.doConvertBuffer[0]) {
      convertStreamBuffer(OUTPUT);
      buffer = stream_.deviceBuffer;
      channels = stream_.nDeviceChannels[0];
      format = stream_.deviceFormat[0];
    }
    else {
      buffer = stream_.userBuffer;
      channels = stream_.nUserChannels[0];
      format = stream_.userFormat;
    }

    // Do byte swapping if necessary.
    if (stream_.doByteSwap[0])
      byteSwapBuffer(buffer, stream_.bufferSize * channels, format);

    // Write samples to device in interleaved/non-interleaved format.
    if (stream_.deInterleave[0]) {
      void *bufs[channels];
      size_t offset = stream_.bufferSize * formatBytes(format);
      for (int i=0; i<channels; i++)
        bufs[i] = (void *) (buffer + (i * offset));
      err = snd_pcm_writen(handle[0], bufs, stream_.bufferSize);
    }
    else
      err = snd_pcm_writei(handle[0], buffer, stream_.bufferSize);

    if (err < stream_.bufferSize) {
      // Either an error or underrun occured.
      if (err == -EPIPE) {
        snd_pcm_state_t state = snd_pcm_state(handle[0]);
        if (state == SND_PCM_STATE_XRUN) {
          sprintf(message_, "RtApiAlsa: underrun detected.");
          error(RtError::WARNING);
          err = snd_pcm_prepare(handle[0]);
          if (err < 0) {
            sprintf(message_, "RtApiAlsa: error preparing handle after underrun: %s.",
                    snd_strerror(err));
            MUTEX_UNLOCK(&stream_.mutex);
            error(RtError::DRIVER_ERROR);
          }
        }
        else {
          sprintf(message_, "RtApiAlsa: tickStream() error, current state is %s.",
                  snd_pcm_state_name(state));
          MUTEX_UNLOCK(&stream_.mutex);
          error(RtError::DRIVER_ERROR);
        }
        goto unlock;
      }
      else {
        sprintf(message_, "RtApiAlsa: audio write error for device (%s): %s.",
                devices_[stream_.device[0]].name.c_str(), snd_strerror(err));
        MUTEX_UNLOCK(&stream_.mutex);
        error(RtError::DRIVER_ERROR);
      }
    }
  }

  if (stream_.mode == INPUT || stream_.mode == DUPLEX) {

    // Setup parameters.
    if (stream_.doConvertBuffer[1]) {
      buffer = stream_.deviceBuffer;
      channels = stream_.nDeviceChannels[1];
      format = stream_.deviceFormat[1];
    }
    else {
      buffer = stream_.userBuffer;
      channels = stream_.nUserChannels[1];
      format = stream_.userFormat;
    }

    // Read samples from device in interleaved/non-interleaved format.
    if (stream_.deInterleave[1]) {
      void *bufs[channels];
      size_t offset = stream_.bufferSize * formatBytes(format);
      for (int i=0; i<channels; i++)
        bufs[i] = (void *) (buffer + (i * offset));
      err = snd_pcm_readn(handle[1], bufs, stream_.bufferSize);
    }
    else
      err = snd_pcm_readi(handle[1], buffer, stream_.bufferSize);

    if (err < stream_.bufferSize) {
      // Either an error or underrun occured.
      if (err == -EPIPE) {
        snd_pcm_state_t state = snd_pcm_state(handle[1]);
        if (state == SND_PCM_STATE_XRUN) {
          sprintf(message_, "RtApiAlsa: overrun detected.");
          error(RtError::WARNING);
          err = snd_pcm_prepare(handle[1]);
          if (err < 0) {
            sprintf(message_, "RtApiAlsa: error preparing handle after overrun: %s.",
                    snd_strerror(err));
            MUTEX_UNLOCK(&stream_.mutex);
            error(RtError::DRIVER_ERROR);
          }
        }
        else {
          sprintf(message_, "RtApiAlsa: tickStream() error, current state is %s.",
                  snd_pcm_state_name(state));
          MUTEX_UNLOCK(&stream_.mutex);
          error(RtError::DRIVER_ERROR);
        }
        goto unlock;
      }
      else {
        sprintf(message_, "RtApiAlsa: audio read error for device (%s): %s.",
                devices_[stream_.device[1]].name.c_str(), snd_strerror(err));
        MUTEX_UNLOCK(&stream_.mutex);
        error(RtError::DRIVER_ERROR);
      }
    }

    // Do byte swapping if necessary.
    if (stream_.doByteSwap[1])
      byteSwapBuffer(buffer, stream_.bufferSize * channels, format);

    // Do buffer conversion if necessary.
    if (stream_.doConvertBuffer[1])
      convertStreamBuffer(INPUT);
  }

 unlock:
  MUTEX_UNLOCK(&stream_.mutex);

  if (stream_.callbackInfo.usingCallback && stopStream)
    this->stopStream();

  RtApi::tickStream();
}

void RtApiAlsa :: setStreamCallback(RtAudioCallback callback, void *userData)
{
  verifyStream();

  CallbackInfo *info = (CallbackInfo *) &stream_.callbackInfo;
  if ( info->usingCallback ) {
    sprintf(message_, "RtApiAlsa: A callback is already set for this stream!");
    error(RtError::WARNING);
    return;
  }

  info->callback = (void *) callback;
  info->userData = userData;
  info->usingCallback = true;
  info->object = (void *) this;

  // Set the thread attributes for joinable and realtime scheduling
  // priority.  The higher priority will only take affect if the
  // program is run as root or suid.
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
  pthread_attr_setschedpolicy(&attr, SCHED_RR);

  int err = pthread_create(&info->thread, &attr, alsaCallbackHandler, &stream_.callbackInfo);
  pthread_attr_destroy(&attr);
  if (err) {
    info->usingCallback = false;
    sprintf(message_, "RtApiAlsa: error starting callback thread!");
    error(RtError::THREAD_ERROR);
  }
}

void RtApiAlsa :: cancelStreamCallback()
{
  verifyStream();

  if (stream_.callbackInfo.usingCallback) {

    if (stream_.state == STREAM_RUNNING)
      stopStream();

    MUTEX_LOCK(&stream_.mutex);

    stream_.callbackInfo.usingCallback = false;
    pthread_join(stream_.callbackInfo.thread, NULL);
    stream_.callbackInfo.thread = 0;
    stream_.callbackInfo.callback = NULL;
    stream_.callbackInfo.userData = NULL;

    MUTEX_UNLOCK(&stream_.mutex);
  }
}

extern "C" void *alsaCallbackHandler(void *ptr)
{
  CallbackInfo *info = (CallbackInfo *) ptr;
  RtApiAlsa *object = (RtApiAlsa *) info->object;
  bool *usingCallback = &info->usingCallback;

  while ( *usingCallback ) {
    try {
      object->tickStream();
    }
    catch (RtError &exception) {
      fprintf(stderr, "\nRtApiAlsa: callback thread error (%s) ... closing thread.\n\n",
              exception.getMessageString());
      break;
    }
  }

  pthread_exit(NULL);
}

//******************** End of __LINUX_ALSA__ *********************//
#endif

#if defined(__WINDOWS_ASIO__) // ASIO API on Windows

// The ASIO API is designed around a callback scheme, so this
// implementation is similar to that used for OS-X CoreAudio and Linux
// Jack.  The primary constraint with ASIO is that it only allows
// access to a single driver at a time.  Thus, it is not possible to
// have more than one simultaneous RtAudio stream.
//
// This implementation also requires a number of external ASIO files
// and a few global variables.  The ASIO callback scheme does not
// allow for the passing of user data, so we must create a global
// pointer to our callbackInfo structure.
//
// On unix systems, we make use of a pthread condition variable.
// Since there is no equivalent in Windows, I hacked something based
// on information found in
// http://www.cs.wustl.edu/~schmidt/win32-cv-1.html.

#include "asio/asiosys.h"
#include "asio/asio.h"
#include "asio/asiodrivers.h"
#include <math.h>

AsioDrivers drivers;
ASIOCallbacks asioCallbacks;
ASIODriverInfo driverInfo;
CallbackInfo *asioCallbackInfo;

struct AsioHandle {
  bool stopStream;
  ASIOBufferInfo *bufferInfos;
  HANDLE condition;

  AsioHandle()
    :stopStream(false), bufferInfos(0) {}
};

RtApiAsio :: RtApiAsio()
{
  this->initialize();

  if (nDevices_ <= 0) {
    sprintf(message_, "RtApiAsio: no Windows ASIO audio drivers found!");
    error(RtError::NO_DEVICES_FOUND);
  }
}

RtApiAsio :: ~RtApiAsio()
{
  if ( stream_.mode != UNINITIALIZED ) closeStream();
}

void RtApiAsio :: initialize(void)
{
  nDevices_ = drivers.asioGetNumDev();
  if (nDevices_ <= 0) return;

  // Create device structures and write device driver names to each.
  RtApiDevice device;
  char name[128];
  for (int i=0; i<nDevices_; i++) {
    if ( drivers.asioGetDriverName( i, name, 128 ) == 0 ) {
      device.name.erase();
      device.name.append( (const char *)name, strlen(name)+1);
      devices_.push_back(device);
    }
    else {
      sprintf(message_, "RtApiAsio: error getting driver name for device index %d!", i);
      error(RtError::WARNING);
    }
  }

  nDevices_ = (int) devices_.size();

  drivers.removeCurrentDriver();
  driverInfo.asioVersion = 2;
  // See note in DirectSound implementation about GetDesktopWindow().
  driverInfo.sysRef = GetForegroundWindow();
}

void RtApiAsio :: probeDeviceInfo(RtApiDevice *info)
{
  // Don't probe if a stream is already open.
  if ( stream_.mode != UNINITIALIZED ) {
    sprintf(message_, "RtApiAsio: unable to probe driver while a stream is open.");
    error(RtError::DEBUG_WARNING);
    return;
  }

  if ( !drivers.loadDriver( (char *)info->name.c_str() ) ) {
    sprintf(message_, "RtApiAsio: error loading driver (%s).", info->name.c_str());
    error(RtError::DEBUG_WARNING);
    return;
  }

  ASIOError result = ASIOInit( &driverInfo );
  if ( result != ASE_OK ) {
    char details[32];
    if ( result == ASE_HWMalfunction )
      sprintf(details, "hardware malfunction");
    else if ( result == ASE_NoMemory )
      sprintf(details, "no memory");
    else if ( result == ASE_NotPresent )
      sprintf(details, "driver/hardware not present");
    else
      sprintf(details, "unspecified");
    sprintf(message_, "RtApiAsio: error (%s) initializing driver (%s).", details, info->name.c_str());
    error(RtError::DEBUG_WARNING);
    return;
  }

  // Determine the device channel information.
  long inputChannels, outputChannels;
  result = ASIOGetChannels( &inputChannels, &outputChannels );
  if ( result != ASE_OK ) {
    drivers.removeCurrentDriver();
    sprintf(message_, "RtApiAsio: error getting input/output channel count (%s).", info->name.c_str());
    error(RtError::DEBUG_WARNING);
    return;
  }

  info->maxOutputChannels = outputChannels;
  if ( outputChannels > 0 ) info->minOutputChannels = 1;

  info->maxInputChannels = inputChannels;
  if ( inputChannels > 0 ) info->minInputChannels = 1;

  // If device opens for both playback and capture, we determine the channels.
  if (info->maxOutputChannels > 0 && info->maxInputChannels > 0) {
    info->hasDuplexSupport = true;
    info->maxDuplexChannels = (info->maxOutputChannels > info->maxInputChannels) ?
      info->maxInputChannels : info->maxOutputChannels;
    info->minDuplexChannels = (info->minOutputChannels > info->minInputChannels) ?
      info->minInputChannels : info->minOutputChannels;
  }

  // Determine the supported sample rates.
  info->sampleRates.clear();
  for (unsigned int i=0; i<MAX_SAMPLE_RATES; i++) {
    result = ASIOCanSampleRate( (ASIOSampleRate) SAMPLE_RATES[i] );
    if ( result == ASE_OK )
      info->sampleRates.push_back( SAMPLE_RATES[i] );
  }

  if (info->sampleRates.size() == 0) {
    drivers.removeCurrentDriver();
    sprintf( message_, "RtApiAsio: No supported sample rates found for driver (%s).", info->name.c_str() );
    error(RtError::DEBUG_WARNING);
    return;
  }

  // Determine supported data types ... just check first channel and assume rest are the same.
  ASIOChannelInfo channelInfo;
  channelInfo.channel = 0;
  channelInfo.isInput = true;
  if ( info->maxInputChannels <= 0 ) channelInfo.isInput = false;
  result = ASIOGetChannelInfo( &channelInfo );
  if ( result != ASE_OK ) {
    drivers.removeCurrentDriver();
    sprintf(message_, "RtApiAsio: error getting driver (%s) channel information.", info->name.c_str());
    error(RtError::DEBUG_WARNING);
    return;
  }

  if ( channelInfo.type == ASIOSTInt16MSB || channelInfo.type == ASIOSTInt16LSB )
    info->nativeFormats |= RTAUDIO_SINT16;
  else if ( channelInfo.type == ASIOSTInt32MSB || channelInfo.type == ASIOSTInt32LSB )
    info->nativeFormats |= RTAUDIO_SINT32;
  else if ( channelInfo.type == ASIOSTFloat32MSB || channelInfo.type == ASIOSTFloat32LSB )
    info->nativeFormats |= RTAUDIO_FLOAT32;
  else if ( channelInfo.type == ASIOSTFloat64MSB || channelInfo.type == ASIOSTFloat64LSB )
    info->nativeFormats |= RTAUDIO_FLOAT64;

	// Check that we have at least one supported format.
  if (info->nativeFormats == 0) {
    drivers.removeCurrentDriver();
    sprintf(message_, "RtApiAsio: driver (%s) data format not supported by RtAudio.",
            info->name.c_str());
    error(RtError::DEBUG_WARNING);
    return;
  }

  info->probed = true;
  drivers.removeCurrentDriver();
}

void bufferSwitch(long index, ASIOBool processNow)
{
  RtApiAsio *object = (RtApiAsio *) asioCallbackInfo->object;
  try {
    object->callbackEvent( index );
  }
  catch (RtError &exception) {
    fprintf(stderr, "\nRtApiAsio: callback handler error (%s)!\n\n", exception.getMessageString());
    return;
  }

  return;
}

void sampleRateChanged(ASIOSampleRate sRate)
{
  // The ASIO documentation says that this usually only happens during
  // external sync.  Audio processing is not stopped by the driver,
  // actual sample rate might not have even changed, maybe only the
  // sample rate status of an AES/EBU or S/PDIF digital input at the
  // audio device.

  RtAudio *object = (RtAudio *) asioCallbackInfo->object;
  try {
    object->stopStream();
  }
  catch (RtError &exception) {
    fprintf(stderr, "\nRtApiAsio: sampleRateChanged() error (%s)!\n\n", exception.getMessageString());
    return;
  }

  fprintf(stderr, "\nRtApiAsio: driver reports sample rate changed to %d ... stream stopped!!!", (int) sRate);
}

long asioMessages(long selector, long value, void* message, double* opt)
{
  long ret = 0;
  switch(selector) {
  case kAsioSelectorSupported:
    if(value == kAsioResetRequest
       || value == kAsioEngineVersion
       || value == kAsioResyncRequest
       || value == kAsioLatenciesChanged
       // The following three were added for ASIO 2.0, you don't
       // necessarily have to support them.
       || value == kAsioSupportsTimeInfo
       || value == kAsioSupportsTimeCode
       || value == kAsioSupportsInputMonitor)
      ret = 1L;
    break;
  case kAsioResetRequest:
    // Defer the task and perform the reset of the driver during the
    // next "safe" situation.  You cannot reset the driver right now,
    // as this code is called from the driver.  Reset the driver is
    // done by completely destruct is. I.e. ASIOStop(),
    // ASIODisposeBuffers(), Destruction Afterwards you initialize the
    // driver again.
    fprintf(stderr, "\nRtApiAsio: driver reset requested!!!");
    ret = 1L;
    break;
  case kAsioResyncRequest:
    // This informs the application that the driver encountered some
    // non-fatal data loss.  It is used for synchronization purposes
    // of different media.  Added mainly to work around the Win16Mutex
    // problems in Windows 95/98 with the Windows Multimedia system,
    // which could lose data because the Mutex was held too long by
    // another thread.  However a driver can issue it in other
    // situations, too.
    fprintf(stderr, "\nRtApiAsio: driver resync requested!!!");
    ret = 1L;
    break;
  case kAsioLatenciesChanged:
    // This will inform the host application that the drivers were
    // latencies changed.  Beware, it this does not mean that the
    // buffer sizes have changed!  You might need to update internal
    // delay data.
    fprintf(stderr, "\nRtApiAsio: driver latency may have changed!!!");
    ret = 1L;
    break;
  case kAsioEngineVersion:
    // Return the supported ASIO version of the host application.  If
    // a host application does not implement this selector, ASIO 1.0
    // is assumed by the driver.
    ret = 2L;
    break;
  case kAsioSupportsTimeInfo:
    // Informs the driver whether the
    // asioCallbacks.bufferSwitchTimeInfo() callback is supported.
    // For compatibility with ASIO 1.0 drivers the host application
    // should always support the "old" bufferSwitch method, too.
    ret = 0;
    break;
  case kAsioSupportsTimeCode:
    // Informs the driver wether application is interested in time
    // code info.  If an application does not need to know about time
    // code, the driver has less work to do.
    ret = 0;
    break;
  }
  return ret;
}

bool RtApiAsio :: probeDeviceOpen(int device, StreamMode mode, int channels, 
                                  int sampleRate, RtAudioFormat format,
                                  int *bufferSize, int numberOfBuffers)
{
  // For ASIO, a duplex stream MUST use the same driver.
  if ( mode == INPUT && stream_.mode == OUTPUT && stream_.device[0] != device ) {
    sprintf(message_, "RtApiAsio: duplex stream must use the same device for input and output.");
    error(RtError::WARNING);
    return FAILURE;
  }

  // Only load the driver once for duplex stream.
  ASIOError result;
  if ( mode != INPUT || stream_.mode != OUTPUT ) {
    if ( !drivers.loadDriver( (char *)devices_[device].name.c_str() ) ) {
      sprintf(message_, "RtApiAsio: error loading driver (%s).", devices_[device].name.c_str());
      error(RtError::DEBUG_WARNING);
      return FAILURE;
    }

    result = ASIOInit( &driverInfo );
    if ( result != ASE_OK ) {
      char details[32];
      if ( result == ASE_HWMalfunction )
        sprintf(details, "hardware malfunction");
      else if ( result == ASE_NoMemory )
        sprintf(details, "no memory");
      else if ( result == ASE_NotPresent )
        sprintf(details, "driver/hardware not present");
      else
        sprintf(details, "unspecified");
      sprintf(message_, "RtApiAsio: error (%s) initializing driver (%s).", details, devices_[device].name.c_str());
      error(RtError::DEBUG_WARNING);
      return FAILURE;
    }
  }

  // Check the device channel count.
  long inputChannels, outputChannels;
  result = ASIOGetChannels( &inputChannels, &outputChannels );
  if ( result != ASE_OK ) {
    drivers.removeCurrentDriver();
    sprintf(message_, "RtApiAsio: error getting input/output channel count (%s).",
            devices_[device].name.c_str());
    error(RtError::DEBUG_WARNING);
    return FAILURE;
  }

  if ( ( mode == OUTPUT && channels > outputChannels) ||
       ( mode == INPUT && channels > inputChannels) ) {
    drivers.removeCurrentDriver();
    sprintf(message_, "RtApiAsio: driver (%s) does not support requested channel count (%d).",
            devices_[device].name.c_str(), channels);
    error(RtError::DEBUG_WARNING);
    return FAILURE;
  }
  stream_.nDeviceChannels[mode] = channels;
  stream_.nUserChannels[mode] = channels;

  // Verify the sample rate is supported.
  result = ASIOCanSampleRate( (ASIOSampleRate) sampleRate );
  if ( result != ASE_OK ) {
    drivers.removeCurrentDriver();
    sprintf(message_, "RtApiAsio: driver (%s) does not support requested sample rate (%d).",
            devices_[device].name.c_str(), sampleRate);
    error(RtError::DEBUG_WARNING);
    return FAILURE;
  }

  // Set the sample rate.
  result = ASIOSetSampleRate( (ASIOSampleRate) sampleRate );
  if ( result != ASE_OK ) {
    drivers.removeCurrentDriver();
    sprintf(message_, "RtApiAsio: driver (%s) error setting sample rate (%d).",
            devices_[device].name.c_str(), sampleRate);
    error(RtError::DEBUG_WARNING);
    return FAILURE;
  }

  // Determine the driver data type.
  ASIOChannelInfo channelInfo;
  channelInfo.channel = 0;
  if ( mode == OUTPUT ) channelInfo.isInput = false;
  else channelInfo.isInput = true;
  result = ASIOGetChannelInfo( &channelInfo );
  if ( result != ASE_OK ) {
    drivers.removeCurrentDriver();
    sprintf(message_, "RtApiAsio: driver (%s) error getting data format.",
            devices_[device].name.c_str());
    error(RtError::DEBUG_WARNING);
    return FAILURE;
  }

  // Assuming WINDOWS host is always little-endian.
  stream_.doByteSwap[mode] = false;
  stream_.userFormat = format;
  stream_.deviceFormat[mode] = 0;
  if ( channelInfo.type == ASIOSTInt16MSB || channelInfo.type == ASIOSTInt16LSB ) {
    stream_.deviceFormat[mode] = RTAUDIO_SINT16;
    if ( channelInfo.type == ASIOSTInt16MSB ) stream_.doByteSwap[mode] = true;
  }
  else if ( channelInfo.type == ASIOSTInt32MSB || channelInfo.type == ASIOSTInt32LSB ) {
    stream_.deviceFormat[mode] = RTAUDIO_SINT32;
    if ( channelInfo.type == ASIOSTInt32MSB ) stream_.doByteSwap[mode] = true;
  }
  else if ( channelInfo.type == ASIOSTFloat32MSB || channelInfo.type == ASIOSTFloat32LSB ) {
    stream_.deviceFormat[mode] = RTAUDIO_FLOAT32;
    if ( channelInfo.type == ASIOSTFloat32MSB ) stream_.doByteSwap[mode] = true;
  }
  else if ( channelInfo.type == ASIOSTFloat64MSB || channelInfo.type == ASIOSTFloat64LSB ) {
    stream_.deviceFormat[mode] = RTAUDIO_FLOAT64;
    if ( channelInfo.type == ASIOSTFloat64MSB ) stream_.doByteSwap[mode] = true;
  }

  if ( stream_.deviceFormat[mode] == 0 ) {
    drivers.removeCurrentDriver();
    sprintf(message_, "RtApiAsio: driver (%s) data format not supported by RtAudio.",
            devices_[device].name.c_str());
    error(RtError::DEBUG_WARNING);
    return FAILURE;
  }

  // Set the buffer size.  For a duplex stream, this will end up
  // setting the buffer size based on the input constraints, which
  // should be ok.
  long minSize, maxSize, preferSize, granularity;
  result = ASIOGetBufferSize( &minSize, &maxSize, &preferSize, &granularity );
  if ( result != ASE_OK ) {
    drivers.removeCurrentDriver();
    sprintf(message_, "RtApiAsio: driver (%s) error getting buffer size.",
            devices_[device].name.c_str());
    error(RtError::DEBUG_WARNING);
    return FAILURE;
  }

  if ( *bufferSize < minSize ) *bufferSize = minSize;
  else if ( *bufferSize > maxSize ) *bufferSize = maxSize;
  else if ( granularity == -1 ) {
    // Make sure bufferSize is a power of two.
    double power = log10( (double) *bufferSize ) / log10( 2.0 );
    *bufferSize = (int) pow( 2.0, floor(power+0.5) );
    if ( *bufferSize < minSize ) *bufferSize = minSize;
    else if ( *bufferSize > maxSize ) *bufferSize = maxSize;
    else *bufferSize = preferSize;
  }

  if ( mode == INPUT && stream_.mode == OUTPUT && stream_.bufferSize != *bufferSize )
    std::cerr << "Possible input/output buffersize discrepancy!" << std::endl;

  stream_.bufferSize = *bufferSize;
  stream_.nBuffers = 2;

  // ASIO always uses deinterleaved channels.
  stream_.deInterleave[mode] = true;

  // Allocate, if necessary, our AsioHandle structure for the stream.
  AsioHandle *handle = (AsioHandle *) stream_.apiHandle;
  if ( handle == 0 ) {
    handle = (AsioHandle *) calloc(1, sizeof(AsioHandle));
    if ( handle == NULL ) {
      drivers.removeCurrentDriver();
      sprintf(message_, "RtApiAsio: error allocating AsioHandle memory (%s).",
              devices_[device].name.c_str());
      error(RtError::DEBUG_WARNING);
      return FAILURE;
    }
    handle->bufferInfos = 0;
    // Create a manual-reset event.
    handle->condition = CreateEvent(NULL,  // no security
                                    TRUE,  // manual-reset
                                    FALSE, // non-signaled initially
                                    NULL); // unnamed
    stream_.apiHandle = (void *) handle;
  }

  // Create the ASIO internal buffers.  Since RtAudio sets up input
  // and output separately, we'll have to dispose of previously
  // created output buffers for a duplex stream.
  if ( mode == INPUT && stream_.mode == OUTPUT ) {
    ASIODisposeBuffers();
    if ( handle->bufferInfos ) free( handle->bufferInfos );
  }

  // Allocate, initialize, and save the bufferInfos in our stream callbackInfo structure.
  int i, nChannels = stream_.nDeviceChannels[0] + stream_.nDeviceChannels[1];
  handle->bufferInfos = (ASIOBufferInfo *) malloc( nChannels * sizeof(ASIOBufferInfo) );
  if (handle->bufferInfos == NULL) {
    sprintf(message_, "RtApiAsio: error allocating bufferInfo memory (%s).",
            devices_[device].name.c_str());
    goto error;
  }
  ASIOBufferInfo *infos;
  infos = handle->bufferInfos;
  for ( i=0; i<stream_.nDeviceChannels[0]; i++, infos++ ) {
    infos->isInput = ASIOFalse;
    infos->channelNum = i;
    infos->buffers[0] = infos->buffers[1] = 0;
  }
  for ( i=0; i<stream_.nDeviceChannels[1]; i++, infos++ ) {
    infos->isInput = ASIOTrue;
    infos->channelNum = i;
    infos->buffers[0] = infos->buffers[1] = 0;
  }

  // Set up the ASIO callback structure and create the ASIO data buffers.
  asioCallbacks.bufferSwitch = &bufferSwitch;
  asioCallbacks.sampleRateDidChange = &sampleRateChanged;
  asioCallbacks.asioMessage = &asioMessages;
  asioCallbacks.bufferSwitchTimeInfo = NULL;
  result = ASIOCreateBuffers( handle->bufferInfos, nChannels, stream_.bufferSize, &asioCallbacks);
  if ( result != ASE_OK ) {
    sprintf(message_, "RtApiAsio: driver (%s) error creating buffers.",
            devices_[device].name.c_str());
    goto error;
  }

  // Set flags for buffer conversion.
  stream_.doConvertBuffer[mode] = false;
  if (stream_.userFormat != stream_.deviceFormat[mode])
    stream_.doConvertBuffer[mode] = true;
  if (stream_.nUserChannels[mode] < stream_.nDeviceChannels[mode])
    stream_.doConvertBuffer[mode] = true;
  if (stream_.nUserChannels[mode] > 1 && stream_.deInterleave[mode])
    stream_.doConvertBuffer[mode] = true;

  // Allocate necessary internal buffers
  if ( stream_.nUserChannels[0] != stream_.nUserChannels[1] ) {

    long buffer_bytes;
    if (stream_.nUserChannels[0] >= stream_.nUserChannels[1])
      buffer_bytes = stream_.nUserChannels[0];
    else
      buffer_bytes = stream_.nUserChannels[1];

    buffer_bytes *= *bufferSize * formatBytes(stream_.userFormat);
    if (stream_.userBuffer) free(stream_.userBuffer);
    stream_.userBuffer = (char *) calloc(buffer_bytes, 1);
    if (stream_.userBuffer == NULL) {
      sprintf(message_, "RtApiAsio: error allocating user buffer memory (%s).",
              devices_[device].name.c_str());
      goto error;
    }
  }

  if ( stream_.doConvertBuffer[mode] ) {

    long buffer_bytes;
    bool makeBuffer = true;
    if ( mode == OUTPUT )
      buffer_bytes = stream_.nDeviceChannels[0] * formatBytes(stream_.deviceFormat[0]);
    else { // mode == INPUT
      buffer_bytes = stream_.nDeviceChannels[1] * formatBytes(stream_.deviceFormat[1]);
      if ( stream_.mode == OUTPUT && stream_.deviceBuffer ) {
        long bytes_out = stream_.nDeviceChannels[0] * formatBytes(stream_.deviceFormat[0]);
        if ( buffer_bytes < bytes_out ) makeBuffer = false;
      }
    }

    if ( makeBuffer ) {
      buffer_bytes *= *bufferSize;
      if (stream_.deviceBuffer) free(stream_.deviceBuffer);
      stream_.deviceBuffer = (char *) calloc(buffer_bytes, 1);
      if (stream_.deviceBuffer == NULL) {
        sprintf(message_, "RtApiAsio: error allocating device buffer memory (%s).",
                devices_[device].name.c_str());
        goto error;
      }
    }
  }

  stream_.device[mode] = device;
  stream_.state = STREAM_STOPPED;
  if ( stream_.mode == OUTPUT && mode == INPUT )
    // We had already set up an output stream.
    stream_.mode = DUPLEX;
  else
    stream_.mode = mode;
  stream_.sampleRate = sampleRate;
  asioCallbackInfo = &stream_.callbackInfo;
  stream_.callbackInfo.object = (void *) this;

  return SUCCESS;

 error:
  ASIODisposeBuffers();
  drivers.removeCurrentDriver();

  if ( handle ) {
    CloseHandle( handle->condition );
    if ( handle->bufferInfos )
      free( handle->bufferInfos );
    free( handle );
    stream_.apiHandle = 0;
  }

  if (stream_.userBuffer) {
    free(stream_.userBuffer);
    stream_.userBuffer = 0;
  }

  error(RtError::WARNING);
  return FAILURE;
}

void RtApiAsio :: closeStream()
{
  // We don't want an exception to be thrown here because this
  // function is called by our class destructor.  So, do our own
  // streamId check.
  if ( stream_.mode == UNINITIALIZED ) {
    sprintf(message_, "RtApiAsio::closeStream(): no open stream to close!");
    error(RtError::WARNING);
    return;
  }

  if (stream_.state == STREAM_RUNNING)
    ASIOStop();

  ASIODisposeBuffers();
  drivers.removeCurrentDriver();

  AsioHandle *handle = (AsioHandle *) stream_.apiHandle;
  if ( handle ) {
    CloseHandle( handle->condition );
    if ( handle->bufferInfos )
      free( handle->bufferInfos );
    free( handle );
    stream_.apiHandle = 0;
  }

  if (stream_.userBuffer) {
    free(stream_.userBuffer);
    stream_.userBuffer = 0;
  }

  if (stream_.deviceBuffer) {
    free(stream_.deviceBuffer);
    stream_.deviceBuffer = 0;
  }

  stream_.mode = UNINITIALIZED;
}

void RtApiAsio :: setStreamCallback(RtAudioCallback callback, void *userData)
{
  verifyStream();

  if ( stream_.callbackInfo.usingCallback ) {
    sprintf(message_, "RtApiAsio: A callback is already set for this stream!");
    error(RtError::WARNING);
    return;
  }

  stream_.callbackInfo.callback = (void *) callback;
  stream_.callbackInfo.userData = userData;
  stream_.callbackInfo.usingCallback = true;
}

void RtApiAsio :: cancelStreamCallback()
{
  verifyStream();

  if (stream_.callbackInfo.usingCallback) {

    if (stream_.state == STREAM_RUNNING)
      stopStream();

    MUTEX_LOCK(&stream_.mutex);

    stream_.callbackInfo.usingCallback = false;
    stream_.callbackInfo.userData = NULL;
    stream_.state = STREAM_STOPPED;
    stream_.callbackInfo.callback = NULL;

    MUTEX_UNLOCK(&stream_.mutex);
  }
}

void RtApiAsio :: startStream()
{
  verifyStream();
  if (stream_.state == STREAM_RUNNING) return;

  MUTEX_LOCK(&stream_.mutex);

  ASIOError result = ASIOStart();
  if ( result != ASE_OK ) {
    sprintf(message_, "RtApiAsio: error starting device (%s).",
              devices_[stream_.device[0]].name.c_str());
    MUTEX_UNLOCK(&stream_.mutex);
    error(RtError::DRIVER_ERROR);
  }
  AsioHandle *handle = (AsioHandle *) stream_.apiHandle;
  handle->stopStream = false;
  stream_.state = STREAM_RUNNING;

  MUTEX_UNLOCK(&stream_.mutex);
}

void RtApiAsio :: stopStream()
{
  verifyStream();
  if (stream_.state == STREAM_STOPPED) return;

  // Change the state before the lock to improve shutdown response
  // when using a callback.
  stream_.state = STREAM_STOPPED;
  MUTEX_LOCK(&stream_.mutex);

  ASIOError result = ASIOStop();
  if ( result != ASE_OK ) {
    sprintf(message_, "RtApiAsio: error stopping device (%s).",
              devices_[stream_.device[0]].name.c_str());
    MUTEX_UNLOCK(&stream_.mutex);
    error(RtError::DRIVER_ERROR);
  }

  MUTEX_UNLOCK(&stream_.mutex);
}

void RtApiAsio :: abortStream()
{
  stopStream();
}

void RtApiAsio :: tickStream()
{
  verifyStream();

  if (stream_.state == STREAM_STOPPED)
    return;

  if (stream_.callbackInfo.usingCallback) {
    sprintf(message_, "RtApiAsio: tickStream() should not be used when a callback function is set!");
    error(RtError::WARNING);
    return;
  }

  AsioHandle *handle = (AsioHandle *) stream_.apiHandle;

  MUTEX_LOCK(&stream_.mutex);

  // Release the stream_mutex here and wait for the event
  // to become signaled by the callback process.
  MUTEX_UNLOCK(&stream_.mutex);
  WaitForMultipleObjects(1, &handle->condition, FALSE, INFINITE);
  ResetEvent( handle->condition );

  RtApi::tickStream();
}

void RtApiAsio :: callbackEvent(long bufferIndex)
{
  verifyStream();

  if (stream_.state == STREAM_STOPPED) return;

  CallbackInfo *info = (CallbackInfo *) &stream_.callbackInfo;
  AsioHandle *handle = (AsioHandle *) stream_.apiHandle;
  if ( info->usingCallback && handle->stopStream ) {
    // Check if the stream should be stopped (via the previous user
    // callback return value).  We stop the stream here, rather than
    // after the function call, so that output data can first be
    // processed.
    this->stopStream();
    return;
  }

  MUTEX_LOCK(&stream_.mutex);

  // Invoke user callback first, to get fresh output data.
  if ( info->usingCallback ) {
    RtAudioCallback callback = (RtAudioCallback) info->callback;
    if ( callback(stream_.userBuffer, stream_.bufferSize, info->userData) )
      handle->stopStream = true;
  }

  int bufferBytes, j;
  int nChannels = stream_.nDeviceChannels[0] + stream_.nDeviceChannels[1];
  if ( stream_.mode == OUTPUT || stream_.mode == DUPLEX ) {

    bufferBytes = stream_.bufferSize * formatBytes(stream_.deviceFormat[0]);
    if (stream_.doConvertBuffer[0]) {

      convertStreamBuffer(OUTPUT);
      if ( stream_.doByteSwap[0] )
        byteSwapBuffer(stream_.deviceBuffer,
                       stream_.bufferSize * stream_.nDeviceChannels[0],
                       stream_.deviceFormat[0]);

      // Always de-interleave ASIO output data.
      j = 0;
      for ( int i=0; i<nChannels; i++ ) {
        if ( handle->bufferInfos[i].isInput != ASIOTrue )
          memcpy(handle->bufferInfos[i].buffers[bufferIndex],
                 &stream_.deviceBuffer[j++*bufferBytes], bufferBytes );
      }
    }
    else { // single channel only

      if (stream_.doByteSwap[0])
        byteSwapBuffer(stream_.userBuffer,
                       stream_.bufferSize * stream_.nUserChannels[0],
                       stream_.userFormat);

      for ( int i=0; i<nChannels; i++ ) {
        if ( handle->bufferInfos[i].isInput != ASIOTrue ) {
          memcpy(handle->bufferInfos[i].buffers[bufferIndex], stream_.userBuffer, bufferBytes );
          break;
        }
      }
    }
  }

  if ( stream_.mode == INPUT || stream_.mode == DUPLEX ) {

    bufferBytes = stream_.bufferSize * formatBytes(stream_.deviceFormat[1]);
    if (stream_.doConvertBuffer[1]) {

      // Always interleave ASIO input data.
      j = 0;
      for ( int i=0; i<nChannels; i++ ) {
        if ( handle->bufferInfos[i].isInput == ASIOTrue )
          memcpy(&stream_.deviceBuffer[j++*bufferBytes],
                 handle->bufferInfos[i].buffers[bufferIndex],
                 bufferBytes );
      }

      if ( stream_.doByteSwap[1] )
        byteSwapBuffer(stream_.deviceBuffer,
                       stream_.bufferSize * stream_.nDeviceChannels[1],
                       stream_.deviceFormat[1]);
      convertStreamBuffer(INPUT);

    }
    else { // single channel only
      for ( int i=0; i<nChannels; i++ ) {
        if ( handle->bufferInfos[i].isInput == ASIOTrue ) {
          memcpy(stream_.userBuffer,
                 handle->bufferInfos[i].buffers[bufferIndex],
                 bufferBytes );
          break;
        }
      }

      if (stream_.doByteSwap[1])
        byteSwapBuffer(stream_.userBuffer,
                       stream_.bufferSize * stream_.nUserChannels[1],
                       stream_.userFormat);
    }
  }

  if ( !info->usingCallback )
    SetEvent( handle->condition );

  MUTEX_UNLOCK(&stream_.mutex);
}

//******************** End of __WINDOWS_ASIO__ *********************//
#endif

#if defined(__WINDOWS_DS__) // Windows DirectSound API

#include <dsound.h>

// A structure to hold various information related to the DirectSound
// API implementation.
struct DsHandle {
  void *object;
  void *buffer;
  UINT bufferPointer;  
};

// Declarations for utility functions, callbacks, and structures
// specific to the DirectSound implementation.
static bool CALLBACK deviceCountCallback(LPGUID lpguid,
                                         LPCSTR lpcstrDescription,
                                         LPCSTR lpcstrModule,
                                         LPVOID lpContext);

static bool CALLBACK deviceInfoCallback(LPGUID lpguid,
                                        LPCSTR lpcstrDescription,
                                        LPCSTR lpcstrModule,
                                        LPVOID lpContext);

static bool CALLBACK defaultDeviceCallback(LPGUID lpguid,
                                           LPCSTR lpcstrDescription,
                                           LPCSTR lpcstrModule,
                                           LPVOID lpContext);

static bool CALLBACK deviceIdCallback(LPGUID lpguid,
                                      LPCSTR lpcstrDescription,
                                      LPCSTR lpcstrModule,
                                      LPVOID lpContext);

static char* getErrorString(int code);

extern "C" unsigned __stdcall callbackHandler(void *ptr);

struct enum_info {
  char name[64];
  LPGUID id;
  bool isInput;
  bool isValid;
};

RtApiDs :: RtApiDs()
{
  this->initialize();

  if (nDevices_ <= 0) {
    sprintf(message_, "RtApiDs: no Windows DirectSound audio devices found!");
    error(RtError::NO_DEVICES_FOUND);
 }
}

RtApiDs :: ~RtApiDs()
{
  if ( stream_.mode != UNINITIALIZED ) closeStream();
}

int RtApiDs :: getDefaultInputDevice(void)
{
  enum_info info;
  info.name[0] = '\0';

  // Enumerate through devices to find the default output.
  HRESULT result = DirectSoundCaptureEnumerate((LPDSENUMCALLBACK)defaultDeviceCallback, &info);
  if ( FAILED(result) ) {
    sprintf(message_, "RtApiDs: Error performing default input device enumeration: %s.",
            getErrorString(result));
    error(RtError::WARNING);
    return 0;
  }

  for ( int i=0; i<nDevices_; i++ ) {
    if ( strncmp( info.name, devices_[i].name.c_str(), 64 ) == 0 ) return i;
  }


  return 0;
}

int RtApiDs :: getDefaultOutputDevice(void)
{
  enum_info info;
  info.name[0] = '\0';

  // Enumerate through devices to find the default output.
  HRESULT result = DirectSoundEnumerate((LPDSENUMCALLBACK)defaultDeviceCallback, &info);
  if ( FAILED(result) ) {
    sprintf(message_, "RtApiDs: Error performing default output device enumeration: %s.",
            getErrorString(result));
    error(RtError::WARNING);
    return 0;
  }

  for ( int i=0; i<nDevices_; i++ )
    if ( strncmp( info.name, devices_[i].name.c_str(), 64 ) == 0 ) return i;

  return 0;
}

void RtApiDs :: initialize(void)
{
  int i, ins = 0, outs = 0, count = 0;
  HRESULT result;
  nDevices_ = 0;

  // Count DirectSound devices.
  result = DirectSoundEnumerate((LPDSENUMCALLBACK)deviceCountCallback, &outs);
  if ( FAILED(result) ) {
    sprintf(message_, "RtApiDs: Unable to enumerate through sound playback devices: %s.",
            getErrorString(result));
    error(RtError::DRIVER_ERROR);
  }

  // Count DirectSoundCapture devices.
  result = DirectSoundCaptureEnumerate((LPDSENUMCALLBACK)deviceCountCallback, &ins);
  if ( FAILED(result) ) {
    sprintf(message_, "RtApiDs: Unable to enumerate through sound capture devices: %s.",
            getErrorString(result));
    error(RtError::DRIVER_ERROR);
  }

  count = ins + outs;
  if (count == 0) return;

  std::vector<enum_info> info(count);
  for (i=0; i<count; i++) {
    info[i].name[0] = '\0';
    if (i < outs) info[i].isInput = false;
    else info[i].isInput = true;
  }

  // Get playback device info and check capabilities.
  result = DirectSoundEnumerate((LPDSENUMCALLBACK)deviceInfoCallback, &info[0]);
  if ( FAILED(result) ) {
    sprintf(message_, "RtApiDs: Unable to enumerate through sound playback devices: %s.",
            getErrorString(result));
    error(RtError::DRIVER_ERROR);
  }

  // Get capture device info and check capabilities.
  result = DirectSoundCaptureEnumerate((LPDSENUMCALLBACK)deviceInfoCallback, &info[0]);
  if ( FAILED(result) ) {
    sprintf(message_, "RtApiDs: Unable to enumerate through sound capture devices: %s.",
            getErrorString(result));
    error(RtError::DRIVER_ERROR);
  }

  // Create device structures for valid devices and write device names
  // to each.  Devices are considered invalid if they cannot be
  // opened, they report < 1 supported channels, or they report no
  // supported data (capture only).
  RtApiDevice device;
  int index = 0;
  for (i=0; i<count; i++) {
    if ( info[i].isValid ) {
      device.name.erase();
      device.name.append( (const char *)info[i].name, strlen(info[i].name)+1);
      devices_.push_back(device);
    }
  }

  nDevices_ = devices_.size();
  return;
}

void RtApiDs :: probeDeviceInfo(RtApiDevice *info)
{
  enum_info dsinfo;
  strncpy( dsinfo.name, info->name.c_str(), 64 );
  dsinfo.isValid = false;

  // Enumerate through input devices to find the id (if it exists).
  HRESULT result = DirectSoundCaptureEnumerate((LPDSENUMCALLBACK)deviceIdCallback, &dsinfo);
  if ( FAILED(result) ) {
    sprintf(message_, "RtApiDs: Error performing input device id enumeration: %s.",
            getErrorString(result));
    error(RtError::WARNING);
    return;
  }

  // Do capture probe first.
  if ( dsinfo.isValid == false )
    goto playback_probe;

  LPDIRECTSOUNDCAPTURE  input;
  result = DirectSoundCaptureCreate( dsinfo.id, &input, NULL );
  if ( FAILED(result) ) {
    sprintf(message_, "RtApiDs: Could not create capture object (%s): %s.",
            info->name.c_str(), getErrorString(result));
    error(RtError::WARNING);
    goto playback_probe;
  }

  DSCCAPS in_caps;
  in_caps.dwSize = sizeof(in_caps);
  result = input->GetCaps( &in_caps );
  if ( FAILED(result) ) {
    input->Release();
    sprintf(message_, "RtApiDs: Could not get capture capabilities (%s): %s.",
            info->name.c_str(), getErrorString(result));
    error(RtError::WARNING);
    goto playback_probe;
  }

  // Get input channel information.
  info->minInputChannels = 1;
  info->maxInputChannels = in_caps.dwChannels;

  // Get sample rate and format information.
  info->sampleRates.clear();
  if( in_caps.dwChannels == 2 ) {
    if( in_caps.dwFormats & WAVE_FORMAT_1S16 ) info->nativeFormats |= RTAUDIO_SINT16;
    if( in_caps.dwFormats & WAVE_FORMAT_2S16 ) info->nativeFormats |= RTAUDIO_SINT16;
    if( in_caps.dwFormats & WAVE_FORMAT_4S16 ) info->nativeFormats |= RTAUDIO_SINT16;
    if( in_caps.dwFormats & WAVE_FORMAT_1S08 ) info->nativeFormats |= RTAUDIO_SINT8;
    if( in_caps.dwFormats & WAVE_FORMAT_2S08 ) info->nativeFormats |= RTAUDIO_SINT8;
    if( in_caps.dwFormats & WAVE_FORMAT_4S08 ) info->nativeFormats |= RTAUDIO_SINT8;

    if ( info->nativeFormats & RTAUDIO_SINT16 ) {
      if( in_caps.dwFormats & WAVE_FORMAT_1S16 ) info->sampleRates.push_back( 11025 );
      if( in_caps.dwFormats & WAVE_FORMAT_2S16 ) info->sampleRates.push_back( 22050 );
      if( in_caps.dwFormats & WAVE_FORMAT_4S16 ) info->sampleRates.push_back( 44100 );
    }
    else if ( info->nativeFormats & RTAUDIO_SINT8 ) {
      if( in_caps.dwFormats & WAVE_FORMAT_1S08 ) info->sampleRates.push_back( 11025 );
      if( in_caps.dwFormats & WAVE_FORMAT_2S08 ) info->sampleRates.push_back( 22050 );
      if( in_caps.dwFormats & WAVE_FORMAT_4S08 ) info->sampleRates.push_back( 44100 );
    }
  }
  else if ( in_caps.dwChannels == 1 ) {
    if( in_caps.dwFormats & WAVE_FORMAT_1M16 ) info->nativeFormats |= RTAUDIO_SINT16;
    if( in_caps.dwFormats & WAVE_FORMAT_2M16 ) info->nativeFormats |= RTAUDIO_SINT16;
    if( in_caps.dwFormats & WAVE_FORMAT_4M16 ) info->nativeFormats |= RTAUDIO_SINT16;
    if( in_caps.dwFormats & WAVE_FORMAT_1M08 ) info->nativeFormats |= RTAUDIO_SINT8;
    if( in_caps.dwFormats & WAVE_FORMAT_2M08 ) info->nativeFormats |= RTAUDIO_SINT8;
    if( in_caps.dwFormats & WAVE_FORMAT_4M08 ) info->nativeFormats |= RTAUDIO_SINT8;

    if ( info->nativeFormats & RTAUDIO_SINT16 ) {
      if( in_caps.dwFormats & WAVE_FORMAT_1M16 ) info->sampleRates.push_back( 11025 );
      if( in_caps.dwFormats & WAVE_FORMAT_2M16 ) info->sampleRates.push_back( 22050 );
      if( in_caps.dwFormats & WAVE_FORMAT_4M16 ) info->sampleRates.push_back( 44100 );
    }
    else if ( info->nativeFormats & RTAUDIO_SINT8 ) {
      if( in_caps.dwFormats & WAVE_FORMAT_1M08 ) info->sampleRates.push_back( 11025 );
      if( in_caps.dwFormats & WAVE_FORMAT_2M08 ) info->sampleRates.push_back( 22050 );
      if( in_caps.dwFormats & WAVE_FORMAT_4M08 ) info->sampleRates.push_back( 44100 );
    }
  }
  else info->minInputChannels = 0; // technically, this would be an error

  input->Release();

 playback_probe:

  dsinfo.isValid = false;

  // Enumerate through output devices to find the id (if it exists).
  result = DirectSoundEnumerate((LPDSENUMCALLBACK)deviceIdCallback, &dsinfo);
  if ( FAILED(result) ) {
    sprintf(message_, "RtApiDs: Error performing output device id enumeration: %s.",
            getErrorString(result));
    error(RtError::WARNING);
    return;
  }

  // Now do playback probe.
  if ( dsinfo.isValid == false )
    goto check_parameters;

  LPDIRECTSOUND  output;
  DSCAPS out_caps;
  result = DirectSoundCreate( dsinfo.id, &output, NULL );
  if ( FAILED(result) ) {
    sprintf(message_, "RtApiDs: Could not create playback object (%s): %s.",
            info->name.c_str(), getErrorString(result));
    error(RtError::WARNING);
    goto check_parameters;
  }

  out_caps.dwSize = sizeof(out_caps);
  result = output->GetCaps( &out_caps );
  if ( FAILED(result) ) {
    output->Release();
    sprintf(message_, "RtApiDs: Could not get playback capabilities (%s): %s.",
            info->name.c_str(), getErrorString(result));
    error(RtError::WARNING);
    goto check_parameters;
  }

  // Get output channel information.
  info->minOutputChannels = 1;
  info->maxOutputChannels = ( out_caps.dwFlags & DSCAPS_PRIMARYSTEREO ) ? 2 : 1;

  // Get sample rate information.  Use capture device rate information
  // if it exists.
  if ( info->sampleRates.size() == 0 ) {
    info->sampleRates.push_back( (int) out_caps.dwMinSecondarySampleRate );
    info->sampleRates.push_back( (int) out_caps.dwMaxSecondarySampleRate );
  }
  else {
    // Check input rates against output rate range.
    for ( int i=info->sampleRates.size()-1; i>=0; i-- ) {
      if ( (unsigned int) info->sampleRates[i] > out_caps.dwMaxSecondarySampleRate )
        info->sampleRates.erase( info->sampleRates.begin() + i );
    }
    while ( info->sampleRates.size() > 0 &&
            ((unsigned int) info->sampleRates[0] < out_caps.dwMinSecondarySampleRate) ) {
      info->sampleRates.erase( info->sampleRates.begin() );
    }
  }

  // Get format information.
  if ( out_caps.dwFlags & DSCAPS_PRIMARY16BIT ) info->nativeFormats |= RTAUDIO_SINT16;
  if ( out_caps.dwFlags & DSCAPS_PRIMARY8BIT ) info->nativeFormats |= RTAUDIO_SINT8;

  output->Release();

 check_parameters:
  if ( info->maxInputChannels == 0 && info->maxOutputChannels == 0 ) {
    sprintf(message_, "RtApiDs: no reported input or output channels for device (%s).",
            info->name.c_str());
    error(RtError::DEBUG_WARNING);
    return;
  }
  if ( info->sampleRates.size() == 0 || info->nativeFormats == 0 ) {
    sprintf(message_, "RtApiDs: no reported sample rates or data formats for device (%s).",
            info->name.c_str());
    error(RtError::DEBUG_WARNING);
    return;
  }

  // Determine duplex status.
  if (info->maxInputChannels < info->maxOutputChannels)
    info->maxDuplexChannels = info->maxInputChannels;
  else
    info->maxDuplexChannels = info->maxOutputChannels;
  if (info->minInputChannels < info->minOutputChannels)
    info->minDuplexChannels = info->minInputChannels;
  else
    info->minDuplexChannels = info->minOutputChannels;

  if ( info->maxDuplexChannels > 0 ) info->hasDuplexSupport = true;
  else info->hasDuplexSupport = false;

  info->probed = true;

  return;
}

bool RtApiDs :: probeDeviceOpen( int device, StreamMode mode, int channels, 
                                 int sampleRate, RtAudioFormat format,
                                 int *bufferSize, int numberOfBuffers)
{
  HRESULT result;
  HWND hWnd = GetForegroundWindow();

  // According to a note in PortAudio, using GetDesktopWindow()
  // instead of GetForegroundWindow() is supposed to avoid problems
  // that occur when the application's window is not the foreground
  // window.  Also, if the application window closes before the
  // DirectSound buffer, DirectSound can crash.  However, for console
  // applications, no sound was produced when using GetDesktopWindow().
  long buffer_size;
  LPVOID audioPtr;
  DWORD dataLen;
  int nBuffers;

  // Check the numberOfBuffers parameter and limit the lowest value to
  // two.  This is a judgement call and a value of two is probably too
  // low for capture, but it should work for playback.
  if (numberOfBuffers < 2)
    nBuffers = 2;
  else
    nBuffers = numberOfBuffers;

  // Define the wave format structure (16-bit PCM, srate, channels)
  WAVEFORMATEX waveFormat;
  ZeroMemory(&waveFormat, sizeof(WAVEFORMATEX));
  waveFormat.wFormatTag = WAVE_FORMAT_PCM;
  waveFormat.nChannels = channels;
  waveFormat.nSamplesPerSec = (unsigned long) sampleRate;

  // Determine the data format.
  if ( devices_[device].nativeFormats ) { // 8-bit and/or 16-bit support
    if ( format == RTAUDIO_SINT8 ) {
      if ( devices_[device].nativeFormats & RTAUDIO_SINT8 )
        waveFormat.wBitsPerSample = 8;
      else
        waveFormat.wBitsPerSample = 16;
    }
    else {
      if ( devices_[device].nativeFormats & RTAUDIO_SINT16 )
        waveFormat.wBitsPerSample = 16;
      else
        waveFormat.wBitsPerSample = 8;
    }
  }
  else {
    sprintf(message_, "RtApiDs: no reported data formats for device (%s).",
            devices_[device].name.c_str());
    error(RtError::DEBUG_WARNING);
    return FAILURE;
  }

  waveFormat.nBlockAlign = waveFormat.nChannels * waveFormat.wBitsPerSample / 8;
  waveFormat.nAvgBytesPerSec = waveFormat.nSamplesPerSec * waveFormat.nBlockAlign;

  enum_info dsinfo;
  void *ohandle = 0, *bhandle = 0;
  strncpy( dsinfo.name, devices_[device].name.c_str(), 64 );
  dsinfo.isValid = false;
  if ( mode == OUTPUT ) {

    if ( devices_[device].maxOutputChannels < channels ) {
      sprintf(message_, "RtApiDs: requested channels (%d) > than supported (%d) by device (%s).",
              channels, devices_[device].maxOutputChannels, devices_[device].name.c_str());
      error(RtError::DEBUG_WARNING);
      return FAILURE;
    }

    // Enumerate through output devices to find the id (if it exists).
    result = DirectSoundEnumerate((LPDSENUMCALLBACK)deviceIdCallback, &dsinfo);
    if ( FAILED(result) ) {
      sprintf(message_, "RtApiDs: Error performing output device id enumeration: %s.",
              getErrorString(result));
      error(RtError::DEBUG_WARNING);
      return FAILURE;
    }

    if ( dsinfo.isValid == false ) {
      sprintf(message_, "RtApiDs: output device (%s) id not found!", devices_[device].name.c_str());
      error(RtError::DEBUG_WARNING);
      return FAILURE;
    }

    LPGUID id = dsinfo.id;
    LPDIRECTSOUND  object;
    LPDIRECTSOUNDBUFFER buffer;
    DSBUFFERDESC bufferDescription;
    
    result = DirectSoundCreate( id, &object, NULL );
    if ( FAILED(result) ) {
      sprintf(message_, "RtApiDs: Could not create playback object (%s): %s.",
              devices_[device].name.c_str(), getErrorString(result));
      error(RtError::DEBUG_WARNING);
      return FAILURE;
    }

    // Set cooperative level to DSSCL_EXCLUSIVE
    result = object->SetCooperativeLevel(hWnd, DSSCL_EXCLUSIVE);
    if ( FAILED(result) ) {
      object->Release();
      sprintf(message_, "RtApiDs: Unable to set cooperative level (%s): %s.",
              devices_[device].name.c_str(), getErrorString(result));
      error(RtError::WARNING);
      return FAILURE;
    }

    // Even though we will write to the secondary buffer, we need to
    // access the primary buffer to set the correct output format
    // (since the default is 8-bit, 22 kHz!).  Setup the DS primary
    // buffer description.
    ZeroMemory(&bufferDescription, sizeof(DSBUFFERDESC));
    bufferDescription.dwSize = sizeof(DSBUFFERDESC);
    bufferDescription.dwFlags = DSBCAPS_PRIMARYBUFFER;
    // Obtain the primary buffer
    result = object->CreateSoundBuffer(&bufferDescription, &buffer, NULL);
    if ( FAILED(result) ) {
      object->Release();
      sprintf(message_, "RtApiDs: Unable to access primary buffer (%s): %s.",
              devices_[device].name.c_str(), getErrorString(result));
      error(RtError::WARNING);
      return FAILURE;
    }

    // Set the primary DS buffer sound format.
    result = buffer->SetFormat(&waveFormat);
    if ( FAILED(result) ) {
      object->Release();
      sprintf(message_, "RtApiDs: Unable to set primary buffer format (%s): %s.",
              devices_[device].name.c_str(), getErrorString(result));
      error(RtError::WARNING);
      return FAILURE;
    }

    // Setup the secondary DS buffer description.
    buffer_size = channels * *bufferSize * nBuffers * waveFormat.wBitsPerSample / 8;
    ZeroMemory(&bufferDescription, sizeof(DSBUFFERDESC));
    bufferDescription.dwSize = sizeof(DSBUFFERDESC);
    bufferDescription.dwFlags = ( DSBCAPS_STICKYFOCUS |
                                  DSBCAPS_GETCURRENTPOSITION2 |
                                  DSBCAPS_LOCHARDWARE );  // Force hardware mixing
    bufferDescription.dwBufferBytes = buffer_size;
    bufferDescription.lpwfxFormat = &waveFormat;

    // Try to create the secondary DS buffer.  If that doesn't work,
    // try to use software mixing.  Otherwise, there's a problem.
    result = object->CreateSoundBuffer(&bufferDescription, &buffer, NULL);
    if ( FAILED(result) ) {
      bufferDescription.dwFlags = ( DSBCAPS_STICKYFOCUS |
                                    DSBCAPS_GETCURRENTPOSITION2 |
                                    DSBCAPS_LOCSOFTWARE );  // Force software mixing
      result = object->CreateSoundBuffer(&bufferDescription, &buffer, NULL);
      if ( FAILED(result) ) {
        object->Release();
        sprintf(message_, "RtApiDs: Unable to create secondary DS buffer (%s): %s.",
                devices_[device].name.c_str(), getErrorString(result));
        error(RtError::WARNING);
        return FAILURE;
      }
    }

    // Get the buffer size ... might be different from what we specified.
    DSBCAPS dsbcaps;
    dsbcaps.dwSize = sizeof(DSBCAPS);
    buffer->GetCaps(&dsbcaps);
    buffer_size = dsbcaps.dwBufferBytes;

    // Lock the DS buffer
    result = buffer->Lock(0, buffer_size, &audioPtr, &dataLen, NULL, NULL, 0);
    if ( FAILED(result) ) {
      object->Release();
      buffer->Release();
      sprintf(message_, "RtApiDs: Unable to lock buffer (%s): %s.",
              devices_[device].name.c_str(), getErrorString(result));
      error(RtError::WARNING);
      return FAILURE;
    }

    // Zero the DS buffer
    ZeroMemory(audioPtr, dataLen);

    // Unlock the DS buffer
    result = buffer->Unlock(audioPtr, dataLen, NULL, 0);
    if ( FAILED(result) ) {
      object->Release();
      buffer->Release();
      sprintf(message_, "RtApiDs: Unable to unlock buffer(%s): %s.",
              devices_[device].name.c_str(), getErrorString(result));
      error(RtError::WARNING);
      return FAILURE;
    }

    ohandle = (void *) object;
    bhandle = (void *) buffer;
    stream_.nDeviceChannels[0] = channels;
  }

  if ( mode == INPUT ) {

    if ( devices_[device].maxInputChannels < channels )
      return FAILURE;

    // Enumerate through input devices to find the id (if it exists).
    result = DirectSoundCaptureEnumerate((LPDSENUMCALLBACK)deviceIdCallback, &dsinfo);
    if ( FAILED(result) ) {
      sprintf(message_, "RtApiDs: Error performing input device id enumeration: %s.",
              getErrorString(result));
      error(RtError::DEBUG_WARNING);
      return FAILURE;
    }

    if ( dsinfo.isValid == false ) {
      sprintf(message_, "RtAudioDS: input device (%s) id not found!", devices_[device].name.c_str());
      error(RtError::DEBUG_WARNING);
      return FAILURE;
    }

    LPGUID id = dsinfo.id;
    LPDIRECTSOUNDCAPTURE  object;
    LPDIRECTSOUNDCAPTUREBUFFER buffer;
    DSCBUFFERDESC bufferDescription;

    result = DirectSoundCaptureCreate( id, &object, NULL );
    if ( FAILED(result) ) {
      sprintf(message_, "RtApiDs: Could not create capture object (%s): %s.",
              devices_[device].name.c_str(), getErrorString(result));
      error(RtError::WARNING);
      return FAILURE;
    }

    // Setup the secondary DS buffer description.
    buffer_size = channels * *bufferSize * nBuffers * waveFormat.wBitsPerSample / 8;
    ZeroMemory(&bufferDescription, sizeof(DSCBUFFERDESC));
    bufferDescription.dwSize = sizeof(DSCBUFFERDESC);
    bufferDescription.dwFlags = 0;
    bufferDescription.dwReserved = 0;
    bufferDescription.dwBufferBytes = buffer_size;
    bufferDescription.lpwfxFormat = &waveFormat;

    // Create the capture buffer.
    result = object->CreateCaptureBuffer(&bufferDescription, &buffer, NULL);
    if ( FAILED(result) ) {
      object->Release();
      sprintf(message_, "RtApiDs: Unable to create capture buffer (%s): %s.",
              devices_[device].name.c_str(), getErrorString(result));
      error(RtError::WARNING);
      return FAILURE;
    }

    // Lock the capture buffer
    result = buffer->Lock(0, buffer_size, &audioPtr, &dataLen, NULL, NULL, 0);
    if ( FAILED(result) ) {
      object->Release();
      buffer->Release();
      sprintf(message_, "RtApiDs: Unable to lock capture buffer (%s): %s.",
              devices_[device].name.c_str(), getErrorString(result));
      error(RtError::WARNING);
      return FAILURE;
    }

    // Zero the buffer
    ZeroMemory(audioPtr, dataLen);

    // Unlock the buffer
    result = buffer->Unlock(audioPtr, dataLen, NULL, 0);
    if ( FAILED(result) ) {
      object->Release();
      buffer->Release();
      sprintf(message_, "RtApiDs: Unable to unlock capture buffer (%s): %s.",
              devices_[device].name.c_str(), getErrorString(result));
      error(RtError::WARNING);
      return FAILURE;
    }

    ohandle = (void *) object;
    bhandle = (void *) buffer;
    stream_.nDeviceChannels[1] = channels;
  }

  stream_.userFormat = format;
  if ( waveFormat.wBitsPerSample == 8 )
    stream_.deviceFormat[mode] = RTAUDIO_SINT8;
  else
    stream_.deviceFormat[mode] = RTAUDIO_SINT16;
  stream_.nUserChannels[mode] = channels;
  *bufferSize = buffer_size / (channels * nBuffers * waveFormat.wBitsPerSample / 8);
  stream_.bufferSize = *bufferSize;

  // Set flags for buffer conversion
  stream_.doConvertBuffer[mode] = false;
  if (stream_.userFormat != stream_.deviceFormat[mode])
    stream_.doConvertBuffer[mode] = true;
  if (stream_.nUserChannels[mode] < stream_.nDeviceChannels[mode])
    stream_.doConvertBuffer[mode] = true;

  // Allocate necessary internal buffers
  if ( stream_.nUserChannels[0] != stream_.nUserChannels[1] ) {

    long buffer_bytes;
    if (stream_.nUserChannels[0] >= stream_.nUserChannels[1])
      buffer_bytes = stream_.nUserChannels[0];
    else
      buffer_bytes = stream_.nUserChannels[1];

    buffer_bytes *= *bufferSize * formatBytes(stream_.userFormat);
    if (stream_.userBuffer) free(stream_.userBuffer);
    stream_.userBuffer = (char *) calloc(buffer_bytes, 1);
    if (stream_.userBuffer == NULL) {
      sprintf(message_, "RtApiDs: error allocating user buffer memory (%s).",
              devices_[device].name.c_str());
      goto error;
    }
  }

  if ( stream_.doConvertBuffer[mode] ) {

    long buffer_bytes;
    bool makeBuffer = true;
    if ( mode == OUTPUT )
      buffer_bytes = stream_.nDeviceChannels[0] * formatBytes(stream_.deviceFormat[0]);
    else { // mode == INPUT
      buffer_bytes = stream_.nDeviceChannels[1] * formatBytes(stream_.deviceFormat[1]);
      if ( stream_.mode == OUTPUT && stream_.deviceBuffer ) {
        long bytes_out = stream_.nDeviceChannels[0] * formatBytes(stream_.deviceFormat[0]);
        if ( buffer_bytes < bytes_out ) makeBuffer = false;
      }
    }

    if ( makeBuffer ) {
      buffer_bytes *= *bufferSize;
      if (stream_.deviceBuffer) free(stream_.deviceBuffer);
      stream_.deviceBuffer = (char *) calloc(buffer_bytes, 1);
      if (stream_.deviceBuffer == NULL) {
        sprintf(message_, "RtApiDs: error allocating device buffer memory (%s).",
                devices_[device].name.c_str());
        goto error;
      }
    }
  }

  // Allocate our DsHandle structures for the stream.
  DsHandle *handles;
  if ( stream_.apiHandle == 0 ) {
    handles = (DsHandle *) calloc(2, sizeof(DsHandle));
    if ( handles == NULL ) {
      sprintf(message_, "RtApiDs: Error allocating DsHandle memory (%s).",
              devices_[device].name.c_str());
      goto error;
    }
    handles[0].object = 0;
    handles[1].object = 0;
    stream_.apiHandle = (void *) handles;
  }
  else
    handles = (DsHandle *) stream_.apiHandle;
  handles[mode].object = ohandle;
  handles[mode].buffer = bhandle;

  stream_.device[mode] = device;
  stream_.state = STREAM_STOPPED;
  if ( stream_.mode == OUTPUT && mode == INPUT )
    // We had already set up an output stream.
    stream_.mode = DUPLEX;
  else
    stream_.mode = mode;
  stream_.nBuffers = nBuffers;
  stream_.sampleRate = sampleRate;

  return SUCCESS;

 error:
  if (handles) {
    if (handles[0].object) {
      LPDIRECTSOUND object = (LPDIRECTSOUND) handles[0].object;
      LPDIRECTSOUNDBUFFER buffer = (LPDIRECTSOUNDBUFFER) handles[0].buffer;
      if (buffer) buffer->Release();
      object->Release();
    }
    if (handles[1].object) {
      LPDIRECTSOUNDCAPTURE object = (LPDIRECTSOUNDCAPTURE) handles[1].object;
      LPDIRECTSOUNDCAPTUREBUFFER buffer = (LPDIRECTSOUNDCAPTUREBUFFER) handles[1].buffer;
      if (buffer) buffer->Release();
      object->Release();
    }
    free(handles);
    stream_.apiHandle = 0;
  }

  if (stream_.userBuffer) {
    free(stream_.userBuffer);
    stream_.userBuffer = 0;
  }

  error(RtError::WARNING);
  return FAILURE;
}

void RtApiDs :: setStreamCallback(RtAudioCallback callback, void *userData)
{
  verifyStream();

  CallbackInfo *info = (CallbackInfo *) &stream_.callbackInfo;
  if ( info->usingCallback ) {
    sprintf(message_, "RtApiDs: A callback is already set for this stream!");
    error(RtError::WARNING);
    return;
  }

  info->callback = (void *) callback;
  info->userData = userData;
  info->usingCallback = true;
  info->object = (void *) this;

  unsigned thread_id;
  info->thread = _beginthreadex(NULL, 0, &callbackHandler,
                                &stream_.callbackInfo, 0, &thread_id);
  if (info->thread == 0) {
    info->usingCallback = false;
    sprintf(message_, "RtApiDs: error starting callback thread!");
    error(RtError::THREAD_ERROR);
  }

  // When spawning multiple threads in quick succession, it appears to be
  // necessary to wait a bit for each to initialize ... another windoism!
  Sleep(1);
}

void RtApiDs :: cancelStreamCallback()
{
  verifyStream();

  if (stream_.callbackInfo.usingCallback) {

    if (stream_.state == STREAM_RUNNING)
      stopStream();

    MUTEX_LOCK(&stream_.mutex);

    stream_.callbackInfo.usingCallback = false;
    WaitForSingleObject( (HANDLE)stream_.callbackInfo.thread, INFINITE );
    CloseHandle( (HANDLE)stream_.callbackInfo.thread );
    stream_.callbackInfo.thread = 0;
    stream_.callbackInfo.callback = NULL;
    stream_.callbackInfo.userData = NULL;

    MUTEX_UNLOCK(&stream_.mutex);
  }
}

void RtApiDs :: closeStream()
{
  // We don't want an exception to be thrown here because this
  // function is called by our class destructor.  So, do our own
  // streamId check.
  if ( stream_.mode == UNINITIALIZED ) {
    sprintf(message_, "RtApiDs::closeStream(): no open stream to close!");
    error(RtError::WARNING);
    return;
  }

  if (stream_.callbackInfo.usingCallback) {
    stream_.callbackInfo.usingCallback = false;
    WaitForSingleObject( (HANDLE)stream_.callbackInfo.thread, INFINITE );
    CloseHandle( (HANDLE)stream_.callbackInfo.thread );
  }

  DsHandle *handles = (DsHandle *) stream_.apiHandle;
  if (handles) {
    if (handles[0].object) {
      LPDIRECTSOUND object = (LPDIRECTSOUND) handles[0].object;
      LPDIRECTSOUNDBUFFER buffer = (LPDIRECTSOUNDBUFFER) handles[0].buffer;
      if (buffer) {
        buffer->Stop();
        buffer->Release();
      }
      object->Release();
    }

    if (handles[1].object) {
      LPDIRECTSOUNDCAPTURE object = (LPDIRECTSOUNDCAPTURE) handles[1].object;
      LPDIRECTSOUNDCAPTUREBUFFER buffer = (LPDIRECTSOUNDCAPTUREBUFFER) handles[1].buffer;
      if (buffer) {
        buffer->Stop();
        buffer->Release();
      }
      object->Release();
    }
    free(handles);
    stream_.apiHandle = 0;
  }
    
  if (stream_.userBuffer) {
    free(stream_.userBuffer);
    stream_.userBuffer = 0;
  }

  if (stream_.deviceBuffer) {
    free(stream_.deviceBuffer);
    stream_.deviceBuffer = 0;
  }

  stream_.mode = UNINITIALIZED;
}

void RtApiDs :: startStream()
{
  verifyStream();
  if (stream_.state == STREAM_RUNNING) return;

  MUTEX_LOCK(&stream_.mutex);

  HRESULT result;
  DsHandle *handles = (DsHandle *) stream_.apiHandle;
  if (stream_.mode == OUTPUT || stream_.mode == DUPLEX) {
    LPDIRECTSOUNDBUFFER buffer = (LPDIRECTSOUNDBUFFER) handles[0].buffer;
    result = buffer->Play(0, 0, DSBPLAY_LOOPING );
    if ( FAILED(result) ) {
      sprintf(message_, "RtApiDs: Unable to start buffer (%s): %s.",
              devices_[stream_.device[0]].name.c_str(), getErrorString(result));
      error(RtError::DRIVER_ERROR);
    }
  }

  if (stream_.mode == INPUT || stream_.mode == DUPLEX) {
    LPDIRECTSOUNDCAPTUREBUFFER buffer = (LPDIRECTSOUNDCAPTUREBUFFER) handles[1].buffer;
    result = buffer->Start(DSCBSTART_LOOPING );
    if ( FAILED(result) ) {
      sprintf(message_, "RtApiDs: Unable to start capture buffer (%s): %s.",
              devices_[stream_.device[1]].name.c_str(), getErrorString(result));
      error(RtError::DRIVER_ERROR);
    }
  }
  stream_.state = STREAM_RUNNING;

  MUTEX_UNLOCK(&stream_.mutex);
}

void RtApiDs :: stopStream()
{
  verifyStream();
  if (stream_.state == STREAM_STOPPED) return;

  // Change the state before the lock to improve shutdown response
  // when using a callback.
  stream_.state = STREAM_STOPPED;
  MUTEX_LOCK(&stream_.mutex);

  // There is no specific DirectSound API call to "drain" a buffer
  // before stopping.  We can hack this for playback by writing zeroes
  // for another bufferSize * nBuffers frames.  For capture, the
  // concept is less clear so we'll repeat what we do in the
  // abortStream() case.
  HRESULT result;
  DWORD dsBufferSize;
  LPVOID buffer1 = NULL;
  LPVOID buffer2 = NULL;
  DWORD bufferSize1 = 0;
  DWORD bufferSize2 = 0;
  DsHandle *handles = (DsHandle *) stream_.apiHandle;
  if (stream_.mode == OUTPUT || stream_.mode == DUPLEX) {

    DWORD currentPos, safePos;
    long buffer_bytes = stream_.bufferSize * stream_.nDeviceChannels[0];
    buffer_bytes *= formatBytes(stream_.deviceFormat[0]);

    LPDIRECTSOUNDBUFFER dsBuffer = (LPDIRECTSOUNDBUFFER) handles[0].buffer;
    UINT nextWritePos = handles[0].bufferPointer;
    dsBufferSize = buffer_bytes * stream_.nBuffers;

    // Write zeroes for nBuffer counts.
    for (int i=0; i<stream_.nBuffers; i++) {

      // Find out where the read and "safe write" pointers are.
      result = dsBuffer->GetCurrentPosition(&currentPos, &safePos);
      if ( FAILED(result) ) {
        sprintf(message_, "RtApiDs: Unable to get current position (%s): %s.",
                devices_[stream_.device[0]].name.c_str(), getErrorString(result));
        error(RtError::DRIVER_ERROR);
      }

      if ( currentPos < nextWritePos ) currentPos += dsBufferSize; // unwrap offset
      DWORD endWrite = nextWritePos + buffer_bytes;

      // Check whether the entire write region is behind the play pointer.
      while ( currentPos < endWrite ) {
        double millis = (endWrite - currentPos) * 900.0;
        millis /= ( formatBytes(stream_.deviceFormat[0]) * stream_.sampleRate);
        if ( millis < 1.0 ) millis = 1.0;
        Sleep( (DWORD) millis );

        // Wake up, find out where we are now
        result = dsBuffer->GetCurrentPosition( &currentPos, &safePos );
        if ( FAILED(result) ) {
          sprintf(message_, "RtApiDs: Unable to get current position (%s): %s.",
                  devices_[stream_.device[0]].name.c_str(), getErrorString(result));
          error(RtError::DRIVER_ERROR);
        }
        if ( currentPos < nextWritePos ) currentPos += dsBufferSize; // unwrap offset
      }

      // Lock free space in the buffer
      result = dsBuffer->Lock (nextWritePos, buffer_bytes, &buffer1,
                               &bufferSize1, &buffer2, &bufferSize2, 0);
      if ( FAILED(result) ) {
        sprintf(message_, "RtApiDs: Unable to lock buffer during playback (%s): %s.",
                devices_[stream_.device[0]].name.c_str(), getErrorString(result));
        error(RtError::DRIVER_ERROR);
      }

      // Zero the free space
      ZeroMemory(buffer1, bufferSize1);
      if (buffer2 != NULL) ZeroMemory(buffer2, bufferSize2);

      // Update our buffer offset and unlock sound buffer
      dsBuffer->Unlock (buffer1, bufferSize1, buffer2, bufferSize2);
      if ( FAILED(result) ) {
        sprintf(message_, "RtApiDs: Unable to unlock buffer during playback (%s): %s.",
                devices_[stream_.device[0]].name.c_str(), getErrorString(result));
        error(RtError::DRIVER_ERROR);
      }
      nextWritePos = (nextWritePos + bufferSize1 + bufferSize2) % dsBufferSize;
      handles[0].bufferPointer = nextWritePos;
    }

    // If we play again, start at the beginning of the buffer.
    handles[0].bufferPointer = 0;
  }

  if (stream_.mode == INPUT || stream_.mode == DUPLEX) {
    LPDIRECTSOUNDCAPTUREBUFFER buffer = (LPDIRECTSOUNDCAPTUREBUFFER) handles[1].buffer;
    buffer1 = NULL;
    bufferSize1 = 0;

    result = buffer->Stop();
    if ( FAILED(result) ) {
      sprintf(message_, "RtApiDs: Unable to stop capture buffer (%s): %s",
              devices_[stream_.device[1]].name.c_str(), getErrorString(result));
      error(RtError::DRIVER_ERROR);
    }

    dsBufferSize = stream_.bufferSize * stream_.nDeviceChannels[1];
    dsBufferSize *= formatBytes(stream_.deviceFormat[1]) * stream_.nBuffers;

    // Lock the buffer and clear it so that if we start to play again,
    // we won't have old data playing.
    result = buffer->Lock(0, dsBufferSize, &buffer1, &bufferSize1, NULL, NULL, 0);
    if ( FAILED(result) ) {
      sprintf(message_, "RtApiDs: Unable to lock capture buffer (%s): %s.",
              devices_[stream_.device[1]].name.c_str(), getErrorString(result));
      error(RtError::DRIVER_ERROR);
    }

    // Zero the DS buffer
    ZeroMemory(buffer1, bufferSize1);

    // Unlock the DS buffer
    result = buffer->Unlock(buffer1, bufferSize1, NULL, 0);
    if ( FAILED(result) ) {
      sprintf(message_, "RtApiDs: Unable to unlock capture buffer (%s): %s.",
              devices_[stream_.device[1]].name.c_str(), getErrorString(result));
      error(RtError::DRIVER_ERROR);
    }

    // If we start recording again, we must begin at beginning of buffer.
    handles[1].bufferPointer = 0;
  }

  MUTEX_UNLOCK(&stream_.mutex);
}

void RtApiDs :: abortStream()
{
  verifyStream();
  if (stream_.state == STREAM_STOPPED) return;

  // Change the state before the lock to improve shutdown response
  // when using a callback.
  stream_.state = STREAM_STOPPED;
  MUTEX_LOCK(&stream_.mutex);

  HRESULT result;
  long dsBufferSize;
  LPVOID audioPtr;
  DWORD dataLen;
  DsHandle *handles = (DsHandle *) stream_.apiHandle;
  if (stream_.mode == OUTPUT || stream_.mode == DUPLEX) {
    LPDIRECTSOUNDBUFFER buffer = (LPDIRECTSOUNDBUFFER) handles[0].buffer;
    result = buffer->Stop();
    if ( FAILED(result) ) {
      sprintf(message_, "RtApiDs: Unable to stop buffer (%s): %s",
              devices_[stream_.device[0]].name.c_str(), getErrorString(result));
      error(RtError::DRIVER_ERROR);
    }

    dsBufferSize = stream_.bufferSize * stream_.nDeviceChannels[0];
    dsBufferSize *= formatBytes(stream_.deviceFormat[0]) * stream_.nBuffers;

    // Lock the buffer and clear it so that if we start to play again,
    // we won't have old data playing.
    result = buffer->Lock(0, dsBufferSize, &audioPtr, &dataLen, NULL, NULL, 0);
    if ( FAILED(result) ) {
      sprintf(message_, "RtApiDs: Unable to lock buffer (%s): %s.",
              devices_[stream_.device[0]].name.c_str(), getErrorString(result));
      error(RtError::DRIVER_ERROR);
    }

    // Zero the DS buffer
    ZeroMemory(audioPtr, dataLen);

    // Unlock the DS buffer
    result = buffer->Unlock(audioPtr, dataLen, NULL, 0);
    if ( FAILED(result) ) {
      sprintf(message_, "RtApiDs: Unable to unlock buffer (%s): %s.",
              devices_[stream_.device[0]].name.c_str(), getErrorString(result));
      error(RtError::DRIVER_ERROR);
    }

    // If we start playing again, we must begin at beginning of buffer.
    handles[0].bufferPointer = 0;
  }

  if (stream_.mode == INPUT || stream_.mode == DUPLEX) {
    LPDIRECTSOUNDCAPTUREBUFFER buffer = (LPDIRECTSOUNDCAPTUREBUFFER) handles[1].buffer;
    audioPtr = NULL;
    dataLen = 0;

    result = buffer->Stop();
    if ( FAILED(result) ) {
      sprintf(message_, "RtApiDs: Unable to stop capture buffer (%s): %s",
              devices_[stream_.device[1]].name.c_str(), getErrorString(result));
      error(RtError::DRIVER_ERROR);
    }

    dsBufferSize = stream_.bufferSize * stream_.nDeviceChannels[1];
    dsBufferSize *= formatBytes(stream_.deviceFormat[1]) * stream_.nBuffers;

    // Lock the buffer and clear it so that if we start to play again,
    // we won't have old data playing.
    result = buffer->Lock(0, dsBufferSize, &audioPtr, &dataLen, NULL, NULL, 0);
    if ( FAILED(result) ) {
      sprintf(message_, "RtApiDs: Unable to lock capture buffer (%s): %s.",
              devices_[stream_.device[1]].name.c_str(), getErrorString(result));
      error(RtError::DRIVER_ERROR);
    }

    // Zero the DS buffer
    ZeroMemory(audioPtr, dataLen);

    // Unlock the DS buffer
    result = buffer->Unlock(audioPtr, dataLen, NULL, 0);
    if ( FAILED(result) ) {
      sprintf(message_, "RtApiDs: Unable to unlock capture buffer (%s): %s.",
              devices_[stream_.device[1]].name.c_str(), getErrorString(result));
      error(RtError::DRIVER_ERROR);
    }

    // If we start recording again, we must begin at beginning of buffer.
    handles[1].bufferPointer = 0;
  }

  MUTEX_UNLOCK(&stream_.mutex);
}

int RtApiDs :: streamWillBlock()
{
  verifyStream();
  if (stream_.state == STREAM_STOPPED) return 0;

  MUTEX_LOCK(&stream_.mutex);

  int channels;
  int frames = 0;
  HRESULT result;
  DWORD currentPos, safePos;
  channels = 1;
  DsHandle *handles = (DsHandle *) stream_.apiHandle;
  if (stream_.mode == OUTPUT || stream_.mode == DUPLEX) {

    LPDIRECTSOUNDBUFFER dsBuffer = (LPDIRECTSOUNDBUFFER) handles[0].buffer;
    UINT nextWritePos = handles[0].bufferPointer;
    channels = stream_.nDeviceChannels[0];
    DWORD dsBufferSize = stream_.bufferSize * channels;
    dsBufferSize *= formatBytes(stream_.deviceFormat[0]) * stream_.nBuffers;

    // Find out where the read and "safe write" pointers are.
    result = dsBuffer->GetCurrentPosition(&currentPos, &safePos);
    if ( FAILED(result) ) {
      sprintf(message_, "RtApiDs: Unable to get current position (%s): %s.",
              devices_[stream_.device[0]].name.c_str(), getErrorString(result));
      error(RtError::DRIVER_ERROR);
    }

    if ( currentPos < nextWritePos ) currentPos += dsBufferSize; // unwrap offset
    frames = currentPos - nextWritePos;
    frames /= channels * formatBytes(stream_.deviceFormat[0]);
  }

  if (stream_.mode == INPUT || stream_.mode == DUPLEX) {

    LPDIRECTSOUNDCAPTUREBUFFER dsBuffer = (LPDIRECTSOUNDCAPTUREBUFFER) handles[1].buffer;
    UINT nextReadPos = handles[1].bufferPointer;
    channels = stream_.nDeviceChannels[1];
    DWORD dsBufferSize = stream_.bufferSize * channels;
    dsBufferSize *= formatBytes(stream_.deviceFormat[1]) * stream_.nBuffers;

    // Find out where the write and "safe read" pointers are.
    result = dsBuffer->GetCurrentPosition(&currentPos, &safePos);
    if ( FAILED(result) ) {
      sprintf(message_, "RtApiDs: Unable to get current capture position (%s): %s.",
              devices_[stream_.device[1]].name.c_str(), getErrorString(result));
      error(RtError::DRIVER_ERROR);
    }

    if ( safePos < nextReadPos ) safePos += dsBufferSize; // unwrap offset

    if (stream_.mode == DUPLEX ) {
      // Take largest value of the two.
      int temp = safePos - nextReadPos;
      temp /= channels * formatBytes(stream_.deviceFormat[1]);
      frames = ( temp > frames ) ? temp : frames;
    }
    else {
      frames = safePos - nextReadPos;
      frames /= channels * formatBytes(stream_.deviceFormat[1]);
    }
  }

  frames = stream_.bufferSize - frames;
  if (frames < 0) frames = 0;

  MUTEX_UNLOCK(&stream_.mutex);
  return frames;
}

void RtApiDs :: tickStream()
{
  verifyStream();

  int stopStream = 0;
  if (stream_.state == STREAM_STOPPED) {
    if (stream_.callbackInfo.usingCallback) Sleep(50); // sleep 50 milliseconds
    return;
  }
  else if (stream_.callbackInfo.usingCallback) {
    RtAudioCallback callback = (RtAudioCallback) stream_.callbackInfo.callback;
    stopStream = callback(stream_.userBuffer, stream_.bufferSize, stream_.callbackInfo.userData);
  }

  MUTEX_LOCK(&stream_.mutex);

  // The state might change while waiting on a mutex.
  if (stream_.state == STREAM_STOPPED) {
    MUTEX_UNLOCK(&stream_.mutex);
    return;
  }

  HRESULT result;
  DWORD currentPos, safePos;
  LPVOID buffer1 = NULL;
  LPVOID buffer2 = NULL;
  DWORD bufferSize1 = 0;
  DWORD bufferSize2 = 0;
  char *buffer;
  long buffer_bytes;
  DsHandle *handles = (DsHandle *) stream_.apiHandle;
  if (stream_.mode == OUTPUT || stream_.mode == DUPLEX) {

    // Setup parameters and do buffer conversion if necessary.
    if (stream_.doConvertBuffer[0]) {
      convertStreamBuffer(OUTPUT);
      buffer = stream_.deviceBuffer;
      buffer_bytes = stream_.bufferSize * stream_.nDeviceChannels[0];
      buffer_bytes *= formatBytes(stream_.deviceFormat[0]);
    }
    else {
      buffer = stream_.userBuffer;
      buffer_bytes = stream_.bufferSize * stream_.nUserChannels[0];
      buffer_bytes *= formatBytes(stream_.userFormat);
    }

    // No byte swapping necessary in DirectSound implementation.

    LPDIRECTSOUNDBUFFER dsBuffer = (LPDIRECTSOUNDBUFFER) handles[0].buffer;
    UINT nextWritePos = handles[0].bufferPointer;
    DWORD dsBufferSize = buffer_bytes * stream_.nBuffers;

    // Find out where the read and "safe write" pointers are.
    result = dsBuffer->GetCurrentPosition(&currentPos, &safePos);
    if ( FAILED(result) ) {
      sprintf(message_, "RtApiDs: Unable to get current position (%s): %s.",
              devices_[stream_.device[0]].name.c_str(), getErrorString(result));
      error(RtError::DRIVER_ERROR);
    }

    if ( currentPos < nextWritePos ) currentPos += dsBufferSize; // unwrap offset
    DWORD endWrite = nextWritePos + buffer_bytes;

    // Check whether the entire write region is behind the play pointer.
    while ( currentPos < endWrite ) {
      // If we are here, then we must wait until the play pointer gets
      // beyond the write region.  The approach here is to use the
      // Sleep() function to suspend operation until safePos catches
      // up. Calculate number of milliseconds to wait as:
      //   time = distance * (milliseconds/second) * fudgefactor /
      //          ((bytes/sample) * (samples/second))
      // A "fudgefactor" less than 1 is used because it was found
      // that sleeping too long was MUCH worse than sleeping for
      // several shorter periods.
      double millis = (endWrite - currentPos) * 900.0;
      millis /= ( formatBytes(stream_.deviceFormat[0]) * stream_.sampleRate);
      if ( millis < 1.0 ) millis = 1.0;
      Sleep( (DWORD) millis );

      // Wake up, find out where we are now
      result = dsBuffer->GetCurrentPosition( &currentPos, &safePos );
      if ( FAILED(result) ) {
        sprintf(message_, "RtApiDs: Unable to get current position (%s): %s.",
              devices_[stream_.device[0]].name.c_str(), getErrorString(result));
        error(RtError::DRIVER_ERROR);
      }
      if ( currentPos < nextWritePos ) currentPos += dsBufferSize; // unwrap offset
    }

    // Lock free space in the buffer
    result = dsBuffer->Lock (nextWritePos, buffer_bytes, &buffer1,
                             &bufferSize1, &buffer2, &bufferSize2, 0);
    if ( FAILED(result) ) {
      sprintf(message_, "RtApiDs: Unable to lock buffer during playback (%s): %s.",
              devices_[stream_.device[0]].name.c_str(), getErrorString(result));
      error(RtError::DRIVER_ERROR);
    }

    // Copy our buffer into the DS buffer
    CopyMemory(buffer1, buffer, bufferSize1);
    if (buffer2 != NULL) CopyMemory(buffer2, buffer+bufferSize1, bufferSize2);

    // Update our buffer offset and unlock sound buffer
    dsBuffer->Unlock (buffer1, bufferSize1, buffer2, bufferSize2);
    if ( FAILED(result) ) {
      sprintf(message_, "RtApiDs: Unable to unlock buffer during playback (%s): %s.",
              devices_[stream_.device[0]].name.c_str(), getErrorString(result));
      error(RtError::DRIVER_ERROR);
    }
    nextWritePos = (nextWritePos + bufferSize1 + bufferSize2) % dsBufferSize;
    handles[0].bufferPointer = nextWritePos;
  }

  if (stream_.mode == INPUT || stream_.mode == DUPLEX) {

    // Setup parameters.
    if (stream_.doConvertBuffer[1]) {
      buffer = stream_.deviceBuffer;
      buffer_bytes = stream_.bufferSize * stream_.nDeviceChannels[1];
      buffer_bytes *= formatBytes(stream_.deviceFormat[1]);
    }
    else {
      buffer = stream_.userBuffer;
      buffer_bytes = stream_.bufferSize * stream_.nUserChannels[1];
      buffer_bytes *= formatBytes(stream_.userFormat);
    }

    LPDIRECTSOUNDCAPTUREBUFFER dsBuffer = (LPDIRECTSOUNDCAPTUREBUFFER) handles[1].buffer;
    UINT nextReadPos = handles[1].bufferPointer;
    DWORD dsBufferSize = buffer_bytes * stream_.nBuffers;

    // Find out where the write and "safe read" pointers are.
    result = dsBuffer->GetCurrentPosition(&currentPos, &safePos);
    if ( FAILED(result) ) {
      sprintf(message_, "RtApiDs: Unable to get current capture position (%s): %s.",
              devices_[stream_.device[1]].name.c_str(), getErrorString(result));
      error(RtError::DRIVER_ERROR);
    }

    if ( safePos < nextReadPos ) safePos += dsBufferSize; // unwrap offset
    DWORD endRead = nextReadPos + buffer_bytes;

    // Check whether the entire write region is behind the play pointer.
    while ( safePos < endRead ) {
      // See comments for playback.
      double millis = (endRead - safePos) * 900.0;
      millis /= ( formatBytes(stream_.deviceFormat[1]) * stream_.sampleRate);
      if ( millis < 1.0 ) millis = 1.0;
      Sleep( (DWORD) millis );

      // Wake up, find out where we are now
      result = dsBuffer->GetCurrentPosition( &currentPos, &safePos );
      if ( FAILED(result) ) {
        sprintf(message_, "RtApiDs: Unable to get current capture position (%s): %s.",
                devices_[stream_.device[1]].name.c_str(), getErrorString(result));
        error(RtError::DRIVER_ERROR);
      }
      
      if ( safePos < nextReadPos ) safePos += dsBufferSize; // unwrap offset
    }

    // Lock free space in the buffer
    result = dsBuffer->Lock (nextReadPos, buffer_bytes, &buffer1,
                             &bufferSize1, &buffer2, &bufferSize2, 0);
    if ( FAILED(result) ) {
      sprintf(message_, "RtApiDs: Unable to lock buffer during capture (%s): %s.",
              devices_[stream_.device[1]].name.c_str(), getErrorString(result));
      error(RtError::DRIVER_ERROR);
    }

    // Copy our buffer into the DS buffer
    CopyMemory(buffer, buffer1, bufferSize1);
    if (buffer2 != NULL) CopyMemory(buffer+bufferSize1, buffer2, bufferSize2);

    // Update our buffer offset and unlock sound buffer
    nextReadPos = (nextReadPos + bufferSize1 + bufferSize2) % dsBufferSize;
    dsBuffer->Unlock (buffer1, bufferSize1, buffer2, bufferSize2);
    if ( FAILED(result) ) {
      sprintf(message_, "RtApiDs: Unable to unlock buffer during capture (%s): %s.",
              devices_[stream_.device[1]].name.c_str(), getErrorString(result));
      error(RtError::DRIVER_ERROR);
    }
    handles[1].bufferPointer = nextReadPos;

    // No byte swapping necessary in DirectSound implementation.

    // Do buffer conversion if necessary.
    if (stream_.doConvertBuffer[1])
      convertStreamBuffer(INPUT);
  }

  MUTEX_UNLOCK(&stream_.mutex);

  if (stream_.callbackInfo.usingCallback && stopStream)
    this->stopStream();

  RtApi::tickStream();
}

// Definitions for utility functions and callbacks
// specific to the DirectSound implementation.

extern "C" unsigned __stdcall callbackHandler(void *ptr)
{
  CallbackInfo *info = (CallbackInfo *) ptr;
  RtApiDs *object = (RtApiDs *) info->object;
  bool *usingCallback = &info->usingCallback;

  while ( *usingCallback ) {
    try {
      object->tickStream();
    }
    catch (RtError &exception) {
      fprintf(stderr, "\nRtApiDs: callback thread error (%s) ... closing thread.\n\n",
              exception.getMessageString());
      break;
    }
  }

  _endthreadex( 0 );
  return 0;
}

static bool CALLBACK deviceCountCallback(LPGUID lpguid,
                                         LPCSTR lpcstrDescription,
                                         LPCSTR lpcstrModule,
                                         LPVOID lpContext)
{
  int *pointer = ((int *) lpContext);
  (*pointer)++;

  return true;
}

static bool CALLBACK deviceInfoCallback(LPGUID lpguid,
                                        LPCSTR lpcstrDescription,
                                        LPCSTR lpcstrModule,
                                        LPVOID lpContext)
{
  enum_info *info = ((enum_info *) lpContext);
  while (strlen(info->name) > 0) info++;

  strncpy(info->name, lpcstrDescription, 64);
  info->id = lpguid;

	HRESULT    hr;
  info->isValid = false;
  if (info->isInput == true) {
    DSCCAPS               caps;
    LPDIRECTSOUNDCAPTURE  object;

    hr = DirectSoundCaptureCreate(  lpguid, &object,   NULL );
    if( hr != DS_OK ) return true;

    caps.dwSize = sizeof(caps);
    hr = object->GetCaps( &caps );
    if( hr == DS_OK ) {
      if (caps.dwChannels > 0 && caps.dwFormats > 0)
        info->isValid = true;
    }
    object->Release();
  }
  else {
    DSCAPS         caps;
    LPDIRECTSOUND  object;
    hr = DirectSoundCreate(  lpguid, &object,   NULL );
    if( hr != DS_OK ) return true;

    caps.dwSize = sizeof(caps);
    hr = object->GetCaps( &caps );
    if( hr == DS_OK ) {
      if ( caps.dwFlags & DSCAPS_PRIMARYMONO || caps.dwFlags & DSCAPS_PRIMARYSTEREO )
        info->isValid = true;
    }
    object->Release();
  }

  return true;
}

static bool CALLBACK defaultDeviceCallback(LPGUID lpguid,
                                           LPCSTR lpcstrDescription,
                                           LPCSTR lpcstrModule,
                                           LPVOID lpContext)
{
  enum_info *info = ((enum_info *) lpContext);

  if ( lpguid == NULL ) {
    strncpy(info->name, lpcstrDescription, 64);
    return false;
  }

  return true;
}

static bool CALLBACK deviceIdCallback(LPGUID lpguid,
                                      LPCSTR lpcstrDescription,
                                      LPCSTR lpcstrModule,
                                      LPVOID lpContext)
{
  enum_info *info = ((enum_info *) lpContext);

  if ( strncmp( info->name, lpcstrDescription, 64 ) == 0 ) {
    info->id = lpguid;
    info->isValid = true;
    return false;
  }

  return true;
}

static char* getErrorString(int code)
{
	switch (code) {

  case DSERR_ALLOCATED:
    return "Direct Sound already allocated";

  case DSERR_CONTROLUNAVAIL:
    return "Direct Sound control unavailable";

  case DSERR_INVALIDPARAM:
    return "Direct Sound invalid parameter";

  case DSERR_INVALIDCALL:
    return "Direct Sound invalid call";

  case DSERR_GENERIC:
    return "Direct Sound generic error";

  case DSERR_PRIOLEVELNEEDED:
    return "Direct Sound Priority level needed";

  case DSERR_OUTOFMEMORY:
    return "Direct Sound out of memory";

  case DSERR_BADFORMAT:
    return "Direct Sound bad format";

  case DSERR_UNSUPPORTED:
    return "Direct Sound unsupported error";

  case DSERR_NODRIVER:
    return "Direct Sound no driver error";

  case DSERR_ALREADYINITIALIZED:
    return "Direct Sound already initialized";

  case DSERR_NOAGGREGATION:
    return "Direct Sound no aggregation";

  case DSERR_BUFFERLOST:
    return "Direct Sound buffer lost";

  case DSERR_OTHERAPPHASPRIO:
    return "Direct Sound other app has priority";

  case DSERR_UNINITIALIZED:
    return "Direct Sound uninitialized";

  default:
    return "Direct Sound unknown error";
	}
}

//******************** End of __WINDOWS_DS__ *********************//
#endif

#if defined(__IRIX_AL__) // SGI's AL API for IRIX

#include <dmedia/audio.h>
#include <unistd.h>
#include <errno.h>

extern "C" void *callbackHandler(void * ptr);

RtApiAl :: RtApiAl()
{
  this->initialize();

  if (nDevices_ <= 0) {
    sprintf(message_, "RtApiAl: no Irix AL audio devices found!");
    error(RtError::NO_DEVICES_FOUND);
 }
}

RtApiAl :: ~RtApiAl()
{
  // The subclass destructor gets called before the base class
  // destructor, so close any existing streams before deallocating
  // apiDeviceId memory.
  if ( stream_.mode != UNINITIALIZED ) closeStream();

  // Free our allocated apiDeviceId memory.
  long *id;
  for ( unsigned int i=0; i<devices_.size(); i++ ) {
    id = (long *) devices_[i].apiDeviceId;
    if (id) free(id);
  }
}

void RtApiAl :: initialize(void)
{
  // Count cards and devices
  nDevices_ = 0;

  // Determine the total number of input and output devices.
  nDevices_ = alQueryValues(AL_SYSTEM, AL_DEVICES, 0, 0, 0, 0);
  if (nDevices_ < 0) {
    sprintf(message_, "RtApiAl: error counting devices: %s.",
            alGetErrorString(oserror()));
    error(RtError::DRIVER_ERROR);
  }

  if (nDevices_ <= 0) return;

  ALvalue *vls = (ALvalue *) new ALvalue[nDevices_];

  // Create our list of devices and write their ascii identifiers and resource ids.
  char name[64];
  int outs, ins, i;
  ALpv pvs[1];
  pvs[0].param = AL_NAME;
  pvs[0].value.ptr = name;
  pvs[0].sizeIn = 64;
  RtApiDevice device;
  long *id;

  outs = alQueryValues(AL_SYSTEM, AL_DEFAULT_OUTPUT, vls, nDevices_, 0, 0);
  if (outs < 0) {
    delete [] vls;
    sprintf(message_, "RtApiAl: error getting output devices: %s.",
            alGetErrorString(oserror()));
    error(RtError::DRIVER_ERROR);
  }

  for (i=0; i<outs; i++) {
    if (alGetParams(vls[i].i, pvs, 1) < 0) {
      delete [] vls;
      sprintf(message_, "RtApiAl: error querying output devices: %s.",
              alGetErrorString(oserror()));
      error(RtError::DRIVER_ERROR);
    }
    device.name.erase();
    device.name.append( (const char *)name, strlen(name)+1);
    devices_.push_back(device);
    id = (long *) calloc(2, sizeof(long));
    id[0] = vls[i].i;
    devices_[i].apiDeviceId = (void *) id;
  }

  ins = alQueryValues(AL_SYSTEM, AL_DEFAULT_INPUT, &vls[outs], nDevices_-outs, 0, 0);
  if (ins < 0) {
    delete [] vls;
    sprintf(message_, "RtApiAl: error getting input devices: %s.",
            alGetErrorString(oserror()));
    error(RtError::DRIVER_ERROR);
  }

  for (i=outs; i<ins+outs; i++) {
    if (alGetParams(vls[i].i, pvs, 1) < 0) {
      delete [] vls;
      sprintf(message_, "RtApiAl: error querying input devices: %s.",
              alGetErrorString(oserror()));
      error(RtError::DRIVER_ERROR);
    }
    device.name.erase();
    device.name.append( (const char *)name, strlen(name)+1);
    devices_.push_back(device);
    id = (long *) calloc(2, sizeof(long));
    id[1] = vls[i].i;
    devices_[i].apiDeviceId = (void *) id;
  }

  delete [] vls;
}

int RtApiAl :: getDefaultInputDevice(void)
{
  ALvalue value;
  long *id;
  int result = alQueryValues(AL_SYSTEM, AL_DEFAULT_INPUT, &value, 1, 0, 0);
  if (result < 0) {
    sprintf(message_, "RtApiAl: error getting default input device id: %s.",
            alGetErrorString(oserror()));
    error(RtError::WARNING);
  }
  else {
    for ( unsigned int i=0; i<devices_.size(); i++ ) {
      id = (long *) devices_[i].apiDeviceId;
      if ( id[1] == value.i ) return i;
    }
  }

  return 0;
}

int RtApiAl :: getDefaultOutputDevice(void)
{
  ALvalue value;
  long *id;
  int result = alQueryValues(AL_SYSTEM, AL_DEFAULT_OUTPUT, &value, 1, 0, 0);
  if (result < 0) {
    sprintf(message_, "RtApiAl: error getting default output device id: %s.",
            alGetErrorString(oserror()));
    error(RtError::WARNING);
  }
  else {
    for ( unsigned int i=0; i<devices_.size(); i++ ) {
      id = (long *) devices_[i].apiDeviceId;
      if ( id[0] == value.i ) return i;
    }
  }

  return 0;
}

void RtApiAl :: probeDeviceInfo(RtApiDevice *info)
{
  int result;
  long resource;
  ALvalue value;
  ALparamInfo pinfo;

  // Get output resource ID if it exists.
  long *id = (long *) info->apiDeviceId;
  resource = id[0];
  if (resource > 0) {

    // Probe output device parameters.
    result = alQueryValues(resource, AL_CHANNELS, &value, 1, 0, 0);
    if (result < 0) {
      sprintf(message_, "RtApiAl: error getting device (%s) channels: %s.",
              info->name.c_str(), alGetErrorString(oserror()));
      error(RtError::WARNING);
    }
    else {
      info->maxOutputChannels = value.i;
      info->minOutputChannels = 1;
    }

    result = alGetParamInfo(resource, AL_RATE, &pinfo);
    if (result < 0) {
      sprintf(message_, "RtApiAl: error getting device (%s) rates: %s.",
              info->name.c_str(), alGetErrorString(oserror()));
      error(RtError::WARNING);
    }
    else {
      info->sampleRates.clear();
      for (unsigned int k=0; k<MAX_SAMPLE_RATES; k++) {
        if ( SAMPLE_RATES[k] >= pinfo.min.i && SAMPLE_RATES[k] <= pinfo.max.i )
          info->sampleRates.push_back( SAMPLE_RATES[k] );
      }
    }

    // The AL library supports all our formats, except 24-bit and 32-bit ints.
    info->nativeFormats = (RtAudioFormat) 51;
  }

  // Now get input resource ID if it exists.
  resource = id[1];
  if (resource > 0) {

    // Probe input device parameters.
    result = alQueryValues(resource, AL_CHANNELS, &value, 1, 0, 0);
    if (result < 0) {
      sprintf(message_, "RtApiAl: error getting device (%s) channels: %s.",
              info->name.c_str(), alGetErrorString(oserror()));
      error(RtError::WARNING);
    }
    else {
      info->maxInputChannels = value.i;
      info->minInputChannels = 1;
    }

    result = alGetParamInfo(resource, AL_RATE, &pinfo);
    if (result < 0) {
      sprintf(message_, "RtApiAl: error getting device (%s) rates: %s.",
              info->name.c_str(), alGetErrorString(oserror()));
      error(RtError::WARNING);
    }
    else {
      // In the case of the default device, these values will
      // overwrite the rates determined for the output device.  Since
      // the input device is most likely to be more limited than the
      // output device, this is ok.
      info->sampleRates.clear();
      for (unsigned int k=0; k<MAX_SAMPLE_RATES; k++) {
        if ( SAMPLE_RATES[k] >= pinfo.min.i && SAMPLE_RATES[k] <= pinfo.max.i )
          info->sampleRates.push_back( SAMPLE_RATES[k] );
      }
    }

    // The AL library supports all our formats, except 24-bit and 32-bit ints.
    info->nativeFormats = (RtAudioFormat) 51;
  }

  if ( info->maxInputChannels == 0 && info->maxOutputChannels == 0 )
    return;
  if ( info->sampleRates.size() == 0 )
    return;

  // Determine duplex status.
  if (info->maxInputChannels < info->maxOutputChannels)
    info->maxDuplexChannels = info->maxInputChannels;
  else
    info->maxDuplexChannels = info->maxOutputChannels;
  if (info->minInputChannels < info->minOutputChannels)
    info->minDuplexChannels = info->minInputChannels;
  else
    info->minDuplexChannels = info->minOutputChannels;

  if ( info->maxDuplexChannels > 0 ) info->hasDuplexSupport = true;
  else info->hasDuplexSupport = false;

  info->probed = true;

  return;
}

bool RtApiAl :: probeDeviceOpen(int device, StreamMode mode, int channels, 
                                int sampleRate, RtAudioFormat format,
                                int *bufferSize, int numberOfBuffers)
{
  int result, nBuffers;
  long resource;
  ALconfig al_config;
  ALport port;
  ALpv pvs[2];
  long *id = (long *) devices_[device].apiDeviceId;

  // Get a new ALconfig structure.
  al_config = alNewConfig();
  if ( !al_config ) {
    sprintf(message_,"RtApiAl: can't get AL config: %s.",
            alGetErrorString(oserror()));
    error(RtError::WARNING);
    return FAILURE;
  }

  // Set the channels.
  result = alSetChannels(al_config, channels);
  if ( result < 0 ) {
    alFreeConfig(al_config);
    sprintf(message_,"RtApiAl: can't set %d channels in AL config: %s.",
            channels, alGetErrorString(oserror()));
    error(RtError::WARNING);
    return FAILURE;
  }

  // Attempt to set the queue size.  The al API doesn't provide a
  // means for querying the minimum/maximum buffer size of a device,
  // so if the specified size doesn't work, take whatever the
  // al_config structure returns.
  if ( numberOfBuffers < 1 )
    nBuffers = 1;
  else
    nBuffers = numberOfBuffers;
  long buffer_size = *bufferSize * nBuffers;
  result = alSetQueueSize(al_config, buffer_size); // in sample frames
  if ( result < 0 ) {
    // Get the buffer size specified by the al_config and try that.
    buffer_size = alGetQueueSize(al_config);
    result = alSetQueueSize(al_config, buffer_size);
    if ( result < 0 ) {
      alFreeConfig(al_config);
      sprintf(message_,"RtApiAl: can't set buffer size (%ld) in AL config: %s.",
              buffer_size, alGetErrorString(oserror()));
      error(RtError::WARNING);
      return FAILURE;
    }
    *bufferSize = buffer_size / nBuffers;
  }

  // Set the data format.
  stream_.userFormat = format;
  stream_.deviceFormat[mode] = format;
  if (format == RTAUDIO_SINT8) {
    result = alSetSampFmt(al_config, AL_SAMPFMT_TWOSCOMP);
    result = alSetWidth(al_config, AL_SAMPLE_8);
  }
  else if (format == RTAUDIO_SINT16) {
    result = alSetSampFmt(al_config, AL_SAMPFMT_TWOSCOMP);
    result = alSetWidth(al_config, AL_SAMPLE_16);
  }
  else if (format == RTAUDIO_SINT24) {
    // Our 24-bit format assumes the upper 3 bytes of a 4 byte word.
    // The AL library uses the lower 3 bytes, so we'll need to do our
    // own conversion.
    result = alSetSampFmt(al_config, AL_SAMPFMT_FLOAT);
    stream_.deviceFormat[mode] = RTAUDIO_FLOAT32;
  }
  else if (format == RTAUDIO_SINT32) {
    // The AL library doesn't seem to support the 32-bit integer
    // format, so we'll need to do our own conversion.
    result = alSetSampFmt(al_config, AL_SAMPFMT_FLOAT);
    stream_.deviceFormat[mode] = RTAUDIO_FLOAT32;
  }
  else if (format == RTAUDIO_FLOAT32)
    result = alSetSampFmt(al_config, AL_SAMPFMT_FLOAT);
  else if (format == RTAUDIO_FLOAT64)
    result = alSetSampFmt(al_config, AL_SAMPFMT_DOUBLE);

  if ( result == -1 ) {
    alFreeConfig(al_config);
    sprintf(message_,"RtApiAl: error setting sample format in AL config: %s.",
            alGetErrorString(oserror()));
    error(RtError::WARNING);
    return FAILURE;
  }

  if (mode == OUTPUT) {

    // Set our device.
    if (device == 0)
      resource = AL_DEFAULT_OUTPUT;
    else
      resource = id[0];
    result = alSetDevice(al_config, resource);
    if ( result == -1 ) {
      alFreeConfig(al_config);
      sprintf(message_,"RtApiAl: error setting device (%s) in AL config: %s.",
              devices_[device].name.c_str(), alGetErrorString(oserror()));
      error(RtError::WARNING);
      return FAILURE;
    }

    // Open the port.
    port = alOpenPort("RtApiAl Output Port", "w", al_config);
    if( !port ) {
      alFreeConfig(al_config);
      sprintf(message_,"RtApiAl: error opening output port: %s.",
              alGetErrorString(oserror()));
      error(RtError::WARNING);
      return FAILURE;
    }

    // Set the sample rate
    pvs[0].param = AL_MASTER_CLOCK;
    pvs[0].value.i = AL_CRYSTAL_MCLK_TYPE;
    pvs[1].param = AL_RATE;
    pvs[1].value.ll = alDoubleToFixed((double)sampleRate);
    result = alSetParams(resource, pvs, 2);
    if ( result < 0 ) {
      alClosePort(port);
      alFreeConfig(al_config);
      sprintf(message_,"RtApiAl: error setting sample rate (%d) for device (%s): %s.",
              sampleRate, devices_[device].name.c_str(), alGetErrorString(oserror()));
      error(RtError::WARNING);
      return FAILURE;
    }
  }
  else { // mode == INPUT

    // Set our device.
    if (device == 0)
      resource = AL_DEFAULT_INPUT;
    else
      resource = id[1];
    result = alSetDevice(al_config, resource);
    if ( result == -1 ) {
      alFreeConfig(al_config);
      sprintf(message_,"RtApiAl: error setting device (%s) in AL config: %s.",
              devices_[device].name.c_str(), alGetErrorString(oserror()));
      error(RtError::WARNING);
      return FAILURE;
    }

    // Open the port.
    port = alOpenPort("RtApiAl Input Port", "r", al_config);
    if( !port ) {
      alFreeConfig(al_config);
      sprintf(message_,"RtApiAl: error opening input port: %s.",
              alGetErrorString(oserror()));
      error(RtError::WARNING);
      return FAILURE;
    }

    // Set the sample rate
    pvs[0].param = AL_MASTER_CLOCK;
    pvs[0].value.i = AL_CRYSTAL_MCLK_TYPE;
    pvs[1].param = AL_RATE;
    pvs[1].value.ll = alDoubleToFixed((double)sampleRate);
    result = alSetParams(resource, pvs, 2);
    if ( result < 0 ) {
      alClosePort(port);
      alFreeConfig(al_config);
      sprintf(message_,"RtApiAl: error setting sample rate (%d) for device (%s): %s.",
              sampleRate, devices_[device].name.c_str(), alGetErrorString(oserror()));
      error(RtError::WARNING);
      return FAILURE;
    }
  }

  alFreeConfig(al_config);

  stream_.nUserChannels[mode] = channels;
  stream_.nDeviceChannels[mode] = channels;

  // Save stream handle.
  ALport *handle = (ALport *) stream_.apiHandle;
  if ( handle == 0 ) {
    handle = (ALport *) calloc(2, sizeof(ALport));
    if ( handle == NULL ) {
      sprintf(message_, "RtApiAl: Irix Al error allocating handle memory (%s).",
              devices_[device].name.c_str());
      goto error;
    }
    stream_.apiHandle = (void *) handle;
    handle[0] = 0;
    handle[1] = 0;
  }
  handle[mode] = port;

  // Set flags for buffer conversion
  stream_.doConvertBuffer[mode] = false;
  if (stream_.userFormat != stream_.deviceFormat[mode])
    stream_.doConvertBuffer[mode] = true;

  // Allocate necessary internal buffers
  if ( stream_.nUserChannels[0] != stream_.nUserChannels[1] ) {

    long buffer_bytes;
    if (stream_.nUserChannels[0] >= stream_.nUserChannels[1])
      buffer_bytes = stream_.nUserChannels[0];
    else
      buffer_bytes = stream_.nUserChannels[1];

    buffer_bytes *= *bufferSize * formatBytes(stream_.userFormat);
    if (stream_.userBuffer) free(stream_.userBuffer);
    stream_.userBuffer = (char *) calloc(buffer_bytes, 1);
    if (stream_.userBuffer == NULL) {
      sprintf(message_, "RtApiAl: error allocating user buffer memory (%s).",
              devices_[device].name.c_str());
      goto error;
    }
  }

  if ( stream_.doConvertBuffer[mode] ) {

    long buffer_bytes;
    bool makeBuffer = true;
    if ( mode == OUTPUT )
      buffer_bytes = stream_.nDeviceChannels[0] * formatBytes(stream_.deviceFormat[0]);
    else { // mode == INPUT
      buffer_bytes = stream_.nDeviceChannels[1] * formatBytes(stream_.deviceFormat[1]);
      if ( stream_.mode == OUTPUT && stream_.deviceBuffer ) {
        long bytes_out = stream_.nDeviceChannels[0] * formatBytes(stream_.deviceFormat[0]);
        if ( buffer_bytes < bytes_out ) makeBuffer = false;
      }
    }

    if ( makeBuffer ) {
      buffer_bytes *= *bufferSize;
      if (stream_.deviceBuffer) free(stream_.deviceBuffer);
      stream_.deviceBuffer = (char *) calloc(buffer_bytes, 1);
      if (stream_.deviceBuffer == NULL) {
        sprintf(message_, "RtApiAl: error allocating device buffer memory (%s).",
                devices_[device].name.c_str());
        goto error;
      }
    }
  }

  stream_.device[mode] = device;
  stream_.state = STREAM_STOPPED;
  if ( stream_.mode == OUTPUT && mode == INPUT )
    // We had already set up an output stream.
    stream_.mode = DUPLEX;
  else
    stream_.mode = mode;
  stream_.nBuffers = nBuffers;
  stream_.bufferSize = *bufferSize;
  stream_.sampleRate = sampleRate;

  return SUCCESS;

 error:
  if (handle) {
    if (handle[0])
      alClosePort(handle[0]);
    if (handle[1])
      alClosePort(handle[1]);
    free(handle);
    stream_.apiHandle = 0;
  }

  if (stream_.userBuffer) {
    free(stream_.userBuffer);
    stream_.userBuffer = 0;
  }

  error(RtError::WARNING);
  return FAILURE;
}

void RtApiAl :: closeStream()
{
  // We don't want an exception to be thrown here because this
  // function is called by our class destructor.  So, do our own
  // streamId check.
  if ( stream_.mode == UNINITIALIZED ) {
    sprintf(message_, "RtApiAl::closeStream(): no open stream to close!");
    error(RtError::WARNING);
    return;
  }

  ALport *handle = (ALport *) stream_.apiHandle;
  if (stream_.state == STREAM_RUNNING) {
    int buffer_size = stream_.bufferSize * stream_.nBuffers;
    if (stream_.mode == OUTPUT || stream_.mode == DUPLEX)
      alDiscardFrames(handle[0], buffer_size);
    if (stream_.mode == INPUT || stream_.mode == DUPLEX)
      alDiscardFrames(handle[1], buffer_size);
    stream_.state = STREAM_STOPPED;
  }

  if (stream_.callbackInfo.usingCallback) {
    stream_.callbackInfo.usingCallback = false;
    pthread_join(stream_.callbackInfo.thread, NULL);
  }

  if (handle) {
    if (handle[0]) alClosePort(handle[0]);
    if (handle[1]) alClosePort(handle[1]);
    free(handle);
    stream_.apiHandle = 0;
  }

  if (stream_.userBuffer) {
    free(stream_.userBuffer);
    stream_.userBuffer = 0;
  }

  if (stream_.deviceBuffer) {
    free(stream_.deviceBuffer);
    stream_.deviceBuffer = 0;
  }

  stream_.mode = UNINITIALIZED;
}

void RtApiAl :: startStream()
{
  verifyStream();
  if (stream_.state == STREAM_RUNNING) return;

  MUTEX_LOCK(&stream_.mutex);

  // The AL port is ready as soon as it is opened.
  stream_.state = STREAM_RUNNING;

  MUTEX_UNLOCK(&stream_.mutex);
}

void RtApiAl :: stopStream()
{
  verifyStream();
  if (stream_.state == STREAM_STOPPED) return;

  // Change the state before the lock to improve shutdown response
  // when using a callback.
  stream_.state = STREAM_STOPPED;
  MUTEX_LOCK(&stream_.mutex);

  int result, buffer_size = stream_.bufferSize * stream_.nBuffers;
  ALport *handle = (ALport *) stream_.apiHandle;

  if (stream_.mode == OUTPUT || stream_.mode == DUPLEX)
    alZeroFrames(handle[0], buffer_size);

  if (stream_.mode == INPUT || stream_.mode == DUPLEX) {
    result = alDiscardFrames(handle[1], buffer_size);
    if (result == -1) {
      sprintf(message_, "RtApiAl: error draining stream device (%s): %s.",
              devices_[stream_.device[1]].name.c_str(), alGetErrorString(oserror()));
      error(RtError::DRIVER_ERROR);
    }
  }

  MUTEX_UNLOCK(&stream_.mutex);
}

void RtApiAl :: abortStream()
{
  verifyStream();
  if (stream_.state == STREAM_STOPPED) return;

  // Change the state before the lock to improve shutdown response
  // when using a callback.
  stream_.state = STREAM_STOPPED;
  MUTEX_LOCK(&stream_.mutex);

  ALport *handle = (ALport *) stream_.apiHandle;
  if (stream_.mode == OUTPUT || stream_.mode == DUPLEX) {

    int buffer_size = stream_.bufferSize * stream_.nBuffers;
    int result = alDiscardFrames(handle[0], buffer_size);
    if (result == -1) {
      sprintf(message_, "RtApiAl: error aborting stream device (%s): %s.",
              devices_[stream_.device[0]].name.c_str(), alGetErrorString(oserror()));
      error(RtError::DRIVER_ERROR);
    }
  }

  // There is no clear action to take on the input stream, since the
  // port will continue to run in any event.

  MUTEX_UNLOCK(&stream_.mutex);
}

int RtApiAl :: streamWillBlock()
{
  verifyStream();

  if (stream_.state == STREAM_STOPPED) return 0;

  MUTEX_LOCK(&stream_.mutex);

  int frames = 0;
  int err = 0;
  ALport *handle = (ALport *) stream_.apiHandle;
  if (stream_.mode == OUTPUT || stream_.mode == DUPLEX) {
    err = alGetFillable(handle[0]);
    if (err < 0) {
      sprintf(message_, "RtApiAl: error getting available frames for stream (%s): %s.",
              devices_[stream_.device[0]].name.c_str(), alGetErrorString(oserror()));
      error(RtError::DRIVER_ERROR);
    }
  }

  frames = err;

  if (stream_.mode == INPUT || stream_.mode == DUPLEX) {
    err = alGetFilled(handle[1]);
    if (err < 0) {
      sprintf(message_, "RtApiAl: error getting available frames for stream (%s): %s.",
              devices_[stream_.device[1]].name.c_str(), alGetErrorString(oserror()));
      error(RtError::DRIVER_ERROR);
    }
    if (frames > err) frames = err;
  }

  frames = stream_.bufferSize - frames;
  if (frames < 0) frames = 0;

  MUTEX_UNLOCK(&stream_.mutex);
  return frames;
}

void RtApiAl :: tickStream()
{
  verifyStream();

  int stopStream = 0;
  if (stream_.state == STREAM_STOPPED) {
    if (stream_.callbackInfo.usingCallback) usleep(50000); // sleep 50 milliseconds
    return;
  }
  else if (stream_.callbackInfo.usingCallback) {
    RtAudioCallback callback = (RtAudioCallback) stream_.callbackInfo.callback;
    stopStream = callback(stream_.userBuffer, stream_.bufferSize, stream_.callbackInfo.userData);
  }

  MUTEX_LOCK(&stream_.mutex);

  // The state might change while waiting on a mutex.
  if (stream_.state == STREAM_STOPPED)
    goto unlock;

  char *buffer;
  int channels;
  RtAudioFormat format;
  ALport *handle = (ALport *) stream_.apiHandle;
  if (stream_.mode == OUTPUT || stream_.mode == DUPLEX) {

    // Setup parameters and do buffer conversion if necessary.
    if (stream_.doConvertBuffer[0]) {
      convertStreamBuffer(OUTPUT);
      buffer = stream_.deviceBuffer;
      channels = stream_.nDeviceChannels[0];
      format = stream_.deviceFormat[0];
    }
    else {
      buffer = stream_.userBuffer;
      channels = stream_.nUserChannels[0];
      format = stream_.userFormat;
    }

    // Do byte swapping if necessary.
    if (stream_.doByteSwap[0])
      byteSwapBuffer(buffer, stream_.bufferSize * channels, format);

    // Write interleaved samples to device.
    alWriteFrames(handle[0], buffer, stream_.bufferSize);
  }

  if (stream_.mode == INPUT || stream_.mode == DUPLEX) {

    // Setup parameters.
    if (stream_.doConvertBuffer[1]) {
      buffer = stream_.deviceBuffer;
      channels = stream_.nDeviceChannels[1];
      format = stream_.deviceFormat[1];
    }
    else {
      buffer = stream_.userBuffer;
      channels = stream_.nUserChannels[1];
      format = stream_.userFormat;
    }

    // Read interleaved samples from device.
    alReadFrames(handle[1], buffer, stream_.bufferSize);

    // Do byte swapping if necessary.
    if (stream_.doByteSwap[1])
      byteSwapBuffer(buffer, stream_.bufferSize * channels, format);

    // Do buffer conversion if necessary.
    if (stream_.doConvertBuffer[1])
      convertStreamBuffer(INPUT);
  }

 unlock:
  MUTEX_UNLOCK(&stream_.mutex);

  if (stream_.callbackInfo.usingCallback && stopStream)
    this->stopStream();

  RtApi::tickStream();
}

void RtApiAl :: setStreamCallback(RtAudioCallback callback, void *userData)
{
  verifyStream();

  CallbackInfo *info = (CallbackInfo *) &stream_.callbackInfo;
  if ( info->usingCallback ) {
    sprintf(message_, "RtApiAl: A callback is already set for this stream!");
    error(RtError::WARNING);
    return;
  }

  info->callback = (void *) callback;
  info->userData = userData;
  info->usingCallback = true;
  info->object = (void *) this;

  // Set the thread attributes for joinable and realtime scheduling
  // priority.  The higher priority will only take affect if the
  // program is run as root or suid.
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
  pthread_attr_setschedpolicy(&attr, SCHED_RR);

  int err = pthread_create(&info->thread, &attr, callbackHandler, &stream_.callbackInfo);
  pthread_attr_destroy(&attr);
  if (err) {
    info->usingCallback = false;
    sprintf(message_, "RtApiAl: error starting callback thread!");
    error(RtError::THREAD_ERROR);
  }
}

void RtApiAl :: cancelStreamCallback()
{
  verifyStream();

  if (stream_.callbackInfo.usingCallback) {

    if (stream_.state == STREAM_RUNNING)
      stopStream();

    MUTEX_LOCK(&stream_.mutex);

    stream_.callbackInfo.usingCallback = false;
    pthread_join(stream_.callbackInfo.thread, NULL);
    stream_.callbackInfo.thread = 0;
    stream_.callbackInfo.callback = NULL;
    stream_.callbackInfo.userData = NULL;

    MUTEX_UNLOCK(&stream_.mutex);
  }
}

extern "C" void *callbackHandler(void *ptr)
{
  CallbackInfo *info = (CallbackInfo *) ptr;
  RtApiAl *object = (RtApiAl *) info->object;
  bool *usingCallback = &info->usingCallback;

  while ( *usingCallback ) {
    try {
      object->tickStream();
    }
    catch (RtError &exception) {
      fprintf(stderr, "\nRtApiAl: callback thread error (%s) ... closing thread.\n\n",
              exception.getMessageString());
      break;
    }
  }

  return 0;
}

//******************** End of __IRIX_AL__ *********************//
#endif


// *************************************************** //
//
// Protected common (OS-independent) RtAudio methods.
//
// *************************************************** //

// This method can be modified to control the behavior of error
// message reporting and throwing.
void RtApi :: error(RtError::Type type)
{
  if (type == RtError::WARNING) {
    fprintf(stderr, "\n%s\n\n", message_);
  }
  else if (type == RtError::DEBUG_WARNING) {
#if defined(__RTAUDIO_DEBUG__)
    fprintf(stderr, "\n%s\n\n", message_);
#endif
  }
  else {
#if defined(__RTAUDIO_DEBUG__)
    fprintf(stderr, "\n%s\n\n", message_);
#endif
    throw RtError(std::string(message_), type);
  }
}

void RtApi :: verifyStream()
{
  if ( stream_.mode == UNINITIALIZED ) {
    sprintf(message_, "RtAudio: a stream was not previously opened!");
    error(RtError::INVALID_STREAM);
  }
}

void RtApi :: clearDeviceInfo(RtApiDevice *info)
{
  // Don't clear the name or DEVICE_ID fields here ... they are
  // typically set prior to a call of this function.
  info->probed = false;
  info->maxOutputChannels = 0;
  info->maxInputChannels = 0;
  info->maxDuplexChannels = 0;
  info->minOutputChannels = 0;
  info->minInputChannels = 0;
  info->minDuplexChannels = 0;
  info->hasDuplexSupport = false;
  info->sampleRates.clear();
  info->nativeFormats = 0;
}

void RtApi :: clearStreamInfo()
{
  stream_.mode = UNINITIALIZED;
  stream_.state = STREAM_STOPPED;
  stream_.sampleRate = 0;
  stream_.bufferSize = 0;
  stream_.nBuffers = 0;
  stream_.userFormat = 0;
  stream_.streamTime = 0.0;
  for ( int i=0; i<2; i++ ) {
    stream_.device[i] = 0;
    stream_.doConvertBuffer[i] = false;
    stream_.deInterleave[i] = false;
    stream_.doByteSwap[i] = false;
    stream_.nUserChannels[i] = 0;
    stream_.nDeviceChannels[i] = 0;
    stream_.deviceFormat[i] = 0;
  }
}

int RtApi :: formatBytes(RtAudioFormat format)
{
  if (format == RTAUDIO_SINT16)
    return 2;
  else if (format == RTAUDIO_SINT24 || format == RTAUDIO_SINT32 ||
           format == RTAUDIO_FLOAT32)
    return 4;
  else if (format == RTAUDIO_FLOAT64)
    return 8;
  else if (format == RTAUDIO_SINT8)
    return 1;

  sprintf(message_,"RtApi: undefined format in formatBytes().");
  error(RtError::WARNING);

  return 0;
}

void RtApi :: convertStreamBuffer( StreamMode mode )
{
  // This method does format conversion, input/output channel compensation, and
  // data interleaving/deinterleaving.  24-bit integers are assumed to occupy
  // the upper three bytes of a 32-bit integer.

  int j, jump_in, jump_out, channels;
  RtAudioFormat format_in, format_out;
  char *input, *output;

  if (mode == INPUT) { // convert device to user buffer
    input = stream_.deviceBuffer;
    output = stream_.userBuffer;
    jump_in = stream_.nDeviceChannels[1];
    jump_out = stream_.nUserChannels[1];
    format_in = stream_.deviceFormat[1];
    format_out = stream_.userFormat;
  }
  else { // convert user to device buffer
    input = stream_.userBuffer;
    output = stream_.deviceBuffer;
    jump_in = stream_.nUserChannels[0];
    jump_out = stream_.nDeviceChannels[0];
    format_in = stream_.userFormat;
    format_out = stream_.deviceFormat[0];

    // clear our device buffer when in/out duplex device channels are different
    if ( stream_.mode == DUPLEX &&
         stream_.nDeviceChannels[0] != stream_.nDeviceChannels[1] )
      memset(output, 0, stream_.bufferSize * jump_out * formatBytes(format_out));
  }

  channels = (jump_in < jump_out) ? jump_in : jump_out;

  // Set up the interleave/deinterleave offsets
  std::vector<int> offset_in(channels);
  std::vector<int> offset_out(channels);
  if (mode == INPUT && stream_.deInterleave[1]) {
    for (int k=0; k<channels; k++) {
      offset_in[k] = k * stream_.bufferSize;
      offset_out[k] = k;
      jump_in = 1;
    }
  }
  else if (mode == OUTPUT && stream_.deInterleave[0]) {
    for (int k=0; k<channels; k++) {
      offset_in[k] = k;
      offset_out[k] = k * stream_.bufferSize;
      jump_out = 1;
    }
  }
  else {
    for (int k=0; k<channels; k++) {
      offset_in[k] = k;
      offset_out[k] = k;
    }
  }

  if (format_out == RTAUDIO_FLOAT64) {
    Float64 scale;
    Float64 *out = (Float64 *)output;

    if (format_in == RTAUDIO_SINT8) {
      signed char *in = (signed char *)input;
      scale = 1.0 / 128.0;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = (Float64) in[offset_in[j]];
          out[offset_out[j]] *= scale;
        }
        in += jump_in;
        out += jump_out;
      }
    }
    else if (format_in == RTAUDIO_SINT16) {
      Int16 *in = (Int16 *)input;
      scale = 1.0 / 32768.0;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = (Float64) in[offset_in[j]];
          out[offset_out[j]] *= scale;
        }
        in += jump_in;
        out += jump_out;
      }
    }
    else if (format_in == RTAUDIO_SINT24) {
      Int32 *in = (Int32 *)input;
      scale = 1.0 / 2147483648.0;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = (Float64) (in[offset_in[j]] & 0xffffff00);
          out[offset_out[j]] *= scale;
        }
        in += jump_in;
        out += jump_out;
      }
    }
    else if (format_in == RTAUDIO_SINT32) {
      Int32 *in = (Int32 *)input;
      scale = 1.0 / 2147483648.0;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = (Float64) in[offset_in[j]];
          out[offset_out[j]] *= scale;
        }
        in += jump_in;
        out += jump_out;
      }
    }
    else if (format_in == RTAUDIO_FLOAT32) {
      Float32 *in = (Float32 *)input;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = (Float64) in[offset_in[j]];
        }
        in += jump_in;
        out += jump_out;
      }
    }
    else if (format_in == RTAUDIO_FLOAT64) {
      // Channel compensation and/or (de)interleaving only.
      Float64 *in = (Float64 *)input;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = in[offset_in[j]];
        }
        in += jump_in;
        out += jump_out;
      }
    }
  }
  else if (format_out == RTAUDIO_FLOAT32) {
    Float32 scale;
    Float32 *out = (Float32 *)output;

    if (format_in == RTAUDIO_SINT8) {
      signed char *in = (signed char *)input;
      scale = 1.0 / 128.0;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = (Float32) in[offset_in[j]];
          out[offset_out[j]] *= scale;
        }
        in += jump_in;
        out += jump_out;
      }
    }
    else if (format_in == RTAUDIO_SINT16) {
      Int16 *in = (Int16 *)input;
      scale = 1.0 / 32768.0;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = (Float32) in[offset_in[j]];
          out[offset_out[j]] *= scale;
        }
        in += jump_in;
        out += jump_out;
      }
    }
    else if (format_in == RTAUDIO_SINT24) {
      Int32 *in = (Int32 *)input;
      scale = 1.0 / 2147483648.0;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = (Float32) (in[offset_in[j]] & 0xffffff00);
          out[offset_out[j]] *= scale;
        }
        in += jump_in;
        out += jump_out;
      }
    }
    else if (format_in == RTAUDIO_SINT32) {
      Int32 *in = (Int32 *)input;
      scale = 1.0 / 2147483648.0;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = (Float32) in[offset_in[j]];
          out[offset_out[j]] *= scale;
        }
        in += jump_in;
        out += jump_out;
      }
    }
    else if (format_in == RTAUDIO_FLOAT32) {
      // Channel compensation and/or (de)interleaving only.
      Float32 *in = (Float32 *)input;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = in[offset_in[j]];
        }
        in += jump_in;
        out += jump_out;
      }
    }
    else if (format_in == RTAUDIO_FLOAT64) {
      Float64 *in = (Float64 *)input;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = (Float32) in[offset_in[j]];
        }
        in += jump_in;
        out += jump_out;
      }
    }
  }
  else if (format_out == RTAUDIO_SINT32) {
    Int32 *out = (Int32 *)output;
    if (format_in == RTAUDIO_SINT8) {
      signed char *in = (signed char *)input;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = (Int32) in[offset_in[j]];
          out[offset_out[j]] <<= 24;
        }
        in += jump_in;
        out += jump_out;
      }
    }
    else if (format_in == RTAUDIO_SINT16) {
      Int16 *in = (Int16 *)input;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = (Int32) in[offset_in[j]];
          out[offset_out[j]] <<= 16;
        }
        in += jump_in;
        out += jump_out;
      }
    }
    else if (format_in == RTAUDIO_SINT24) {
      Int32 *in = (Int32 *)input;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = (Int32) in[offset_in[j]];
        }
        in += jump_in;
        out += jump_out;
      }
    }
    else if (format_in == RTAUDIO_SINT32) {
      // Channel compensation and/or (de)interleaving only.
      Int32 *in = (Int32 *)input;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = in[offset_in[j]];
        }
        in += jump_in;
        out += jump_out;
      }
    }
    else if (format_in == RTAUDIO_FLOAT32) {
      Float32 *in = (Float32 *)input;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = (Int32) (in[offset_in[j]] * 2147483647.0);
        }
        in += jump_in;
        out += jump_out;
      }
    }
    else if (format_in == RTAUDIO_FLOAT64) {
      Float64 *in = (Float64 *)input;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = (Int32) (in[offset_in[j]] * 2147483647.0);
        }
        in += jump_in;
        out += jump_out;
      }
    }
  }
  else if (format_out == RTAUDIO_SINT24) {
    Int32 *out = (Int32 *)output;
    if (format_in == RTAUDIO_SINT8) {
      signed char *in = (signed char *)input;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = (Int32) in[offset_in[j]];
          out[offset_out[j]] <<= 24;
        }
        in += jump_in;
        out += jump_out;
      }
    }
    else if (format_in == RTAUDIO_SINT16) {
      Int16 *in = (Int16 *)input;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = (Int32) in[offset_in[j]];
          out[offset_out[j]] <<= 16;
        }
        in += jump_in;
        out += jump_out;
      }
    }
    else if (format_in == RTAUDIO_SINT24) {
      // Channel compensation and/or (de)interleaving only.
      Int32 *in = (Int32 *)input;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = in[offset_in[j]];
        }
        in += jump_in;
        out += jump_out;
      }
    }
    else if (format_in == RTAUDIO_SINT32) {
      Int32 *in = (Int32 *)input;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = (Int32) (in[offset_in[j]] & 0xffffff00);
        }
        in += jump_in;
        out += jump_out;
      }
    }
    else if (format_in == RTAUDIO_FLOAT32) {
      Float32 *in = (Float32 *)input;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = (Int32) (in[offset_in[j]] * 2147483647.0);
        }
        in += jump_in;
        out += jump_out;
      }
    }
    else if (format_in == RTAUDIO_FLOAT64) {
      Float64 *in = (Float64 *)input;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = (Int32) (in[offset_in[j]] * 2147483647.0);
        }
        in += jump_in;
        out += jump_out;
      }
    }
  }
  else if (format_out == RTAUDIO_SINT16) {
    Int16 *out = (Int16 *)output;
    if (format_in == RTAUDIO_SINT8) {
      signed char *in = (signed char *)input;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = (Int16) in[offset_in[j]];
          out[offset_out[j]] <<= 8;
        }
        in += jump_in;
        out += jump_out;
      }
    }
    else if (format_in == RTAUDIO_SINT16) {
      // Channel compensation and/or (de)interleaving only.
      Int16 *in = (Int16 *)input;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = in[offset_in[j]];
        }
        in += jump_in;
        out += jump_out;
      }
    }
    else if (format_in == RTAUDIO_SINT24) {
      Int32 *in = (Int32 *)input;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = (Int16) ((in[offset_in[j]] >> 16) & 0x0000ffff);
        }
        in += jump_in;
        out += jump_out;
      }
    }
    else if (format_in == RTAUDIO_SINT32) {
      Int32 *in = (Int32 *)input;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = (Int16) ((in[offset_in[j]] >> 16) & 0x0000ffff);
        }
        in += jump_in;
        out += jump_out;
      }
    }
    else if (format_in == RTAUDIO_FLOAT32) {
      Float32 *in = (Float32 *)input;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = (Int16) (in[offset_in[j]] * 32767.0);
        }
        in += jump_in;
        out += jump_out;
      }
    }
    else if (format_in == RTAUDIO_FLOAT64) {
      Float64 *in = (Float64 *)input;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = (Int16) (in[offset_in[j]] * 32767.0);
        }
        in += jump_in;
        out += jump_out;
      }
    }
  }
  else if (format_out == RTAUDIO_SINT8) {
    signed char *out = (signed char *)output;
    if (format_in == RTAUDIO_SINT8) {
      // Channel compensation and/or (de)interleaving only.
      signed char *in = (signed char *)input;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = in[offset_in[j]];
        }
        in += jump_in;
        out += jump_out;
      }
    }
    if (format_in == RTAUDIO_SINT16) {
      Int16 *in = (Int16 *)input;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = (signed char) ((in[offset_in[j]] >> 8) & 0x00ff);
        }
        in += jump_in;
        out += jump_out;
      }
    }
    else if (format_in == RTAUDIO_SINT24) {
      Int32 *in = (Int32 *)input;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = (signed char) ((in[offset_in[j]] >> 24) & 0x000000ff);
        }
        in += jump_in;
        out += jump_out;
      }
    }
    else if (format_in == RTAUDIO_SINT32) {
      Int32 *in = (Int32 *)input;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = (signed char) ((in[offset_in[j]] >> 24) & 0x000000ff);
        }
        in += jump_in;
        out += jump_out;
      }
    }
    else if (format_in == RTAUDIO_FLOAT32) {
      Float32 *in = (Float32 *)input;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = (signed char) (in[offset_in[j]] * 127.0);
        }
        in += jump_in;
        out += jump_out;
      }
    }
    else if (format_in == RTAUDIO_FLOAT64) {
      Float64 *in = (Float64 *)input;
      for (int i=0; i<stream_.bufferSize; i++) {
        for (j=0; j<channels; j++) {
          out[offset_out[j]] = (signed char) (in[offset_in[j]] * 127.0);
        }
        in += jump_in;
        out += jump_out;
      }
    }
  }
}

void RtApi :: byteSwapBuffer( char *buffer, int samples, RtAudioFormat format )
{
  register char val;
  register char *ptr;

  ptr = buffer;
  if (format == RTAUDIO_SINT16) {
    for (int i=0; i<samples; i++) {
      // Swap 1st and 2nd bytes.
      val = *(ptr);
      *(ptr) = *(ptr+1);
      *(ptr+1) = val;

      // Increment 2 bytes.
      ptr += 2;
    }
  }
  else if (format == RTAUDIO_SINT24 ||
           format == RTAUDIO_SINT32 ||
           format == RTAUDIO_FLOAT32) {
    for (int i=0; i<samples; i++) {
      // Swap 1st and 4th bytes.
      val = *(ptr);
      *(ptr) = *(ptr+3);
      *(ptr+3) = val;

      // Swap 2nd and 3rd bytes.
      ptr += 1;
      val = *(ptr);
      *(ptr) = *(ptr+1);
      *(ptr+1) = val;

      // Increment 4 bytes.
      ptr += 4;
    }
  }
  else if (format == RTAUDIO_FLOAT64) {
    for (int i=0; i<samples; i++) {
      // Swap 1st and 8th bytes
      val = *(ptr);
      *(ptr) = *(ptr+7);
      *(ptr+7) = val;

      // Swap 2nd and 7th bytes
      ptr += 1;
      val = *(ptr);
      *(ptr) = *(ptr+5);
      *(ptr+5) = val;

      // Swap 3rd and 6th bytes
      ptr += 1;
      val = *(ptr);
      *(ptr) = *(ptr+3);
      *(ptr+3) = val;

      // Swap 4th and 5th bytes
      ptr += 1;
      val = *(ptr);
      *(ptr) = *(ptr+1);
      *(ptr+1) = val;

      // Increment 8 bytes.
      ptr += 8;
    }
  }
}

// Indentation settings for Vim and Emacs
//
// Local Variables:
// c-basic-offset: 2
// indent-tabs-mode: nil
// End:
//
// vim: et sts=2 sw=2

