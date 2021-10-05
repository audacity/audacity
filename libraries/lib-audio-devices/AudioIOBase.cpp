/**********************************************************************

Audacity: A Digital Audio Editor

AudioIOBase.cpp

Paul Licameli split from AudioIO.cpp

**********************************************************************/


#include "AudioIOBase.h"

#include <wx/log.h>
#include <wx/sstream.h>
#include <wx/txtstrm.h>

#include "Meter.h"
#include "Prefs.h"

#if USE_PORTMIXER
#include "portmixer.h"
#endif

int AudioIOBase::mCachedPlaybackIndex = -1;
std::vector<long> AudioIOBase::mCachedPlaybackRates;
int AudioIOBase::mCachedCaptureIndex = -1;
std::vector<long> AudioIOBase::mCachedCaptureRates;
std::vector<long> AudioIOBase::mCachedSampleRates;
double AudioIOBase::mCachedBestRateIn = 0.0;

const int AudioIOBase::StandardRates[] = {
   8000,
   11025,
   16000,
   22050,
   32000,
   44100,
   48000,
   88200,
   96000,
   176400,
   192000,
   352800,
   384000
};

const int AudioIOBase::NumStandardRates = WXSIZEOF(AudioIOBase::StandardRates);

const int AudioIOBase::RatesToTry[] = {
   8000,
   9600,
   11025,
   12000,
   15000,
   16000,
   22050,
   24000,
   32000,
   44100,
   48000,
   88200,
   96000,
   176400,
   192000,
   352800,
   384000
};
const int AudioIOBase::NumRatesToTry = WXSIZEOF(AudioIOBase::RatesToTry);

wxString AudioIOBase::DeviceName(const PaDeviceInfo* info)
{
   wxString infoName = wxSafeConvertMB2WX(info->name);

   return infoName;
}

wxString AudioIOBase::HostName(const PaDeviceInfo* info)
{
   wxString hostapiName = wxSafeConvertMB2WX(Pa_GetHostApiInfo(info->hostApi)->name);

   return hostapiName;
}

std::unique_ptr<AudioIOBase> AudioIOBase::ugAudioIO;

AudioIOExtBase::~AudioIOExtBase() = default;

AudioIOBase *AudioIOBase::Get()
{
   return ugAudioIO.get();
}

AudioIOBase::AudioIOBase() = default;

AudioIOBase::~AudioIOBase() = default;

void AudioIOBase::SetMixer(int inputSource)
{
#if defined(USE_PORTMIXER)
   int oldRecordSource = Px_GetCurrentInputSource(mPortMixer);
   if ( inputSource != oldRecordSource )
         Px_SetCurrentInputSource(mPortMixer, inputSource);
#endif
}

void AudioIOBase::HandleDeviceChange()
{
   // This should not happen, but it would screw things up if it did.
   // Vaughan, 2010-10-08: But it *did* happen, due to a bug, and nobody
   // caught it because this method just returned. Added wxASSERT().
   wxASSERT(!IsStreamActive());
   if (IsStreamActive())
      return;

   // get the selected record and playback devices
   const int playDeviceNum = getPlayDevIndex();
   const int recDeviceNum = getRecordDevIndex();

   // If no change needed, return
   if (mCachedPlaybackIndex == playDeviceNum &&
       mCachedCaptureIndex == recDeviceNum)
       return;

   // cache playback/capture rates
   mCachedPlaybackRates = GetSupportedPlaybackRates(playDeviceNum);
   mCachedCaptureRates = GetSupportedCaptureRates(recDeviceNum);
   mCachedSampleRates = GetSupportedSampleRates(playDeviceNum, recDeviceNum);
   mCachedPlaybackIndex = playDeviceNum;
   mCachedCaptureIndex = recDeviceNum;
   mCachedBestRateIn = 0.0;

#if defined(USE_PORTMIXER)

   // if we have a PortMixer object, close it down
   if (mPortMixer) {
      #if __WXMAC__
      // on the Mac we must make sure that we restore the hardware playthrough
      // state of the sound device to what it was before, because there isn't
      // a UI for this (!)
      if (Px_SupportsPlaythrough(mPortMixer) && mPreviousHWPlaythrough >= 0.0)
         Px_SetPlaythrough(mPortMixer, mPreviousHWPlaythrough);
         mPreviousHWPlaythrough = -1.0;
      #endif
      Px_CloseMixer(mPortMixer);
      mPortMixer = NULL;
   }

   // that might have given us no rates whatsoever, so we have to guess an
   // answer to do the next bit
   int numrates = mCachedSampleRates.size();
   int highestSampleRate;
   if (numrates > 0)
   {
      highestSampleRate = mCachedSampleRates[numrates - 1];
   }
   else
   {  // we don't actually have any rates that work for Rec and Play. Guess one
      // to use for messing with the mixer, which doesn't actually do either
      highestSampleRate = 44100;
      // mCachedSampleRates is still empty, but it's not used again, so
      // can ignore
   }
   mInputMixerWorks = false;
   mEmulateMixerOutputVol = true;
   mMixerOutputVol = 1.0;

   int error;
   // This tries to open the device with the samplerate worked out above, which
   // will be the highest available for play and record on the device, or
   // 44.1kHz if the info cannot be fetched.

   PaStream *stream;

   PaStreamParameters playbackParameters;

   playbackParameters.device = playDeviceNum;
   playbackParameters.sampleFormat = paFloat32;
   playbackParameters.hostApiSpecificStreamInfo = NULL;
   playbackParameters.channelCount = 1;
   if (Pa_GetDeviceInfo(playDeviceNum))
      playbackParameters.suggestedLatency =
         Pa_GetDeviceInfo(playDeviceNum)->defaultLowOutputLatency;
   else
      playbackParameters.suggestedLatency =
         AudioIOLatencyCorrection.GetDefault()/1000.0;

   PaStreamParameters captureParameters;

   captureParameters.device = recDeviceNum;
   captureParameters.sampleFormat = paFloat32;;
   captureParameters.hostApiSpecificStreamInfo = NULL;
   captureParameters.channelCount = 1;
   if (Pa_GetDeviceInfo(recDeviceNum))
      captureParameters.suggestedLatency =
         Pa_GetDeviceInfo(recDeviceNum)->defaultLowInputLatency;
   else
      captureParameters.suggestedLatency =
         AudioIOLatencyCorrection.GetDefault()/1000.0;

   // try opening for record and playback
   // Not really doing I/O so pass nullptr for the callback function
   error = Pa_OpenStream(&stream,
                         &captureParameters, &playbackParameters,
                         highestSampleRate, paFramesPerBufferUnspecified,
                         paClipOff | paDitherOff,
                         nullptr, NULL);

   if (!error) {
      // Try portmixer for this stream
      mPortMixer = Px_OpenMixer(stream, recDeviceNum, playDeviceNum, 0);
      if (!mPortMixer) {
         Pa_CloseStream(stream);
         error = true;
      }
   }

   // if that failed, try just for record
   if( error ) {
      error = Pa_OpenStream(&stream,
                            &captureParameters, NULL,
                            highestSampleRate, paFramesPerBufferUnspecified,
                            paClipOff | paDitherOff,
                            nullptr, NULL);

      if (!error) {
         mPortMixer = Px_OpenMixer(stream, recDeviceNum, playDeviceNum, 0);
         if (!mPortMixer) {
            Pa_CloseStream(stream);
            error = true;
         }
      }
   }

   // finally, try just for playback
   if ( error ) {
      error = Pa_OpenStream(&stream,
                            NULL, &playbackParameters,
                            highestSampleRate, paFramesPerBufferUnspecified,
                            paClipOff | paDitherOff,
                            nullptr, NULL);

      if (!error) {
         mPortMixer = Px_OpenMixer(stream, recDeviceNum, playDeviceNum, 0);
         if (!mPortMixer) {
            Pa_CloseStream(stream);
            error = true;
         }
      }
   }

   // FIXME: TRAP_ERR errors in HandleDeviceChange not reported.
   // if it's still not working, give up
   if( error )
      return;

   // Set input source
#if USE_PORTMIXER
   auto sourceIndex = AudioIORecordingSourceIndex.Read(); // defaults to -1
   if (sourceIndex >= 0) {
      //the current index of our source may be different because the stream
      //is a combination of two devices, so update it.
      sourceIndex = getRecordSourceIndex(mPortMixer);
      if (sourceIndex >= 0)
         SetMixer(sourceIndex);
   }
#endif

   // Determine mixer capabilities - if it doesn't support control of output
   // signal level, we emulate it (by multiplying this value by all outgoing
   // samples)

   mMixerOutputVol = Px_GetPCMOutputVolume(mPortMixer);
   mEmulateMixerOutputVol = false;
   Px_SetPCMOutputVolume(mPortMixer, 0.0);
   if (Px_GetPCMOutputVolume(mPortMixer) > 0.1)
      mEmulateMixerOutputVol = true;
   Px_SetPCMOutputVolume(mPortMixer, 0.2f);
   if (Px_GetPCMOutputVolume(mPortMixer) < 0.1 ||
       Px_GetPCMOutputVolume(mPortMixer) > 0.3)
      mEmulateMixerOutputVol = true;
   Px_SetPCMOutputVolume(mPortMixer, mMixerOutputVol);

   float inputVol = Px_GetInputVolume(mPortMixer);
   mInputMixerWorks = true;   // assume it works unless proved wrong
   Px_SetInputVolume(mPortMixer, 0.0);
   if (Px_GetInputVolume(mPortMixer) > 0.1)
      mInputMixerWorks = false;  // can't set to zero
   Px_SetInputVolume(mPortMixer, 0.2f);
   if (Px_GetInputVolume(mPortMixer) < 0.1 ||
       Px_GetInputVolume(mPortMixer) > 0.3)
      mInputMixerWorks = false;  // can't set level accurately
   Px_SetInputVolume(mPortMixer, inputVol);

   Pa_CloseStream(stream);


   #if 0
   wxPrintf("PortMixer: Playback: %s Recording: %s\n",
          mEmulateMixerOutputVol? "emulated": "native",
          mInputMixerWorks? "hardware": "no control");
   #endif

   mMixerOutputVol = 1.0;

#endif   // USE_PORTMIXER
}

void AudioIOBase::SetCaptureMeter(
   const std::shared_ptr<AudacityProject> &project, const std::weak_ptr<Meter> &wMeter)
{
   if (auto pOwningProject = mOwningProject.lock();
       ( pOwningProject ) && ( pOwningProject != project))
      return;

   auto meter = wMeter.lock();
   if (meter)
   {
      mInputMeter = meter;
      meter->Reset(mRate, true);
   }
   else
      mInputMeter.reset();
}

void AudioIOBase::SetPlaybackMeter(
   const std::shared_ptr<AudacityProject> &project, const std::weak_ptr<Meter> &wMeter)
{
   if (auto pOwningProject = mOwningProject.lock();
       ( pOwningProject ) && ( pOwningProject != project))
      return;

   auto meter = wMeter.lock();
   if (meter)
   {
      mOutputMeter = meter;
      meter->Reset(mRate, true);
   }
   else
      mOutputMeter.reset();
}

bool AudioIOBase::IsPaused() const
{
   return mPaused;
}

bool AudioIOBase::IsBusy() const
{
   if (mStreamToken != 0)
      return true;

   return false;
}

bool AudioIOBase::IsStreamActive() const
{
   bool isActive = false;
   // JKC: Not reporting any Pa error, but that looks OK.
   if( mPortStreamV19 )
      isActive = (Pa_IsStreamActive( mPortStreamV19 ) > 0);

   isActive = isActive ||
      std::any_of(mAudioIOExt.begin(), mAudioIOExt.end(),
         [](auto &pExt){ return pExt && pExt->IsOtherStreamActive(); });
   return isActive;
}

bool AudioIOBase::IsStreamActive(int token) const
{
   return (this->IsStreamActive() && this->IsAudioTokenActive(token));
}

bool AudioIOBase::IsAudioTokenActive(int token) const
{
   return ( token > 0 && token == mStreamToken );
}

bool AudioIOBase::IsMonitoring() const
{
   return ( mPortStreamV19 && mStreamToken==0 );
}

std::vector<long> AudioIOBase::GetSupportedPlaybackRates(int devIndex, double rate)
{
   if (devIndex == -1)
   {  // weren't given a device index, get the prefs / default one
      devIndex = getPlayDevIndex();
   }

   // Check if we can use the cached rates
   if (mCachedPlaybackIndex != -1 && devIndex == mCachedPlaybackIndex
         && (rate == 0.0 || make_iterator_range(mCachedPlaybackRates).contains(rate)))
   {
      return mCachedPlaybackRates;
   }

   std::vector<long> supported;
   int irate = (int)rate;
   const PaDeviceInfo* devInfo = NULL;
   int i;

   devInfo = Pa_GetDeviceInfo(devIndex);

   if (!devInfo)
   {
      wxLogDebug(wxT("GetSupportedPlaybackRates() Could not get device info!"));
      return supported;
   }

   // LLL: Remove when a proper method of determining actual supported
   //      DirectSound rate is devised.
   const PaHostApiInfo* hostInfo = Pa_GetHostApiInfo(devInfo->hostApi);
   bool isDirectSound = (hostInfo && hostInfo->type == paDirectSound);

   PaStreamParameters pars;

   pars.device = devIndex;
   pars.channelCount = 1;
   pars.sampleFormat = paFloat32;
   pars.suggestedLatency = devInfo->defaultHighOutputLatency;
   pars.hostApiSpecificStreamInfo = NULL;

   // JKC: PortAudio Errors handled OK here.  No need to report them
   for (i = 0; i < NumRatesToTry; i++)
   {
      // LLL: Remove when a proper method of determining actual supported
      //      DirectSound rate is devised.
      if (!(isDirectSound && RatesToTry[i] > 200000)){
         if (Pa_IsFormatSupported(NULL, &pars, RatesToTry[i]) == 0)
            supported.push_back(RatesToTry[i]);
         Pa_Sleep( 10 );// There are ALSA drivers that don't like being probed
         // too quickly.
      }
   }

   if (irate != 0 && !make_iterator_range(supported).contains(irate))
   {
      // LLL: Remove when a proper method of determining actual supported
      //      DirectSound rate is devised.
      if (!(isDirectSound && RatesToTry[i] > 200000))
         if (Pa_IsFormatSupported(NULL, &pars, irate) == 0)
            supported.push_back(irate);
   }

   return supported;
}

std::vector<long> AudioIOBase::GetSupportedCaptureRates(int devIndex, double rate)
{
   if (devIndex == -1)
   {  // not given a device, look up in prefs / default
      devIndex = getRecordDevIndex();
   }

   // Check if we can use the cached rates
   if (mCachedCaptureIndex != -1 && devIndex == mCachedCaptureIndex
         && (rate == 0.0 || make_iterator_range(mCachedCaptureRates).contains(rate)))
   {
      return mCachedCaptureRates;
   }

   std::vector<long> supported;
   int irate = (int)rate;
   const PaDeviceInfo* devInfo = NULL;
   int i;

   devInfo = Pa_GetDeviceInfo(devIndex);

   if (!devInfo)
   {
      wxLogDebug(wxT("GetSupportedCaptureRates() Could not get device info!"));
      return supported;
   }

   auto latencyDuration = AudioIOLatencyDuration.Read();
   // Why not defaulting to 2 as elsewhere?
   auto recordChannels = AudioIORecordChannels.ReadWithDefault(1);

   // LLL: Remove when a proper method of determining actual supported
   //      DirectSound rate is devised.
   const PaHostApiInfo* hostInfo = Pa_GetHostApiInfo(devInfo->hostApi);
   bool isDirectSound = (hostInfo && hostInfo->type == paDirectSound);

   PaStreamParameters pars;

   pars.device = devIndex;
   pars.channelCount = recordChannels;
   pars.sampleFormat = paFloat32;
   pars.suggestedLatency = latencyDuration / 1000.0;
   pars.hostApiSpecificStreamInfo = NULL;

   for (i = 0; i < NumRatesToTry; i++)
   {
      // LLL: Remove when a proper method of determining actual supported
      //      DirectSound rate is devised.
      if (!(isDirectSound && RatesToTry[i] > 200000))
      {
         if (Pa_IsFormatSupported(&pars, NULL, RatesToTry[i]) == 0)
            supported.push_back(RatesToTry[i]);
         Pa_Sleep( 10 );// There are ALSA drivers that don't like being probed
         // too quickly.
      }
   }

   if (irate != 0 && !make_iterator_range(supported).contains(irate))
   {
      // LLL: Remove when a proper method of determining actual supported
      //      DirectSound rate is devised.
      if (!(isDirectSound && RatesToTry[i] > 200000))
         if (Pa_IsFormatSupported(&pars, NULL, irate) == 0)
            supported.push_back(irate);
   }

   return supported;
}

std::vector<long> AudioIOBase::GetSupportedSampleRates(
   int playDevice, int recDevice, double rate)
{
   // Not given device indices, look up prefs
   if (playDevice == -1) {
      playDevice = getPlayDevIndex();
   }
   if (recDevice == -1) {
      recDevice = getRecordDevIndex();
   }

   // Check if we can use the cached rates
   if (mCachedPlaybackIndex != -1 && mCachedCaptureIndex != -1 &&
         playDevice == mCachedPlaybackIndex &&
         recDevice == mCachedCaptureIndex &&
         (rate == 0.0 || make_iterator_range(mCachedSampleRates).contains(rate)))
   {
      return mCachedSampleRates;
   }

   auto playback = GetSupportedPlaybackRates(playDevice, rate);
   auto capture = GetSupportedCaptureRates(recDevice, rate);
   int i;

   // Return only sample rates which are in both arrays
   std::vector<long> result;

   for (i = 0; i < (int)playback.size(); i++)
      if (make_iterator_range(capture).contains(playback[i]))
         result.push_back(playback[i]);

   // If this yields no results, use the default sample rates nevertheless
/*   if (result.empty())
   {
      for (i = 0; i < NumStandardRates; i++)
         result.push_back(StandardRates[i]);
   }*/

   return result;
}

/** \todo: should this take into account PortAudio's value for
 * PaDeviceInfo::defaultSampleRate? In principal this should let us work out
 * which rates are "real" and which resampled in the drivers, and so prefer
 * the real rates. */
int AudioIOBase::GetOptimalSupportedSampleRate()
{
   auto rates = GetSupportedSampleRates();

   if (make_iterator_range(rates).contains(44100))
      return 44100;

   if (make_iterator_range(rates).contains(48000))
      return 48000;

   // if there are no supported rates, the next bit crashes. So check first,
   // and give them a "sensible" value if there are no valid values. They
   // will still get an error later, but with any luck may have changed
   // something by then. It's no worse than having an invalid default rate
   // stored in the preferences, which we don't check for
   if (rates.empty()) return 44100;

   return rates.back();
}

#if USE_PORTMIXER
int AudioIOBase::getRecordSourceIndex(PxMixer *portMixer)
{
   int i;
   auto sourceName = AudioIORecordingSource.Read();
   int numSources = Px_GetNumInputSources(portMixer);
   for (i = 0; i < numSources; i++) {
      if (sourceName == wxString(wxSafeConvertMB2WX(Px_GetInputSourceName(portMixer, i))))
         return i;
   }
   return -1;
}
#endif

int AudioIOBase::getPlayDevIndex(const wxString &devNameArg)
{
   wxString devName(devNameArg);
   // if we don't get given a device, look up the preferences
   if (devName.empty())
      devName = AudioIOPlaybackDevice.Read();

   auto hostName = AudioIOHost.Read();
   PaHostApiIndex hostCnt = Pa_GetHostApiCount();
   PaHostApiIndex hostNum;
   for (hostNum = 0; hostNum < hostCnt; hostNum++)
   {
      const PaHostApiInfo *hinfo = Pa_GetHostApiInfo(hostNum);
      if (hinfo && wxString(wxSafeConvertMB2WX(hinfo->name)) == hostName)
      {
         for (PaDeviceIndex hostDevice = 0; hostDevice < hinfo->deviceCount; hostDevice++)
         {
            PaDeviceIndex deviceNum = Pa_HostApiDeviceIndexToDeviceIndex(hostNum, hostDevice);

            const PaDeviceInfo *dinfo = Pa_GetDeviceInfo(deviceNum);
            if (dinfo && DeviceName(dinfo) == devName && dinfo->maxOutputChannels > 0 )
            {
               // this device name matches the stored one, and works.
               // So we say this is the answer and return it
               return deviceNum;
            }
         }

         // The device wasn't found so use the default for this host.
         // LL:  At this point, preferences and active no longer match.
         return hinfo->defaultOutputDevice;
      }
   }

   // The host wasn't found, so use the default output device.
   // FIXME: TRAP_ERR PaErrorCode not handled well (this code is similar to input code
   // and the input side has more comments.)

   PaDeviceIndex deviceNum = Pa_GetDefaultOutputDevice();

   // Sometimes PortAudio returns -1 if it cannot find a suitable default
   // device, so we just use the first one available
   //
   // LL:  At this point, preferences and active no longer match
   //
   //      And I can't imagine how far we'll get specifying an "invalid" index later
   //      on...are we certain "0" even exists?
   if (deviceNum < 0) {
      wxASSERT(false);
      deviceNum = 0;
   }

   return deviceNum;
}

int AudioIOBase::getRecordDevIndex(const wxString &devNameArg)
{
   wxString devName(devNameArg);
   // if we don't get given a device, look up the preferences
   if (devName.empty())
      devName = AudioIORecordingDevice.Read();

   auto hostName = AudioIOHost.Read();
   PaHostApiIndex hostCnt = Pa_GetHostApiCount();
   PaHostApiIndex hostNum;
   for (hostNum = 0; hostNum < hostCnt; hostNum++)
   {
      const PaHostApiInfo *hinfo = Pa_GetHostApiInfo(hostNum);
      if (hinfo && wxString(wxSafeConvertMB2WX(hinfo->name)) == hostName)
      {
         for (PaDeviceIndex hostDevice = 0; hostDevice < hinfo->deviceCount; hostDevice++)
         {
            PaDeviceIndex deviceNum = Pa_HostApiDeviceIndexToDeviceIndex(hostNum, hostDevice);

            const PaDeviceInfo *dinfo = Pa_GetDeviceInfo(deviceNum);
            if (dinfo && DeviceName(dinfo) == devName && dinfo->maxInputChannels > 0 )
            {
               // this device name matches the stored one, and works.
               // So we say this is the answer and return it
               return deviceNum;
            }
         }

         // The device wasn't found so use the default for this host.
         // LL:  At this point, preferences and active no longer match.
         return hinfo->defaultInputDevice;
      }
   }

   // The host wasn't found, so use the default input device.
   // FIXME: TRAP_ERR PaErrorCode not handled well in getRecordDevIndex()
   PaDeviceIndex deviceNum = Pa_GetDefaultInputDevice();

   // Sometimes PortAudio returns -1 if it cannot find a suitable default
   // device, so we just use the first one available
   // PortAudio has an error reporting function.  We should log/report the error?
   //
   // LL:  At this point, preferences and active no longer match
   //
   //      And I can't imagine how far we'll get specifying an "invalid" index later
   //      on...are we certain "0" even exists?
   if (deviceNum < 0) {
      // JKC: This ASSERT will happen if you run with no config file
      // This happens once.  Config file will exist on the next run.
      // TODO: Look into this a bit more.  Could be relevant to blank Device Toolbar.
      wxASSERT(false);
      deviceNum = 0;
   }

   return deviceNum;
}

wxString AudioIOBase::GetDeviceInfo() const
{
   wxStringOutputStream o;
   wxTextOutputStream s(o, wxEOL_UNIX);

   if (IsStreamActive()) {
      return XO("Stream is active ... unable to gather information.\n")
         .Translation();
   }


   // FIXME: TRAP_ERR PaErrorCode not handled.  3 instances in GetDeviceInfo().
   int recDeviceNum = Pa_GetDefaultInputDevice();
   int playDeviceNum = Pa_GetDefaultOutputDevice();
   int cnt = Pa_GetDeviceCount();

   // PRL:  why only into the log?
   wxLogDebug(wxT("Portaudio reports %d audio devices"),cnt);
   
   s << wxT("==============================\n");
   s << XO("Default recording device number: %d\n").Format( recDeviceNum );
   s << XO("Default playback device number: %d\n").Format( playDeviceNum);

   auto recDevice = AudioIORecordingDevice.Read();
   auto playDevice = AudioIOPlaybackDevice.Read();
   int j;

   // This gets info on all available audio devices (input and output)
   if (cnt <= 0) {
      s << XO("No devices found\n");
      return o.GetString();
   }

   const PaDeviceInfo* info;

   for (j = 0; j < cnt; j++) {
      s << wxT("==============================\n");

      info = Pa_GetDeviceInfo(j);
      if (!info) {
         s << XO("Device info unavailable for: %d\n").Format( j );
         continue;
      }

      wxString name = DeviceName(info);
      s << XO("Device ID: %d\n").Format( j );
      s << XO("Device name: %s\n").Format( name );
      s << XO("Host name: %s\n").Format( HostName(info) );
      s << XO("Recording channels: %d\n").Format( info->maxInputChannels );
      s << XO("Playback channels: %d\n").Format( info->maxOutputChannels );
      s << XO("Low Recording Latency: %g\n").Format( info->defaultLowInputLatency );
      s << XO("Low Playback Latency: %g\n").Format( info->defaultLowOutputLatency );
      s << XO("High Recording Latency: %g\n").Format( info->defaultHighInputLatency );
      s << XO("High Playback Latency: %g\n").Format( info->defaultHighOutputLatency );

      auto rates = GetSupportedPlaybackRates(j, 0.0);

      /* i18n-hint: Supported, meaning made available by the system */
      s << XO("Supported Rates:\n");
      for (int k = 0; k < (int) rates.size(); k++) {
         s << wxT("    ") << (int)rates[k] << wxT("\n");
      }

      if (name == playDevice && info->maxOutputChannels > 0)
         playDeviceNum = j;

      if (name == recDevice && info->maxInputChannels > 0)
         recDeviceNum = j;

      // Sometimes PortAudio returns -1 if it cannot find a suitable default
      // device, so we just use the first one available
      if (recDeviceNum < 0 && info->maxInputChannels > 0){
         recDeviceNum = j;
      }
      if (playDeviceNum < 0 && info->maxOutputChannels > 0){
         playDeviceNum = j;
      }
   }

   bool haveRecDevice = (recDeviceNum >= 0);
   bool havePlayDevice = (playDeviceNum >= 0);

   s << wxT("==============================\n");
   if (haveRecDevice)
      s << XO("Selected recording device: %d - %s\n").Format( recDeviceNum, recDevice );
   else
      s << XO("No recording device found for '%s'.\n").Format( recDevice );

   if (havePlayDevice)
      s << XO("Selected playback device: %d - %s\n").Format( playDeviceNum, playDevice );
   else
      s << XO("No playback device found for '%s'.\n").Format( playDevice );

   std::vector<long> supportedSampleRates;

   if (havePlayDevice && haveRecDevice) {
      supportedSampleRates = GetSupportedSampleRates(playDeviceNum, recDeviceNum);

      s << XO("Supported Rates:\n");
      for (int k = 0; k < (int) supportedSampleRates.size(); k++) {
         s << wxT("    ") << (int)supportedSampleRates[k] << wxT("\n");
      }
   }
   else {
      s << XO("Cannot check mutual sample rates without both devices.\n");
      return o.GetString();
   }

#if defined(USE_PORTMIXER)
   if (supportedSampleRates.size() > 0)
      {
      int highestSampleRate = supportedSampleRates.back();
      bool EmulateMixerInputVol = true;
      bool EmulateMixerOutputVol = true;
      float MixerInputVol = 1.0;
      float MixerOutputVol = 1.0;

      int error;

      PaStream *stream;

      PaStreamParameters playbackParameters;

      playbackParameters.device = playDeviceNum;
      playbackParameters.sampleFormat = paFloat32;
      playbackParameters.hostApiSpecificStreamInfo = NULL;
      playbackParameters.channelCount = 1;
      if (Pa_GetDeviceInfo(playDeviceNum)){
         playbackParameters.suggestedLatency =
            Pa_GetDeviceInfo(playDeviceNum)->defaultLowOutputLatency;
      }
      else
         playbackParameters.suggestedLatency =
            AudioIOLatencyCorrection.GetDefault()/1000.0;

      PaStreamParameters captureParameters;

      captureParameters.device = recDeviceNum;
      captureParameters.sampleFormat = paFloat32;;
      captureParameters.hostApiSpecificStreamInfo = NULL;
      captureParameters.channelCount = 1;
      if (Pa_GetDeviceInfo(recDeviceNum)){
         captureParameters.suggestedLatency =
            Pa_GetDeviceInfo(recDeviceNum)->defaultLowInputLatency;
      }
      else
         captureParameters.suggestedLatency =
            AudioIOLatencyCorrection.GetDefault()/1000.0;

      // Not really doing I/O so pass nullptr for the callback function
      error = Pa_OpenStream(&stream,
                         &captureParameters, &playbackParameters,
                         highestSampleRate, paFramesPerBufferUnspecified,
                         paClipOff | paDitherOff,
                         nullptr, NULL);

      if (error) {
         error = Pa_OpenStream(&stream,
                            &captureParameters, NULL,
                            highestSampleRate, paFramesPerBufferUnspecified,
                            paClipOff | paDitherOff,
                            nullptr, NULL);
      }

      if (error) {
         s << XO("Received %d while opening devices\n").Format( error );
         return o.GetString();
      }

      PxMixer *PortMixer = Px_OpenMixer(stream, recDeviceNum, playDeviceNum, 0);

      if (!PortMixer) {
         s << XO("Unable to open Portmixer\n");
         Pa_CloseStream(stream);
         return o.GetString();
      }

      s << wxT("==============================\n");
      s << XO("Available mixers:\n");

      // FIXME: ? PortMixer errors on query not reported in GetDeviceInfo
      cnt = Px_GetNumMixers(stream);
      for (int i = 0; i < cnt; i++) {
         wxString name = wxSafeConvertMB2WX(Px_GetMixerName(stream, i));
         s << XO("%d - %s\n").Format( i, name );
      }

      s << wxT("==============================\n");
      s << XO("Available recording sources:\n");
      cnt = Px_GetNumInputSources(PortMixer);
      for (int i = 0; i < cnt; i++) {
         wxString name = wxSafeConvertMB2WX(Px_GetInputSourceName(PortMixer, i));
         s << XO("%d - %s\n").Format( i, name );
      }

      s << wxT("==============================\n");
      s << XO("Available playback volumes:\n");
      cnt = Px_GetNumOutputVolumes(PortMixer);
      for (int i = 0; i < cnt; i++) {
         wxString name = wxSafeConvertMB2WX(Px_GetOutputVolumeName(PortMixer, i));
         s << XO("%d - %s\n").Format( i, name );
      }

      // Determine mixer capabilities - if it doesn't support either
      // input or output, we emulate them (by multiplying this value
      // by all incoming/outgoing samples)

      MixerOutputVol = Px_GetPCMOutputVolume(PortMixer);
      EmulateMixerOutputVol = false;
      Px_SetPCMOutputVolume(PortMixer, 0.0);
      if (Px_GetPCMOutputVolume(PortMixer) > 0.1)
         EmulateMixerOutputVol = true;
      Px_SetPCMOutputVolume(PortMixer, 0.2f);
      if (Px_GetPCMOutputVolume(PortMixer) < 0.1 ||
          Px_GetPCMOutputVolume(PortMixer) > 0.3)
         EmulateMixerOutputVol = true;
      Px_SetPCMOutputVolume(PortMixer, MixerOutputVol);

      MixerInputVol = Px_GetInputVolume(PortMixer);
      EmulateMixerInputVol = false;
      Px_SetInputVolume(PortMixer, 0.0);
      if (Px_GetInputVolume(PortMixer) > 0.1)
         EmulateMixerInputVol = true;
      Px_SetInputVolume(PortMixer, 0.2f);
      if (Px_GetInputVolume(PortMixer) < 0.1 ||
          Px_GetInputVolume(PortMixer) > 0.3)
         EmulateMixerInputVol = true;
      Px_SetInputVolume(PortMixer, MixerInputVol);

      Pa_CloseStream(stream);

      s << wxT("==============================\n");
      s << ( EmulateMixerInputVol
         ? XO("Recording volume is emulated\n")
         : XO("Recording volume is native\n") );
      s << ( EmulateMixerOutputVol
         ? XO("Playback volume is emulated\n")
         : XO("Playback volume is native\n") );

      Px_CloseMixer(PortMixer);

      }  //end of massive if statement if a valid sample rate has been found
#endif
   return o.GetString();
}

auto AudioIOBase::GetAllDeviceInfo() -> std::vector<AudioIODiagnostics>
{
   std::vector<AudioIODiagnostics> result;
   result.push_back({
      wxT("audiodev.txt"), GetDeviceInfo(), wxT("Audio Device Info") });
   for( auto &pExt : mAudioIOExt )
      if ( pExt )
         result.emplace_back(pExt->Dump());
   return result;
}

StringSetting AudioIOHost{
   L"/AudioIO/Host", L"" };
DoubleSetting AudioIOLatencyCorrection{
   L"/AudioIO/LatencyCorrection", -130.0 };
DoubleSetting AudioIOLatencyDuration{
   L"/AudioIO/LatencyDuration", 100.0 };
StringSetting AudioIOPlaybackDevice{
   L"/AudioIO/PlaybackDevice", L"" };
IntSetting AudioIORecordChannels{
   L"/AudioIO/RecordChannels", 2 };
StringSetting AudioIORecordingDevice{
   L"/AudioIO/RecordingDevice", L"" };
StringSetting AudioIORecordingSource{
   L"/AudioIO/RecordingSource", L"" };
IntSetting AudioIORecordingSourceIndex{
   L"/AudioIO/RecordingSourceIndex", -1 };
