/**********************************************************************

Audacity: A Digital Audio Editor

AudioIOBase.cpp

Paul Licameli split from AudioIO.cpp

**********************************************************************/

#include "AudioIOBase.h"

#include <cassert>

#include <wx/log.h>
#include <wx/sstream.h>
#include <wx/txtstrm.h>

#include "au3-utility/IteratorX.h"
#include "au3-preferences/Prefs.h"
#include "DeviceManager.h"

#include "portaudio.h"

#if USE_PORTMIXER
#include "portmixer.h"
#endif

int AudioIOBase::mCurrentPlaybackIndex { -1 };
int AudioIOBase::mCurrentCaptureIndex { -1 };
double AudioIOBase::mCachedBestRateIn { 0.0 };

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

AudioIOBase* AudioIOBase::Get()
{
    return ugAudioIO.get();
}

AudioIOBase::AudioIOBase() = default;

AudioIOBase::~AudioIOBase() = default;

void AudioIOBase::SetMixer(int inputSource)
{
#if defined(USE_PORTMIXER)
    int oldRecordSource = Px_GetCurrentInputSource(mPortMixer);
    if (inputSource != oldRecordSource) {
        Px_SetCurrentInputSource(mPortMixer, inputSource);
    }
#endif
}

void AudioIOBase::HandleDeviceChange()
{
    // This should not happen, but it would screw things up if it did.
    // Vaughan, 2010-10-08: But it *did* happen, due to a bug, and nobody
    // caught it because this method just returned. Added wxASSERT().
    wxASSERT(!IsStreamActive());
    if (IsStreamActive()) {
        return;
    }

    // get the selected record and playback devices
    const int playDeviceNum = DeviceManager::getPlayDevIndex();
    const int recDeviceNum = DeviceManager::getRecordDevIndex();

    // If no change needed, return
    if (mCurrentPlaybackIndex == playDeviceNum
        && mCurrentCaptureIndex == recDeviceNum) {
        return;
    }

    // Update playback/capture device indices
    mCurrentPlaybackIndex = playDeviceNum;
    mCurrentCaptureIndex = recDeviceNum;
    mCachedBestRateIn = 0.0;

#if defined(USE_PORTMIXER)

    // if we have a PortMixer object, close it down
    if (mPortMixer) {
      #if __WXMAC__
        // on the Mac we must make sure that we restore the hardware playthrough
        // state of the sound device to what it was before, because there isn't
        // a UI for this (!)
        if (Px_SupportsPlaythrough(mPortMixer) && mPreviousHWPlaythrough >= 0.0) {
            Px_SetPlaythrough(mPortMixer, mPreviousHWPlaythrough);
        }
        mPreviousHWPlaythrough = -1.0;
      #endif
        Px_CloseMixer(mPortMixer);
        mPortMixer = NULL;
    }

    // portmixer does not support asio
    if (DeviceManager::IsAsioDevice(playDeviceNum)
        || DeviceManager::IsAsioDevice(recDeviceNum)) {
        mInputMixerWorks = false;
        return;
    }

    // Looking for highest supported sample rate for a given
    // play/rec device pair
    long highestSampleRate = DeviceManager::GetClosestSupportedSampleRate(playDeviceNum, recDeviceNum, INT_MAX);
    if (highestSampleRate == 0) {
        // we don't actually have any rates that work for Rec and Play. Guess one
        // to use for messing with the mixer, which doesn't actually do either
        highestSampleRate = 44100;
    }

    mInputMixerWorks = false;

    int error;
    // This tries to open the device with the samplerate worked out above, which
    // will be the highest available for play and record on the device, or
    // 44.1kHz if the info cannot be fetched.

    PaStream* stream;

    PaStreamParameters playbackParameters;

    playbackParameters.device = playDeviceNum;
    playbackParameters.sampleFormat = paFloat32;
    playbackParameters.hostApiSpecificStreamInfo = NULL;
    playbackParameters.channelCount = 1;
    if (Pa_GetDeviceInfo(playDeviceNum)) {
        playbackParameters.suggestedLatency
            =Pa_GetDeviceInfo(playDeviceNum)->defaultLowOutputLatency;
    } else {
        playbackParameters.suggestedLatency = AudioIOLatencyCompensation.GetDefault() / 1000.0;
    }

    PaStreamParameters captureParameters;

    captureParameters.device = recDeviceNum;
    captureParameters.sampleFormat = paFloat32;
    captureParameters.hostApiSpecificStreamInfo = NULL;
    captureParameters.channelCount = 1;
    if (Pa_GetDeviceInfo(recDeviceNum)) {
        captureParameters.suggestedLatency
            =Pa_GetDeviceInfo(recDeviceNum)->defaultLowInputLatency;
    } else {
        captureParameters.suggestedLatency = AudioIOLatencyCompensation.GetDefault() / 1000.0;
    }

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
    if (error) {
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
    if (error) {
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
    if (error) {
        return;
    }

    // Set input source
#if USE_PORTMIXER
    auto sourceIndex = AudioIORecordingSourceIndex.Read(); // defaults to -1
    if (sourceIndex >= 0) {
        //the current index of our source may be different because the stream
        //is a combination of two devices, so update it.
        sourceIndex = getRecordSourceIndex(mPortMixer);
        if (sourceIndex >= 0) {
            SetMixer(sourceIndex);
        }
    }
#endif

    // Determine mixer capabilities - if it doesn't support control of output
    // signal level, we emulate it (by multiplying this value by all outgoing
    // samples)

    float inputVol = Px_GetInputVolume(mPortMixer);
    mInputMixerWorks = true;  // assume it works unless proved wrong
    Px_SetInputVolume(mPortMixer, 0.0);
    if (Px_GetInputVolume(mPortMixer) > 0.1) {
        mInputMixerWorks = false; // can't set to zero
    }
    Px_SetInputVolume(mPortMixer, 0.2f);
    if (Px_GetInputVolume(mPortMixer) < 0.1
        || Px_GetInputVolume(mPortMixer) > 0.3) {
        mInputMixerWorks = false; // can't set level accurately
    }
    Px_SetInputVolume(mPortMixer, inputVol);

    Pa_CloseStream(stream);

   #if 0
    wxPrintf("PortMixer: Recording: %s\n"
             mInputMixerWorks ? "hardware" : "no control");
   #endif
#endif   // USE_PORTMIXER
}

void AudioIOBase::StartMeters()
{
    if (auto pInputMeter = mInputMeter.lock()) {
        pInputMeter->start(mRate);
    }
    if (auto pOutputMeter = mOutputMeter.lock()) {
        pOutputMeter->start(mRate);
    }
}

void AudioIOBase::StopMeters()
{
    if (auto pInputMeter = mInputMeter.lock()) {
        pInputMeter->stop();
    }
    if (auto pOutputMeter = mOutputMeter.lock()) {
        pOutputMeter->stop();
    }
}

void AudioIOBase::ResetMeters()
{
    mInputMeter.reset();
    mOutputMeter.reset();
}

void AudioIOBase::SetCaptureMeter(
    const std::shared_ptr<AudacityProject>& project, const std::weak_ptr<IMeterSender>& wMeter)
{
    if (auto pOwningProject = mOwningProject.lock();
        (pOwningProject) && (pOwningProject != project)) {
        return;
    }

    auto meter = wMeter.lock();
    if (meter) {
        mInputMeter = meter;
    } else {
        mInputMeter.reset();
    }
}

void AudioIOBase::SetPlaybackMeter(
    const std::shared_ptr<AudacityProject>& project, const std::weak_ptr<IMeterSender>& wMeter)
{
    if (auto pOwningProject = mOwningProject.lock();
        (pOwningProject) && (pOwningProject != project)) {
        return;
    }

    auto meter = wMeter.lock();
    if (meter) {
        mOutputMeter = meter;
    } else {
        mOutputMeter.reset();
    }
}

bool AudioIOBase::IsPaused() const
{
    return mPaused.load(std::memory_order_relaxed);
}

bool AudioIOBase::IsBusy() const
{
    if (mStreamToken != 0) {
        return true;
    }

    return false;
}

bool AudioIOBase::IsStreamActive() const
{
    bool isActive = false;
    // JKC: Not reporting any Pa error, but that looks OK.
    if (mPortStreamV19) {
        isActive = (Pa_IsStreamActive(mPortStreamV19) > 0);
    }

    isActive = isActive
               || std::any_of(mAudioIOExt.begin(), mAudioIOExt.end(),
                              [](auto& pExt){ return pExt && pExt->IsOtherStreamActive(); });
    return isActive;
}

bool AudioIOBase::IsStreamActive(int token) const
{
    return this->IsStreamActive() && this->IsAudioTokenActive(token);
}

bool AudioIOBase::IsAudioTokenActive(int token) const
{
    return token > 0 && token == mStreamToken;
}

bool AudioIOBase::IsMonitoring() const
{
    return mPortStreamV19 && mStreamToken == 0;
}

/** \todo: should this take into account PortAudio's value for
 * PaDeviceInfo::defaultSampleRate? In principal this should let us work out
 * which rates are "real" and which resampled in the drivers, and so prefer
 * the real rates. */
#if USE_PORTMIXER
int AudioIOBase::getRecordSourceIndex(PxMixer* portMixer)
{
    int i;
    auto sourceName = AudioIORecordingSource.Read();
    int numSources = Px_GetNumInputSources(portMixer);
    for (i = 0; i < numSources; i++) {
        if (sourceName == wxString(wxSafeConvertMB2WX(Px_GetInputSourceName(portMixer, i)))) {
            return i;
        }
    }
    return -1;
}

#endif

wxString AudioIOBase::GetDeviceInfo() const
{
    wxStringOutputStream o;
    wxTextOutputStream s(o, wxEOL_UNIX);

    if (IsStreamActive()) {
        return TranslatableString("audio-devices", "Stream is active … unable to gather information.\n")
               .Translation();
    }

    // FIXME: TRAP_ERR PaErrorCode not handled.  3 instances in GetDeviceInfo().
    int recDeviceNum = Pa_GetDefaultInputDevice();
    int playDeviceNum = Pa_GetDefaultOutputDevice();
    int cnt = Pa_GetDeviceCount();

    // PRL:  why only into the log?
    wxLogDebug(wxT("Portaudio reports %d audio devices"), cnt);

    s << wxT("==============================\n");
    s << TranslatableString("audio-devices", "Default recording device number: %1\n").arg(recDeviceNum);
    s << TranslatableString("audio-devices", "Default playback device number: %1\n").arg(playDeviceNum);

    auto recDevice = AudioIORecordingDevice.Read();
    auto playDevice = AudioIOPlaybackDevice.Read();
    int j;

    // This gets info on all available audio devices (input and output)
    if (cnt <= 0) {
        s << TranslatableString("audio-devices", "No devices found\n");
        return o.GetString();
    }

    const PaDeviceInfo* info;

    for (j = 0; j < cnt; j++) {
        s << wxT("==============================\n");

        info = Pa_GetDeviceInfo(j);
        if (!info) {
            //: %1 is the device index number
            s << TranslatableString("audio-devices", "Device info unavailable for: %1\n").arg(j);
            continue;
        }

        wxString name = DeviceName(info);
        s << TranslatableString("audio-devices", "Device ID: %1\n").arg(j);
        s << TranslatableString("audio-devices", "Device name: %1\n").arg(name);
        s << TranslatableString("audio-devices", "Host name: %1\n").arg(HostName(info));
        s << TranslatableString("audio-devices", "Recording channels: %1\n").arg(info->maxInputChannels);
        s << TranslatableString("audio-devices", "Playback channels: %1\n").arg(info->maxOutputChannels);
        s << TranslatableString("audio-devices", "Low Recording Latency: %1\n").arg(info->defaultLowInputLatency);
        s << TranslatableString("audio-devices", "Low Playback Latency: %1\n").arg(info->defaultLowOutputLatency);
        s << TranslatableString("audio-devices", "High Recording Latency: %1\n").arg(info->defaultHighInputLatency);
        s << TranslatableString("audio-devices", "High Playback Latency: %1\n").arg(info->defaultHighOutputLatency);

        if (info->maxOutputChannels) {
            auto rates = DeviceManager::GetSupportedPlaybackRates(j);

            /*: Supported, meaning made available by the system */
            s << TranslatableString("audio-devices", "Supported Playback Rates:\n");
            for (int k = 0; k < (int)rates.size(); k++) {
                s << wxT("    ") << (int)rates[k] << wxT("\n");
            }
        }

        if (info->maxInputChannels) {
            auto rates = DeviceManager::GetSupportedCaptureRates(j);

            /*: Supported, meaning made available by the system */
            s << TranslatableString("audio-devices", "Supported Capture Rates:\n");
            for (int k = 0; k < (int)rates.size(); k++) {
                s << wxT("    ") << (int)rates[k] << wxT("\n");
            }
        }

        if (name == playDevice && info->maxOutputChannels > 0) {
            playDeviceNum = j;
        }

        if (name == recDevice && info->maxInputChannels > 0) {
            recDeviceNum = j;
        }

        // Sometimes PortAudio returns -1 if it cannot find a suitable default
        // device, so we just use the first one available
        if (recDeviceNum < 0 && info->maxInputChannels > 0) {
            recDeviceNum = j;
        }
        if (playDeviceNum < 0 && info->maxOutputChannels > 0) {
            playDeviceNum = j;
        }
    }

    bool haveRecDevice = (recDeviceNum >= 0);
    bool havePlayDevice = (playDeviceNum >= 0);

    s << wxT("==============================\n");
    if (haveRecDevice) {
        //: %1 is the recording device number, %2 is the device name
        s << TranslatableString("audio-devices", "Selected recording device: %1 - %2\n").arg(recDeviceNum).arg(recDevice);
    } else {
        s << TranslatableString("audio-devices", "No recording device found for “%1”.\n").arg(recDevice);
    }

    if (havePlayDevice) {
        //: %1 is the playback device number, %2 is the device name
        s << TranslatableString("audio-devices", "Selected playback device: %1 - %2\n").arg(playDeviceNum).arg(playDevice);
    } else {
        s << TranslatableString("audio-devices", "No playback device found for “%1”.\n").arg(playDevice);
    }

    std::vector<long> supportedSampleRates;

    if (havePlayDevice && haveRecDevice) {
        supportedSampleRates = DeviceManager::GetSupportedSampleRates(playDeviceNum, recDeviceNum);

        s << TranslatableString("audio-devices", "Supported Rates:\n");
        for (int k = 0; k < (int)supportedSampleRates.size(); k++) {
            s << wxT("    ") << (int)supportedSampleRates[k] << wxT("\n");
        }
    } else {
        s << TranslatableString("audio-devices", "Cannot check mutual sample rates without both devices.\n");
        return o.GetString();
    }

#if defined(USE_PORTMIXER)
    if (supportedSampleRates.size() > 0) {
        int highestSampleRate = supportedSampleRates.back();
        bool EmulateMixerInputVol = true;
        float MixerInputVol = 1.0;
        float MixerOutputVol = 1.0;

        int error;

        PaStream* stream;

        PaStreamParameters playbackParameters;

        playbackParameters.device = playDeviceNum;
        playbackParameters.sampleFormat = paFloat32;
        playbackParameters.hostApiSpecificStreamInfo = NULL;
        playbackParameters.channelCount = 1;
        if (Pa_GetDeviceInfo(playDeviceNum)) {
            playbackParameters.suggestedLatency
                =Pa_GetDeviceInfo(playDeviceNum)->defaultLowOutputLatency;
        } else {
            playbackParameters.suggestedLatency = AudioIOLatencyCompensation.GetDefault() / 1000.0;
        }

        PaStreamParameters captureParameters;

        captureParameters.device = recDeviceNum;
        captureParameters.sampleFormat = paFloat32;
        captureParameters.hostApiSpecificStreamInfo = NULL;
        captureParameters.channelCount = 1;
        if (Pa_GetDeviceInfo(recDeviceNum)) {
            captureParameters.suggestedLatency
                =Pa_GetDeviceInfo(recDeviceNum)->defaultLowInputLatency;
        } else {
            captureParameters.suggestedLatency = AudioIOLatencyCompensation.GetDefault() / 1000.0;
        }

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
            //: %1 is a PortAudio error code returned while opening the devices
            s << TranslatableString("audio-devices", "Received %1 while opening devices\n").arg(error);
            return o.GetString();
        }

        PxMixer* PortMixer = Px_OpenMixer(stream, recDeviceNum, playDeviceNum, 0);

        if (!PortMixer) {
            s << TranslatableString("audio-devices", "Unable to open Portmixer\n");
            Pa_CloseStream(stream);
            return o.GetString();
        }

        s << wxT("==============================\n");
        s << TranslatableString("audio-devices", "Available mixers:\n");

        // FIXME: ? PortMixer errors on query not reported in GetDeviceInfo
        cnt = Px_GetNumMixers(stream);
        for (int i = 0; i < cnt; i++) {
            wxString name = wxSafeConvertMB2WX(Px_GetMixerName(stream, i));
            //: %1 is the mixer index number, %2 is the mixer name
            s << TranslatableString("audio-devices", "%1 - %2\n").arg(i).arg(name);
        }

        s << wxT("==============================\n");
        s << TranslatableString("audio-devices", "Available recording sources:\n");
        cnt = Px_GetNumInputSources(PortMixer);
        for (int i = 0; i < cnt; i++) {
            wxString name = wxSafeConvertMB2WX(Px_GetInputSourceName(PortMixer, i));
            //: %1 is the recording source index number, %2 is the source name
            s << TranslatableString("audio-devices", "%1 - %2\n").arg(i).arg(name);
        }

        s << wxT("==============================\n");
        s << TranslatableString("audio-devices", "Available playback volumes:\n");
        cnt = Px_GetNumOutputVolumes(PortMixer);
        for (int i = 0; i < cnt; i++) {
            wxString name = wxSafeConvertMB2WX(Px_GetOutputVolumeName(PortMixer, i));
            //: %1 is the playback volume index number, %2 is the volume control name
            s << TranslatableString("audio-devices", "%1 - %2\n").arg(i).arg(name);
        }

        // Check, if PortMixer supports adjusting input levels on the interface

        MixerInputVol = Px_GetInputVolume(PortMixer);
        EmulateMixerInputVol = false;
        Px_SetInputVolume(PortMixer, 0.0);
        if (Px_GetInputVolume(PortMixer) > 0.1) {
            EmulateMixerInputVol = true;
        }
        Px_SetInputVolume(PortMixer, 0.2f);
        if (Px_GetInputVolume(PortMixer) < 0.1
            || Px_GetInputVolume(PortMixer) > 0.3) {
            EmulateMixerInputVol = true;
        }
        Px_SetInputVolume(PortMixer, MixerInputVol);

        Pa_CloseStream(stream);

        s << wxT("==============================\n");
        s << (EmulateMixerInputVol
              ? TranslatableString("audio-devices", "Recording volume is emulated\n")
              : TranslatableString("audio-devices", "Recording volume is native\n"));

        Px_CloseMixer(PortMixer);
    }    //end of massive if statement if a valid sample rate has been found
#endif
    return o.GetString();
}

auto AudioIOBase::GetAllDeviceInfo() -> std::vector<AudioIODiagnostics>
{
    std::vector<AudioIODiagnostics> result;
    result.push_back({
        wxT("audiodev.txt"), GetDeviceInfo(), wxT("Audio Device Info") });
    for ( auto& pExt : mAudioIOExt ) {
        if (pExt) {
            result.emplace_back(pExt->Dump());
        }
    }
    return result;
}
