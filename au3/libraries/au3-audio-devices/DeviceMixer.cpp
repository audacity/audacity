/*
 * Audacity: A Digital Audio Editor
 */
#include "DeviceMixer.h"

#include <climits>

#include <wx/log.h>

#include "AudioDeviceSettings.h"
#include "DeviceManager.h"

#include "portaudio.h"

#if USE_PORTMIXER
#include "portmixer.h"
#endif

DeviceMixer::~DeviceMixer()
{
    Close();
}

void DeviceMixer::Open(int playDevice, int recDevice)
{
#if USE_PORTMIXER
    Close();

    // portmixer does not support asio
    if (DeviceManager::IsAsioDevice(playDevice) || DeviceManager::IsAsioDevice(recDevice)) {
        mInputVolumeWorks = false;
        return;
    }

    // Looking for highest supported sample rate for a given
    // play/rec device pair
    long highestSampleRate = DeviceManager::GetClosestSupportedSampleRate(playDevice, recDevice, INT_MAX);
    if (highestSampleRate == 0) {
        // we don't actually have any rates that work for Rec and Play. Guess one
        // to use for messing with the mixer, which doesn't actually do either
        highestSampleRate = 44100;
    }

    mInputVolumeWorks = false;

    int error;
    // This tries to open the device with the samplerate worked out above, which
    // will be the highest available for play and record on the device, or
    // 44.1kHz if the info cannot be fetched.

    PaStream* stream;

    PaStreamParameters playbackParameters;

    playbackParameters.device = playDevice;
    playbackParameters.sampleFormat = paFloat32;
    playbackParameters.hostApiSpecificStreamInfo = NULL;
    playbackParameters.channelCount = 1;
    if (Pa_GetDeviceInfo(playDevice)) {
        playbackParameters.suggestedLatency
            =Pa_GetDeviceInfo(playDevice)->defaultLowOutputLatency;
    } else {
        playbackParameters.suggestedLatency = AudioIOLatencyCompensation.GetDefault() / 1000.0;
    }

    PaStreamParameters captureParameters;

    captureParameters.device = recDevice;
    captureParameters.sampleFormat = paFloat32;
    captureParameters.hostApiSpecificStreamInfo = NULL;
    captureParameters.channelCount = 1;
    if (Pa_GetDeviceInfo(recDevice)) {
        captureParameters.suggestedLatency
            =Pa_GetDeviceInfo(recDevice)->defaultLowInputLatency;
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
        mMixer = Px_OpenMixer(stream, recDevice, playDevice, 0);
        if (!mMixer) {
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
            mMixer = Px_OpenMixer(stream, recDevice, playDevice, 0);
            if (!mMixer) {
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
            mMixer = Px_OpenMixer(stream, recDevice, playDevice, 0);
            if (!mMixer) {
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
    auto sourceIndex = AudioIORecordingSourceIndex.Read(); // defaults to -1
    if (sourceIndex >= 0) {
        //the current index of our source may be different because the stream
        //is a combination of two devices, so update it.
        sourceIndex = GetSettingsInputSource();
        if (sourceIndex >= 0) {
            SetInputSource(sourceIndex);
        }
    }

    // Determine mixer capabilities - if it doesn't support control of output
    // signal level, we emulate it (by multiplying this value by all outgoing
    // samples)

    float inputVol = Px_GetInputVolume(mMixer);
    mInputVolumeWorks = true;  // assume it works unless proved wrong
    Px_SetInputVolume(mMixer, 0.0);
    if (Px_GetInputVolume(mMixer) > 0.1) {
        mInputVolumeWorks = false; // can't set to zero
    }
    Px_SetInputVolume(mMixer, 0.2f);
    if (Px_GetInputVolume(mMixer) < 0.1
        || Px_GetInputVolume(mMixer) > 0.3) {
        mInputVolumeWorks = false; // can't set level accurately
    }
    Px_SetInputVolume(mMixer, inputVol);

    Pa_CloseStream(stream);
#else
    (void)playDevice;
    (void)recDevice;
    mInputVolumeWorks = false;
#endif
}

void DeviceMixer::Close()
{
#if USE_PORTMIXER
    if (mMixer) {
        RestoreHardwarePlaythrough();
        Px_CloseMixer(mMixer);
        mMixer = nullptr;
    }
#endif
    mInputVolumeWorks = false;
}

float DeviceMixer::GetInputVolume() const
{
#if USE_PORTMIXER
    if (mMixer) {
        return Px_GetInputVolume(mMixer);
    }
#endif
    return 1.0f;
}

void DeviceMixer::SetInputVolume(float volume)
{
#if USE_PORTMIXER
    if (mMixer) {
        Px_SetInputVolume(mMixer, volume);
    }
#else
    (void)volume;
#endif
}

int DeviceMixer::GetInputSource() const
{
#if USE_PORTMIXER
    if (mMixer) {
        return Px_GetCurrentInputSource(mMixer);
    }
#endif
    return -1;
}

void DeviceMixer::SetInputSource(int source)
{
#if USE_PORTMIXER
    if (mMixer && source != Px_GetCurrentInputSource(mMixer)) {
        Px_SetCurrentInputSource(mMixer, source);
    }
#else
    (void)source;
#endif
}

wxArrayString DeviceMixer::GetInputSourceNames() const
{
    wxArrayString names;
#if USE_PORTMIXER
    if (mMixer) {
        const int numSources = Px_GetNumInputSources(mMixer);
        for (int source = 0; source < numSources; source++) {
            names.push_back(wxString(wxSafeConvertMB2WX(Px_GetInputSourceName(mMixer, source))));
        }
    } else {
        wxLogDebug(wxT("DeviceMixer::GetInputSourceNames(): PortMixer not initialised!"));
    }
#endif
    return names;
}

void DeviceMixer::DisableHardwarePlaythrough()
{
#if USE_PORTMIXER && defined(__WXMAC__)
    if (mMixer && Px_SupportsPlaythrough(mMixer)) {
        mPreviousHardwarePlaythrough = Px_GetPlaythrough(mMixer);

        // Bug 388.  Feature not supported.
        //gPrefs->Read(wxT("/AudioIO/Playthrough"), &playthrough, false);
        Px_SetPlaythrough(mMixer, 0.0);
    }
#endif
}

void DeviceMixer::RestoreHardwarePlaythrough()
{
#if USE_PORTMIXER && defined(__WXMAC__)
    // on the Mac we must make sure that we restore the hardware playthrough
    // state of the sound device to what it was before, because there isn't
    // a UI for this (!)
    if (mMixer && Px_SupportsPlaythrough(mMixer) && mPreviousHardwarePlaythrough >= 0.0) {
        Px_SetPlaythrough(mMixer, mPreviousHardwarePlaythrough);
    }
    mPreviousHardwarePlaythrough = -1.0f;
#endif
}

int DeviceMixer::GetSettingsInputSource() const
{
#if USE_PORTMIXER
    auto sourceName = AudioIORecordingSource.Read();
    int numSources = Px_GetNumInputSources(mMixer);
    for (int i = 0; i < numSources; i++) {
        if (sourceName == wxString(wxSafeConvertMB2WX(Px_GetInputSourceName(mMixer, i)))) {
            return i;
        }
    }
#endif
    return -1;
}
