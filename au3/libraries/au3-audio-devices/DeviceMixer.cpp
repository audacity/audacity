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

DeviceMixer::DeviceMixer()
{
    mPortMixer = NULL;
    mPreviousHWPlaythrough = -1.0;
    mInputMixerWorks = false;
}

DeviceMixer::~DeviceMixer()
{
    Close();
}

void DeviceMixer::Open(int playDeviceNum, int recDeviceNum)
{
#if defined(USE_PORTMIXER)

    // if we have a PortMixer object, close it down
    Close();

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
        playbackParameters.suggestedLatency = AudioDeviceSettings::LatencyCompensation.GetDefault() / 1000.0;
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
        captureParameters.suggestedLatency = AudioDeviceSettings::LatencyCompensation.GetDefault() / 1000.0;
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
    auto sourceIndex = AudioDeviceSettings::RecordingSourceIndex.Read(); // defaults to -1
    if (sourceIndex >= 0) {
        //the current index of our source may be different because the stream
        //is a combination of two devices, so update it.
        sourceIndex = getRecordSourceIndex(mPortMixer);
        if (sourceIndex >= 0) {
            SetInputSource(sourceIndex);
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
#else
    (void)playDeviceNum;
    (void)recDeviceNum;
#endif   // USE_PORTMIXER
}

void DeviceMixer::Close()
{
#if defined(USE_PORTMIXER)
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
#endif
}

bool DeviceMixer::IsOpen() const
{
#if defined(USE_PORTMIXER)
    return mPortMixer != NULL;
#else
    return false;
#endif
}

float DeviceMixer::GetInputVolume() const
{
#if defined(USE_PORTMIXER)
    return Px_GetInputVolume(mPortMixer);
#else
    return 1.0f;
#endif
}

void DeviceMixer::SetInputVolume(float volume)
{
#if defined(USE_PORTMIXER)
    Px_SetInputVolume(mPortMixer, volume);
#else
    (void)volume;
#endif
}

int DeviceMixer::GetInputSource() const
{
#if defined(USE_PORTMIXER)
    return Px_GetCurrentInputSource(mPortMixer);
#else
    return -1;
#endif
}

void DeviceMixer::SetInputSource(int inputSource)
{
#if defined(USE_PORTMIXER)
    int oldRecordSource = Px_GetCurrentInputSource(mPortMixer);
    if (inputSource != oldRecordSource) {
        Px_SetCurrentInputSource(mPortMixer, inputSource);
    }
#else
    (void)inputSource;
#endif
}

wxArrayString DeviceMixer::GetInputSourceNames() const
{
#if defined(USE_PORTMIXER)

    wxArrayString deviceNames;

    if (mPortMixer) {
        int numSources = Px_GetNumInputSources(mPortMixer);
        for ( int source = 0; source < numSources; source++ ) {
            deviceNames.push_back(wxString(wxSafeConvertMB2WX(Px_GetInputSourceName(mPortMixer, source))));
        }
    } else {
        wxLogDebug(wxT("DeviceMixer::GetInputSourceNames(): PortMixer not initialised!"));
    }

    return deviceNames;

#else

    wxArrayString blank;

    return blank;

#endif
}

void DeviceMixer::DisableHardwarePlaythrough()
{
#if USE_PORTMIXER
      #ifdef __WXMAC__
    if (mPortMixer) {
        if (Px_SupportsPlaythrough(mPortMixer)) {
            bool playthrough = false;

            mPreviousHWPlaythrough = Px_GetPlaythrough(mPortMixer);

            // Bug 388.  Feature not supported.
            //gPrefs->Read(wxT("/AudioIO/Playthrough"), &playthrough, false);
            if (playthrough) {
                Px_SetPlaythrough(mPortMixer, 1.0);
            } else {
                Px_SetPlaythrough(mPortMixer, 0.0);
            }
        }
    }
      #endif
#endif
}

void DeviceMixer::RestoreHardwarePlaythrough()
{
  #if defined(USE_PORTMIXER)
    if (mPortMixer) {
      #if __WXMAC__
        if (Px_SupportsPlaythrough(mPortMixer) && mPreviousHWPlaythrough >= 0.0) {
            Px_SetPlaythrough(mPortMixer, mPreviousHWPlaythrough);
        }
        mPreviousHWPlaythrough = -1.0;
      #endif
    }
  #endif
}

#if USE_PORTMIXER
int DeviceMixer::getRecordSourceIndex(PxMixer* portMixer)
{
    int i;
    auto sourceName = AudioDeviceSettings::RecordingSource.Read();
    int numSources = Px_GetNumInputSources(portMixer);
    for (i = 0; i < numSources; i++) {
        if (sourceName == wxString(wxSafeConvertMB2WX(Px_GetInputSourceName(portMixer, i)))) {
            return i;
        }
    }
    return -1;
}

#endif
