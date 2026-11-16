/*
 * Audacity: A Digital Audio Editor
 */
#include "PortAudioStream.h"

#include <chrono>
#include <thread>

#include <wx/log.h>

#include "AudioDeviceSettings.h"
#include "DeviceManager.h"
#include "au3-preferences/Prefs.h"

#ifdef __WXMSW__
#include "pa_win_wasapi.h"
#endif

namespace PortAudioStream {
PaError Prepare(const Request& request, Parameters& parameters)
{
    parameters = {};
    parameters.rate = request.rate;
    // Bidirectional streams are used when recording while overdub or software playthrough options are enabled
    const bool isUnsupportedSampleRate = request.numCaptureChannels > 0 && request.numPlaybackChannels > 0 && request.rate == 0.0;
    const auto latencyDuration = AudioIOLatencyDuration.Read();

    if (request.numPlaybackChannels > 0) {
        parameters.usePlayback = true;
        // this sets the device index to whatever is "right" based on preferences, then defaults
        parameters.playback.device = DeviceManager::getPlayDevIndex();
        const PaDeviceInfo* playbackDeviceInfo = Pa_GetDeviceInfo(parameters.playback.device);
        if (playbackDeviceInfo == nullptr || playbackDeviceInfo->maxOutputChannels <= 0) {
            return paInvalidDevice;
        }
        parameters.playback.sampleFormat = request.playbackFormat;
        parameters.playback.hostApiSpecificStreamInfo = nullptr;
        parameters.playback.channelCount = static_cast<int>(request.numPlaybackChannels);

        const PaHostApiInfo* hostInfo = Pa_GetHostApiInfo(playbackDeviceInfo->hostApi);
        const bool isWASAPI = hostInfo && hostInfo->type == paWASAPI;
        const bool isMME = hostInfo && hostInfo->type == paMME;
        // If the host API is WASAPI, the stream is bidirectional and there is no supported sample rate, enable the WASAPI
        // Sample Rate Conversion for the playback device.
        parameters.wasapiAutoConvert = isWASAPI && isUnsupportedSampleRate;

        if (request.softwarePlaythrough && !isMME) {
            parameters.playback.suggestedLatency = playbackDeviceInfo->defaultLowOutputLatency;
        } else {
            // When using WASAPI, the suggested latency does not affect
            // the latency of the playback, but the position of playback is given as if
            // there was the suggested latency. This results in the last "suggested latency"
            // of a selection not being played. So for WASAPI use 0.0 for the suggested
            // latency regardless of user setting. See bug 1949.
            parameters.playback.suggestedLatency = isWASAPI ? 0.0 : latencyDuration / 1000.0;
        }
    }

    if (request.numCaptureChannels > 0) {
        parameters.useCapture = true;
        // retrieve the index of the device set in the prefs, or a sensible default if it isn't set/valid
        parameters.capture.device = DeviceManager::getRecordDevIndex();
        const PaDeviceInfo* captureDeviceInfo = Pa_GetDeviceInfo(parameters.capture.device);
        if (captureDeviceInfo == nullptr || captureDeviceInfo->maxInputChannels <= 0) {
            return paInvalidDevice;
        }
        const PaHostApiInfo* hostInfo = Pa_GetHostApiInfo(captureDeviceInfo->hostApi);
        const bool isWASAPI = hostInfo && hostInfo->type == paWASAPI;
        const bool isMME = hostInfo && hostInfo->type == paMME;
        // If the stream is bidirectional and there is no supported sample rate, use the rate the capture device supports
        if (isWASAPI && isUnsupportedSampleRate) {
            parameters.rate = captureDeviceInfo->defaultSampleRate;
        }
        parameters.capture.sampleFormat = request.captureFormat;
        parameters.capture.hostApiSpecificStreamInfo = nullptr;
        parameters.capture.channelCount = static_cast<int>(request.numCaptureChannels);
        if (request.softwarePlaythrough && !isMME) {
            parameters.capture.suggestedLatency = captureDeviceInfo->defaultHighInputLatency;
        } else {
            parameters.capture.suggestedLatency = latencyDuration / 1000.0;
        }
    }

    const auto deviceInfo = parameters.usePlayback ? Pa_GetDeviceInfo(parameters.playback.device)
                            : Pa_GetDeviceInfo(parameters.capture.device);
    if (deviceInfo != nullptr) {
        if (const auto hostApiInfo = Pa_GetHostApiInfo(deviceInfo->hostApi)) {
            parameters.usingAlsa = hostApiInfo->type == paALSA;
            parameters.usingJack = hostApiInfo->type == paJACK;
        }
    }
    return paNoError;
}

PaError Open(const Parameters& parameters, PaStreamCallback* callback, void* userData, Opened& opened)
{
    opened = {};
    auto playback = parameters.playback;
#ifdef __WXMSW__
    PaWasapiStreamInfo wasapiStreamInfo {};
    if (parameters.wasapiAutoConvert) {
        wasapiStreamInfo.size = sizeof(PaWasapiStreamInfo);
        wasapiStreamInfo.hostApiType = paWASAPI;
        wasapiStreamInfo.version = 1;
        wasapiStreamInfo.flags = paWinWasapiAutoConvert;
        playback.hostApiSpecificStreamInfo = &wasapiStreamInfo;
    }
#endif

    // (Linux, bug 1885) After scanning devices it takes a little time for the
    // ALSA device to be available, so allow retries.
    // On my test machine, no more than 3 attempts are required.
    unsigned int maxTries = 1;
#ifdef __WXGTK__
    {
        using namespace std::chrono;
        if (DeviceManager::Instance()->GetTimeSinceRescan() < 10s) {
            maxTries = 5;
        }
    }
#endif

    PaError error = paNoError;
    for (unsigned int tries = 0; tries < maxTries; tries++) {
        error = Pa_OpenStream(&opened.stream,
                              parameters.useCapture ? &parameters.capture : nullptr,
                              parameters.usePlayback ? &playback : nullptr,
                              parameters.rate, paFramesPerBufferUnspecified,
                              paNoFlag,
                              callback, userData);
        if (error == paNoError) {
            const auto info = Pa_GetStreamInfo(opened.stream);
            opened.inputLatency = info->inputLatency;
            // When using Jack as a host, PA calculates the wrong latency
            // if a non system port is used. Assume, that Jack provides a very
            // low latency, lower than user requested
            // (https://github.com/audacity/audacity/issues/4646)
            opened.outputLatency = parameters.usingJack ? AudioIOLatencyDuration.Read() / 1000.0 : info->outputLatency;
            break;
        }
        opened.stream = nullptr;
        wxLogDebug("Attempt %u to open capture stream failed with: %d", 1 + tries, error);
        using namespace std::chrono;
        std::this_thread::sleep_for(1s);
    }
    return error;
}

double AsioRate(bool playing)
{
    // Opening ASIO streams at the device's current rate allows simultaneous playback with drivers that support it
    if (gPrefs->ReadBool(wxT("/AudioIO/ASIO/UseDeviceSampleRate"), true)) {
        const int asioDevIndex = playing ? DeviceManager::getPlayDevIndex() : DeviceManager::getRecordDevIndex();
        if (DeviceManager::IsAsioDevice(asioDevIndex)) {
            return DeviceManager::GetAsioDeviceCurrentSampleRate(asioDevIndex);
        }
    }
    return 0.0;
}

double BestRate(bool capturing, bool playing, double requested)
{
    if (const auto asioRate = AsioRate(playing); asioRate != 0.0) {
        return asioRate;
    }
    const auto rate = static_cast<long>(requested);
    if (capturing && !playing) {
        return DeviceManager::GetClosestSupportedCaptureRate(-1, rate);
    }
    if (playing && !capturing) {
        return DeviceManager::GetClosestSupportedPlaybackRate(-1, rate);
    }
    return DeviceManager::GetClosestSupportedSampleRate(-1, -1, rate);
}
}
