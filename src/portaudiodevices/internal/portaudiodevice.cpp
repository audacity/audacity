/*
 * Audacity: A Digital Audio Editor
 */
#include "portaudiodevice.h"

#include <algorithm>

#include "au3-audio-devices/DeviceManager.h"
#include "au3-audio-devices/PortAudioStream.h"

using namespace au::portaudiodevices;

namespace {
//! PortAudio calls back with as many frames as the host likes, which it doesn't tell: the callbacks process them by as many
constexpr int processingBlockSize = 512;
}

PortAudioDevice::PortAudioDevice(DeviceMixer& mixer)
    : m_mixer{mixer}
{
}

PortAudioDevice::~PortAudioDevice()
{
    stop();
    if (m_stream) {
        Pa_CloseStream(m_stream);
    }
}

bool PortAudioDevice::open(int numInputChannels, int numOutputChannels, double sampleRate, std::string& error)
{
    PortAudioStream::Request request;
    request.numCaptureChannels = static_cast<unsigned>(std::max(0, numInputChannels));
    request.numPlaybackChannels = static_cast<unsigned>(std::max(0, numOutputChannels));
    // A buffer per channel, as the callbacks take them
    request.captureFormat = paFloat32 | paNonInterleaved;
    request.playbackFormat = paFloat32 | paNonInterleaved;
    request.rate = PortAudioStream::BestRate(numInputChannels > 0, numOutputChannels > 0, sampleRate);

    PortAudioStream::Parameters parameters;
    auto paError = PortAudioStream::Prepare(request, parameters);
    if (paError != paNoError) {
        error = Pa_GetErrorText(paError);
        return false;
    }
    // As many of the channels as the devices have
    if (parameters.useCapture) {
        const auto info = Pa_GetDeviceInfo(parameters.capture.device);
        parameters.capture.channelCount = std::min(parameters.capture.channelCount, info->maxInputChannels);
    }
    if (parameters.usePlayback) {
        const auto info = Pa_GetDeviceInfo(parameters.playback.device);
        parameters.playback.channelCount = std::min(parameters.playback.channelCount, info->maxOutputChannels);
    }

#ifdef __WXMSW__
    // As AudioIO: Pa_OpenStream resets the input volume on Windows
    const auto inputVolume = m_mixer.GetInputVolume();
#endif
    PortAudioStream::Opened opened;
    paError = PortAudioStream::Open(parameters, &PortAudioDevice::callback, this, opened);
#ifdef __WXMSW__
    m_mixer.SetInputVolume(inputVolume);
#endif
    if (paError != paNoError) {
        error = Pa_GetErrorText(paError);
        return false;
    }
    m_stream = opened.stream;
    m_numInputChannels = parameters.useCapture ? parameters.capture.channelCount : 0;
    m_numOutputChannels = parameters.usePlayback ? parameters.playback.channelCount : 0;
    m_inputLatency = opened.inputLatency;
    m_outputLatency = opened.outputLatency;
    return true;
}

double PortAudioDevice::sampleRate() const
{
    return Pa_GetStreamInfo(m_stream)->sampleRate;
}

int PortAudioDevice::blockSize() const
{
    return processingBlockSize;
}

void PortAudioDevice::start(audio::IDeviceCallback& callback)
{
    m_callback = &callback;
    m_started = Pa_StartStream(m_stream) == paNoError;
    if (m_started) {
        m_mixer.DisableHardwarePlaythrough();
    }
}

void PortAudioDevice::stop()
{
    if (m_started) {
        // Returns once the callback returned
        Pa_StopStream(m_stream);
        m_started = false;
        m_mixer.RestoreHardwarePlaythrough();
    }
}

int PortAudioDevice::callback(const void* input, void* output, unsigned long frames, const PaStreamCallbackTimeInfo*,
                              PaStreamCallbackFlags flags, void* userData)
{
    auto& device = *static_cast<PortAudioDevice*>(userData);
    if (flags & (paOutputUnderflow | paInputOverflow)) {
        ++device.m_xruns;
    }
    // Non-interleaved: arrays of a buffer per channel
    const auto inputs = static_cast<const float* const*>(input);
    const auto outputs = static_cast<float* const*>(output);
    device.m_callback->process(inputs, inputs ? device.m_numInputChannels : 0, outputs, outputs ? device.m_numOutputChannels : 0,
                               static_cast<int>(frames));
    return paContinue;
}

std::unique_ptr<au::audio::IDevice> PortAudioDeviceOpener::open(const audio::AudioConfiguration&, int numInputChannels,
                                                                int numOutputChannels, double sampleRate, std::string& error)
{
    auto device = std::make_unique<PortAudioDevice>(m_hardwareVolume.deviceMixer());
    if (!device->open(numInputChannels, numOutputChannels, sampleRate, error)) {
        return nullptr;
    }
    return device;
}

void PortAudioHardwareVolume::reopen()
{
    m_mixer.Open(DeviceManager::getPlayDevIndex(), DeviceManager::getRecordDevIndex());
    m_opened = true;
}

bool PortAudioHardwareVolume::inputVolumeWorks() const
{
    openOnce();
    return m_mixer.InputVolumeWorks();
}

float PortAudioHardwareVolume::inputVolume() const
{
    openOnce();
    return m_mixer.GetInputVolume();
}

void PortAudioHardwareVolume::setInputVolume(float volume)
{
    openOnce();
    m_mixer.SetInputVolume(volume);
}

DeviceMixer& PortAudioHardwareVolume::deviceMixer()
{
    openOnce();
    return m_mixer;
}

void PortAudioHardwareVolume::openOnce() const
{
    // Not before the devices are known
    if (!m_opened) {
        m_mixer.Open(DeviceManager::getPlayDevIndex(), DeviceManager::getRecordDevIndex());
        m_opened = true;
    }
}
