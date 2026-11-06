/*
* Audacity: A Digital Audio Editor
*/
#include "jucemonitoringcallback.h"

#include <algorithm>
#include <chrono>

#include "framework/global/log.h"

using namespace au::au4juce;

JuceMonitoringCallback::JuceMonitoringCallback(AudacityProject& project, std::unique_ptr<juce::AudioIODevice> device, bool playThrough,
                                               audio::IAudioMeterPtr meter, const std::atomic<float>& inputVolume)
    : project{project}, playThrough{playThrough}, m_device{std::move(device)}, m_sampleRate{m_device->getCurrentSampleRate()},
    m_meter{std::move(meter)}, m_inputVolume{inputVolume},
    m_input{m_device->getActiveInputChannels().countNumberOfSetBits(), std::max(1, m_device->getCurrentBufferSizeSamples())},
    m_inputPointers(static_cast<size_t>(m_input.numChannels()))
{
}

JuceMonitoringCallback::~JuceMonitoringCallback()
{
    stop();
}

void JuceMonitoringCallback::start()
{
    if (m_meter) {
        m_meter->start(m_sampleRate);
    }
    m_device->start(this);
}

void JuceMonitoringCallback::stop()
{
    if (m_device) {
        m_device->stop();
        m_device->close();
        m_device.reset();
    }
    if (m_meter) {
        m_meter->stop();
    }
}

void JuceMonitoringCallback::audioDeviceIOCallbackWithContext(const float* const* inputChannelData, int numInputChannels,
                                                        float* const* outputChannelData, int numOutputChannels, int numSamples,
                                                        const juce::AudioIODeviceCallbackContext&)
{
    for (int ch = 0; ch < numOutputChannels; ++ch) {
        if (outputChannelData[ch]) {
            std::fill_n(outputChannelData[ch], numSamples, 0.f);
        }
    }
    if (numInputChannels < m_input.numChannels()) {
        return;
    }

    // The device may call back with more frames than it said it would
    const auto now = std::chrono::steady_clock::now();
    for (int done = 0; done < numSamples;) {
        const auto frames = std::min(numSamples - done, std::max(1, m_device->getCurrentBufferSizeSamples()));
        for (int ch = 0; ch < m_input.numChannels(); ++ch) {
            m_inputPointers[ch] = inputChannelData[ch] ? inputChannelData[ch] + done : nullptr;
        }
        m_input.read(m_inputPointers.data(), frames, m_inputVolume.load(std::memory_order_relaxed));
        if (m_meter) {
            m_input.sendToMeter(*m_meter, {}, now);
        }
        if (playThrough) {
            m_input.playThrough(outputChannelData, numOutputChannels, done);
        }
        done += frames;
    }
}

void JuceMonitoringCallback::audioDeviceError(const juce::String& errorMessage)
{
    LOGE() << "audio device error: " << errorMessage.toStdString();
}
