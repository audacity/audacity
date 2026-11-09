/*
* Audacity: A Digital Audio Editor
*/
#include "monitoringcallback.h"

#include <algorithm>
#include <chrono>

#include "framework/global/log.h"

using namespace au::au4audio;

MonitoringCallback::MonitoringCallback(AudacityProject& project, std::unique_ptr<audio::IDevice> device, bool playThrough,
                                       audio::IAudioMeterPtr meter, const std::atomic<float>& inputVolume)
    : project{project}, playThrough{playThrough}, m_device{std::move(device)}, m_sampleRate{m_device->sampleRate()},
    m_meter{std::move(meter)}, m_inputVolume{inputVolume},
    m_input{m_device->numInputChannels(), m_device->blockSize()},
    m_inputPointers(static_cast<size_t>(m_input.numChannels()))
{
}

MonitoringCallback::~MonitoringCallback()
{
    stop();
}

void MonitoringCallback::start()
{
    if (m_meter) {
        m_meter->start(m_sampleRate);
    }
    m_device->start(*this);
}

void MonitoringCallback::stop()
{
    if (m_device) {
        m_device->stop();
        m_device.reset();
    }
    if (m_meter) {
        m_meter->stop();
    }
}

void MonitoringCallback::process(const float* const* inputChannelData, int numInputChannels, float* const* outputChannelData,
                                 int numOutputChannels, int numSamples)
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
        const auto frames = std::min(numSamples - done, m_device->blockSize());
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
