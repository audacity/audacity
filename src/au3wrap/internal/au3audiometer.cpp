/*
* Audacity: A Digital Audio Editor
*/

#include <algorithm>
#include <cmath>

#include "au3audiometer.h"

#include "global/async/async.h"

#include "libraries/lib-utility/MemoryX.h"

namespace {
constexpr double METER_SAMPLE_PERIOD = 0.10;
constexpr double MAX_ALLOWED_TIMESTAMP_DIFF = 1.0;
}

namespace au::au3 {
Meter::Sample Meter::getSamplesMaxValue(const float* buffer, size_t frames, size_t step)
{
    auto sptr = buffer;
    float peak = 0.0f;
    float rms = 0.0f;

    for (unsigned long i = 0; i < frames; i++) {
        peak = std::max(peak, static_cast<float>(std::fabs(*sptr)));
        rms += (*sptr) * (*sptr);
        sptr += step;
    }

    rms = std::sqrt(rms / static_cast<float>(frames));

    return Meter::Sample{ std::min(peak, 1.0f), std::min(rms, 1.0f) };
}

void Meter::push(uint8_t channel, const IMeterSender::InterleavedSampleData& sampleData, int64_t key)
{
    if (m_lastSampleTimestamp.find(channel) == m_lastSampleTimestamp.end()) {
        // First time seeing this channel, initialize the timestamp and max values.
        m_lastSampleTimestamp[channel] = sampleData.firstSampleTimestamp;
        m_maxValue[channel] = Sample{ 0.0, 0.0 };
    }

    if (std::abs(sampleData.firstSampleTimestamp - *m_lastSampleTimestamp[channel]) > MAX_ALLOWED_TIMESTAMP_DIFF) {
        // If the timestamp difference is too large the stream has likely been stopped and restarted.
        // Reset the peak and RMS values to avoid displaying old data.
        m_lastSampleTimestamp[channel] = sampleData.firstSampleTimestamp;
        m_maxValue[channel] = Sample{ 0.0, 0.0 };
    }

    const auto value = getSamplesMaxValue(sampleData.buffer, sampleData.frames, sampleData.nChannels);
    if (value.peak > m_maxValue[channel].peak) {
        m_maxValue[channel] = value;
    }

    const double lastSampleTimestamp = sampleData.firstSampleTimestamp + sampleData.frames * (1 / m_sampleRate);
    if (lastSampleTimestamp > *m_lastSampleTimestamp[channel] + METER_SAMPLE_PERIOD) {
        // Send data every METER_SAMPLE_PERIOD seconds approximately.
        push(channel, m_maxValue[channel], key);
        m_maxValue[channel] = Sample{ 0.0, 0.0 };
        m_lastSampleTimestamp[channel] = lastSampleTimestamp;
    }
}

void Meter::push(uint8_t channel, const Sample& sample, int64_t key)
{
    m_trackData.push_back(Data { key, channel,
                                 au::audio::AudioSignalVal { sample.peak, static_cast<au::audio::volume_dbfs_t>(LINEAR_TO_DB(
                                                                                                                    sample.peak)) },
                                 au::audio::AudioSignalVal { sample.rms,
                                                             static_cast<au::audio::volume_dbfs_t>(LINEAR_TO_DB(sample.rms)) } });
}

void Meter::reset()
{
    // The reset is deferred to ensure these zeroed values are the last ones processed
    muse::async::Async::call(this, [this]() {
        for (auto& [key, _] : m_channels) {
            push(0, { 0.0, 0.0 }, key);
            push(1, { 0.0, 0.0 }, key);
        }
        sendAll();
    });
}

void Meter::reserve(size_t size)
{
    m_trackData.reserve(size);
}

void Meter::sendAll()
{
    for (const auto& item : m_trackData) {
        const auto it = m_channels.find(item.key);
        if (it != m_channels.end()) {
            const au::audio::MeterSignal signal { item.peak, item.rms };
            it->second.send(item.channel, signal);
        }
    }
    m_trackData = {};
}

void Meter::setSampleRate(double rate)
{
    m_sampleRate = rate;
}

muse::async::Channel<au::audio::audioch_t, au::audio::MeterSignal> Meter::dataChanged(int64_t key)
{
    auto channel = m_channels[key];
    channel.onClose(this, [this, key]() {
        m_channels.erase(key);
    });

    return channel;
}
}
