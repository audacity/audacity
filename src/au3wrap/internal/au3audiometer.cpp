/*
* Audacity: A Digital Audio Editor
*/

#include <algorithm>
#include <cmath>

#include "au3audiometer.h"

#include "libraries/lib-utility/MemoryX.h"

namespace {
IMeterSender::Sample GetAbsValue(const float* buffer, size_t frames, size_t step)
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

    return IMeterSender::Sample{ std::min(peak, 1.0f), std::min(rms, 1.0f) };
}
}

namespace au::au3 {
Meter::Meter()
{
    m_audioSignalChanges.onReceive(this,
                                   [this](const std::vector<Meter::Data>& data) {
        for (const Meter::Data& item : data) {
            const auto it = m_channels.find(item.key);
            if (it != m_channels.end()) {
                const au::audio::MeterSignal signal { item.peak, item.rms };
                it->second.send(item.channel, signal);
            }
        }
    });
}

void Meter::push(uint8_t channel, const IMeterSender::InterleavedSampleData& sampleData, int64_t key)
{
    const auto value = GetAbsValue(sampleData.buffer, sampleData.frames, sampleData.nChannels);
    push(channel, value, key);
}

void Meter::push(uint8_t channel, const IMeterSender::Sample& sample, int64_t key)
{
    m_trackData.push_back(Data { key, channel,
                                 au::audio::AudioSignalVal { sample.peak, static_cast<au::audio::volume_dbfs_t>(LINEAR_TO_DB(
                                                                                                                    sample.peak)) },
                                 au::audio::AudioSignalVal { sample.rms,
                                                             static_cast<au::audio::volume_dbfs_t>(LINEAR_TO_DB(sample.rms)) } });
}

void Meter::reset()
{
    for (auto& [key, _] : m_channels) {
        push(0, { 0.0, 0.0 }, key);
        push(1, { 0.0, 0.0 }, key);
    }
    sendAll();
}

void Meter::reserve(size_t size)
{
    m_trackData.reserve(size);
}

void Meter::sendAll()
{
    m_audioSignalChanges.send(std::move(m_trackData));
    m_trackData = {};
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
