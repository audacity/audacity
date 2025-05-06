/*
* Audacity: A Digital Audio Editor
*/

#include <algorithm>
#include <cmath>

#include "au3audiooutmeter.h"

#include "libraries/lib-utility/MemoryX.h"

namespace au::playback {
OutMeter::OutMeter()
{
    m_audioSignalChanges.onReceive(this,
                                   [this](const std::vector<OutMeter::Data>& data) {
        for (const OutMeter::Data& item : data) {
            const auto it = m_channels.find(item.key);
            if (it != m_channels.end()) {
                it->second.send(item.channel, item.signal);
            }
        }
    });
}

void OutMeter::push(uint8_t channel, float signal, int64_t key)
{
    m_trackData.push_back(Data { key, channel,
                                 au::audio::AudioSignalVal { signal, static_cast<au::audio::volume_dbfs_t>(LINEAR_TO_DB(signal)) } });
}

void OutMeter::reset()
{
    for (auto& [key, _] : m_channels) {
        push(0, 0.0, key);
        push(1, 0.0, key);
    }
    sendAll();
}

void OutMeter::reserve(size_t size)
{
    m_trackData.reserve(size);
}

void OutMeter::sendAll()
{
    m_audioSignalChanges.send(std::move(m_trackData));
    m_trackData = {};
}

muse::async::Channel<au::audio::audioch_t, au::audio::AudioSignalVal> OutMeter::dataChanged(int64_t key)
{
    auto channel = m_channels[key];
    channel.onClose(this, [this, key]() {
        m_channels.erase(key);
    });

    return channel;
}
}
