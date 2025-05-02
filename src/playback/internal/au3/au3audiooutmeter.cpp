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
                                   [this](const std::map<int64_t, std::vector<OutMeter::Data> >& data) {
        for (const auto& [key, values] : data) {
            auto it = m_channels.find(key);
            if (it != m_channels.end()) {
                for (const auto& value : values) {
                    it->second.send(value.channel, value.signal);
                }
            } 
        }
    });
}

void OutMeter::push(uint8_t channel, float signal, int64_t key)
{
    auto& trackData = m_trackData[key];
    trackData.push_back(Data { channel, au::audio::AudioSignalVal { signal, static_cast<au::audio::volume_dbfs_t>(LINEAR_TO_DB(signal)) } });
}

void OutMeter::reset()
{
    for (auto& [key, _] : m_channels) {
        push(0, 0.0, key);
        push(1, 0.0, key);
    }
    sendAll();
}

void OutMeter::sendAll()
{
    m_audioSignalChanges.send(m_trackData);
    m_trackData.clear();
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