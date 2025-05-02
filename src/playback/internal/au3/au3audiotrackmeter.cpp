/*
* Audacity: A Digital Audio Editor
*/

#include <algorithm>
#include <cmath>

#include "au3audiotrackmeter.h"

#include "libraries/lib-utility/MemoryX.h"

au::playback::TrackMeter::TrackMeter()
{
    m_audioSignalChanges.onReceive(this,
                                   [this](const std::map<int64_t, std::vector<TrackMeter::Data> >& data) {
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

void au::playback::TrackMeter::push(int64_t key, au::audio::audioch_t channel, au::audio::AudioSignalVal signal)
{
    auto& trackData = m_trackData[key];
    trackData.push_back(Data { channel, signal });
}

void au::playback::TrackMeter::sendAll()
{
    m_audioSignalChanges.send(m_trackData);
    m_trackData.clear();
}

muse::async::Channel<au::audio::audioch_t, au::audio::AudioSignalVal> au::playback::TrackMeter::dataChanged(int64_t key)
{
    auto channel = m_channels[key];

    channel.onClose(this, [this, key]() {
        m_channels.erase(key);
    });

    return channel;
}
