/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "global/async/asyncable.h"
#include "global/async/promise.h"
#include "global/async/channel.h"

#include "libraries/lib-audio-devices/MeterChannel.h"
namespace au::playback {
class TrackMeter : public MeterChannel, public muse::async::Asyncable
{
public:
    TrackMeter();
    void push(int64_t key, au::audio::audioch_t channel, au::audio::AudioSignalVal signal) override;
    void sendAll() override;
    muse::async::Channel<au::audio::audioch_t, au::audio::AudioSignalVal> dataChanged(int64_t key) override;

private:
    struct Data
    {
        au::audio::audioch_t channel;
        au::audio::AudioSignalVal signal;
    };

    muse::async::Channel < std::map<int64_t, std::vector<Data> > > m_audioSignalChanges;
    std::map < int64_t, std::vector<Data> > m_trackData;
    std::map<int64_t, muse::async::Channel<au::audio::audioch_t, au::audio::AudioSignalVal> > m_channels;
};
}
