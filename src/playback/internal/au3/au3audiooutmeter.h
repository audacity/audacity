/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "libraries/lib-audio-devices/IMeterChannel.h"

#include "global/async/asyncable.h"
#include "global/async/promise.h"
#include "global/async/channel.h"

#include "au3audio/audiotypes.h"

namespace au::playback {
class OutMeter : public IMeterChannel, public muse::async::Asyncable
{
public:
    OutMeter();
    void push(uint8_t channel, float signal, int64_t key) override;
    void sendAll() override;
    void reset() override;
    muse::async::Channel<au::audio::audioch_t, au::audio::AudioSignalVal> dataChanged(int64_t key = MAIN_METER_KEY);

private:
    struct Data
    {
        int64_t key;
        au::audio::audioch_t channel;
        au::audio::AudioSignalVal signal;
    };

    muse::async::Channel <std::vector<Data> > m_audioSignalChanges;
    std::vector<Data> m_trackData;
    std::map<int64_t, muse::async::Channel<au::audio::audioch_t, au::audio::AudioSignalVal> > m_channels;
};
}
