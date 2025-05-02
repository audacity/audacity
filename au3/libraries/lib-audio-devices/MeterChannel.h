/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "global/async/channel.h"

#include "au3audio/audiotypes.h"

class AUDIO_DEVICES_API MeterChannel
{
public:
    virtual ~MeterChannel() = default;
    virtual void push(int64_t key, au::audio::audioch_t channel, au::audio::AudioSignalVal signal) = 0;
    virtual void sendAll() = 0;
    virtual muse::async::Channel<au::audio::audioch_t, au::audio::AudioSignalVal> dataChanged(int64_t key) = 0;
};
