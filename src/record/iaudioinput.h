/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <memory>

#include "global/async/promise.h"
#include "global/async/channel.h"

#include "au3audio/audiotypes.h"

namespace au::record {
class IAudioInput
{
public:
    virtual ~IAudioInput() = default;

    virtual muse::async::Promise<float> recordVolume() const = 0;
    virtual void setRecordVolume(float volume) = 0;
    virtual muse::async::Channel<float> recordVolumeChanged() const = 0;

    virtual muse::async::Promise<muse::async::Channel<audio::audioch_t, audio::AudioSignalVal> > recordSignalChanges() const = 0;
};

using IAudioInputPtr = std::shared_ptr<IAudioInput>;
}
