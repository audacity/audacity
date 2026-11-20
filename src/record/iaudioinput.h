/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <memory>
#include <vector>

#include "global/async/channel.h"
#include "global/async/notification.h"

#include "audio/audiotypes.h"
#include "trackedit/trackedittypes.h"

namespace au::record {
using TrackInputChannels = std::vector<std::vector<audio::audioch_t> >;

class IAudioInput
{
public:
    virtual ~IAudioInput() = default;

    virtual float recordVolume() const = 0;
    virtual void setRecordVolume(float volume) = 0;
    virtual muse::async::Channel<float> recordVolumeChanged() const = 0;

    virtual muse::async::Channel<audio::audioch_t, audio::MeterSignal> recordSignalChanges() const = 0;
    virtual muse::async::Channel<audio::audioch_t, audio::MeterSignal> recordTrackSignalChanges(int64_t key) const = 0;
    virtual muse::async::Channel<audio::audioch_t, audio::MeterSignal> recordInputChannelSignalChanges() const = 0;

    virtual TrackInputChannels monitoredInputChannels(trackedit::TrackId trackId) const = 0;
    virtual muse::async::Notification monitoredTracksChanged() const = 0;
};

using IAudioInputPtr = std::shared_ptr<IAudioInput>;
}
