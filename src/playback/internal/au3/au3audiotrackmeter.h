/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "global/async/asyncable.h"
#include "global/async/promise.h"
#include "global/async/channel.h"

#include "au3audio/audiotypes.h"

#include "libraries/lib-audio-devices/TrackMeter.h"

namespace au::playback {
class Au3TrackMeter : public TrackMeter, public muse::async::Asyncable
{
public:
    void Update(int64_t trackId, size_t channel, const float* sampleData, unsigned long numFrames) override;

    muse::async::Promise<muse::async::Channel<int64_t, au::audio::audioch_t, au::audio::AudioSignalVal> > signalChanges() const;

private:
    muse::async::Channel<int64_t, au::audio::audioch_t, au::audio::AudioSignalVal> m_audioSignalChanges;
};
}
