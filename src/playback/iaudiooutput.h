#pragma once

#include "global/async/promise.h"
#include "global/async/channel.h"

#include "au3audio/audiotypes.h"

//! NOTE Implemented in Au3Wrap
namespace au::playback {
class IAudioOutput
{
public:
    virtual ~IAudioOutput() = default;

    virtual muse::async::Promise<float> playbackVolume() const = 0;
    virtual void setPlaybackVolume(float volume) = 0;
    virtual muse::async::Channel<float> playbackVolumeChanged() const = 0;

    virtual audio::sample_rate_t sampleRate() const = 0;
    virtual muse::async::Channel<audio::sample_rate_t> sampleRateChanged() const = 0;

    virtual muse::async::Promise<muse::async::Channel<audio::audioch_t, audio::AudioSignalVal> >
    playbackSignalChanges() const = 0;
};
}
