/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "../../iaudiooutput.h"

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "au3wrap/au3types.h"
#include "au3wrap/internal/au3audiometer.h"

#include "libraries/lib-utility/LockFreeQueue.h"

namespace au::playback {
class Au3AudioOutput : public IAudioOutput, public muse::async::Asyncable
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    Au3AudioOutput();

    muse::async::Promise<float> playbackVolume() const override;
    void setPlaybackVolume(float volume) override;
    muse::async::Channel<float> playbackVolumeChanged() const override;

    audio::sample_rate_t sampleRate() const override;
    muse::async::Channel<audio::sample_rate_t> sampleRateChanged() const override;

    muse::async::Channel<audio::audioch_t, audio::MeterSignal> playbackSignalChanges() const override;
    muse::async::Channel<au::audio::audioch_t, au::audio::MeterSignal> playbackTrackSignalChanges(int64_t key) const override;

private:
    au3::Au3Project* projectRef() const;

    void initMeter();

    void notifyAboutSampleRateChanged();

    muse::async::Channel<float> m_playbackVolumeChanged;
    muse::async::Channel<audio::sample_rate_t> m_sampleRateChanged;

    const std::shared_ptr<au::au3::Meter> m_outputMeter;
};
}
