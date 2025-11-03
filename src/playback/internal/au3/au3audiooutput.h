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

    auaudio::sample_rate_t sampleRate() const override;
    muse::async::Channel<auaudio::sample_rate_t> sampleRateChanged() const override;

    muse::async::Channel<auaudio::audioch_t, auaudio::MeterSignal> playbackSignalChanges() const override;
    muse::async::Channel<auaudio::audioch_t, au::auaudio::MeterSignal> playbackTrackSignalChanges(int64_t key) const override;

private:
    au3::Au3Project* projectRef() const;

    void initMeter();

    void notifyAboutSampleRateChanged();

    muse::async::Channel<float> m_playbackVolumeChanged;
    muse::async::Channel<auaudio::sample_rate_t> m_sampleRateChanged;

    const std::shared_ptr<au::au3::AudioMeterWrapper> m_outputMeter;
};
}
