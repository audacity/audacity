/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "../../iaudiooutput.h"
#include "au3audiotrackmeter.h"

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "au3wrap/au3types.h"

namespace au::playback {
class InOutMeter;
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

    muse::async::Channel<audio::audioch_t, audio::AudioSignalVal> playbackSignalChanges() const override;
    muse::async::Channel<au::audio::audioch_t, au::audio::AudioSignalVal> playbackTrackSignalChanges(int64_t key) const override;

private:
    au3::Au3Project* projectRef() const;

    void initMeter();

    void notifyAboutSampleRateChanged();

    mutable muse::async::Channel<float> m_playbackVolumeChanged;
    mutable muse::async::Channel<audio::sample_rate_t> m_sampleRateChanged;

    std::shared_ptr<InOutMeter> m_outputMeter;
    std::shared_ptr<TrackMeter> m_trackMeter;
};
}
