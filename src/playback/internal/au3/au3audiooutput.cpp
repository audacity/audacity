/*
* Audacity: A Digital Audio Editor
*/

#include "au3audiooutput.h"

#include "framework/global/async/async.h"
#include "framework/global/types/ratio.h"

#include "au3-audio-io/ProjectAudioIO.h"
#include "au3-project-rate/ProjectRate.h"

#include "au3wrap/au3types.h"
#include "au3wrap/internal/au3audiometerfactory.h"

#include "audio/audiotypes.h"

using namespace muse;
using namespace muse::async;
using namespace au::playback;
using namespace au::au3;

Au3AudioOutput::Au3AudioOutput()
    : m_outputMeter{au::au3::createAudioMeter()}
{
    globalContext()->currentProjectChanged().onNotify(this, [this](){
        auto currentProject = globalContext()->currentProject();
        if (!currentProject) {
            return;
        }

        initMeter();
        notifyAboutSampleRateChanged();
    });
}

void Au3AudioOutput::initMeter()
{
    Au3Project* project = projectRef();
    if (!project) {
        return;
    }

    auto& projectAudioIO = ProjectAudioIO::Get(*project);
    projectAudioIO.SetPlaybackMeter(m_outputMeter);
}

void Au3AudioOutput::notifyAboutSampleRateChanged()
{
    m_sampleRateChanged.send(sampleRate());
}

muse::async::Promise<float> Au3AudioOutput::playbackVolume() const
{
    return muse::async::Promise<float>([this](auto resolve, auto /*reject*/) {
        const float outputVolume = audioEngine()->getPlaybackVolume();
        return resolve(muse::linear_to_db(outputVolume));
    });
}

void Au3AudioOutput::setPlaybackVolume(float volume)
{
    muse::async::Async::call(this, [this, volume]() {
        audioEngine()->setPlaybackVolume(muse::db_to_linear(volume));
        m_playbackVolumeChanged.send(volume);
    });
}

muse::async::Channel<float> Au3AudioOutput::playbackVolumeChanged() const
{
    return m_playbackVolumeChanged;
}

au::audio::sample_rate_t Au3AudioOutput::sampleRate() const
{
    Au3Project* project = projectRef();
    if (!project) {
        return 1.0;
    }

    return ProjectRate::Get(*project).GetRate();
}

muse::async::Channel<au::audio::sample_rate_t> Au3AudioOutput::sampleRateChanged() const
{
    return m_sampleRateChanged;
}

muse::async::Channel<au::audio::audioch_t, au::audio::MeterSignal> Au3AudioOutput::playbackSignalChanges() const
{
    return m_outputMeter->dataChanged();
}

muse::async::Channel<au::audio::audioch_t, au::audio::MeterSignal> Au3AudioOutput::playbackTrackSignalChanges(
    int64_t key) const
{
    return m_outputMeter->dataChanged(audio::IAudioMeter::TrackId { key });
}

Au3Project* Au3AudioOutput::projectRef() const
{
    if (!globalContext()->currentProject()) {
        return nullptr;
    }

    return reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
}
