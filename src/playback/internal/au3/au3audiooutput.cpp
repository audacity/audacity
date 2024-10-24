/*
* Audacity: A Digital Audio Editor
*/

#include "au3audiooutput.h"

#include "global/async/async.h"

#include "libraries/lib-audio-io/AudioIO.h"
#include "libraries/lib-audio-io/ProjectAudioIO.h"
#include "libraries/lib-project-rate/ProjectRate.h"

#include "au3audioinoutmeter.h"
#include "au3wrap/au3types.h"

#include "au3audio/audiotypes.h"

#include "log.h"

using namespace muse;
using namespace muse::async;
using namespace au::playback;
using namespace au::au3;

Au3AudioOutput::Au3AudioOutput()
{
    m_outputMeter = std::make_shared<InOutMeter>();

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
    return muse::async::Promise<float>([](auto resolve, auto /*reject*/) {
        float inputVolume;
        float outputVolume;
        int inputSource;

        auto gAudioIO = AudioIO::Get();
        gAudioIO->GetMixer(&inputSource, &inputVolume, &outputVolume);

        return resolve(au3VolumeToLocal(outputVolume));
    });
}

void Au3AudioOutput::setPlaybackVolume(float volume)
{
    muse::async::Async::call(this, [this, volume]() {
        float inputVolume;
        float outputVolume;
        int inputSource;

        auto gAudioIO = AudioIO::Get();
        gAudioIO->GetMixer(&inputSource, &inputVolume, &outputVolume);

        gAudioIO->SetMixer(inputSource, inputVolume, localVolumeToAu3(volume));

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

muse::async::Promise<muse::async::Channel<au::audio::audioch_t, au::audio::AudioSignalVal> > Au3AudioOutput::playbackSignalChanges() const
{
    return m_outputMeter->signalChanges();
}

Au3Project* Au3AudioOutput::projectRef() const
{
    if (!globalContext()->currentProject()) {
        return nullptr;
    }

    return reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
}
