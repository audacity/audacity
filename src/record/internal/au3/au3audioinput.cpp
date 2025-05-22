/*
* Audacity: A Digital Audio Editor
*/

#include "au3audioinput.h"

#include "global/async/async.h"

#include "libraries/lib-audio-io/AudioIO.h"
#include "libraries/lib-audio-io/ProjectAudioIO.h"

#include "au3wrap/au3types.h"

#include "au3audio/audiotypes.h"

#include "log.h"

using namespace muse;
using namespace muse::async;
using namespace au::record;
using namespace au::playback;
using namespace au::au3;

Au3AudioInput::Au3AudioInput()
{
    m_inputMeter = std::make_shared<au::au3::Meter>();

    globalContext()->currentProjectChanged().onNotify(this, [this](){
        auto currentProject = globalContext()->currentProject();
        if (!currentProject) {
            return;
        }

        initMeter();
    });
}

void Au3AudioInput::initMeter()
{
    Au3Project& project = projectRef();

    auto& projectAudioIO = ProjectAudioIO::Get(project);
    projectAudioIO.SetCaptureMeter(m_inputMeter);
}

muse::async::Promise<float> Au3AudioInput::recordVolume() const
{
    return muse::async::Promise<float>([](auto resolve, auto /*reject*/) {
        float inputVolume;
        float outputVolume;
        int inputSource;

        auto gAudioIO = AudioIO::Get();
        gAudioIO->GetMixer(&inputSource, &inputVolume, &outputVolume);

        return resolve(au3VolumeToLocal(inputVolume));
    });
}

void Au3AudioInput::setRecordVolume(float volume)
{
    muse::async::Async::call(this, [this, volume]() {
        float inputVolume;
        float outputVolume;
        int inputSource;

        auto gAudioIO = AudioIO::Get();
        gAudioIO->GetMixer(&inputSource, &inputVolume, &outputVolume);

        gAudioIO->SetMixer(inputSource, localVolumeToAu3(volume), outputVolume);

        m_recordVolumeChanged.send(volume);
    });
}

muse::async::Channel<float> Au3AudioInput::recordVolumeChanged() const
{
    return m_recordVolumeChanged;
}

muse::async::Channel<au::audio::audioch_t, au::audio::MeterSignal> Au3AudioInput::recordSignalChanges() const
{
    return m_inputMeter->dataChanged();
}

muse::async::Channel<au::audio::audioch_t, au::audio::MeterSignal> Au3AudioInput::recordTrackSignalChanges(
    int64_t key) const
{
    return m_inputMeter->dataChanged(key);
}

Au3Project& Au3AudioInput::projectRef() const
{
    Au3Project* project = reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    return *project;
}
