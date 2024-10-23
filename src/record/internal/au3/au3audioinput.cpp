/*
* Audacity: A Digital Audio Editor
*/

#include "au3audioinput.h"

#include "global/async/async.h"

#include "libraries/lib-audio-io/AudioIO.h"
#include "libraries/lib-audio-io/ProjectAudioIO.h"

#include "playback/internal/au3/au3audioinoutmeter.h"
#include "au3wrap/internal/au3types.h"

#include "au3audio/audiotypes.h"

#include "log.h"

using namespace muse;
using namespace muse::async;
using namespace au::record;
using namespace au::playback;
using namespace au::au3;

Au3AudioInput::Au3AudioInput()
{
    m_inputMeter = std::make_shared<InOutMeter>();

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
    AudacityProject& project = projectRef();

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

muse::async::Promise<muse::async::Channel<au::audio::audioch_t, au::audio::AudioSignalVal> > Au3AudioInput::recordSignalChanges() const
{
    return m_inputMeter->signalChanges();
}

AudacityProject& Au3AudioInput::projectRef() const
{
    AudacityProject* project = reinterpret_cast<AudacityProject*>(globalContext()->currentProject()->au3ProjectPtr());
    return *project;
}
