/*
* Audacity: A Digital Audio Editor
*/

#include "au3audioinput.h"

#include "global/async/async.h"

#include "libraries/lib-audio-io/AudioIO.h"
#include "libraries/lib-audio-io/ProjectAudioIO.h"

#include "au3wrap/au3types.h"

#include "au3audio/audiotypes.h"

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

        m_inputChannelsCount = audioDevicesProvider()->currentInputChannelsCount();
        m_focusedTrackId = selectionController()->focusedTrack();

        initMeter();
        restartMonitoring();

        configuration()->isMicMeteringOnChanged().onNotify(this, [this]() {
            restartMonitoring();
        });

        controller()->isRecordingChanged().onNotify(this, [this]() {
            if (!controller()->isRecording()) {
                restartMonitoring();
            }
        });

        playbackController()->isPlayingChanged().onNotify(this, [this]() {
            if (playbackController()->isStopped()) {
                restartMonitoring();
            }
        });

        audioDevicesProvider()->inputChannelsChanged().onNotify(this, [this]() {
            m_inputChannelsCount = audioDevicesProvider()->currentInputChannelsCount();
            restartMonitoring();
        });

        selectionController()->focusedTrackChanged().onReceive(this, [this](const trackedit::TrackId& trackId) {
            m_focusedTrackId = trackId;
            restartMonitoring();
        });

        meterController()->isRecordMeterVisibleChanged().onNotify(this, [this]() {
            restartMonitoring();
        });
    });
}

void Au3AudioInput::initMeter()
{
    Au3Project* project = projectRef();
    if (!project) {
        return;
    }

    auto& projectAudioIO = ProjectAudioIO::Get(*project);
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

bool Au3AudioInput::audibleInputMonitoring() const
{
    bool swPlaythrough = false;
    gPrefs->Read(wxT("/AudioIO/SWPlaythrough"), &swPlaythrough, false);
    return swPlaythrough;
}

void Au3AudioInput::setAudibleInputMonitoring(bool enable)
{
    gPrefs->Write(wxT("/AudioIO/SWPlaythrough"), enable);
    gPrefs->Flush();

    restartMonitoring();
}

void Au3AudioInput::startMonitoring()
{
    muse::async::Async::call(this, [this]() {
        auto gAudioIO = AudioIO::Get();
        if (!gAudioIO) {
            return;
        }

        Au3Project* project = projectRef();
        if (!project) {
            return;
        }

        if (gAudioIO->IsMonitoring()) {
            return;
        }

        gAudioIO->StopStream();
        using namespace std::chrono;
        while (gAudioIO->IsBusy()) {
            std::this_thread::sleep_for(100ms);
        }

        gAudioIO->StartMonitoring(ProjectAudioIO::GetDefaultOptions(*project));
    });
}

void Au3AudioInput::stopMonitoring()
{
    muse::async::Async::call(this, []() {
        auto gAudioIO = AudioIO::Get();
        if (!gAudioIO) {
            return;
        }

        if (!gAudioIO->IsMonitoring()) {
            return;
        }

        gAudioIO->StopStream();
        using namespace std::chrono;
        while (gAudioIO->IsBusy()) {
            std::this_thread::sleep_for(100ms);
        }
    });
}

void Au3AudioInput::restartMonitoring()
{
    stopMonitoring();

    if (!configuration()->isMicMeteringOn() && !audibleInputMonitoring()) {
        return;
    }

    if (!audibleInputMonitoring() && configuration()->isMicMeteringOn() && !isTrackMeterMonitoring()
        && !meterController()->isRecordMeterVisible()) {
        return;
    }

    startMonitoring();
}

bool Au3AudioInput::isTrackMeterMonitoring() const
{
    auto prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return false;
    }

    std::optional<trackedit::Track> track = prj->track(m_focusedTrackId);
    if (!track) {
        return false;
    }

    if ((track->type == trackedit::TrackType::Mono && m_inputChannelsCount == 1)
        || (track->type == trackedit::TrackType::Stereo && m_inputChannelsCount == 2)) {
        return true;
    }

    return false;
}

Au3Project* Au3AudioInput::projectRef() const
{
    const auto context = globalContext();
    if (!context) {
        return nullptr;
    }

    const auto currentProject = context->currentProject();
    if (!currentProject) {
        return nullptr;
    }

    return reinterpret_cast<Au3Project*>(currentProject->au3ProjectPtr());
}
