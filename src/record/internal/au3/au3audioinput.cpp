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
        m_focusedTrackChannels = getFocusedTrackChannels();

        initMeter();

        updateMonitoring(MonitoringChangeReason::Initialization);

        // register all callbacks for monitoring change reasons
        configuration()->isMicMeteringOnChanged().onNotify(this, [this]() {
            updateMonitoring(MonitoringChangeReason::MicMetering);
        });

        controller()->isRecordingChanged().onNotify(this, [this]() {
            updateMonitoring(MonitoringChangeReason::RecordingState);
        });

        playbackController()->isPlayingChanged().onNotify(this, [this]() {
            updateMonitoring(MonitoringChangeReason::PlaybackState);
        });

        audioDevicesProvider()->inputChannelsChanged().onNotify(this, [this]() {
            m_inputChannelsCount = audioDevicesProvider()->currentInputChannelsCount();
            updateMonitoring(MonitoringChangeReason::InputChannelsChanged);
        });

        selectionController()->focusedTrackChanged().onReceive(this, [this](const trackedit::TrackId&) {
            const int focusedTrackChannels = getFocusedTrackChannels();
            if (focusedTrackChannels != m_focusedTrackChannels) {
                m_focusedTrackChannels = focusedTrackChannels;
                updateMonitoring(MonitoringChangeReason::FocusedTrackChanged);
            }
        });

        meterController()->isRecordMeterVisibleChanged().onNotify(this, [this]() {
            updateMonitoring(MonitoringChangeReason::RecordMeterVisibilityChanged);
        });

        audibleInputMonitoringChanged = [&]() {
            updateMonitoring(MonitoringChangeReason::AudibleInputMonitoring);
        };
    });
}

void Au3AudioInput::updateMonitoring(const MonitoringChangeReason reason)
{
    switch (reason) {
    case MonitoringChangeReason::Initialization:
        updateMonitoring();
        break;
    case MonitoringChangeReason::RecordingState:
        updateMonitoring();
        break;
    case MonitoringChangeReason::PlaybackState:
        // when the playback stops we need to restart the monitoring if mic metering is on or input monitoring is on
        updateMonitoring();
        break;
    case MonitoringChangeReason::MicMetering:
        // when updating the mic metering we need to either stop or restart the monitoring
        updateMonitoring();
        break;
    case MonitoringChangeReason::FocusedTrackChanged:
        updateMonitoring();
        break;
    case MonitoringChangeReason::RecordMeterVisibilityChanged:
        // we don't really have to update the monitoring when the meter is shown/hidden
        //updateMonitoring();
        break;
    case MonitoringChangeReason::InputChannelsChanged:
        updateMonitoring();
        break;
    case MonitoringChangeReason::AudibleInputMonitoring:
        // when updating the audible input monitoring we need to either stop or restart the monitoring
        updateMonitoring();
        break;
    }
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
    return m_inputMeter->dataChanged(IMeterSender::TrackId { key });
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
    // Is there notification being sent when adjusting Prefs?
    // we need to find a way to notify that the monitoring state has changed
    // we could add a local lambda callback
    audibleInputMonitoringChanged();
}

void Au3AudioInput::startMonitoring() const
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

        // we should not have to force restart the monitoring by stopping the stream
        // gAudioIO->StopStream();
        // while (gAudioIO->IsBusy()) {
        //     using namespace std::chrono;
        //     std::this_thread::sleep_for(100ms);
        // }

        gAudioIO->StartMonitoring(ProjectAudioIO::GetDefaultOptions(*project));
    });
}

void Au3AudioInput::stopMonitoring() const
{
    muse::async::Async::call(this, []() {
        auto gAudioIO = AudioIO::Get();
        if (!gAudioIO) {
            return;
        }
        gAudioIO->StopMonitoring();
    });
}

void Au3AudioInput::updateMonitoring()
{
    if (canStartMonitoring()) {
        if (shouldStartMonitoring()) {
            startMonitoring();
        } else {
            stopMonitoring();
        }
    }
}

bool Au3AudioInput::canStartMonitoring() const
{
    // monitoring can't be started if we are recording or playing/pause
    // it can only be started when we are stopped
    return !controller()->isRecording() && !playbackController()->isPlaying() && !playbackController()->isPaused();
}

bool Au3AudioInput::shouldStartMonitoring() const
{
    if (audibleInputMonitoring()) {
        return true;
    }

    if (!configuration()->isMicMeteringOn()) { // PAUL: this look strange
        return false;
    }

    if (isTrackMeterMonitoring() || meterController()->isRecordMeterVisible()) {
        return true;
    }

    return false;
}

int Au3AudioInput::getFocusedTrackChannels() const
{
    if (globalContext() == nullptr) {
        return 0;
    }

    auto prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return 0;
    }

    const auto trackId = selectionController()->focusedTrack();

    std::optional<au::trackedit::Track> track = prj->track(trackId);
    if (!track) {
        return 0;
    }

    int channels = 0;
    if (track->type == au::trackedit::TrackType::Mono) {
        channels = 1;
    } else if (track->type == au::trackedit::TrackType::Stereo) {
        channels = 2;
    }

    return channels;
}

bool Au3AudioInput::isTrackMeterMonitoring() const
{
    return m_inputChannelsCount == m_focusedTrackChannels;
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
