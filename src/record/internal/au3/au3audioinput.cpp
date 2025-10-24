/*
* Audacity: A Digital Audio Editor
*/

#include "au3audioinput.h"

#include "global/async/async.h"

#include "libraries/lib-audio-io/AudioIO.h"
#include "libraries/lib-audio-io/ProjectAudioIO.h"

#include "au3wrap/au3types.h"
#include "au3wrap/internal/au3audiometerfactory.h"

#include "au3audio/audiotypes.h"

using namespace muse;
using namespace muse::async;
using namespace au::record;
using namespace au::playback;
using namespace au::au3;

Au3AudioInput::Au3AudioInput()
    : m_inputMeter{au::au3::createAudioMeter()}
{
    globalContext()->currentProjectChanged().onNotify(this, [this](){
        auto currentProject = globalContext()->currentProject();
        if (!currentProject) {
            return;
        }

        m_inputChannelsCount = audioDevicesProvider()->currentInputChannelsCount();
        m_focusedTrackChannels = getFocusedTrackChannels();

        initMeter();

        updateAudioEngineMonitoring();

        // register all callbacks for monitoring change reasons
        configuration()->isMicMeteringOnChanged().onNotify(this, [this]() {
            // when updating the mic metering we need to either stop or restart the monitoring
            updateAudioEngineMonitoring();
        }, muse::async::Asyncable::Mode::SetReplace);

        configuration()->isInputMonitoringOnChanged().onNotify(this, [this]() {
            // when updating the audible input monitoring we need to either stop or restart the monitoring
            updateAudioEngineMonitoring();
        }, muse::async::Asyncable::Mode::SetReplace);

        controller()->isRecordingChanged().onNotify(this, [this]() {
            updateAudioEngineMonitoring();
        }, muse::async::Asyncable::Mode::SetReplace);

        playbackController()->isPlayingChanged().onNotify(this, [this]() {
            // when the playback stops we need to restart the monitoring if mic metering is on or input monitoring is on
            updateAudioEngineMonitoring();
        }, muse::async::Asyncable::Mode::SetReplace);

        audioDevicesProvider()->inputChannelsChanged().onNotify(this, [this]() {
            m_inputChannelsCount = audioDevicesProvider()->currentInputChannelsCount();
            updateAudioEngineMonitoring();
        }, muse::async::Asyncable::Mode::SetReplace);

        selectionController()->focusedTrackChanged().onReceive(this, [this](const trackedit::TrackId&) {
            const int focusedTrackChannels = getFocusedTrackChannels();
            if (focusedTrackChannels != m_focusedTrackChannels) {
                m_focusedTrackChannels = focusedTrackChannels;
                updateAudioEngineMonitoring();
            }
        }, muse::async::Asyncable::Mode::SetReplace);

        meterController()->isRecordMeterVisibleChanged().onNotify(this, [this]() {
            // we have to update the monitoring when the meter is shown/hidden
            // because current track channels might have different number of channels than the input device channels
            // causing the monitoring to be stopped so we need to restart it
            updateAudioEngineMonitoring();
        }, muse::async::Asyncable::Mode::SetReplace);
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
    return m_inputMeter->dataChanged(IMeterSender::TrackId { key });
}

void Au3AudioInput::startAudioEngineMonitoring() const
{
    muse::async::Async::call(this, [this]() {
        Au3Project* project = projectRef();
        if (!project) {
            return;
        }
        audioEngine()->startMonitoring(*project);
    });
}

void Au3AudioInput::stopAudioEngineMonitoring() const
{
    muse::async::Async::call(this, [this]() {
        audioEngine()->stopMonitoring();
    });
}

void Au3AudioInput::updateAudioEngineMonitoring() const
{
    if (audioEngineShouldBeMonitoring()) {
        startAudioEngineMonitoring();
    } else {
        stopAudioEngineMonitoring();
    }
}

bool Au3AudioInput::canStartAudioEngineMonitoring() const
{
    // Monitoring can't be started if we are recording or playing/pause
    // it can only be started when we are stopped
    return !controller()->isRecording() && !playbackController()->isPlaying() && !playbackController()->isPaused();
}

bool Au3AudioInput::audioEngineShouldBeMonitoring() const
{
    // When to enable Monitoring on the Audio Engine?
    // Monitoring can't be started if we are recording or playing/pause
    // it can only be started when we are stopped
    if (!canStartAudioEngineMonitoring()) {
        return false;
    }
    // otherwise Monitoring can be started if:
    // - ALWAYS: if we are monitoring input (listening to the input)
    // - CONDITIONALLY if we are metering the mic (showing the meter)
    //   -> only if the track in focus has the same number of channels as the input device
    //   OR
    //   -> only if the record meter is visible
    if (configuration()->isInputMonitoringOn()) {
        return true;
    }

    if (configuration()->isMicMeteringOn()) {
        // saving some resources here
        if (isTrackMeterMonitoring() || meterController()->isRecordMeterVisible()) {
            return true;
        }
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
