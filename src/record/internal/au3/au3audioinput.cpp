/*
* Audacity: A Digital Audio Editor
*/

#include "au3audioinput.h"

#include "global/async/async.h"
#include "au3-wave-track/WaveTrack.h"

#include "au3wrap/au3types.h"

#include "au3recordingtracks.h"

#include "audio/audiotypes.h"
#include "types/ratio.h"

using namespace muse;
using namespace muse::async;
using namespace au::record;
using namespace au::playback;
using namespace au::au3;

Au3AudioInput::Au3AudioInput(const muse::modularity::ContextPtr& ctx)
    : muse::Contextable(ctx), m_inputMeter{audio::IAudioMeter::create()}
{
    globalContext()->currentProjectChanged().onNotify(this, [this](){
        m_monitoredTracks.clear();
        audioEngine()->setMonitoredTracks(m_monitoredTracks);

        auto currentProject = globalContext()->currentProject();
        if (!currentProject) {
            return;
        }

        m_inputChannelsCount = audioDriverController()->configuration().inputChannels;

        initMeter();

        updateMonitoredTracks();
        updateAudioEngineMonitoring();
        subscribeToTrackChanges();

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
            updateMonitoredTracks();
            updateAudioEngineMonitoring();
        }, muse::async::Asyncable::Mode::SetReplace);

        playbackController()->isPlayingChanged().onNotify(this, [this]() {
            // when the playback stops we need to restart the monitoring if mic metering is on or input monitoring is on
            updateMonitoredTracks();
            updateAudioEngineMonitoring();
        }, muse::async::Asyncable::Mode::SetReplace);

        audioDriverController()->configurationChanged().onReceive(this, [this](const audio::AudioConfigurationDelta& delta) {
            if (delta.contains(audio::AudioConfigurationField::InputChannels)) {
                m_inputChannelsCount = audioDriverController()->configuration().inputChannels;
                updateMonitoredTracks();
                updateAudioEngineMonitoring();
            }
        }, muse::async::Asyncable::Mode::SetReplace);

        meterController()->isRecordMeterVisibleChanged().onNotify(this, [this]() {
            // we have to update the monitoring when the meter is shown/hidden
            // because current track channels might have different number of channels than the input device channels
            // causing the monitoring to be stopped so we need to restart it
            updateAudioEngineMonitoring();
        }, muse::async::Asyncable::Mode::SetReplace);

        meterController()->isSoundActivationMeterVisibleChanged().onNotify(this, [this]() {
            updateAudioEngineMonitoring();
        }, muse::async::Asyncable::Mode::SetReplace);
    });
}

void Au3AudioInput::subscribeToTrackChanges()
{
    trackNavigationController()->focusedTrackChanged().onReceive(this, [this](const trackedit::TrackId&, bool) {
        onRecordingTracksChanged();
    }, muse::async::Asyncable::Mode::SetReplace);

    selectionController()->selectedTracksChanged().onReceive(this, [this](const trackedit::TrackIdList&) {
        onRecordingTracksChanged();
    }, muse::async::Asyncable::Mode::SetReplace);

    const auto prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    const auto onTrack = [this](const trackedit::Track&) {
        onRecordingTracksChanged();
    };
    prj->trackAdded().onReceive(this, onTrack, muse::async::Asyncable::Mode::SetReplace);
    prj->trackChanged().onReceive(this, onTrack, muse::async::Asyncable::Mode::SetReplace);
    prj->trackRemoved().onReceive(this, onTrack, muse::async::Asyncable::Mode::SetReplace);
    const auto onTrackPlaced = [this](const trackedit::Track&, int) {
        onRecordingTracksChanged();
    };
    prj->trackInserted().onReceive(this, onTrackPlaced, muse::async::Asyncable::Mode::SetReplace);
    prj->trackMoved().onReceive(this, onTrackPlaced, muse::async::Asyncable::Mode::SetReplace);
    prj->tracksChanged().onReceive(this, [this](const std::vector<trackedit::Track>&) {
        onRecordingTracksChanged();
    }, muse::async::Asyncable::Mode::SetReplace);
}

void Au3AudioInput::onRecordingTracksChanged()
{
    if (updateMonitoredTracks()) {
        updateAudioEngineMonitoring();
    }
}

bool Au3AudioInput::updateMonitoredTracks()
{
    std::vector<audio::MonitoredTrack> tracks;
    Au3Project* project = isTransportActive() ? nullptr : projectRef();
    if (project) {
        for (const auto& track : ChooseRecordingTracks(*project, trackNavigationController()->focusedTrack(), m_inputChannelsCount)) {
            tracks.push_back({ track->GetId(), track->NChannels() });
        }
    }

    if (tracks == m_monitoredTracks) {
        return false;
    }

    m_monitoredTracks = std::move(tracks);
    audioEngine()->setMonitoredTracks(m_monitoredTracks);
    return true;
}

void Au3AudioInput::initMeter()
{
    Au3Project* project = projectRef();
    if (!project) {
        return;
    }

    audioEngine()->setCaptureMeter(*project, m_inputMeter);
}

float Au3AudioInput::recordVolume() const
{
    return muse::linear_to_db(audioEngine()->getInputVolume());
}

void Au3AudioInput::setRecordVolume(float volume)
{
    muse::async::Async::call(this, [this, volume]() {
        audioEngine()->setInputVolume(muse::db_to_linear(volume));
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
    return m_inputMeter->dataChanged(audio::IAudioMeter::TrackId { key });
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
    const bool hasDevice = !audioDriverController()->inputDevices().empty() && audioDriverController()->inputChannelsAvailable() > 0;

    // Monitoring can't be started if we are recording or playing/pause
    // it can only be started when we are stopped
    return !isTransportActive() && hasDevice;
}

bool Au3AudioInput::isTransportActive() const
{
    return controller()->isRecording() || playbackController()->isPlaying() || playbackController()->isPaused();
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
    // - ALWAYS: if we are monitoring input (listening to the input) or showing the sound activation level meter
    // - CONDITIONALLY if we are metering the mic (showing the meter)
    //   -> only if a track would be recorded into (see updateMonitoredTracks())
    //   OR
    //   -> only if the record meter is visible
    if (configuration()->isInputMonitoringOn() || meterController()->isSoundActivationMeterVisible()) {
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

bool Au3AudioInput::isTrackMeterMonitoring() const
{
    return !m_monitoredTracks.empty();
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
