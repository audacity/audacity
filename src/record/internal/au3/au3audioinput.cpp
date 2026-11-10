/*
* Audacity: A Digital Audio Editor
*/

#include "au3audioinput.h"

#include "global/async/async.h"

#include "au3-audio-io/ProjectAudioIO.h"
#include "au3-wave-track/WaveTrack.h"

#include "au3wrap/au3types.h"
#include "au3wrap/internal/au3audiometerfactory.h"

#include "au3recordingtracks.h"

#include "audio/audiotypes.h"
#include "types/ratio.h"

using namespace muse;
using namespace muse::async;
using namespace au::record;
using namespace au::playback;
using namespace au::au3;

Au3AudioInput::Au3AudioInput(const muse::modularity::ContextPtr& ctx)
    : muse::Contextable(ctx), m_inputMeter{au::au3::createAudioMeter()}
{
    globalContext()->currentProjectChanged().onNotify(this, [this](){
        auto currentProject = globalContext()->currentProject();
        if (!currentProject) {
            return;
        }

        m_inputChannelsCount = audioDriverController()->configuration().inputChannels;

        initMeter();

        m_monitoredTracks.clear();
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
            updateAudioEngineMonitoring();
        }, muse::async::Asyncable::Mode::SetReplace);

        playbackController()->isPlayingChanged().onNotify(this, [this]() {
            // when the playback stops we need to restart the monitoring if mic metering is on or input monitoring is on
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
    });
}

void Au3AudioInput::subscribeToTrackChanges()
{
    trackNavigationController()->focusedTrackChanged().onReceive(this, [this](const trackedit::TrackId&, bool) {
        updateMonitoredTracks();
    }, muse::async::Asyncable::Mode::SetReplace);

    selectionController()->selectedTracksChanged().onReceive(this, [this](const trackedit::TrackIdList&) {
        updateMonitoredTracks();
    }, muse::async::Asyncable::Mode::SetReplace);

    const auto prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    const auto onTrack = [this](const trackedit::Track&) {
        updateMonitoredTracks();
    };
    prj->trackAdded().onReceive(this, onTrack, muse::async::Asyncable::Mode::SetReplace);
    prj->trackChanged().onReceive(this, onTrack, muse::async::Asyncable::Mode::SetReplace);
    prj->trackRemoved().onReceive(this, onTrack, muse::async::Asyncable::Mode::SetReplace);
    prj->tracksChanged().onReceive(this, [this](const std::vector<trackedit::Track>&) {
        updateMonitoredTracks();
    }, muse::async::Asyncable::Mode::SetReplace);
}

namespace {
std::map<au::trackedit::TrackId, TrackInputChannels> routeInputChannels(const WritableSampleTrackArray& tracks, const int inputChannels)
{
    std::map<au::trackedit::TrackId, TrackInputChannels> result;
    std::vector<std::pair<au::trackedit::TrackId, size_t> > layout;
    for (const auto& track : tracks) {
        const au::trackedit::TrackId trackId(track->GetId());
        result[trackId].resize(track->NChannels());
        for (size_t channel = 0; channel < track->NChannels(); ++channel) {
            layout.emplace_back(trackId, channel);
        }
    }

    const auto source = [&result, &layout](size_t trackChannel) -> std::vector<au::audio::audioch_t>& {
        return result[layout[trackChannel].first][layout[trackChannel].second];
    };

    if (inputChannels == 2 && layout.size() == 1) {
        source(0) = { 0, 1 };
    } else if (inputChannels == 1 && layout.size() == 2) {
        source(0) = { 0 };
        source(1) = { 0 };
    } else {
        const size_t inputCount = inputChannels > 0 ? static_cast<size_t>(inputChannels) : 0;
        for (size_t channel = 0; channel < std::min(layout.size(), inputCount); ++channel) {
            source(channel) = { static_cast<au::audio::audioch_t>(channel) };
        }
    }

    return result;
}
}

void Au3AudioInput::updateMonitoredTracks()
{
    std::map<trackedit::TrackId, TrackInputChannels> tracks;
    if (Au3Project* project = projectRef()) {
        tracks = routeInputChannels(ChooseRecordingTracks(*project, trackNavigationController()->focusedTrack(), m_inputChannelsCount),
                                    m_inputChannelsCount);
    }

    if (tracks == m_monitoredTracks) {
        return;
    }

    m_monitoredTracks = std::move(tracks);
    m_monitoredTracksChanged.notify();
    updateAudioEngineMonitoring();
}

TrackInputChannels Au3AudioInput::monitoredInputChannels(const trackedit::TrackId trackId) const
{
    const auto it = m_monitoredTracks.find(trackId);
    return it != m_monitoredTracks.end() ? it->second : TrackInputChannels {};
}

muse::async::Notification Au3AudioInput::monitoredTracksChanged() const
{
    return m_monitoredTracksChanged;
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
    return !controller()->isRecording() && !playbackController()->isPlaying() && !playbackController()->isPaused() && hasDevice;
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
    //   -> only if a track would be recorded into (see monitoredInputChannels())
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
