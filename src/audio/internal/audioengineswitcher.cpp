/*
* Audacity: A Digital Audio Editor
*/
#include "audioengineswitcher.h"

#include <cstdlib>

#include "framework/global/log.h"

using namespace au::audio;

AudioEngineSwitcher::AudioEngineSwitcher() = default;

void AudioEngineSwitcher::registerBackend(const AudioBackend& backend)
{
    IF_ASSERT_FAILED(backend.engine && backend.driverController) {
        return;
    }

    m_backends.push_back(backend);

    // TODO: replace with a persistent user setting
    const char* const requested = std::getenv("AU_AUDIO_BACKEND");
    if (!m_engine || (requested && backend.id == requested)) {
        LOGI() << "audio backend: " << backend.id;
        activate(backend);
    }
}

void AudioEngineSwitcher::activate(const AudioBackend& backend)
{
    m_engine = backend.engine;
    m_driver = backend.driverController;
    if (backend.activated) {
        backend.activated();
    }

    // Drops the subscriptions to the previous backend
    m_relay = std::make_unique<muse::async::Asyncable>();
    auto* const relay = m_relay.get();

    // recordingClipChanged() is not relayed: it is a bus between the clients, owned here.
    m_engine->recordingUpdateRequested().onNotify(relay, [this] { m_recordingUpdateRequested.notify(); });
    m_engine->recordingNewBlocks().onNotify(relay, [this] { m_recordingNewBlocks.notify(); });
    m_engine->recordingCommitRequested().onNotify(relay, [this] { m_recordingCommitRequested.notify(); });
    m_engine->recordingFinished().onNotify(relay, [this] { m_recordingFinished.notify(); });
    m_engine->streamStopped().onNotify(relay, [this] { m_streamStopped.notify(); });

    m_driver->configurationChanged().onReceive(relay, [this](const AudioConfigurationDelta& delta) {
        m_configurationChanged.send(delta);
    });
    m_driver->audioDeviceListChanged().onNotify(relay, [this] { m_audioDeviceListChanged.notify(); });
    m_driver->usedOutputDeviceChanged().onReceive(relay, [this](const std::string& device) {
        m_usedOutputDeviceChanged.send(device);
    });
    m_driver->usedInputDeviceChanged().onReceive(relay, [this](const std::string& device) {
        m_usedInputDeviceChanged.send(device);
    });
}

IAudioEngine& AudioEngineSwitcher::engine() const
{
    assert(m_engine && "no audio backend registered");
    return *m_engine;
}

IAudioDriverController& AudioEngineSwitcher::driver() const
{
    assert(m_driver && "no audio backend registered");
    return *m_driver;
}

// IAudioEngine

bool AudioEngineSwitcher::isBusy() const
{
    return engine().isBusy();
}

bool AudioEngineSwitcher::isCapturing() const
{
    return engine().isCapturing();
}

bool AudioEngineSwitcher::isMonitoring() const
{
    return engine().isMonitoring();
}

std::optional<AudioStreamDescriptor> AudioEngineSwitcher::currentStream() const
{
    return engine().currentStream();
}

bool AudioEngineSwitcher::isStreamActive(AudacityProject& project) const
{
    return engine().isStreamActive(project);
}

double AudioEngineSwitcher::transportTime() const
{
    return engine().transportTime();
}

int AudioEngineSwitcher::startTransport(const TransportSequences& sequences, double startTime, double endTime, double mixerEndTime,
                                        AudacityProject& project, const StartTransportOptions& options)
{
    return engine().startTransport(sequences, startTime, endTime, mixerEndTime, project, options);
}

void AudioEngineSwitcher::stopTransport()
{
    engine().stopTransport();
}

void AudioEngineSwitcher::pauseTransport(bool pause)
{
    engine().pauseTransport(pause);
}

void AudioEngineSwitcher::seek(double time)
{
    engine().seek(time);
}

std::shared_ptr<RealtimeEffectState> AudioEngineSwitcher::addRealtimeEffectState(AudacityProject& project, ChannelGroup* group,
                                                                                 const std::string& effectId)
{
    return engine().addRealtimeEffectState(project, group, effectId);
}

void AudioEngineSwitcher::removeRealtimeEffectState(AudacityProject& project, ChannelGroup* group,
                                                    const std::shared_ptr<RealtimeEffectState>& state)
{
    engine().removeRealtimeEffectState(project, group, state);
}

std::shared_ptr<RealtimeEffectState> AudioEngineSwitcher::replaceRealtimeEffectState(AudacityProject& project, ChannelGroup* group,
                                                                                     size_t effectListIndex,
                                                                                     const std::string& newEffectId)
{
    return engine().replaceRealtimeEffectState(project, group, effectListIndex, newEffectId);
}

void AudioEngineSwitcher::setPlaybackMeter(AudacityProject& project, const IAudioMeterPtr& meter)
{
    engine().setPlaybackMeter(project, meter);
}

void AudioEngineSwitcher::setCaptureMeter(AudacityProject& project, const IAudioMeterPtr& meter)
{
    engine().setCaptureMeter(project, meter);
}

void AudioEngineSwitcher::stopMeters(AudacityProject& project)
{
    engine().stopMeters(project);
}

void AudioEngineSwitcher::startMonitoring(AudacityProject& project)
{
    engine().startMonitoring(project);
}

void AudioEngineSwitcher::stopMonitoring()
{
    engine().stopMonitoring();
}

void AudioEngineSwitcher::setMonitoredTrack(std::optional<int64_t> track)
{
    m_monitoredTrack = track;
    engine().setMonitoredTrack(track);
}

void AudioEngineSwitcher::setInputVolume(float newInputVolume)
{
    engine().setInputVolume(newInputVolume);
}

float AudioEngineSwitcher::getInputVolume() const
{
    return engine().getInputVolume();
}

void AudioEngineSwitcher::setPlaybackVolume(float newPlaybackVolume)
{
    engine().setPlaybackVolume(newPlaybackVolume);
}

float AudioEngineSwitcher::getPlaybackVolume() const
{
    return engine().getPlaybackVolume();
}

bool AudioEngineSwitcher::canStopTransport(AudacityProject& project) const
{
    return engine().canStopTransport(project);
}

void AudioEngineSwitcher::handleDeviceChange()
{
    engine().handleDeviceChange();
}

muse::String AudioEngineSwitcher::lastErrorString() const
{
    return engine().lastErrorString();
}

double AudioEngineSwitcher::getPlaybackSampleRate() const
{
    return engine().getPlaybackSampleRate();
}

void AudioEngineSwitcher::updateTimePosition(unsigned long newlyConsumedSamples)
{
    engine().updateTimePosition(newlyConsumedSamples);
}

std::optional<AudioCallbackInfo> AudioEngineSwitcher::consumeNextCallbackInfo()
{
    return engine().consumeNextCallbackInfo();
}

muse::async::Notification AudioEngineSwitcher::recordingUpdateRequested() const
{
    return m_recordingUpdateRequested;
}

muse::async::Notification AudioEngineSwitcher::recordingNewBlocks() const
{
    return m_recordingNewBlocks;
}

muse::async::Notification AudioEngineSwitcher::recordingCommitRequested() const
{
    return m_recordingCommitRequested;
}

muse::async::Notification AudioEngineSwitcher::recordingFinished() const
{
    return m_recordingFinished;
}

muse::async::Notification AudioEngineSwitcher::streamStopped() const
{
    return m_streamStopped;
}

muse::async::Channel<au::au3::Au3TrackId, au::au3::Au3ClipId> AudioEngineSwitcher::recordingClipChanged() const
{
    return m_recordingClipChanged;
}

// IAudioDriverController

AudioConfiguration AudioEngineSwitcher::configuration() const
{
    return driver().configuration();
}

muse::async::Channel<AudioConfigurationDelta> AudioEngineSwitcher::configurationChanged() const
{
    return m_configurationChanged;
}

std::vector<std::string> AudioEngineSwitcher::apis() const
{
    return driver().apis();
}

std::vector<std::string> AudioEngineSwitcher::outputDevices() const
{
    return driver().outputDevices();
}

std::vector<std::string> AudioEngineSwitcher::outputDevices(const std::string& api) const
{
    return driver().outputDevices(api);
}

std::string AudioEngineSwitcher::systemDefaultOutputDevice(const std::string& api) const
{
    return driver().systemDefaultOutputDevice(api);
}

std::vector<std::string> AudioEngineSwitcher::inputDevices() const
{
    return driver().inputDevices();
}

std::vector<std::string> AudioEngineSwitcher::inputDevices(const std::string& api) const
{
    return driver().inputDevices(api);
}

std::string AudioEngineSwitcher::systemDefaultInputDevice(const std::string& api) const
{
    return driver().systemDefaultInputDevice(api);
}

int AudioEngineSwitcher::inputChannelsAvailable() const
{
    return driver().inputChannelsAvailable();
}

int AudioEngineSwitcher::inputChannelsAvailable(const std::string& api, const AudioDeviceSelection& inputDevice) const
{
    return driver().inputChannelsAvailable(api, inputDevice);
}

std::vector<uint64_t> AudioEngineSwitcher::sampleRates() const
{
    return driver().sampleRates();
}

std::vector<std::string> AudioEngineSwitcher::sampleFormats() const
{
    return driver().sampleFormats();
}

ApplyResult AudioEngineSwitcher::apply(const muse::modularity::ContextPtr& requester, const AudioConfigurationChange& change)
{
    return driver().apply(requester, change);
}

ApplyResult AudioEngineSwitcher::rescan()
{
    return driver().rescan();
}

ApplyResult AudioEngineSwitcher::reload(const muse::modularity::ContextPtr& requester)
{
    return driver().reload(requester);
}

ApplyResult AudioEngineSwitcher::openAsioDriverSettings(const AudioRoutingChange& routing)
{
    return driver().openAsioDriverSettings(routing);
}

muse::async::Notification AudioEngineSwitcher::audioDeviceListChanged() const
{
    return m_audioDeviceListChanged;
}

muse::async::Channel<std::string> AudioEngineSwitcher::usedOutputDeviceChanged() const
{
    return m_usedOutputDeviceChanged;
}

muse::async::Channel<std::string> AudioEngineSwitcher::usedInputDeviceChanged() const
{
    return m_usedInputDeviceChanged;
}
