/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>
#include <string>
#include <vector>

#include "framework/global/async/asyncable.h"

#include "audio/driver/iaudiodrivercontroller.h"
#include "audio/iaudiobackendregister.h"
#include "audio/iaudiobackendselector.h"
#include "audio/iaudioengine.h"

namespace au::audio {
//! The exported IAudioEngine and IAudioDriverController: forwards to the active backend.
//! Owns the notifications and channels, relaying the active backend's, so that
//! subscriptions survive a change of backend.
class AudioEngineSwitcher final : public IAudioEngine, public IAudioDriverController, public IAudioBackendRegister,
    public IAudioBackendSelector
{
public:
    AudioEngineSwitcher();

    // IAudioBackendRegister
    void registerBackend(const AudioBackend& backend) override;

    //! Once the backends are registered and the settings loaded: activates the backend chosen last
    void init();

    // IAudioBackendSelector
    std::vector<std::string> backends() const override;
    std::string activeBackend() const override;
    std::string backendTitle(const std::string& id) const override;
    ApplyResult setActiveBackend(const std::string& id) override;
    muse::async::Notification activeBackendChanged() const override;

    // IAudioEngine
    bool isBusy() const override;
    bool isCapturing() const override;
    bool isMonitoring() const override;
    std::optional<AudioStreamDescriptor> currentStream() const override;
    bool isStreamActive(AudacityProject& project) const override;
    double transportTime() const override;

    int startTransport(const TransportSequences& sequences, double startTime, double endTime, double mixerEndTime,
                       AudacityProject& project, const StartTransportOptions& options) override;
    void stopTransport() override;
    void pauseTransport(bool pause) override;
    void seek(double time) override;

    std::shared_ptr<RealtimeEffectState> addRealtimeEffectState(AudacityProject& project, ChannelGroup* group,
                                                                const std::string& effectId) override;
    void removeRealtimeEffectState(AudacityProject& project, ChannelGroup* group,
                                   const std::shared_ptr<RealtimeEffectState>& state) override;
    std::shared_ptr<RealtimeEffectState> replaceRealtimeEffectState(AudacityProject& project, ChannelGroup* group,
                                                                    size_t effectListIndex, const std::string& newEffectId) override;

    void setPlaybackMeter(AudacityProject& project, const IAudioMeterPtr& meter) override;
    void setCaptureMeter(AudacityProject& project, const IAudioMeterPtr& meter) override;
    void stopMeters(AudacityProject& project) override;

    void startMonitoring(AudacityProject& project) override;
    void stopMonitoring() override;

    void setInputVolume(float newInputVolume) override;
    float getInputVolume() const override;
    void setPlaybackVolume(float newPlaybackVolume) override;
    float getPlaybackVolume() const override;

    bool canStopTransport(AudacityProject& project) const override;

    void handleDeviceChange() override;

    muse::String lastErrorString() const override;
    double getPlaybackSampleRate() const override;
    void updateTimePosition(unsigned long newlyConsumedSamples) override;
    std::optional<AudioCallbackInfo> consumeNextCallbackInfo() override;

    muse::async::Notification recordingUpdateRequested() const override;
    muse::async::Notification recordingNewBlocks() const override;
    muse::async::Notification recordingCommitRequested() const override;
    muse::async::Notification recordingFinished() const override;
    muse::async::Notification streamStopped() const override;
    muse::async::Channel<au3::Au3TrackId, au3::Au3ClipId> recordingClipChanged() const override;

    // IAudioDriverController
    AudioConfiguration configuration() const override;
    muse::async::Channel<AudioConfigurationDelta> configurationChanged() const override;

    std::vector<std::string> apis() const override;
    std::vector<std::string> outputDevices() const override;
    std::vector<std::string> outputDevices(const std::string& api) const override;
    std::string systemDefaultOutputDevice(const std::string& api) const override;
    std::vector<std::string> inputDevices() const override;
    std::vector<std::string> inputDevices(const std::string& api) const override;
    std::string systemDefaultInputDevice(const std::string& api) const override;
    int inputChannelsAvailable() const override;
    int inputChannelsAvailable(const std::string& api, const AudioDeviceSelection& inputDevice) const override;
    std::vector<uint64_t> sampleRates() const override;
    std::vector<std::string> sampleFormats() const override;

    ApplyResult apply(const muse::modularity::ContextPtr& requester, const AudioConfigurationChange& change) override;
    ApplyResult rescan() override;
    ApplyResult reload(const muse::modularity::ContextPtr& requester) override;
    ApplyResult openAsioDriverSettings(const AudioRoutingChange& routing) override;

    muse::async::Notification audioDeviceListChanged() const override;
    muse::async::Channel<std::string> usedOutputDeviceChanged() const override;
    muse::async::Channel<std::string> usedInputDeviceChanged() const override;

private:
    void activate(const AudioBackend& backend);

    IAudioEngine& engine() const;
    IAudioDriverController& driver() const;

    std::vector<AudioBackend> m_backends;
    std::string m_activeId;
    std::shared_ptr<IAudioEngine> m_engine;
    std::shared_ptr<IAudioDriverController> m_driver;

    //! Subscriptions to the active backend; replaced on activation.
    std::unique_ptr<muse::async::Asyncable> m_relay;

    muse::async::Notification m_recordingUpdateRequested;
    muse::async::Notification m_recordingNewBlocks;
    muse::async::Notification m_recordingCommitRequested;
    muse::async::Notification m_recordingFinished;
    muse::async::Notification m_streamStopped;
    muse::async::Channel<au3::Au3TrackId, au3::Au3ClipId> m_recordingClipChanged;

    muse::async::Channel<AudioConfigurationDelta> m_configurationChanged;
    muse::async::Notification m_audioDeviceListChanged;
    muse::async::Channel<std::string> m_usedOutputDeviceChanged;
    muse::async::Channel<std::string> m_usedInputDeviceChanged;
    muse::async::Notification m_activeBackendChanged;
};
}
