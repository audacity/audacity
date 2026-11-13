/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>
#include <optional>
#include <string>

#include "framework/global/async/asyncable.h"
#include "framework/global/iapplication.h"
#include "framework/global/modularity/ioc.h"
#include "framework/global/settings.h"

#include "audio/driver/iaudiodrivercontroller.h"
#include "audio/iaudioengine.h"
#include "audio/iaudiostreamsuspender.h"
#include "audio/isystemaudiodeviceslistener.h"

#include "iaudiodevices.h"

namespace au::audio {
//! The device configuration of an audio backend: the backend-specific parts are its IAudioDevices and its IAudioEngine.
class AudioDriverController final : public IAudioDriverController, public muse::async::Asyncable
{
    //! The engine of this backend; set it, as the global one forwards to whichever backend is active.
    muse::GlobalInject<IAudioEngine> audioEngine;
    muse::GlobalInject<muse::IApplication> application;
    muse::GlobalInject<ISystemAudioDevicesListener> systemAudioDevicesListener;

public:
    //! @param settingsGroup prefix of this backend's settings keys
    AudioDriverController(std::shared_ptr<IAudioDevices> devices, const std::string& settingsGroup);

    //! May change, where backends share the devices: then the active one's
    void setAudioEngine(const std::shared_ptr<IAudioEngine>& engine);

    //! Stops the engine's stream, if any, as a change of the configuration does; see AudioBackend::suspendStream
    AudioStreamRestorer suspendStream() const;

    void init();

    //! Settings shared by all backends: the defaults for new projects
    static void initSharedDefaults(uint64_t defaultSampleRate);

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
    friend class AudioDriverControllerTests;

    struct SettingsKeys {
        explicit SettingsKeys(const std::string& group);
        const muse::Settings::Key audioHost;
        const muse::Settings::Key playbackDevice;
        const muse::Settings::Key recordingDevice;
        const muse::Settings::Key inputChannels;
        const muse::Settings::Key automaticLatencyCompensation;
        const muse::Settings::Key latencyDuration;
        const muse::Settings::Key softwareBufferLength;
        const muse::Settings::Key latencyCompensation;
        const muse::Settings::Key asioUseDeviceSampleRate;
    };

    std::string effectiveOutputDevice(const std::string& api, const AudioDeviceSelection& selection) const;
    std::string effectiveInputDevice(const std::string& api, const AudioDeviceSelection& selection) const;

    void initDefaults();
    void onSystemDevicesChanged();
    AudioConfiguration configurationFromSettings() const;
    void refreshInputDeviceSettings(const std::string& api, const AudioDeviceSelection& inputDevice);

    std::optional<AudioConfiguration> normalizedConfiguration(
        const AudioConfiguration& current, const AudioConfigurationChange& change) const;
    AudioConfigurationDelta makeDelta(const AudioConfiguration& before, const AudioConfiguration& after) const;
    bool streamNeedsSuspension(const AudioConfigurationDelta& delta, uint64_t defaultSampleRate,
                               const muse::modularity::ContextPtr& requester,
                               const std::optional<AudioStreamDescriptor>& stream) const;
    AudacityProject* projectForContext(const muse::modularity::ContextPtr& context) const;
    muse::modularity::ContextPtr contextForProject(const AudacityProject* project) const;
    AudioStreamRestorer suspend(const AudioStreamDescriptor& stream) const;
    AudioStreamRestorer suspendOrForceStop(const AudioStreamDescriptor& stream) const;
    void writeConfiguration(const AudioConfiguration& value, const AudioConfigurationDelta& delta,
                            const muse::modularity::ContextPtr& requester);
    bool rollbackAndRestore(const AudioConfiguration& before, const AudioConfiguration& attempted,
                            const muse::modularity::ContextPtr& requester, std::optional<double> requesterProjectRate,
                            bool refreshDeviceState, bool writeStarted, const AudioStreamRestorer& restoreStream) noexcept;
    void publish(const AudioConfigurationDelta& delta, bool deviceListChanged = false) noexcept;
    void subscribeToAudioEngine();
    const std::shared_ptr<IAudioDevices> m_devices;
    const SettingsKeys m_keys;

    AudioConfiguration m_configuration;
    bool m_applying = false;
    bool m_pendingSystemDevicesChange = false;

    muse::async::Channel<AudioConfigurationDelta> m_configurationChanged;
    muse::async::Notification m_audioDeviceListChanged;
    muse::async::Channel<std::string> m_usedOutputDeviceChanged;
    muse::async::Channel<std::string> m_usedInputDeviceChanged;
    //! Subscriptions to the engine; replaced with it
    std::unique_ptr<muse::async::Asyncable> m_engineSubscriptions;
};
}
