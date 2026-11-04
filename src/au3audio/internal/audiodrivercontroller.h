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
#include "au3audio/isystemaudiodeviceslistener.h"

#include "iaudiodevices.h"

namespace au::au3audio {
//! The device configuration of an audio backend: the backend-specific parts are its IAudioDevices and its IAudioEngine.
class AudioDriverController final : public audio::IAudioDriverController, public muse::async::Asyncable
{
    //! The engine of this backend; set it, as the global one forwards to whichever backend is active.
    muse::GlobalInject<audio::IAudioEngine> audioEngine;
    muse::GlobalInject<muse::IApplication> application;
    muse::GlobalInject<ISystemAudioDevicesListener> systemAudioDevicesListener;

public:
    //! @param settingsGroup prefix of this backend's settings keys
    AudioDriverController(std::shared_ptr<IAudioDevices> devices, const std::string& settingsGroup);

    void setAudioEngine(const std::shared_ptr<audio::IAudioEngine>& engine) { audioEngine.set(engine); }

    //! Stops the engine's stream, if any, as a change of the configuration does; see audio::AudioBackend::suspendStream
    audio::AudioStreamRestorer suspendStream() const;

    void init();

    //! Settings shared by all backends: the defaults for new projects
    static void initSharedDefaults(uint64_t defaultSampleRate);

    audio::AudioConfiguration configuration() const override;
    muse::async::Channel<audio::AudioConfigurationDelta> configurationChanged() const override;

    std::vector<std::string> apis() const override;
    std::vector<std::string> outputDevices() const override;
    std::vector<std::string> outputDevices(const std::string& api) const override;
    std::string systemDefaultOutputDevice(const std::string& api) const override;
    std::vector<std::string> inputDevices() const override;
    std::vector<std::string> inputDevices(const std::string& api) const override;
    std::string systemDefaultInputDevice(const std::string& api) const override;
    int inputChannelsAvailable() const override;
    int inputChannelsAvailable(const std::string& api, const audio::AudioDeviceSelection& inputDevice) const override;
    std::vector<uint64_t> sampleRates() const override;
    std::vector<std::string> sampleFormats() const override;

    audio::ApplyResult apply(const muse::modularity::ContextPtr& requester, const audio::AudioConfigurationChange& change) override;
    audio::ApplyResult rescan() override;
    audio::ApplyResult reload(const muse::modularity::ContextPtr& requester) override;
    audio::ApplyResult openAsioDriverSettings(const audio::AudioRoutingChange& routing) override;

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
        const muse::Settings::Key latencyCompensation;
        const muse::Settings::Key asioUseDeviceSampleRate;
    };

    std::string effectiveOutputDevice(const std::string& api, const audio::AudioDeviceSelection& selection) const;
    std::string effectiveInputDevice(const std::string& api, const audio::AudioDeviceSelection& selection) const;

    void initDefaults();
    void onSystemDevicesChanged();
    audio::AudioConfiguration configurationFromSettings() const;
    void refreshInputDeviceSettings(const std::string& api, const audio::AudioDeviceSelection& inputDevice);

    std::optional<audio::AudioConfiguration> normalizedConfiguration(
        const audio::AudioConfiguration& current, const audio::AudioConfigurationChange& change) const;
    audio::AudioConfigurationDelta makeDelta(const audio::AudioConfiguration& before, const audio::AudioConfiguration& after) const;
    bool streamNeedsSuspension(const audio::AudioConfigurationDelta& delta, uint64_t defaultSampleRate,
                               const muse::modularity::ContextPtr& requester,
                               const std::optional<audio::AudioStreamDescriptor>& stream) const;
    AudacityProject* projectForContext(const muse::modularity::ContextPtr& context) const;
    muse::modularity::ContextPtr contextForProject(const AudacityProject* project) const;
    audio::AudioStreamRestorer suspend(const audio::AudioStreamDescriptor& stream) const;
    audio::AudioStreamRestorer suspendOrForceStop(const audio::AudioStreamDescriptor& stream) const;
    void writeConfiguration(const audio::AudioConfiguration& value, const audio::AudioConfigurationDelta& delta,
                            const muse::modularity::ContextPtr& requester);
    bool rollbackAndRestore(const audio::AudioConfiguration& before, const audio::AudioConfiguration& attempted,
                            const muse::modularity::ContextPtr& requester, std::optional<double> requesterProjectRate,
                            bool refreshDeviceState, bool writeStarted, const audio::AudioStreamRestorer& restoreStream) noexcept;
    void publish(const audio::AudioConfigurationDelta& delta, bool deviceListChanged = false) noexcept;
    const std::shared_ptr<IAudioDevices> m_devices;
    const SettingsKeys m_keys;

    audio::AudioConfiguration m_configuration;
    bool m_applying = false;
    bool m_pendingSystemDevicesChange = false;

    muse::async::Channel<audio::AudioConfigurationDelta> m_configurationChanged;
    muse::async::Notification m_audioDeviceListChanged;
    muse::async::Channel<std::string> m_usedOutputDeviceChanged;
    muse::async::Channel<std::string> m_usedInputDeviceChanged;
};
}
