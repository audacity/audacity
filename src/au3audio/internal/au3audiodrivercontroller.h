/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <optional>

#include "framework/global/async/asyncable.h"
#include "framework/global/iapplication.h"
#include "framework/global/modularity/ioc.h"

#include "audio/driver/iaudiodrivercontroller.h"
#include "audio/iaudioengine.h"
#include "audio/iaudiostreamsuspender.h"
#include "au3audio/isystemaudiodeviceslistener.h"

namespace au::au3audio {
class Au3AudioDriverController final : public audio::IAudioDriverController, public muse::async::Asyncable
{
    muse::GlobalInject<audio::IAudioEngine> audioEngine;
    muse::GlobalInject<muse::IApplication> application;
    muse::GlobalInject<ISystemAudioDevicesListener> systemAudioDevicesListener;

public:
    void init();

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
    friend class Au3AudioDriverControllerTests;

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
    audio::AudioConfiguration m_configuration;
    bool m_applying = false;
    bool m_pendingSystemDevicesChange = false;

    muse::async::Channel<audio::AudioConfigurationDelta> m_configurationChanged;
    muse::async::Notification m_audioDeviceListChanged;
    muse::async::Channel<std::string> m_usedOutputDeviceChanged;
    muse::async::Channel<std::string> m_usedInputDeviceChanged;
};
}
