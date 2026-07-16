/*
* Audacity: A Digital Audio Editor
*/

#ifndef AU_AU3WRAP_AU3AUDIODEVICESPROVIDER_H
#define AU_AU3WRAP_AU3AUDIODEVICESPROVIDER_H

#include <functional>

#include "framework/global/modularity/ioc.h"

#include "context/iglobalcontext.h"
#include "audio/iaudiodevicesprovider.h"
#include "audio/iaudioengine.h"

namespace au::au3audio {
class Au3AudioDevicesProvider : public audio::IAudioDevicesProvider, public muse::Contextable
{
    friend class Au3AudioDevicesProviderTests;

    muse::GlobalInject<au::audio::IAudioEngine> audioEngine;

    muse::ContextInject<context::IGlobalContext> globalContext { this };

public:
    Au3AudioDevicesProvider(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void init();

    std::vector<std::string> outputDevices() const override;
    std::string currentOutputDevice() const override;
    void setOutputDevice(const std::string& device) override;
    muse::async::Notification outputDeviceChanged() const override;

    std::vector<std::string> inputDevices() const override;
    std::string currentInputDevice() const override;
    void setInputDevice(const std::string& device) override;
    muse::async::Notification inputDeviceChanged() const override;

    void handleDeviceChange() override;

    std::vector<std::string> apis() const override;
    std::string currentApi() const override;
    void setApi(const std::string& api) override;
    muse::async::Notification apiChanged() const override;

    int inputChannelsAvailable() const override;
    int inputChannelsSelected() const override;
    void setInputChannels(const int count) override;
    muse::async::Notification inputChannelsAvailableChanged() const override;
    muse::async::Notification inputChannelsChanged() const override;

    double bufferLength() const override;
    void setBufferLength(double newBufferLength) override;
    muse::async::Notification bufferLengthChanged() const override;

    bool automaticCompensationEnabled() const override;
    void setAutomaticCompensationEnabled(bool enabled) override;
    muse::async::Notification automaticCompensationEnabledChanged() const override;

    double latencyCompensation() const override;
    void setLatencyCompensation(double newLatencyCompensation) override;
    muse::async::Notification latencyCompensationChanged() const override;

    std::vector<uint64_t> sampleRates() const override;
    uint64_t defaultSampleRate() const override;
    void setDefaultSampleRate(uint64_t newRate) override;
    muse::async::Notification defaultSampleRateChanged() const override;

    std::vector<std::string> sampleFormats() const override;
    std::string defaultSampleFormat() const override;
    void setDefaultSampleFormat(const std::string& format) override;
    muse::async::Notification defaultSampleFormatChanged() const override;

    bool asioUseDeviceSampleRate() const override;
    void setAsioUseDeviceSampleRate(bool use) override;
    muse::async::Notification asioUseDeviceSampleRateChanged() const override;

    void showAsioControlPanel() override;

    void rescan() override;

private:
    void initHosts();
    void initInputChannels();

    // Stops input monitoring around `change` and, when it was on beforehand,
    // restarts it once the change is done. Scopes nest: a composite change (a
    // host switch cascades into output- and input-device changes) restores
    // monitoring only once, at the outermost scope, so no monitor stream is
    // opened on a half-switched configuration.
    void withMonitoringRestored(const std::function<void()>& change);

    void updateInputOutputDevices();
    void setupInputDevice(const std::string& newDevice);

    std::string defaultOutputDevice();
    std::string defaultInputDevice();

    std::vector<std::string> m_audioApis;
    std::vector<std::string> m_outputDevices;
    std::vector<std::string> m_inputDevices;
    int m_inputChannelsAvailable = 0;
    int m_monitoringRestoreDepth = 0;

    muse::async::Notification m_audioOutputDeviceChanged;
    muse::async::Notification m_audioInputDeviceChanged;
    muse::async::Notification m_audioApiChanged;
    muse::async::Notification m_inputChannelsChanged;
    muse::async::Notification m_inputChannelsListChanged;
    muse::async::Notification m_bufferLengthChanged;
    muse::async::Notification m_automaticCompensationEnabledChanged;
    muse::async::Notification m_latencyCompensationChanged;
    muse::async::Notification m_defaultSampleRateChanged;
    muse::async::Notification m_defaultSampleFormatChanged;
    muse::async::Notification m_asioUseDeviceSampleRateChanged;
};
}

#endif // AU_AU3WRAP_AU3AUDIODEVICESPROVIDER_H
