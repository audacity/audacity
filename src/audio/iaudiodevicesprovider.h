/*
* Audacity: A Digital Audio Editor
*/
#pragma once

//! NOTE Implemented in Au3Wrap
//! This is equivalent of IAudioDriver/IAudioConfiguration of Musescore's audio engine
#include <optional>

#include "framework/global/async/channel.h"
#include "framework/global/async/notification.h"
#include "framework/global/modularity/imoduleinterface.h"

namespace au::audio {
class IAudioDevicesProvider : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IAudioDevicesProvider)
public:
    virtual ~IAudioDevicesProvider() = default;

    virtual std::vector<std::string> outputDevices() const = 0;
    virtual std::optional<std::string> currentOutputDevice() const = 0;
    virtual void setOutputDevice(const std::optional<std::string>& device) = 0;
    virtual muse::async::Notification outputDeviceChanged() const = 0;
    //! NOTE The device the system default currently resolves to, empty when unknown
    virtual std::string systemDefaultOutputDevice() const = 0;

    virtual std::vector<std::string> inputDevices() const = 0;
    virtual std::optional<std::string> currentInputDevice() const = 0;
    virtual void setInputDevice(const std::optional<std::string>& device) = 0;
    virtual muse::async::Notification inputDeviceChanged() const = 0;
    virtual std::string systemDefaultInputDevice() const = 0;
    virtual bool hasRecordingDevices() const = 0;

    //! NOTE Fired when the device in use changed because of a system-side
    //! change (default device switched, device unplugged); sends the name
    //! of the newly used device
    virtual muse::async::Channel<std::string> usedOutputDeviceChanged() const = 0;
    virtual muse::async::Channel<std::string> usedInputDeviceChanged() const = 0;

    virtual std::vector<std::string> apis() const = 0;
    virtual std::string currentApi() const = 0;
    virtual void setApi(const std::string& api) = 0;
    virtual muse::async::Notification apiChanged() const = 0;

    virtual int inputChannelsAvailable() const = 0;
    virtual int inputChannelsSelected() const = 0;
    virtual void setInputChannels(const int count) = 0;
    virtual muse::async::Notification inputChannelsAvailableChanged() const = 0;
    virtual muse::async::Notification inputChannelsChanged() const = 0;

    virtual double bufferLength() const = 0;
    virtual void setBufferLength(double newBufferLength) = 0;
    virtual muse::async::Notification bufferLengthChanged() const = 0;

    virtual bool automaticCompensationEnabled() const = 0;
    virtual void setAutomaticCompensationEnabled(bool enabled) = 0;
    virtual muse::async::Notification automaticCompensationEnabledChanged() const = 0;

    virtual double latencyCompensation() const = 0;
    virtual void setLatencyCompensation(double newLatencyCompensation) = 0;
    virtual muse::async::Notification latencyCompensationChanged() const = 0;

    virtual std::vector<uint64_t> sampleRates() const = 0;
    virtual uint64_t defaultSampleRate() const = 0;
    virtual void setDefaultSampleRate(uint64_t newRate) = 0;
    virtual muse::async::Notification defaultSampleRateChanged() const = 0;

    virtual std::vector<std::string> sampleFormats() const = 0;
    virtual std::string defaultSampleFormat() const = 0;
    virtual void setDefaultSampleFormat(const std::string& format) = 0;
    virtual muse::async::Notification defaultSampleFormatChanged() const = 0;

    virtual bool asioUseDeviceSampleRate() const = 0;
    virtual void setAsioUseDeviceSampleRate(bool use) = 0;
    virtual muse::async::Notification asioUseDeviceSampleRateChanged() const = 0;

    virtual void showAsioControlPanel() = 0;

    virtual void handleDeviceChange() = 0;

    virtual void rescan() = 0;
};
}
