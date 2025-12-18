/*
* Audacity: A Digital Audio Editor
*/
#pragma once

//! NOTE Implemented in Au3Wrap
//! This is equivalent of IAudioDriver/IAudioConfiguration of Musescore's audio engine
#include "framework/global/async/notification.h"
#include "framework/global/modularity/imoduleinterface.h"

namespace au::audio {
class IAudioDevicesProvider : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IAudioDevicesProvider)
public:
    virtual ~IAudioDevicesProvider() = default;

    virtual std::vector<std::string> outputDevices() const = 0;
    virtual std::string currentOutputDevice() const = 0;
    virtual void setOutputDevice(const std::string& device) = 0;
    virtual muse::async::Notification outputDeviceChanged() const = 0;

    virtual std::vector<std::string> inputDevices() const = 0;
    virtual std::string currentInputDevice() const = 0;
    virtual void setInputDevice(const std::string& device) = 0;
    virtual muse::async::Notification inputDeviceChanged() const = 0;

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

    virtual void handleDeviceChange() = 0;

    virtual void rescan() = 0;
};
}
