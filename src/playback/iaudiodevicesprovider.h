/*
* Audacity: A Digital Audio Editor
*/
#pragma once

//! NOTE Implemented in Au3Wrap
//! This is equivalent of IAudioDriver/IAudioConfiguration of Musescore's audio engine
#include "async/notification.h"
#include "modularity/imoduleinterface.h"

namespace au::playback {
class IAudioDevicesProvider : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IAudioDevicesProvider)
public:
    virtual ~IAudioDevicesProvider() = default;

    virtual std::vector<std::string> audioOutputDevices() const = 0;
    virtual std::string currentAudioOutputDevice() const = 0;
    virtual void setAudioOutputDevice(const std::string& dev) = 0;
    virtual muse::async::Notification audioOutputDeviceChanged() const = 0;

    virtual std::vector<std::string> audioInputDevices() const = 0;
    virtual std::string currentAudioInputDevice() const = 0;
    virtual void setAudioInputDevice(const std::string& dev) = 0;
    virtual muse::async::Notification audioInputDeviceChanged() const = 0;

    virtual std::vector<std::string> audioApiList() const = 0;
    virtual std::string currentAudioApi() const = 0;
    virtual void setAudioApi(const std::string& audioApi) = 0;
    virtual muse::async::Notification audioApiChanged() const = 0;

    virtual std::vector<std::string> inputChannelsList() const = 0;
    virtual std::string currentInputChannels() const = 0;
    virtual void setInputChannels(const std::string& channels) = 0;
    virtual muse::async::Notification inputChannelsChanged() const = 0;
    virtual muse::async::Notification inputChannelsListChanged() const = 0;

    virtual double bufferLength() const = 0;
    virtual void setBufferLength(double newBufferLength) = 0;
    virtual muse::async::Notification bufferLengthChanged() const = 0;

    virtual double latencyCompensation() const = 0;
    virtual void setLatencyCompensation(double newLatencyCompensation) = 0;
    virtual muse::async::Notification latencyCompensationChanged() const = 0;

    virtual std::vector<uint64_t> availableSampleRateList() const = 0;
    virtual uint64_t defaultSampleRate() const = 0;
    virtual void setDefaultSampleRate(uint64_t newRate) = 0;
    virtual muse::async::Notification defaultSampleRateChanged() const = 0;

    virtual std::vector<std::string> defaultSampleFormatList() const = 0;
    virtual std::string defaultSampleFormat() const = 0;
    virtual void setDefaultSampleFormat(const std::string& format) = 0;
    virtual muse::async::Notification defaultSampleFormatChanged() const = 0;

    virtual void handleDeviceChange() = 0;
};
}
