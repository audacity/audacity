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

    virtual void handleDeviceChange() = 0;
};
}
