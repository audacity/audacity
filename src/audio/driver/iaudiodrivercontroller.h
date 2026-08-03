/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <vector>

#include "framework/global/async/channel.h"
#include "framework/global/async/notification.h"
#include "framework/global/modularity/imoduleinterface.h"
#include "framework/global/modularity/ioc.h"

#include "audio/audioconfigurationtypes.h"

namespace au::audio {
class IAudioDriverController : MODULE_GLOBAL_INTERFACE
{
    INTERFACE_ID(IAudioDriverController)

public:
    virtual ~IAudioDriverController() = default;

    virtual AudioConfiguration configuration() const = 0;
    //! Sent synchronously while the triggering apply()/rescan() is still in
    //! progress: a listener calling back into apply()/rescan() gets Busy.
    //! Defer such work instead of reacting inside the notification.
    virtual muse::async::Channel<AudioConfigurationDelta> configurationChanged() const = 0;

    virtual std::vector<std::string> apis() const = 0;
    virtual std::vector<std::string> outputDevices() const = 0;
    virtual std::vector<std::string> outputDevices(const std::string& api) const = 0;
    virtual std::string systemDefaultOutputDevice(const std::string& api) const = 0;
    virtual std::vector<std::string> inputDevices() const = 0;
    virtual std::vector<std::string> inputDevices(const std::string& api) const = 0;
    virtual std::string systemDefaultInputDevice(const std::string& api) const = 0;
    virtual int inputChannelsAvailable() const = 0;
    virtual int inputChannelsAvailable(const std::string& api, const AudioDeviceSelection& inputDevice) const = 0;
    virtual std::vector<uint64_t> sampleRates() const = 0;
    virtual std::vector<std::string> sampleFormats() const = 0;

    virtual ApplyResult apply(const muse::modularity::ContextPtr& requester, const AudioConfigurationChange& change) = 0;
    virtual ApplyResult rescan() = 0;
    virtual ApplyResult reload(const muse::modularity::ContextPtr& requester) = 0;
    virtual ApplyResult openAsioDriverSettings(const AudioRoutingChange& routing) = 0;

    virtual muse::async::Notification audioDeviceListChanged() const = 0;

    virtual muse::async::Channel<std::string> usedOutputDeviceChanged() const = 0;
    virtual muse::async::Channel<std::string> usedInputDeviceChanged() const = 0;
};
}
