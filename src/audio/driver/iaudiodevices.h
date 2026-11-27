/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <cstdint>
#include <string>
#include <vector>

namespace au::audio {
//! The device enumeration of an audio backend, for AudioDriverController.
//! Devices are identified by their display name within an API.
class IAudioDevices
{
public:
    virtual ~IAudioDevices() = default;

    virtual std::vector<std::string> apis() const = 0;
    //! The API to select when the configured one is not available.
    virtual std::string preferredApi(const std::vector<std::string>& apis) const = 0;

    virtual std::vector<std::string> outputDevices(const std::string& api) const = 0;
    virtual std::vector<std::string> inputDevices(const std::string& api) const = 0;
    virtual std::string systemDefaultOutputDevice(const std::string& api) const = 0;
    virtual std::string systemDefaultInputDevice(const std::string& api) const = 0;
    //! 0 if the device is unknown
    virtual int inputChannels(const std::string& api, const std::string& inputDevice) const = 0;

    virtual std::vector<uint64_t> sampleRates() const = 0;

    virtual void rescan() = 0;
    //! Called when the effective input device may have changed.
    virtual void refreshInputDevice(const std::string& api, const std::string& inputDevice) = 0;

    virtual bool hasAsioControlPanel(const std::string& api, const std::string& outputDevice,
                                     const std::string& inputDevice) const = 0;
    virtual void showAsioControlPanel(const std::string& api, const std::string& outputDevice,
                                      const std::string& inputDevice) = 0;
};
}
