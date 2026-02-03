/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>
#include <vector>

#include "au3-audio-devices/DeviceManager.h"

namespace au::au3audio {
class IAu3DeviceManager
{
public:
    virtual ~IAu3DeviceManager() = default;

    virtual const std::vector<DeviceSourceMap>& inputDeviceMaps() const = 0;
    virtual const std::vector<DeviceSourceMap>& outputDeviceMaps() const = 0;

    virtual int systemDefaultOutputDeviceIndex(const std::string& hostName) const = 0;
    virtual int systemDefaultInputDeviceIndex(const std::string& hostName) const = 0;

    virtual int hostIndex(const std::string& hostName) const = 0;

    virtual DeviceSourceMap* defaultOutputDevice(int hostIndex) const = 0;
    virtual DeviceSourceMap* defaultInputDevice(int hostIndex) const = 0;

    virtual void rescan() = 0;
};
}
