/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>
#include <vector>

#include "framework/global/modularity/imoduleinterface.h"

#include "au3-audio-devices/DeviceManager.h"

namespace au::au3audio {
//! NOTE Facade over the AU3 DeviceManager singleton, so that device
//! enumeration can be replaced in tests
class IAu3DeviceManager : MODULE_GLOBAL_INTERFACE
{
    INTERFACE_ID(IAu3DeviceManager)
public:
    virtual ~IAu3DeviceManager() = default;

    virtual const std::vector<DeviceSourceMap>& inputDeviceMaps() = 0;
    virtual const std::vector<DeviceSourceMap>& outputDeviceMaps() = 0;

    virtual const DeviceSourceMap* defaultInputDevice(int hostIndex) = 0;
    virtual const DeviceSourceMap* defaultOutputDevice(int hostIndex) = 0;

    virtual int hostIndex(const std::string& hostName) = 0;

    virtual int inputDevicePaIndex(const std::string& hostName, const std::string& deviceId) = 0;
    virtual int outputDevicePaIndex(const std::string& hostName, const std::string& deviceId) = 0;

    virtual void updateAsioDeviceCaps(int paDeviceIndex) = 0;
    virtual void showAsioControlPanel(int paDeviceIndex) = 0;

    virtual void rescan() = 0;
};
}
