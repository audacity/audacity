/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "au3audio/iau3devicemanager.h"

namespace au::au3audio {
class Au3DeviceManager : public IAu3DeviceManager
{
public:
    const std::vector<DeviceSourceMap>& inputDeviceMaps() override;
    const std::vector<DeviceSourceMap>& outputDeviceMaps() override;

    const DeviceSourceMap* defaultInputDevice(int hostIndex) override;
    const DeviceSourceMap* defaultOutputDevice(int hostIndex) override;

    int hostIndex(const std::string& hostName) override;

    int inputDevicePaIndex(const std::string& hostName, const std::string& deviceId) override;
    int outputDevicePaIndex(const std::string& hostName, const std::string& deviceId) override;

    void updateAsioDeviceCaps(int paDeviceIndex) override;
    void showAsioControlPanel(int paDeviceIndex) override;

    void rescan() override;
};
}
