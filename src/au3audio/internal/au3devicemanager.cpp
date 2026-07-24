/*
* Audacity: A Digital Audio Editor
*/

#include "au3devicemanager.h"

using namespace au::au3audio;

const std::vector<DeviceSourceMap>& Au3DeviceManager::inputDeviceMaps()
{
    return DeviceManager::Instance()->GetInputDeviceMaps();
}

const std::vector<DeviceSourceMap>& Au3DeviceManager::outputDeviceMaps()
{
    return DeviceManager::Instance()->GetOutputDeviceMaps();
}

const DeviceSourceMap* Au3DeviceManager::defaultInputDevice(int hostIndex)
{
    return DeviceManager::Instance()->GetDefaultInputDevice(hostIndex);
}

const DeviceSourceMap* Au3DeviceManager::defaultOutputDevice(int hostIndex)
{
    return DeviceManager::Instance()->GetDefaultOutputDevice(hostIndex);
}

int Au3DeviceManager::hostIndex(const std::string& hostName)
{
    return DeviceManager::Instance()->GetHostIndex(hostName);
}

int Au3DeviceManager::inputDevicePaIndex(const std::string& hostName, const std::string& deviceId)
{
    return DeviceManager::Instance()->GetInputDevicePaIndex(hostName, deviceId);
}

int Au3DeviceManager::outputDevicePaIndex(const std::string& hostName, const std::string& deviceId)
{
    return DeviceManager::Instance()->GetOutputDevicePaIndex(hostName, deviceId);
}

void Au3DeviceManager::updateAsioDeviceCaps(int paDeviceIndex)
{
    DeviceManager::Instance()->UpdateAsioDeviceCaps(paDeviceIndex);
}

void Au3DeviceManager::showAsioControlPanel(int paDeviceIndex)
{
    DeviceManager::ShowAsioControlPanel(paDeviceIndex);
}

void Au3DeviceManager::rescan()
{
    DeviceManager::Instance()->Rescan();
}
