/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <algorithm>
#include <map>
#include <string>
#include <vector>

#include "au3audio/iau3devicemanager.h"

namespace au::au3audio {
//! NOTE Seedable in-memory replacement for the AU3 DeviceManager:
//! tests fill the device maps, mutate them and call the provider
class Au3DeviceManagerFake : public IAu3DeviceManager
{
public:
    std::vector<std::string> hosts; // position == host index
    std::vector<DeviceSourceMap> inMaps;
    std::vector<DeviceSourceMap> outMaps;
    std::map<int, int> defaultInputByHost;  // host index -> device index
    std::map<int, int> defaultOutputByHost;
    int rescanCount = 0;

    DeviceSourceMap& addInputDevice(const std::string& host, const std::string& name, int deviceIndex, int numChannels)
    {
        inMaps.push_back(makeDevice(host, name, deviceIndex, numChannels));
        return inMaps.back();
    }

    DeviceSourceMap& addOutputDevice(const std::string& host, const std::string& name, int deviceIndex, int numChannels = 2)
    {
        outMaps.push_back(makeDevice(host, name, deviceIndex, numChannels));
        return outMaps.back();
    }

    void setDefaultInputDevice(const std::string& host, int deviceIndex)
    {
        defaultInputByHost[ensureHost(host)] = deviceIndex;
    }

    void setDefaultOutputDevice(const std::string& host, int deviceIndex)
    {
        defaultOutputByHost[ensureHost(host)] = deviceIndex;
    }

    const std::vector<DeviceSourceMap>& inputDeviceMaps() override
    {
        return inMaps;
    }

    const std::vector<DeviceSourceMap>& outputDeviceMaps() override
    {
        return outMaps;
    }

    const DeviceSourceMap* defaultInputDevice(int hostIdx) override
    {
        return findDefault(inMaps, defaultInputByHost, hostIdx);
    }

    const DeviceSourceMap* defaultOutputDevice(int hostIdx) override
    {
        return findDefault(outMaps, defaultOutputByHost, hostIdx);
    }

    int hostIndex(const std::string& hostName) override
    {
        const auto it = std::find(hosts.begin(), hosts.end(), hostName);
        return it == hosts.end() ? -1 : static_cast<int>(std::distance(hosts.begin(), it));
    }

    int inputDevicePaIndex(const std::string&, const std::string&) override
    {
        return -1;
    }

    int outputDevicePaIndex(const std::string&, const std::string&) override
    {
        return -1;
    }

    void updateAsioDeviceCaps(int) override
    {
    }

    void showAsioControlPanel(int) override
    {
    }

    void rescan() override
    {
        ++rescanCount;
    }

private:
    DeviceSourceMap makeDevice(const std::string& host, const std::string& name, int deviceIndex, int numChannels)
    {
        DeviceSourceMap map;
        map.deviceIndex = deviceIndex;
        map.sourceIndex = 0;
        map.hostIndex = ensureHost(host);
        map.totalSources = 0;
        map.numChannels = numChannels;
        map.deviceString = wxString(name);
        map.hostString = wxString(host);
        return map;
    }

    int ensureHost(const std::string& host)
    {
        const auto it = std::find(hosts.begin(), hosts.end(), host);
        if (it != hosts.end()) {
            return static_cast<int>(std::distance(hosts.begin(), it));
        }
        hosts.push_back(host);
        return static_cast<int>(hosts.size()) - 1;
    }

    const DeviceSourceMap* findDefault(const std::vector<DeviceSourceMap>& maps, const std::map<int, int>& defaults, int hostIdx) const
    {
        const auto it = defaults.find(hostIdx);
        if (it == defaults.end()) {
            return nullptr;
        }
        for (const auto& map : maps) {
            if (map.deviceIndex == it->second) {
                return &map;
            }
        }
        return nullptr;
    }
};
}
