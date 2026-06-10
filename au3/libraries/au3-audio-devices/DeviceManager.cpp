/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2010 Audacity Team
   Michael Chinen

******************************************************************/

#include "DeviceManager.h"

#include <map>
#include <thread>
#include <wx/log.h>

#include "RtAudioBackend.h"
#include "AudioIOBase.h"
#include "DeviceChange.h" // for HAVE_DEVICE_CHANGE

namespace {

std::vector<audacity::rta::ApiInfo> apisCached;

const std::vector<audacity::rta::ApiInfo>& apis()
{
    if (apisCached.empty()) {
        apisCached = audacity::rta::compiledApis();
    }
    return apisCached;
}

} // namespace

DeviceManager DeviceManager::dm;

DeviceManager* DeviceManager::Instance()
{
    return &dm;
}

const std::vector<DeviceSourceMap>& DeviceManager::GetInputDeviceMaps()
{
    if (!m_inited) {
        Init();
    }
    return mInputDeviceSourceMaps;
}

const std::vector<DeviceSourceMap>& DeviceManager::GetOutputDeviceMaps()
{
    if (!m_inited) {
        Init();
    }
    return mOutputDeviceSourceMaps;
}

static std::string GetBaseDeviceName(const DeviceSourceMap* map)
{
    if (!map) {
        return {};
    }
    std::string ret = map->deviceString.ToStdString(wxConvUTF8);
    if (map->totalSources > 1) {
        ret += ": " + map->sourceString.ToStdString(wxConvUTF8);
    }
    return ret;
}

std::string MakeDeviceSourceString(const DeviceSourceMap* map,
                                   const std::vector<DeviceSourceMap>& deviceMaps)
{
    if (!map) {
        return {};
    }

    std::string baseName = GetBaseDeviceName(map);

    int occurrence = 0;
    for (const auto& device : deviceMaps) {
        if (&device == map) {
            break;
        }
        if (device.hostString == map->hostString && GetBaseDeviceName(&device) == baseName) {
            occurrence++;
        }
    }

    if (occurrence == 0) {
        return baseName;
    } else {
        return baseName + "#" + std::to_string(occurrence + 1);
    }
}

static std::pair<std::string, int> ParseDeviceId(const std::string& stableId)
{
    size_t hashPos = stableId.rfind('#');
    if (hashPos == std::string::npos) {
        return { stableId, 0 };
    }

    std::string name = stableId.substr(0, hashPos);
    std::string indexStr = stableId.substr(hashPos + 1);

    try {
        int occurrence = std::stoi(indexStr) - 1;
        return { name, occurrence };
    } catch (...) {
        return { stableId, 0 };
    }
}

static int FindDeviceIndexByDeviceId(const std::vector<DeviceSourceMap>& deviceMaps,
                                     const std::string& hostName,
                                     const std::string& deviceId)
{
    auto [targetName, targetOccurrence] = ParseDeviceId(deviceId);

    std::map<std::string, int> nameCount;
    for (const auto& device : deviceMaps) {
        if (device.hostString != hostName) {
            continue;
        }
        std::string deviceName = GetBaseDeviceName(&device);
        int occurrence = nameCount[deviceName]++;

        if (deviceName == targetName && occurrence == targetOccurrence) {
            return device.deviceIndex;
        }
    }
    return -1;
}

int DeviceManager::GetInputDevicePaIndex(const std::string& hostName, const std::string& deviceId)
{
    if (!m_inited) {
        Init();
    }
    return FindDeviceIndexByDeviceId(mInputDeviceSourceMaps, hostName, deviceId);
}

int DeviceManager::GetOutputDevicePaIndex(const std::string& hostName, const std::string& deviceId)
{
    if (!m_inited) {
        Init();
    }
    return FindDeviceIndexByDeviceId(mOutputDeviceSourceMaps, hostName, deviceId);
}

DeviceSourceMap* DeviceManager::GetDefaultDevice(int hostIndex, int isInput)
{
    namespace rta = audacity::rta;
    const auto& apiList = apis();
    if (hostIndex < 0 || static_cast<size_t>(hostIndex) >= apiList.size()) {
        return nullptr;
    }

    std::vector<DeviceSourceMap>& maps = isInput ? mInputDeviceSourceMaps : mOutputDeviceSourceMaps;

    // Walk the cached maps for the requested host and return whichever entry
    // RtAudio flagged as default.
    for (auto& map : maps) {
        if (map.hostIndex != hostIndex) {
            continue;
        }
        // We need to ask rtaudio whether this is default; cheapest is to
        // re-enumerate the api once and match by encoded id.
        const auto devices = rta::enumerateDevices(apiList[hostIndex].api);
        for (const auto& d : devices) {
            if (audacity::rta::encodeDeviceIndex(hostIndex, d.id) != map.deviceIndex) {
                continue;
            }
            if ((isInput && d.isDefaultInput) || (!isInput && d.isDefaultOutput)) {
                return &map;
            }
        }
    }

    wxLogDebug(wxT("GetDefaultDevice() no default device"));
    return nullptr;
}

DeviceSourceMap* DeviceManager::GetDefaultOutputDevice(int hostIndex)
{
    return GetDefaultDevice(hostIndex, 0);
}

DeviceSourceMap* DeviceManager::GetDefaultInputDevice(int hostIndex)
{
    return GetDefaultDevice(hostIndex, 1);
}

int DeviceManager::GetHostIndex(const std::string& hostName)
{
    const auto& list = apis();
    for (size_t i = 0; i < list.size(); ++i) {
        if (list[i].displayName == hostName || list[i].name == hostName) {
            return static_cast<int>(i);
        }
    }
    return -1;
}

//--------------- Device Enumeration --------------------------

static void FillFromRtaDevice(DeviceSourceMap& map, int apiIndex,
                              const wxString& hostName,
                              const audacity::rta::DeviceInfo& d, bool isInput)
{
    map.deviceIndex  = audacity::rta::encodeDeviceIndex(apiIndex, d.id);
    map.hostIndex    = apiIndex;
    map.deviceString = wxString::FromUTF8(d.name.c_str());
    map.hostString   = hostName;
    map.numChannels  = isInput ? d.maxInputChannels : d.maxOutputChannels;
    map.sourceIndex  = 0;
    map.totalSources = 0;   // Source enumeration is not exposed by the backend; software gain only.
    map.sourceString.clear();
}

void DeviceManager::Rescan()
{
    namespace rta = audacity::rta;

    mInputDeviceSourceMaps.clear();
    mOutputDeviceSourceMaps.clear();

    // Stop monitoring before tearing down so the active stream is released
    // before we refresh the cached device list.
    if (m_inited) {
        auto gAudioIO = AudioIOBase::Get();
        gAudioIO->StopMonitoring(); // TODO Inject and use IAudioEngine instead
    }

    // Refresh the api list each rescan so a fresh enumeration runs against
    // the backend's current view of the system.
    apisCached = rta::compiledApis();

    for (size_t apiIdx = 0; apiIdx < apisCached.size(); ++apiIdx) {
        const auto& api = apisCached[apiIdx];
        const wxString hostName = wxString::FromUTF8(api.displayName.c_str());

        const auto devices = rta::enumerateDevices(api.api);
        for (const auto& d : devices) {
            if (d.maxOutputChannels > 0) {
                DeviceSourceMap map;
                FillFromRtaDevice(map, static_cast<int>(apiIdx), hostName, d, /*isInput*/ false);
                mOutputDeviceSourceMaps.push_back(map);
            }
            if (d.maxInputChannels > 0) {
                DeviceSourceMap map;
                FillFromRtaDevice(map, static_cast<int>(apiIdx), hostName, d, /*isInput*/ true);
                mInputDeviceSourceMaps.push_back(map);
            }
        }
    }

    if (m_inited) {
        Publish(DeviceChangeMessage::Rescan);
    }

    m_inited = true;
    mRescanTime = std::chrono::steady_clock::now();
}

std::chrono::duration<float> DeviceManager::GetTimeSinceRescan()
{
    auto now = std::chrono::steady_clock::now();
    auto dur = std::chrono::duration_cast<std::chrono::duration<float> >(now - mRescanTime);
    return dur;
}

DeviceManager::DeviceManager()
#if defined(EXPERIMENTAL_DEVICE_CHANGE_HANDLER)
#if defined(HAVE_DEVICE_CHANGE)
    :  DeviceChangeHandler()
#endif
#endif
{
    m_inited = false;
    mRescanTime = std::chrono::steady_clock::now();
}

DeviceManager::~DeviceManager()
{
}

void DeviceManager::Init()
{
    Rescan();

#if defined(EXPERIMENTAL_DEVICE_CHANGE_HANDLER)
#if defined(HAVE_DEVICE_CHANGE)
    DeviceChangeHandler::Enable(true);
#endif
#endif
}

#if defined(EXPERIMENTAL_DEVICE_CHANGE_HANDLER)
#if defined(HAVE_DEVICE_CHANGE)
void DeviceManager::DeviceChangeNotification()
{
    Rescan();
    return;
}

#endif
#endif
