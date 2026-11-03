/*
* Audacity: A Digital Audio Editor
*/
#include "portaudiodevices.h"

#include <algorithm>

#include "framework/global/containers.h"
#include "framework/global/modularity/ioc.h"
#include "framework/global/settings.h"

#include "au3wrap/internal/wxtypes_convert.h"

#include "au3-audio-devices/DeviceManager.h"

using namespace au::au3;
using namespace au::au3audio;

namespace {
const muse::Settings::Key RECORDING_SOURCE("au3audio", "AudioIO/RecordingSource");
const muse::Settings::Key RECORDING_SOURCE_INDEX("au3audio", "AudioIO/RecordingSourceIndex");

std::vector<std::string> devicesOf(const std::vector<DeviceSourceMap>& maps, const std::string& api)
{
    std::vector<std::string> result;
    for (const auto& device : maps) {
        if (device.hostString == api) {
            result.push_back(MakeDeviceSourceString(&device, maps));
        }
    }
    return result;
}

const DeviceSourceMap* findDevice(const std::vector<DeviceSourceMap>& maps,
                                  const std::string& api, const std::string& device)
{
    if (device.empty()) {
        return nullptr;
    }
    const auto it = std::find_if(maps.begin(), maps.end(), [&](const DeviceSourceMap& candidate) {
        return candidate.hostString == api && MakeDeviceSourceString(&candidate, maps) == device;
    });
    return it == maps.end() ? nullptr : &*it;
}
}

std::vector<std::string> PortAudioDevices::apis() const
{
    std::vector<std::string> result;
    const auto appendHosts = [&result](const auto& devices) {
        for (const auto& device : devices) {
            const auto host = wxToStdString(device.hostString);
            if (!muse::contains(result, host)) {
                result.push_back(host);
            }
        }
    };
    appendHosts(DeviceManager::Instance()->GetInputDeviceMaps());
    appendHosts(DeviceManager::Instance()->GetOutputDeviceMaps());
    return result;
}

std::string PortAudioDevices::preferredApi(const std::vector<std::string>& hosts) const
{
    if (hosts.empty()) {
        return {};
    }

#if defined(_WIN32)
    constexpr const char* preferred[] = { "Windows WASAPI", "ASIO", "Windows DirectSound", "MME" };
#elif defined(__APPLE__)
    constexpr const char* preferred[] = { "Core Audio" };
#elif defined(__linux__)
    constexpr const char* preferred[] = { "ALSA", "JACK", "OSS" };
#else
    constexpr const char* preferred[] = {};
#endif

    for (const auto candidate : preferred) {
        if (muse::contains(hosts, std::string(candidate))) {
            return candidate;
        }
    }
    return hosts.front();
}

std::vector<std::string> PortAudioDevices::outputDevices(const std::string& api) const
{
    return devicesOf(DeviceManager::Instance()->GetOutputDeviceMaps(), api);
}

std::vector<std::string> PortAudioDevices::inputDevices(const std::string& api) const
{
    return devicesOf(DeviceManager::Instance()->GetInputDeviceMaps(), api);
}

std::string PortAudioDevices::systemDefaultOutputDevice(const std::string& api) const
{
    const auto& maps = DeviceManager::Instance()->GetOutputDeviceMaps();
    const int hostIndex = DeviceManager::Instance()->GetHostIndex(api);
    return MakeDeviceSourceString(DeviceManager::Instance()->GetDefaultOutputDevice(hostIndex), maps);
}

std::string PortAudioDevices::systemDefaultInputDevice(const std::string& api) const
{
    const auto& maps = DeviceManager::Instance()->GetInputDeviceMaps();
    const int hostIndex = DeviceManager::Instance()->GetHostIndex(api);
    return MakeDeviceSourceString(DeviceManager::Instance()->GetDefaultInputDevice(hostIndex), maps);
}

int PortAudioDevices::inputChannels(const std::string& api, const std::string& inputDevice) const
{
    const DeviceSourceMap* device = findDevice(DeviceManager::Instance()->GetInputDeviceMaps(), api, inputDevice);
    return device ? device->numChannels : 0;
}

std::vector<uint64_t> PortAudioDevices::sampleRates() const
{
    std::vector<uint64_t> rates;
    for (int i = 0; i < DeviceManager::NumStandardRates; ++i) {
        rates.push_back(DeviceManager::StandardRates[i]);
    }
    return rates;
}

void PortAudioDevices::rescan()
{
    DeviceManager::Instance()->Rescan();
}

void PortAudioDevices::refreshInputDevice(const std::string& api, const std::string& inputDevice)
{
    const DeviceSourceMap* device = findDevice(DeviceManager::Instance()->GetInputDeviceMaps(), api, inputDevice);
    if (!device) {
        return;
    }

    const auto settings = muse::settings();
    DeviceManager::Instance()->UpdateAsioDeviceCaps(device->deviceIndex);
    settings->setLocalValue(RECORDING_SOURCE_INDEX, muse::Val(device->sourceIndex));
    settings->setLocalValue(RECORDING_SOURCE,
                            muse::Val(device->totalSources >= 1 ? wxToStdString(device->sourceString) : std::string()));
}

int PortAudioDevices::asioDeviceIndex(const std::string& api, const std::string& outputDevice, const std::string& inputDevice) const
{
    int paIndex = DeviceManager::Instance()->GetOutputDevicePaIndex(api, outputDevice);
    if (paIndex < 0) {
        paIndex = DeviceManager::Instance()->GetInputDevicePaIndex(api, inputDevice);
    }
    return paIndex;
}

bool PortAudioDevices::hasAsioControlPanel(const std::string& api, const std::string& outputDevice,
                                           const std::string& inputDevice) const
{
    return asioDeviceIndex(api, outputDevice, inputDevice) >= 0;
}

void PortAudioDevices::showAsioControlPanel(const std::string& api, const std::string& outputDevice,
                                            const std::string& inputDevice)
{
    DeviceManager::ShowAsioControlPanel(asioDeviceIndex(api, outputDevice, inputDevice));
}
