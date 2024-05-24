/*
* Audacity: A Digital Audio Editor
*/

#include "au3audiodevicesmanager.h"

#include "libraries/lib-audio-devices/DeviceManager.h"
#include "libraries/lib-audio-devices/AudioIOBase.h"
#include "libraries/lib-utility/IteratorX.h"

#include "log.h"

using namespace muse;
using namespace au::au3;

void Au3AudioDevicesManager::init()
{
    initHosts();
    initHostDevices();
    initInputChannels();
}

void Au3AudioDevicesManager::initHosts()
{
    const std::vector<DeviceSourceMap>& inMaps = DeviceManager::Instance()->GetInputDeviceMaps();
    const std::vector<DeviceSourceMap>& outMaps = DeviceManager::Instance()->GetOutputDeviceMaps();

    wxArrayString hosts;

    // go over our lists add the host to the list if it isn't there yet

    for (auto& device : inMaps) {
        if (!make_iterator_range(hosts).contains(device.hostString)) {
            hosts.push_back(device.hostString);
        }
    }

    for (auto& device : outMaps) {
        if (!make_iterator_range(hosts).contains(device.hostString)) {
            hosts.push_back(device.hostString);
        }
    }

    mHost.Set(std::move(hosts));
}

void Au3AudioDevicesManager::initHostDevices()
{
    const std::vector<DeviceSourceMap>& inMaps = DeviceManager::Instance()->GetInputDeviceMaps();
    const std::vector<DeviceSourceMap>& outMaps = DeviceManager::Instance()->GetOutputDeviceMaps();

    //read what is in the prefs
    auto host = AudioIOHost.Read();
    int foundHostIndex = -1;

    // if the host is not in the hosts combo then we rescanned.
    // set it to blank so we search for another host.
    if (mHost.Find(host) < 0) {
        host = wxT("");
    }

    // Try to find a hostIndex, among either inputs or outputs, assumed to be
    // unique among the union of the set of input and output devices
    for (auto& device : outMaps) {
        if (device.hostString == host) {
            foundHostIndex = device.hostIndex;
            break;
        }
    }

    if (foundHostIndex == -1) {
        for (auto& device : inMaps) {
            if (device.hostString == host) {
                foundHostIndex = device.hostIndex;
                break;
            }
        }
    }

    // If no host was found based on the prefs device host, load the first available one
    if (foundHostIndex == -1) {
        if (outMaps.size()) {
            foundHostIndex = outMaps[0].hostIndex;
        } else if (inMaps.size()) {
            foundHostIndex = inMaps[0].hostIndex;
        }
    }

    // Make sure in/out are clear in case no host was found
    mInput.Clear();
    mOutput.Clear();

    // If we still have no host it means no devices, in which case do nothing.
    if (foundHostIndex == -1) {
        return;
    }

    // Repopulate the Input/Output device list available to the user
    wxArrayStringEx mInputDeviceNames;
    for (size_t i = 0; i < inMaps.size(); ++i) {
        auto& device = inMaps[i];
        if (foundHostIndex == device.hostIndex) {
            mInputDeviceNames.push_back(MakeDeviceSourceString(&device));
            if (host.empty()) {
                host = device.hostString;
                AudioIOHost.Write(host);
                mHost.Set(host);
            }
        }
    }
    mInput.Set(std::move(mInputDeviceNames));

    wxArrayStringEx mOutputDeviceNames;
    for (size_t i = 0; i < outMaps.size(); ++i) {
        auto& device = outMaps[i];
        if (foundHostIndex == device.hostIndex) {
            mOutputDeviceNames.push_back(MakeDeviceSourceString(&device));
            if (host.empty()) {
                host = device.hostString;
                AudioIOHost.Write(host);
                mHost.Set(host);
            }
        }
    }
    mOutput.Set(std::move(mOutputDeviceNames));

    gPrefs->Flush();

    // The setting of the Device is left up to menu handlers
}

void Au3AudioDevicesManager::initInputChannels()
{
    //!NOTE: Copied from AudioSetupToolBar::FillInputChannels

    const std::vector<DeviceSourceMap>& inMaps = DeviceManager::Instance()->GetInputDeviceMaps();
    auto host = AudioIOHost.Read();
    auto device = AudioIORecordingDevice.Read();
    auto source = AudioIORecordingSource.Read();
    long newChannels = 0;

    auto oldChannels = AudioIORecordChannels.Read();

    wxArrayStringEx names;
    for (auto& dev: inMaps) {
        if (source == dev.sourceString
            && device == dev.deviceString
            && host == dev.hostString) {
            // add one selection for each channel of this source
            for (size_t j = 0; j < (unsigned int)dev.numChannels; j++) {
                wxString name;

                if (j == 0) {
                    name = _("1 (Mono) Recording Channel");
                } else if (j == 1) {
                    name = _("2 (Stereo) Recording Channels");
                } else {
                    name = wxString::Format(wxT("%d"), (int)j + 1);
                }
                names.push_back(name);
            }
            newChannels = dev.numChannels;
            if (oldChannels <= newChannels && oldChannels >= 1) {
                newChannels = oldChannels;
            }
            AudioIORecordChannels.Write(newChannels);
            break;
        }
    }
    mInputChannels.Set(std::move(names));
    if (newChannels >= 1) {
        // Correct to 0-based index in choice
        mInputChannels.Set(newChannels - 1);
    }
}
