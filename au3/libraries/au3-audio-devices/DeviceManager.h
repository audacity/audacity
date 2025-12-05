/**********************************************************************

  Audacity: A Digital Audio Editor

  DeviceManager.h

  Created by Michael Chinen (mchinen) on 2/12/11
  Audacity(R) is copyright (c) 1999-2011 Audacity Team.
  License: GPL v2 or later.  See License.txt.

******************************************************************//**

\class DeviceManager
\brief A singleton that manages the audio devices known to Audacity

*//*******************************************************************/

#ifndef __AUDACITY_DEVICEMANAGER__
#define __AUDACITY_DEVICEMANAGER__

#include <chrono>
#include <vector>

#include <wx/string.h> // member variables

#include "DeviceChange.h"

typedef struct DeviceSourceMap {
    int deviceIndex;
    int sourceIndex;
    int hostIndex;
    int totalSources;
    int numChannels;
    wxString sourceString;
    wxString deviceString;
    wxString hostString;
} DeviceSourceMap;

AUDIO_DEVICES_API
wxString MakeDeviceSourceString(const DeviceSourceMap* map);

class AUDIO_DEVICES_API DeviceManager final
#if defined(EXPERIMENTAL_DEVICE_CHANGE_HANDLER) && defined(HAVE_DEVICE_CHANGE)
    : public DeviceChangeHandler
#else
    : public DeviceChangeMessagePublisher
#endif
{
public:
    /// Gets the singleton instance
    static DeviceManager* Instance();

    /// Gets a NEW list of devices by terminating and restarting portaudio
    /// Assumes that DeviceManager is only used on the main thread.
    void Rescan();

    // Time since devices scanned in seconds.
    std::chrono::duration<float> GetTimeSinceRescan();

    DeviceSourceMap* GetDefaultOutputDevice(int hostIndex);
    DeviceSourceMap* GetDefaultInputDevice(int hostIndex);

    const std::vector<DeviceSourceMap>& GetInputDeviceMaps();
    const std::vector<DeviceSourceMap>& GetOutputDeviceMaps();

#if defined(EXPERIMENTAL_DEVICE_CHANGE_HANDLER)
#if defined(HAVE_DEVICE_CHANGE)
    // DeviceChangeHandler implementation
    void DeviceChangeNotification();
#endif
#endif

private:
    std::chrono::time_point<std::chrono::steady_clock> mRescanTime;

protected:
    //private constructor - Singleton.
    DeviceManager();
    ~DeviceManager();
    /// Does an initial scan.
    /// Called by GetInputDeviceMaps and GetOutputDeviceMaps when needed.
    void Init();

    DeviceSourceMap* GetDefaultDevice(int hostIndex, int isInput);

    bool m_inited;

    std::vector<DeviceSourceMap> mInputDeviceSourceMaps;
    std::vector<DeviceSourceMap> mOutputDeviceSourceMaps;

    static DeviceManager dm;
};

#endif
