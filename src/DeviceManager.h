/**********************************************************************

  Audacity: A Digital Audio Editor

  DeviceManager.h

  Created by Michael Chinen (mchinen) on 2/12/11
  Audacity(R) is copyright (c) 1999-2011 Audacity Team.
  License: GPL v2.  See License.txt.

******************************************************************//**

\class DeviceManager
\brief A singleton that manages the audio devices known to Audacity

*//*******************************************************************/

#ifndef __AUDACITY_DEVICEMANAGER__
#define __AUDACITY_DEVICEMANAGER__

#include <vector>
#include "wx/wx.h"

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

wxString MakeDeviceSourceString(const DeviceSourceMap *map);

class DeviceManager
{
 public:
   /// Gets the singleton instance
   static DeviceManager* Instance();

   /// Releases memory assosiated with the singleton
   static void Destroy();

   /// Gets a new list of devices by terminating and restarting portaudio
   /// Assumes that DeviceManager is only used on the main thread.
   void Rescan();

   DeviceSourceMap* GetDefaultOutputDevice(int hostIndex);
   DeviceSourceMap* GetDefaultInputDevice(int hostIndex);

   const std::vector<DeviceSourceMap> &GetInputDeviceMaps();
   const std::vector<DeviceSourceMap> &GetOutputDeviceMaps();

  protected:
   //private constructor - Singleton.
   DeviceManager();
   virtual ~DeviceManager();
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

