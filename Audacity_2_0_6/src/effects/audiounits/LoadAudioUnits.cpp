/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadAudioUnits.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/defs.h>
#include <wx/wx.h>

#include "../EffectManager.h"
#include "AudioUnitEffect.h"

void LoadAudioUnitsOfType(OSType inAUType)
{
   ComponentDescription desc;
   Component component;

   desc.componentType = inAUType;
   desc.componentSubType = 0;
   desc.componentManufacturer = 0;
   desc.componentFlags = 0;
   desc.componentFlagsMask = 0;

   component = FindNextComponent(NULL, &desc);
   while (component != NULL) {
      ComponentDescription found;
      Handle nameHandle = NewHandle(0);
      GetComponentInfo(component, &found, nameHandle, 0, 0);
      HLock(nameHandle);
      int len = ((const char *)(*nameHandle))[0];
      wxString name(((const char *)(*nameHandle)+1), wxConvISO8859_1, len);
      HUnlock(nameHandle);
      DisposeHandle(nameHandle);

      EffectManager::Get().RegisterEffect(new AudioUnitEffect(name, component));

      component = FindNextComponent (component, &desc);
   }
}

void LoadAudioUnits()
{
   LoadAudioUnitsOfType(kAudioUnitType_Effect); //'aufx'
   LoadAudioUnitsOfType(kAudioUnitType_MusicEffect); //'aumf'
}
