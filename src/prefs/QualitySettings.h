/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 QualitySettings.h
 
 Paul Licameli split from QualityPrefs.h
 
 **********************************************************************/

#ifndef __AUDACITY_QUALITY_SETTINGS__
#define __AUDACITY_QUALITY_SETTINGS__

#include <audacity/Types.h>
#include "Prefs.h" // for EnumSetting
#include "SampleFormat.h"

class IntSetting;

namespace QualitySettings {
extern AUDACITY_DLL_API IntSetting DefaultSampleRate;
extern AUDACITY_DLL_API EnumSetting< sampleFormat > SampleFormatSetting;
extern AUDACITY_DLL_API sampleFormat SampleFormatChoice();
}

#endif
