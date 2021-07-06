/**********************************************************************
 
 Sneedacity: A Digital Audio Editor
 
 QualitySettings.h
 
 Paul Licameli split from QualityPrefs.h
 
 **********************************************************************/

#ifndef __SNEEDACITY_QUALITY_SETTINGS__
#define __SNEEDACITY_QUALITY_SETTINGS__

#include "Prefs.h" // for EnumSetting

class IntSetting;

namespace QualitySettings {
extern SNEEDACITY_DLL_API IntSetting DefaultSampleRate;
extern SNEEDACITY_DLL_API EnumSetting< sampleFormat > SampleFormatSetting;
extern SNEEDACITY_DLL_API sampleFormat SampleFormatChoice();
}

#endif
