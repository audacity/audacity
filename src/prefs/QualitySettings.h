/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 QualitySettings.h
 
 Paul Licameli split from QualityPrefs.h
 
 **********************************************************************/

#ifndef __AUDACITY_QUALITY_SETTINGS__
#define __AUDACITY_QUALITY_SETTINGS__

#include "Prefs.h" // for EnumSetting

class IntSetting;

namespace QualitySettings {
extern IntSetting DefaultSampleRate;
extern EnumSetting< sampleFormat > SampleFormatSetting;
extern sampleFormat SampleFormatChoice();
}

#endif
