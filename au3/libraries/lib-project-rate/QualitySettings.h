/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 QualitySettings.h
 
 Paul Licameli split from QualityPrefs.h
 
 **********************************************************************/

#ifndef __AUDACITY_QUALITY_SETTINGS__
#define __AUDACITY_QUALITY_SETTINGS__

#include "Prefs.h" // for EnumSetting
#include "SampleFormat.h"

class IntSetting;

namespace QualitySettings {
extern PROJECT_RATE_API IntSetting DefaultSampleRate;
extern PROJECT_RATE_API EnumSetting< sampleFormat > SampleFormatSetting;
extern PROJECT_RATE_API sampleFormat SampleFormatChoice();
}

#endif
