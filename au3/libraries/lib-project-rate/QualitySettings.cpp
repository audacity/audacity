/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 QualitySettings.cpp
 
 Paul Licameli split from QualityPrefs.cpp
 
 **********************************************************************/

#include "QualitySettings.h"
#include "AudioIOBase.h"
#include "Internat.h"

IntSetting QualitySettings::DefaultSampleRate{
   L"/SamplingRate/DefaultProjectSampleRate",
   AudioIOBase::GetOptimalSupportedSampleRate
};

EnumSetting< sampleFormat > QualitySettings::SampleFormatSetting{
   L"/SamplingRate/DefaultProjectSampleFormatChoice",
   {
      { L"Format16Bit", XO("16-bit") },
      { L"Format24Bit", XO("24-bit") },
      { L"Format32BitFloat", XO("32-bit float") }
   },
   2, // floatSample

   // for migrating old preferences:
   {
      int16Sample,
      int24Sample,
      floatSample
   },
   L"/SamplingRate/DefaultProjectSampleFormat",
};

sampleFormat QualitySettings::SampleFormatChoice()
{
   return SampleFormatSetting.ReadEnum();
}
