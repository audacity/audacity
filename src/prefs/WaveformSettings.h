/**********************************************************************

Audacity: A Digital Audio Editor

WaveformSettings.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_WAVEFORM_SETTINGS__
#define __AUDACITY_WAVEFORM_SETTINGS__

#include "Prefs.h"

class EnumValueSymbols;

class AUDACITY_DLL_API WaveformSettings : public PrefsListener
{
public:

   // Singleton for settings that are not per-track
   class AUDACITY_DLL_API Globals
   {
   public:
      static Globals &Get();
      void SavePrefs();

   private:
      Globals();
      void LoadPrefs();
   };

   static WaveformSettings &defaults();
   WaveformSettings();
   WaveformSettings(const WaveformSettings &other);
   WaveformSettings& operator= (const WaveformSettings &other);
   ~WaveformSettings();

   bool IsDefault() const
   {
      return this == &defaults();
   }

   bool Validate(bool quiet);
   void LoadPrefs();
   void SavePrefs();
   void Update();

   void UpdatePrefs() override;

   void ConvertToEnumeratedDBRange();
   void ConvertToActualDBRange();
   void NextLowerDBRange();
   void NextHigherDBRange();

   typedef int ScaleType;
   enum ScaleTypeValues : int {
      stLinearAmp,
      stLogarithmicDb,
      stLinearDb,

      stNumScaleTypes,
   };

   static const EnumValueSymbols &GetScaleNames();

   ScaleType scaleType;
   int dBRange;

   // Convenience
   bool isLinear() const { return scaleType == stLinearAmp || scaleType == stLinearDb; }
   bool isAmp() const { return scaleType == stLinearAmp; }
};
#endif
