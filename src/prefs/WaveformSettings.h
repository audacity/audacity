/**********************************************************************

Audacity: A Digital Audio Editor

WaveformSettings.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_WAVEFORM_SETTINGS__
#define __AUDACITY_WAVEFORM_SETTINGS__

class wxArrayString;

class WaveformSettings
{
public:

   // Singleton for settings that are not per-track
   class Globals
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

   enum ScaleType {
      stLinear,
      stLogarithmic,

      stNumScaleTypes,
   };

   static void InvalidateNames(); // in case of language change
   static const wxArrayString &GetScaleNames();

   ScaleType scaleType;

   // Convenience
   bool isLinear() const { return stLinear == scaleType; }
};
#endif
