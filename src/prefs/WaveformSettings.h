/**********************************************************************

Audacity: A Digital Audio Editor

WaveformSettings.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_WAVEFORM_SETTINGS__
#define __AUDACITY_WAVEFORM_SETTINGS__

#include "Prefs.h"

class EnumValueSymbols;
class WaveTrack;

class AUDACITY_DLL_API WaveformSettings : public PrefsListener
{
public:

   //! Create waveform settings for the track on demand
   static WaveformSettings &Get( WaveTrack &track );

   //! @copydoc Get
   static const WaveformSettings &Get( const WaveTrack &track );

   //! Guarantee independence of settings, then assign
   static void Set(
      WaveTrack &track, std::unique_ptr<WaveformSettings> pSettings );

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
      stLinear,
      stLogarithmic,

      stNumScaleTypes,
   };

   static const EnumValueSymbols &GetScaleNames();

   ScaleType scaleType;
   int dBRange;

   // Convenience
   bool isLinear() const { return stLinear == scaleType; }
};
#endif
