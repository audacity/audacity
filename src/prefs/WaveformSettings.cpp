/**********************************************************************

Audacity: A Digital Audio Editor

WaveformSettings.cpp

Paul Licameli

*******************************************************************//**

\class WaveformSettings
\brief Waveform settings, either for one track or as defaults.

*//*******************************************************************/

#include "../Audacity.h"
#include "WaveformSettings.h"
#include "GUISettings.h"
#include "GUIPrefs.h"

#include <algorithm>
#include <wx/intl.h>

#include "../Prefs.h"
#include "../TranslatableStringArray.h"

WaveformSettings::Globals::Globals()
{
   LoadPrefs();
}

void WaveformSettings::Globals::SavePrefs()
{
}

void WaveformSettings::Globals::LoadPrefs()
{
}

WaveformSettings::Globals
&WaveformSettings::Globals::Get()
{
   static Globals instance;
   return instance;
}

WaveformSettings::WaveformSettings()
{
   LoadPrefs();
}

WaveformSettings::WaveformSettings(const WaveformSettings &other)
   : scaleType(other.scaleType)
   , dBRange(other.dBRange)
{
}

WaveformSettings &WaveformSettings::operator= (const WaveformSettings &other)
{
   if (this != &other) {
      scaleType = other.scaleType;
      dBRange = other.dBRange;
   }
   return *this;
}

WaveformSettings& WaveformSettings::defaults()
{
   static WaveformSettings instance;
   return instance;
}

bool WaveformSettings::Validate(bool /* quiet */)
{
   scaleType = ScaleType(
      std::max(0, std::min((int)(stNumScaleTypes) - 1, (int)(scaleType)))
   );

   ConvertToEnumeratedDBRange();
   ConvertToActualDBRange();

   return true;
}

void WaveformSettings::LoadPrefs()
{
   scaleType = ScaleType(gPrefs->Read(wxT("/Waveform/ScaleType"), 0L));
   bool newPrefFound = gPrefs->Read(wxT("/Waveform/dBRange"), &dBRange);
   if (!newPrefFound)
      dBRange = gPrefs->Read(ENV_DB_KEY, ENV_DB_RANGE);

   // Enforce legal values
   Validate(true);

   Update();
}

void WaveformSettings::SavePrefs()
{
   gPrefs->Write(wxT("/Waveform/ScaleType"), long(scaleType));
   gPrefs->Write(wxT("/Waveform/dBRange"), long(dBRange));
}

void WaveformSettings::Update()
{
}

void WaveformSettings::ConvertToEnumeratedDBRange()
{
   // Assumes the codes are in ascending sequence.
   wxArrayString codes;
   GUIPrefs::GetRangeChoices(NULL, &codes);
   int ii = 0;
   for (int nn = codes.size(); ii < nn; ++ii) {
      long value = 0;
      codes[ii].ToLong(&value);
      if (dBRange < value)
         break;
   }
   dBRange = std::max(0, ii - 1);
}

void WaveformSettings::ConvertToActualDBRange()
{
   wxArrayString codes;
   GUIPrefs::GetRangeChoices(NULL, &codes);
   long value = 0;
   codes[std::max(0, std::min((int)(codes.size()) - 1, dBRange))]
      .ToLong(&value);
   dBRange = (int)(value);
}

void WaveformSettings::NextLowerDBRange()
{
   ConvertToEnumeratedDBRange();
   ++dBRange;
   ConvertToActualDBRange();
}

void WaveformSettings::NextHigherDBRange()
{
   ConvertToEnumeratedDBRange();
   --dBRange;
   ConvertToActualDBRange();
}

//static
const wxArrayString &WaveformSettings::GetScaleNames()
{
   class ScaleNamesArray final : public TranslatableStringArray
   {
      void Populate() override
      {
         // Keep in correspondence with enum WaveTrack::WaveTrackDisplay:
         mContents.Add(_("Linear"));
         mContents.Add(_("Logarithmic"));
      }
   };

   static ScaleNamesArray theArray;
   return theArray.Get();
}

WaveformSettings::~WaveformSettings()
{
}
