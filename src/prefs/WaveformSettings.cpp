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

#include <algorithm>
#include <wx/intl.h>

#include "../Prefs.h"

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
{
}

WaveformSettings &WaveformSettings::operator= (const WaveformSettings &other)
{
   if (this != &other) {
      scaleType = other.scaleType;
   }
   return *this;
}

WaveformSettings& WaveformSettings::defaults()
{
   static WaveformSettings instance;
   return instance;
}

bool WaveformSettings::Validate(bool quiet)
{
   quiet;

   scaleType = ScaleType(
      std::max(0, std::min(int(stNumScaleTypes) - 1, int(scaleType)))
   );

   return true;
}

void WaveformSettings::LoadPrefs()
{
   scaleType = ScaleType(gPrefs->Read(wxT("/Waveform/ScaleType"), 0L));

   // Enforce legal values
   Validate(true);

   Update();
}

void WaveformSettings::SavePrefs()
{
   gPrefs->Write(wxT("/Waveform/ScaleType"), long(scaleType));
}

void WaveformSettings::Update()
{
}

namespace
{
   wxArrayString &scaleNamesArray()
   {
      static wxArrayString theArray;
      return theArray;
   }
}

//static
void WaveformSettings::InvalidateNames()
{
   scaleNamesArray().Clear();
}

//static
const wxArrayString &WaveformSettings::GetScaleNames()
{
   wxArrayString &theArray = scaleNamesArray();

   if (theArray.IsEmpty()) {
      // Keep in correspondence with enum WaveTrack::WaveTrackDisplay:
      theArray.Add(_("Linear"));
      theArray.Add(_("Logarithmic"));
   }

   return theArray;
}

WaveformSettings::~WaveformSettings()
{
}
