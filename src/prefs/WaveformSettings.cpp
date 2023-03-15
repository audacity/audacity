/**********************************************************************

Audacity: A Digital Audio Editor

WaveformSettings.cpp

Paul Licameli

*******************************************************************//**

\class WaveformSettings
\brief Waveform settings, either for one track or as defaults.

*//*******************************************************************/


#include "WaveformSettings.h"
#include "WaveTrack.h"

#include "Decibels.h"
#include "GUIPrefs.h"
#include "TracksPrefs.h"

#include <algorithm>

#include "Prefs.h"


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

static WaveTrack::Attachments::RegisteredFactory key1{
   [](SampleTrack&){
      return std::make_unique<WaveformSettings>(WaveformSettings::defaults());
   }
};

WaveformSettings &WaveformSettings::Get(const WaveTrack &track)
{
   auto &mutTrack = const_cast<WaveTrack&>(track);
   return static_cast<WaveformSettings&>(
      mutTrack.WaveTrack::Attachments::Get(key1));
}

void WaveformSettings::Set(
   WaveTrack &track, std::unique_ptr<WaveformSettings> pSettings)
{
   track.WaveTrack::Attachments::Assign(key1, move(pSettings));
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
   scaleType = TracksPrefs::WaveformScaleChoice();

   dBRange = DecibelScaleCutoff.Read();

   // Enforce legal values
   Validate(true);

   Update();
}

void WaveformSettings::SavePrefs()
{
}

void WaveformSettings::Update()
{
}

// This is a temporary hack until WaveformSettings gets fully integrated
void WaveformSettings::UpdatePrefs()
{
   if (scaleType == defaults().scaleType) {
      scaleType = TracksPrefs::WaveformScaleChoice();
   }

   if (dBRange == defaults().dBRange){
      dBRange = DecibelScaleCutoff.Read();
   }

   // Enforce legal values
   Validate(true);
}

void WaveformSettings::ConvertToEnumeratedDBRange()
{
   // Assumes the codes are in ascending sequence.
   wxArrayStringEx codes;
   GUIPrefs::GetRangeChoices(nullptr, &codes);
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
   wxArrayStringEx codes;
   GUIPrefs::GetRangeChoices(nullptr, &codes);
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
const EnumValueSymbols &WaveformSettings::GetScaleNames()
{
   static const EnumValueSymbols result{
      // Keep in correspondence with ScaleTypeValues:
      { wxT("Linear"), XO("Linear (amp)") },
      { wxT("dB"), XO("Logarithmic (dB)") },
      { wxT("LinearDB"), XO("Linear (dB)") },
   };
   return result;
}

WaveformSettings::~WaveformSettings()
{
}

auto WaveformSettings::Clone() const -> PointerType
{
   return std::make_unique<WaveformSettings>(*this);
}

static WaveTrack::Attachments::RegisteredFactory key2{
   [](SampleTrack&){
      return std::make_unique<WaveformScale>();
   }
};

WaveformScale &WaveformScale::Get(const WaveTrack &track)
{
   auto &mutTrack = const_cast<WaveTrack&>(track);
   return static_cast<WaveformScale&>(
      mutTrack.WaveTrack::Attachments::Get(key2));
}

WaveformScale::~WaveformScale() = default;

auto WaveformScale::Clone() const -> PointerType
{
   return std::make_unique<WaveformScale>(*this);
}

int WaveformScale::ZeroLevelYCoordinate(wxRect rect) const
{
   return rect.GetTop() +
      (int)((mDisplayMax / (mDisplayMax - mDisplayMin)) * rect.height);
}
