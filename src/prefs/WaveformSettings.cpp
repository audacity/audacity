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
{
}

WaveformSettings &WaveformSettings::operator= (const WaveformSettings &other)
{
   if (this != &other) {
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

   return true;
}

void WaveformSettings::LoadPrefs()
{
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

WaveformSettings::~WaveformSettings()
{
}
