/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file EffectPlugin.cpp

  Paul Licameli split from EffectInterface.cpp

**********************************************************************/
#include "EffectPlugin.h"
#include "BasicUI.h"
#include "WaveTrack.h"
#include <wx/window.h>

void EffectContext::CountWaveTracks(const TrackList &tracks)
{
   numTracks = tracks.Selected< const WaveTrack >().size();
   numGroups = tracks.SelectedLeaders< const WaveTrack >().size();
}

bool EffectContext::TotalProgress(
   double frac, const TranslatableString &msg) const
{
   auto updateResult = (pProgress
      ? pProgress->Poll(frac * 1000, 1000, msg)
      : BasicUI::ProgressResult::Success);
   return (updateResult != BasicUI::ProgressResult::Success);
}

bool EffectContext::TrackProgress(
   int whichTrack, double frac, const TranslatableString &msg) const
{
   auto updateResult = (pProgress
      ? pProgress->Poll((whichTrack + frac) * 1000,
         (double) numTracks * 1000, msg)
      : BasicUI::ProgressResult::Success);
   return (updateResult != BasicUI::ProgressResult::Success);
}

bool EffectContext::TrackGroupProgress(
   int whichGroup, double frac, const TranslatableString &msg) const
{
   auto updateResult = (pProgress
      ? pProgress->Poll((whichGroup + frac) * 1000,
         (double) numGroups * 1000, msg)
      : BasicUI::ProgressResult::Success);
   return (updateResult != BasicUI::ProgressResult::Success);
}

EffectPlugin::~EffectPlugin() = default;

const wxString EffectPlugin::kUserPresetIdent = wxT("User Preset:");
const wxString EffectPlugin::kFactoryPresetIdent = wxT("Factory Preset:");
const wxString EffectPlugin::kCurrentSettingsIdent = wxT("<Current Settings>");
const wxString EffectPlugin::kFactoryDefaultsIdent = wxT("<Factory Defaults>");

EffectInstanceEx::~EffectInstanceEx() = default;

bool EffectInstanceEx::Init()
{
   return true;
}
