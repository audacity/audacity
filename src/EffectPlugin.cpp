/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file EffectPlugin.cpp

  Paul Licameli split from EffectInterface.cpp

**********************************************************************/
#include "EffectPlugin.h"
#include <wx/window.h>

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
