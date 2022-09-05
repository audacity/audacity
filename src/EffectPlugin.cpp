/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file EffectPlugin.cpp

  Paul Licameli split from EffectInterface.cpp

**********************************************************************/
#include "EffectPlugin.h"

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

EffectUIValidator::EffectUIValidator(
   EffectUIClientInterface &effect, EffectSettingsAccess &access)
   : mEffect{effect}
   , mAccess{access}
{}

EffectUIValidator::~EffectUIValidator() = default;

bool EffectUIValidator::UpdateUI()
{
   return true;
}

bool EffectUIValidator::IsGraphicalUI()
{
   return false;
}

void EffectUIValidator::Disconnect()
{
}

void EffectUIValidator::OnClose()
{
   if (!mUIClosed)
   {
      mEffect.CloseUI();
      mUIClosed = true;
   }
}

EffectUIClientInterface::~EffectUIClientInterface() = default;
