/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file EffectHostInterface.cpp

  Paul Licameli split from EffectInterface.cpp

**********************************************************************/
#include "EffectHostInterface.h"

EffectHostInterface::~EffectHostInterface() = default;

EffectUIHostInterface::~EffectUIHostInterface() = default;

const wxString EffectUIHostInterface::kUserPresetIdent = wxT("User Preset:");
const wxString EffectUIHostInterface::kFactoryPresetIdent = wxT("Factory Preset:");
const wxString EffectUIHostInterface::kCurrentSettingsIdent = wxT("<Current Settings>");
const wxString EffectUIHostInterface::kFactoryDefaultsIdent = wxT("<Factory Defaults>");
