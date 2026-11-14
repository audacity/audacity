/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DoEffect.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include <wx/string.h>

class AudacityProject;
using PluginID = wxString;

namespace EffectUI {
bool DoEffect(const PluginID& ID, AudacityProject& project, unsigned flags);
}
