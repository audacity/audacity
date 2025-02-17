/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AudacityApplicationLogic.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "CommandFlag.h"
#include "AudacityApplicationLogicTypes.h"
#include "PluginProvider.h"

class AudacityProject;

namespace AudacityApplicationLogic {
/** Run an effect given the plugin ID */
// Returns true on success.  Will only operate on tracks that
// have the "selected" flag set to true, which is consistent with
// Audacity's standard UI.
AUDACITY_APPLICATION_LOGIC_API bool DoEffect(
    const PluginID& ID, AudacityProject & project, unsigned flags,
    ShowEffectHostInterfaceCb, StopPlaybackCb, SelectAllIfNoneCb);
} // namespace AudacityApplicationLogic
