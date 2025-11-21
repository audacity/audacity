/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AudacityApplicationLogicTypes.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "TranslatableString.h"
#include <functional>
#include <memory>

class Effect;
class EffectInstance;
class SimpleEffectSettingsAccess;

namespace AudacityApplicationLogic {
using ShowEffectHostInterfaceCb = std::function<bool (
                                                    Effect&, std::shared_ptr<EffectInstance>&, SimpleEffectSettingsAccess&)>;

using StopPlaybackCb = std::function<void ()>;

using SelectAllIfNoneCb = std::function<void ()>;
} // namespace AudacityApplicationLogic
