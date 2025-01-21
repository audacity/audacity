/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>

#include "global/types/ret.h"
#include "effectstypes.h"

namespace au::effects {
class IEffectViewLauncher
{
public:
    virtual ~IEffectViewLauncher() = default;

    virtual muse::Ret showEffect(const EffectId& effectId, const EffectInstanceId& instanceId) = 0;
};

using IEffectViewLauncherPtr = std::shared_ptr<IEffectViewLauncher>;
}
