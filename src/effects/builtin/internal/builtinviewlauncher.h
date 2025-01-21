/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "effects/effects_base/ieffectviewlauncher.h"

#include "modularity/ioc.h"
#include "global/iinteractive.h"

namespace au::effects {
class BuiltinViewLauncher : public IEffectViewLauncher
{
    muse::Inject<muse::IInteractive> interactive;

public:
    BuiltinViewLauncher() = default;

    muse::Ret showEffect(const EffectId& effectId, const EffectInstanceId& instanceId) override;
};
}
