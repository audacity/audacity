/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "../ieffectsprovider.h"
#include "../ieffectinstancesregister.h"

namespace au::effects {
class TempConceptExecutor
{
    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<IEffectsProvider> effectsProvider;
    muse::Inject<IEffectInstancesRegister> effectInstancesRegister;

public:
    TempConceptExecutor() = default;

    void execute(const std::string& effectId);
};
}
