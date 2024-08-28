/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../ieffectexecutionscenario.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "../ieffectsprovider.h"
#include "../ieffectinstancesregister.h"

class AudacityProject;
class Effect;
class EffectBase;
class EffectInstance;
class SimpleEffectSettingsAccess;
namespace au::effects {
class EffectExecutionScenario : public IEffectExecutionScenarion
{
    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<IEffectsProvider> effectsProvider;
    muse::Inject<IEffectInstancesRegister> effectInstancesRegister;

public:
    EffectExecutionScenario() = default;

    muse::Ret performEffect(const EffectId& effectId) override;

private:

    muse::Ret doPerformEffect(AudacityProject& project, const EffectId& effectId, unsigned int flags);

    using ShowEffectHostInterfaceCb = std::function<bool (Effect&, std::shared_ptr<EffectInstance>&, SimpleEffectSettingsAccess&)>;
    using StopPlaybackCb = std::function<void ()>;
    using SelectAllIfNoneCb = std::function<void ()>;

    bool DoEffect(const EffectId& effectId, AudacityProject& project, unsigned flags);
};
}
