/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../ieffectexecutionscenario.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "../ieffectsprovider.h"
#include "../ieffectinstancesregister.h"

#include "libraries/lib-effects/EffectPlugin.h"

class AudacityProject;
class Effect;
class EffectBase;
class EffectInstance;
class SimpleEffectSettingsAccess;
namespace au::effects {
class EffectExecutionScenarion : public IEffectExecutionScenarion
{
    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<IEffectsProvider> effectsProvider;
    muse::Inject<IEffectInstancesRegister> effectInstancesRegister;

public:
    EffectExecutionScenarion() = default;

    muse::Ret performEffect(const EffectId& effectId) override;

private:

    using ShowEffectHostInterfaceCb = std::function<bool (Effect&, std::shared_ptr<EffectInstance>&, SimpleEffectSettingsAccess&)>;
    using StopPlaybackCb = std::function<void ()>;
    using SelectAllIfNoneCb = std::function<void ()>;

    bool DoEffect(const EffectId& effectId, AudacityProject& project, unsigned flags, ShowEffectHostInterfaceCb showEffectHostInterfaceCb,
                  StopPlaybackCb stopPlaybackCb, SelectAllIfNoneCb selectAllIfNoneCb);

    bool EffectBaseDoEffect(EffectBase* effect, EffectSettings& settings, const EffectPlugin::InstanceFinder& finder, double projectRate,
                            TrackList* list, WaveTrackFactory* factory, NotifyingSelectedRegion& selectedRegion, unsigned flags,
                            const EffectPlugin::EffectSettingsAccessPtr& pAccess);
};
}
