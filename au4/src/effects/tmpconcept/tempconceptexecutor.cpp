/*
* Audacity: A Digital Audio Editor
*/
#include "tempconceptexecutor.h"

#include "libraries/lib-project/Project.h"
#include "libraries/lib-audacity-application-logic/AudacityApplicationLogic.h"
#include "libraries/lib-effects/Effect.h"
#include "libraries/lib-module-manager/PluginManager.h"
#include "libraries/lib-command-parameters/ShuttleAutomation.h"

#include "au3wrap/internal/wxtypes_convert.h"

using namespace au::effects;

void TempConceptExecutor::execute(const std::string& effectId_)
{
    PluginID effectId = effectId_;

    auto& project = *reinterpret_cast<::AudacityProject*>(globalContext()->currentProject()->au3ProjectPtr());

    auto showEffectHostInterfaceCb = [this](Effect& effect,
                                            std::shared_ptr<EffectInstance>& instance,
                                            SimpleEffectSettingsAccess& settings)
    {
        const std::optional<EffectPlugin::InstancePointer> result = EffectBase::FindInstance(effect);
        if (!result.has_value()) {
            return false;
        }
        instance = *result;
        if (!instance) {
            return false;
        }

        muse::String type = au3::wxToSting(effect.GetSymbol().Internal());
        //! NOTE The goal is that we need to pass the instance ID to the view model
        //! and get a pointer to the effect instance there.
        //! For built-in effects, we can register and unregister the instance
        //! in the constructor and destructor.
        //! But now I'm not sure we can do this for all effects.
        //! Therefore, we register here and immediately unregister here.
        //! This is a hack...
        //! But it looks like later everything will be different, at some point we will remove it

        EffectInstanceId instanceId = effectInstancesRegister()->regInstance(&effect);
        muse::Ret ret = effectsProvider()->showEffect(type, instanceId);
        effectInstancesRegister()->unregInstance(&effect);
        if (!ret) {
            LOGE() << "failed show effect: " << type << ", ret: " << ret.toString();
            return false;
        }

        effect.SaveUserPreset(CurrentSettingsGroup(), settings.Get());
        return true;
    };

    auto stopPlaybackCb = [] {};
    auto selectAllIfNoneCb = [] {};

    AudacityApplicationLogic::DoEffect(
        effectId, project, 0,
        std::move(showEffectHostInterfaceCb), std::move(stopPlaybackCb),
        std::move(selectAllIfNoneCb));

    // const auto pluginRange = PluginManager::Get().EffectsOfType(EffectTypeProcess);
    // for (auto it = pluginRange.begin(); it != pluginRange.end(); ++it) {
    //     const auto id = (*it).GetID();
    //     AudacityApplicationLogic::DoEffect(
    //         id, project, 0,
    //         std::move(showEffectHostInterfaceCb), std::move(stopPlaybackCb),
    //         std::move(selectAllIfNoneCb));
    // }
}
