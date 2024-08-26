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
        //! NOTE This implementation is temporary
        //! Probably we need to register instances with their IDs somewhere
        //! and get an instance from this register in a view model.
        muse::String instanceId = muse::String::number(reinterpret_cast<size_t>(&effect));
        muse::Ret ret = effectsProvider()->showEffect(type, instanceId);
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
