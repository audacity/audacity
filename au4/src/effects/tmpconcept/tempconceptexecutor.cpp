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

void TempConceptExecutor::execute(const EffectId& effectId_)
{
    PluginID effectId = effectId_.toStdString();

    auto& project = *reinterpret_cast<::AudacityProject*>(globalContext()->currentProject()->au3ProjectPtr());

    auto showEffectHostInterfaceCb = [this, effectId_](Effect& effect,
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

        muse::String type = au3::wxToString(effect.GetSymbol().Internal());
        muse::Ret ret = effectsProvider()->showEffect(type, effectId_);
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
