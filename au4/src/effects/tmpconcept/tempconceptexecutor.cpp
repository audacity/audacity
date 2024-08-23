/*
* Audacity: A Digital Audio Editor
*/
#include "tempconceptexecutor.h"

#include "libraries/lib-project/Project.h"
#include "libraries/lib-audacity-application-logic/AudacityApplicationLogic.h"
#include "libraries/lib-effects/Effect.h"
#include "libraries/lib-module-manager/PluginManager.h"
#include "libraries/lib-command-parameters/ShuttleAutomation.h"

#include "ieffectdialog.h"

using namespace au::effects;

void TempConceptExecutor::execute(const std::string& effectId_)
{
    PluginID effectId = effectId_;

    auto& project = *reinterpret_cast<::AudacityProject*>(globalContext()->currentProject()->au3ProjectPtr());

    auto showEffectHostInterfaceCb = [](Effect& effect,
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
        // Here show the interface and adjust the parameters
        // Not sure how use `settings`, but in some (all?) cases,
        // the file settings are automagically bound to the instance via the
        // definition of `EffectParameter`s. (E.g. see AmplifyBase).
        // The `au::au3::IEffectDialog` implementation can therefore proceed
        // with no further arguments.
        IEffectDialog* dialog = dynamic_cast<IEffectDialog*>(&effect);
        if (!dialog) {
            return false;
        }
        if (!dialog->show()) {
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
