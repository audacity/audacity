/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <QObject>

#include "framework/interactive/iinteractive.h"
#include "framework/global/modularity/ioc.h"
#include "framework/global/async/asyncable.h"
#include "framework/rcommand/commandable.h"
#include "framework/rcommand/icommanddispatcher.h"
#include "framework/shortcuts_v2/icommandshortcutsregister.h"

#include "playback/iplaybackcontroller.h"
#include "../ieffectexecutionscenario.h"
#include "../ieffectsprovider.h"
#include "../ieffectpresetsscenario.h"
#include "../ieffectsconfiguration.h"
#include "../ieffectinstancesregister.h"

namespace au::effects {
class EffectsActionsController : public muse::rcommand::Commandable, public muse::async::Asyncable, public muse::Contextable
{
    muse::GlobalInject<IEffectsConfiguration> configuration;
    muse::GlobalInject<IEffectsProvider> effectsProvider;
    muse::GlobalInject<IEffectInstancesRegister> instancesRegister;

    muse::ContextInject<muse::rcommand::ICommandDispatcher> commandDispatcher{ this };
    muse::GlobalInject<muse::shortcuts::ICommandShortcutsRegister> commandShortcutsRegister;
    muse::ContextInject<IEffectExecutionScenario> effectExecutionScenario{ this };
    muse::ContextInject<IEffectPresetsScenario> presetsScenario{ this };
    muse::ContextInject<muse::IInteractive> interactive{ this };
    muse::ContextInject<au::playback::IPlaybackController> playbackController{ this };

public:
    EffectsActionsController(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void init();

private:
    void registerActions();

    muse::Ret openEffect(const EffectId& effectId);
    muse::Ret applyEffect(const EffectId& effectId, const muse::rcommand::Params& params);
    muse::rcommand::Response configureEffect(const muse::rcommand::Request& request, const EffectId& effectId);
    muse::Ret repeatLastEffect();

    muse::Ret applyPreset(const muse::rcommand::Params& params);
    muse::Ret savePreset(const muse::rcommand::Params& params);
    muse::Ret savePresetAs(const muse::rcommand::Params& params);
    muse::Ret deletePreset(const muse::rcommand::Params& params);
    muse::Ret importPreset(const muse::rcommand::Params& params);
    muse::Ret exportPreset(const muse::rcommand::Params& params);
    muse::Ret toggleVendorUI(const muse::rcommand::Params& params);
    muse::Ret openPluginManager();

    QObject m_queuedCallContext;
};
}
