/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "framework/interactive/iinteractive.h"
#include "framework/global/modularity/ioc.h"
#include "framework/global/async/asyncable.h"
#include "framework/global/async/channel.h"
#include "framework/actions/actionable.h"
#include "framework/actions/iactionsdispatcher.h"
#include "framework/rcommand/commandable.h"
#include "framework/rcommand/icommanddispatcher.h"
#include "framework/ui/iuiactionsregister.h"
#include "framework/shortcuts/ishortcutsregister.h"

#include "spectrogram/ispectraleffectsregister.h"
#include "spectrogram/ifrequencyselectioncontroller.h"

#include "playback/iplaybackcontroller.h"
#include "../ieffectexecutionscenario.h"
#include "../ieffectsprovider.h"
#include "../ieffectpresetsscenario.h"
#include "../ieffectsconfiguration.h"
#include "../ieffectinstancesregister.h"

namespace au::effects {
class EffectsUiActions;
class EffectsActionsController : public muse::actions::Actionable, public muse::rcommand::Commandable, public muse::async::Asyncable,
    public muse::Contextable
{
    muse::GlobalInject<IEffectsConfiguration> configuration;
    muse::GlobalInject<spectrogram::ISpectralEffectsRegister> spectralEffectsRegister;
    muse::GlobalInject<IEffectsProvider> effectsProvider;
    muse::GlobalInject<IEffectInstancesRegister> instancesRegister;

    muse::ContextInject<muse::actions::IActionsDispatcher> dispatcher{ this };
    muse::ContextInject<muse::rcommand::ICommandDispatcher> commandDispatcher{ this };
    muse::ContextInject<muse::ui::IUiActionsRegister> uiActionsRegister{ this };
    muse::ContextInject<muse::shortcuts::IShortcutsRegister> shortcutsRegister{ this };
    muse::ContextInject<IEffectExecutionScenario> effectExecutionScenario{ this };
    muse::ContextInject<IEffectPresetsScenario> presetsScenario{ this };
    muse::ContextInject<muse::IInteractive> interactive{ this };
    muse::ContextInject<au::playback::IPlaybackController> playbackController{ this };
    muse::ContextInject<spectrogram::IFrequencySelectionController> frequencySelectionController{ this };

public:
    EffectsActionsController(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void init();
    bool canReceiveAction(const muse::actions::ActionCode&) const override;
    muse::async::Channel<muse::actions::ActionCodeList> canReceiveActionsChanged() const;

private:
    void registerActions();
    void notifyAboutSpectralEffectsAvailability();

    muse::Ret openEffect(const EffectId& effectId);
    muse::Ret applyEffect(const EffectId& effectId, const muse::rcommand::Params& params);
    muse::Ret repeatLastEffect();

    muse::Ret toggleVendorUI(const EffectId& effectId);
    muse::Ret openPluginManager();

    std::shared_ptr<EffectsUiActions> m_uiActions;
    muse::async::Channel<muse::actions::ActionCodeList> m_canReceiveActionsChanged;
};
}
