/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "ui/iuiactionsmodule.h"
#include "context/iuicontextresolver.h"
#include "effectsactionscontroller.h"
#include "ieffectsprovider.h"

namespace au::effects {
class EffectsUiActions : public muse::ui::IUiActionsModule, public muse::async::Asyncable, public muse::Contextable
{
    muse::GlobalInject<IEffectsConfiguration> configuration;
    muse::GlobalInject<IEffectsProvider> effectsProvider;

    muse::ContextInject<context::IUiContextResolver> uicontextResolver{ this };
    muse::ContextInject<IEffectExecutionScenario> effectExecutionScenario{ this };

public:
    EffectsUiActions(const muse::modularity::ContextPtr& ctx, EffectsActionsController* controller);

    void reload();

    const muse::ui::UiActionList& actionsList() const override;

    bool actionEnabled(const muse::ui::UiAction& act) const override;
    muse::async::Channel<muse::actions::ActionCodeList> actionEnabledChanged() const override;

    bool actionChecked(const muse::ui::UiAction& act) const override;
    muse::async::Channel<muse::actions::ActionCodeList> actionCheckedChanged() const override;

    muse::async::Channel<muse::ui::UiActionList> actionsChanged() const override;

private:
    void makeActions(EffectMetaList effects);
    muse::ui::UiActionList m_actions;
    EffectsActionsController* const m_controller = nullptr;
    muse::async::Channel<muse::ui::UiActionList> m_actionsChanged;
    muse::async::Channel<muse::actions::ActionCodeList> m_actionCheckedChanged;
};
}
