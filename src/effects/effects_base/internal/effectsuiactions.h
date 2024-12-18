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
class EffectsUiActions : public muse::ui::IUiActionsModule, public muse::async::Asyncable
{
    muse::Inject<context::IUiContextResolver> uicontextResolver;
    muse::Inject<IEffectsProvider> effectsProvider;
    muse::Inject<IEffectExecutionScenario> effectExecutionScenario;

public:
    EffectsUiActions(std::shared_ptr<EffectsActionsController> controller);

    void reload();

    const muse::ui::UiActionList& actionsList() const override;

    bool actionEnabled(const muse::ui::UiAction& act) const override;
    muse::async::Channel<muse::actions::ActionCodeList> actionEnabledChanged() const override;

    bool actionChecked(const muse::ui::UiAction& act) const override;
    muse::async::Channel<muse::actions::ActionCodeList> actionCheckedChanged() const override;

    muse::async::Channel<muse::ui::UiActionList> actionsChanged() const override;

private:
    muse::ui::UiActionList m_actions;
    const std::shared_ptr<EffectsActionsController> m_controller;
    muse::async::Channel<muse::ui::UiActionList> m_actionsChanged;
};
}
