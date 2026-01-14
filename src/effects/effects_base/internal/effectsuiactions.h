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
class EffectsUiActions : public muse::ui::IUiActionsModule, public muse::async::Asyncable, public muse::Injectable
{
    muse::GlobalInject<IEffectsConfiguration> configuration;

    muse::Inject<context::IUiContextResolver> uicontextResolver{ this };
    muse::Inject<IEffectsProvider> effectsProvider{ this };
    muse::Inject<IEffectExecutionScenario> effectExecutionScenario{ this };

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
    void makeActions(EffectMetaList effects);
    muse::ui::UiActionList m_actions;
    const std::shared_ptr<EffectsActionsController> m_controller;
    muse::async::Channel<muse::ui::UiActionList> m_actionsChanged;
    muse::async::Channel<muse::actions::ActionCodeList> m_actionCheckedChanged;
};
}
