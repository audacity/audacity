/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "ui/iuiactionsmodule.h"
#include "context/iuicontextresolver.h"

namespace au::effects {
class EffectsUiActions : public muse::ui::IUiActionsModule, public muse::async::Asyncable
{
    muse::Inject<context::IUiContextResolver> uicontextResolver;

public:
    const muse::ui::UiActionList& actionsList() const override;

    bool actionEnabled(const muse::ui::UiAction& act) const override;
    muse::async::Channel<muse::actions::ActionCodeList> actionEnabledChanged() const override;

    bool actionChecked(const muse::ui::UiAction& act) const override;
    muse::async::Channel<muse::actions::ActionCodeList> actionCheckedChanged() const override;

private:
    static const muse::ui::UiActionList m_actions;
};
}
