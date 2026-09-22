/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "modularity/ioc.h"
#include "ui/iuiactionsmodule.h"

namespace au::aistudio {
class AIStudioController;

class AIStudioUiActions final : public muse::ui::IUiActionsModule, public muse::Contextable
{
public:
    AIStudioUiActions(const muse::modularity::ContextPtr& ctx, std::shared_ptr<AIStudioController> controller);

    const muse::ui::UiActionList& actionsList() const override;
    bool actionEnabled(const muse::ui::UiAction& action) const override;
    muse::async::Channel<muse::actions::ActionCodeList> actionEnabledChanged() const override;
    bool actionChecked(const muse::ui::UiAction&) const override;
    muse::async::Channel<muse::actions::ActionCodeList> actionCheckedChanged() const override;

private:
    std::shared_ptr<AIStudioController> m_controller;
};
}
