/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/global/modularity/ioc.h"
#include "framework/ui/iuiactionsmodule.h"

namespace au::aimusic {
class AiMusicController;

class AiMusicUiActions : public muse::ui::IUiActionsModule, public muse::Contextable
{
public:
    AiMusicUiActions(const muse::modularity::ContextPtr& ctx, std::shared_ptr<AiMusicController> controller);

    const muse::ui::UiActionList& actionsList() const override;
    bool actionEnabled(const muse::ui::UiAction& action) const override;
    muse::async::Channel<muse::actions::ActionCodeList> actionEnabledChanged() const override;
    bool actionChecked(const muse::ui::UiAction& action) const override;
    muse::async::Channel<muse::actions::ActionCodeList> actionCheckedChanged() const override;

private:
    std::shared_ptr<AiMusicController> m_controller;
};
}
