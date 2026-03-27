/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/ui/iuiactionsmodule.h"
#include "framework/global/modularity/ioc.h"
#include "framework/global/async/asyncable.h"

namespace au::spectrogram {
class SpectrogramUiActions : public muse::ui::IUiActionsModule, public muse::async::Asyncable, public muse::Contextable
{
public:
    SpectrogramUiActions(const muse::modularity::ContextPtr& ctx);

    const muse::ui::UiActionList& actionsList() const override;

    bool actionEnabled(const muse::ui::UiAction& act) const override;
    muse::async::Channel<muse::actions::ActionCodeList> actionEnabledChanged() const override;

    bool actionChecked(const muse::ui::UiAction& act) const override;
    muse::async::Channel<muse::actions::ActionCodeList> actionCheckedChanged() const override;

private:
    muse::ui::UiActionList m_actions;
    muse::async::Channel<muse::actions::ActionCodeList> m_actionEnabledChanged;
    muse::async::Channel<muse::actions::ActionCodeList> m_actionCheckedChanged;
};
}
