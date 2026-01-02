/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/actions/actiontypes.h"
#include "framework/ui/uiaction.h"
#include "framework/global/async/channel.h"
#include "framework/global/async/asyncable.h"
#include "framework/ui/iuiactionsmodule.h"

namespace au::toastnotification {
class ToastNotificationUiActions : public muse::ui::IUiActionsModule, public muse::async::Asyncable
{
public:
    ToastNotificationUiActions() = default;

    const muse::ui::UiActionList& actionsList() const override;

    bool actionEnabled(const muse::ui::UiAction& act) const override;
    muse::async::Channel<muse::actions::ActionCodeList> actionEnabledChanged() const override;

    bool actionChecked(const muse::ui::UiAction& act) const override;
    muse::async::Channel<muse::actions::ActionCodeList> actionCheckedChanged() const override;

private:
    static const muse::ui::UiActionList m_mainActions;

    muse::async::Channel<muse::actions::ActionCodeList> m_actionEnabledChanged;
    muse::async::Channel<muse::actions::ActionCodeList> m_actionCheckedChanged;
};
}
