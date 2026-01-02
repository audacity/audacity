/*
* Audacity: A Digital Audio Editor
*/

#include "context/uicontext.h"
#include "context/shortcutcontext.h"

#include "toastnotificationuiactions.h"

using namespace au::toastnotification;
using namespace muse;
using namespace muse::ui;
using namespace muse::actions;

namespace {
const ActionQuery ADD_ITEM_QUERY("action://toastnotification/add-item");
const ActionQuery DISMISS_ITEM_QUERY("action://toastnotification/dismiss-item");
}

const UiActionList ToastNotificationUiActions::m_mainActions = {
    UiAction(ADD_ITEM_QUERY.toString(),
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Add notification"),
             TranslatableString("action", "Add a new toast notification")
             ),
    UiAction(DISMISS_ITEM_QUERY.toString(),
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Dismiss notification"),
             TranslatableString("action", "Dismiss the tosat notification")
             ),
};

const UiActionList& ToastNotificationUiActions::actionsList() const
{
    static UiActionList alist;
    if (alist.empty()) {
        alist.insert(alist.end(), m_mainActions.cbegin(), m_mainActions.cend());
    }
    return alist;
}

bool ToastNotificationUiActions::actionEnabled(const UiAction&) const
{
    return true;
}

bool ToastNotificationUiActions::actionChecked(const UiAction&) const
{
    return false;
}

muse::async::Channel<ActionCodeList> ToastNotificationUiActions::actionEnabledChanged() const
{
    return m_actionEnabledChanged;
}

muse::async::Channel<ActionCodeList> ToastNotificationUiActions::actionCheckedChanged() const
{
    return m_actionCheckedChanged;
}
