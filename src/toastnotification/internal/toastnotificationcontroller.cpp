/*
* Audacity: A Digital Audio Editor
*/
#include <string>
#include <optional>

#include "framework/global/types/val.h"
#include "framework/actions/actiontypes.h"

#include "toastnotification/toastnotificationitembuilder.h"
#include "toastnotificationcontroller.h"

using namespace muse;
using namespace au::toastnotification;
using namespace muse::async;
using namespace muse::actions;

namespace {
const ActionQuery ADD_ITEM_QUERY("action://toastnotification/add-item");
const ActionQuery DISMISS_ITEM_QUERY("action://toastnotification/dismiss-item");
}

void ToastNotificationController::init()
{
    dispatcher()->reg(this, ADD_ITEM_QUERY, this, &ToastNotificationController::addItem);
    dispatcher()->reg(this, DISMISS_ITEM_QUERY, this, &ToastNotificationController::dismissItem);
}

void ToastNotificationController::addItem(const muse::actions::ActionQuery& q)
{
    if (q.param("title").isNull() || q.param("message").isNull()) {
        return;
    }

    const int iconCode = q.param("iconCode", muse::Val(0)).toInt();
    const std::string title = q.param("title").toString();
    const std::string message = q.param("message").toString();
    const bool dismissable = q.param("dismissable", muse::Val(true)).toBool();
    const std::optional<int> autoDismissTimeout
        = q.param("autoDismissTimeout").isNull() ? std::nullopt : std::make_optional(q.param("autoDismissTimeout").toInt());

    globalContext()->currentToastNotification()->addNotification(
        ToastNotificationItemBuilder()
        .withIconCode(iconCode)
        .withTitle(title)
        .withMessage(message)
        .withDismissable(dismissable)
        .withAutoDismissTimeout(autoDismissTimeout.has_value() ? autoDismissTimeout.value() : 0)
        .build()
        );
}

void ToastNotificationController::dismissItem(const muse::actions::ActionQuery& q)
{
    IF_ASSERT_FAILED(q.params().size() == 1) {
        return;
    }

    const int id = q.param("id").toInt();

    globalContext()->currentToastNotification()->dismissNotification(id);
}
