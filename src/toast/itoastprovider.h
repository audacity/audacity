/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/modularity/imoduleinterface.h"
#include "framework/global/async/channel.h"
#include "framework/global/async/promise.h"

#include "internal/toastitem.h"
#include "toast/toasttypes.h"

namespace au::toast {
class IToastProvider : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IToastProvider)
public:
    virtual ~IToastProvider() = default;

    virtual muse::async::Promise<ToastActionCode> show(ToastItem item) = 0;

    virtual muse::async::Channel<std::shared_ptr<ToastItem> > toastAdded() const = 0;
    virtual muse::async::Channel<int> toastDismissed() const = 0;

    virtual void dismissToast(int id) = 0;
    virtual void executeAction(int id, ToastActionCode actionCode) = 0;
};
}
