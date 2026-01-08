/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>
#include <vector>

#include "framework/global/modularity/imoduleinterface.h"
#include "framework/global/async/channel.h"

#include "internal/toastitem.h"

namespace au::toast {
class IToastProvider : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IToastProvider)
public:
    virtual ~IToastProvider() = default;

    virtual void show(std::shared_ptr<ToastItem> item) = 0;

    virtual void setMaxItems(int maxItems) = 0;

    virtual muse::async::Channel<std::shared_ptr<ToastItem> > toastAdded() const = 0;
    virtual muse::async::Channel<int> toastDismissed() const = 0;

    virtual void dismissToast(int id) = 0;
};
}
