/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "async/notification.h"

#include "modularity/imoduleinterface.h"

#include "effectstypes.h"

namespace au::effects {
class IEffectsProvider : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IEffectsProvider)

public:
    virtual ~IEffectsProvider() = default;

    virtual void reloadEffects() = 0;

    virtual ManifestList manifestList() const = 0;
    virtual muse::async::Notification manifestListChanged() const = 0;

    virtual Manifest manifest(const muse::String& id) const = 0;
};
}
