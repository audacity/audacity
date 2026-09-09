/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/global/async/notification.h"
#include "framework/global/modularity/imoduleinterface.h"
#include "trackedit/trackedittypes.h"
#include <optional>

namespace au::projectscene {
class IRealtimeEffectPanelTrackSelection : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IRealtimeEffectPanelTrackSelection);

public:
    virtual ~IRealtimeEffectPanelTrackSelection() = default;

    virtual std::optional<au::trackedit::TrackId> selectedTrackId() const = 0;
    virtual muse::async::Notification selectedTrackIdChanged() const = 0;
};
}
