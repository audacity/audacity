/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <optional>

#include "modularity/imoduleinterface.h"
#include "async/channel.h"

#include "trackedittypes.h"

namespace au::trackedit {
class ITracksViewRequestsService : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ITracksViewRequestsService)

public:
    virtual ~ITracksViewRequestsService() = default;

    virtual void requestItemMove(secs_t timeOffset, int trackOffset) = 0;
    virtual muse::async::Channel<secs_t, int> itemMoveRequested() const = 0;

    //! NOTE Set when a label or a clip should enter title edit mode; the view consumes
    //! the request once the item exists, whenever that happens
    virtual void requestLabelTitleEdit(const LabelKey& labelKey) = 0;
    virtual std::optional<LabelKey> pendingLabelTitleEdit() const = 0;
    virtual void labelTitleEditRequestHandled(const LabelKey& labelKey) = 0;
    virtual muse::async::Channel<LabelKey> labelTitleEditRequested() const = 0;

    virtual void requestClipTitleEdit(const ClipKey& clipKey) = 0;
    virtual std::optional<ClipKey> pendingClipTitleEdit() const = 0;
    virtual void clipTitleEditRequestHandled(const ClipKey& clipKey) = 0;
    virtual muse::async::Channel<ClipKey> clipTitleEditRequested() const = 0;
};
}
