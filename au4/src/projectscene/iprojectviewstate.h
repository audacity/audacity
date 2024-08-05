#pragma once

#include <memory>

#include "global/types/retval.h"

#include "types/projectscenetypes.h"
#include "trackedit/trackedittypes.h"

namespace au::projectscene {
class IProjectViewState
{
public:
    virtual ~IProjectViewState() = default;

    // context of all tracks
    virtual muse::ValCh<int> tracksVericalY() const = 0;
    virtual void changeTracksVericalY(int deltaY) = 0;

    // context of track
    virtual muse::ValCh<int> trackHeight(const trackedit::TrackId& trackId) const = 0;
    virtual muse::ValCh<bool> isTrackCollapsed(const trackedit::TrackId& trackId) const = 0;
    virtual void changeTrackHeight(const trackedit::TrackId& trackId, int deltaY) = 0;

    // snap
    virtual muse::ValCh<bool> isSnapEnabled() const = 0;
    virtual void setIsSnapEnabled(bool enabled) = 0;

    virtual muse::ValCh<SnapType> snapType() const = 0;
    virtual void setSnapType(SnapType type) = 0;

    virtual muse::ValCh<bool> isSnapTripletsEnabled() const = 0;
    virtual void setIsSnapTripletsEnabled(bool enabled) = 0;
};

using IProjectViewStatePtr = std::shared_ptr<IProjectViewState>;
}
