#pragma once

#include <memory>

#include "global/types/retval.h"

#include "processing/processingtypes.h"

namespace au::projectscene {
class IProjectViewState
{
public:
    virtual ~IProjectViewState() = default;

    // context of all tracks
    virtual muse::ValCh<int> tracksVericalY() const = 0;
    virtual void changeTracksVericalY(int deltaY) = 0;

    // context of track
    virtual muse::ValCh<int> trackHeight(const processing::TrackId& trackId) const = 0;
    virtual muse::ValCh<bool> isTrackCollapsed(const processing::TrackId& trackId) const = 0;
    virtual void changeTrackHeight(const processing::TrackId& trackId, int deltaY) = 0;
};

using IProjectViewStatePtr = std::shared_ptr<IProjectViewState>;
}
