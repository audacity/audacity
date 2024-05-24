#pragma once

#include <memory>

#include "global/types/retval.h"

#include "processing/processingtypes.h"

namespace au::projectscene {
class IProjectViewState
{
public:
    virtual ~IProjectViewState() = default;

    virtual muse::ValCh<int> trackHeight(const processing::TrackId& trackId) const = 0;

    virtual void changeTrackHeight(const processing::TrackId& trackId, int deltaY) = 0;
};

using IProjectViewStatePtr = std::shared_ptr<IProjectViewState>;
}
