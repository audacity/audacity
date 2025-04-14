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

    //State of elements
    virtual muse::ValCh<int> trackHeight(const trackedit::TrackId& trackId) const = 0;
    virtual muse::ValCh<bool> isTrackCollapsed(const trackedit::TrackId& trackId) const = 0;

    virtual bool isSnapEnabled() const = 0;
    virtual void setIsSnapEnabled(bool enabled) = 0;

    virtual SnapType snapType() const = 0;
    virtual void setSnapType(SnapType type) = 0;

    virtual bool isSnapTripletsEnabled() const = 0;
    virtual void setIsSnapTripletsEnabled(bool enabled) = 0;

    virtual void setSnap(const Snap& s) = 0;
    virtual Snap getSnap() const = 0;
    virtual muse::ValCh<Snap> snap() const = 0;

    //State of user interaction
    virtual double mousePositionY() const = 0;
    virtual void setMousePositionY(double y) = 0;

    virtual muse::ValCh<int> tracksVericalY() const = 0;
    virtual void changeTracksVericalY(int deltaY) = 0;
    virtual muse::ValCh<bool> tracksVerticalScrollLocked() const = 0;
    virtual void setTracksVerticalScrollLocked(bool lock) = 0;

    virtual int trackYPosition(const trackedit::TrackId& trackId) const = 0;
    virtual void changeTrackHeight(const trackedit::TrackId& trackId, int deltaY) = 0;

    virtual void setClipEditStartTimeOffset(double val) = 0;
    virtual double clipEditStartTimeOffset() const = 0;

    virtual void setClipEditEndTimeOffset(double val) = 0;
    virtual double clipEditEndTimeOffset() const = 0;

    virtual void setMoveInitiated(bool val) = 0;
    virtual bool moveInitiated() const = 0;

    virtual muse::ValCh<bool> altPressed() const = 0;
    virtual muse::ValCh<bool> ctrlPressed() const = 0;
};

using IProjectViewStatePtr = std::shared_ptr<IProjectViewState>;
}
