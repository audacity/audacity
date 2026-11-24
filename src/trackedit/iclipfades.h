/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <optional>

#include "modularity/imoduleinterface.h"

#include "trackedittypes.h"
#include "dom/clip.h"

namespace au::trackedit {
class IClipFades : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IClipFades)

public:
    virtual ~IClipFades() = default;

    //! Starts a drag of the fade on `side` of `leadClip`, which those of the other `clips` follow, after canceling any
    //! ongoing drag. False if it couldn't start.
    virtual bool startFadeDrag(const ClipKeyList& clips, const ClipKey& leadClip, ClipSide side) = 0;

    //! The lead clip's fade becomes `duration` long.
    virtual bool changeFade(secs_t duration) = 0;

    //! Ends the drag, keeping what it changed, as one undo step. False if no fade changed, and then there is no undo step.
    virtual bool commitFadeDrag() = 0;

    //! Ends the drag, if any.
    virtual void cancelFadeDrag() = 0;

    struct DraggedFade {
        ClipKey leadClip;
        ClipSide side;
    };
    virtual std::optional<DraggedFade> draggedFade() const = 0;
};
}
