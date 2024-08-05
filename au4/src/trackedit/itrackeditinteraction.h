#pragma once

#include "modularity/imoduleinterface.h"

#include "global/types/string.h"
#include "global/async/channel.h"

#include "trackedittypes.h"

namespace au::trackedit {
//! NOTE Interface for interacting with the project
//! When it gets big, maybe we’ll divide it into several
//! Currently implemented in the au3wrap module
class ITrackeditInteraction : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ITrackeditInteraction)

public:
    virtual ~ITrackeditInteraction() = default;

    virtual secs_t clipStartTime(const ClipKey& clipKey) const = 0;

    //! NOTE Can be called by moving a clip
    //! if the changes is completed, then it is necessary to pass: `completed = true`
    virtual bool changeClipStartTime(const ClipKey& clipKey, double newStartTime, bool completed) = 0;
    virtual muse::async::Channel<ClipKey, double /*newStartTime*/, bool /*completed*/> clipStartTimeChanged() const = 0;

    virtual bool changeClipTitle(const ClipKey& clipKey, const muse::String& newTitle) = 0;
    virtual bool removeClip(const ClipKey& clipKey) = 0;
    virtual bool removeClipData(const ClipKey& clipKey, double begin, double end) = 0;
    virtual secs_t clipDuration(const ClipKey& clipKey) const = 0;
};
}
