#pragma once

#include "trackedittypes.h"

#include "modularity/imoduleinterface.h"

#include "async/channel.h"
#include "async/notification.h"

namespace au::trackedit {
class ITrackeditConfiguration : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ITrackeditConfiguration)

public:

    virtual ~ITrackeditConfiguration() = default;

    virtual bool pasteAsNewClip() const = 0;
    virtual void setPasteAsNewClip(bool value) = 0;
    virtual muse::async::Notification pasteAsNewClipChanged() const = 0;

    virtual bool askBeforeConvertingToMonoOrStereo() const = 0;
    virtual void setAskBeforeConvertingToMonoOrStereo(bool value) = 0;
    virtual muse::async::Notification askBeforeConvertingToMonoOrStereoChanged() const = 0;

    virtual DeleteBehavior deleteBehavior() const = 0;
    virtual void setDeleteBehavior(DeleteBehavior value) = 0;
    virtual muse::async::Notification deleteBehaviorChanged() const = 0;

    virtual CloseGapBehavior closeGapBehavior() const = 0;
    virtual void setCloseGapBehavior(CloseGapBehavior value) = 0;
    virtual muse::async::Notification closeGapBehaviorChanged() const = 0;

    virtual PasteBehavior pasteBehavior() const = 0;
    virtual void setPasteBehavior(PasteBehavior value) = 0;
    virtual muse::async::Notification pasteBehaviorChanged() const = 0;

    virtual PasteInsertBehavior pasteInsertBehavior() const = 0;
    virtual void setPasteInsertBehavior(PasteInsertBehavior value) = 0;
    virtual muse::async::Notification pasteInsertBehaviorChanged() const = 0;

};
}
