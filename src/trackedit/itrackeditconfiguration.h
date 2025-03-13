#pragma once

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
};
}
