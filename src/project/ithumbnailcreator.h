#pragma once

#include "modularity/imoduleinterface.h"
#include "global/async/promise.h"
#include "global/async/channel.h"
#include "global/async/notification.h"
#include "global/types/ret.h"

namespace au::project {

class IThumbnailCreator : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(au::project::ithumbnailcreator)
    
public:
    virtual ~IThumbnailCreator() = default;
    virtual void onThumbnailCreated(bool success) = 0;
    virtual muse::async::Promise<muse::Ret> createThumbnail() = 0;
    virtual muse::async::Notification captureThumbnailRequested() const = 0;
};

}