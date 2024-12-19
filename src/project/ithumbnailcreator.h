#pragma once

#include "modularity/imoduleinterface.h"
#include "global/async/channel.h"
#include "global/types/ret.h"

namespace au::project {

class IThumbnailCreator : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(au::project::ithumbnailcreator)

public:
    virtual ~IThumbnailCreator() = default;
    virtual void onThumbnailCreated(bool success) = 0;
    virtual muse::Ret createThumbnail(const std::string& path) = 0;
    virtual muse::async::Channel<std::string> captureThumbnailRequested() const = 0;
};

}
