#pragma once

#include "modularity/imoduleinterface.h"
#include "global/async/channel.h"
#include "global/types/ret.h"
#include "global/io/path.h"

namespace au::project {
class IThumbnailCreator : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(au::project::ithumbnailcreator)

public:
    virtual ~IThumbnailCreator() = default;
    virtual muse::Ret createThumbnail(const muse::io::path_t& path) = 0;

    //for internal use
    virtual muse::async::Channel<muse::io::path_t> captureThumbnailRequested() const = 0;
    virtual void onThumbnailCreated(bool success) = 0;
};
}
