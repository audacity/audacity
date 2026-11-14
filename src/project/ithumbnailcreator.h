#pragma once

#include <vector>
#include <optional>
#include <cstdint>

#include "framework/global/modularity/imoduleinterface.h"
#include "framework/global/async/notification.h"

namespace au::project {
class IThumbnailCreator : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(au::project::ithumbnailcreator)

public:
    virtual ~IThumbnailCreator() = default;

    virtual std::optional<std::vector<uint8_t> > createThumbnail() = 0;

    virtual muse::async::Notification captureThumbnailRequested() const = 0;
    virtual void onThumbnailCreated(std::vector<uint8_t> pngData) = 0;
};
}
