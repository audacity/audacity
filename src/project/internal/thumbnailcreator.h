#pragma once

#include <vector>
#include <optional>
#include <cstdint>

#include "framework/global/async/asyncable.h"
#include "framework/global/async/channel.h"
#include "framework/global/async/notification.h"

#include "ithumbnailcreator.h"

namespace au::project {
class ThumbnailCreator final : public IThumbnailCreator, public muse::async::Asyncable
{
public:
    ThumbnailCreator() = default;

    void onThumbnailCreated(std::vector<uint8_t> pngData) override;
    std::optional<std::vector<uint8_t> > createThumbnail() override;
    muse::async::Notification captureThumbnailRequested() const override;

private:
    muse::async::Notification m_createThumbnailRequested;
    muse::async::Channel<std::vector<uint8_t> > m_thumbnailCreated;
};
}
