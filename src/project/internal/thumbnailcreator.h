#pragma once

#include "ithumbnailcreator.h"

#include "global/async/asyncable.h"
#include "global/async/channel.h"

namespace au::project {
class ThumbnailCreator : public IThumbnailCreator, public muse::async::Asyncable
{
public:
    ThumbnailCreator() = default;
    void onThumbnailCreated(bool success) override;
    muse::Ret createThumbnail(const muse::io::path_t& path) override;
    muse::async::Channel<muse::io::path_t> captureThumbnailRequested() const override;

private:
    muse::io::path_t thumbnailPath(const muse::io::path_t& path) const;

    muse::async::Channel<muse::io::path_t> m_createThumbnailRequested;
    muse::async::Channel<bool> m_thumbnailCreated;
};
}
