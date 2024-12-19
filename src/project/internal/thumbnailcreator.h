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
    muse::Ret createThumbnail(const std::string& path) override;
    muse::async::Channel<std::string> captureThumbnailRequested() const override;

private:
    muse::async::Channel<std::string> m_createThumbnailRequested;
    muse::async::Channel<bool> m_thumbnailCreated;
};

}