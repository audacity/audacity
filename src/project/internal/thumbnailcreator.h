#pragma once

#include "ithumbnailcreator.h"

#include "global/async/asyncable.h"

namespace au::project {

class ThumbnailCreator : public IThumbnailCreator, public muse::async::Asyncable
{
public:
    ThumbnailCreator() = default;
    void onThumbnailCreated(bool success) override;
    muse::async::Promise<muse::Ret> createThumbnail() override;
    muse::async::Notification captureThumbnailRequested() const override;

private:
    muse::async::Notification m_createThumbnailRequested;
    muse::async::Channel<bool> m_thumbnailCreated;
};

}