#include "thumbnailcreator.h"

#include <QEventLoop>

using namespace au::project;

void ThumbnailCreator::onThumbnailCreated(std::vector<uint8_t> pngData)
{
    m_thumbnailCreated.send(pngData);
}

muse::async::Notification ThumbnailCreator::captureThumbnailRequested() const
{
    return m_createThumbnailRequested;
}

std::optional<std::vector<uint8_t> > ThumbnailCreator::createThumbnail()
{
    std::optional<std::vector<uint8_t> > result;
    QEventLoop loop;
    m_thumbnailCreated.onReceive(this, [&loop, &result](std::vector<uint8_t> data) {
        result = std::move(data);
        loop.quit();
    });

    m_createThumbnailRequested.notify();
    loop.exec();
    m_thumbnailCreated.disconnect(this);

    return result;
}
