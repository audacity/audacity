#include "thumbnailcreator.h"

using namespace au::project;

void ThumbnailCreator::onThumbnailCreated(bool success)
{
    m_thumbnailCreated.send(success);
}

muse::async::Notification ThumbnailCreator::captureThumbnailRequested() const
{
    return m_createThumbnailRequested;
}

muse::Ret ThumbnailCreator::createThumbnail()
{
    muse::Ret ret;
    QEventLoop loop;
    m_thumbnailCreated.onReceive(this, [&loop, &ret](bool ok) {
        ret = ok ? muse::make_ok() :  muse::make_ret(muse::Ret::Code::UnknownError);
        loop.quit();
    });

    m_createThumbnailRequested.notify();
    loop.exec(); 

    return ret;
}