#include "thumbnailcreator.h"

using namespace au::project;

void ThumbnailCreator::onThumbnailCreated(bool success)
{
    m_thumbnailCreated.send(success);
}

muse::async::Channel<std::string> ThumbnailCreator::captureThumbnailRequested() const
{
    return m_createThumbnailRequested;
}

muse::Ret ThumbnailCreator::createThumbnail(const std::string& path)
{
    muse::Ret ret;
    QEventLoop loop;
    m_thumbnailCreated.resetOnReceive(this);
    m_thumbnailCreated.onReceive(this, [&loop, &ret](bool ok) {
        ret = ok ? muse::make_ok() :  muse::make_ret(muse::Ret::Code::UnknownError);
        loop.quit();
    });

    m_createThumbnailRequested.send(path);
    loop.exec(); 

    return ret;
}