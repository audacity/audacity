#include "thumbnailcreator.h"

#include "global/io/fileinfo.h"

using namespace au::project;

constexpr auto FILE_SUFFIX = ".png";

void ThumbnailCreator::onThumbnailCreated(bool success)
{
    m_thumbnailCreated.send(success);
}

muse::async::Channel<muse::io::path_t> ThumbnailCreator::captureThumbnailRequested() const
{
    return m_createThumbnailRequested;
}

muse::Ret ThumbnailCreator::createThumbnail(const muse::io::path_t& path)
{
    muse::Ret ret;
    QEventLoop loop;
    m_thumbnailCreated.onReceive(this, [&loop, &ret](bool ok) {
        ret = ok ? muse::make_ok() : muse::make_ret(muse::Ret::Code::UnknownError);
        loop.quit();
    });

    m_createThumbnailRequested.send(thumbnailPath(path));
    loop.exec();
    m_thumbnailCreated.resetOnReceive(this);

    return ret;
}

muse::io::path_t ThumbnailCreator::thumbnailPath(const muse::io::path_t& path) const
{
    muse::io::FileInfo fileInfo(path);
    muse::io::path_t completePath = fileInfo.dirPath()
                                    .appendingComponent(fileInfo.baseName())
                                    .appendingSuffix(FILE_SUFFIX);

    return completePath;
}
