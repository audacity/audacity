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

muse::async::Promise<muse::Ret> ThumbnailCreator::createThumbnail()
{
    return muse::async::Promise<muse::Ret>([this](auto resolve, auto reject) {
        m_thumbnailCreated.onReceive(this, [resolve, reject](bool ok) {
            if (ok) {
                (void)resolve(muse::make_ret(muse::Ret::Code::Ok));
            }
            else {
                muse::Ret ret = muse::make_ret(muse::Ret::Code::UnknownError);
                (void)reject(ret.code(), ret.text());
            }
        });
        m_createThumbnailRequested.notify();
        return muse::async::Promise<muse::Ret>::Result::unchecked();
    }, muse::async::Promise<muse::Ret>::AsynchronyType::ProvidedByBody);

}