/*
* Audacity: A Digital Audio Editor
*/
#include "au3audiocomservicestub.h"

using namespace au::au3cloud;

bool Au3AudioComServiceStub::enabled() const
{
    return false;
}

muse::async::Promise<ProjectList> Au3AudioComServiceStub::downloadProjectList(size_t, size_t, const FetchOptions&)
{
    return muse::async::Promise<ProjectList>([](auto resolve, auto) {
        return resolve(ProjectList {});
    });
}

void Au3AudioComServiceStub::clearProjectListCache()
{
}

muse::async::Promise<AudioList> Au3AudioComServiceStub::downloadAudioList(size_t, size_t, const FetchOptions&)
{
    return muse::async::Promise<AudioList>([](auto resolve, auto) {
        return resolve(AudioList {});
    });
}

void Au3AudioComServiceStub::clearAudioListCache()
{
}

muse::RetVal<muse::ProgressPtr> Au3AudioComServiceStub::uploadProject(au::project::IAudacityProjectPtr, const std::string&,
                                                                      std::function<bool()>, bool)
{
    return muse::RetVal<muse::ProgressPtr>::make_ret(muse::Ret::Code::NotSupported);
}

muse::RetVal<muse::ProgressPtr> Au3AudioComServiceStub::shareAudio(const std::string&)
{
    return muse::RetVal<muse::ProgressPtr>::make_ret(muse::Ret::Code::NotSupported);
}

muse::RetVal<muse::ProgressPtr> Au3AudioComServiceStub::downloadAudioFile(const std::string&)
{
    return muse::RetVal<muse::ProgressPtr>::make_ret(muse::Ret::Code::NotSupported);
}

muse::RetVal<muse::ProgressPtr> Au3AudioComServiceStub::openCloudProject(const muse::io::path_t&, const std::string&, bool)
{
    return muse::RetVal<muse::ProgressPtr>::make_ret(muse::Ret::Code::NotSupported);
}

muse::RetVal<muse::ProgressPtr> Au3AudioComServiceStub::resumeProjectSync(au::project::IAudacityProjectPtr)
{
    return muse::RetVal<muse::ProgressPtr>::make_ret(muse::Ret::Code::NotSupported);
}

muse::async::Channel<std::string, muse::io::path_t> Au3AudioComServiceStub::audioThumbnailFileUpdated() const
{
    return {};
}

std::string Au3AudioComServiceStub::getCloudAudioPage(const std::string& /*unused*/) const
{
    return {};
}

std::string Au3AudioComServiceStub::getCloudProjectPage(const std::string& /*unused*/) const
{
    return {};
}

void Au3AudioComServiceStub::deinit()
{
}
