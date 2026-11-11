/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <gmock/gmock.h>

#include "au3cloud/iau3audiocomservice.h"

namespace au::au3cloud {
class Au3AudioComServiceMock : public IAu3AudioComService
{
public:
    MOCK_METHOD(bool, enabled, (), (const, override));

    MOCK_METHOD(muse::async::Promise<ProjectList>, downloadProjectList, (size_t, size_t, const FetchOptions&), (override));
    MOCK_METHOD(void, clearProjectListCache, (), (override));

    MOCK_METHOD(muse::async::Promise<AudioList>, downloadAudioList, (size_t, size_t, const FetchOptions&), (override));
    MOCK_METHOD(void, clearAudioListCache, (), (override));
    MOCK_METHOD((muse::async::Channel<std::string, muse::io::path_t>), audioThumbnailFileUpdated, (), (const, override));

    MOCK_METHOD(muse::RetVal<muse::ProgressPtr>, uploadProject,
                (au::project::IAudacityProjectPtr, const std::string&, std::function<bool()>, UploadMode), (override));
    MOCK_METHOD(muse::RetVal<muse::ProgressPtr>, updateAudioPreview, (au::project::IAudacityProjectPtr), (override));
    MOCK_METHOD(muse::RetVal<muse::ProgressPtr>, openCloudProject,
                (const muse::io::path_t&, const std::string&, const std::string&, bool), (override));
    MOCK_METHOD(muse::RetVal<muse::ProgressPtr>, resumeProjectSync, (au::project::IAudacityProjectPtr), (override));
    MOCK_METHOD(muse::ValCh<bool>, syncingInProgressChanged, (), (const, override));
    MOCK_METHOD(void, stopProjectSync, (), (override));

    MOCK_METHOD(muse::RetVal<muse::ProgressPtr>, shareAudio, (const std::string&), (override));
    MOCK_METHOD(muse::RetVal<muse::ProgressPtr>, downloadAudioFile, (const std::string&), (override));

    MOCK_METHOD(std::string, getCloudProjectPage, (const std::string&), (const, override));
    MOCK_METHOD(std::string, getCloudAudioPage, (const std::string&), (const, override));
    MOCK_METHOD(std::string, getCloudProfilePage, (), (const, override));
    MOCK_METHOD(std::string, getTourPage, (), (const, override));

    MOCK_METHOD(muse::Ret, deleteCloudProject, (const muse::io::path_t&), (override));

    MOCK_METHOD(void, deinit, (), (override));
};
}
