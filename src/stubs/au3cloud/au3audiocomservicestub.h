/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "au3cloud/iau3audiocomservice.h"
#include "framework/global/modularity/ioc.h"

namespace au::au3cloud {
class Au3AudioComServiceStub : public IAu3AudioComService, public muse::Injectable
{
public:
    Au3AudioComServiceStub(const muse::modularity::ContextPtr& ctx)
        : muse::Injectable(ctx) {}

    bool enabled() const override;

    muse::async::Promise<ProjectList> downloadProjectList(size_t projectsPerBatch, size_t batchNumber,
                                                          const FetchOptions& options) override;
    void clearProjectListCache() override;

    muse::async::Promise<AudioList> downloadAudioList(size_t audiosPerBatch, size_t batchNumber, const FetchOptions& options) override;
    void clearAudioListCache() override;
    muse::async::Channel<std::string, muse::io::path_t> audioThumbnailFileUpdated() const override;

    muse::RetVal<muse::ProgressPtr> uploadProject(au::project::IAudacityProjectPtr project, const std::string& name,
                                                  std::function<bool()> projectSaveCallback = nullptr, bool forceOverwrite = false) override;
    muse::RetVal<muse::ProgressPtr> shareAudio(const std::string& title) override;
    muse::RetVal<muse::ProgressPtr> downloadAudioFile(const std::string& audioId) override;

    muse::RetVal<muse::ProgressPtr> openCloudProject(const muse::io::path_t& localPath, const std::string& projectId = {},
                                                     bool forceOverwrite = false) override;
    muse::RetVal<muse::ProgressPtr> resumeProjectSync(au::project::IAudacityProjectPtr project) override;

    std::string getCloudProjectPage(const std::string& slug) const override;
    std::string getCloudAudioPage(const std::string& audioId) const override;

    void deinit() override;
};
}
