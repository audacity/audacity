/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <map>
#include <mutex>

#include "context/iglobalcontext.h"
#include "framework/global/async/asyncable.h"
#include "framework/global/async/promise.h"
#include "framework/global/modularity/ioc.h"
#include "framework/global/io/ifilesystem.h"

#include "au3cloud/cloudtypes.h"
#include "au3cloud/iau3audiocomservice.h"
#include "importexport/export/iexporter.h"
#include "project/iprojectconfiguration.h"

#include "au3-cloud-audiocom/UploadService.h"
#include "au3-utility/Observer.h"

namespace au::au3cloud {
class Au3AudioComService : public IAu3AudioComService, public muse::async::Asyncable, public muse::Injectable
{
    muse::GlobalInject<muse::io::IFileSystem> filesystem;
    muse::GlobalInject<project::IProjectConfiguration> projectConfiguration;
    muse::Inject<importexport::IExporter> exporter{ this };
    muse::Inject<context::IGlobalContext> globalContext { this };

public:
    Au3AudioComService(const muse::modularity::ContextPtr& ctx)
        : muse::Injectable(ctx) {}

    muse::async::Promise<ProjectList> downloadProjectList(size_t projectsPerBatch, size_t batchNumber,
                                                          const FetchOptions& options) override;
    void clearProjectListCache() override;

    muse::async::Promise<AudioList> downloadAudioList(size_t audiosPerBatch, size_t batchNumber, const FetchOptions& options) override;
    void clearAudioListCache() override;

    muse::ProgressPtr uploadProject(au::project::IAudacityProjectPtr project, const std::string& name) override;
    std::string getCloudProjectPage(au::project::IAudacityProjectPtr project) override;

    muse::ProgressPtr shareAudio(const std::string& title) override;
    std::string getSharedAudioPage() const override;

private:
    struct CachedProjectItem {
        ProjectList projectList;
        std::chrono::system_clock::time_point timestamp;
    };
    struct CachedAudioItem {
        AudioList audioList;
        std::chrono::system_clock::time_point timestamp;
    };

    std::map<size_t, CachedProjectItem> m_projectListCache;
    size_t m_projectsPerBatch = 0;

    std::map<size_t, CachedAudioItem> m_audioListCache;
    size_t m_audiosPerBatch = 0;

    std::mutex m_cacheMutex;

    Observer::Subscription m_projectUploadSubscription;

    std::shared_ptr <audacity::cloud::audiocom::UploadService> m_uploadService;
    audacity::cloud::audiocom::UploadOperationHandle m_uploadOperationHandle;

    std::string m_sharedAudioUrl;
};
}
