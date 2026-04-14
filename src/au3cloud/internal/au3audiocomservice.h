/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <map>
#include <mutex>
#include <optional>

#include "framework/global/async/asyncable.h"
#include "framework/global/async/promise.h"

#include "framework/global/modularity/ioc.h"
#include "framework/global/io/ifilesystem.h"
#include "project/iprojectconfiguration.h"
#include "importexport/export/iexporter.h"
#include "context/iglobalcontext.h"

#include "au3-cloud-audiocom/sync/CloudProjectsDatabase.h"
#include "au3-utility/Observer.h"

#include "au3cloud/cloudtypes.h"
#include "au3cloud/iau3audiocomservice.h"

namespace au::au3cloud {
class Au3AudioComService : public IAu3AudioComService, public muse::async::Asyncable, public muse::Contextable
{
    muse::GlobalInject<muse::io::IFileSystem> filesystem;
    muse::GlobalInject<project::IProjectConfiguration> projectConfiguration;

    muse::ContextInject<importexport::IExporter> exporter{ this };
    muse::ContextInject<context::IGlobalContext> globalContext { this };

public:
    Au3AudioComService(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void init();

    bool enabled() const override;

    muse::async::Promise<ProjectList> downloadProjectList(size_t projectsPerBatch, size_t batchNumber,
                                                          const FetchOptions& options) override;
    void clearProjectListCache() override;

    muse::async::Promise<AudioList> downloadAudioList(size_t audiosPerBatch, size_t batchNumber, const FetchOptions& options) override;
    void clearAudioListCache() override;

    muse::ProgressPtr uploadProject(au::project::IAudacityProjectPtr project, const std::string& name,
                                    std::function<bool()> projectSaveCallback, bool forceOverwrite = false) override;
    muse::ProgressPtr shareAudio(const std::string& title) override;

    muse::ProgressPtr openCloudProject(const muse::io::path_t& localPath, const std::string& projectId = {},
                                       bool forceOverwrite = false) override;

    muse::ProgressPtr resumeProjectSync(au::project::IAudacityProjectPtr project) override;

private:
    std::string getCloudProjectPage(au::project::IAudacityProjectPtr project);

    static void removeProjectFromDatabase(const muse::io::path_t& localPath);
    bool isSnapshotUpToDate(const std::optional<audacity::cloud::audiocom::sync::DBProjectData>& dbProjectData);
    std::optional<std::string> getHeadSnapshotID(const std::string& projectId);

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

    struct UploadSubscriptionEntry {
        Observer::Subscription subscription;
        std::shared_ptr<std::atomic<bool> > done;
    };
    std::vector<UploadSubscriptionEntry> m_projectUploadSubscriptions;
    std::mutex m_uploadSubscriptionsMutex;

    Observer::Subscription m_resumeSyncSubscription;
};
}
