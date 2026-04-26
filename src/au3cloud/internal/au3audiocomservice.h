/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <map>
#include <memory>
#include <mutex>
#include <optional>
#include <string>

#include "au3cloud/internal/downloadmanager.h"
#include "framework/global/async/asyncable.h"
#include "framework/global/async/promise.h"

#include "framework/global/modularity/ioc.h"
#include "framework/global/io/ifilesystem.h"

#include "appshell/iappshellconfiguration.h"
#include "project/iprojectconfiguration.h"
#include "au3cloud/iauthorization.h"
#include "importexport/export/iexporter.h"
#include "context/iglobalcontext.h"

#include "au3-cloud-audiocom/CloudSyncService.h"
#include "au3-cloud-audiocom/sync/CloudProjectsDatabase.h"
#include "au3-concurrency/concurrency/CancellationContext.h"
#include "au3-utility/Observer.h"

#include "au3cloud/cloudtypes.h"
#include "au3cloud/iau3audiocomservice.h"

namespace au::au3cloud {
class Au3AudioComService : public IAu3AudioComService, public muse::async::Asyncable, public muse::Contextable,
    public std::enable_shared_from_this<Au3AudioComService>
{
    muse::GlobalInject<muse::io::IFileSystem> filesystem;
    muse::GlobalInject<project::IProjectConfiguration> projectConfiguration;
    muse::GlobalInject<appshell::IAppShellConfiguration> appshellConfiguration;
    muse::GlobalInject<au::au3cloud::IAuthorization> authorization;

    muse::ContextInject<importexport::IExporter> exporter{ this };
    muse::ContextInject<context::IGlobalContext> globalContext { this };

public:
    Au3AudioComService(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx), m_downloadManager(std::make_unique<DownloadManager>())
    {}

    void init();

    bool enabled() const override;

    muse::async::Promise<ProjectList> downloadProjectList(size_t projectsPerBatch, size_t batchNumber,
                                                          const FetchOptions& options) override;
    void clearProjectListCache() override;

    muse::async::Promise<AudioList> downloadAudioList(size_t audiosPerBatch, size_t batchNumber, const FetchOptions& options) override;
    void clearAudioListCache() override;
    muse::async::Channel<std::string, muse::io::path_t> audioThumbnailFileUpdated() const override;

    muse::RetVal<muse::ProgressPtr> uploadProject(au::project::IAudacityProjectPtr project, const std::string& name,
                                                  std::function<bool()> projectSaveCallback, bool forceOverwrite = false) override;

    muse::RetVal<muse::ProgressPtr> openCloudProject(const muse::io::path_t& localPath, const std::string& projectId = {},
                                                     bool forceOverwrite = false) override;

    muse::RetVal<muse::ProgressPtr> resumeProjectSync(au::project::IAudacityProjectPtr project) override;

    muse::RetVal<muse::ProgressPtr> shareAudio(const std::string& title) override;
    muse::RetVal<muse::ProgressPtr> downloadAudioFile(const std::string& audioId) override;

    std::string getCloudProjectPage(const std::string& slug) const override;
    std::string getCloudAudioPage(const std::string& slug) const override;

    void deinit() override;

private:
    std::string getCloudProjectPage(au::project::IAudacityProjectPtr project) const;

    static void removeProjectFromDatabase(const muse::io::path_t& localPath);
    bool isSnapshotUpToDate(
        const std::optional<audacity::cloud::audiocom::sync::DBProjectData>& dbProjectData,
        audacity::cloud::audiocom::sync::ProgressCallback progressCallback, audacity::concurrency::CancellationContextPtr context);
    std::optional<std::string> getHeadSnapshotID(
        const std::string& projectId, audacity::cloud::audiocom::sync::ProgressCallback progressCallback,
        audacity::concurrency::CancellationContextPtr context);
    std::optional<ProjectList::Item> findCachedProject(const std::string& projectId) const;
    muse::Ret checkUnsyncedProject(const std::string& cloudProjectId) const;

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

    mutable std::mutex m_cacheMutex;

    struct UploadSubscriptionEntry {
        Observer::Subscription subscription;
        std::shared_ptr<std::atomic<bool> > done;
    };
    std::vector<UploadSubscriptionEntry> m_projectUploadSubscriptions;
    std::mutex m_uploadSubscriptionsMutex;

    Observer::Subscription m_resumeSyncSubscription;

    std::unique_ptr<DownloadManager> m_downloadManager;
    muse::async::Channel<std::string, muse::io::path_t> m_audioThumbnailFileUpdatedChannel;
};
}
