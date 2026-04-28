/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <functional>
#include <optional>
#include <chrono>
#include <string>

#include "framework/global/modularity/imoduleinterface.h"
#include "framework/global/async/promise.h"
#include "framework/global/progress.h"
#include "framework/global/io/path.h"
#include "framework/global/async/channel.h"

#include "project/iaudacityproject.h"
#include "cloudtypes.h"

namespace au::au3cloud {
enum class CachePolicy {
    CacheFirst,
    Network,
};

struct FetchOptions {
    CachePolicy cachePolicy = CachePolicy::CacheFirst;
    std::optional<std::chrono::seconds> maxCacheAge = std::nullopt;
};

class IAu3AudioComService : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IAu3AudioComService)

public:
    virtual ~IAu3AudioComService() = default;

    virtual bool enabled() const = 0;

    virtual muse::async::Promise<ProjectList> downloadProjectList(size_t projectsPerBatch, size_t batchNumber,
                                                                  const FetchOptions& options = {}) = 0;
    virtual void clearProjectListCache() = 0;

    virtual muse::async::Promise<AudioList> downloadAudioList(size_t audiosPerBatch, size_t batchNumber,
                                                              const FetchOptions& options = {}) = 0;
    virtual void clearAudioListCache() = 0;
    virtual muse::async::Channel<std::string, muse::io::path_t> audioThumbnailFileUpdated() const = 0;

    virtual muse::RetVal<muse::ProgressPtr> uploadProject(au::project::IAudacityProjectPtr project, const std::string& name,
                                                          std::function<bool()> projectSaveCallback = nullptr,
                                                          bool forceOverwrite = false) = 0;

    virtual muse::RetVal<muse::ProgressPtr> openCloudProject(const muse::io::path_t& localPath, const std::string& projectId = {},
                                                             bool forceOverwrite = false) = 0;
    virtual muse::RetVal<muse::ProgressPtr> resumeProjectSync(au::project::IAudacityProjectPtr project) = 0;

    virtual muse::RetVal<muse::ProgressPtr> shareAudio(const std::string& title) = 0;
    virtual muse::RetVal<muse::ProgressPtr> downloadAudioFile(const std::string& audioId) = 0;

    virtual std::string getCloudProjectPage(const std::string& slug) const = 0;
    virtual std::string getCloudAudioPage(const std::string& slug) const = 0;

    virtual void deinit() = 0;
};
}
