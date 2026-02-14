/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <optional>
#include <chrono>

#include "framework/global/modularity/imoduleinterface.h"
#include "framework/global/async/promise.h"

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

    virtual muse::async::Promise<ProjectList> downloadProjectList(size_t projectsPerBatch, size_t batchNumber,
                                                                  const FetchOptions& options = {}) = 0;
    virtual void clearProjectListCache() = 0;

    virtual muse::async::Promise<AudioList> downloadAudioList(size_t audiosPerBatch, size_t batchNumber,
                                                              const FetchOptions& options = {}) = 0;
    virtual void clearAudioListCache() = 0;

    virtual void cancelRequests() = 0;
};
}
