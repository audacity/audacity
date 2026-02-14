/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <map>
#include <mutex>

#include "framework/global/async/asyncable.h"
#include "framework/global/async/promise.h"

#include "au3-concurrency/concurrency/CancellationContext.h"

#include "au3cloud/cloudtypes.h"
#include "au3cloud/iau3audiocomservice.h"

namespace au::au3cloud {
class Au3AudioComService : public IAu3AudioComService, public muse::async::Asyncable
{
public:
    muse::async::Promise<ProjectList> downloadProjectList(size_t projectsPerBatch, size_t batchNumber,
                                                          const FetchOptions& options) override;
    void clearProjectListCache() override;

    muse::async::Promise<AudioList> downloadAudioList(size_t audiosPerBatch, size_t batchNumber, const FetchOptions& options) override;
    void clearAudioListCache() override;

    void cancelRequests() override;

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

    std::mutex m_pendingRequestsMutex;
    std::set<audacity::concurrency::CancellationContextPtr> m_pendingCancellationContexts;
};
}
