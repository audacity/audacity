/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MixdownUploader.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <atomic>
#include <cstdint>
#include <condition_variable>
#include <functional>
#include <future>
#include <memory>
#include <mutex>
#include <optional>
#include <string>

#include "CloudSyncDTO.h"

#include "NetworkUtils.h"

#include "concurrency/CancellationContext.h"
#include "concurrency/ICancellable.h"

class AudacityProject;

namespace audacity::cloud::audiocom {
class ServiceConfig;
}

namespace audacity::cloud::audiocom::sync {
enum class MixdownState : uint32_t
{
    Exporting,
    WaitingForUrls,
    Uploading,
    Succeeded,
    Cancelled,
    Failed,
    Empty,
};

struct CLOUD_AUDIOCOM_API MixdownResult final
{
    MixdownState State;
    ResponseResult UploadResult;
};

using MixdownProgressCallback = std::function<void (double progress)>;

class CLOUD_AUDIOCOM_API MixdownUploader final : public std::enable_shared_from_this<MixdownUploader>, public concurrency::ICancellable
{
    struct Tag final
    {
    };

public:
    MixdownUploader(
        Tag, concurrency::CancellationContextPtr cancellationContext, const ServiceConfig& config, const AudacityProject& project,
        MixdownProgressCallback progressCallback);

    ~MixdownUploader();

    static std::shared_ptr<MixdownUploader> Upload(
        concurrency::CancellationContextPtr cancellationContext, const ServiceConfig& config, const AudacityProject& project,
        MixdownProgressCallback progressCallback);

    void SetUrls(const UploadUrls& urls);

    std::future<MixdownResult> GetResultFuture();

private:
    void Cancel();

    void ReportProgress(
        MixdownState state, double progress, ResponseResult uploadResult);
    void ExportProject();
    void UploadMixdown();

    const ServiceConfig& mServiceConfig;
    const AudacityProject& mProject;

    std::mutex mUploadUrlsMutex;
    std::condition_variable mUploadUrlsSet;
    std::optional<UploadUrls> mUploadUrls;

    MixdownProgressCallback mProgressCallback;

    class DataExporter;
    std::unique_ptr<DataExporter> mDataExporter;

    std::string mExportedFilePath;

    std::atomic<double> mProgress {};

    bool mProgressUpdateQueued { false };

    std::atomic<bool> mUploadCancelled { false };
    std::atomic<bool> mFinished { false };

    std::promise<MixdownResult> mPromise;

    concurrency::CancellationContextPtr mCancellationContext;
}; // class MixdownUploader
} // namespace audacity::cloud::audiocom::sync
