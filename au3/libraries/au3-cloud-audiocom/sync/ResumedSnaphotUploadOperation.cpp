/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ResumedSnaphotUploadOperation.cpp

  Dmitry Vedenko

**********************************************************************/

#include "ResumedSnaphotUploadOperation.h"

#include <algorithm>
#include <memory>
#include <string>
#include <string_view>

#include "concurrency/CancellationContext.h"

#include "CloudProjectsDatabase.h"
#include "DataUploader.h"
#include "MissingBlocksUploader.h"
#include "ProjectCloudExtension.h"
#include "ProjectUploadOperation.h"
#include "ServiceConfig.h"

#include "ExportUtils.h"
#include "SampleBlock.h"
#include "WaveTrack.h"

#include "CodeConversions.h"
#include "DateTimeConversions.h"
#include "FromChars.h"
#include "IResponse.h"
#include "NetworkManager.h"
#include "Request.h"
#include "UriParser.h"

namespace audacity::cloud::audiocom::sync {
namespace {
bool IsUrlExpired(const std::string& url)
{
    const auto parsedUri   = ParseUri(url);
    const auto parsedQuery = ParseUriQuery(parsedUri.Query);

    const auto amzDateIt = parsedQuery.find("X-Amz-Date");

    if (amzDateIt == parsedQuery.end()) {
        return false;
    }

    SystemTime time;

    if (!ParseISO8601Date(std::string(amzDateIt->second), &time)) {
        return false;
    }

    const auto amzExpiresIt = parsedQuery.find("X-Amz-Expires");

    if (amzExpiresIt == parsedQuery.end()) {
        return false;
    }

    int64_t expiresSeconds;

    auto expiresParseResult = FromChars(
        amzExpiresIt->second.data(),
        amzExpiresIt->second.data() + amzExpiresIt->second.size(),
        expiresSeconds);

    if (expiresParseResult.ec != std::errc {}) {
        return false;
    }

    return (time + std::chrono::seconds { expiresSeconds })
           < std::chrono::system_clock::now();
}

class ResumedSnaphotUploadOperation final : public ProjectUploadOperation,
    public std::enable_shared_from_this<ResumedSnaphotUploadOperation>
{
    struct Tag
    {
    };

public:
    ResumedSnaphotUploadOperation(
        Tag, ProjectCloudExtension& projectCloudExtension,
        std::string_view snapshotId, std::string_view confirmationUrl)
        : mProjectCloudExtension{projectCloudExtension}
        , mProjectId{mProjectCloudExtension.GetCloudProjectId()}
        , mSnapshotId{snapshotId}
        , mConfirmationUrl{confirmationUrl}
        , mCancellationContext{concurrency::CancellationContext::Create()}
    {
    }

    ~ResumedSnaphotUploadOperation() override
    {
    }

    static void Perform(
        ProjectCloudExtension& projectCloudExtension, std::string_view snapshotId,
        std::string_view confirmationUrl)
    {
        auto& cloudProjectsDatabase = CloudProjectsDatabase::Get();

        auto operation = std::make_shared<ResumedSnaphotUploadOperation>(
            Tag {}, projectCloudExtension, snapshotId, confirmationUrl);

        const auto projectId = projectCloudExtension.GetCloudProjectId();

        operation->mPendingProjectBlobData
            =cloudProjectsDatabase.GetPendingProjectBlob(projectId, snapshotId);

        operation->mPendingProjectBlocks
            =cloudProjectsDatabase.GetPendingProjectBlocks(projectId, snapshotId);

        const int64_t totalBlocks = operation->mPendingProjectBlocks.size();

        if (operation->mPendingProjectBlobData) {
            operation->mHasExpiredUrls
                =IsUrlExpired(operation->mPendingProjectBlobData->UploadUrl);
        }

        for (const auto& block : operation->mPendingProjectBlocks) {
            if (operation->mHasExpiredUrls) {
                break;
            }

            operation->mHasExpiredUrls = IsUrlExpired(block.UploadUrl);
        }

        projectCloudExtension.OnSyncResumed(
            operation, totalBlocks,
            operation->mPendingProjectBlobData.has_value());
    }

private:
    void UploadSnapshot()
    {
        const auto urls = UploadUrls { {},
            mPendingProjectBlobData->UploadUrl,
            mPendingProjectBlobData->ConfirmUrl,
            mPendingProjectBlobData->FailUrl };

        DataUploader::Get().Upload(
            mCancellationContext, GetServiceConfig(), urls,
            mPendingProjectBlobData->BlobData,
            [this, weakThis = weak_from_this()](auto result)
        {
            auto strongThis = weakThis.lock();
            if (!strongThis) {
                return;
            }

            if (!IsUploadRecoverable(result.Code)) {
                CloudProjectsDatabase::Get().RemovePendingProjectBlob(
                    mProjectId, mSnapshotId);
            }

            if (result.Code == SyncResultCode::Success) {
                mProjectCloudExtension.OnProjectDataUploaded(*this);
                UploadBlocks();
            } else {
                FailSync(std::move(result));
            }
        });
    }

    void CompleteSync()
    {
        if (!mCompleted.exchange(true)) {
            mProjectCloudExtension.OnSyncCompleted(
                this, {}, AudiocomTrace::ProjectOpenedAndUploadResumed);
        }
    }

    void FailSync(CloudSyncError error)
    {
        if (!mCompleted.exchange(true)) {
            mProjectCloudExtension.OnSyncCompleted(
                this, error, AudiocomTrace::ProjectOpenedAndUploadResumed);
        }
    }

    void FailSync(ResponseResult result)
    {
        FailSync(CloudSyncError { DeduceError(result.Code), result.Content });
    }

    void UploadBlocks()
    {
        if (mPendingProjectBlocks.empty()) {
            MarkSnapshotSynced();
        }

        auto project = mProjectCloudExtension.GetProject().lock();

        if (!project) {
            FailSync({ SyncResultCode::InternalClientError });
            return;
        }

        auto& waveTrackFactory   = WaveTrackFactory::Get(*project);
        auto& sampleBlockFactory = waveTrackFactory.GetSampleBlockFactory();

        std::vector<BlockUploadTask> blockTasks;
        blockTasks.reserve(mPendingProjectBlocks.size());

        for (const auto& pendingBlock : mPendingProjectBlocks) {
            BlockUploadTask task;

            task.BlockUrls = { {},
                pendingBlock.UploadUrl,
                pendingBlock.ConfirmUrl,
                pendingBlock.FailUrl };

            task.Block.Format
                =static_cast<sampleFormat>(pendingBlock.BlockSampleFormat);
            task.Block.Hash = pendingBlock.BlockHash;
            task.Block.Id   = pendingBlock.BlockId;
            try
            {
                task.Block.Block = sampleBlockFactory->CreateFromId(
                    task.Block.Format, pendingBlock.BlockId);

                blockTasks.push_back(std::move(task));
            }
            catch (const FileException& e)
            {
                // We have failed to resume the upload, local data is missing
                CloudProjectsDatabase::Get().RemovePendingSnapshot(
                    mProjectId, mSnapshotId);
                FailSync(
                    { SyncResultCode::InternalClientError,
                      ToUTF8(
                          XO("Local project data was removed before the sync has completed")
                          .Translation()) });
                return;
            }
        }

        mMissingBlocksUploader = MissingBlocksUploader::Create(
            mCancellationContext, GetServiceConfig(), std::move(blockTasks),
            [this, weakThis = weak_from_this()](
                const MissingBlocksUploadProgress& progress,
                const LockedBlock& block, ResponseResult blockResponseResult)
        {
            auto strongThis = weakThis.lock();
            if (!strongThis) {
                return;
            }

            if (
                blockResponseResult.Code != SyncResultCode::ConnectionFailed
                && blockResponseResult.Code != SyncResultCode::Cancelled) {
                CloudProjectsDatabase::Get().RemovePendingProjectBlock(
                    mProjectId, block.Id);
            }

            mProjectCloudExtension.OnBlockUploaded(
                *this, block.Hash,
                blockResponseResult.Code == SyncResultCode::Success);

            const auto completed
                =progress.UploadedBlocks == progress.TotalBlocks
                  || progress.FailedBlocks != 0;

            const bool succeeded = completed && progress.FailedBlocks == 0;

            if (!completed) {
                return;
            }

            if (succeeded) {
                MarkSnapshotSynced();
            } else {
                FailSync(std::move(blockResponseResult));
            }
        });
    }

    void Start() override
    {
        if (mHasExpiredUrls) {
            RefreshUrls();
        } else if (mPendingProjectBlobData.has_value()) {
            UploadSnapshot();
        } else {
            UploadBlocks();
        }
    }

    void RefreshUrls()
    {
        using namespace network_manager;
        Request request { GetServiceConfig().GetSnapshotSyncUrl(
                              mProjectId, mSnapshotId) };

        SetCommonHeaders(request);

        auto response = NetworkManager::GetInstance().doGet(request);

        response->setRequestFinishedCallback(
            [this, response, weakThis = weak_from_this()](auto)
        {
            auto strongThis = weakThis.lock();
            if (!strongThis) {
                return;
            }

            if (response->getError() != NetworkError::NoError) {
                FailSync(DeduceUploadError(*response));
                return;
            }

            auto syncState
                =DeserializeProjectSyncState(response->readAll<std::string>());

            if (!syncState) {
                FailSync(
                    MakeClientFailure(XO("Failed to deserialize the response")));
                return;
            }

            UpdateUrls(*syncState);

            if (mPendingProjectBlobData.has_value()) {
                UploadSnapshot();
            } else {
                UploadBlocks();
            }
        });
    }

    void UpdateUrls(const ProjectSyncState& syncState)
    {
        if (mPendingProjectBlobData.has_value()) {
            if (syncState.FileUrls.UploadUrl.empty()) {
                mPendingProjectBlobData = {};
                CloudProjectsDatabase::Get().RemovePendingProjectBlob(
                    mProjectId, mSnapshotId);
            } else {
                mPendingProjectBlobData->UploadUrl  = syncState.FileUrls.UploadUrl;
                mPendingProjectBlobData->ConfirmUrl = syncState.FileUrls.SuccessUrl;
                mPendingProjectBlobData->FailUrl    = syncState.FileUrls.SuccessUrl;
            }
        }

        std::unordered_map<std::string, UploadUrls> urlsLookup;
        for (const auto& urls : syncState.MissingBlocks) {
            urlsLookup.emplace(urls.Id, urls);
        }

        for (auto& block : mPendingProjectBlocks) {
            auto it = urlsLookup.find(block.BlockHash);

            if (it == urlsLookup.end()) {
                CloudProjectsDatabase::Get().RemovePendingProjectBlock(
                    mProjectId, block.BlockId);

                continue;
            }

            block.UploadUrl  = urlsLookup[block.BlockHash].UploadUrl;
            block.ConfirmUrl = urlsLookup[block.BlockHash].SuccessUrl;
            block.FailUrl    = urlsLookup[block.BlockHash].FailUrl;
        }

        mPendingProjectBlocks.erase(
            std::remove_if(
                mPendingProjectBlocks.begin(), mPendingProjectBlocks.end(),
                [&urlsLookup](auto& block)
        { return urlsLookup.find(block.BlockHash) == urlsLookup.end(); }),
            mPendingProjectBlocks.end());
    }

    void MarkSnapshotSynced()
    {
        using namespace network_manager;
        Request request { mConfirmationUrl };

        SetCommonHeaders(request);

        auto response = NetworkManager::GetInstance().doPost(request, nullptr, 0);

        response->setRequestFinishedCallback(
            [this, response, weakThis = weak_from_this()](auto)
        {
            auto strongThis = weakThis.lock();
            if (!strongThis) {
                return;
            }

            CloudProjectsDatabase::Get().RemovePendingSnapshot(
                mProjectId, mSnapshotId);

            if (response->getError() != NetworkError::NoError) {
                FailSync(DeduceUploadError(*response));
                return;
            }

            CompleteSync();
        });

        mCancellationContext->OnCancelled(response);
    }

    void SetUploadData(const ProjectUploadData& data) override
    {
        // This method will never be called for resumed operations
    }

    bool IsCompleted() const override
    {
        return mCompleted.load();
    }

    void Cancel() override
    {
        mCancellationContext->Cancel();
        FailSync(CloudSyncError { CloudSyncError::Cancelled });
    }

    ProjectCloudExtension& mProjectCloudExtension;

    std::string mProjectId;
    std::string mSnapshotId;
    std::string mConfirmationUrl;

    concurrency::CancellationContextPtr mCancellationContext;

    std::optional<PendingProjectBlobData> mPendingProjectBlobData;
    std::vector<PendingProjectBlockData> mPendingProjectBlocks;

    std::shared_ptr<MissingBlocksUploader> mMissingBlocksUploader;

    std::atomic<bool> mCompleted { false };

    bool mHasExpiredUrls { false };
}; // class ResumedProjectUploadOperation
} // namespace

void ResumeProjectUpload(
    ProjectCloudExtension& projectCloudExtension,
    std::function<void(AudiocomTrace)> onBeforeUploadStarts)
{
    auto& cloudProjectsDatabase = CloudProjectsDatabase::Get();

    auto pendingSnapshots = cloudProjectsDatabase.GetPendingSnapshots(
        projectCloudExtension.GetCloudProjectId());

    if (!pendingSnapshots.empty() && onBeforeUploadStarts) {
        onBeforeUploadStarts(AudiocomTrace::ProjectOpenedAndUploadResumed);
    }

    for (const auto& snapshot : pendingSnapshots) {
        ResumedSnaphotUploadOperation::Perform(
            projectCloudExtension, snapshot.SnapshotId, snapshot.ConfirmUrl);
    }
}
} // namespace audacity::cloud::audiocom::sync
