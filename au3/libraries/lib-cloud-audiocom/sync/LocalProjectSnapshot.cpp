/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudProjectSnapshot.cpp

  Dmitry Vedenko

**********************************************************************/
#include "LocalProjectSnapshot.h"

#include <algorithm>
#include <future>

#include "../OAuthService.h"
#include "../ServiceConfig.h"

#include "BasicUI.h"

#include "BlockHasher.h"
#include "CloudProjectsDatabase.h"
#include "DataUploader.h"
#include "MixdownUploader.h"
#include "ProjectCloudExtension.h"

#include "ExportUtils.h"
#include "MemoryX.h"
#include "Project.h"
#include "SampleBlock.h"
#include "Sequence.h"
#include "Track.h"
#include "WaveClip.h"
#include "WaveTrack.h"
#include "WaveTrackUtilities.h"

#include "IResponse.h"
#include "NetworkManager.h"
#include "Request.h"

#include "MissingBlocksUploader.h"

#include "StringUtils.h"

#include "crypto/SHA256.h"

namespace audacity::cloud::audiocom::sync {
struct LocalProjectSnapshot::ProjectBlocksLock final : private BlockHashCache
{
    ProjectCloudExtension& Extension;

    SampleBlockIDSet BlockIds;

    std::vector<LockedBlock> Blocks;
    std::vector<BlockUploadTask> MissingBlocks;

    std::unordered_map<int64_t, size_t> BlockIdToIndex;
    std::unordered_map<std::string, size_t> BlockHashToIndex;

    std::unique_ptr<BlockHasher> Hasher;

    std::future<void> UpdateCacheFuture;
    std::vector<std::pair<int64_t, std::string> > NewHashes;

    std::function<void()> OnBlocksLocked;

    explicit ProjectBlocksLock(
        ProjectCloudExtension& extension, AudacityProject& project,
        std::function<void()> onBlocksLocked)
        : Extension{extension}
        , OnBlocksLocked{std::move(onBlocksLocked)}
    {
        VisitBlocks(TrackList::Get(project));

        if (Extension.IsCloudProject()) {
            CloudProjectsDatabase::Get().UpdateProjectBlockList(
                Extension.GetCloudProjectId(), BlockIds);
        }

        Hasher = std::make_unique<BlockHasher>();

        Hasher->ComputeHashes(*this, Blocks, [this] { CollectHashes(); });
    }

    ~ProjectBlocksLock() override
    {
    }

    void VisitBlocks(TrackList& tracks)
    {
        const auto visitor = [this](const SampleBlockPtr& pBlock){
            const auto id = pBlock->GetBlockID();
            if (id >= 0) {
                Blocks.push_back({
                        id, pBlock->GetSampleFormat(), pBlock });
                BlockIdToIndex[id] = Blocks.size() - 1;
            }
            //Do not compute hashes for negative id's, which describe
            //the length of a silenced sequence. Making an id record in
            //project blob is enough to restore block contents fully.
            //VS: Older versions of Audacity encoded them as a regular
            //blocks, but due to wrong sanity checks in `WavPackCompressor`
            //attempt to load them could fail. The check above will purge
            //such blocks if they are present in old project.
        };
        WaveTrackUtilities::VisitBlocks(tracks, visitor, &BlockIds);
    }

    void CollectHashes()
    {
        // TakeResult() will call UpdateHash() for each block
        // not found in the cache
        const auto result = Hasher->TakeResult();

        for (auto [id, hash] : result) {
            auto it = BlockIdToIndex.find(id);

            if (it == BlockIdToIndex.end()) {
                assert(false);
                continue;
            }

            while (BlockHashToIndex.find(hash) != BlockHashToIndex.end())
            {
                // Hash is used by another block, rehash
                hash = crypto::sha256(hash);
            }

            BlockHashToIndex[hash]  = BlockIdToIndex[id];
            Blocks[it->second].Hash = std::move(hash);
        }

        // This will potentially block, if the cache is being updated
        // already
        UpdateProjectHashesInCache();

        if (OnBlocksLocked) {
            OnBlocksLocked();
        }
    }

    void UpdateProjectHashesInCache()
    {
        if (!Extension.IsCloudProject()) {
            return;
        }

        UpdateCacheFuture = std::async(
            std::launch::async,
            [this, hashes = std::move(NewHashes)]
        {
            CloudProjectsDatabase::Get().UpdateBlockHashes(
                Extension.GetCloudProjectId(), hashes);
        });
    }

    bool GetHash(int64_t blockId, std::string& hash) const override
    {
        if (!Extension.IsCloudProject()) {
            return false;
        }

        auto cachedResult = CloudProjectsDatabase::Get().GetBlockHash(
            Extension.GetCloudProjectId(), blockId);

        if (!cachedResult) {
            return false;
        }

        hash = std::move(*cachedResult);

        return true;
    }

    void UpdateHash(int64_t blockId, const std::string& hash) override
    {
        NewHashes.emplace_back(blockId, hash);
    }

    void FillMissingBlocks(const std::vector<UploadUrls>& missingBlockUrls)
    {
        for (const auto& urls : missingBlockUrls) {
            auto it = BlockHashToIndex.find(ToUpper(urls.Id));

            if (it == BlockHashToIndex.end()) {
                assert(false);
                continue;
            }

            const auto index = it->second;

            MissingBlocks.push_back(BlockUploadTask { urls, Blocks[index] });
        }
    }
};

LocalProjectSnapshot::LocalProjectSnapshot(
    Tag, const ServiceConfig& config, const OAuthService& oauthService,
    ProjectCloudExtension& extension, std::string name, UploadMode mode,
    AudiocomTrace trace)
    : mProjectCloudExtension{extension}
    , mWeakProject{extension.GetProject()}
    , mServiceConfig{config}
    , mOAuthService{oauthService}
    , mAudiocomTrace{trace}
    , mProjectName{std::move(name)}
    , mUploadMode{mode}
    , mCancellationContext{concurrency::CancellationContext::Create()}
{
}

LocalProjectSnapshot::~LocalProjectSnapshot()
{
}

LocalProjectSnapshot::Future LocalProjectSnapshot::Create(
    const ServiceConfig& config, const OAuthService& oauthService,
    ProjectCloudExtension& extension, std::string name, UploadMode mode,
    AudiocomTrace trace)
{
    auto project = extension.GetProject().lock();

    if (!project) {
        return {}
    }

    auto snapshot = std::make_shared<LocalProjectSnapshot>(
        Tag {}, config, oauthService, extension, std::move(name), mode, trace);

    snapshot->mProjectCloudExtension.OnUploadOperationCreated(snapshot);

    snapshot->mProjectBlocksLock = std::make_unique<ProjectBlocksLock>(
        extension, *project,
        [weakSnapshot = std::weak_ptr(snapshot)]
    {
        auto snapshot = weakSnapshot.lock();

        if (snapshot == nullptr) {
            return;
        }

        auto project = snapshot->GetProject();

        if (project == nullptr) {
            return;
        }

        snapshot->mProjectCloudExtension.OnBlocksHashed(*snapshot);
    });

    return snapshot->mCreateSnapshotPromise.get_future();
}

bool LocalProjectSnapshot::IsCompleted() const
{
    return mCompleted.load(std::memory_order_acquire);
}

std::shared_ptr<AudacityProject> LocalProjectSnapshot::GetProject()
{
    return mWeakProject.lock();
}

void LocalProjectSnapshot::Start()
{
    UpdateProjectSnapshot();
}

void LocalProjectSnapshot::SetUploadData(const ProjectUploadData& data)
{
    mProjectDataReady.store(true);
    mProjectDataPromise.set_value(data);
}

void LocalProjectSnapshot::Cancel()
{
    mCancelled.store(true, std::memory_order_release);

    mCancellationContext->Cancel();

    if (!mProjectDataReady.load(std::memory_order_acquire)) {
        mProjectDataPromise.set_value({});
    }

    UploadFailed({ CloudSyncError::Cancelled });
}

void LocalProjectSnapshot::Abort()
{
    mCancelled.store(true, std::memory_order_release);

    mCancellationContext->Cancel();

    if (!mProjectDataReady.load(std::memory_order_acquire)) {
        mProjectDataPromise.set_value({});
    }

    UploadFailed({ CloudSyncError::Aborted });

    DeleteSnapshot();
}

void LocalProjectSnapshot::UploadFailed(CloudSyncError error)
{
    if (!mCompleted.exchange(true, std::memory_order_release)) {
        mProjectCloudExtension.OnSyncCompleted(
            this, std::make_optional(error), mAudiocomTrace);
    }
}

void LocalProjectSnapshot::DataUploadFailed(const ResponseResult& uploadResult)
{
    UploadFailed({ DeduceError(uploadResult.Code), uploadResult.Content });
}

void LocalProjectSnapshot::DataUploadFailed(
    const MissingBlocksUploadProgress& uploadResult)
{
    CloudSyncError::ErrorType errorType = CloudSyncError::DataUploadFailed;

    for (const auto& uploadError : uploadResult.UploadErrors) {
        if (
            uploadError.Code == SyncResultCode::Success
            || uploadError.Code == SyncResultCode::Conflict) {
            continue;
        }

        const auto deducedError = DeduceError(uploadError.Code);

        if (
            errorType == CloudSyncError::DataUploadFailed
            && deducedError == CloudSyncError::Network) {
            errorType = deducedError;
        } else if (
            deducedError == CloudSyncError::ProjectStorageLimitReached
            || deducedError == CloudSyncError::Cancelled) {
            errorType = deducedError;
            break;
        }
    }

    UploadFailed({ errorType, {} });
}

void LocalProjectSnapshot::UpdateProjectSnapshot()
{
    auto project = mWeakProject.lock();

    if (project == nullptr) {
        UploadFailed(MakeClientFailure(
                         XO("Project was closed before snapshot was created")));
        return;
    }

    const bool isCloudProject = mProjectCloudExtension.IsCloudProject();
    const bool createNew
        =mUploadMode == UploadMode::CreateNew || !isCloudProject;

    ProjectForm projectForm;

    if (createNew) {
        projectForm.Name = mProjectName;
    } else {
        projectForm.HeadSnapshotId = mProjectCloudExtension.GetSnapshotId();
    }

    // For empty projects, mProjectBlocksLock will be nullptr at this point
    if (mProjectBlocksLock != nullptr) {
        projectForm.Hashes.reserve(mProjectBlocksLock->Blocks.size());
        std::transform(
            mProjectBlocksLock->Blocks.begin(), mProjectBlocksLock->Blocks.end(),
            std::back_inserter(projectForm.Hashes),
            [](const auto& block) { return block.Hash; });
    }

    using namespace audacity::network_manager;

    const auto url = createNew ? mServiceConfig.GetCreateProjectUrl()
                     : mServiceConfig.GetCreateSnapshotUrl(
        mProjectCloudExtension.GetCloudProjectId());

    projectForm.Force = !createNew && mUploadMode == UploadMode::ForceOverwrite;

    auto request = Request(url);

    request.setHeader(
        common_headers::ContentType, common_content_types::ApplicationJson);
    request.setHeader(
        common_headers::Accept, common_content_types::ApplicationJson);
    // request.setHeader(common_headers::ContentEncoding, "gzip");

    const auto language = mServiceConfig.GetAcceptLanguageValue();

    if (!language.empty()) {
        request.setHeader(
            audacity::network_manager::common_headers::AcceptLanguage, language);
    }

    request.setHeader(
        common_headers::Authorization, mOAuthService.GetAccessToken());

    auto serializedForm = Serialize(projectForm);

    auto response = NetworkManager::GetInstance().doPost(
        request, serializedForm.data(), serializedForm.size());

    response->setRequestFinishedCallback(
        [this, response, createNew, weakThis = weak_from_this()](auto)
    {
        auto strongThis = weakThis.lock();
        if (!strongThis) {
            return;
        }

        const auto error = response->getError();

        if (error != NetworkError::NoError) {
            UploadFailed(DeduceUploadError(*response));

            mCreateSnapshotPromise.set_value({});
            return;
        }

        const auto body = response->readAll<std::string>();
        auto result     = DeserializeCreateSnapshotResponse(body);

        if (!result) {
            UploadFailed(MakeClientFailure(
                             XO("Invalid Response: %s").Format(body).Translation()));

            mCreateSnapshotPromise.set_value({});
            return;
        }

        OnSnapshotCreated(*result, createNew);
    });

    mCancellationContext->OnCancelled(response);
}

void LocalProjectSnapshot::OnSnapshotCreated(
    const CreateSnapshotResponse& response, bool newProject)
{
    auto project = mWeakProject.lock();

    if (project == nullptr) {
        UploadFailed(MakeClientFailure(
                         XO("Project was closed before snapshot was created")));
        return;
    }

    if (newProject) {
        mProjectBlocksLock->UpdateProjectHashesInCache();
    }

    mProjectBlocksLock->FillMissingBlocks(response.SyncState.MissingBlocks);

    mProjectCloudExtension.OnSnapshotCreated(*this, response);

    {
        auto lock = std::lock_guard { mCreateSnapshotResponseMutex };
        mCreateSnapshotResponse = response;
    }

    mCreateSnapshotPromise.set_value({ response, shared_from_this() });

    auto projectData = mProjectDataPromise.get_future().get();

    if (mCancelled.load(std::memory_order_acquire)) {
        return;
    }

    StorePendingSnapshot(response, projectData);

    DataUploader::Get().Upload(
        mCancellationContext, mServiceConfig, response.SyncState.FileUrls,
        projectData.ProjectSnapshot,
        [this, weakThis = weak_from_this()](ResponseResult result)
    {
        auto strongThis = weakThis.lock();
        if (!strongThis) {
            return;
        }

        auto& db = CloudProjectsDatabase::Get();

        const auto projectId  = mCreateSnapshotResponse->Project.Id;
        const auto snapshotId = mCreateSnapshotResponse->Snapshot.Id;

        if (result.Code != SyncResultCode::Success) {
            db.RemovePendingSnapshot(projectId, snapshotId);
            db.RemovePendingProjectBlob(projectId, snapshotId);
            db.RemovePendingProjectBlocks(projectId, snapshotId);

            DataUploadFailed(result);
            return;
        }

        mProjectCloudExtension.OnProjectDataUploaded(*this);
        db.RemovePendingProjectBlob(projectId, snapshotId);

        if (mProjectBlocksLock->MissingBlocks.empty()) {
            MarkSnapshotSynced();
            return;
        }

        mMissingBlockUploader = MissingBlocksUploader::Create(
            mCancellationContext, mServiceConfig,
            mProjectBlocksLock->MissingBlocks,
            [this, weakThis = weak_from_this()](
                auto result, auto block, auto uploadResult)
        {
            auto strongThis = weakThis.lock();
            if (!strongThis) {
                return;
            }

            if (!IsUploadRecoverable(uploadResult.Code)) {
                CloudProjectsDatabase::Get().RemovePendingProjectBlock(
                    mCreateSnapshotResponse->Project.Id, block.Id);
            }

            mProjectCloudExtension.OnBlockUploaded(
                *this, block.Hash,
                uploadResult.Code == SyncResultCode::Success);

            const auto completed
                =result.UploadedBlocks == result.TotalBlocks
                  || result.FailedBlocks != 0;
            const bool succeeded = completed && result.FailedBlocks == 0;

            if (!completed) {
                return;
            }

            if (succeeded) {
                MarkSnapshotSynced();
            } else {
                DataUploadFailed(result);
            }
        });
    });
}

void LocalProjectSnapshot::StorePendingSnapshot(
    const CreateSnapshotResponse& response, const ProjectUploadData& projectData)
{
    CloudProjectsDatabase::Get().AddPendingSnapshot(
        { response.Project.Id, response.Snapshot.Id,
          mServiceConfig.GetSnapshotSyncUrl(
              response.Project.Id, response.Snapshot.Id) });

    CloudProjectsDatabase::Get().AddPendingProjectBlob(
        { response.Project.Id, response.Snapshot.Id,
          response.SyncState.FileUrls.UploadUrl,
          response.SyncState.FileUrls.SuccessUrl,
          response.SyncState.FileUrls.FailUrl, projectData.ProjectSnapshot });

    if (mProjectBlocksLock->MissingBlocks.empty()) {
        return;
    }

    std::vector<PendingProjectBlockData> pendingBlocks;
    pendingBlocks.reserve(mProjectBlocksLock->MissingBlocks.size());

    for (const auto& block : mProjectBlocksLock->MissingBlocks) {
        pendingBlocks.push_back(PendingProjectBlockData {
                response.Project.Id, response.Snapshot.Id, block.BlockUrls.UploadUrl,
                block.BlockUrls.SuccessUrl, block.BlockUrls.FailUrl, block.Block.Id,
                static_cast<int>(block.Block.Format), block.Block.Hash });
    }

    CloudProjectsDatabase::Get().AddPendingProjectBlocks(pendingBlocks);
}

void LocalProjectSnapshot::MarkSnapshotSynced()
{
    using namespace network_manager;
    Request request(mServiceConfig.GetSnapshotSyncUrl(
                        mCreateSnapshotResponse->Project.Id,
                        mCreateSnapshotResponse->Snapshot.Id));

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
            mCreateSnapshotResponse->Project.Id,
            mCreateSnapshotResponse->Snapshot.Id);

        if (response->getError() != NetworkError::NoError) {
            UploadFailed(DeduceUploadError(*response));
            return;
        }

        mCompleted.store(true, std::memory_order_release);
        mProjectCloudExtension.OnSyncCompleted(this, {}, mAudiocomTrace);
    });

    mCancellationContext->OnCancelled(response);
}

void LocalProjectSnapshot::DeleteSnapshot()
{
    if (!mCreateSnapshotResponse) {
        return;
    }

    using namespace network_manager;

    Request request(mServiceConfig.GetDeleteSnapshotUrl(
                        mCreateSnapshotResponse->Project.Id,
                        mCreateSnapshotResponse->Snapshot.Id));

    SetCommonHeaders(request);

    auto response = NetworkManager::GetInstance().doDelete(request);

    response->setRequestFinishedCallback(
        [this, response, strongThis = shared_from_this()](auto)
    {
        CloudProjectsDatabase::Get().RemovePendingSnapshot(
            mCreateSnapshotResponse->Project.Id,
            mCreateSnapshotResponse->Snapshot.Id);
    });
}
} // namespace audacity::cloud::audiocom::sync
