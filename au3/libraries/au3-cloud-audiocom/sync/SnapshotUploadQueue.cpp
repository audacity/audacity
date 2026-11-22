/*
* Audacity: A Digital Audio Editor
*/
#include "SnapshotUploadQueue.h"

#include <algorithm>
#include <cstring>
#include <future>
#include <iterator>
#include <unordered_map>

#include "../ServiceConfig.h"

#include "BlockHasher.h"
#include "CloudProjectsDatabase.h"
#include "DataUploader.h"
#include "MissingBlocksUploader.h"
#include "SnapshotReconcile.h"

#include "au3-basic-ui/BasicUI.h"
#include "au3-crypto/crypto/SHA256.h"
#include "au3-files/FileException.h"
#include "au3-import-export/ExportUtils.h"
#include "au3-network-manager/IResponse.h"
#include "au3-network-manager/NetworkManager.h"
#include "au3-network-manager/Request.h"
#include "au3-project-file-io/ProjectFileIO.h"
#include "au3-project/Project.h"
#include "au3-string-utils/StringUtils.h"
#include "au3-strings/TranslatableString.h"
#include "au3-track/Track.h"
#include "au3-utility/MemoryX.h"
#include "au3-wave-track/SampleBlock.h"
#include "au3-wave-track/WaveTrack.h"
#include "au3-wave-track/WaveTrackUtilities.h"

namespace audacity::cloud::audiocom::sync {
namespace {
constexpr auto TRACE = AudiocomTrace::SaveProjectSaveToCloudMenu;

std::vector<uint8_t> ReadSavedProjectDocument(ProjectFileIO& projectFileIO)
{
    std::vector<uint8_t> dict;
    std::vector<uint8_t> doc;

    if (!projectFileIO.ReadDoc("project", dict, doc)) {
        return {};
    }

    std::vector<uint8_t> data(sizeof(uint64_t) + dict.size() + doc.size());

    const uint64_t dictSize = dict.size();
    const uint64_t dictSizeData
        =IsLittleEndian() ? dictSize : SwapIntBytes(dictSize);

    std::memcpy(data.data(), &dictSizeData, sizeof(uint64_t));

    const auto docBegin = std::copy(dict.begin(), dict.end(), data.begin() + sizeof(uint64_t));
    std::copy(doc.begin(), doc.end(), docBegin);

    return data;
}

bool IsFatal(const CloudSyncError& error)
{
    return error.Type == CloudSyncError::ProjectVersionConflict
           || error.Type == CloudSyncError::ProjectNotFound;
}

CloudSyncError InvalidResponse(const std::string& body)
{
    return MakeClientFailure(TranslatableString("cloud-audiocom", "Invalid Response: %1").arg(body));
}

CloudSyncError LocalDataMissing()
{
    return MakeClientFailure(
        TranslatableString("cloud-audiocom", "Local project data was removed before the sync has completed"));
}
} // namespace

struct SnapshotUploadQueue::BlockCapture final : private BlockHashCache
{
    ProjectCloudExtension& Extension;

    SampleBlockIDSet BlockIds;
    std::vector<LockedBlock> Blocks;
    std::unordered_map<int64_t, size_t> BlockIdToIndex;

    std::unique_ptr<BlockHasher> Hasher;

    std::future<void> UpdateCacheFuture;
    std::vector<std::pair<int64_t, std::string> > NewHashes;

    std::function<void()> OnHashed;

    BlockCapture(ProjectCloudExtension& extension, AudacityProject& project, std::function<void()> onHashed)
        : Extension{extension}
        , OnHashed{std::move(onHashed)}
    {
        WaveTrackUtilities::VisitBlocks(
            TrackList::Get(project),
            [this](const SampleBlockPtr& block) {
            const auto id = block->GetBlockID();
            if (id >= 0) {
                Blocks.push_back({ id, block->GetSampleFormat(), block });
                BlockIdToIndex[id] = Blocks.size() - 1;
            }
        },
            &BlockIds);

        if (Extension.IsCloudProject()) {
            CloudProjectsDatabase::Get().UpdateProjectBlockList(Extension.GetCloudProjectId(), BlockIds);
        }

        Hasher = std::make_unique<BlockHasher>();
        Hasher->ComputeHashes(*this, Blocks, [this] { CollectHashes(); });
    }

    void CollectHashes()
    {
        std::unordered_map<std::string, size_t> usedHashes;

        for (auto [id, hash] : Hasher->TakeResult()) {
            const auto it = BlockIdToIndex.find(id);

            if (it == BlockIdToIndex.end()) {
                continue;
            }

            while (usedHashes.find(hash) != usedHashes.end()) {
                hash = crypto::sha256(hash);
            }

            usedHashes[hash] = it->second;
            Blocks[it->second].Hash = std::move(hash);
        }

        UpdateProjectHashesInCache();

        if (OnHashed) {
            OnHashed();
        }
    }

    void UpdateProjectHashesInCache()
    {
        if (!Extension.IsCloudProject()) {
            return;
        }

        UpdateCacheFuture = std::async(
            std::launch::async,
            [this, hashes = std::move(NewHashes)] {
            CloudProjectsDatabase::Get().UpdateBlockHashes(Extension.GetCloudProjectId(), hashes);
        });
    }

    bool GetHash(int64_t blockId, std::string& hash) const override
    {
        if (!Extension.IsCloudProject()) {
            return false;
        }

        auto cachedResult = CloudProjectsDatabase::Get().GetBlockHash(Extension.GetCloudProjectId(), blockId);

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
};

SnapshotUploadQueue::SnapshotUploadQueue(ProjectCloudExtension& extension)
    : mExtension{extension}
{
}

SnapshotUploadQueue::~SnapshotUploadQueue()
{
    if (mUploadContext) {
        mUploadContext->Cancel();
    }
}

bool SnapshotUploadQueue::IsEmpty() const
{
    return mSize.load() == 0;
}

bool SnapshotUploadQueue::Enqueue(std::string name, UploadMode mode)
{
    auto project = mExtension.GetProject().lock();

    if (!project) {
        return false;
    }

    auto blob = ReadSavedProjectDocument(ProjectFileIO::Get(*project));

    if (blob.empty()) {
        return false;
    }

    if (mode != UploadMode::Normal) {
        DropSuperseded();
    }

    Entry entry;
    entry.Id          = mNextEntryId++;
    entry.Name        = std::move(name);
    entry.Mode        = mode;
    entry.ProjectBlob = std::move(blob);

    entry.Capture = std::make_shared<BlockCapture>(
        mExtension, *project,
        [weakThis = weak_from_this(), entryId = entry.Id] {
        BasicUI::CallAfter([weakThis, entryId] {
            if (auto self = weakThis.lock()) {
                self->OnEntryHashed(entryId);
            }
        });
    });

    mEntries.push_back(std::move(entry));
    mSize.store(mEntries.size());

    mExtension.RecordSnapshotRequested();

    return true;
}

bool SnapshotUploadQueue::RestorePending()
{
    if (!mEntries.empty()) {
        return true;
    }

    const auto projectId = mExtension.GetCloudProjectId();

    if (projectId.empty()) {
        return false;
    }

    auto project = mExtension.GetProject().lock();

    if (!project) {
        return false;
    }

    auto& database = CloudProjectsDatabase::Get();
    const auto localHeadId = mExtension.GetSnapshotId();

    for (const auto& pending : database.GetPendingSnapshots(projectId)) {
        if (pending.SnapshotId != localHeadId || !mEntries.empty()) {
            database.RemovePendingSnapshot(projectId, pending.SnapshotId);
            continue;
        }

        Entry entry;
        entry.Id            = mNextEntryId++;
        entry.CurrentStage  = Stage::Created;
        entry.ProjectId     = projectId;
        entry.SnapshotId    = pending.SnapshotId;
        entry.PendingStored = true;

        if (auto blob = database.GetPendingProjectBlob(projectId, pending.SnapshotId)) {
            entry.ProjectBlob = std::move(blob->BlobData);
        }

        auto& sampleBlockFactory = WaveTrackFactory::Get(*project).GetSampleBlockFactory();

        try
        {
            for (const auto& block : database.GetPendingProjectBlocks(projectId, pending.SnapshotId)) {
                const auto format = static_cast<sampleFormat>(block.BlockSampleFormat);
                entry.Blocks.push_back(
                    { block.BlockId, format, sampleBlockFactory->CreateFromId(format, block.BlockId), block.BlockHash });
            }
        }
        catch (const FileException&)
        {
            database.RemovePendingSnapshot(projectId, pending.SnapshotId);
            continue;
        }

        mEntries.push_back(std::move(entry));
    }

    mSize.store(mEntries.size());

    return !mEntries.empty();
}

void SnapshotUploadQueue::Run()
{
    mPaused = false;
    mPauseError.reset();

    Drive();
    PublishStatus();
}

void SnapshotUploadQueue::Pause()
{
    if (mEntries.empty()) {
        return;
    }

    mPaused     = true;
    mPauseError = CloudSyncError { CloudSyncError::Cancelled };

    AbortUploadStep();
    PublishStatus();
}

SnapshotUploadQueue::Entry* SnapshotUploadQueue::FindEntry(uint64_t entryId)
{
    const auto it = std::find_if(
        mEntries.begin(), mEntries.end(),
        [entryId](const Entry& entry) { return entry.Id == entryId; });

    return it != mEntries.end() ? &*it : nullptr;
}

void SnapshotUploadQueue::OnEntryHashed(uint64_t entryId)
{
    auto entry = FindEntry(entryId);

    if (!entry || entry->CurrentStage != Stage::Hashing) {
        return;
    }

    entry->Blocks       = entry->Capture->Blocks;
    entry->CurrentStage = Stage::ReadyToCreate;

    Drive();
}

void SnapshotUploadQueue::Drive()
{
    if (mStepInFlight || mPaused || mEntries.empty()) {
        return;
    }

    auto& head = mEntries.front();

    switch (head.CurrentStage) {
    case Stage::Hashing:
    case Stage::CreateSent:
        break;
    case Stage::ReadyToCreate:
        SendCreate(head);
        break;
    case Stage::CreateResultUnknown:
        Reconcile(head);
        break;
    case Stage::Created:
        StartUpload(head);
        break;
    }
}

void SnapshotUploadQueue::SendCreate(Entry& entry)
{
    const bool createNew = entry.Mode == UploadMode::CreateNew || !mExtension.IsCloudProject();

    ProjectForm form;

    if (createNew) {
        form.Name = entry.Name;
    } else {
        form.HeadSnapshotId = mExtension.GetSnapshotId();
    }

    form.Force = !createNew && entry.Mode == UploadMode::ForceOverwrite;

    form.Hashes.reserve(entry.Blocks.size());
    std::transform(
        entry.Blocks.begin(), entry.Blocks.end(), std::back_inserter(form.Hashes),
        [](const LockedBlock& block) { return block.Hash; });

    using namespace network_manager;

    const auto& config = GetServiceConfig();

    Request request(
        createNew ? config.GetCreateProjectUrl() : config.GetCreateSnapshotUrl(mExtension.GetCloudProjectId()));

    request.setHeader(common_headers::ContentType, common_content_types::ApplicationJson);
    request.setHeader(common_headers::Accept, common_content_types::ApplicationJson);

    SetCommonHeaders(request);
    SetOptionalHeaders(request);

    const auto serializedForm = Serialize(form);

    entry.CurrentStage = Stage::CreateSent;
    mStepInFlight      = true;

    auto response = NetworkManager::GetInstance().doPost(request, serializedForm.data(), serializedForm.size());

    response->setRequestFinishedCallback(
        [weakThis = weak_from_this(), response, createNew](auto) {
        BasicUI::CallAfter([weakThis, response, createNew] {
            auto self = weakThis.lock();

            if (!self || self->mEntries.empty()) {
                return;
            }

            auto& head = self->mEntries.front();
            self->mStepInFlight = false;

            if (response->getError() != NetworkError::NoError) {
                auto error = DeduceUploadError(*response);

                if (error.Type == CloudSyncError::Network && !createNew) {
                    self->OnCreateResultUnknown(std::move(error));
                    return;
                }

                head.CurrentStage = Stage::ReadyToCreate;
                self->Fail(std::move(error));
                return;
            }

            const auto body = response->readAll<std::string>();
            auto result     = DeserializeCreateSnapshotResponse(body);

            if (!result) {
                head.CurrentStage = Stage::ReadyToCreate;
                self->Fail(InvalidResponse(body));
                return;
            }

            self->OnCreated(head, result->Project, result->Snapshot, createNew);
            head.SyncState = std::move(result->SyncState);

            self->Drive();
            self->PublishStatus();
        });
    });
}

void SnapshotUploadQueue::OnCreated(
    Entry& entry, const ProjectInfo& project, const SnapshotInfo& snapshot, bool newProject)
{
    entry.CurrentStage = Stage::Created;
    entry.ProjectId    = project.Id;
    entry.SnapshotId   = snapshot.Id;

    mExtension.RecordSnapshotCreated(project, snapshot);

    if (newProject && entry.Capture) {
        entry.Capture->UpdateProjectHashesInCache();
    }
}

void SnapshotUploadQueue::OnCreateResultUnknown(CloudSyncError error)
{
    mEntries.front().CurrentStage = Stage::CreateResultUnknown;
    Suspend(std::move(error));
}

void SnapshotUploadQueue::Reconcile(Entry& entry)
{
    entry.CurrentStage = Stage::CreateSent;
    mStepInFlight      = true;

    const auto& config   = GetServiceConfig();
    const auto projectId = mExtension.GetCloudProjectId();

    const ErrorHandler onError = [this](CloudSyncError error) {
        mStepInFlight = false;
        OnCreateResultUnknown(std::move(error));
    };

    Get(
        config.GetProjectInfoUrl(projectId),
        [this, projectId, onError](const std::string& projectBody) {
        auto project = DeserializeProjectInfo(projectBody);

        if (!project) {
            onError(InvalidResponse(projectBody));
            return;
        }

        Get(
            GetServiceConfig().GetSnapshotInfoUrl(projectId, project->HeadSnapshot.Id),
            [this, project = std::move(*project), onError](const std::string& snapshotBody) {
            const auto serverHead = DeserializeSnapshotInfo(snapshotBody);

            if (!serverHead) {
                onError(InvalidResponse(snapshotBody));
                return;
            }

            auto& head = mEntries.front();
            mStepInFlight = false;

            SampleBlockHashes hashes;
            hashes.reserve(head.Blocks.size());
            std::transform(
                head.Blocks.begin(), head.Blocks.end(), std::back_inserter(hashes),
                [](const LockedBlock& block) { return block.Hash; });

            switch (DecideReconcile(mExtension.GetSnapshotId(), *serverHead, hashes)) {
                case ReconcileDecision::Resend:
                    head.CurrentStage = Stage::ReadyToCreate;
                    break;
                case ReconcileDecision::Adopt:
                    OnCreated(head, project, *serverHead, false);
                    break;
                case ReconcileDecision::Diverged:
                    DropAll({ CloudSyncError::ProjectVersionConflict });
                    return;
            }

            Drive();
            PublishStatus();
        },
            onError);
    },
        onError);
}

void SnapshotUploadQueue::Get(const std::string& url, BodyHandler onBody, ErrorHandler onError)
{
    using namespace network_manager;

    Request request(url);

    SetCommonHeaders(request);
    SetOptionalHeaders(request);

    auto response = NetworkManager::GetInstance().doGet(request);

    response->setRequestFinishedCallback(
        [weakThis = weak_from_this(), response, onBody = std::move(onBody), onError = std::move(onError)](auto) {
        BasicUI::CallAfter([weakThis, response, onBody, onError] {
            auto self = weakThis.lock();

            if (!self || self->mEntries.empty()) {
                return;
            }

            if (response->getError() != NetworkError::NoError) {
                onError(DeduceUploadError(*response));
                return;
            }

            onBody(response->readAll<std::string>());
        });
    });
}

void SnapshotUploadQueue::StartUpload(Entry& entry)
{
    mStepInFlight  = true;
    mUploadContext = concurrency::CancellationContext::Create();

    const auto token = ++mUploadToken;

    if (entry.SyncState) {
        const auto state = std::move(*entry.SyncState);
        entry.SyncState.reset();
        UploadWithState(token, state);
        return;
    }

    Get(
        GetServiceConfig().GetSnapshotSyncUrl(entry.ProjectId, entry.SnapshotId),
        [this, token](const std::string& body) {
        if (token != mUploadToken) {
            return;
        }

        const auto state = DeserializeProjectSyncState(body);

        if (!state) {
            OnUploadFailed(token, InvalidResponse(body));
            return;
        }

        UploadWithState(token, *state);
    },
        [this, token](CloudSyncError error) { OnUploadFailed(token, std::move(error)); });
}

void SnapshotUploadQueue::StorePending(Entry& entry, const ProjectSyncState& state)
{
    auto& database = CloudProjectsDatabase::Get();

    database.AddPendingSnapshot(
        { entry.ProjectId, entry.SnapshotId,
          GetServiceConfig().GetSnapshotSyncUrl(entry.ProjectId, entry.SnapshotId) });

    if (!state.FileUrls.UploadUrl.empty()) {
        database.AddPendingProjectBlob(
            { entry.ProjectId, entry.SnapshotId, state.FileUrls.UploadUrl, state.FileUrls.SuccessUrl,
              state.FileUrls.FailUrl, entry.ProjectBlob });
    }

    std::unordered_map<std::string, const UploadUrls*> missing;
    for (const auto& urls : state.MissingBlocks) {
        missing.emplace(ToUpper(urls.Id), &urls);
    }

    std::vector<PendingProjectBlockData> pendingBlocks;

    for (const auto& block : entry.Blocks) {
        const auto it = missing.find(ToUpper(block.Hash));

        if (it == missing.end()) {
            continue;
        }

        pendingBlocks.push_back(
            { entry.ProjectId, entry.SnapshotId, it->second->UploadUrl, it->second->SuccessUrl, it->second->FailUrl,
              block.Id, static_cast<int>(block.Format), block.Hash });
    }

    if (!pendingBlocks.empty()) {
        database.AddPendingProjectBlocks(pendingBlocks);
    }

    entry.PendingStored = true;
}

void SnapshotUploadQueue::UploadWithState(uint64_t token, const ProjectSyncState& state)
{
    auto& head = mEntries.front();

    if (!head.PendingStored) {
        StorePending(head, state);
    }

    std::unordered_map<std::string, const LockedBlock*> blocksByHash;
    for (const auto& block : head.Blocks) {
        blocksByHash.emplace(ToUpper(block.Hash), &block);
    }

    std::vector<BlockUploadTask> tasks;
    tasks.reserve(state.MissingBlocks.size());

    for (const auto& urls : state.MissingBlocks) {
        const auto it = blocksByHash.find(ToUpper(urls.Id));

        if (it == blocksByHash.end()) {
            CloudProjectsDatabase::Get().RemovePendingSnapshot(head.ProjectId, head.SnapshotId);
            DropAll(LocalDataMissing());
            return;
        }

        tasks.push_back({ urls, *it->second });
    }

    const bool fileNeeded = !state.FileUrls.UploadUrl.empty();

    if (fileNeeded && head.ProjectBlob.empty()) {
        CloudProjectsDatabase::Get().RemovePendingSnapshot(head.ProjectId, head.SnapshotId);
        DropAll(LocalDataMissing());
        return;
    }

    head.BlocksTotal   = tasks.size();
    head.BlocksHandled = 0;
    head.FileUploaded  = !fileNeeded;

    PublishStatus();

    if (!fileNeeded) {
        UploadBlocks(token, std::move(tasks));
        return;
    }

    DataUploader::Get().Upload(
        mUploadContext, GetServiceConfig(), state.FileUrls, head.ProjectBlob,
        [weakThis = weak_from_this(), token, tasks = std::move(tasks)](ResponseResult result) {
        BasicUI::CallAfter([weakThis, token, tasks, result] {
            auto self = weakThis.lock();

            if (!self || token != self->mUploadToken) {
                return;
            }

            if (result.Code != SyncResultCode::Success) {
                self->OnUploadFailed(token, { DeduceError(result.Code), result.Content });
                return;
            }

            auto& head = self->mEntries.front();
            head.FileUploaded = true;
            CloudProjectsDatabase::Get().RemovePendingProjectBlob(head.ProjectId, head.SnapshotId);

            self->PublishStatus();
            self->UploadBlocks(token, tasks);
        });
    });
}

void SnapshotUploadQueue::UploadBlocks(uint64_t token, std::vector<BlockUploadTask> tasks)
{
    if (tasks.empty()) {
        Confirm(token);
        return;
    }

    mBlocksUploader = MissingBlocksUploader::Create(
        mUploadContext, GetServiceConfig(), std::move(tasks),
        [weakThis = weak_from_this(), token](
            const MissingBlocksUploadProgress& progress, const LockedBlock& block, ResponseResult result) {
        const bool completed = progress.UploadedBlocks == progress.TotalBlocks || progress.FailedBlocks != 0;
        const bool succeeded = completed && progress.FailedBlocks == 0;

        BasicUI::CallAfter([weakThis, token, completed, succeeded, blockId = block.Id, result] {
            auto self = weakThis.lock();

            if (!self || token != self->mUploadToken) {
                return;
            }

            auto& head = self->mEntries.front();

            if (result.Code == SyncResultCode::Success) {
                CloudProjectsDatabase::Get().RemovePendingProjectBlock(head.ProjectId, blockId);
            }

            head.BlocksHandled++;
            self->PublishStatus();

            if (!completed) {
                return;
            }

            if (succeeded) {
                self->mBlocksUploader.reset();
                self->Confirm(token);
                return;
            }

            const auto type = result.Code == SyncResultCode::Conflict
                              ? CloudSyncError::DataUploadFailed
                              : DeduceError(result.Code);

            self->OnUploadFailed(token, { type, result.Content });
        });
    });
}

void SnapshotUploadQueue::Confirm(uint64_t token)
{
    using namespace network_manager;

    const auto& head = mEntries.front();

    Request request(GetServiceConfig().GetSnapshotSyncUrl(head.ProjectId, head.SnapshotId));

    SetCommonHeaders(request);
    SetOptionalHeaders(request);

    auto response = NetworkManager::GetInstance().doPost(request, nullptr, 0);

    response->setRequestFinishedCallback(
        [weakThis = weak_from_this(), response, token](auto) {
        BasicUI::CallAfter([weakThis, response, token] {
            auto self = weakThis.lock();

            if (!self || token != self->mUploadToken) {
                return;
            }

            if (response->getError() != NetworkError::NoError) {
                self->OnUploadFailed(token, DeduceUploadError(*response));
                return;
            }

            self->OnConfirmed();
        });
    });

    mUploadContext->OnCancelled(response);
}

void SnapshotUploadQueue::OnConfirmed()
{
    const auto& head = mEntries.front();

    CloudProjectsDatabase::Get().RemovePendingSnapshot(head.ProjectId, head.SnapshotId);

    mStepInFlight = false;
    mBlocksUploader.reset();

    mEntries.pop_front();
    mSize.store(mEntries.size());

    if (!mEntries.empty()) {
        Drive();
        PublishStatus();
        return;
    }

    mExtension.MarkProjectSynced(DBProjectData::SyncStatusSynced);
    mExtension.Publish({ TRACE, ProjectSyncStatus::Synced }, false);
}

void SnapshotUploadQueue::AbortUploadStep()
{
    if (mEntries.empty() || mEntries.front().CurrentStage != Stage::Created || !mStepInFlight) {
        return;
    }

    ++mUploadToken;
    mUploadContext->Cancel();
    mBlocksUploader.reset();
    mStepInFlight = false;
}

void SnapshotUploadQueue::OnUploadFailed(uint64_t token, CloudSyncError error)
{
    if (token != mUploadToken) {
        return;
    }

    AbortUploadStep();

    if (IsFatal(error)) {
        const auto& head = mEntries.front();
        CloudProjectsDatabase::Get().RemovePendingSnapshot(head.ProjectId, head.SnapshotId);
    }

    Fail(std::move(error));
}

void SnapshotUploadQueue::Suspend(CloudSyncError error)
{
    mPaused     = true;
    mPauseError = std::move(error);

    PublishStatus();
}

void SnapshotUploadQueue::Fail(CloudSyncError error)
{
    if (IsFatal(error)) {
        DropAll(std::move(error));
    } else {
        Suspend(std::move(error));
    }
}

void SnapshotUploadQueue::DropAll(CloudSyncError error)
{
    AbortUploadStep();

    mStepInFlight = false;
    mPaused       = false;
    mPauseError.reset();

    mEntries.clear();
    mSize.store(0);

    mExtension.MarkProjectSynced(DBProjectData::SyncStatusPendingSnapshot);
    mExtension.Publish({ TRACE, ProjectSyncStatus::Failed, {}, std::move(error) }, false);
}

void SnapshotUploadQueue::DropSuperseded()
{
    AbortUploadStep();

    auto& database = CloudProjectsDatabase::Get();

    for (const auto& entry : mEntries) {
        if (entry.CurrentStage == Stage::Created) {
            database.RemovePendingSnapshot(entry.ProjectId, entry.SnapshotId);
        }
    }

    std::erase_if(mEntries, [](const Entry& entry) { return entry.CurrentStage != Stage::CreateSent; });
    mSize.store(mEntries.size());
}

void SnapshotUploadQueue::PublishStatus()
{
    if (mEntries.empty()) {
        return;
    }

    if (mPaused) {
        mExtension.Publish({ TRACE, ProjectSyncStatus::Failed, {}, mPauseError }, false);
        return;
    }

    int64_t handled = 0;
    int64_t total   = 0;

    for (const auto& entry : mEntries) {
        handled += entry.BlocksHandled + int(entry.FileUploaded);
        total   += entry.BlocksTotal + 1;
    }

    mExtension.Publish(
        { AudiocomTrace::ignore, ProjectSyncStatus::Syncing, double(handled) / double(total) }, true);
}
} // namespace audacity::cloud::audiocom::sync
