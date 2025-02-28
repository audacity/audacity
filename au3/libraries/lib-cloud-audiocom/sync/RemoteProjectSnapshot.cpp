/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  RemoteProjectSnapshot.cpp

  Dmitry Vedenko

**********************************************************************/
#include "RemoteProjectSnapshot.h"

#include <algorithm>
#include <unordered_set>

#include <wx/datetime.h>

#include "CloudProjectsDatabase.h"

#include "CodeConversions.h"
#include "Internat.h"
#include "MemoryX.h"
#include "StringUtils.h"

#include "IResponse.h"
#include "NetworkManager.h"
#include "Request.h"

#include "WavPackCompressor.h"

namespace audacity::cloud::audiocom::sync {
namespace {
std::vector<std::string> ListAttachedDatabases()
{
    auto db        = CloudProjectsDatabase::Get().GetConnection();
    auto statement = db->CreateStatement("PRAGMA database_list");
    auto result    = statement->Prepare().Run();

    std::vector<std::string> attachedDBs;

    for (auto row : result) {
        std::string dbName;

        if (!row.Get(1, dbName)) {
            continue;
        }

        if (dbName == "main" || dbName == "temp") {
            continue;
        }

        attachedDBs.push_back(std::move(dbName));
    }

    return attachedDBs;
}
} // namespace

RemoteProjectSnapshot::RemoteProjectSnapshot(
    Tag, ProjectInfo projectInfo, SnapshotInfo snapshotInfo, std::string path,
    RemoteProjectSnapshotStateCallback callback, bool downloadDetached)
    : mSnapshotDBName{std::string("s_") + projectInfo.Id}
    , mProjectInfo{std::move(projectInfo)}
    , mSnapshotInfo{std::move(snapshotInfo)}
    , mPath{std::move(path)}
    , mCallback{std::move(callback)}
    , mDownloadDetached{downloadDetached}
{
    auto db = CloudProjectsDatabase::Get().GetConnection();
    // RemoteProjectSnapshot always receives a path to the database
    // that has AudacityProject schema installed, even if it's a detached
    // or was deleted from the disk before
    auto attachStmt = db->CreateStatement("ATTACH DATABASE ? AS ?");
    auto result     = attachStmt->Prepare(mPath, mSnapshotDBName).Run();

    if (!result.IsOk()) {
        return;
    }

    mAttachedDBNames.push_back(mSnapshotDBName);

    auto blocksSource = mSnapshotDBName;

    if (mDownloadDetached) {
        if (auto name = AttachOriginalDB(); !name.empty()) {
            blocksSource = name;
        }
    }

    // This would return and empty set when the project
    // is detached
    auto knownBlocks = CalculateKnownBlocks(blocksSource);

    if (mDownloadDetached) {
        // We can assume, that if the known blocks are present,
        // they come from the "original" database
        SetupBlocksCopy(blocksSource, knownBlocks);
    } else if (knownBlocks.size() == mSnapshotInfo.Blocks.size()) {
        auto syncInfo
            =CloudProjectsDatabase::Get().GetProjectData(mProjectInfo.Id);

        if (
            syncInfo && syncInfo->SnapshotId == mSnapshotInfo.Id
            && syncInfo->SyncStatus == DBProjectData::SyncStatusSynced) {
            mCallback({ {}, 0, 0, true });
            mNothingToDo = true;
            return;
        }
    }

    if (!mDownloadDetached) {
        CleanupOrphanBlocks();
    }

    MarkProjectInDB(false);

    mMissingBlocks = mDownloadDetached
                     ? mSnapshotInfo.Blocks.size()
                     : mSnapshotInfo.Blocks.size() - knownBlocks.size();

    mRequests.reserve(1 + mMissingBlocks);

    mRequests.push_back(std::make_pair(
                            mSnapshotInfo.FileUrl,
                            [this](auto response) { OnProjectBlobDownloaded(response); }));

    for (auto& block : mSnapshotInfo.Blocks) {
        if (knownBlocks.find(ToUpper(block.Hash)) != knownBlocks.end()) {
            continue;
        }

        mRequests.push_back(std::make_pair(
                                block.Url, [this, hash = ToUpper(block.Hash)](auto response)
        { OnBlockDownloaded(std::move(hash), response); }));
    }

    mRequestsThread
        =std::thread { &RemoteProjectSnapshot::RequestsThread, this };
}

RemoteProjectSnapshot::~RemoteProjectSnapshot()
{
    DoCancel();

    if (mRequestsThread.joinable()) {
        mRequestsThread.join();
    }

    if (mCopyBlocksFuture.has_value()) {
        mCopyBlocksFuture->wait();
    }

    {
        auto lock = std::unique_lock { mResponsesMutex };
        mResponsesEmptyCV.wait(lock, [this] { return mResponses.empty(); });
    }

    auto db = CloudProjectsDatabase::Get().GetConnection();

    for (const auto& dbName : ListAttachedDatabases()) {
        auto detachStmt = db->CreateStatement("DETACH DATABASE ?");
        detachStmt->Prepare(dbName).Run();
    }
}

std::shared_ptr<RemoteProjectSnapshot> RemoteProjectSnapshot::Sync(
    ProjectInfo projectInfo, SnapshotInfo snapshotInfo, std::string path,
    RemoteProjectSnapshotStateCallback callback, bool downloadDetached)
{
    auto snapshot = std::make_shared<RemoteProjectSnapshot>(
        Tag {}, std::move(projectInfo), std::move(snapshotInfo), std::move(path),
        std::move(callback), downloadDetached);

    if (snapshot->mAttachedDBNames.empty()) {
        snapshot->mCallback(
            { { SyncResultCode::InternalClientError,
                audacity::ToUTF8(
                    XO("Failed to attach to the Cloud project database")
                    .Translation()) },
                0,
                0,
                false });

        return {};
    }

    if (snapshot->mNothingToDo) {
        return {}
    }

    return snapshot;
}

void RemoteProjectSnapshot::Cancel()
{
    DoCancel();

    mCallback({ { SyncResultCode::Cancelled, {} },
                  mDownloadedBlocks.load() + mCopiedBlocks.load(),
                  mMissingBlocks,
                  mProjectDownloaded.load() });
}

TransferStats RemoteProjectSnapshot::GetTransferStats() const
{
    const auto duration
        =mState.load(std::memory_order_acquire) == State::Downloading
          ? Clock::now() - mStartTime
          : mEndTime - mStartTime;

    return TransferStats {}
           .SetBlocksTransferred(mDownloadedBlocks.load())
           .SetBytesTransferred(mDownloadedBytes.load())
           .SetProjectFilesTransferred(mProjectDownloaded.load() ? 1 : 0)
           .SetTransferDuration(
        std::chrono::duration_cast<TransferStats::Duration>(duration));
}

std::string_view RemoteProjectSnapshot::GetProjectId() const
{
    return mProjectInfo.Id;
}

std::string RemoteProjectSnapshot::AttachOriginalDB()
{
    const std::string dbName = "o_" + mProjectInfo.Id;

    const auto projectData
        =CloudProjectsDatabase::Get().GetProjectData(mProjectInfo.Id);

    if (!projectData) {
        return {}
    }

    auto db = CloudProjectsDatabase::Get().GetConnection();
    // RemoteProjectSnapshot always receives a path to the database
    // that has AudacityProject schema installed, even if it's a detached
    // or was deleted from the disk before
    auto attachStmt = db->CreateStatement("ATTACH DATABASE ? AS ?");
    auto result     = attachStmt->Prepare(projectData->LocalPath, dbName).Run();

    if (!result.IsOk()) {
        return {}
    }

    mAttachedDBNames.push_back(dbName);

    return dbName;
}

void RemoteProjectSnapshot::SetupBlocksCopy(
    const std::string& dbName, std::unordered_set<std::string> blocks)
{
    // Still, better be safe than sorry
    if (dbName == mSnapshotDBName) {
        return;
    }

    if (blocks.empty()) {
        return;
    }

    mCopyBlocksFuture = std::async(
        std::launch::async,
        [this, dbName = dbName, blocks = std::move(blocks)]()
    {
        const auto queryString
            ="INSERT INTO " + mSnapshotDBName
              + ".sampleblocks "
                "SELECT * FROM "
              + dbName
              + ".sampleblocks WHERE blockid IN (SELECT block_id FROM block_hashes WHERE hash = ?)";

        // Only lock DB for one block a time so the download thread can
        // continue to work
        for (const auto& block : blocks) {
            if (!InProgress()) {
                return false;
            }

            auto db = CloudProjectsDatabase::Get().GetConnection();

            auto copyBlocksStatement = db->CreateStatement(queryString);

            if (!copyBlocksStatement) {
                OnFailure({ SyncResultCode::InternalClientError,
                            audacity::ToUTF8(copyBlocksStatement.GetError()
                                             .GetErrorString()
                                             .Translation()) });

                return false;
            }

            auto result = copyBlocksStatement->Prepare(block).Run();

            if (!result.IsOk()) {
                OnFailure({ SyncResultCode::InternalClientError,
                            audacity::ToUTF8(result.GetErrors()
                                             .front()
                                             .GetErrorString()
                                             .Translation()) });
                return false;
            }

            const auto rowsUpdated = result.GetModifiedRowsCount();
            mCopiedBlocks.fetch_add(rowsUpdated, std::memory_order_acq_rel);

            ReportProgress();
        }

        return true;
    });
}

std::unordered_set<std::string> RemoteProjectSnapshot::CalculateKnownBlocks(
    const std::string& attachedDbName) const
{
    std::unordered_set<std::string> remoteBlocks;

    for (const auto& block : mSnapshotInfo.Blocks) {
        remoteBlocks.insert(ToUpper(block.Hash));
    }

    auto db = CloudProjectsDatabase::Get().GetConnection();

    auto fn = db->CreateScalarFunction(
        "inRemoteBlocks", [&remoteBlocks](const std::string& hash)
    { return remoteBlocks.find(hash) != remoteBlocks.end(); });

    auto statement = db->CreateStatement(
        "SELECT hash FROM block_hashes WHERE project_id = ? AND inRemoteBlocks(hash) AND block_id IN (SELECT blockid FROM "
        + attachedDbName + ".sampleblocks)");

    if (!statement) {
        return {}
    }

    auto result = statement->Prepare(mProjectInfo.Id).Run();

    std::unordered_set<std::string> knownBlocks;

    for (auto row : result) {
        std::string hash;

        if (!row.Get(0, hash)) {
            continue;
        }

        knownBlocks.insert(hash);
    }

    return knownBlocks;
}

void RemoteProjectSnapshot::DoCancel()
{
    if (mState.load(std::memory_order_acquire) != State::Downloading) {
        return;
    }

    SetState(State::Cancelled);

    mRequestsCV.notify_one();

    {
        auto responsesLock = std::lock_guard { mResponsesMutex };
        for (auto& response : mResponses) {
            response->abort();
        }
    }
}

void RemoteProjectSnapshot::DownloadBlob(
    std::string url, SuccessHandler onSuccess, int retries)
{
    using namespace audacity::network_manager;

    auto request = Request(url);

    auto response = NetworkManager::GetInstance().doGet(request);

    {
        auto responsesLock = std::lock_guard { mResponsesMutex };
        mResponses.push_back(response);
    }

    response->setRequestFinishedCallback(
        [this, self = weak_from_this(), onSuccess = std::move(onSuccess), retries, response](auto)
    {
        auto strong = self.lock();
        if (!strong) {
            return;
        }

        mDownloadedBytes.fetch_add(
            response->getBytesAvailable(), std::memory_order_acq_rel);

        RemoveResponse(response.get());

        auto responseResult = GetResponseResult(*response, false);

        if (responseResult.Code == SyncResultCode::Cancelled) {
            return;
        }

        if (
            responseResult.Code != SyncResultCode::Success
            && responseResult.Code != SyncResultCode::ConnectionFailed) {
            OnFailure(std::move(responseResult));
            return;
        }

        if (responseResult.Code == SyncResultCode::ConnectionFailed) {
            if (retries <= 0) {
                OnFailure(std::move(responseResult));
                return;
            }

            DownloadBlob(
                response->getRequest().getURL(), std::move(onSuccess),
                retries - 1);

            return;
        }

        onSuccess(response);
    });
}

namespace {
std::vector<uint8_t>
ReadResponseData(audacity::network_manager::IResponse& response)
{
    const auto size = response.getBytesAvailable();

    if (size == 0) {
        return response.readAll<std::vector<uint8_t> >();
    }

    std::vector<uint8_t> data(size);
    response.readData(data.data(), size);

    return data;
}
} // namespace

void RemoteProjectSnapshot::OnProjectBlobDownloaded(
    audacity::network_manager::ResponsePtr response)
{
    const std::vector<uint8_t> data = ReadResponseData(*response);
    uint64_t dictSize               = 0;

    if (data.size() < sizeof(uint64_t)) {
        OnFailure({ SyncResultCode::UnexpectedResponse, {} });
        return;
    }

    std::memcpy(&dictSize, data.data(), sizeof(uint64_t));

    if (!IsLittleEndian()) {
        dictSize = SwapIntBytes(dictSize);
    }

    if (data.size() < sizeof(uint64_t) + dictSize) {
        OnFailure({ SyncResultCode::UnexpectedResponse, {} });
        return;
    }

    auto db          = CloudProjectsDatabase::Get().GetConnection();
    auto transaction = db->BeginTransaction("p_" + mProjectInfo.Id);

    auto updateProjectStatement = db->CreateStatement(
        "INSERT INTO " + mSnapshotDBName
        + ".project (id, dict, doc) VALUES (1, ?1, ?2) "
          "ON CONFLICT(id) DO UPDATE SET dict = ?1, doc = ?2");

    if (!updateProjectStatement) {
        OnFailure({ SyncResultCode::InternalClientError,
                    audacity::ToUTF8(updateProjectStatement.GetError()
                                     .GetErrorString()
                                     .Translation()) });
        return;
    }

    auto& preparedUpdateProjectStatement = updateProjectStatement->Prepare();

    preparedUpdateProjectStatement.Bind(
        1, data.data() + sizeof(uint64_t), dictSize, false);

    preparedUpdateProjectStatement.Bind(
        2, data.data() + sizeof(uint64_t) + dictSize,
        data.size() - sizeof(uint64_t) - dictSize, false);

    auto result = preparedUpdateProjectStatement.Run();

    if (!result.IsOk()) {
        OnFailure(
            { SyncResultCode::InternalClientError,
              audacity::ToUTF8(
                  result.GetErrors().front().GetErrorString().Translation()) });

        return;
    }

    auto deleteAutosaveStatement = db->CreateStatement(
        "DELETE FROM " + mSnapshotDBName + ".autosave WHERE id = 1");

    if (!deleteAutosaveStatement) {
        OnFailure({ SyncResultCode::InternalClientError,
                    audacity::ToUTF8(deleteAutosaveStatement.GetError()
                                     .GetErrorString()
                                     .Translation()) });
        return;
    }

    result = deleteAutosaveStatement->Prepare().Run();

    if (!result.IsOk()) {
        OnFailure(
            { SyncResultCode::InternalClientError,
              audacity::ToUTF8(
                  result.GetErrors().front().GetErrorString().Translation()) });
        return;
    }

    if (auto error = transaction.Commit(); error.IsError()) {
        OnFailure({ SyncResultCode::InternalClientError,
                    audacity::ToUTF8(error.GetErrorString().Translation()) });
        return;
    }

    mProjectDownloaded.store(true, std::memory_order_release);
    ReportProgress();
}

void RemoteProjectSnapshot::OnBlockDownloaded(
    std::string blockHash, audacity::network_manager::ResponsePtr response)
{
    const auto compressedData = ReadResponseData(*response);

    const auto blockData
        =DecompressBlock(compressedData.data(), compressedData.size());

    if (!blockData) {
        OnFailure(
            { SyncResultCode::InternalClientError,
              audacity::ToUTF8(XO("Failed to decompress the Cloud project block")
                               .Translation()) });
        return;
    }

    auto db          = CloudProjectsDatabase::Get().GetConnection();
    auto transaction = db->BeginTransaction("b_" + blockHash);

    auto hashesStatement = db->CreateStatement(
        "INSERT INTO block_hashes (project_id, block_id, hash) VALUES (?1, ?2, ?3) "
        "ON CONFLICT(project_id, block_id) DO UPDATE SET hash = ?3");

    auto result
        =hashesStatement->Prepare(mProjectInfo.Id, blockData->BlockId, blockHash)
          .Run();

    if (!result.IsOk()) {
        OnFailure(
            { SyncResultCode::InternalClientError,
              audacity::ToUTF8(
                  result.GetErrors().front().GetErrorString().Translation()) });
        return;
    }

    auto blockStatement = db->CreateStatement(
        "INSERT INTO " + mSnapshotDBName
        + ".sampleblocks (blockid, sampleformat, summin, summax, sumrms, summary256, summary64k, samples) VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8) "
          "ON CONFLICT(blockid) DO UPDATE SET sampleformat = ?2, summin = ?3, summax = ?4, sumrms = ?5, summary256 = ?6, summary64k = ?7, samples = ?8");

    if (!blockStatement) {
        OnFailure(
            { SyncResultCode::InternalClientError,
              audacity::ToUTF8(
                  blockStatement.GetError().GetErrorString().Translation()) });
        return;
    }

    auto& preparedStatement = blockStatement->Prepare();

    preparedStatement.Bind(1, blockData->BlockId);
    preparedStatement.Bind(2, static_cast<int64_t>(blockData->Format));
    preparedStatement.Bind(3, blockData->BlockMinMaxRMS.Min);
    preparedStatement.Bind(4, blockData->BlockMinMaxRMS.Max);
    preparedStatement.Bind(5, blockData->BlockMinMaxRMS.RMS);
    preparedStatement.Bind(
        6, blockData->Summary256.data(),
        blockData->Summary256.size() * sizeof(MinMaxRMS), false);
    preparedStatement.Bind(
        7, blockData->Summary64k.data(),
        blockData->Summary64k.size() * sizeof(MinMaxRMS), false);
    preparedStatement.Bind(
        8, blockData->Data.data(), blockData->Data.size(), false);

    result = preparedStatement.Run();

    if (!result.IsOk()) {
        OnFailure(
            { SyncResultCode::InternalClientError,
              audacity::ToUTF8(
                  result.GetErrors().front().GetErrorString().Translation()) });
        return;
    }

    if (auto error = transaction.Commit(); error.IsError()) {
        OnFailure({ SyncResultCode::InternalClientError,
                    audacity::ToUTF8(error.GetErrorString().Translation()) });
        return;
    }

    mDownloadedBlocks.fetch_add(1, std::memory_order_acq_rel);

    ReportProgress();
}

void RemoteProjectSnapshot::OnFailure(ResponseResult result)
{
    SetState(State::Failed);
    mCallback({ result,
                mDownloadedBlocks.load(std::memory_order_acquire)
                + mCopiedBlocks.load(std::memory_order_acquire),
                mMissingBlocks,
                mProjectDownloaded.load(std::memory_order_acquire) });
}

void RemoteProjectSnapshot::RemoveResponse(
    audacity::network_manager::IResponse* response)
{
    {
        auto lock = std::lock_guard { mResponsesMutex };
        mResponses.erase(
            std::remove_if(
                mResponses.begin(), mResponses.end(),
                [response](auto& r) { return r.get() == response; }),
            mResponses.end());

        if (mResponses.empty()) {
            mResponsesEmptyCV.notify_all();
        }
    }
    {
        auto lock = std::lock_guard { mRequestsMutex };
        mRequestsInProgress--;
        mRequestsCV.notify_one();
    }
}

void RemoteProjectSnapshot::MarkProjectInDB(bool successfulDownload)
{
    if (mDownloadDetached) {
        return;
    }

    auto& db         = CloudProjectsDatabase::Get();
    auto currentData = db.GetProjectData(mProjectInfo.Id);

    auto data = currentData ? *currentData : DBProjectData {};

    data.ProjectId  = mProjectInfo.Id;
    data.SnapshotId = mSnapshotInfo.Id;
    data.SyncStatus = successfulDownload ? DBProjectData::SyncStatusSynced
                      : DBProjectData::SyncStatusDownloading;
    data.LastRead   = wxDateTime::Now().GetTicks();
    data.LocalPath  = mPath;

    if (data.SavesCount == 0) {
        data.SavesCount = 1;
    }

    // For the downloaded projects - we don't need to show the dialog
    data.FirstSyncDialogShown = true;

    db.UpdateProjectData(data);

    if (successfulDownload) {
        db.SetProjectUserSlug(mProjectInfo.Id, mProjectInfo.Username);
    }
}

void RemoteProjectSnapshot::ReportProgress()
{
    if (mState.load(std::memory_order_acquire) != State::Downloading) {
        return;
    }

    const auto projectDownloaded
        =mProjectDownloaded.load(std::memory_order_acquire);
    const auto blocksDownloaded
        =mDownloadedBlocks.load(std::memory_order_acquire);

    const auto blockCopied = mCopiedBlocks.load(std::memory_order_acquire);

    const auto processedBlocks = blocksDownloaded + blockCopied;

    const auto completed
        =processedBlocks == mMissingBlocks && projectDownloaded;

    if (completed) {
        CleanupOrphanBlocks();
        SetState(State::Succeeded);
        MarkProjectInDB(true);
    }

    mCallback({ {}, processedBlocks, mMissingBlocks, projectDownloaded });
}

bool RemoteProjectSnapshot::InProgress() const
{
    return mState.load(std::memory_order_acquire) == State::Downloading;
}

void RemoteProjectSnapshot::RequestsThread()
{
    constexpr auto MAX_CONCURRENT_REQUESTS = 6;

    while (InProgress())
    {
        std::pair<std::string, SuccessHandler> request;

        {
            auto lock = std::unique_lock { mRequestsMutex };

            if (mRequestsInProgress >= MAX_CONCURRENT_REQUESTS) {
                mRequestsCV.wait(
                    lock,
                    [this, MAX_CONCURRENT_REQUESTS] {
                    return mRequestsInProgress < MAX_CONCURRENT_REQUESTS
                           || !InProgress();
                });
            }

            if (!InProgress()) {
                return;
            }

            if (mNextRequestIndex >= mRequests.size()) {
                return;
            }

            request = mRequests[mNextRequestIndex++];
            mRequestsInProgress++;
        }

        DownloadBlob(std::move(request.first), std::move(request.second), 3);

        // TODO: Random sleep to avoid overloading the server
        std::this_thread::sleep_for(std::chrono::milliseconds(50));
    }
}

void RemoteProjectSnapshot::SetState(State state)
{
    if (state != State::Downloading) {
        mEndTime = Clock::now();
    }

    mState.exchange(state);
}

void cloud::audiocom::sync::RemoteProjectSnapshot::CleanupOrphanBlocks()
{
    auto db = CloudProjectsDatabase::Get().GetConnection();

    auto transaction = db->BeginTransaction("d_" + mProjectInfo.Id);

    std::unordered_set<std::string> snaphotBlockHashes;

    for (const auto& block : mSnapshotInfo.Blocks) {
        snaphotBlockHashes.insert(ToUpper(block.Hash));
    }

    auto inSnaphotFunction = db->CreateScalarFunction(
        "inSnapshot", [&snaphotBlockHashes](const std::string& hash)
    { return snaphotBlockHashes.find(hash) != snaphotBlockHashes.end(); });

    // Delete blocks not in the snapshot
    auto deleteBlocksStatement = db->CreateStatement(
        "DELETE FROM " + mSnapshotDBName
        + ".sampleblocks WHERE blockid NOT IN (SELECT block_id FROM block_hashes WHERE project_id = ? AND inSnapshot(hash))");

    if (!deleteBlocksStatement) {
        return;
    }

    auto result = deleteBlocksStatement->Prepare(mProjectInfo.Id).Run();

    if (!result.IsOk()) {
        return;
    }

    auto deleteHashesStatement = db->CreateStatement(
        "DELETE FROM block_hashes WHERE project_id = ? AND NOT inSnapshot(hash)");

    if (!deleteHashesStatement) {
        return;
    }

    result = deleteHashesStatement->Prepare(mProjectInfo.Id).Run();

    if (!result.IsOk()) {
        return;
    }

    transaction.Commit();
}

bool RemoteProjectSnapshotState::IsComplete() const noexcept
{
    return (BlocksDownloaded == BlocksTotal && ProjectDownloaded)
           || Result.Code != SyncResultCode::Success;
}
} // namespace audacity::cloud::audiocom::sync
