/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <atomic>
#include <cstdint>
#include <deque>
#include <functional>
#include <memory>
#include <optional>
#include <string>
#include <vector>

#include "CloudSyncDTO.h"
#include "CloudSyncError.h"
#include "ProjectCloudExtension.h"

#include "au3-concurrency/concurrency/CancellationContext.h"

namespace audacity::cloud::audiocom::sync {
class MissingBlocksUploader;
struct BlockUploadTask;

class SnapshotUploadQueue final : public std::enable_shared_from_this<SnapshotUploadQueue>
{
public:
    explicit SnapshotUploadQueue(ProjectCloudExtension& extension);
    ~SnapshotUploadQueue();

    bool Enqueue(std::string name, UploadMode mode);
    bool RestorePending();

    void Run();
    void Pause();

    bool IsEmpty() const;

private:
    struct BlockCapture;

    enum class Stage
    {
        Hashing,
        ReadyToCreate,
        CreateSent,
        CreateResultUnknown,
        Created,
    };

    struct Entry final
    {
        uint64_t Id {};
        std::string Name;
        UploadMode Mode { UploadMode::Normal };

        std::vector<uint8_t> ProjectBlob;
        std::shared_ptr<BlockCapture> Capture;
        std::vector<LockedBlock> Blocks;

        Stage CurrentStage { Stage::Hashing };

        std::string ProjectId;
        std::string SnapshotId;
        std::optional<ProjectSyncState> SyncState;
        bool PendingStored { false };

        int64_t BlocksTotal { 0 };
        int64_t BlocksHandled { 0 };
        bool FileUploaded { false };
    };

    using BodyHandler = std::function<void (const std::string&)>;
    using ErrorHandler = std::function<void (CloudSyncError)>;

    Entry* FindEntry(uint64_t entryId);
    void OnEntryHashed(uint64_t entryId);

    void Drive();

    void SendCreate(Entry& entry);
    void OnCreated(Entry& entry, const ProjectInfo& project, const SnapshotInfo& snapshot, bool newProject);

    void Reconcile(Entry& entry);
    void OnCreateResultUnknown(CloudSyncError error);

    void StartUpload(Entry& entry);
    void UploadWithState(uint64_t token, const ProjectSyncState& state);
    void UploadBlocks(uint64_t token, std::vector<BlockUploadTask> tasks);
    void Confirm(uint64_t token);
    void OnConfirmed();
    void OnUploadFailed(uint64_t token, CloudSyncError error);
    void AbortUploadStep();
    void StorePending(Entry& entry, const ProjectSyncState& state);

    void Get(const std::string& url, BodyHandler onBody, ErrorHandler onError);

    void Suspend(CloudSyncError error);
    void Fail(CloudSyncError error);
    void DropAll(CloudSyncError error);
    void DropSuperseded();

    void PublishStatus();

    ProjectCloudExtension& mExtension;

    std::deque<Entry> mEntries;
    std::atomic<size_t> mSize { 0 };
    uint64_t mNextEntryId { 1 };

    bool mPaused { false };
    std::optional<CloudSyncError> mPauseError;

    bool mStepInFlight { false };
    uint64_t mUploadToken { 0 };
    concurrency::CancellationContextPtr mUploadContext;
    std::shared_ptr<MissingBlocksUploader> mBlocksUploader;
};
} // namespace audacity::cloud::audiocom::sync
