/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <atomic>
#include <cstdint>
#include <deque>
#include <memory>
#include <string>
#include <vector>

#include "CloudSyncDTO.h"
#include "ProjectCloudExtension.h"

namespace audacity::cloud::audiocom::sync {
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
    };

    Entry* FindEntry(uint64_t entryId);
    void OnEntryHashed(uint64_t entryId);

    ProjectCloudExtension& mExtension;

    std::deque<Entry> mEntries;
    std::atomic<size_t> mSize { 0 };
    uint64_t mNextEntryId { 1 };
};
} // namespace audacity::cloud::audiocom::sync
