/*
* Audacity: A Digital Audio Editor
*/
#include "SnapshotUploadQueue.h"

#include <algorithm>
#include <cstring>
#include <future>
#include <unordered_map>

#include "BlockHasher.h"
#include "CloudProjectsDatabase.h"

#include "au3-basic-ui/BasicUI.h"
#include "au3-crypto/crypto/SHA256.h"
#include "au3-project-file-io/ProjectFileIO.h"
#include "au3-project/Project.h"
#include "au3-track/Track.h"
#include "au3-utility/MemoryX.h"
#include "au3-wave-track/SampleBlock.h"
#include "au3-wave-track/WaveTrackUtilities.h"

namespace audacity::cloud::audiocom::sync {
namespace {
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
    return false;
}

void SnapshotUploadQueue::Run()
{
}

void SnapshotUploadQueue::Pause()
{
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
}
} // namespace audacity::cloud::audiocom::sync
