/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MissingBlocksUploader.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <array>
#include <atomic>
#include <condition_variable>
#include <cstdint>
#include <mutex>
#include <thread>

#include <functional>

#include "CloudSyncDTO.h"
#include "NetworkUtils.h"

#include "concurrency/CancellationContext.h"
#include "concurrency/ICancellable.h"

namespace audacity::network_manager {
class IResponse;
}

class AudacityProject;

namespace audacity::cloud::audiocom {
class ServiceConfig;
} // namespace audacity::cloud::audiocom

namespace audacity::cloud::audiocom::sync {
using concurrency::CancellationContextPtr;

struct MissingBlocksUploadProgress final
{
    int64_t TotalBlocks    = 0;
    int64_t UploadedBlocks = 0;
    int64_t FailedBlocks   = 0;

    std::vector<ResponseResult> UploadErrors;
};

struct BlockUploadTask final
{
    UploadUrls BlockUrls;
    LockedBlock Block;
};

using MissingBlocksUploadProgressCallback = std::function<void (
                                                              const MissingBlocksUploadProgress&, const LockedBlock&,
                                                              ResponseResult blockResponseResult)>;

class MissingBlocksUploader final : public concurrency::ICancellable, public std::enable_shared_from_this<MissingBlocksUploader>
{
    struct Tag final
    {
    };

public:
    static constexpr auto NUM_PRODUCERS    = 3;
    static constexpr auto NUM_UPLOADERS    = 6;
    static constexpr auto RING_BUFFER_SIZE = 16;

    MissingBlocksUploader(Tag, const ServiceConfig& serviceConfig);

    static std::shared_ptr<MissingBlocksUploader> Create(
        CancellationContextPtr cancellationContex, const ServiceConfig& serviceConfig, std::vector<BlockUploadTask> uploadTasks,
        MissingBlocksUploadProgressCallback progress);

    ~MissingBlocksUploader();

private:
    struct ProducedItem final
    {
        BlockUploadTask Task;
        std::vector<uint8_t> CompressedData;
    };

    void Start(
        CancellationContextPtr cancellationContex, std::vector<BlockUploadTask> uploadTasks, MissingBlocksUploadProgressCallback progress);

    void Cancel() override;

    ProducedItem ProduceBlock();
    void ConsumeBlock(ProducedItem item);

    void PushBlockToQueue(ProducedItem item);
    ProducedItem PopBlockFromQueue();

    void ConfirmBlock(BlockUploadTask task);
    void HandleFailedBlock(const ResponseResult& result, BlockUploadTask task);

    void ProducerThread();
    void ConsumerThread();

    const ServiceConfig& mServiceConfig;

    std::vector<BlockUploadTask> mUploadTasks;
    MissingBlocksUploadProgressCallback mProgressCallback;

    std::atomic_bool mIsRunning { true };

    std::thread mProducerThread[NUM_PRODUCERS];
    std::thread mConsumerThread;

    std::mutex mBlocksMutex;
    size_t mFirstUnprocessedBlockIndex { 0 };

    std::mutex mUploadsMutex;
    std::condition_variable mUploadsNotFull;
    size_t mConcurrentUploads { 0 };

    std::mutex mRingBufferMutex;

    std::condition_variable mRingBufferNotEmpty;
    std::condition_variable mRingBufferNotFull;

    std::array<ProducedItem, RING_BUFFER_SIZE> mRingBuffer;
    size_t mRingBufferWriteIndex { 0 };
    size_t mRingBufferReadIndex { 0 };

    std::mutex mProgressDataMutex;
    MissingBlocksUploadProgress mProgressData;

    CancellationContextPtr mCancellationContext;
};
} // namespace audacity::cloud::audiocom::sync
