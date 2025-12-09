/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MissingBlocksUploader.cpp

  Dmitry Vedenko

**********************************************************************/

#include "MissingBlocksUploader.h"

#include "DataUploader.h"

#include "WavPackCompressor.h"

namespace audacity::cloud::audiocom::sync {
MissingBlocksUploader::MissingBlocksUploader(
    Tag, const ServiceConfig& serviceConfig)
    : mServiceConfig{serviceConfig}
{
}

std::shared_ptr<MissingBlocksUploader> MissingBlocksUploader::Create(
    CancellationContextPtr cancellationContex, const ServiceConfig& serviceConfig,
    std::vector<BlockUploadTask> uploadTasks,
    MissingBlocksUploadProgressCallback progressCallback)
{
    auto uploader
        =std::make_shared<MissingBlocksUploader>(Tag {}, serviceConfig);

    if (!cancellationContex) {
        cancellationContex = concurrency::CancellationContext::Create();
    }

    cancellationContex->OnCancelled(uploader);

    uploader->Start(
        std::move(cancellationContex), std::move(uploadTasks),
        std::move(progressCallback));

    return uploader;
}

MissingBlocksUploader::~MissingBlocksUploader()
{
    Cancel();
}

void MissingBlocksUploader::Start(
    CancellationContextPtr cancellationContex,
    std::vector<BlockUploadTask> uploadTasks,
    MissingBlocksUploadProgressCallback progressCallback)
{
    mCancellationContext = std::move(cancellationContex);
    mUploadTasks         = std::move(uploadTasks);
    mProgressCallback    = std::move(progressCallback);
    if (!mProgressCallback) {
        mProgressCallback = [](auto...) {} }

    mProgressData.TotalBlocks = mUploadTasks.size();

    for (auto& thread : mProducerThread) {
        thread = std::thread([this] { ProducerThread(); });
    }

    mConsumerThread = std::thread([this] { ConsumerThread(); });
}

void MissingBlocksUploader::Cancel()
{
    if (!mIsRunning.exchange(false)) {
        return;
    }

    mRingBufferNotEmpty.notify_all();
    mRingBufferNotFull.notify_all();
    mUploadsNotFull.notify_all();

    for (auto& thread : mProducerThread) {
        thread.join();
    }

    mConsumerThread.join();

    std::lock_guard lock(mProgressDataMutex);
}

MissingBlocksUploader::ProducedItem MissingBlocksUploader::ProduceBlock()
{
    const auto index = mFirstUnprocessedBlockIndex++;
    const auto& task = mUploadTasks[index];

    return { task, CompressBlock(task.Block) };
}

void MissingBlocksUploader::ConsumeBlock(ProducedItem item)
{
    {
        std::unique_lock<std::mutex> lock(mUploadsMutex);
        mUploadsNotFull.wait(
            lock,
            [this]
        {
            return mConcurrentUploads < NUM_UPLOADERS
                   || !mIsRunning.load(std::memory_order_consume);
        });

        if (!mIsRunning.load(std::memory_order_relaxed)) {
            return;
        }

        ++mConcurrentUploads;
    }

    DataUploader::Get().Upload(
        mCancellationContext, mServiceConfig, item.Task.BlockUrls,
        std::move(item.CompressedData),
        [this, task = item.Task,
         weakThis = weak_from_this()](ResponseResult result)
    {
        auto lock = weakThis.lock();

        if (!lock) {
            return;
        }

        if (result.Code != SyncResultCode::Success) {
            HandleFailedBlock(result, task);
        } else {
            ConfirmBlock(task);
        }
    });
}

void MissingBlocksUploader::PushBlockToQueue(ProducedItem item)
{
    std::unique_lock<std::mutex> lock(mRingBufferMutex);
    mRingBufferNotFull.wait(
        lock,
        [this]
    {
        return ((mRingBufferWriteIndex + 1) % RING_BUFFER_SIZE)
               != mRingBufferReadIndex
               || !mIsRunning.load(std::memory_order_consume);
    });

    if (!mIsRunning.load(std::memory_order_relaxed)) {
        return;
    }

    mRingBuffer[mRingBufferWriteIndex] = std::move(item);
    mRingBufferWriteIndex = (mRingBufferWriteIndex + 1) % RING_BUFFER_SIZE;

    mRingBufferNotEmpty.notify_one();
}

MissingBlocksUploader::ProducedItem MissingBlocksUploader::PopBlockFromQueue()
{
    std::unique_lock<std::mutex> lock(mRingBufferMutex);
    mRingBufferNotEmpty.wait(
        lock,
        [this]
    {
        return mRingBufferWriteIndex != mRingBufferReadIndex
               || !mIsRunning.load(std::memory_order_consume);
    });

    if (!mIsRunning.load(std::memory_order_relaxed)) {
        return {}
    }

    auto item            = std::move(mRingBuffer[mRingBufferReadIndex]);
    mRingBufferReadIndex = (mRingBufferReadIndex + 1) % RING_BUFFER_SIZE;

    mRingBufferNotFull.notify_one();

    return std::move(item);
}

void MissingBlocksUploader::ConfirmBlock(BlockUploadTask item)
{
    MissingBlocksUploadProgress progressData;
    {
        std::lock_guard<std::mutex> lock(mProgressDataMutex);
        mProgressData.UploadedBlocks++;
        progressData = mProgressData;
    }

    mProgressCallback(progressData, item.Block, {});

    {
        std::lock_guard<std::mutex> lock(mUploadsMutex);
        --mConcurrentUploads;
        mUploadsNotFull.notify_one();
    }
}

void MissingBlocksUploader::HandleFailedBlock(
    const ResponseResult& result, BlockUploadTask task)
{
    MissingBlocksUploadProgress progressData;
    {
        std::lock_guard<std::mutex> lock(mProgressDataMutex);

        mProgressData.FailedBlocks++;
        mProgressData.UploadErrors.push_back(result);
        progressData = mProgressData;
    }

    mProgressCallback(progressData, task.Block, result);

    {
        std::lock_guard<std::mutex> lock(mUploadsMutex);
        --mConcurrentUploads;
        mUploadsNotFull.notify_one();
    }

    mCancellationContext->Cancel();
}

void MissingBlocksUploader::ProducerThread()
{
    while (mIsRunning.load(std::memory_order_consume))
    {
        BlockUploadTask task;

        {
            std::lock_guard<std::mutex> lock(mBlocksMutex);

            if (mFirstUnprocessedBlockIndex >= mUploadTasks.size()) {
                return;
            }

            const auto index = mFirstUnprocessedBlockIndex++;
            task = std::move(mUploadTasks[index]);
        }

        auto compressedData = CompressBlock(task.Block);

        if (compressedData.empty()) {
            MissingBlocksUploadProgress progressData;
            {
                std::lock_guard<std::mutex> lock(mProgressDataMutex);
                mProgressData.FailedBlocks++;
                progressData = mProgressData;
            }

            mProgressCallback(
                progressData, task.Block,
                { SyncResultCode::InternalClientError, {} });
        } else {
            PushBlockToQueue(
                ProducedItem { std::move(task), std::move(compressedData) });
        }
    }
}

void MissingBlocksUploader::ConsumerThread()
{
    while (mIsRunning.load(std::memory_order_consume))
    {
        auto item = PopBlockFromQueue();
        ConsumeBlock(std::move(item));
    }
}
} // namespace audacity::cloud::audiocom::sync
