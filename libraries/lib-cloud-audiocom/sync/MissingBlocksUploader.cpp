/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MissingBlocksUploader.cpp

  Dmitry Vedenko

**********************************************************************/

#include "MissingBlocksUploader.h"

#include "NetworkManager.h"
#include "Request.h"
#include "IResponse.h"
#include "BasicUI.h"

#include "DataUploader.h"

#include "WavPackCompressor.h"

namespace cloud::audiocom::sync
{

MissingBlocksUploader::MissingBlocksUploader(
   const ServiceConfig& serviceConfig, std::vector<BlockUploadTask> uploadTasks,
   MissingBlocksUploadProgressCallback progress)
    : mServiceConfig { serviceConfig }
    , mUploadTasks { std::move(uploadTasks) }
    , mProgressCallback { std::move(progress) }
{
   mProgressData.TotalBlocks = mUploadTasks.size();

   for (auto& thread : mProducerThread)
      thread = std::thread([this] { ProducerThread(); });

   mConsumerThread = std::thread([this] { ConsumerThread(); });

   if (!mProgressCallback)
      mProgressCallback = [](auto...) {};
}

MissingBlocksUploader::~MissingBlocksUploader()
{
   mIsRunning.store(false, std::memory_order_release);

   mRingBufferNotEmpty.notify_all();
   mRingBufferNotFull.notify_all();
   mUploadsNotFull.notify_all();

   for (auto& thread : mProducerThread)
      thread.join();

   mConsumerThread.join();
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
            return mConcurrentUploads < NUM_UPLOADERS ||
                   !mIsRunning.load(std::memory_order_consume);
         });

      if (!mIsRunning.load(std::memory_order_relaxed))
         return;

      ++mConcurrentUploads;
   }

   DataUploader::Get().Upload(
      mServiceConfig, item.Task.BlockUrls, std::move(item.CompressedData),
      [this, task = item.Task](UploadResult result)
      {
         if (result.Code != UploadResultCode::Success)
            HandleFailedBlock(result, task);
         else
            ConfirmBlock(task);
      });
}

void MissingBlocksUploader::PushBlockToQueue(ProducedItem item)
{
   std::unique_lock<std::mutex> lock(mRingBufferMutex);
   mRingBufferNotFull.wait(
      lock,
      [this]
      {
         return ((mRingBufferWriteIndex + 1) % RING_BUFFER_SIZE) !=
                   mRingBufferReadIndex ||
                !mIsRunning.load(std::memory_order_consume);
      });

   if (!mIsRunning.load(std::memory_order_relaxed))
      return;

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
         return mRingBufferWriteIndex != mRingBufferReadIndex ||
                !mIsRunning.load(std::memory_order_consume);
      });

   if (!mIsRunning.load(std::memory_order_relaxed))
      return {};

   auto item = std::move(mRingBuffer[mRingBufferReadIndex]);
   mRingBufferReadIndex = (mRingBufferReadIndex + 1) % RING_BUFFER_SIZE;

   mRingBufferNotFull.notify_one();

   return std::move(item);
}

void MissingBlocksUploader::ConfirmBlock(BlockUploadTask item)
{
   {
      std::lock_guard<std::mutex> lock(mProgressDataMutex);
      mProgressData.UploadedBlocks++;
      if (mProgressCallback)
      {
         BasicUI::CallAfter(
            [this, task = std::move(item)]()
            {
               std::lock_guard<std::mutex> lock(mProgressDataMutex);
               mProgressCallback(mProgressData, task.Block, BlockAction::RemoveFromMissing);
            });
      }
   }

   {
      std::lock_guard<std::mutex> lock(mUploadsMutex);
      --mConcurrentUploads;
      mUploadsNotFull.notify_one();
   }
}

void MissingBlocksUploader::HandleFailedBlock(
   const UploadResult& result, BlockUploadTask task)
{
   {
      std::lock_guard<std::mutex> lock(mProgressDataMutex);

      mProgressData.FailedBlocks++;
      mProgressData.ErrorMessages.push_back(result.ErrorMessage);

      const auto action =
         result.Code == UploadResultCode::Conflict ?
            BlockAction::RemoveFromMissing :
            (result.Code == UploadResultCode::Expired ? BlockAction::Expire :
                                                        BlockAction::Ignore);

      BasicUI::CallAfter(
         [this, action, task = std::move(task)]()
         {
            std::lock_guard<std::mutex> lock(mProgressDataMutex);
            mProgressCallback(mProgressData, task.Block, action);
         });
   }

   {
      std::lock_guard<std::mutex> lock(mUploadsMutex);
      --mConcurrentUploads;
      mUploadsNotFull.notify_one();
   }
}

void MissingBlocksUploader::ProducerThread()
{
   while (mIsRunning.load (std::memory_order_consume))
   {
      std::lock_guard<std::mutex> lock(mBlocksMutex);

      if (mFirstUnprocessedBlockIndex >= mUploadTasks.size())
         return;

      auto item = ProduceBlock();

      if (item.CompressedData.empty())
      {
         BasicUI::CallAfter(
            [this, task = std::move(item.Task)]()
            {
               std::lock_guard<std::mutex> lock(mProgressDataMutex);
               mProgressData.FailedBlocks++;
               mProgressCallback(mProgressData, task.Block, BlockAction::RemoveFromMissing);
            });

         continue;
      }

      PushBlockToQueue(std::move(item));
   }
}

void MissingBlocksUploader::ConsumerThread()
{
   while (mIsRunning.load (std::memory_order_consume))
   {
      auto item = PopBlockFromQueue();
      ConsumeBlock(std::move(item));
   }
}

} // namespace cloud::audiocom::sync
