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

#include "WavPackCompressor.h"

namespace cloud::audiocom::sync
{

MissingBlocksUploader::MissingBlocksUploader(
   std::shared_ptr<AudacityProject> project, std::vector<MissingBlock> blocks,
   MissingBlocksUploadProgressCallback progress)
    : mProject { project }
    , mBlocks { std::move(blocks) }
    , mProgressCallback { std::move(progress) }
{
   mProgressData.TotalBlocks = mBlocks.size();

   for (auto& thread : mProducerThread)
      thread = std::thread([this] { ProducerThread(); });

   mConsumerThread = std::thread([this] { ConsumerThread(); });
}

MissingBlocksUploader::ProducedItem MissingBlocksUploader::ProduceBlock()
{
   const auto index = mFirstUnprocessedBlockIndex++;

   const auto missingBlockId = mBlocks[index].Id;

   return { mBlocks[index], CompressBlock(*mProject, missingBlockId) };
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

   using namespace audacity::network_manager;

   auto request = Request(item.Block.UploadUrl);

   request.setHeader(
      common_headers::ContentType,
      common_content_types::ApplicationXOctetStream);

   auto response = NetworkManager::GetInstance().doPut(
      request, item.CompressedData.data(), item.CompressedData.size());

   item.CompressedData = {};

   response->setRequestFinishedCallback(
      [this, response, item = std::move(item)](auto)
      {
         if (response->getError() != NetworkError::NoError)
         {
            HandleFailedBlock(*response, item.Block.Id);
            return;
         }

         ConfirmBlock(std::move(item));
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
   mRingBufferNotEmpty.wait(lock, [this] { return mRingBufferWriteIndex != mRingBufferReadIndex || !mIsRunning.load(std::memory_order_consume); });

   if (!mIsRunning.load(std::memory_order_relaxed))
      return {};

   auto item = std::move(mRingBuffer[mRingBufferReadIndex]);
   mRingBufferReadIndex = (mRingBufferReadIndex + 1) % RING_BUFFER_SIZE;

   mRingBufferNotFull.notify_one();

   return std::move(item);
}

void MissingBlocksUploader::ConfirmBlock(ProducedItem item)
{
   using namespace audacity::network_manager;

   const auto request = Request(item.Block.ConfirmUrl);

   auto response = NetworkManager::GetInstance().doPost(request, nullptr, 0);

   response->setRequestFinishedCallback(
      [this, response, item = std::move(item)](auto)
      {
         if (response->getError() != NetworkError::NoError)
         {
            HandleFailedBlock(*response, item.Block.Id);
            return;
         }

         {
            std::lock_guard<std::mutex> lock(mProgressDataMutex);
            mProgressData.UploadedBlocks++;
            if (mProgressCallback)
            {
               BasicUI::CallAfter(
                  [this, blockId = item.Block.Id]()
                  {
                     if (mProject)
                        RemoveMissingBlock(*mProject, blockId);

                     std::lock_guard<std::mutex> lock(mProgressDataMutex);
                     mProgressCallback(mProgressData);
                  });
            }
         }

         {
            std::lock_guard<std::mutex> lock(mUploadsMutex);
            --mConcurrentUploads;
            mUploadsNotFull.notify_one();
         }
      });
}

void MissingBlocksUploader::HandleFailedBlock(
   audacity::network_manager::IResponse& response, BlockID blockId)
{
   std::lock_guard<std::mutex> lock(mProgressDataMutex);

   mProgressData.FailedBlocks++;

   mProgressData.ErrorMessages.push_back(response.getErrorString());

   if (response.getError() == audacity::network_manager::NetworkError::HTTPError)
   {
      mProgressData.ErrorMessages.push_back(
         std::to_string(response.getHTTPCode()) + ": " +
         response.readAll<std::string>());
   }

   if (mProgressCallback)
   {
      const bool removeBlock = response.getHTTPCode() == 409;

      BasicUI::CallAfter(
         [this, removeBlock, blockId]()
         {
            if (mProject && removeBlock)
               RemoveMissingBlock(*mProject, blockId);
            std::lock_guard<std::mutex> lock(mProgressDataMutex);
            mProgressCallback(mProgressData);
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

      if (mFirstUnprocessedBlockIndex >= mBlocks.size())
         return;

      auto item = ProduceBlock();

      if (item.CompressedData.empty())
      {
         BasicUI::CallAfter(
            [this, blockId = item.Block.Id]()
            {
               if (mProject)
                  RemoveMissingBlock(*mProject, blockId);

               std::lock_guard<std::mutex> lock(mProgressDataMutex);
               mProgressData.FailedBlocks++;
               mProgressCallback(mProgressData);
            });

         return;
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
