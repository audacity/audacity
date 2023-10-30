/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MissingBlocksUploader.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <atomic>
#include <array>
#include <condition_variable>
#include <mutex>
#include <thread>

#include <functional>
#include "CloudSyncUtils.h"

namespace audacity::network_manager
{
class IResponse;
}

namespace cloud::audiocom::sync
{
struct MissingBlocksUploadProgress final
{
   std::size_t TotalBlocks = 0;
   std::size_t UploadedBlocks = 0;
   std::size_t FailedBlocks = 0;

   std::vector<std::string> ErrorMessages;
};

using MissingBlocksUploadProgressCallback = std::function<void(MissingBlocksUploadProgress)>;

class MissingBlocksUploader final
{
public:
   static constexpr auto NUM_PRODUCERS = 3;
   static constexpr auto NUM_UPLOADERS = 6;
   static constexpr auto RING_BUFFER_SIZE = 16;

   MissingBlocksUploader(std::shared_ptr<AudacityProject> project,
      std::vector<MissingBlock> blocks, MissingBlocksUploadProgressCallback progress);

private:
   struct ProducedItem final
   {
      MissingBlock Block;
      std::vector<uint8_t> CompressedData;
   };

   ProducedItem ProduceBlock();
   void ConsumeBlock(ProducedItem item);

   void PushBlockToQueue(ProducedItem item);
   ProducedItem PopBlockFromQueue();

   void ConfirmBlock(ProducedItem item);
   void HandleFailedBlock(audacity::network_manager::IResponse& response, BlockID blockId);

   void ProducerThread();
   void ConsumerThread();

   std::shared_ptr<AudacityProject> mProject;
   std::vector<MissingBlock> mBlocks;
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
};
} // namespace cloud::audiocom::sync
