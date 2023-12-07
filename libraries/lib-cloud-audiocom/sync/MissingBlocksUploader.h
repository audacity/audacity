/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MissingBlocksUploader.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <atomic>
#include <array>
#include <cstdint>
#include <condition_variable>
#include <mutex>
#include <thread>

#include <functional>
#include "CloudSyncUtils.h"

namespace audacity::network_manager
{
class IResponse;
}

class AudacityProject;


namespace cloud::audiocom
{
class ServiceConfig;
} // namespace cloud::audiocom

namespace cloud::audiocom::sync
{
struct UploadResult;

struct MissingBlocksUploadProgress final
{
   int64_t TotalBlocks = 0;
   int64_t UploadedBlocks = 0;
   int64_t FailedBlocks = 0;

   std::vector<std::string> ErrorMessages;
};

struct BlockUploadTask final
{
   UploadUrls BlockUrls;
   LockedBlock Block;
};

enum class BlockAction
{
   RemoveFromMissing,
   Expire,
   Ignore,
};

using MissingBlocksUploadProgressCallback = std::function<void(const MissingBlocksUploadProgress&, const LockedBlock&, BlockAction)>;

class MissingBlocksUploader final
{
public:
   static constexpr auto NUM_PRODUCERS = 3;
   static constexpr auto NUM_UPLOADERS = 6;
   static constexpr auto RING_BUFFER_SIZE = 16;

   MissingBlocksUploader(
      const ServiceConfig& serviceConfig, std::vector<BlockUploadTask> uploadTasks,
      MissingBlocksUploadProgressCallback progress);

private:
   struct ProducedItem final
   {
      BlockUploadTask Task;
      std::vector<uint8_t> CompressedData;
   };

   ProducedItem ProduceBlock();
   void ConsumeBlock(ProducedItem item);

   void PushBlockToQueue(ProducedItem item);
   ProducedItem PopBlockFromQueue();

   void ConfirmBlock(BlockUploadTask task);
   void HandleFailedBlock(const UploadResult& result, BlockUploadTask task);

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
};
} // namespace cloud::audiocom::sync
