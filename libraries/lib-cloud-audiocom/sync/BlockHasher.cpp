/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  BlockHasher.cpp

  Dmitry Vedenko

**********************************************************************/
#include "BlockHasher.h"

#include <algorithm>
#include <atomic>
#include <future>
#include <utility>

#include "BasicUI.h"
#include "MemoryX.h"
#include "SampleBlock.h"

#include "crypto/SHA256.h"

namespace cloud::audiocom::sync
{
class BlockHasher::Workers final
{
public:
   using SampleData = std::vector<std::remove_pointer_t<samplePtr>>;

   explicit Workers(
      BlockHashCache& cache, const std::vector<LockedBlock> blocks,
      std::function<void()> onComplete)
       : mThreadsCount { std::max(1u, std::thread::hardware_concurrency() / 2) }
       , mCache { cache }
       , mOnComplete { std::move(onComplete) }
   {
      mResults.reserve(mThreadsCount);

      const auto blocksCount = blocks.size();
      // Try to add no more that 1 extra block per thread
      const size_t blockPerThread = blocks.size() / mThreadsCount + 1;

      for (size_t i = 0; i < mThreadsCount; ++i)
      {
         const size_t startIndex = i;

         if (startIndex >= blocks.size())
            break;

         mTasksLeft.fetch_add(1, std::memory_order_seq_cst);

         std::vector<LockedBlock> threadBlocks;
         threadBlocks.reserve(blockPerThread);

         for (size_t j = startIndex; j < blocksCount; j += mThreadsCount)
            threadBlocks.emplace_back(blocks[j]);
            
         mResults.emplace_back(std::async(std::launch::async,
            [this, threadBlocks = std::move(threadBlocks)]()
            {
               Result result;
               SampleData sampleData;

               for (const auto& block : threadBlocks)
                  result.emplace(block.Id, ComputeHash(sampleData, block));

               if (mTasksLeft.fetch_sub(1, std::memory_order_seq_cst) == 1)
                  NotifyReady();

               return result;
            }));
      }
   }

   bool IsReady() const
   {
      return std::all_of(
         mResults.begin(), mResults.end(),
         [](const auto& result)
         {
            return result.wait_for(std::chrono::seconds(0)) ==
                   std::future_status::ready;
         });
   }

   std::pair<std::string, bool>
   ComputeHash(SampleData& sampleData, const LockedBlock& block) const
   {
      std::string hash;

      if (mCache.GetHash(block.Id, hash))
         return { hash, false };

      const auto sampleFormat = block.Format;
      const auto sampleCount = block.Block->GetSampleCount();
      const auto dataSize = sampleCount * SAMPLE_SIZE(sampleFormat);

      sampleData.resize(dataSize);

      const size_t samplesRead = block.Block->GetSamples(
         sampleData.data(), sampleFormat, 0, sampleCount, false);

      if (samplesRead != sampleCount)
         return { {}, false };

      hash = crypto::sha256(sampleData);

      return { hash, true };
   }

   void NotifyReady()
   {
      BasicUI::CallAfter(
         [this]
         {
            if (mOnComplete)
               mOnComplete();
         });
   }

   std::vector<std::pair<int64_t, std::string>> TakeResult()
   {
      std::vector<std::pair<int64_t, std::string>> result;

      for (auto& fut : mResults)
      {
         const auto& threadResult = fut.get();

         for (const auto& [id, hash] : threadResult)
         {
            result.emplace_back(std::make_pair(id, hash.first));

            if (hash.second)
               mCache.UpdateHash(id, hash.first);
         }
      }


      mResults.clear();
      
      return result;
   }

private:
   const size_t mThreadsCount;

   std::atomic<size_t> mTasksLeft;

   BlockHashCache& mCache;

   using Result = std::unordered_map<int64_t, std::pair<std::string, bool>>;
   std::vector<std::future<Result>> mResults;

   std::function<void()> mOnComplete;
};

BlockHasher::BlockHasher() = default;
BlockHasher::~BlockHasher() = default;

bool BlockHasher::ComputeHashes(
   BlockHashCache& cache, std::vector<LockedBlock> blocks,
   std::function<void()> onComplete)
{
   if (mWorkers != nullptr && !mWorkers->IsReady())
      return false;

   if (blocks.empty())
   {
      if (onComplete)
         onComplete();

      return true;
   }

   mWorkers = std::make_unique<Workers>(
      cache, std::move(blocks), std::move(onComplete));

   return true;
}

bool BlockHasher::IsReady() const
{
   return mWorkers != nullptr && mWorkers->IsReady();
}

std::vector<std::pair<int64_t, std::string>> BlockHasher::TakeResult()
{
   if (mWorkers == nullptr)
      return {};

   auto cleanup = finally([this]() { mWorkers.reset(); });

   return mWorkers->TakeResult();
}

} // namespace cloud::audiocom::sync
