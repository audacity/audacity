/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  BlockHasher.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <cstdint>
#include <functional>
#include <string>
#include <memory>
#include <vector>
#include <unordered_map>

#include "CloudSyncDTO.h"

namespace audacity::cloud::audiocom::sync {
class BlockHashCache /* not final */
{
public:
    virtual ~BlockHashCache() = default;

    virtual bool GetHash(int64_t blockId, std::string& hash) const = 0;
    virtual void UpdateHash(int64_t blockId, const std::string& hash) = 0;
};

class BlockHasher final
{
public:
    BlockHasher();
    ~BlockHasher();

    BlockHasher(const BlockHasher&) = delete;
    BlockHasher(BlockHasher&&) = delete;
    BlockHasher& operator=(const BlockHasher&) = delete;
    BlockHasher& operator=(BlockHasher&&) = delete;

    bool ComputeHashes(BlockHashCache& cache, std::vector<LockedBlock> blocks, std::function<void()> onComplete);
    bool IsReady() const;

    std::vector<std::pair<int64_t, std::string> > TakeResult();

private:
    class Workers;
    std::unique_ptr<Workers> mWorkers;
};
} // namespace audacity::cloud::audiocom::sync
