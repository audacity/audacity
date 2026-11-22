/*
* Audacity: A Digital Audio Editor
*/
#include "SnapshotReconcile.h"

#include <algorithm>
#include <iterator>

#include "au3-string-utils/StringUtils.h"

namespace audacity::cloud::audiocom::sync {
namespace {
template<typename Range, typename Projection>
SampleBlockHashes NormalizedHashes(const Range& range, Projection projection)
{
    SampleBlockHashes hashes;
    hashes.reserve(range.size());
    std::transform(
        range.begin(), range.end(), std::back_inserter(hashes),
        [&](const auto& item) { return ToUpper(projection(item)); });
    std::sort(hashes.begin(), hashes.end());
    return hashes;
}
} // namespace

ReconcileDecision DecideReconcile(
    const std::string& localHeadId, const SnapshotInfo& serverHead, const SampleBlockHashes& requestBlockHashes)
{
    if (serverHead.Id == localHeadId) {
        return ReconcileDecision::Resend;
    }

    const bool sameContent
        =NormalizedHashes(serverHead.Blocks, [](const SnapshotBlockInfo& block) { return block.Hash; })
          == NormalizedHashes(requestBlockHashes, [](const BlockHash& hash) { return hash; });

    if (serverHead.ParentId == localHeadId && serverHead.Synced == 0 && sameContent) {
        return ReconcileDecision::Adopt;
    }

    return ReconcileDecision::Diverged;
}
} // namespace audacity::cloud::audiocom::sync
