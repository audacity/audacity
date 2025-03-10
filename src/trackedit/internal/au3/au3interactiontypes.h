/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <cstddef>
#include <vector>

namespace au::trackedit {
struct TrackListInfo {
    TrackListInfo(size_t size, std::vector<size_t> stereoTrackIndices, std::vector<size_t> emptyTrackIndices)
        : size{size}
        , stereoTrackIndices{std::move(stereoTrackIndices)}
        , emptyTrackIndices{std::move(emptyTrackIndices)}
    {
    }

    const size_t size;
    const std::vector<size_t> stereoTrackIndices;
    const std::vector<size_t> emptyTrackIndices;
};

enum class NeedsDownmixing {
    Yes,
    No,
};

constexpr NeedsDownmixing operator|=(NeedsDownmixing& lhs, NeedsDownmixing rhs)
{
    return lhs = lhs == NeedsDownmixing::Yes || rhs == NeedsDownmixing::Yes ? NeedsDownmixing::Yes : NeedsDownmixing::No;
}
}
