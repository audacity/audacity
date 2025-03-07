/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <cstddef>
#include <vector>

namespace au::trackedit {
struct TrackListInfo {
    TrackListInfo(size_t size, std::vector<size_t> stereos, std::vector<size_t> empties)
        : size{size}
        , stereos{std::move(stereos)}
        , empties{std::move(empties)}
    {
    }

    const size_t size;
    //! Short for "stereo-track indices"
    const std::vector<size_t> stereos;
    //! Short for "empty-track indices"
    const std::vector<size_t> empties;
};

enum class ClipConversionType {
    NoConversion = 0b00,
    MonoToStereo = 0b01,
    StereoToMono = 0b10,
    Bothways = 0b11
};

constexpr ClipConversionType operator|=(ClipConversionType& lhs, ClipConversionType rhs)
{
    return lhs = static_cast<ClipConversionType>(static_cast<int>(lhs) | static_cast<int>(rhs));
}
}
